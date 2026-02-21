/*
Copyright 2026 Adobe
  Distributed under the Boost Software License, Version 1.0.
  (See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
*/

#ifndef CHAIN_SPLIT_HPP
#define CHAIN_SPLIT_HPP

#include <chain/config.hpp>

#include <condition_variable>
#include <exception>
#include <functional>
#include <memory>
#include <mutex>
#include <optional>
#include <type_traits>

namespace chain {
inline namespace CHAIN_VERSION_NAMESPACE() {

template <class T>
    requires std::is_copy_constructible_v<T>
struct split_state {
    std::mutex _m;
    std::condition_variable _cv;
    std::optional<T> _value;
    std::exception_ptr _error{nullptr};
    std::vector<std::function<void(const T&)>> _continuations;
    std::atomic_bool _started{false};
    std::atomic_bool _completed{false};

    void set_value(T v) {
        if (_completed.load(std::memory_order_acquire)) return;
        {
            std::lock_guard lk(_m);
            _value.emplace(std::move(v));
            _completed.store(true, std::memory_order_release);
        }
        auto continuations = extract_continuations();
        for (auto& c : continuations)
            c(*_value);
        _cv.notify_all();
    }

    void set_exception(std::exception_ptr p) {
        {
            std::lock_guard lk(_m);
            _error = p;
            _completed.store(true, std::memory_order_release);
        }
        // auto continuations = extract_continuations();
        // for (auto& c : continuations) {
        //     // We skip invoking branch continuations on exception; if needed they can be
        //     // generalized to receive exception too.
        // }
        _cv.notify_all();
    }

    void add_continuation(std::function<void(const T&)> fn) {
        std::unique_lock lk(_m);
        if (_completed.load(std::memory_order_acquire) && _value) {
            auto v = *_value; // copy out
            lk.unlock();
            fn(v);
            return;
        }
        _continuations.push_back(std::move(fn));
    }

private:
    std::vector<std::function<void(const T&)>> extract_continuations() {
        std::vector<std::function<void(const T&)>> tmp;
        std::swap(tmp, _continuations);
        return tmp;
    }
};

template <class Upstream>
struct split_holder {
    std::shared_ptr<Upstream> _upstream;
    std::shared_ptr<void> _state;
    std::once_flag _init_once;

    explicit split_holder(Upstream&& u) : _upstream(std::make_shared<Upstream>(std::move(u))) {}

private:
    template <class F>
    auto make_branch(F&& f) {
        using upstream_t = Upstream;

        auto branch_segment = segment{
            type<void>{},
            [this]<typename Composed, typename... StartArgs>(Composed&& composed,
                                                             StartArgs&&... start_args) mutable {
                using result_t = typename upstream_t::template result_type<StartArgs...>;

                // Allocate shared state only once (first branch start)
                std::call_once(_init_once,
                               [this] { _state = std::make_shared<split_state<result_t>>(); });

                auto state = std::static_pointer_cast<split_state<result_t>>(_state);

                // Register this branch's continuation
                state->add_continuation(
                    [comp = std::forward<Composed>(composed)](const result_t& v) mutable noexcept {
                        try {
                            comp(v);
                        } catch (...) { /* optional: log */
                        }
                    });

                // Start upstream only once
                if (!state->_started.exchange(true, std::memory_order_acq_rel)) {
                    struct upstream_receiver {
                        std::shared_ptr<split_state<result_t>> _s;
                        void operator()(result_t&& val) { _s->set_value(std::move(val)); }
                        void set_exception(std::exception_ptr p) { _s->set_exception(p); }
                        bool canceled() const { return false; }
                    };
                    auto receiver = std::make_shared<upstream_receiver>();
                    receiver->_s = state;
                    // Move upstream here, because chain wants an rvalue and we only start once.
                    std::move(*_upstream).invoke(receiver, std::forward<StartArgs>(start_args)...);
                }
            },
            std::forward<F>(f)};

        return chain{std::tuple<>{}, std::move(branch_segment)};
    }

public:
    template <class F>
    auto fan(F&& f) & {
        return make_branch(std::forward<F>(f));
    }
    template <class F>
    auto fan(F&& f) && {
        return make_branch(std::forward<F>(f));
    }
};

template <class Chain>
auto split(Chain&& c) {
    return split_holder<Chain>{std::forward<Chain>(c)};
}

template <class Upstream, class... BoundArgs>
struct split_holder_bound {
    using upstream_result_t = typename Upstream::template result_type<BoundArgs...>;

    std::shared_ptr<Upstream> _upstream;
    std::tuple<std::decay_t<BoundArgs>...> _bound_args;
    std::shared_ptr<split_state<upstream_result_t>> _state;
    std::once_flag _start_once;

    explicit split_holder_bound(Upstream&& u, BoundArgs&&... args)
        : _upstream(std::make_shared<Upstream>(std::move(u))),
          _bound_args(std::forward<BoundArgs>(args)...),
          _state(std::make_shared<split_state<upstream_result_t>>()) {}

private:
    template <class F>
    auto make_branch(F&& f) {
        // Segment injects upstream_result_t so result_type<> with () sees correct type.
        auto branch_segment = segment{
            type<upstream_result_t>{},
            [this]<typename Composed>(Composed&& composed) mutable {
                // Register branch continuation (called after upstream completes)
                _state->add_continuation(
                    [comp = std::forward<Composed>(composed)](const upstream_result_t& v) mutable {
                        try {
                            comp(v);
                        } catch (...) { /* optional branch error handling */
                        }
                    });

                // Start upstream only once
                if (!_state->_started.exchange(true, std::memory_order_acq_rel)) {
                    struct upstream_receiver {
                        std::shared_ptr<split_state<upstream_result_t>> _s;
                        void operator()(upstream_result_t&& val) { _s->set_value(std::move(val)); }
                        void set_exception(std::exception_ptr p) { _s->set_exception(p); }
                        bool canceled() const { return false; }
                    };
                    auto receiver = std::make_shared<upstream_receiver>();
                    receiver->_s = _state;

                    // Invoke upstream with bound arguments (no external start args)
                    std::apply(
                        [this, &receiver](auto&... args) {
                            std::move(*_upstream).invoke(receiver, args...);
                        },
                        _bound_args);
                }
            },
            std::forward<F>(f) // branch function (receives upstream_result_t injected as first arg)
        };

        return chain{std::tuple<>{}, std::move(branch_segment)};
    }

public:
    template <class F>
    auto fan(F&& f) & {
        return make_branch(std::forward<F>(f));
    }
    template <class F>
    auto fan(F&& f) && {
        return make_branch(std::forward<F>(f));
    }
};

// Helper to build a bound split holder
template <class Chain, class... Args>
auto split_bind(Chain&& c, Args&&... args) {
    return split_holder_bound<std::decay_t<Chain>, std::decay_t<Args>...>{
        std::forward<Chain>(c), std::forward<Args>(args)...};
}

} // namespace CHAIN_VERSION_NAMESPACE()
} // namespace chain

#endif
