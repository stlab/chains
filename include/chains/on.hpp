/*
Copyright 2026 Adobe
  Distributed under the Boost Software License, Version 1.0.
  (See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
*/

#ifndef CHAINS_ON_HPP
#define CHAINS_ON_HPP

#include <chains/config.hpp>
#include <chains/segment.hpp>

#include <atomic>
#include <memory>
#include <tuple>
#include <utility>

namespace chains {
inline namespace CHAINS_VERSION_NAMESPACE() {

/*

Each segment invokes the next segment with result and returns void. Promise is bound to the
last item in the chain as a segment.

*/
template <class E>
auto on(E&& executor) {
    return segment{type<void>{},
                   [_executor = std::forward<E>(executor)]<typename F, typename... Args>(
                       F&& f, Args&&... args) mutable {
                       std::move(_executor)(
                           [_f = std::forward<F>(f),
                            _args = std::tuple{std::forward<Args>(args)...}]() mutable noexcept {
                               std::apply(std::move(_f), std::move(_args));
                           });
                       // return std::monostate{};
                   }};
}

struct cancellation_source {
    struct state {
        std::atomic_bool canceled{false};
    };
    std::shared_ptr<state> _state = std::make_shared<state>();
    void cancel() const { _state->canceled.store(true, std::memory_order_relaxed); }
};

struct cancellation_token {
    std::shared_ptr<cancellation_source::state> _state;
    auto canceled() const { return _state->canceled.load(std::memory_order_relaxed); }
};

// Segment that injects a cancellation_token (Injects != void)
inline auto with_cancellation(cancellation_source src) {
    return segment{
        chains::type<cancellation_token>{},
        [_src = std::move(src)]<typename F, typename... Args>(F&& f, Args&&... args) mutable {
            // Create token and forward it as first argument
            cancellation_token token{_src._state};
            std::forward<F>(f)(token, std::forward<Args>(args)...);
        }};
}

// executor variant that also injects the token and schedules asynchronously
template <class E>
auto on_with_cancellation(E&& executor, cancellation_source source) {
    return chains::segment{
        chains::type<cancellation_token>{},
        [_executor = std::forward<E>(executor),
         _source = std::move(source)]<typename F, typename... Args>(F&& f, Args&&... args) mutable {
            cancellation_token token{_source._state};
            std::move(_executor)(
                [_f = std::forward<F>(f), _token = token,
                 _args = std::tuple{std::forward<Args>(args)...}]() mutable noexcept {
                    std::apply(
                        [&_f, &_token]<typename... As>(As&&... as) {
                            std::forward<decltype(_f)>(_f)(_token, std::forward<As>(as)...);
                        },
                        std::move(_args));
                });
        }};
}
} // namespace CHAINS_VERSION_NAMESPACE()
} // namespace chains

#endif