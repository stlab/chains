/*
    Copyright 2026 Adobe
    Distributed under the Boost Software License, Version 1.0.
    (See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
*/

#ifndef CHAINS_CHAINS_HPP
#define CHAINS_CHAINS_HPP

#include <chains/config.hpp>
#include <chains/segment.hpp>
#include <chains/tuple.hpp>

#include <exception>
#include <tuple>
#include <utility>

#define STLAB_FWD(x) std::forward<decltype(x)>(x)

/*

If exception inside a segment _apply_ function throws an exception then the exception must be
set on the receiver.

*/

namespace chains::inline CHAINS_VERSION_NAMESPACE() {

/*
segment is invoked with a receiver -
*/

template <class Receiver>
struct receiver_ref {
    Receiver* _receiver;
    auto operator()(auto&&... args) -> void {
        _receiver->operator()(std::forward<decltype(args)>(args)...);
    }
    auto set_exception(std::exception_ptr p) -> void { _receiver->set_exception(p); }
    [[nodiscard]] auto canceled() const -> bool { return _receiver->canceled(); }
};

namespace detail {

/// Apply a recursive lambda to each element in the tuple-like Segments.
template <class Fold, class Segments>
constexpr auto fold_over(Fold fold, Segments&& segments) {
    return std::apply(
        [fold]<typename... Links>(Links&&... links) mutable {
            return fold(fold, std::forward<Links>(links)...);
        },
        STLAB_FWD(segments));
}

} // namespace detail

/*
    simplify this code by handing the multi-argument case earlier (somehow).
*/

template <class Tail, class Injects, class Applicator, class... Fs>
class chain {
    Tail _tail;
    segment<Injects, Applicator, Fs...> _head;

    /// Return a lambda with the signature of
    /// head( tail<n>( tail<1>( tail<0>( auto&& args... ) ) ) )
    /// for computing the result type of this chain.
    static consteval auto result_type_helper(Tail&& tail,
                                             segment<Injects, Applicator, Fs...>&& head) {
        return detail::fold_over(
            []<typename Fold, typename First, typename... Rest>([[maybe_unused]] Fold fold,
                                                                First&& first, Rest&&... rest) {
                if constexpr (sizeof...(rest) == 0) {
                    return [_segment = std::forward<First>(first)]<typename... Args>(
                               Args&&... args) mutable {
                        return std::move(_segment).result_type_helper(std::forward<Args>(args)...);
                    };
                } else {
                    return [_segment = std::forward<First>(first).append(
                                fold(fold, std::forward<Rest>(rest)...))]<typename... Args>(
                               Args&&... args) mutable {
                        return std::move(_segment).result_type_helper(std::forward<Args>(args)...);
                    };
                }
            },
            std::tuple_cat(std::move(tail), std::tuple{std::move(head)}));
    }

    template <class R>
    auto expand(R&& receiver) && {
        return detail::fold_over(
            [_receiver =
                 std::forward<R>(receiver)]<typename Fold, typename First, typename... Rest>(
                [[maybe_unused]] Fold fold, First&& first, Rest&&... rest) mutable {
                if constexpr (sizeof...(rest) == 0) {
                    return [_receiver, _segment = std::forward<First>(first).append(
                                           [_receiver]<typename V>(V&& val) {
                                               _receiver->operator()(std::forward<V>(val));
                                           })]<typename... Args>(Args&&... args) mutable {
                        return std::move(_segment).invoke(_receiver, std::forward<Args>(args)...);
                    };
                } else {
                    return [_receiver, _segment = std::forward<First>(first).append(fold(
                                           fold, std::forward<Rest>(rest)...))]<typename... Args>(
                               Args&&... args) mutable {
                        return std::move(_segment).invoke(_receiver, std::forward<Args>(args)...);
                    };
                }
            },
            std::tuple_cat(std::move(_tail), std::tuple{std::move(_head)}));
    }

    template <class... Args>
    struct result_type_void_injects {
        using type = decltype(result_type_helper(
            std::declval<Tail>(),
            std::declval<segment<Injects, Applicator, Fs...>>())(std::declval<Args>()...));
    };

    template <class... Args>
    struct result_type_injects {
        using type = decltype(result_type_helper(
            std::declval<Tail>(), std::declval<segment<Injects, Applicator, Fs...>>())(
            std::declval<Injects>(), std::declval<Args>()...));
    };

public:
    template <class... Args>
    using result_type = std::conditional_t<std::is_same_v<Injects, void>,
                                           result_type_void_injects<Args...>,
                                           result_type_injects<Args...>>::type;

    explicit chain(Tail&& tail, segment<Injects, Applicator, Fs...>&& head)
        : _tail{std::move(tail)}, _head{std::move(head)} {}

    /*
        The basic operations should follow those from C++ lambdas, for now default everything.
        and see if the compiler gets it correct.
    */

    chain(const chain&) = default;
    chain(chain&&) noexcept = default;
    auto operator=(const chain&) -> chain& = default;
    auto operator=(chain&&) noexcept -> chain& = default;

    // append function to the last sequence
    template <class F>
    auto append(F&& f) && {
        return chains::chain{std::move(_tail), std::move(_head).append(std::forward<F>(f))};
    }

    template <class Jnjects, class I, class... Gs>
    auto append(segment<Jnjects, I, Gs...>&& head) && {
        return chains::chain{std::tuple_cat(std::move(_tail), std::tuple{std::move(_head)}),
                             std::move(head)};
    }

    template <class Receiver, class... Args>
    auto invoke(Receiver&& receiver, Args&&... args) && {
        return std::move(*this).expand(std::forward<Receiver>(receiver))(
            std::forward<Args>(args)...);
    }

    template <class F>
    friend auto operator|(chain&& c, F&& f) {
        return std::move(c).append(std::forward<F>(f));
    }

    template <class Jnjects, class I, class... Gs>
    friend auto operator|(chain&& c, segment<Jnjects, I, Gs...>&& head) {
        return std::move(c).append(std::move(head));
    }
};

template <class Tail, class Injects, class Applicator, class... Fs>
chain(Tail&& tail, segment<Injects, Applicator, Fs...>&& head)
    -> chain<Tail, Injects, Applicator, Fs...>;

template <class F, class Injects, class Applicator, class... Fs>
auto operator|(segment<Injects, Applicator, Fs...>&& head, F&& f) {
    return chain{std::tuple<>{}, std::move(head).append(std::forward<F>(f))};
}

} // namespace chains::inline CHAINS_VERSION_NAMESPACE()

#endif
