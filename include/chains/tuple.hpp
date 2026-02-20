/*
    Copyright 2026 Adobe
    Distributed under the Boost Software License, Version 1.0.
    (See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
*/

#ifndef CHAIN_TUPLE_HPP
#define CHAIN_TUPLE_HPP

#include <chains/config.hpp>

#include <cstddef>     // std::size_t
#include <functional>  // std::invoke
#include <tuple>       // std::tuple, std::get, std::apply, std::tuple_size_v
#include <type_traits> // std::is_same_v, std::decay_t, std::is_void_v
#include <utility>     // std::forward, std::move, std::index_sequence, std::make_index_sequence
#include <variant>     // std::monostate

namespace chains::inline CHAINS_VERSION_NAMESPACE() {

//--------------------------------------------------------------------------------------------------

namespace detail {

/* Map void return to std::monostate */
template <class F>
auto void_to_monostate(F& f) {
    return [&_f = f]<typename... Args>(Args&&... args) mutable {
        if constexpr (std::is_same_v<decltype(std::move(_f)(std::forward<Args>(args)...)), void>) {
            std::move(_f)(std::forward<Args>(args)...);
            return std::monostate{};
        } else {
            return std::move(_f)(std::forward<Args>(args)...);
        }
    };
}

template <class T>
struct tuple_pipeable {
    T _value;
    explicit tuple_pipeable(T&& a) : _value{std::move(a)} {}
};

template <class T, class F>
auto operator|(tuple_pipeable<T>&& p, F& f) {
    return tuple_pipeable{void_to_monostate(f)(std::move(p._value))};
}

/* Check if F is invocable with first K elements of tuple T */
template <class F, class T, std::size_t... Is>
constexpr auto invocable_with_prefix(std::index_sequence<Is...>) {
    return requires(F&& f, T&& tup) { std::invoke(f, std::move(std::get<Is>(tup))...); };
}

//--------------------------------------------------------------------------------------------------

/* Find the largest prefix size (0..N) for which F is invocable */
template <class F, class T, std::size_t N>
struct find_max_prefix {
    static constexpr std::size_t value =
        invocable_with_prefix<F, T>(std::make_index_sequence<N>{}) ?
            N :
            find_max_prefix<F, T, N - 1>::value;
};

template <class F, class T>
struct find_max_prefix<F, T, 0> {
    static constexpr std::size_t value = 0;
};

/* Invoke F with first K elements of tuple t (K known at compile time) */
template <std::size_t K, class F, class Tuple>
constexpr auto invoke_prefix(F&& f, Tuple&& t) {
    if constexpr (K == 0) {
        if constexpr (requires(F&& f2) { std::invoke(f2); }) {
            if constexpr (std::is_void_v<decltype(std::invoke(f))>) {
                std::invoke(f);
                return std::monostate{};
            } else {
                return std::invoke(f);
            }
        } else {
            return std::monostate{};
        }
    } else {
        return [&]<std::size_t... Is>(std::index_sequence<Is...>) {
            if constexpr (std::is_void_v<decltype(std::invoke(f, std::move(std::get<Is>(t))...))>) {
                std::invoke(f, std::move(std::get<Is>(t))...);
                return std::monostate{};
            } else {
                return std::invoke(f, std::move(std::get<Is>(t))...);
            }
        }(std::make_index_sequence<K>{});
    }
}

template <std::size_t Offset, class Tuple, std::size_t... Is>
constexpr auto move_tuple_tail_at_impl(Tuple&& t, std::index_sequence<Is...>) {
    return std::tuple{std::move(std::get<Offset + Is>(t))...};
}

} // namespace detail

//--------------------------------------------------------------------------------------------------

template <class... Fs>
auto tuple_compose(std::tuple<Fs...>&& sequence) {
    return [_sequence = std::move(sequence)]<typename... Args>(Args&&... args) mutable {
        return std::move(std::apply(
                             [_args = std::forward_as_tuple(std::forward<Args>(args)...)](
                                 auto& first, auto&... functions) mutable {
                                 return (
                                     detail::tuple_pipeable{std::apply(first, std::move(_args))} |
                                     ... | functions);
                             },
                             _sequence)
                             ._value);
    };
}

//--------------------------------------------------------------------------------------------------

/*
 * Take the remainder of a given tuple starting at Offset
 */
template <std::size_t Offset, class Tuple>
constexpr auto move_tuple_tail_at(Tuple&& t) {
    return detail::move_tuple_tail_at_impl<Offset, Tuple>(
        std::move(t), std::make_index_sequence<std::tuple_size_v<Tuple> - Offset>{});
}

//--------------------------------------------------------------------------------------------------
/**
 *  tuple_consume:
 *  Invokes the given callable with the largest invocable prefix of the stored tuple.
 *  Returns pair<result, remaining_tuple>.
 *  - If no prefix is invocable, result is std::monostate and remaining_tuple is the original tuple.
 *  - If callable returns void, result is std::monostate.
 */
template <class Tuple>
constexpr auto tuple_consume(Tuple&& values) {
    return [_values = std::forward<Tuple>(values)]<typename F>(F&& f) mutable {
        using tuple_t = std::decay_t<Tuple>;
        constexpr std::size_t N = std::tuple_size_v<tuple_t>;

        constexpr std::size_t consumed = detail::find_max_prefix<F, tuple_t, N>::value;
        auto result = detail::invoke_prefix<consumed>(std::forward<F>(f), _values);

        if constexpr (consumed == 0) {
            // Remaining is original tuple (no elements consumed)
            return std::tuple_cat(std::make_tuple(std::move(result)), std::move(_values));
        } else {
            auto remaining = move_tuple_tail_at<consumed, tuple_t>(std::move(_values));
            return std::tuple_cat(std::make_tuple(std::move(result)), std::move(remaining));
        }
    };
}

//--------------------------------------------------------------------------------------------------

namespace detail {
template <std::size_t I, typename F, typename T>
constexpr auto interpret_impl_step(F& f, T t) {
    if constexpr (I == std::tuple_size_v<F>) {
        // Base case: we finished applying all functions.
        // If there are no remaining tuple elements, return std::monostate
        // (mirrors void -> monostate mapping elsewhere).
        if constexpr (std::tuple_size_v<T> == 0) {
            return std::monostate{};
        } else {
            return std::get<0>(std::move(t));
        }
    } else {
        auto&& fn = std::get<I>(f);
        auto next = chains::tuple_consume(std::move(t))(fn);
        return interpret_impl_step<I + 1>(f, std::move(next));
    }
}

template <typename F, typename... Args>
constexpr auto interpret_impl(F f, Args&&... args) {
    return interpret_impl_step<0>(f, std::tuple{std::forward<Args>(args)...});
}
} // namespace detail

//--------------------------------------------------------------------------------------------------

/**
 * This function returns a callable object that works in a Forth like manner on all given arguments.
 * Interim results and not needed initial arguments will be pushed on an internal stack and popped
 * while evaluating the next function.
 * No allocations are made during the calculation, because the stack is evaluated at compile time.
 * The callable objects can have 0..n arguments and may even return void.
 * Each callable object tries pop greedy as many arguments as possible from the stack. So if a
 * callable object has several overloaded call operators that match the next set of values on the
 * stack, then the one with the maximum matching arguments is chosen.
 * If the last callable object is of result type void, then the result of the overall interpret(...)
 * call is an object of type std::monostate.
 *
 * It evaluates a sequence of functions, e.g. h(g(f(x), y)) with
 *          f = [](auto a) { return b; }
 *          g = [](auto b, auto c) { return d; }
 *          h = [](auto d) { return e; }
 *
 *          interpret(std::make_tuple(f, g, h))(a, b);
 *
 * @tparam Fs A list of callable types
 * @param sequence A tuple of callables that can consume in a Forth like manner all values that
 *                 passed as arguments to the resulting callable object
 * @return A callable object that can consume all given arguments.
 */
template <class... Fs>
constexpr auto interpret(std::tuple<Fs...>&& sequence) {
    return [_sequence = std::move(sequence)]<typename... Args>(Args&&... args) mutable {
        return detail::interpret_impl(std::move(_sequence), std::forward<Args>(args)...);
    };
}

} // namespace chains::inline CHAINS_VERSION_NAMESPACE()

//--------------------------------------------------------------------------------------------------

#endif // CHAIN_TUPLE_HPP
