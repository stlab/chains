#ifndef CHAIN_TUPLE_HPP
#define CHAIN_TUPLE_HPP

#include <cstddef>     // std::size_t
#include <functional>  // std::invoke
#include <tuple>       // std::tuple, std::get, std::apply, std::tuple_size_v
#include <type_traits> // std::is_same_v, std::decay_t, std::is_void_v
#include <utility>     // std::forward, std::move, std::index_sequence, std::make_index_sequence
#include <variant>     // std::monostate

namespace chains::inline v0 {

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

/* Find largest prefix size (0..N) for which F is invocable */
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

/* Construct tuple tail starting at Offset (compile time) */
template <class Tuple, std::size_t Offset, std::size_t... Is>
constexpr auto tuple_tail_at(Tuple&& t, std::index_sequence<Is...>) {
    return std::tuple{std::move(std::get<Offset + Is>(t))...};
}

//--------------------------------------------------------------------------------------------------
/*
    tuple_consume:
    Invokes the given callable with the largest invocable prefix of the stored tuple.
    Returns pair<result, remaining_tuple>.
    - If no prefix is invocable, result is std::monostate and remaining_tuple is the original tuple.
    - If callable returns void, result is std::monostate.
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
            auto remaining = tuple_tail_at<tuple_t, consumed>(
                std::move(_values), std::make_index_sequence<N - consumed>{});
            return std::tuple_cat(std::make_tuple(std::move(result)), std::move(remaining));
        }
    };
}

template <std::size_t I, typename F, typename T>
constexpr auto calc_step(F& f, T t) {
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
        return calc_step<I + 1>(f, std::move(next));
    }
}

template <typename F, typename... Args>
constexpr auto calc(F f, Args&&... args) {
    return calc_step<0>(f, std::make_tuple(std::forward<Args>(args)...));
}

template <class... Fs>
constexpr auto tuple_compose_greedy(std::tuple<Fs...>&& sequence) {
    return [_sequence = std::move(sequence)]<typename... Args>(Args&&... args) mutable {
        return calc(std::move(_sequence), std::forward<Args>(args)...);
    };
}

//--------------------------------------------------------------------------------------------------

} // namespace chains::inline v0

//--------------------------------------------------------------------------------------------------

#endif // CHAIN_TUPLE_HPP
