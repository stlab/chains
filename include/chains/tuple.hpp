#include <tuple>
#include <utility>

#ifndef CHAIN_TUPLE_HPP

namespace chain::inline v0 {

namespace detail {

/*
    Operators for fold expression for sequential execution. Simpler way?
*/

template <class F>
inline auto void_to_monostate(F& f) {
    return [&_f = f](auto&&... args) mutable {
        if constexpr (std::is_same_v<decltype(std::move(_f)(std::forward<decltype(args)>(args)...)),
                                     void>) {
            std::move(_f)(std::forward<decltype(args)>(args)...);
            return std::monostate{};
        } else {
            return std::move(_f)(std::forward<decltype(args)>(args)...);
        }
    };
}

template <class T>
struct tuple_pipeable {
    T _value;
    tuple_pipeable(T&& a) : _value{std::move(a)} {}
};

template <class T, class F>
auto operator|(tuple_pipeable<T>&& p, F& f) {
    return tuple_pipeable{void_to_monostate(f)(std::move(p._value))};
}

/* REVISIT (sparent) : how to forward a value through `just`? */

template <class T>
struct just_ref {
    T& _value;
    just_ref(T& a) : _value{a} {}
};

template <class T, class F>
auto operator|(just_ref<T>&& p, F&& f) {
    return tuple_pipeable{std::apply(std::forward<F>(f), std::move(p._value))};
}

} // namespace detail

//--------------------------------------------------------------------------------------------------
template <class... Fs>
auto tuple_compose(std::tuple<Fs...>&& sequence) {
    return [_sequence = std::move(sequence)](auto&&... args) mutable {
        return std::move(std::apply(
                             [_args = std::forward_as_tuple(std::forward<decltype(args)>(args)...)](
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

} // namespace chain::inline v0

//--------------------------------------------------------------------------------------------------

#endif // CHAIN_TUPLE_HPP
