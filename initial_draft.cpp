#include <tuple>
#include <utility>

#include <exception>

// temporary
#include <any>
#include <stlab/concurrency/await.hpp>
#include <stlab/concurrency/future.hpp>

/*

If exception inside of a segment _apply_ function throws an exception then the exception must be
set on the receiver.

*/

namespace stlab::inline v1 {

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
struct pipeable;

template <class T>
struct pipeable {
    T _value;
    pipeable(T&& a) : _value{std::move(a)} {}
};

template <class T, class F>
auto operator|(pipeable<T>&& p, F& f) {
    return pipeable{void_to_monostate(f)(std::move(p._value))};
}

/* REVISIT (sparent) : how to forward a value through `just`? */

template <class T>
struct just_ref {
    T& _value;
    just_ref(T& a) : _value{a} {}
};

template <class T, class F>
auto operator|(just_ref<T>&& p, F&& f) {
    return pipeable{std::apply(std::forward<F>(f), std::move(p._value))};
}

} // namespace detail

template <class... Fs>
auto compose_tuple(std::tuple<Fs...>&& sequence) {
    return [_sequence = std::move(sequence)](auto&&... args) mutable {
        return std::move(std::apply(
                             [_args = std::forward_as_tuple(std::forward<decltype(args)>(args)...)](
                                 auto&... functions) mutable {
                                 return (detail::just_ref{_args} | ... | functions);
                             },
                             _sequence)
                             ._value);
    };
}

/*
segment is invoked with a receiver -



*/

template <class Applicator, class... Fs>
class segment {
    std::tuple<Fs...> _functions;
    Applicator _apply;

public:
    template <class... Args>
    auto result_type_helper(Args&&... args) && {
        return compose_tuple(std::move(_functions))(std::forward<Args>(args)...);
    }

    explicit segment(Applicator&& apply, std::tuple<Fs...>&& functions)
        : _functions{std::move(functions)}, _apply{std::move(apply)} {}
    explicit segment(Applicator&& apply, Fs&&... functions)
        : _functions{std::move(functions)...}, _apply{std::move(apply)} {}

    /*
        The basic operations should follow those from C++ lambdas, for now default everything.
        and see if the compiler gets it correct.
    */
    explicit segment(const segment&) = default;
    segment(segment&&) noexcept = default;
    segment& operator=(const segment&) = default;
    segment& operator=(segment&&) noexcept = default;

    template <class F>
    auto append(F&& f) && -> segment<Applicator, Fs..., std::decay_t<F>> {
        return segment<Applicator, Fs..., std::decay_t<F>>{
            std::move(_apply),
            std::tuple_cat(std::move(_functions), std::tuple{std::forward<F>(f)})};
    }

#if 0
    template <class... Args>
    auto operator()(Args&&... args) && /* const/non-const version? - noexcept(...) */ {
        return std::move(_apply)(compose_tuple(std::move(_functions)), std::forward<Args>(args)...);
    }
#endif
    /*
        The apply function for a segment always returns void.

        Invoke will check the receiver for cancelation -
        If not conceled, apply(segement), cancelation is checked before execution of the segment
        and any exception during the segment is propogated to the receiever.
    */

    template <class R, class... Args>
    void invoke(const R& receiver, Args&&... args) && {
        if (receiver.canceled()) return;

        std::move(_apply)(
            [_f = compose_tuple(std::move(_functions)),
             _receiver = receiver](auto&&... args) mutable noexcept {
                if (_receiver.canceled()) return;
                try {
                    std::move(_f)(std::forward<decltype(args)>(args)...);
                } catch (...) {
                    _receiver.set_exception(std::current_exception());
                }
            },
            std::forward<Args>(args)...);
    }
};

template <class Segment, class... Args>
using segment_result_type =
    decltype(std::declval<Segment>().result_type_helper(std::declval<Args>()...));

/*
    simplify this code by handing the multi-argument case earlier (somehow).
*/

template <class Tail, class Applicator, class... Fs>
class chain {
    Tail _tail;
    segment<Applicator, Fs...> _head;

    template <class Index>
    auto result_type_helper() && {
        if constexpr (Index::value == std::tuple_size_v<Tail>) {
            return [_segment = std::move(_head)](auto&&... args) mutable {
                return std::move(_segment).result_type_helper(
                    std::forward<decltype(args)>(args)...);
            };
        } else {
            return
                [_segment =
                     std::move(std::get<Index::value>(_tail))
                         .append(std::move(*this)
                                     .template result_type_helper<
                                         std::integral_constant<std::size_t, Index::value + 1>>())](
                    auto&&... args) mutable {
                    return std::move(_segment).result_type_helper(
                        std::forward<decltype(args)>(args)...);
                };
        }
    }

    template <class Index, class R>
    auto expand(const R& receiver) && {
        if constexpr (Index::value == std::tuple_size_v<Tail>) {
            return [_segment = std::move(_head).append(receiver),
                    _receiver = receiver](auto&&... args) mutable {
                return std::move(_segment).invoke(_receiver, std::forward<decltype(args)>(args)...);
            };
        } else {
            return [_segment =
                        std::move(std::get<Index::value>(_tail))
                            .append(std::move(*this)
                                        .template expand<
                                            std::integral_constant<std::size_t, Index::value + 1>>(
                                            receiver)),
                    _receiver = receiver](auto&&... args) mutable {
                return std::move(_segment).invoke(_receiver, std::forward<decltype(args)>(args)...);
            };
        }
    }

public:
    explicit chain(Tail&& tail, segment<Applicator, Fs...>&& head)
        : _tail{std::move(tail)}, _head{std::move(head)} {}

    /*
        The basic operations should follow those from C++ lambdas, for now default everything.
        and see if the compiler gets it correct.
    */

    explicit chain(const chain&) = default;
    chain(chain&&) noexcept = default;
    chain& operator=(const chain&) = default;
    chain& operator=(chain&&) noexcept = default;

    // append function to the last sequence
    template <class F>
    auto append(F&& f) && {
        return chain<Tail, Applicator, Fs..., F>{std::move(_tail),
                                                 std::move(_head).append(std::forward<F>(f))};
    }

    template <class I, class... Gs>
    auto append(segment<I, Gs...>&& head) && {
        using tail_type =
            decltype(std::tuple_cat(std::move(_tail), std::make_tuple(std::move(_head))));
        return chain<tail_type, I, Gs...>{
            std::tuple_cat(std::move(_tail), std::make_tuple(std::move(_head))), std::move(head)};
    }

    template <class... Args>
    auto operator()(Args&&... args) && {
        using result_type =
            decltype(std::move(*this)
                         .template result_type_helper<std::integral_constant<std::size_t, 0>>()(
                             std::forward<Args>(args)...));
        auto [receiver, future] =
            stlab::package<result_type(result_type)>(stlab::immediate_executor, std::identity{});
        (void)std::move(*this).template expand<std::integral_constant<std::size_t, 0>>(receiver)(
            std::forward<Args>(args)...);
        return std::move(future);
    }

    template <class F>
    friend auto operator|(chain&& c, F&& f) {
        return std::move(c).append(std::forward<F>(f));
    }

    template <class I, class... Gs>
    friend auto operator|(chain&& c, segment<I, Gs...>&& head) {
        return std::move(c).append(std::move(head));
    }
};

template <class F, class Applicator, class... Fs>
inline auto operator|(segment<Applicator, Fs...>&& head, F&& f) {
    return chain{std::tuple<>{}, std::move(head).append(std::forward<F>(f))};
}

} // namespace stlab::inline v1

//--------------------------------------------------------------------------------------------------

#include <stlab/concurrency/future.hpp>
#include <variant>

namespace stlab::inline v1 {

#if 0
template <class E>
inline auto on(E&& executor) {
    return segment{[_executor = std::forward<E>(executor)](auto&& f, auto&&... args) mutable {
        return stlab::async(std::move(_executor), std::forward<decltype(f)>(f),
                            std::forward<decltype(args)>(args)...);
    }};
}
#endif

/*

Each segment invokes the next segment with result and returns void. Promise is bound to the
last item in the chain as a segment.

*/
template <class E>
inline auto on(E&& executor) {
    return segment{[_executor = std::forward<E>(executor)](auto&& f, auto&&... args) mutable {
        std::move(_executor)(
            [_f = std::forward<decltype(f)>(f),
             _args = std::tuple{std::forward<decltype(args)>(args)...}]() mutable noexcept {
                std::apply(std::move(_f), std::move(_args));
            });
        return std::monostate{};
    }};
}

#if 0


/*
    TODO: The ergonimics of chains are painful with three arguements. We could reduce to a single
    argument or move to a concept? Here I really want the forward reference to be an rvalue ref.

    The implementation of sync_wait is complicated by the fact that the promise is currently hard/
    wired into the chain. sync_wait needs to be able to invoke the promise/receiver - _then_ flag
    the condition that it is ready.
*/


template <class Chain>
inline auto sync_wait(Chain&& chain) {
    /*
        TODO: (sean-parent) - we should have an invoke awaiting parameterized on what we are waiting
        The implementation of which would be used in stlab::await() and used here. With this
        construct we don't spin up more than one thread (hmm, maybe we shouldn't?).
    */
    auto appended = std::forward<Chain>(chain) | [&]
    invoke_awaiting(
    );
}
#endif

#if 0
inline auto apply() {
    return segment{[](auto&& f, auto&&... args) {
        return std::forward<decltype(f)>(f)(std::forward<decltype(args)>(args)...);
    }};
}

template <class F>
inline auto then(F&& future) {
    return segment{[_future = std::forward<F>(future)](auto&& f) {
        return std::move(_future).then(std::forward<decltype(f)>(f));
    }};
}

#endif

} // namespace stlab::inline v1

//--------------------------------------------------------------------------------------------------

#include <iostream>
#include <stlab/concurrency/await.hpp>
#include <stlab/concurrency/default_executor.hpp>
#include <thread>

using namespace std;
using namespace stlab;

int main() {
    auto a0 = on(default_executor) | [] {
        cout << "Hello from thread: " << std::this_thread::get_id() << "\n";
        return 42;
    };

    auto a1 = std::move(a0) | on(default_executor) | [](int x) {
        cout << "received: " << x << " on thread: " << std::this_thread::get_id() << "\n";
        // throw std::runtime_error("test-exception");
        return "forwarding: " + std::to_string(x + 1);
    };

    cout << "Main thread: " << std::this_thread::get_id() << "\n";
    cout << "Ready to go async!\n";

#if 0
    auto a2 = then(std::move(a1)()) | [](std::string s){
        cout << s << "<-- \n";
        return 0;
    };
#endif

#if 0
    {
    auto f = std::move(a1)(); // start and cancel.
    std::this_thread::sleep_for(1ns);
    }
#endif

#if 0
    // TODO: (sean-parent) await on a chain can be optimized.

    try {
        std::cout << any_cast<std::string>(await(std::move(a1)())) << "\n";
    } catch(const std::exception& error) {
        std::cout << "exception: " << error.what() << "\n";
    }
#endif

    // std::this_thread::sleep_for(3s);

    std::cout << await(std::move(a1)()) << "\n";

    pre_exit();
}

#if 0

#include <thread>
#include <tuple>

#include <stlab/concurrency/await.hpp>
#include <stlab/concurrency/default_executor.hpp>
#include <stlab/concurrency/future.hpp>

namespace stlab {namespace detail {

template <class T>
struct pipeable {
    T _value;
    pipeable(T&& a) : _value{std::move(a)} { }
};

template <class T, class F>
auto operator|(pipeable<T>&& p, F&& f) {
    return pipeable{f(std::move(p._value))};
}

template <class T>
struct just {
    T& _value;
    just(T& a) : _value{a} { }
};

template <class T, class F>
auto operator|(just<T>&& p, F&& f) {
    return pipeable{std::apply(std::forward<F>(f), p._value)};
}

}

/*
    Operations on a chain:
    chain(f) // a chain can be created from a function
    chain(f, applyr) // optionally, an applyr can be provided
    chain | f -> chain
    compose(chain, chain) -> chain
    // return applyr(op, args...) where op is a function that represents the composed functions in the chain.
    operator()(args...) -> value
*/

template <class I, class... Fs>
class chain
{
    I _applyr;
    std::tuple<Fs...> _functions;

public:
    // REVISIT: Should `forward_as_tuple(args...)` be `forward_as_tuple(std::forward<decltype(args)>(args)...)`?
    static auto default_applyr = [](chain& c, auto&&... args) {
        return std::apply([_args = std::forward_as_tuple(args...)](auto&&... functions) mutable {
            return (detail::just{_args} | ... | functions);
        }, c._functions)._value;
    };

    chain(I&& applyr, Fs&&... functions) : _applyr{std::forward<I>(applyr)}, _functions{std::make_tuple(std::forward<Fs>(functions)...)} {}
    explicit chain(Fs&&... functions) : chain{default_applyr, std::forward<Fs>(functions)...} {}

    template <class J>
    chain(I&& applyr, chain<J, Fs...>&& c): _applyr{std::forward<I>(applyr)}, _functions{std::move(c._functions)} {}

    template <class... Args>
    auto operator()(Args&&... args) {
        _applyr(*this, std::forward<Args>(args)...);
    }

    chain(const chain&) = delete;
    chain& operator=(const chain&) = delete;

    chain(chain&&) noexcept = default;
    chain& operator=(chain&&) noexcept = default;
};

template <class E, class I, class... Fs>
auto resume_on(chain<I, Fs...>&& c, E&& executor) {
    return chain{[_executor = std::forward<E>(executor), c = std::move(c)](auto& ch, auto&&... args) mutable {
        return stlab::async(std::move(_executor), [_chain = chain{default_applyr, std::move(ch)}]() mutable {
            return _chain();
        });
    }, std::identity{}};
}

#if 0

namespace detail {

template <class T>
struct pipeable {
    T _value;
    pipeable(T&& a) : _value{std::move(a)} { }
};

template <class T, class F>
auto operator|(pipeable<T>&& p, F&& f) {
    return pipeable{f(std::move(p._value))};
}

template <class T>
struct just {
    T& _value;

    just(T& a) : _value{a} { }
};

template <class T, class F>
auto operator|(just<T>&& p, F&& f) {
    return pipeable{std::apply(std::forward<F>(f), p._value)};
}

}


template <class... Fs>
struct chain {
    static constexpr bool chainable{true};
    std::tuple<Fs...> _functions;

    template <class F>
    using next_type = chain<Fs..., F>;

    template <class C>
    chain(C&&, std::tuple<Fs...>&& t) : _functions{std::move(t)} { }

    chain(std::tuple<Fs...>&& t) : _functions{std::move(t)} { }

    // chain(Fs&&... args) : _functions{std::forward<Fs>(args)...} { }

    template <class... Args>
    auto operator()(Args&&... args) {
        return std::apply([_args = std::forward_as_tuple(args...)](auto&&... functions) mutable {
            return (detail::just{_args} | ... | functions);
        }, _functions)._value;
    }
};

// REVISIT : This is binding the executer and arguments to a chainable. I need to factor out
// the bind/wrap operation as a build block for composing algorithms.

template <class E, class O, class... Fs>
struct spawner : chain<Fs...> {
    E _executor;
    O _op;

    template<typename F>
    using next_type = spawner<E, O, Fs..., F>;

    template <class S>
    spawner(S&& s, std::tuple<Fs...>&& t) : _executor{std::move(s._executor)}, _op{std::move(s._op)},
        chain<Fs...>{std::move(t)} { }

    spawner(E&& e, O&& op) : _executor{std::forward<E>(e)}, _op{std::forward<O>(op)}, chain<>{std::tuple{}} { }

    auto operator()() {
        return stlab::async(std::move(_executor), [_chain = chain{std::move(this->_functions)}, _op = std::move(_op)]() mutable {
            return _chain(_op());
        });
    }
};


template <class C, class F>
auto operator|(C&& c, F&& f) requires C::chainable {
    auto functions{std::tuple_cat(std::move(c._functions), std::make_tuple(std::forward<F>(f)))};
    using next_type = C::template next_type<std::decay_t<F>>;
    return next_type{std::move(c), std::move(functions)};
}

/*
    return a chainable (void)->future<T> that will schedule the task and any chained operations on
    the executer when apply. The `T` is the result type of the last chained operation.
*/

template <typename E, typename F>
auto schedule_on(E executor, F&& f) {
    return spawner<std::decay_t<E>, std::decay_t<F>>{std::move(executor), std::forward<F>(f)};
}

#endif

}

#include <iostream>

using namespace std;
using namespace stlab;

int main() {
#if 0
    stlab::chain c{[](int x) -> float { return x; }, [](float a){ return a + 0.5; }};
    auto c2{std::move(c) | [](float a){ return std::to_string(a + 2.0); }};
    std::cout << c2(42) << "\n";
#endif

    auto a0 = schedule_on(default_executor, []{
        cout << "Hello from thread: " << std::this_thread::get_id() << "\n";
        return 42;
    });

    auto a1 = std::move(a0) | [](int x){
        cout << "received: " << x << "\n";
        return "forwarding: " + std::to_string(x + 1);
    };

    cout << "Main thread: " << std::this_thread::get_id() << "\n";
    cout << "Ready to go async!\n";

    std::cout << await(a1()) << "\n";

    pre_exit();
}
#endif
