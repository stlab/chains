
#include <catch2/catch_test_macros.hpp>

#include <chains/tuple.hpp>

#include <tuple>
#include <utility>

#include <exception>

// temporary
#include <any>
#include <stlab/concurrency/await.hpp>
#include <stlab/concurrency/future.hpp>

#define STLAB_FWD(x) std::forward<decltype(x)>(x)

/*

If exception inside of a segment _apply_ function throws an exception then the exception must be
set on the receiver.

*/

namespace chains::inline v1 {

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
        return tuple_compose(std::move(_functions))(std::forward<Args>(args)...);
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
    auto append(F&& f) && {
        return chains::segment{std::move(_apply), std::tuple_cat(std::move(_functions),
                                                                 std::tuple{std::forward<F>(f)})};
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
            [_f = tuple_compose(std::move(_functions)),
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

namespace detail {

/// Apply a recursive lambda to each element in the tuple-like Segments.
template <class Fold, class Segments>
constexpr auto fold_over(Fold fold, Segments&& segments) {
    return std::apply([fold](auto&&... links) { return fold(fold, STLAB_FWD(links)...); },
                      STLAB_FWD(segments));
}

} // namespace detail

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

    /// Return a lambda with the signature of
    /// head( tail<n>( tail<1>( tail<0>( auto&& args... ) ) ) )
    /// for computing the result type of this chain.
    static consteval auto result_type_helper(Tail&& tail, segment<Applicator, Fs...>&& head) {
        return detail::fold_over(
            [](auto fold, auto&& first, auto&&... rest) {
                if constexpr (sizeof...(rest) == 0) {
                    return [_segment = STLAB_FWD(first)](auto&&... args) mutable {
                        return std::move(_segment).result_type_helper(STLAB_FWD(args)...);
                    };
                } else {
                    return [_segment = STLAB_FWD(first).append(fold(fold, STLAB_FWD(rest)...))](
                               auto&&... args) mutable {
                        return std::move(_segment).result_type_helper(STLAB_FWD(args)...);
                    };
                }
            },
            std::tuple_cat(std::move(tail), std::tuple{std::move(head)}));
    }

    template <class R>
    auto expand(const R& receiver) && {
        return detail::fold_over(
            [receiver](auto fold, auto&& first, auto&&... rest) {
                if constexpr (sizeof...(rest) == 0) {
                    return [receiver,
                            _segment = STLAB_FWD(first).append(receiver)](auto&&... args) mutable {
                        return std::move(_segment).invoke(receiver, STLAB_FWD(args)...);
                    };
                } else {
                    return [receiver, _segment = STLAB_FWD(first).append(
                                          fold(fold, STLAB_FWD(rest)...))](auto&&... args) mutable {
                        return std::move(_segment).invoke(receiver, STLAB_FWD(args)...);
                    };
                }
            },
            std::tuple_cat(std::move(_tail), std::tuple{std::move(_head)}));
    }

public:
    template <class... Args>
    using result_type = decltype(result_type_helper(
        std::declval<Tail>(), std::declval<segment<Applicator, Fs...>>())(std::declval<Args>()...));

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
        return chains::chain{std::move(_tail), std::move(_head).append(std::forward<F>(f))};
    }

    template <class I, class... Gs>
    auto append(segment<I, Gs...>&& head) && {
        return chains::chain{std::tuple_cat(std::move(_tail), std::make_tuple(std::move(_head))),
                             std::move(head)};
    }

    template <class Receiver, class... Args>
    void invoke(Receiver&& receiver, Args&&... args) && {
        std::move(*this).expand(std::forward<Receiver>(receiver))(std::forward<Args>(args)...);
    }

    template <class... Args>
    auto operator()(Args&&... args) && {
        using result_t = result_type<Args...>;
        auto [receiver, future] =
            stlab::package<result_t(result_t)>(stlab::immediate_executor, std::identity{});
        (void)std::move(*this).expand(receiver)(std::forward<Args>(args)...);
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

template <class Tail, class Applicator, class... Fs>
chain(Tail&& tail, segment<Applicator, Fs...>&& head) -> chain<Tail, Applicator, Fs...>;

template <class F, class Applicator, class... Fs>
inline auto operator|(segment<Applicator, Fs...>&& head, F&& f) {
    return chain{std::tuple<>{}, std::move(head).append(std::forward<F>(f))};
}

} // namespace chains::inline v1

//--------------------------------------------------------------------------------------------------

#include <stlab/concurrency/future.hpp>
#include <variant>

namespace chains::inline v1 {

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

// TODO: (sean-parent) - whould we make this pipeable?

template <class Chain, class... Args>
inline auto start(Chain&& chain, Args&&... args) {}

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

} // namespace chains::inline v1

//--------------------------------------------------------------------------------------------------

#include <iostream>
#include <stlab/concurrency/await.hpp>
#include <stlab/concurrency/default_executor.hpp>
#include <thread>

using namespace std;
using namespace chains;
using namespace stlab;

TEST_CASE("Initial draft", "[initial_draft]") {
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
