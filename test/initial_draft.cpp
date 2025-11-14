
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


template <class Receiver>
struct receiver_ref {
    Receiver* _receiver;
    void operator()(auto&&... args) {
        _receiver->operator()(std::forward<decltype(args)>(args)...);
    }
    void set_exception(std::exception_ptr p) { _receiver->set_exception(p); }
    bool canceled() const { return _receiver->canceled(); }
};

template <class T>
struct type {};

template <class Injects, class Applicator, class... Fs>
class segment;

#if 0
template <class Injects, class Applicator, class... Fs>
inline auto make_segment(Applicator&& apply, Fs&&... fs) {
    return segment<Injects, std::decay_t<Applicator>, std::decay_t<Fs>...>{
        std::forward<Applicator>(apply), std::forward<Fs>(fs)...};
}
#endif

template <class Injects, class Applicator, class... Fs>
class segment {
    std::tuple<Fs...> _functions;
    Applicator _apply;

public:
    /*
            An apply operation may inject additional arguments into the segment. The plan is that the
        receiver will get sent to apply and this is how cancellation tokens can be injected into an
        operation. Something like `with_cancellation`.

        This feature is also used for the `then` operation where the resolve future is injected into
        the segment.
    */
    template <class... Args>
    auto result_type_helper(Args&&... args) && {
        return tuple_compose_greedy(std::move(_functions))(std::forward<Args>(args)...);
    }

    explicit segment(type<Injects>, Applicator&& apply, std::tuple<Fs...>&& functions)
        : _functions{std::move(functions)}, _apply{std::move(apply)} {}
    explicit segment(type<Injects>, Applicator&& apply, Fs&&... functions)
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
        return chains::segment{
            type<Injects>{}, std::move(_apply),
            std::tuple_cat(std::move(_functions), std::make_tuple(std::forward<F>(f)))};
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
    auto invoke(R&& receiver, Args&&... args) && {
        // TODO: must handle this cancel prior to invoking the segment.
        // if (receiver.canceled()) return;
        return std::move(_apply)(
            [_f = tuple_compose_greedy(std::move(_functions)),
             _receiver = std::forward<R>(receiver)](auto&&... args) mutable noexcept {
                if (_receiver->canceled()) return;
                try {
                    std::move(_f)(std::forward<decltype(args)>(args)...);
                } catch (...) {
                    _receiver->set_exception(std::current_exception());
                }
            },
            std::forward<Args>(args)...);
    }
};

namespace detail {

/// Apply a recursive lambda to each element in the tuple-like Segments.
template <class Fold, class Segments>
constexpr auto fold_over(Fold fold, Segments&& segments) {
    return std::apply([fold](auto&&... links) mutable { return fold(fold, STLAB_FWD(links)...); },
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
            []([[maybe_unused]]auto fold, auto&& first, auto&&... rest) {
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
            std::tuple_cat(STLAB_FWD(tail), std::tuple{STLAB_FWD(head)}));
    }

    template <class R>
    auto expand(R&& receiver) && {
        return detail::fold_over(
            [_receiver = std::forward<R>(receiver)]([[maybe_unused]] auto fold, auto&& first,
                                                    auto&&... rest) mutable {
                if constexpr (sizeof...(rest) == 0) {
                    return [_receiver, _segment = STLAB_FWD(first).append([_receiver](auto&& val) {
                        _receiver->operator()(std::forward<decltype(val)>(val));
                    })](auto&&... args) mutable {
                        return std::move(_segment).invoke(_receiver, STLAB_FWD(args)...);
                    };
                } else {
                    return [_receiver, _segment = STLAB_FWD(first).append(fold(
                                           fold, STLAB_FWD(rest)...))](auto&&... args) mutable {
                        return std::move(_segment).invoke(_receiver, STLAB_FWD(args)...);
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

    explicit chain(const chain&) = default;
    chain(chain&&) noexcept = default;
    chain& operator=(const chain&) = default;
    chain& operator=(chain&&) noexcept = default;

    // append function to the last sequence
    template <class F>
    auto append(F&& f) && {
        return chains::chain{std::move(_tail), std::move(_head).append(std::forward<F>(f))};
    }

    template <class Jnjects, class I, class... Gs>
    auto append(segment<Jnjects, I, Gs...>&& head) && {
        return chains::chain{std::tuple_cat(std::move(_tail), std::make_tuple(std::move(_head))),
                             std::move(head)};
    }

    template <class Receiver, class... Args>
    auto invoke(Receiver&& receiver, Args&&... args) && {
        return std::move(*this).expand(std::forward<Receiver>(receiver))(
            std::forward<Args>(args)...);
    }

#if 0
    template <class... Args>
    [[deprecated]] auto operator()(Args&&... args) && {
        using result_t = result_type<Args...>;
        auto [receiver, future] =
            stlab::package<result_t(result_t)>(stlab::immediate_executor, std::identity{});
        invoke(std::move(receiver), std::forward<Args>(args)...);
        return std::move(future);
    }
#endif

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
inline auto operator|(segment<Injects, Applicator, Fs...>&& head, F&& f) {
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
    return segment{
        type<void>{}, [_executor = std::forward<E>(executor)](auto&& f, auto&&... args) mutable {
            std::move(_executor)(
                [_f = std::forward<decltype(f)>(f),
                 _args = std::tuple{std::forward<decltype(args)>(args)...}]() mutable noexcept {
                    std::apply(std::move(_f), std::move(_args));
                });
            // return std::monostate{};
        }};
}

/*
    The `then` algorithm takes a future and returns a segment (chain) that will schedule the
    segment as a continuation of the future.

    The segment returns void so the future is (kind of) detached - but this should be done
   without the overhead of a future::detach.

    How is cancellation handled here? Let's say we have this:

    `auto f = start(then(future));`

    And we destruct f. We need to _delete_ the (detached) future. Where is this held? f is only
   holding the promise.
*/

template <class F>
inline auto then(F&& future) {
    return chain{std::tuple<>{},
                 segment{type<typename std::decay_t<F>::result_type>{},
                         [_future = std::forward<F>(future)](auto&& f) mutable {
                             return std::move(_future).then(std::forward<decltype(f)>(f));
                         }}};
}

// TODO: (sean-parent) - should we make this pipeable?
// TODO: (sean-parent) - fix case where invoke_t is void.

template <class Chain, class... Args>
inline auto start(Chain&& chain, Args&&... args) {
    using result_t = typename Chain::template result_type<Args...>;

    using package_task_t = decltype(stlab::package<result_t(result_t)>(
                                        stlab::immediate_executor,
                                        [](auto&& v) { return std::forward<decltype(v)>(v); }).first);

    auto shared = std::shared_ptr<package_task_t>();

    // Build the receiver and future first.
    auto [receiver, future] = stlab::package<result_t(result_t)>(
        stlab::immediate_executor, [_shared = shared](auto&& v) { return std::forward<decltype(v)>(v); });

    // Promote receiver to shared_ptr to extend lifetime beyond this scope.
    shared = std::make_shared<package_task_t>(std::move(receiver));

    // Recompute invoke_t based on passing the shared_ptr (pointer semantics).
    using invoke_t =
        decltype(std::forward<Chain>(chain).invoke(shared, std::forward<Args>(args)...));

    if constexpr (std::is_void_v<invoke_t>) {
        // Just invoke; lifetime of receiver is now owned by captures inside the async chain.
        std::forward<Chain>(chain).invoke(shared, std::forward<Args>(args)...);
    } else {
        // Keep any handle the chain returns (e.g. continuation future or cancellation handle).
        auto hold = std::forward<Chain>(chain).invoke(shared, std::forward<Args>(args)...);
        (void)hold; // store or return if you later need it
    }
    return std::move(future);
}


template <class Chain, class... Args>
inline auto sync_wait(Chain&& chain, Args&&... args) {
    using result_t = Chain::template result_type<Args...>;

    struct receiver_t {
        std::optional<result_t> result;
        std::exception_ptr error{nullptr};
        std::mutex m;
        std::condition_variable cv;

        void operator()(result_t&& value) {
            {
                std::lock_guard<std::mutex> lock(m);
                result = std::move(value);
            }
            cv.notify_one();
        }

        void set_exception(std::exception_ptr p) {
            {
                std::lock_guard<std::mutex> lock(m);
                error = p;
            }
            cv.notify_one();
        }

        bool canceled() const { return false; }
    } receiver;

    /*
        REVISIT: (sean-parent) - chain invoke doesn't work with std::ref(receiver). We should
       fix that but for now create a receiver-ref.
    */

    auto hold = std::forward<Chain>(chain).invoke(receiver_ref<receiver_t>{&receiver},
                                                  std::forward<Args>(args)...);

    std::unique_lock<std::mutex> lock(receiver.m);
    receiver.cv.wait(lock, [&] { return receiver.result.has_value() || receiver.error; });

    if (receiver.error) {
        std::rethrow_exception(receiver.error);
    }
    return *receiver.result;
}

/*
    TODO: The ergonomics of chains are painful with three arguments. We could reduce to a
   single argument or move to a concept? Here I really want the forward reference to be an
   rvalue ref.

    The implementation of sync_wait is complicated by the fact that the promise is currently
   hard/ wired into the chain. sync_wait needs to be able to invoke the promise/receiver -
   _then_ flag the condition that it is ready.
*/

} // namespace chains::inline v1

//--------------------------------------------------------------------------------------------------




#include <iostream>
#include <stlab/concurrency/await.hpp>
#include <stlab/concurrency/default_executor.hpp>
#include <thread>

using namespace std;
using namespace chains;
using namespace stlab;

// Cancellation example

struct cancellation_source {
    struct state {
        std::atomic_bool canceled{false};
    };
    std::shared_ptr<state> _s = std::make_shared<state>();
    void cancel() const { _s->canceled.store(true, std::memory_order_relaxed); }
};

struct cancellation_token {
    std::shared_ptr<cancellation_source::state> _s;
    bool canceled() const { return _s->canceled.load(std::memory_order_relaxed); }
};

// Segment that injects a cancellation_token (Injects != void)
inline auto with_cancellation(cancellation_source src) {
    return chains::segment{chains::type<cancellation_token>{},
                           [_src = std::move(src)](auto&& f, auto&&... args) mutable {
                               // Create token and forward it as first argument
                               cancellation_token tok{_src._s};
                               std::forward<decltype(f)>(f)(tok,
                                                            std::forward<decltype(args)>(args)...);
                           }};
}

// executor variant that also injects the token and schedules asynchronously
template <class E>
auto on_with_cancellation(E&& executor, cancellation_source source) {
    return chains::segment{
        chains::type<cancellation_token>{},
        [_executor = std::forward<E>(executor), _source = std::move(source)](auto&& f,
                                                                       auto&&... args) mutable {
            cancellation_token token{_source._s};
            std::move(_executor)(
                [_f = std::forward<decltype(f)>(f), _token = token,
                 _args = std::tuple{std::forward<decltype(args)>(args)...}]() mutable noexcept {
                    std::apply(
                        [&_f, &_token](auto&&... as) {
                            std::forward<decltype(_f)>(_f)(_token, std::forward<decltype(as)>(as)...);
                        },
                        std::move(_args));
                });
        }};
}

TEST_CASE("Cancellation injection", "[initial_draft]") {
    cancellation_source src;

    // Build a chain where each function expects the token as first argument.
    // First function uses the token, returns an int.
    auto c = with_cancellation(src) | [](cancellation_token token, int x) {
        if (token.canceled()) return 0;
        return x * 2;
    } | [](int y) { return y + 10; }; // token only needed by first step

    auto f = start(std::move(c), 5);
    REQUIRE(f.get_ready() == 20); // (5*2)+10

    // Demonstrate cancel before start
    src.cancel();
    auto c2 = with_cancellation(src) | [](cancellation_token token, int x) {
        if (token.canceled()) return 0;
        return x * 3;
    };
    auto f2 = start(std::move(c2), 7);
    REQUIRE(f2.get_ready() == 0);
}

TEST_CASE("Initial draft", "[initial_draft]") {

    GIVEN("A tuple of mixed types") {
        auto oneInt2Int = [](int a) { return a * 2; };
        auto twoInt2Int = [](int a, int b) { return a + b; };
        auto void2Int = []() { return 42; };

        auto a0 = on(stlab::immediate_executor) | oneInt2Int | void2Int | twoInt2Int;

        auto f = start(std::move(a0), 2);
        REQUIRE(f.is_ready());
        auto val = f.get_ready();
        REQUIRE(46 == val);
    }


    auto a0 = on(default_executor) | [] {
        cout << "Hello from thread: " << std::this_thread::get_id() << "\n";
        return 42;
    };
    // std::cout << typeid(decltype(a0)::result_type<>).name() << "\n";
    //   auto future = start(std::move(a0));

    auto a1 = std::move(a0) | on(default_executor) | [](int x) {
        cout << "received: " << x << " on thread: " << std::this_thread::get_id() << "\n";
        // throw std::runtime_error("test-exception");
        return "forwarding: " + std::to_string(x + 1);
    };

    cout << "Main thread: " << std::this_thread::get_id() << "\n";
    cout << "Ready to go async!\n";

#if 0
    auto a2 = then(std::move(a1)) | [](std::string s) {
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

    // std::cout << await(start(std::move(a1))) << "\n";

    //auto future = start(std::move(a1));
    //auto a2 = then(future) | [](std::string s) { return s + "<-- \n"; };

    //std::cout << sync_wait(std::move(a2)) << "\n";

    pre_exit();
}
