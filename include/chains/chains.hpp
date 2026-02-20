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

namespace chains {
inline namespace CHAINS_VERSION_NAMESPACE() {

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
    chain& operator=(const chain&) = default;
    chain& operator=(chain&&) noexcept = default;

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

} // namespace CHAINS_VERSION_NAMESPACE()

//--------------------------------------------------------------------------------------------------

#include <stlab/concurrency/future.hpp>
#include <variant>

namespace chains::inline v1 {} // namespace chains::inline v1

//--------------------------------------------------------------------------------------------------

#include <iostream>
#include <stlab/concurrency/default_executor.hpp>
#include <stlab/test/model.hpp>
#include <thread>

using namespace std;
using namespace chains;
using namespace stlab;

// Cancellation example

TEST_CASE("Cancellation injection", "[initial_draft]") {
    {
        cancellation_source src;

        // Build a chain where the first function expects the token as first argument.
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

    //{
    //    cancellation_source src;

    //    // Build a chain where each function expects the token as first argument.
    //    // First function uses the token, returns an int.
    //    auto c = with_cancellation(src) | [](cancellation_token token, int x) {
    //        if (token.canceled()) return 0;
    //        return x * 2;
    //    } | [](int y) { return y + 10; }; // token only needed by first step

    //    auto f = start(std::move(c), 5);
    //    REQUIRE(f.get_ready() == 20); // (5*2)+10
    //}
}

// --- Example test demonstrating split ---------------------------------------------------------
TEST_CASE("Split fan-out", "[initial_draft]") {
    auto base = on(immediate_executor) | [](int a) { return a; } | [](int x) { return x + 5; };
    auto splitter = split(std::move(base));
    auto left = splitter.fan([](int v) { return v * 2; }) | [](int x) { return x + 1; };
    auto right = splitter.fan([](int v) { return std::string("v=") + std::to_string(v); });

    auto f_right = start(std::move(right), 10);
    auto f_left = start(std::move(left), 5);
    REQUIRE(f_right.get_ready() == std::string("v=15"));
    REQUIRE(f_left.get_ready() == 31);
}

TEST_CASE("Split fan-out bound", "[initial_draft]") {
    auto base = on(immediate_executor) | [](int a) { return a; } | [](int x) { return x + 5; };

    // Bind upstream start argument 10 once:
    auto splitter = split_bind(std::move(base), 10);

    // Branches now start with no args; upstream result (15) is injected.
    auto left = splitter.fan([](int v) { return v * 2; }) | [](int x) { return x + 1; };
    auto right = splitter.fan([](int v) { return std::string("v=") + std::to_string(v); });

    auto f_right = start(std::move(right)); // no argument
    auto f_left = start(std::move(left));   // no argument

    REQUIRE(f_right.get_ready() == std::string("v=15"));
    REQUIRE(f_left.get_ready() == 31);
}

TEST_CASE("Initial draft", "[initial_draft]") {
    GIVEN("a sequence of callables with different arguments") {
        auto oneInt2Int = [](int a) { return a * 2; };
        auto twoInt2Int = [](int a, int b) { return a + b; };
        auto void2Int = []() { return 42; };

        auto a0 = on(stlab::immediate_executor) | oneInt2Int | void2Int | twoInt2Int;

        auto f = start(std::move(a0), 2);
        REQUIRE(f.is_ready());
        auto val = f.get_ready();
        REQUIRE(46 == val);
    }

    GIVEN("a sequence of callables that just work with move only value") {
        auto oneInt2Int = [](move_only a) { return move_only(a.member() * 2); };
        auto twoInt2Int = [](move_only a, move_only b) {
            return move_only(a.member() + b.member());
        };
        auto void2Int = []() { return move_only(42); };

        auto a0 = on(stlab::immediate_executor) | oneInt2Int | void2Int | twoInt2Int;

        auto f = start(std::move(a0), move_only(2));
        REQUIRE(f.is_ready());
        auto val = std::move(f).get_ready();
        REQUIRE(46 == val.member());
    }

    GIVEN("a sequence of callables in a chain of chains synchronous") {
        auto a0 = on(immediate_executor) | [](int x) { return x * 2; } | on(immediate_executor) |
                  [](int x) { return to_string(x); } | on(immediate_executor) |
                  [](const string& s) { return s + "!"; };

        auto f = start(std::move(a0), 42);
        auto val = f.get_ready();
        REQUIRE(val == string("84!"));
    }

    GIVEN("a sequence of callables in a chain of chains asynchronous") {
        auto a0 = on(default_executor) | [](int x) { return x * 2; } | on(immediate_executor) |
                  [](int x) { return to_string(x); } | on(default_executor) |
                  [](const string& s) { return s + "!"; };

        auto val = sync_wait(std::move(a0), 42);
        REQUIRE(val == string("84!"));
    }
}

TEST_CASE("Cancellation of then()", "[initial_draft]") {
    annotate_counters cnt;
    GIVEN("that a ") {
        auto fut =
            async(default_executor, [] {
                std::this_thread::sleep_for(std::chrono::seconds{3});
                std::cout << "Future did run" << std::endl;
                return std::string("42");
            }).then([_counter = annotate{cnt}](const auto& s) { std::cout << s << std::endl; });

        auto result_f = start(then(fut));
    }
    std::this_thread::sleep_for(std::chrono::seconds{5});
    std::cout << cnt << std::endl;
}
} // namespace chains

#endif