#include "catch2/catch_test_macros.hpp"

#include <chains/chains.hpp>

#include <utility>

TEST_CASE("Basic chain operations", "[chain]") {
    SECTION("Can instantiate a simple chain") {
        SECTION("Chain with two lambdas") {
            // Create a simple chain by piping a segment with a function
            auto s = chains::segment{chains::type<void>{},
                                     []<typename... Args>(auto&& f, Args&&... args) {
                                         return f(std::forward<Args>(args)...);
                                     },
                                     [](int x) { return x * 2; }};

            auto c = std::move(s) | [](int x) { return x + 1; };

            // c is now a chains::chain instance
            // We can verify it compiles and the type is deduced correctly
            (void)c; // Suppress unused variable warning
        }
    }
}

#if 0

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
