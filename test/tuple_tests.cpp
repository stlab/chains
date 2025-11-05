#include <chains/tuple.hpp>

#include <catch2/catch_test_macros.hpp>

#include <string>
#include <tuple>
#include <utility>

#include <chains/tuple.hpp>

TEST_CASE("Test tuple compose", "[tuple_compose]") {
    std::tuple t{[](int x) { return x + 1.0; }, [](double x) { return x * 2.0; },
                 [](double x) { return std::to_string(x / 2.0); }};
    REQUIRE(chains::tuple_compose(std::move(t))(1) == "2.000000");
}

struct multi_callable {
    int operator()(auto a, auto b) { return a + b; }
    int operator()(auto a, auto b, auto c) { return static_cast<int>(a + b + c); }
};

struct void_t {};

TEST_CASE("Test tuple consume", "[tuple_consume]") {

    GIVEN("A tuple of mixed types")
    {
        std::tuple<int, float, int, float> t{1, 2.0f, 3, 4.0f};
        
        THEN("it returns the correct result for a lambda with two parameters")
        {
            auto func = [](int a, float b) { return a + static_cast<int>(b); };    
            auto result = chains::tuple_consume(t)(func);
            REQUIRE((result == std::make_pair(3, std::make_tuple(3, 4.0f))));
        }
        THEN("it returns the correct result for a lambda with no parameters") {
            auto func = []() { return 42; };
            auto result = chains::tuple_consume(t)(func);
            REQUIRE((result == std::make_pair(42, std::make_tuple(1, 2.0f, 3, 4.0f))));
        }
        THEN("it returns the correct result for a callable with various call operators") {
            auto result = chains::tuple_consume(t)(multi_callable{});
            REQUIRE((result == std::make_pair(6, std::make_tuple(4.0f))));
        }
    }

}

