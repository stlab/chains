#include <chains/tuple.hpp>

#include <catch2/catch_test_macros.hpp>

#include <string>
#include <tuple>
#include <utility>

TEST_CASE("Test tuple compose", "[tuple_compose]") {
    std::tuple t{[](int x) { return x + 1.0; }, [](double x) { return x * 2.0; },
                 [](double x) { return std::to_string(x / 2.0); }};
    auto f = chains::tuple_compose(std::move(t));
    REQUIRE(f(1) == "2.000000");
}

struct multi_callable {
    int operator()(int a, float b) const { return a + static_cast<int>(b); }
    int operator()(int a, float b, int c) const { return a + static_cast<int>(b) + c; }
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
            REQUIRE((result == std::make_tuple(3, 3, 4.0f)));
        }
        THEN("it returns the correct result for a lambda with no parameters") {
            auto func = []() { return 42; };
            auto result = chains::tuple_consume(t)(func);
            REQUIRE((result == std::make_tuple(42, 1, 2.0f, 3, 4.0f)));
        }
        THEN("it returns the correct result for a callable with various call operators") {
            auto result = chains::tuple_consume(t)(multi_callable{});
            REQUIRE((result == std::make_tuple(6, 4.0f)));
        }
    }

}

