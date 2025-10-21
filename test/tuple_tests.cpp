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
