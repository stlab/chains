#include <chains/tuple.hpp>

#include <catch2/catch_test_macros.hpp>
#include <stlab/test/model.hpp> // moveonly

#include <cstddef>
#include <string>
#include <tuple>
#include <utility>
#include <variant> // monostate

TEST_CASE("Test tuple compose", "[tuple]") {
    std::tuple t{[](int x) { return x + 1.0; }, [](double x) { return x * 2.0; },
                 [](double x) { return std::to_string(x / 2.0); }};
    auto f = chains::tuple_compose(std::move(t));
    REQUIRE(f(1) == "2.000000");
}

struct multi_callable {
    auto operator()(int a, float b) const -> int { return a * static_cast<int>(b); }
    auto operator()(int a, float b, int c) const -> int { return a + static_cast<int>(b) + c; }
};

struct void_t {};

auto oneInt2Int = [](int a) -> int { return a; };
auto twoInt2Int = [](int a, int b) -> int { return a + b; };
auto twoInt2void = [](int, int) -> void {};
auto void2Int = []() -> int { return 42 * 2; };
auto void2void = []() -> void {};
auto oneInt2Void = [](auto) -> void {};
auto string2Int = [](const std::string& s) -> std::size_t { return s.size(); };
auto oneInt2String = [](int a) -> std::string { return std::to_string(a); };
auto moveonly2Int = [](stlab::move_only m) -> int { return m.member(); };
auto oneInt2Moveonly = [](int a) -> stlab::move_only { return {a}; };
auto variadic2Int = [](auto... v) -> int { return (v + ...); };

TEST_CASE("Test tuple consume", "[tuple]") {
    GIVEN("A tuple with a single value is passed to tuple_consume") {
        WHEN("it is of type tuple<int> and it is applied to an int(int) function") {
            std::tuple t{42};
            THEN("a tuple<int> is returned") {
                auto result = chains::tuple_consume(t)(oneInt2Int);
                REQUIRE(result == std::make_tuple(42));
            }
        }
        WHEN("it is of type tuple<int> and it is applied to a variadic int(int...) function") {
            std::tuple t{42};
            THEN("a tuple<int> is returned") {
                auto result = chains::tuple_consume(t)(variadic2Int);
                REQUIRE(result == std::make_tuple(42));
            }
        }
        WHEN("it is of type tuple<int> and it is applied to a string(int) function") {
            std::tuple t{42};
            THEN("a tuple<string> is returned") {
                auto result = chains::tuple_consume(t)(oneInt2String);
                REQUIRE(result == std::make_tuple(std::string("42")));
            }
        }
        WHEN("it is of type tuple<string> and it is applied to an int(string) function") {
            std::tuple t{std::string("42")};
            THEN("a tuple<int> is returned") {
                auto result = chains::tuple_consume(t)(string2Int);
                REQUIRE(result == std::make_tuple(2));
            }
        }
        WHEN("it is of type tuple<int> and it is applied to a move_only(int) function") {
            std::tuple t{42};
            THEN("a tuple<move_only> is returned") {
                auto result = chains::tuple_consume(t)(oneInt2Moveonly);
                REQUIRE(result == std::make_tuple(stlab::move_only(42)));
            }
        }
        WHEN("it is of type tuple<move_only> and it is applied to an int(move_only) function") {
            std::tuple t{stlab::move_only(42)};
            THEN("a tuple<int>") {
                auto result = chains::tuple_consume(std::move(t))(moveonly2Int);
                REQUIRE(result == std::make_tuple(42));
            }
        }
        WHEN("it is of type tuple<int> and it is applied to a void(int) function") {
            std::tuple t{42};
            THEN("a tuple<monostate> is returned") {
                auto result = chains::tuple_consume(t)(oneInt2Void);
                REQUIRE(result == std::tuple(std::monostate{}));
            }
        }
        WHEN("it is of type tuple<int> and is applied to an int(void) function") {
            std::tuple t{42};
            THEN("a tuple<int, int> is returned") {
                auto result = chains::tuple_consume(t)(void2Int);
                REQUIRE(result == std::tuple(84, 42));
            }
        }
        WHEN("it is of type tuple<> and it is applied to an int(void) function") {
            std::tuple t{};
            THEN("a tuple<int> is returned") {
                auto result = chains::tuple_consume(t)(void2Int);
                REQUIRE(result == std::tuple(84));
            }
        }
        WHEN("it is of type tuple<> and is applied to a void(void) function") {
            std::tuple t{};
            THEN("a tuple<monostate> is returned") {
                auto result = chains::tuple_consume(t)(void2void);
                REQUIRE(result == std::tuple(std::monostate{}));
            }
        }
    }

    GIVEN("A tuple with two values is passed to tuple_consume") {
        WHEN("it is of type tuple<int, string> and it is applied to a void(void) function") {
            std::tuple t{42, std::string("Don't panic!")};
            THEN("a tuple<monostate, int, string> is returned") {
                auto result = chains::tuple_consume(t)(void2void);
                CHECK(result == std::make_tuple(std::monostate{}, 42, std::string("Don't panic!")));
            }
        }
        WHEN("it is of type tuple<int, string> and it is applied to a void(int) function") {
            std::tuple t{42, std::string("Don't panic!")};
            THEN("a tuple<monostate, string> is returned") {
                auto result = chains::tuple_consume(t)(oneInt2Void);
                CHECK(result == std::make_tuple(std::monostate{}, std::string("Don't panic!")));
            }
        }
        WHEN("it is of type tuple<int, string> and it is applied to an int(void) function") {
            std::tuple t{42, std::string("Don't panic!")};
            THEN("a tuple<int, int, string> is returned") {
                auto result = chains::tuple_consume(t)(void2Int);
                CHECK(result == std::make_tuple(84, 42, std::string("Don't panic!")));
            }
        }
        WHEN("it is of type tuple<int, int> and it is applied to an void(int, int) function") {
            std::tuple t{1, 2};
            THEN("a tuple<monostate> is returned") {
                auto result = chains::tuple_consume(t)(twoInt2void);
                CHECK(result == std::make_tuple(std::monostate{}));
            }
        }
    }
    GIVEN("A tuple with three values is passed to tuple_consume") {
        WHEN("it is of type tuple<int, int> and it is applied to an int(int, int) function") {
            std::tuple t{1, 2, 3.f};
            THEN("a tuple<int, float> is returned") {
                auto result = chains::tuple_consume(t)(twoInt2Int);
                CHECK(result == std::make_tuple(3, 3.f));
            }
        }
        WHEN(
            "it is of type tuple<int, float, int> and it is applied to a function object with several callables") {
            std::tuple t{1, 2.f, 3};
            THEN(
                "the callable with the maximum matching arguments is taken and a tuple<int> is returned") {
                auto result = chains::tuple_consume(t)(multi_callable{});
                CHECK(result == std::make_tuple(6));
            }
        }
        WHEN(
            "it is of type tuple<int, float> and it is applied to a function object with several callables") {
            std::tuple t{1, 2.f, std::string("Don't panic!")};
            THEN(
                "the callable with the matching arguments is taken and a tuple<int, string> is returned") {
                auto result = chains::tuple_consume(t)(multi_callable{});
                CHECK(result == std::make_tuple(2, std::string("Don't panic!")));
            }
        }
    }
}

//
// TEST_CASE("Test concat functions", "[tuple_consume]") {
//
//     GIVEN("A tuple of mixed types")
//     {
//
//         auto functions = std::make_tuple(oneInt2Int, void2Int, twoInt2Int);
//
//         GIVEN("the calculation is done")
//         {
//             auto result = chains::calc(functions, 2);
//             REQUIRE(46 == result);
//         }
//     }
// }