/*
    Copyright 2026 Adobe
    Distributed under the Boost Software License, Version 1.0.
    (See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
*/

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
    CHECK(f(1) == "2.000000");
}

//--------------------------------------------------------------------------------------------------

TEST_CASE("move_tuple_tail_at", "[tuple]") {
    SECTION("a tail of index zero is requested") {
        SECTION("tuple<>") {
            auto result = chains::move_tuple_tail_at<0>(std::tuple<>{});
            CHECK(result == std::make_tuple());
        }
        SECTION("tuple<int>") {
            auto result = chains::move_tuple_tail_at<0>(std::tuple{42});
            CHECK(result == std::make_tuple(42));
        }
        SECTION("tuple<move_only>") {
            auto result = chains::move_tuple_tail_at<0>(std::tuple{stlab::move_only(42)});
            CHECK(result == std::make_tuple(stlab::move_only(42)));
        }
        SECTION("tuple<move_only, int>") {
            auto result = chains::move_tuple_tail_at<0>(std::tuple{stlab::move_only(42), 42});
            CHECK(result == std::make_tuple(stlab::move_only(42), 42));
        }
    }
    GIVEN("a tail of index one is requested") {
        SECTION("tuple<int>") {
            auto result = chains::move_tuple_tail_at<1>(std::tuple{42});
            CHECK(result == std::make_tuple());
        }
        SECTION("tuple<move_only>") {
            auto result = chains::move_tuple_tail_at<1>(std::tuple{stlab::move_only(42)});
            CHECK(result == std::make_tuple());
        }
        SECTION("tuple<move_only, int>") {
            auto result = chains::move_tuple_tail_at<1>(std::tuple{stlab::move_only(42), 42});
            CHECK(result == std::make_tuple(42));
        }
    }
}

//--------------------------------------------------------------------------------------------------

struct multi_callable {
    auto operator()(int a, float b) const -> int { return a * static_cast<int>(b); }
    auto operator()(int a, float b, int c) const -> int { return a + static_cast<int>(b) + c; }
};

struct oneInt2Int {
    auto operator()(int a) const -> int { return a; }
};

struct twoInt2Int {
    auto operator()(int a, int b) const -> int { return a + b; }
};
struct twoInt2void {
    int& hit;
    auto operator()(int a, int b) const -> void { hit = a + b; }
};
struct void2Int {
    auto operator()() const -> int { return 42; }
};
struct void2void {
    bool& hit;
    auto operator()() -> void { hit = true; }
};
struct oneInt2Void {
    int& hit;
    auto operator()(auto a) -> void { hit = a; }
};
struct string2Int {
    auto operator()(const std::string& s) const -> std::size_t { return s.size(); }
};
struct oneInt2String {
    auto operator()(int a) const -> std::string { return std::to_string(a); }
};
struct moveonly2Int {
    auto operator()(stlab::move_only m) const -> int { return m.member(); }
};
struct oneInt2Moveonly {
    auto operator()(int a) const -> stlab::move_only { return {a}; }
};
struct variadic2Int {
    auto operator()(auto... v) const -> int { return (v + ...); }
};

TEST_CASE("Test tuple consume", "[tuple]") {
    SECTION("A tuple with a single value is passed to tuple_consume") {
        SECTION("tuple<int> -> int(int)") {
            std::tuple t{42};

            auto result = chains::tuple_consume(t)(oneInt2Int{});
            CHECK(result == std::make_tuple(42));
        }
        SECTION("tuple<int> -> int(int...)") {
            std::tuple t{42};
            auto result = chains::tuple_consume(t)(variadic2Int{});
            CHECK(result == std::make_tuple(42));
        }
        SECTION("tuple<int> -> string(int)") {
            std::tuple t{42};
            auto result = chains::tuple_consume(t)(oneInt2String{});
            CHECK(result == std::make_tuple(std::string("42")));
        }
        SECTION("tuple<string> -> int(string)") {
            std::tuple t{std::string("42")};
            auto result = chains::tuple_consume(t)(string2Int{});
            CHECK(result == std::make_tuple(2));
        }
        SECTION("tuple<int> -> move_only(int)") {
            std::tuple t{42};
            auto result = chains::tuple_consume(t)(oneInt2Moveonly{});
            CHECK(result == std::make_tuple(stlab::move_only(42)));
        }
        SECTION("tuple<move_only> -> int(move_only)") {
            std::tuple t{stlab::move_only(42)};
            auto result = chains::tuple_consume(std::move(t))(moveonly2Int{});
            CHECK(result == std::make_tuple(42));
        }
        SECTION("tuple<int> -> void(int)") {
            std::tuple t{42};
            auto hit{0};
            auto result = chains::tuple_consume(t)(oneInt2Void{hit});
            CHECK(hit == 42);
            CHECK(result == std::tuple(std::monostate{}));
        }
        SECTION("tuple<int> -> int(void)") {
            std::tuple t{3};
            auto result = chains::tuple_consume(t)(void2Int{});
            CHECK(result == std::tuple(42, 3));
        }
        SECTION("tuple<> -> void(void) function") {
            std::tuple t{};
            auto hit{false};
            auto result = chains::tuple_consume(t)(void2void{hit});
            CHECK(hit == true);
            CHECK(result == std::tuple(std::monostate{}));
        }
    }

    GIVEN("A tuple with two values is passed to tuple_consume") {
        SECTION("tuple<int, string> -> void(void)") {
            std::tuple t{42, std::string("Don't panic!")};
            auto hit{false};
            auto result = chains::tuple_consume(t)(void2void{hit});
            CHECK(hit == true);
            CHECK(result == std::make_tuple(std::monostate{}, 42, std::string("Don't panic!")));
        }
        SECTION("tuple<int, string> -> void(int)") {
            std::tuple t{42, std::string("Don't panic!")};
            auto hit{0};
            auto result = chains::tuple_consume(t)(oneInt2Void{hit});
            CHECK(hit == 42);
            CHECK(result == std::make_tuple(std::monostate{}, std::string("Don't panic!")));
        }
        SECTION("tuple<int, string> -> int(void)") {
            std::tuple t{3, std::string("Don't panic!")};
            auto result = chains::tuple_consume(t)(void2Int{});
            CHECK(result == std::make_tuple(42, 3, std::string("Don't panic!")));
        }
        SECTION("tuple<int, int> -> void(int, int)") {
            std::tuple t{1, 2};
            auto hit{0};
            auto result = chains::tuple_consume(t)(twoInt2void{hit});
            CHECK(hit == 3);
            CHECK(result == std::make_tuple(std::monostate{}));
        }
    }
    GIVEN("A tuple with three values is passed to tuple_consume") {
        SECTION("tuple<int, int> -> int(int, int) function") {
            std::tuple t{1, 2, 3.f};
            auto result = chains::tuple_consume(t)(twoInt2Int{});
            CHECK(result == std::make_tuple(3, 3.f));
        }
        SECTION(
            "it is of type tuple<int, float, int> and it is applied to a function object with several callables") {
            std::tuple t{1, 2.f, 3};
            auto result = chains::tuple_consume(t)(multi_callable{});
            CHECK(result == std::make_tuple(6));
        }
        SECTION(
            "it is of type tuple<int, float> and it is applied to a function object with several callables") {
            std::tuple t{1, 2.f, std::string("Don't panic!")};
            auto result = chains::tuple_consume(t)(multi_callable{});
            CHECK(result == std::make_tuple(2, std::string("Don't panic!")));
        }
    }
}

TEST_CASE("interpret", "[tuple]") {
    SECTION("test cases with one function as argument") {
        SECTION("void(void)") {
            auto hit{false};
            auto result = chains::interpret(std::make_tuple(void2void{hit}))();
            CHECK(hit);
            CHECK(result == std::monostate{});
        }
        SECTION("void(int)") {
            auto hit{0};
            auto result = chains::interpret(std::make_tuple(oneInt2Void{hit}))(3);
            CHECK(hit == 3);
            CHECK(result == std::monostate{});
        }
        SECTION("int(void)") {
            auto result = chains::interpret(std::make_tuple(void2Int{}))();
            CHECK(result == 42);
        }
        SECTION("int(int)") {
            auto result = chains::interpret(std::make_tuple(oneInt2Int{}))(42);
            CHECK(result == 42);
        }
        SECTION("void(int, int)") {
            auto hit{0};
            auto result = chains::interpret(std::make_tuple(twoInt2void{hit}))(2, 3);
            CHECK(hit == 5);
            CHECK(result == std::monostate{});
        }
        SECTION("int(int, int)") {
            auto result = chains::interpret(std::make_tuple(twoInt2Int{}))(2, 3);
            CHECK(result == 5);
        }
    }
    SECTION("test cases with two functions as arguments") {
        SECTION("void(void), void(void)") {
            auto hit1{false};
            auto hit2{false};
            auto result = chains::interpret(std::make_tuple(void2void{hit1}, void2void{hit2}))();
            CHECK(hit1);
            CHECK(hit2);
            CHECK(result == std::monostate{});
        }
    }
}
