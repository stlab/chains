/*
    Copyright 2026 Adobe
    Distributed under the Boost Software License, Version 1.0.
    (See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
*/

#include <chain/tuple.hpp>

#include <catch2/catch_test_macros.hpp>
#include <stlab/test/model.hpp> // moveonly

#include <cstddef>
#include <string>
#include <tuple>
#include <utility>
#include <variant> // monostate

TEST_CASE("Test tuple compose", "[tuple]") {
    std::tuple t{[](int x) -> double { return x + 1.0; },
                 [](double x) -> double { return x * 2.0; },
                 [](double x) -> std::string { return std::to_string(x / 2.0); }};
    auto f = chain::tuple_compose(std::move(t));
    CHECK(f(1) == "2.000000");
}

//--------------------------------------------------------------------------------------------------

TEST_CASE("move_tuple_tail_at", "[tuple]") {
    SECTION("a tail of index zero is requested") {
        SECTION("tuple<>") {
            auto result = chain::move_tuple_tail_at<0>(std::tuple<>{});
            CHECK(result == std::make_tuple());
        }
        SECTION("tuple<int>") {
            auto result = chain::move_tuple_tail_at<0>(std::tuple{42});
            CHECK(result == std::make_tuple(42));
        }
        SECTION("tuple<move_only>") {
            auto result = chain::move_tuple_tail_at<0>(std::tuple{stlab::move_only(42)});
            CHECK(result == std::make_tuple(stlab::move_only(42)));
        }
        SECTION("tuple<move_only, int>") {
            auto result = chain::move_tuple_tail_at<0>(std::tuple{stlab::move_only(42), 42});
            CHECK(result == std::make_tuple(stlab::move_only(42), 42));
        }
        SECTION("tuple<int, float, string>") {
            auto result = chain::move_tuple_tail_at<0>(std::tuple{1, 2.5f, std::string("test")});
            CHECK(result == std::make_tuple(1, 2.5f, std::string("test")));
        }
        SECTION("tuple<int, int, int, int>") {
            auto result = chain::move_tuple_tail_at<0>(std::tuple{1, 2, 3, 4});
            CHECK(result == std::make_tuple(1, 2, 3, 4));
        }
    }
    GIVEN("a tail of index one is requested") {
        SECTION("tuple<int>") {
            auto result = chain::move_tuple_tail_at<1>(std::tuple{42});
            CHECK(result == std::make_tuple());
        }
        SECTION("tuple<move_only>") {
            auto result = chain::move_tuple_tail_at<1>(std::tuple{stlab::move_only(42)});
            CHECK(result == std::make_tuple());
        }
        SECTION("tuple<move_only, int>") {
            auto result = chain::move_tuple_tail_at<1>(std::tuple{stlab::move_only(42), 42});
            CHECK(result == std::make_tuple(42));
        }
        SECTION("tuple<int, float, string>") {
            auto result = chain::move_tuple_tail_at<1>(std::tuple{1, 2.5f, std::string("test")});
            CHECK(result == std::make_tuple(2.5f, std::string("test")));
        }
        SECTION("tuple<string, int, move_only>") {
            auto result = chain::move_tuple_tail_at<1>(
                std::tuple{std::string("first"), 42, stlab::move_only(100)});
            CHECK(result == std::make_tuple(42, stlab::move_only(100)));
        }
    }
    GIVEN("a tail of index two is requested") {
        SECTION("tuple<int, int, int>") {
            auto result = chain::move_tuple_tail_at<2>(std::tuple{1, 2, 3});
            CHECK(result == std::make_tuple(3));
        }
        SECTION("tuple<int, float, string>") {
            auto result = chain::move_tuple_tail_at<2>(std::tuple{1, 2.5f, std::string("test")});
            CHECK(result == std::make_tuple(std::string("test")));
        }
        SECTION("tuple<int, string, move_only>") {
            auto result = chain::move_tuple_tail_at<2>(
                std::tuple{42, std::string("mid"), stlab::move_only(100)});
            CHECK(result == std::make_tuple(stlab::move_only(100)));
        }
        SECTION("tuple<int, int, int, int>") {
            auto result = chain::move_tuple_tail_at<2>(std::tuple{1, 2, 3, 4});
            CHECK(result == std::make_tuple(3, 4));
        }
        SECTION("tuple<move_only, move_only, move_only>") {
            auto result = chain::move_tuple_tail_at<2>(
                std::tuple{stlab::move_only(1), stlab::move_only(2), stlab::move_only(3)});
            CHECK(result == std::make_tuple(stlab::move_only(3)));
        }
    }
    GIVEN("a tail of index three is requested") {
        SECTION("tuple<int, int, int, int>") {
            auto result = chain::move_tuple_tail_at<3>(std::tuple{1, 2, 3, 4});
            CHECK(result == std::make_tuple(4));
        }
        SECTION("tuple<string, float, int, move_only>") {
            auto result = chain::move_tuple_tail_at<3>(
                std::tuple{std::string("a"), 1.5f, 42, stlab::move_only(100)});
            CHECK(result == std::make_tuple(stlab::move_only(100)));
        }
        SECTION("tuple<int, int, int, int, int>") {
            auto result = chain::move_tuple_tail_at<3>(std::tuple{1, 2, 3, 4, 5});
            CHECK(result == std::make_tuple(4, 5));
        }
    }
    GIVEN("offset equals tuple size") {
        SECTION("tuple<int, int> with offset 2") {
            auto result = chain::move_tuple_tail_at<2>(std::tuple{1, 2});
            CHECK(result == std::make_tuple());
        }
        SECTION("tuple<int, string, float> with offset 3") {
            auto result = chain::move_tuple_tail_at<3>(std::tuple{1, std::string("test"), 2.5f});
            CHECK(result == std::make_tuple());
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
    auto operator()() const -> int { return 0; }
    auto operator()(auto... v) const -> int { return (v + ...); }
};

TEST_CASE("Test tuple consume", "[tuple]") {
    SECTION("A tuple with a single value is passed to tuple_consume") {
        SECTION("tuple<int> -> int(int)") {
            std::tuple t{42};

            auto result = chain::tuple_consume(t)(oneInt2Int{});
            CHECK(result == std::make_tuple(42));
        }
        SECTION("tuple<int> -> int(int...)") {
            std::tuple t{42};
            auto result = chain::tuple_consume(t)(variadic2Int{});
            CHECK(result == std::make_tuple(42));
        }
        SECTION("tuple<int> -> string(int)") {
            std::tuple t{42};
            auto result = chain::tuple_consume(t)(oneInt2String{});
            CHECK(result == std::make_tuple(std::string("42")));
        }
        SECTION("tuple<string> -> int(string)") {
            std::tuple t{std::string("42")};
            auto result = chain::tuple_consume(t)(string2Int{});
            CHECK(result == std::make_tuple(2));
        }
        SECTION("tuple<int> -> move_only(int)") {
            std::tuple t{42};
            auto result = chain::tuple_consume(t)(oneInt2Moveonly{});
            CHECK(result == std::make_tuple(stlab::move_only(42)));
        }
        SECTION("tuple<move_only> -> int(move_only)") {
            std::tuple t{stlab::move_only(42)};
            auto result = chain::tuple_consume(std::move(t))(moveonly2Int{});
            CHECK(result == std::make_tuple(42));
        }
        SECTION("tuple<int> -> void(int)") {
            std::tuple t{42};
            auto hit{0};
            auto result = chain::tuple_consume(t)(oneInt2Void{hit});
            CHECK(hit == 42);
            CHECK(result == std::tuple(std::monostate{}));
        }
        SECTION("tuple<int> -> int(void)") {
            std::tuple t{3};
            auto result = chain::tuple_consume(t)(void2Int{});
            CHECK(result == std::tuple(42, 3));
        }
        SECTION("tuple<> -> void(void) function") {
            std::tuple t{};
            auto hit{false};
            auto result = chain::tuple_consume(t)(void2void{hit});
            CHECK(hit == true);
            CHECK(result == std::tuple(std::monostate{}));
        }
        SECTION("tuple<float> -> int(float)") {
            std::tuple t{3.14f};
            auto result =
                chain::tuple_consume(t)([](float f) -> int { return static_cast<int>(f); });
            CHECK(result == std::make_tuple(3));
        }
    }

    GIVEN("A tuple with two values is passed to tuple_consume") {
        SECTION("tuple<int, string> -> void(void)") {
            std::tuple t{42, std::string("Don't panic!")};
            auto hit{false};
            auto result = chain::tuple_consume(t)(void2void{hit});
            CHECK(hit == true);
            CHECK(result == std::make_tuple(std::monostate{}, 42, std::string("Don't panic!")));
        }
        SECTION("tuple<int, string> -> void(int)") {
            std::tuple t{42, std::string("Don't panic!")};
            auto hit{0};
            auto result = chain::tuple_consume(t)(oneInt2Void{hit});
            CHECK(hit == 42);
            CHECK(result == std::make_tuple(std::monostate{}, std::string("Don't panic!")));
        }
        SECTION("tuple<int, string> -> int(void)") {
            std::tuple t{3, std::string("Don't panic!")};
            auto result = chain::tuple_consume(t)(void2Int{});
            CHECK(result == std::make_tuple(42, 3, std::string("Don't panic!")));
        }
        SECTION("tuple<int, int> -> void(int, int)") {
            std::tuple t{1, 2};
            auto hit{0};
            auto result = chain::tuple_consume(t)(twoInt2void{hit});
            CHECK(hit == 3);
            CHECK(result == std::make_tuple(std::monostate{}));
        }
        SECTION("tuple<int, int> -> int(int, int)") {
            std::tuple t{5, 7};
            auto result = chain::tuple_consume(t)(twoInt2Int{});
            CHECK(result == std::make_tuple(12));
        }
        SECTION("tuple<move_only, int> -> int(move_only)") {
            std::tuple t{stlab::move_only(42), 100};
            auto result = chain::tuple_consume(std::move(t))(moveonly2Int{});
            CHECK(result == std::make_tuple(42, 100));
        }
        SECTION("tuple<int, move_only> -> int(int) leaves move_only") {
            std::tuple t{42, stlab::move_only(100)};
            auto result = chain::tuple_consume(std::move(t))(oneInt2Int{});
            CHECK(result == std::make_tuple(42, stlab::move_only(100)));
        }
        SECTION("tuple<string, string> -> int(string)") {
            std::tuple t{std::string("hello"), std::string("world")};
            auto result = chain::tuple_consume(t)(string2Int{});
            CHECK(result == std::make_tuple(5, std::string("world")));
        }
    }
    GIVEN("A tuple with three values is passed to tuple_consume") {
        SECTION("tuple<int, int> -> int(int, int) function") {
            std::tuple t{1, 2, 3.f};
            auto result = chain::tuple_consume(t)(twoInt2Int{});
            CHECK(result == std::make_tuple(3, 3.f));
        }
        SECTION(
            "it is of type tuple<int, float, int> and it is applied to a function object with several callables") {
            std::tuple t{1, 2.f, 3};
            auto result = chain::tuple_consume(t)(multi_callable{});
            CHECK(result == std::make_tuple(6));
        }
        SECTION(
            "it is of type tuple<int, float> and it is applied to a function object with several callables") {
            std::tuple t{1, 2.f, std::string("Don't panic!")};
            auto result = chain::tuple_consume(t)(multi_callable{});
            CHECK(result == std::make_tuple(2, std::string("Don't panic!")));
        }
        SECTION("tuple<int, int, int> -> void(void) leaves all") {
            std::tuple t{1, 2, 3};
            auto hit{false};
            auto result = chain::tuple_consume(t)(void2void{hit});
            CHECK(hit == true);
            CHECK(result == std::make_tuple(std::monostate{}, 1, 2, 3));
        }
        SECTION("tuple<int, int, int> -> int(void) prepends result") {
            std::tuple t{1, 2, 3};
            auto result = chain::tuple_consume(t)(void2Int{});
            CHECK(result == std::make_tuple(42, 1, 2, 3));
        }
        SECTION("tuple<int, int, int> -> int(int) consumes first") {
            std::tuple t{10, 20, 30};
            auto result = chain::tuple_consume(t)(oneInt2Int{});
            CHECK(result == std::make_tuple(10, 20, 30));
        }
        SECTION("tuple<int, int, int> -> void(int, int, int)") {
            std::tuple t{1, 2, 3};
            auto hit{0};
            auto result =
                chain::tuple_consume(t)([&hit](int a, int b, int c) -> void { hit = a + b + c; });
            CHECK(hit == 6);
            CHECK(result == std::make_tuple(std::monostate{}));
        }
        SECTION("tuple<move_only, int, int> -> int(move_only) leaves tail") {
            std::tuple t{stlab::move_only(5), 10, 15};
            auto result = chain::tuple_consume(std::move(t))(moveonly2Int{});
            CHECK(result == std::make_tuple(5, 10, 15));
        }
        SECTION("tuple<int, move_only, int> with int(int)") {
            std::tuple t{42, stlab::move_only(100), 200};
            auto result = chain::tuple_consume(std::move(t))(oneInt2Int{});
            CHECK(result == std::make_tuple(42, stlab::move_only(100), 200));
        }
        SECTION("tuple<string, int, float> -> int(string)") {
            std::tuple t{std::string("test"), 42, 3.14f};
            auto result = chain::tuple_consume(t)(string2Int{});
            CHECK(result == std::make_tuple(4, 42, 3.14f));
        }
    }
    GIVEN("A tuple with four or more values is passed to tuple_consume") {
        SECTION("tuple<int, int, int, int> -> int(int, int)") {
            std::tuple t{1, 2, 3, 4};
            auto result = chain::tuple_consume(t)(twoInt2Int{});
            CHECK(result == std::make_tuple(3, 3, 4));
        }
        SECTION("tuple<int, int, int, int> -> void(int, int)") {
            std::tuple t{1, 2, 3, 4};
            auto hit{0};
            auto result = chain::tuple_consume(t)(twoInt2void{hit});
            CHECK(hit == 3);
            CHECK(result == std::make_tuple(std::monostate{}, 3, 4));
        }
        SECTION("tuple<int, int, int, int> -> int(int)") {
            std::tuple t{10, 20, 30, 40};
            auto result = chain::tuple_consume(t)(oneInt2Int{});
            CHECK(result == std::make_tuple(10, 20, 30, 40));
        }
        SECTION("tuple<int, int, int, int> -> void(void)") {
            std::tuple t{1, 2, 3, 4};
            auto hit{false};
            auto result = chain::tuple_consume(t)(void2void{hit});
            CHECK(hit == true);
            CHECK(result == std::make_tuple(std::monostate{}, 1, 2, 3, 4));
        }
        SECTION("tuple<int, int, int, int, int> -> int(int, int)") {
            std::tuple t{1, 2, 3, 4, 5};
            auto result = chain::tuple_consume(t)(twoInt2Int{});
            CHECK(result == std::make_tuple(3, 3, 4, 5));
        }
        SECTION("tuple<string, int, float, string> -> int(string)") {
            std::tuple t{std::string("hello"), 42, 3.14f, std::string("world")};
            auto result = chain::tuple_consume(t)(string2Int{});
            CHECK(result == std::make_tuple(5, 42, 3.14f, std::string("world")));
        }
    }
    GIVEN("Variadic function consumption") {
        SECTION("tuple<int, int> -> int(int...)") {
            std::tuple t{10, 20};
            auto result = chain::tuple_consume(t)(variadic2Int{});
            CHECK(result == std::make_tuple(30));
        }
        SECTION("tuple<int, int, int> -> int(int...)") {
            std::tuple t{1, 2, 3};
            auto result = chain::tuple_consume(t)(variadic2Int{});
            CHECK(result == std::make_tuple(6));
        }
        SECTION("tuple<int, int, int, int> -> int(int...)") {
            std::tuple t{1, 2, 3, 4};
            auto result = chain::tuple_consume(t)(variadic2Int{});
            CHECK(result == std::make_tuple(10));
        }
        SECTION("tuple<> -> int(int...)") {
            std::tuple t{};
            auto result = chain::tuple_consume(t)(variadic2Int{});
            CHECK(result == std::make_tuple(0));
        }
    }
    GIVEN("Multiple move-only types") {
        SECTION("tuple<move_only, move_only> -> int(move_only)") {
            std::tuple t{stlab::move_only(10), stlab::move_only(20)};
            auto result = chain::tuple_consume(std::move(t))(moveonly2Int{});
            CHECK(result == std::make_tuple(10, stlab::move_only(20)));
        }
        SECTION("tuple<move_only, move_only, int> -> int(move_only)") {
            std::tuple t{stlab::move_only(5), stlab::move_only(15), 25};
            auto result = chain::tuple_consume(std::move(t))(moveonly2Int{});
            CHECK(result == std::make_tuple(5, stlab::move_only(15), 25));
        }
    }
    SECTION("Complex return types") {
        SECTION("tuple<int> -> tuple(int)") {
            std::tuple t{42};
            auto result = chain::tuple_consume(t)(
                [](int x) -> std::tuple<int, int> { return std::make_tuple(x, x * 2); });
            // Result is tuple<tuple<int, int>> - a 1-element tuple containing a 2-element tuple
            auto nested = std::get<0>(result);
            CHECK(nested == std::make_tuple(42, 84));
            constexpr auto size = std::tuple_size_v<decltype(result)>;
            CHECK(size == 1);
        }
        SECTION("tuple<int, int> -> pair(int, int)") {
            std::tuple t{3, 4};
            auto result = chain::tuple_consume(t)(
                [](int a, int b) -> std::pair<int, int> { return std::make_pair(a, b); });
            // Result is tuple<pair<int, int>> - a 1-element tuple containing a pair
            auto nested = std::get<0>(result);
            CHECK(nested == std::make_pair(3, 4));
            constexpr auto size = std::tuple_size_v<decltype(result)>;
            CHECK(size == 1);
        }
    }
}

TEST_CASE("interpret", "[tuple]") {
    SECTION("test cases with one function as argument") {
        SECTION("void(void)") {
            auto hit{false};
            auto result = chain::interpret(std::make_tuple(void2void{hit}))();
            CHECK(hit);
            CHECK(result == std::monostate{});
        }
        SECTION("void(int)") {
            auto hit{0};
            auto result = chain::interpret(std::make_tuple(oneInt2Void{hit}))(3);
            CHECK(hit == 3);
            CHECK(result == std::monostate{});
        }
        SECTION("int(void)") {
            auto result = chain::interpret(std::make_tuple(void2Int{}))();
            CHECK(result == 42);
        }
        SECTION("int(int)") {
            auto result = chain::interpret(std::make_tuple(oneInt2Int{}))(42);
            CHECK(result == 42);
        }
        SECTION("void(int, int)") {
            auto hit{0};
            auto result = chain::interpret(std::make_tuple(twoInt2void{hit}))(2, 3);
            CHECK(hit == 5);
            CHECK(result == std::monostate{});
        }
        SECTION("int(int, int)") {
            auto result = chain::interpret(std::make_tuple(twoInt2Int{}))(2, 3);
            CHECK(result == 5);
        }
    }
    SECTION("test cases with two functions as arguments") {
        SECTION("void(void), void(void)") {
            auto hit1{false};
            auto hit2{false};
            auto result = chain::interpret(std::make_tuple(void2void{hit1}, void2void{hit2}))();
            CHECK(hit1);
            CHECK(hit2);
            CHECK(result == std::monostate{});
        }
        SECTION("int(void), int(int)") {
            auto result =
                chain::interpret(std::make_tuple(void2Int{}, oneInt2Int{}))(/* no args */);
            CHECK(result == 42);
        }

        SECTION("int(int), int(int)") {
            auto result = chain::interpret(std::make_tuple(oneInt2Int{}, oneInt2Int{}))(42);
            CHECK(result == 42);
        }

        SECTION("void(int), int(void)") {
            auto hit{0};
            auto result = chain::interpret(std::make_tuple(oneInt2Void{hit}, void2Int{}))(5);
            CHECK(hit == 5);
            CHECK(result == 42);
        }

        SECTION("int(int), void(int)") {
            auto hit{0};
            auto result = chain::interpret(std::make_tuple(oneInt2Int{}, oneInt2Void{hit}))(42);
            CHECK(hit == 42);
            CHECK(result == std::monostate{});
        }

        SECTION("int(int, int), int(int) - chained computation") {
            auto result = chain::interpret(std::make_tuple(twoInt2Int{}, oneInt2Int{}))(3, 4);
            CHECK(result == 7); // 3+4=7, then identity
        }

        SECTION("string(int), int(string)") {
            auto result = chain::interpret(std::make_tuple(oneInt2String{}, string2Int{}))(42);
            CHECK(result == 2); // "42" has length 2
        }
    }

    SECTION("test cases with three functions as arguments") {
        SECTION("int(int), int(int), int(int) - identity chain") {
            auto result =
                chain::interpret(std::make_tuple(oneInt2Int{}, oneInt2Int{}, oneInt2Int{}))(42);
            CHECK(result == 42);
        }

        SECTION("int(int, int), int(int), string(int)") {
            auto result = chain::interpret(
                std::make_tuple(twoInt2Int{}, oneInt2Int{}, oneInt2String{}))(3, 4);
            CHECK(result == std::string("7"));
        }

        SECTION("void(void), int(void), int(int)") {
            auto hit{false};
            auto result = chain::interpret(
                std::make_tuple(void2void{hit}, void2Int{}, oneInt2Int{}))(/* no args */);
            CHECK(hit);
            CHECK(result == 42);
        }

        SECTION("int(int), void(int), void(void)") {
            auto hit1{0};
            auto hit2{false};
            auto result = chain::interpret(
                std::make_tuple(oneInt2Int{}, oneInt2Void{hit1}, void2void{hit2}))(42);
            CHECK(hit1 == 42);
            CHECK(hit2);
            CHECK(result == std::monostate{});
        }
    }

    SECTION("test cases with multiple input arguments and stack behavior") {
        SECTION("int(int), int(int) with two initial arguments - processes first, leaves second") {
            auto result = chain::interpret(std::make_tuple(oneInt2Int{}, oneInt2Int{}))(10, 20);
            CHECK(result == 10); // First int processed through both functions, second ignored
        }

        SECTION("int(int, int), int(int) with three initial arguments") {
            auto result = chain::interpret(std::make_tuple(twoInt2Int{}, oneInt2Int{}))(1, 2, 3);
            CHECK(result == 3); // (1+2=3), then identity(3), leaves 3 on stack
        }

        SECTION("void(int, int) with three initial arguments - leaves one") {
            auto hit{0};
            auto result = chain::interpret(std::make_tuple(twoInt2void{hit}))(5, 7, 100);
            CHECK(hit == 12);
            CHECK(result == std::monostate{}); // Last argument remains on stack
        }
    }

    SECTION("test cases with move-only types") {
        SECTION("move_only(int), int(move_only)") {
            auto result = chain::interpret(std::make_tuple(oneInt2Moveonly{}, moveonly2Int{}))(42);
            CHECK(result == 42);
        }

        SECTION("int(move_only), move_only(int)") {
            auto result = chain::interpret(std::make_tuple(moveonly2Int{}, oneInt2Moveonly{}))(
                stlab::move_only(42));
            CHECK(result == stlab::move_only(42));
        }
    }

    SECTION("test cases with no functions") {
        SECTION("empty function tuple with no arguments") {
            auto result = chain::interpret(std::make_tuple())(/* no args */);
            CHECK(result == std::monostate{});
        }

        SECTION("empty function tuple with one argument") {
            auto result = chain::interpret(std::make_tuple())(42);
            CHECK(result == 42);
        }

        SECTION("empty function tuple with multiple arguments") {
            auto result = chain::interpret(std::make_tuple())(1, 2);
            CHECK(result == 1); // Returns first element
        }
    }

    SECTION("test cases with variadic functions") {
        SECTION("int(int...) consuming all arguments") {
            auto result = chain::interpret(std::make_tuple(variadic2Int{}))(1, 2, 3, 4);
            CHECK(result == 10);
        }

        SECTION("int(int...), int(int) - variadic then unary") {
            auto result = chain::interpret(std::make_tuple(variadic2Int{}, oneInt2Int{}))(1, 2, 3);
            CHECK(result == 6);
        }
    }
}
