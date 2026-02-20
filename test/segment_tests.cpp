/*
    Copyright 2026 Adobe
    Distributed under the Boost Software License, Version 1.0.
    (See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
*/

#include <chains/segment.hpp>

#include <catch2/catch_test_macros.hpp>
#include <stlab/test/model.hpp> // moveonly

#include <exception>
#include <memory>
#include <string>

// Mock receiver for testing invoke
struct mock_receiver {
    bool _canceled{false};
    std::exception_ptr _exception;
    int _result{0};

    auto canceled() const -> bool { return _canceled; }
    auto set_exception(std::exception_ptr e) -> void { _exception = std::move(e); }
    auto set_value(int value) -> void { _result = value; }
};

TEST_CASE("Basic segment operations", "[segment]") {
    SECTION("simple creation with variadic constructor") {
        auto sut =
            chains::segment{chains::type<std::tuple<>>{}, [](auto f) { f(); }, []() { return 42; }};
    }

    SECTION("simple creation with tuple constructor") {
        auto sut = chains::segment{chains::type<std::tuple<>>{}, [](auto f) { f(); },
                                   std::make_tuple([]() { return 42; })};
    }

    SECTION("creation with multiple functions") {
        auto sut = chains::segment{chains::type<std::tuple<>>{}, [](auto f) { f(); },
                                   [](int x) { return x + 1; }, [](int x) { return x * 2; }};
    }

    SECTION("creation with empty function tuple") {
        auto sut =
            chains::segment{chains::type<std::tuple<>>{}, [](auto f) { f(); }, std::tuple<>{}};
    }
}

TEST_CASE("Segment copy and move semantics", "[segment]") {
    SECTION("copy constructor") {
        auto original =
            chains::segment{chains::type<std::tuple<>>{}, [](auto f) { f(); }, []() { return 42; }};
        [[maybe_unused]] auto copy{original}; // Use direct initialization due to explicit constructor
        // Both should be valid and independent
    }

    SECTION("move constructor") {
        auto original =
            chains::segment{chains::type<std::tuple<>>{}, [](auto f) { f(); }, []() { return 42; }};
        [[maybe_unused]] auto moved = std::move(original);
        // moved should be valid
    }

    SECTION("segment with move-only types") {
        auto sut = chains::segment{chains::type<std::tuple<>>{}, [](auto f) { f(); },
                                   [m = stlab::move_only(42)]() { return m.member(); }};
        auto moved = std::move(sut);
        // moved should be valid
    }
}

TEST_CASE("Segment result_type_helper", "[segment]") {
    SECTION("single function returning int") {
        auto sut =
            chains::segment{chains::type<std::tuple<>>{}, [](auto f) { f(); }, []() { return 42; }};
        auto result = std::move(sut).result_type_helper();
        CHECK(result == 42);
    }

    SECTION("function chain with transformations") {
        auto sut = chains::segment{chains::type<std::tuple<>>{}, [](auto f) { f(); },
                                   [](int x) { return x + 1; }, [](int x) { return x * 2; }};
        auto result = std::move(sut).result_type_helper(5);
        CHECK(result == 12); // (5 + 1) * 2 = 12
    }

    SECTION("function chain returning string") {
        auto sut =
            chains::segment{chains::type<std::tuple<>>{}, [](auto f) { f(); },
                            [](int x) { return x * 2; }, [](int x) { return std::to_string(x); }};
        auto result = std::move(sut).result_type_helper(21);
        CHECK(result == "42");
    }

    SECTION("void returning function") {
        auto hit = 0;
        auto sut = chains::segment{chains::type<std::tuple<>>{}, [](auto f) { f(); },
                                   [&hit](int x) { hit = x; }};
        std::move(sut).result_type_helper(42);
        CHECK(hit == 42);
    }
}

TEST_CASE("Segment invoke with receiver", "[segment]") {
    SECTION("invoke with non-canceled receiver") {
        auto receiver = std::make_shared<mock_receiver>();
        auto hit = 0;
        auto sut =
            chains::segment{chains::type<std::tuple<>>{}, [](auto f, auto... args) { f(args...); },
                            [&hit](int x) { hit = x; }};

        std::move(sut).invoke(receiver, 42);
        CHECK(hit == 42);
        CHECK(receiver->_exception == nullptr);
    }

    SECTION("invoke with canceled receiver") {
        auto receiver = std::make_shared<mock_receiver>();
        receiver->_canceled = true;
        auto hit = 0;
        auto sut =
            chains::segment{chains::type<std::tuple<>>{}, [](auto f, auto... args) { f(args...); },
                            [&hit](int x) { hit = x; }};

        std::move(sut).invoke(receiver, 42);
        CHECK(hit == 0); // Should not execute
    }

    SECTION("invoke with exception in segment") {
        auto receiver = std::make_shared<mock_receiver>();
        auto sut =
            chains::segment{chains::type<std::tuple<>>{}, [](auto f, auto... args) { f(args...); },
                            [](int x) {
                                if (x == 42) throw std::runtime_error("test error");
                                return x;
                            }};

        std::move(sut).invoke(receiver, 42);
        CHECK(receiver->_exception != nullptr);

        bool caught_exception = false;
        try {
            std::rethrow_exception(receiver->_exception);
        } catch (const std::runtime_error& e) {
            caught_exception = true;
            CHECK(std::string(e.what()) == "test error");
        }
        CHECK(caught_exception);
    }

    SECTION("invoke with applicator that modifies behavior") {
        auto receiver = std::make_shared<mock_receiver>();
        auto hit = 0;

        // Custom applicator that doubles the argument
        auto custom_apply = [](auto f, int x) { f(x * 2); };

        auto sut = chains::segment{chains::type<std::tuple<>>{}, std::move(custom_apply),
                                   [&hit](int x) { hit = x; }};

        std::move(sut).invoke(receiver, 21);
        CHECK(hit == 42); // 21 * 2 = 42
    }

    SECTION("invoke with chained functions") {
        auto receiver = std::make_shared<mock_receiver>();
        auto result = 0;
        auto sut =
            chains::segment{chains::type<std::tuple<>>{}, [](auto f, auto... args) { f(args...); },
                            [](int x) { return x + 1; }, [](int x) { return x * 2; },
                            [&result](int x) { result = x; }};

        std::move(sut).invoke(receiver, 5);
        CHECK(result == 12); // (5 + 1) * 2 = 12
        CHECK(receiver->_exception == nullptr);
    }
}

TEST_CASE("Segment with injected types", "[segment]") {
    SECTION("segment with int injection") {
        auto sut = chains::segment{chains::type<std::tuple<int>>{}, [](auto f) { f(); },
                                   []() { return 42; }};
        // Segment should be constructible with injection type
    }

    SECTION("segment with multiple injection types") {
        auto sut = chains::segment{chains::type<std::tuple<int, std::string>>{},
                                   [](auto f) { f(); }, []() { return 42; }};
        // Segment should be constructible with multiple injection types
    }
}

TEST_CASE("Segment edge cases", "[segment]") {
    SECTION("empty segment with no functions") {
        auto sut =
            chains::segment{chains::type<std::tuple<>>{}, [](auto f) { f(); }, std::tuple<>{}};
        // Should be constructible
    }

    SECTION("segment with void function") {
        auto hit = false;
        auto sut = chains::segment{chains::type<std::tuple<>>{}, [](auto f) { f(); },
                                   [&hit]() { hit = true; }};
        std::move(sut).result_type_helper();
        CHECK(hit);
    }

    SECTION("segment with multiple void functions") {
        auto hit1 = false;
        auto hit2 = false;
        auto sut = chains::segment{chains::type<std::tuple<>>{}, [](auto f) { f(); },
                                   [&hit1]() { hit1 = true; }, [&hit2]() { hit2 = true; }};
        std::move(sut).result_type_helper();
        CHECK(hit1);
        CHECK(hit2);
    }

    SECTION("segment with variadic function") {
        auto sut = chains::segment{chains::type<std::tuple<>>{}, [](auto f) { f(); },
                                   [](auto... args) { return (args + ...); }};
        auto result = std::move(sut).result_type_helper(1, 2, 3, 4);
        CHECK(result == 10);
    }
}