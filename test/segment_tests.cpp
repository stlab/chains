/*
    Copyright 2026 Adobe
    Distributed under the Boost Software License, Version 1.0.
    (See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
*/

#include <chains/segment.hpp>

#include <catch2/catch_test_macros.hpp>
#include <stlab/test/model.hpp> // moveonly

TEST_CASE("Basic segment operations", "[segment]") {
    SECTION("simple creation") {
        auto sut = chains::make_segment<std::tuple<>>([](auto f) { f(); }, []() { return 42;});
        CHECK(42 == sut.invoke());
    }
    
}