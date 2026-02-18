/*
    Copyright 2026 Adobe
    Distributed under the Boost Software License, Version 1.0.
    (See accompanying file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)
*/

#define CATCH_CONFIG_RUNNER

#include <catch2/catch_session.hpp>

// #include <stlab/pre_exit.hpp>

auto main(int argc, char** argv) -> int {
    int result = Catch::Session().run(argc, argv);

    // stlab::pre_exit();

    return result;
}