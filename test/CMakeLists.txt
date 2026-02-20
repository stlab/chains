cmake_minimum_required(VERSION 3.15...3.23)

project(CmakeConfigPackageTests 
  VERSION ${PROJECT_VERSION}
  LANGUAGES CXX)

# ---- Test as standalone project the exported config package ----

if(PROJECT_IS_TOP_LEVEL OR TEST_INSTALLED_VERSION)
  enable_testing()

  find_package(chains CONFIG REQUIRED) # for intro, project_options, ...

  if(NOT TARGET chains_options)
    message(FATAL_ERROR "Required config package not found!")
    return() # be strictly paranoid for Template Janitor github action! CK
  endif()
endif()

# ---- Dependencies ----

include(${Catch2_SOURCE_DIR}/extras/Catch.cmake)

add_executable(tests
  chains_test.cpp
  main.cpp
  segment_tests.cpp
  tuple_tests.cpp)

target_link_libraries(
  tests
  PRIVATE chains::chains_warnings
          chains::chains_options
          chains::chains
          stlab::stlab
          Catch2::Catch2WithMain)

if(WIN32 AND BUILD_SHARED_LIBS)
  add_custom_command(
    TARGET tests
    PRE_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy $<TARGET_RUNTIME_DLLS:tests> $<TARGET_FILE_DIR:tests>
    COMMAND_EXPAND_LISTS)
endif()

# automatically discover tests that are defined in catch based test files you can modify the unittests. Set TEST_PREFIX
# to whatever you want, or use different for different binaries
catch_discover_tests(
  tests
  TEST_PREFIX
  "unittests."
  REPORTER
  XML
  OUTPUT_DIR
  .
  OUTPUT_PREFIX
  "unittests."
  OUTPUT_SUFFIX
  .xml)

# Add a file containing a set of constexpr tests
# add_executable(constexpr_tests constexpr_tests.cpp)
# target_link_libraries(
#   constexpr_tests
#   PRIVATE chains::chains_warnings
#           chains::chains_options
#           chains::sample_library
#           Catch2::Catch2WithMain)

# catch_discover_tests(
#   constexpr_tests
#   TEST_PREFIX
#   "constexpr."
#   REPORTER
#   XML
#   OUTPUT_DIR
#   .
#   OUTPUT_PREFIX
#   "constexpr."
#   OUTPUT_SUFFIX
#   .xml)

# Disable the constexpr portion of the test, and build again this allows us to have an executable that we can debug when
# things go wrong with the constexpr testing
# add_executable(relaxed_constexpr_tests constexpr_tests.cpp)
# target_link_libraries(
#   relaxed_constexpr_tests
#   PRIVATE chains::chains_warnings
#           chains::chains_options
#           chains::sample_library
#           Catch2::Catch2WithMain)
# target_compile_definitions(relaxed_constexpr_tests PRIVATE -DCATCH_CONFIG_RUNTIME_STATIC_REQUIRE)

# catch_discover_tests(
#   relaxed_constexpr_tests
#   TEST_PREFIX
#   "relaxed_constexpr."
#   REPORTER
#   XML
#   OUTPUT_DIR
#   .
#   OUTPUT_PREFIX
#   "relaxed_constexpr."
#   OUTPUT_SUFFIX
#   .xml)
