include(cmake/SystemLink.cmake)
# include(cmake/LibFuzzer.cmake)
include(CMakeDependentOption)
include(CheckCXXCompilerFlag)


include(CheckCXXSourceCompiles)


macro(chains_supports_sanitizers)
  # Emscripten doesn't support sanitizers
  if(EMSCRIPTEN)
    set(SUPPORTS_UBSAN OFF)
    set(SUPPORTS_ASAN OFF)
  elseif((CMAKE_CXX_COMPILER_ID MATCHES ".*Clang.*" OR CMAKE_CXX_COMPILER_ID MATCHES ".*GNU.*") AND NOT WIN32)

    message(STATUS "Sanity checking UndefinedBehaviorSanitizer, it should be supported on this platform")
    set(TEST_PROGRAM "int main() { return 0; }")

    # Check if UndefinedBehaviorSanitizer works at link time
    set(CMAKE_REQUIRED_FLAGS "-fsanitize=undefined")
    set(CMAKE_REQUIRED_LINK_OPTIONS "-fsanitize=undefined")
    check_cxx_source_compiles("${TEST_PROGRAM}" HAS_UBSAN_LINK_SUPPORT)

    if(HAS_UBSAN_LINK_SUPPORT)
      message(STATUS "UndefinedBehaviorSanitizer is supported at both compile and link time.")
      set(SUPPORTS_UBSAN ON)
    else()
      message(WARNING "UndefinedBehaviorSanitizer is NOT supported at link time.")
      set(SUPPORTS_UBSAN OFF)
    endif()
  else()
    set(SUPPORTS_UBSAN OFF)
  endif()

  if((CMAKE_CXX_COMPILER_ID MATCHES ".*Clang.*" OR CMAKE_CXX_COMPILER_ID MATCHES ".*GNU.*") AND WIN32)
    set(SUPPORTS_ASAN OFF)
  else()
    if (NOT WIN32)
      message(STATUS "Sanity checking AddressSanitizer, it should be supported on this platform")
      set(TEST_PROGRAM "int main() { return 0; }")

      # Check if AddressSanitizer works at link time
      set(CMAKE_REQUIRED_FLAGS "-fsanitize=address")
      set(CMAKE_REQUIRED_LINK_OPTIONS "-fsanitize=address")
      check_cxx_source_compiles("${TEST_PROGRAM}" HAS_ASAN_LINK_SUPPORT)

      if(HAS_ASAN_LINK_SUPPORT)
        message(STATUS "AddressSanitizer is supported at both compile and link time.")
        set(SUPPORTS_ASAN ON)
      else()
        message(WARNING "AddressSanitizer is NOT supported at link time.")
        set(SUPPORTS_ASAN OFF)
      endif()
    else()
      set(SUPPORTS_ASAN ON)
    endif()
  endif()
endmacro()

macro(chains_setup_options)
  option(chains_ENABLE_HARDENING "Enable hardening" ON)
  option(chains_ENABLE_COVERAGE "Enable coverage reporting" OFF)
  cmake_dependent_option(
    chains_ENABLE_GLOBAL_HARDENING
    "Attempt to push hardening options to built dependencies"
    ON
    chains_ENABLE_HARDENING
    OFF)

  chains_supports_sanitizers()

  if(NOT PROJECT_IS_TOP_LEVEL OR chains_PACKAGING_MAINTAINER_MODE)
    option(chains_ENABLE_IPO "Enable IPO/LTO" OFF)
    option(chains_WARNINGS_AS_ERRORS "Treat Warnings As Errors" OFF)

    option(chains_ENABLE_SANITIZER_ADDRESS "Enable address sanitizer" OFF)
    option(chains_ENABLE_SANITIZER_LEAK "Enable leak sanitizer" OFF)
    option(chains_ENABLE_SANITIZER_UNDEFINED "Enable undefined sanitizer" OFF)
    option(chains_ENABLE_SANITIZER_THREAD "Enable thread sanitizer" OFF)
    option(chains_ENABLE_SANITIZER_MEMORY "Enable memory sanitizer" OFF)
    option(chains_ENABLE_UNITY_BUILD "Enable unity builds" OFF)
    option(chains_ENABLE_CLANG_TIDY "Enable clang-tidy" OFF)
    option(chains_ENABLE_CPPCHECK "Enable cpp-check analysis" OFF)
    option(chains_ENABLE_PCH "Enable precompiled headers" OFF)
    option(chains_ENABLE_CACHE "Enable ccache" OFF)
  else()
    option(chains_ENABLE_IPO "Enable IPO/LTO" ON)
    option(chains_WARNINGS_AS_ERRORS "Treat Warnings As Errors" ON)

    option(chains_ENABLE_SANITIZER_ADDRESS "Enable address sanitizer" ${SUPPORTS_ASAN})
    option(chains_ENABLE_SANITIZER_LEAK "Enable leak sanitizer" OFF)
    option(chains_ENABLE_SANITIZER_UNDEFINED "Enable undefined sanitizer" ${SUPPORTS_UBSAN})
    option(chains_ENABLE_SANITIZER_THREAD "Enable thread sanitizer" OFF)
    option(chains_ENABLE_SANITIZER_MEMORY "Enable memory sanitizer" OFF)
    option(chains_ENABLE_UNITY_BUILD "Enable unity builds" OFF)
    option(chains_ENABLE_CLANG_TIDY "Enable clang-tidy" ON)
    option(chains_ENABLE_CPPCHECK "Enable cpp-check analysis" ON)
    option(chains_ENABLE_PCH "Enable precompiled headers" OFF)
    option(chains_ENABLE_CACHE "Enable ccache" ON)
  endif()

  if(NOT PROJECT_IS_TOP_LEVEL)
    mark_as_advanced(
      chains_ENABLE_IPO
      chains_WARNINGS_AS_ERRORS

      chains_ENABLE_SANITIZER_ADDRESS
      chains_ENABLE_SANITIZER_LEAK
      chains_ENABLE_SANITIZER_UNDEFINED
      chains_ENABLE_SANITIZER_THREAD
      chains_ENABLE_SANITIZER_MEMORY
      chains_ENABLE_UNITY_BUILD
      chains_ENABLE_CLANG_TIDY
      chains_ENABLE_CPPCHECK
      chains_ENABLE_COVERAGE
      chains_ENABLE_PCH
      chains_ENABLE_CACHE)
  endif()

  # chains_check_libfuzzer_support(LIBFUZZER_SUPPORTED)
  # if(LIBFUZZER_SUPPORTED AND (chains_ENABLE_SANITIZER_ADDRESS OR chains_ENABLE_SANITIZER_THREAD OR chains_ENABLE_SANITIZER_UNDEFINED))
  #   set(DEFAULT_FUZZER ON)
  # else()
  #   set(DEFAULT_FUZZER OFF)
  # endif()

  # option(chains_BUILD_FUZZ_TESTS "Enable fuzz testing executable" ${DEFAULT_FUZZER})

endmacro()

macro(chains_global_options)
  if(chains_ENABLE_IPO)
    include(cmake/InterproceduralOptimization.cmake)
    chains_enable_ipo()
  endif()

  chains_supports_sanitizers()

  if(chains_ENABLE_HARDENING AND chains_ENABLE_GLOBAL_HARDENING)
    include(cmake/Hardening.cmake)
    if(NOT SUPPORTS_UBSAN 
       OR chains_ENABLE_SANITIZER_UNDEFINED
       OR chains_ENABLE_SANITIZER_ADDRESS
       OR chains_ENABLE_SANITIZER_THREAD
       OR chains_ENABLE_SANITIZER_LEAK)
      set(ENABLE_UBSAN_MINIMAL_RUNTIME FALSE)
    else()
      set(ENABLE_UBSAN_MINIMAL_RUNTIME TRUE)
    endif()
    message("${chains_ENABLE_HARDENING} ${ENABLE_UBSAN_MINIMAL_RUNTIME} ${chains_ENABLE_SANITIZER_UNDEFINED}")
    chains_enable_hardening(chains_options ON ${ENABLE_UBSAN_MINIMAL_RUNTIME})
  endif()
endmacro()

macro(chains_local_options)
  if(PROJECT_IS_TOP_LEVEL)
    include(cmake/StandardProjectSettings.cmake)
  endif()

  add_library(chains_warnings INTERFACE)
  add_library(chains_options INTERFACE)

  include(cmake/CompilerWarnings.cmake)
  chains_set_project_warnings(
    chains_warnings
    ${chains_WARNINGS_AS_ERRORS}
    ""
    ""
    ""
    "")

  include(cmake/Linker.cmake)
  # Must configure each target with linker options, we're avoiding setting it globally for now

  include(cmake/Sanitizers.cmake)
  chains_enable_sanitizers(
    chains_options
    ${chains_ENABLE_SANITIZER_ADDRESS}
    ${chains_ENABLE_SANITIZER_LEAK}
    ${chains_ENABLE_SANITIZER_UNDEFINED}
    ${chains_ENABLE_SANITIZER_THREAD}
    ${chains_ENABLE_SANITIZER_MEMORY})

  set_target_properties(chains_options PROPERTIES UNITY_BUILD ${chains_ENABLE_UNITY_BUILD})

  if(chains_ENABLE_PCH)
    target_precompile_headers(
      chains_options
      INTERFACE
      <vector>
      <string>
      <utility>)
  endif()

  if(chains_ENABLE_CACHE)
    include(cmake/Cache.cmake)
    chains_enable_cache()
  endif()

  include(cmake/StaticAnalyzers.cmake)
  if(chains_ENABLE_CLANG_TIDY)
    chains_enable_clang_tidy(chains_options ${chains_WARNINGS_AS_ERRORS})
  endif()

  if(chains_ENABLE_CPPCHECK)
    chains_enable_cppcheck(${chains_WARNINGS_AS_ERRORS} "" # override cppcheck options
    )
  endif()

  if(chains_ENABLE_COVERAGE)
    include(cmake/Tests.cmake)
    chains_enable_coverage(chains_options)
  endif()

  if(chains_WARNINGS_AS_ERRORS)
    check_cxx_compiler_flag("-Wl,--fatal-warnings" LINKER_FATAL_WARNINGS)
    if(LINKER_FATAL_WARNINGS)
      # This is not working consistently, so disabling for now
      # target_link_options(chains_options INTERFACE -Wl,--fatal-warnings)
    endif()
  endif()

  if(chains_ENABLE_HARDENING AND NOT chains_ENABLE_GLOBAL_HARDENING)
    include(cmake/Hardening.cmake)
    if(NOT SUPPORTS_UBSAN 
       OR chains_ENABLE_SANITIZER_UNDEFINED
       OR chains_ENABLE_SANITIZER_ADDRESS
       OR chains_ENABLE_SANITIZER_THREAD
       OR chains_ENABLE_SANITIZER_LEAK)
      set(ENABLE_UBSAN_MINIMAL_RUNTIME FALSE)
    else()
      set(ENABLE_UBSAN_MINIMAL_RUNTIME TRUE)
    endif()
    chains_enable_hardening(chains_options OFF ${ENABLE_UBSAN_MINIMAL_RUNTIME})
  endif()

endmacro()
