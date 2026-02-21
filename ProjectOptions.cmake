include(cmake/SystemLink.cmake)
# include(cmake/LibFuzzer.cmake)
include(CMakeDependentOption)
include(CheckCXXCompilerFlag)


include(CheckCXXSourceCompiles)


macro(chain_supports_sanitizers)
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

macro(chain_setup_options)
  option(chain_ENABLE_HARDENING "Enable hardening" ON)
  option(chain_ENABLE_COVERAGE "Enable coverage reporting" OFF)
  cmake_dependent_option(
    chain_ENABLE_GLOBAL_HARDENING
    "Attempt to push hardening options to built dependencies"
    ON
    chain_ENABLE_HARDENING
    OFF)

  chain_supports_sanitizers()

  if(NOT PROJECT_IS_TOP_LEVEL OR chain_PACKAGING_MAINTAINER_MODE)
    option(chain_ENABLE_IPO "Enable IPO/LTO" OFF)
    option(chain_WARNINGS_AS_ERRORS "Treat Warnings As Errors" OFF)

    option(chain_ENABLE_SANITIZER_ADDRESS "Enable address sanitizer" OFF)
    option(chain_ENABLE_SANITIZER_LEAK "Enable leak sanitizer" OFF)
    option(chain_ENABLE_SANITIZER_UNDEFINED "Enable undefined sanitizer" OFF)
    option(chain_ENABLE_SANITIZER_THREAD "Enable thread sanitizer" OFF)
    option(chain_ENABLE_SANITIZER_MEMORY "Enable memory sanitizer" OFF)
    option(chain_ENABLE_UNITY_BUILD "Enable unity builds" OFF)
    option(chain_ENABLE_CLANG_TIDY "Enable clang-tidy" OFF)
    option(chain_ENABLE_CPPCHECK "Enable cpp-check analysis" OFF)
    option(chain_ENABLE_PCH "Enable precompiled headers" OFF)
    option(chain_ENABLE_CACHE "Enable ccache" OFF)
  else()
    option(chain_ENABLE_IPO "Enable IPO/LTO" ON)
    option(chain_WARNINGS_AS_ERRORS "Treat Warnings As Errors" ON)

    option(chain_ENABLE_SANITIZER_ADDRESS "Enable address sanitizer" ${SUPPORTS_ASAN})
    option(chain_ENABLE_SANITIZER_LEAK "Enable leak sanitizer" OFF)
    option(chain_ENABLE_SANITIZER_UNDEFINED "Enable undefined sanitizer" ${SUPPORTS_UBSAN})
    option(chain_ENABLE_SANITIZER_THREAD "Enable thread sanitizer" OFF)
    option(chain_ENABLE_SANITIZER_MEMORY "Enable memory sanitizer" OFF)
    option(chain_ENABLE_UNITY_BUILD "Enable unity builds" OFF)
    option(chain_ENABLE_CLANG_TIDY "Enable clang-tidy" ON)
    option(chain_ENABLE_CPPCHECK "Enable cpp-check analysis" ON)
    option(chain_ENABLE_PCH "Enable precompiled headers" OFF)
    option(chain_ENABLE_CACHE "Enable ccache" ON)
  endif()

  if(NOT PROJECT_IS_TOP_LEVEL)
    mark_as_advanced(
      chain_ENABLE_IPO
      chain_WARNINGS_AS_ERRORS

      chain_ENABLE_SANITIZER_ADDRESS
      chain_ENABLE_SANITIZER_LEAK
      chain_ENABLE_SANITIZER_UNDEFINED
      chain_ENABLE_SANITIZER_THREAD
      chain_ENABLE_SANITIZER_MEMORY
      chain_ENABLE_UNITY_BUILD
      chain_ENABLE_CLANG_TIDY
      chain_ENABLE_CPPCHECK
      chain_ENABLE_COVERAGE
      chain_ENABLE_PCH
      chain_ENABLE_CACHE)
  endif()

  # chain_check_libfuzzer_support(LIBFUZZER_SUPPORTED)
  # if(LIBFUZZER_SUPPORTED AND (chain_ENABLE_SANITIZER_ADDRESS OR chain_ENABLE_SANITIZER_THREAD OR chain_ENABLE_SANITIZER_UNDEFINED))
  #   set(DEFAULT_FUZZER ON)
  # else()
  #   set(DEFAULT_FUZZER OFF)
  # endif()

  # option(chain_BUILD_FUZZ_TESTS "Enable fuzz testing executable" ${DEFAULT_FUZZER})

endmacro()

macro(chain_global_options)
  if(chain_ENABLE_IPO)
    include(cmake/InterproceduralOptimization.cmake)
    chain_enable_ipo()
  endif()

  chain_supports_sanitizers()

  if(chain_ENABLE_HARDENING AND chain_ENABLE_GLOBAL_HARDENING)
    include(cmake/Hardening.cmake)
    if(NOT SUPPORTS_UBSAN 
       OR chain_ENABLE_SANITIZER_UNDEFINED
       OR chain_ENABLE_SANITIZER_ADDRESS
       OR chain_ENABLE_SANITIZER_THREAD
       OR chain_ENABLE_SANITIZER_LEAK)
      set(ENABLE_UBSAN_MINIMAL_RUNTIME FALSE)
    else()
      set(ENABLE_UBSAN_MINIMAL_RUNTIME TRUE)
    endif()
    message("${chain_ENABLE_HARDENING} ${ENABLE_UBSAN_MINIMAL_RUNTIME} ${chain_ENABLE_SANITIZER_UNDEFINED}")
    chain_enable_hardening(chain_options ON ${ENABLE_UBSAN_MINIMAL_RUNTIME})
  endif()
endmacro()

macro(chain_local_options)
  if(PROJECT_IS_TOP_LEVEL)
    include(cmake/StandardProjectSettings.cmake)
  endif()

  add_library(chain_warnings INTERFACE)
  add_library(chain_options INTERFACE)

  include(cmake/CompilerWarnings.cmake)
  chain_set_project_warnings(
    chain_warnings
    ${chain_WARNINGS_AS_ERRORS}
    ""
    ""
    ""
    "")

  include(cmake/Linker.cmake)
  # Must configure each target with linker options, we're avoiding setting it globally for now

  include(cmake/Sanitizers.cmake)
  chain_enable_sanitizers(
    chain_options
    ${chain_ENABLE_SANITIZER_ADDRESS}
    ${chain_ENABLE_SANITIZER_LEAK}
    ${chain_ENABLE_SANITIZER_UNDEFINED}
    ${chain_ENABLE_SANITIZER_THREAD}
    ${chain_ENABLE_SANITIZER_MEMORY})

  set_target_properties(chain_options PROPERTIES UNITY_BUILD ${chain_ENABLE_UNITY_BUILD})

  if(chain_ENABLE_PCH)
    target_precompile_headers(
      chain_options
      INTERFACE
      <vector>
      <string>
      <utility>)
  endif()

  if(chain_ENABLE_CACHE)
    include(cmake/Cache.cmake)
    chain_enable_cache()
  endif()

  include(cmake/StaticAnalyzers.cmake)
  if(chain_ENABLE_CLANG_TIDY)
    chain_enable_clang_tidy(chain_options ${chain_WARNINGS_AS_ERRORS})
  endif()

  if(chain_ENABLE_CPPCHECK)
    chain_enable_cppcheck(${chain_WARNINGS_AS_ERRORS} "" # override cppcheck options
    )
  endif()

  if(chain_ENABLE_COVERAGE)
    include(cmake/Tests.cmake)
    chain_enable_coverage(chain_options)
  endif()

  if(chain_WARNINGS_AS_ERRORS)
    check_cxx_compiler_flag("-Wl,--fatal-warnings" LINKER_FATAL_WARNINGS)
    if(LINKER_FATAL_WARNINGS)
      # This is not working consistently, so disabling for now
      # target_link_options(chain_options INTERFACE -Wl,--fatal-warnings)
    endif()
  endif()

  if(chain_ENABLE_HARDENING AND NOT chain_ENABLE_GLOBAL_HARDENING)
    include(cmake/Hardening.cmake)
    if(NOT SUPPORTS_UBSAN 
       OR chain_ENABLE_SANITIZER_UNDEFINED
       OR chain_ENABLE_SANITIZER_ADDRESS
       OR chain_ENABLE_SANITIZER_THREAD
       OR chain_ENABLE_SANITIZER_LEAK)
      set(ENABLE_UBSAN_MINIMAL_RUNTIME FALSE)
    else()
      set(ENABLE_UBSAN_MINIMAL_RUNTIME TRUE)
    endif()
    chain_enable_hardening(chain_options OFF ${ENABLE_UBSAN_MINIMAL_RUNTIME})
  endif()

endmacro()
