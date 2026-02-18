macro(chains_configure_linker project_name)
  set(alb_USER_LINKER_OPTION
    "DEFAULT"
      CACHE STRING "Linker to be used")
    set(alb_USER_LINKER_OPTION_VALUES "DEFAULT" "SYSTEM" "LLD" "GOLD" "BFD" "MOLD" "SOLD" "APPLE_CLASSIC" "MSVC")
  set_property(CACHE alb_USER_LINKER_OPTION PROPERTY STRINGS ${alb_USER_LINKER_OPTION_VALUES})
  list(
    FIND
    alb_USER_LINKER_OPTION_VALUES
    ${alb_USER_LINKER_OPTION}
    alb_USER_LINKER_OPTION_INDEX)

  if(${alb_USER_LINKER_OPTION_INDEX} EQUAL -1)
    message(
      STATUS
        "Using custom linker: '${alb_USER_LINKER_OPTION}', explicitly supported entries are ${alb_USER_LINKER_OPTION_VALUES}")
  endif()

  set_target_properties(${project_name} PROPERTIES LINKER_TYPE "${alb_USER_LINKER_OPTION}")
endmacro()
