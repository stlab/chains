target_sources( chain INTERFACE
        FILE_SET headers
        TYPE HEADERS
        BASE_DIRS ../
        FILES

        chain.hpp
        config.hpp
        #on.hpp
        segment.hpp
        #split.hpp
        #start.hpp
        #sync_wait.hpp
        #then.hpp
        tuple.hpp
)

target_sources( chain INTERFACE
        FILE_SET headers
        TYPE HEADERS
        BASE_DIRS ${PROJECT_BINARY_DIR}/include
)
