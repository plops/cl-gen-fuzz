cmake_minimum_required(VERSION 3.8)
project(source)
set(CMAKE_CXX_COMPILER             "/usr/bin/clang++-5.0")
set(LIBFUZZER_FLAGS_BASE "${CMAKE_CXX_FLAGS}")
set(CMAKE_CXX_FLAGS "-g -fno-omit-frame-pointer -fsanitize=address -fsanitize-coverage=trace-pc-guard,edge,trace-cmp,indirect-calls -Werror")
set(CMAKE_CXX_STANDARD 11)

set(SOURCE_FILES
        main.cpp)

add_executable(source ${SOURCE_FILES})
#xtarget_link_libraries(source /usr/lib/llvm-5.0/lib/libFuzzer.a)
target_link_libraries(source pthread)
