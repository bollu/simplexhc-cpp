cmake_minimum_required(VERSION 3.4.3)
project(simplexhc CXX)

# LLVM
find_package(LLVM REQUIRED)
llvm_map_components_to_libnames(LLVM_LIBS Core support)

# FLEX AND BISON
find_package(BISON)
find_package(FLEX)
bison_target(Parser src/parser.yy ${CMAKE_CURRENT_SOURCE_DIR}/src/parser.generated.cpp)
flex_target(Lexer src/lexer.l  ${CMAKE_CURRENT_SOURCE_DIR}/src/lexer.generated.cpp)
add_flex_bison_dependency(Lexer Parser)


# INCLUDES AND SOURCES
include_directories(include)

add_executable(simplexhc
    src/main.cpp
    ${BISON_Parser_OUTPUTS}
    ${FLEX_Lexer_OUTPUTS})

target_link_libraries(simplexhc ${LLVM_LIBS})
set_target_properties(simplexhc PROPERTIES CXX_STANDARD 14)


