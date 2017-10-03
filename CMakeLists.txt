cmake_minimum_required(VERSION 3.4.3)
project(simplexhc C CXX)

# Colors when compiling with clang
set(CMAKE_CXX_FLAGS "-fdiagnostics-color=always -fno-rtti -g")

# LLVM
find_package(LLVM REQUIRED)
llvm_map_components_to_libnames(LLVM_LIBS Core Support Bitwriter)

# FLEX AND BISON
find_package(BISON)
find_package(FLEX)
bison_target(Parser src/parser.yy ${CMAKE_SOURCE_DIR}/src/parser.generated.cpp)
flex_target(Lexer src/lexer.l  ${CMAKE_SOURCE_DIR}/src/lexer.generated.cpp COMPILE_FLAGS -d)
# flex_target(Lexer src/lexer.l  ${CMAKE_SOURCE_DIR}/src/lexer.generated.cpp)
add_flex_bison_dependency(Lexer Parser)


# INCLUDES AND SOURCES
include_directories(include)

add_executable(simplexhc
    src/main.cpp
    src/stgir.cpp
    ${BISON_Parser_OUTPUTS}
    ${FLEX_Lexer_OUTPUTS})

target_link_libraries(simplexhc ${LLVM_LIBS} -lstdc++)
set_target_properties(simplexhc PROPERTIES CXX_STANDARD 14)

# CLANG FORMAT
file(GLOB_RECURSE ALL_SOURCE_FILES *.cpp *.h)

find_program(CLANG_FORMAT clang-format)
add_custom_target(
        format
        COMMAND ${CLANG_FORMAT}
        -style=LLVM
        -i
        ${ALL_SOURCE_FILES}
)

# libstg
add_library(stgruntime
    STATIC
    lib/libstg.c)
set_target_properties(stgruntime PROPERTIES C_STANDARD 99)
target_link_libraries(stgruntime -lstdc)

# DOXYGEN
# first we can indicate the documentation build as an option and set it to ON by default
option(BUILD_DOC "Build documentation" OFF)

# check if Doxygen is installed
find_package(Doxygen)
if (DOXYGEN_FOUND)
    # set input and output files
    set(DOXYGEN_IN ${CMAKE_CURRENT_SOURCE_DIR}/docs/Doxyfile.in)
    set(DOXYGEN_OUT ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile)

    # request to configure the file
    configure_file(${DOXYGEN_IN} ${DOXYGEN_OUT} @ONLY)
    message("Doxygen build started")

    # note the option ALL which allows to build the docs together with the application
    add_custom_target(doc
        COMMAND ${DOXYGEN_EXECUTABLE} ${DOXYGEN_OUT}
        WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
        COMMENT "Generating API documentation with Doxygen"
        VERBATIM )
else (DOXYGEN_FOUND)
  message("Doxygen need to be installed to generate the doxygen documentation")
endif (DOXYGEN_FOUND)
