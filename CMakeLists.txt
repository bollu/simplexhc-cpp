cmake_minimum_required(VERSION 3.4.3)
project(sxhc C CXX)

# Colors when compiling with clang
# set(CMAKE_CXX_FLAGS "-fdiagnostics-color=always -fno-rtti -g -Wall -Werror")
set(CMAKE_CXX_FLAGS "-fdiagnostics-color=always -fno-rtti -g -Wall")

# LLVM
find_package(LLVM REQUIRED)

# Boost
find_package(Boost REQUIRED)
# HACK: I'm using all. I need to ask on the mailing list how to fix this.
llvm_map_components_to_libnames(LLVM_LIBS AllTargetsAsmPrinters 
    AllTargetsAsmParsers Core Support ORCJIT MCJIT ExecutionEngine
    ASMPrinter CodeGen AllTargetsDescs AllTargetsInfos TransformUtils Object
    NVPTXCodeGen ipo Passes Analysis
    nativecodegen MC)

# FLEX AND BISON
find_package(BISON)
find_package(FLEX)
bison_target(Parser src/parser.yy ${CMAKE_SOURCE_DIR}/src/parser.generated.cpp COMPILE_FLAGS -v)
flex_target(Lexer src/lexer.l  ${CMAKE_SOURCE_DIR}/src/lexer.generated.cpp)
add_flex_bison_dependency(Lexer Parser)


# INCLUDES AND SOURCES
include_directories(include ${LLVM_INCLUDE_DIRS} include/sxhc)

add_executable(sxhc
    src/main.cpp
    src/RuntimeDebugBuilder.cpp
    src/stgir.cpp
    src/StackAnalysis.cpp
    lib/libstg.c
    ${BISON_Parser_OUTPUTS}
    ${FLEX_Lexer_OUTPUTS}
        include/sxhc/LazyCodegen.h)

target_link_libraries(sxhc ${LLVM_LIBS}  -lstdc++)
set_target_properties(sxhc PROPERTIES CXX_STANDARD 14)

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
    SHARED lib/libstg.c include/sxhc/LazyCodegen.h)
set_target_properties(stgruntime PROPERTIES C_STANDARD 99)

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

