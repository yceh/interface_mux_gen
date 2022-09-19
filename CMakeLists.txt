cmake_minimum_required(VERSION 3.22)
add_subdirectory(fmt)

# Bring in the required packages
find_package(antlr4-runtime REQUIRED)
find_package(antlr4-generator REQUIRED)
# Set path to generator
set(ANTLR4_JAR_LOCATION ${PROJECT_SOURCE_DIR}/build/antlr-4.11.1-complete.jar)
# generate lexer
antlr4_generate(
  antlrcpptest_lexer
  ${CMAKE_CURRENT_SOURCE_DIR}/SystemVerilogLexer.g4
  LEXER
  FALSE
  FALSE
  "antlrcpptest"
  )

# generate parser
antlr4_generate(
  antlrcpptest_parser
  ${CMAKE_CURRENT_SOURCE_DIR}/SystemVerilogParser.g4
  PARSER
  FALSE
  TRUE
  "antlrcpptest"
  "${ANTLR4_TOKEN_FILES_antlrcpptest_lexer}"
  "${ANTLR4_TOKEN_DIRECTORY_antlrcpptest_lexer}"
  )

# add directories for generated include files
include_directories( ${PROJECT_BINARY_DIR} ${ANTLR4_INCLUDE_DIR} ${ANTLR4_INCLUDE_DIR_antlrcpptest_lexer} ${ANTLR4_INCLUDE_DIR_antlrcpptest_parser} )

# add generated source files
add_executable( Parsertest main.cpp ${ANTLR4_SRC_FILES_antlrcpptest_lexer} ${ANTLR4_SRC_FILES_antlrcpptest_parser} )

# add required runtime library
add_dependencies( Parsertest antlr4_shared )

target_link_libraries( Parsertest PRIVATE
                       antlr4_shared fmt::fmt)

set_property(TARGET Parsertest PROPERTY CXX_STANDARD 17)