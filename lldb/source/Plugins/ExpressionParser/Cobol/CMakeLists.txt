add_lldb_library(lldbPluginExpressionParserCobol
    CobolLexer.cpp
    CobolParser.cpp
    CobolUserExpression.cpp

  LINK_LIBS
    lldbCore
    lldbExpression
    lldbSymbol
    lldbTarget
  LINK_COMPONENTS
    Support
  )
