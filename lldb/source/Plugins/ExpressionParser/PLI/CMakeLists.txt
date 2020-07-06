add_lldb_library(lldbPluginExpressionParserPLI
    PLILexer.cpp
    PLIParser.cpp
    PLIUserExpression.cpp

  LINK_LIBS
    lldbCore
    lldbExpression
    lldbSymbol
    lldbTarget
  LINK_COMPONENTS
    Support
  )
