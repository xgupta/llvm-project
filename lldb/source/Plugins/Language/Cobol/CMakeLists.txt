add_lldb_library(lldbPluginCobolLanguage PLUGIN
  CobolLanguage.cpp

  LINK_LIBS
    lldbCore
    lldbDataFormatters
    lldbExpression
    lldbHost
    lldbSymbol
    lldbTarget
    lldbUtility
)
