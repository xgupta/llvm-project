add_lldb_library(lldbPluginPLILanguage PLUGIN
  PLILanguage.cpp
  PLIBitset.cpp

  LINK_LIBS
    lldbCore
    lldbDataFormatters
    lldbExpression
    lldbHost
    lldbSymbol
    lldbTarget
    lldbUtility
)
