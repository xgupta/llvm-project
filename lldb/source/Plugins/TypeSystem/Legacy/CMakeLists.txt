add_lldb_library(lldbPluginTypeSystemLegacy PLUGIN
  TypeSystemLegacy.cpp

  LINK_LIBS
    lldbCore
    lldbSymbol
    lldbTarget
    lldbUtility
    lldbPluginExpressionParserCobol
    lldbPluginSymbolFileDWARF
    lldbPluginSymbolFilePDB
    lldbPluginObjCRuntime
  LINK_COMPONENTS
    Support
)
