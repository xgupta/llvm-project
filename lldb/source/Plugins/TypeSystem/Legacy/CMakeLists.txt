add_lldb_library(lldbPluginTypeSystemLegacy PLUGIN
  TypeSystemLegacy.cpp

  LINK_LIBS
    lldbCore
    lldbSymbol
    lldbTarget
    lldbUtility
    lldbPluginSymbolFileDWARF
    lldbPluginSymbolFilePDB
    lldbPluginObjCRuntime
  LINK_COMPONENTS
    Support
)

