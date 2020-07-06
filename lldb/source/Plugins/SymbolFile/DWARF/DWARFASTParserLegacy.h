//===-- DWARFASTParserLegacy.h---------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef SymbolFileDWARF_DWARFASTParserLegacy_h_
#define SymbolFileDWARF_DWARFASTParserLegacy_h_

// C Includes
// C++ Includes
// Other libraries and framework includes
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"

// Project includes
#include "DWARFASTParser.h"
#include "DWARFDIE.h"
#include "DWARFDefines.h"
#include "LogChannelDWARF.h"
#include "lldb/Core/PluginInterface.h"

#include "Plugins/TypeSystem/Legacy/TypeSystemLegacy.h"

namespace lldb_private {
class CompileUnit;
}

class DWARFDebugInfoEntry;
class DWARFDIECollection;

class DWARFASTParserLegacy : public DWARFASTParser {
public:
  DWARFASTParserLegacy(lldb_private::TypeSystemLegacy &ast);

  ~DWARFASTParserLegacy() override;

  lldb::TypeSP ParseTypeFromDWARF(const lldb_private::SymbolContext &sc,
                                  const DWARFDIE &die,
                                  bool *type_is_new_ptr) override;

  lldb_private::Function *
  ParseFunctionFromDWARF(lldb_private::CompileUnit &comp_unit,
                         const DWARFDIE &die) override;

  bool CompleteTypeFromDWARF(const DWARFDIE &die, lldb_private::Type *type,
                             lldb_private::CompilerType &legacy_type) override;

  lldb_private::CompilerDeclContext
  GetDeclContextForUIDFromDWARF(const DWARFDIE &die) override {
    return lldb_private::CompilerDeclContext();
  }

  lldb_private::CompilerDeclContext
  GetDeclContextContainingUIDFromDWARF(const DWARFDIE &die) override {
    return lldb_private::CompilerDeclContext();
  }

  lldb_private::CompilerDecl
  GetDeclForUIDFromDWARF(const DWARFDIE &die) override {
    return lldb_private::CompilerDecl();
  }

  void EnsureAllDIEsInDeclContextHaveBeenParsed(
      lldb_private::CompilerDeclContext decl_context) override {}

private:
  lldb_private::TypeSystemLegacy &m_ast;

  size_t ParseChildParameters(
      lldb_private::CompileUnit &comp_unit, const DWARFDIE &parent_die,
      bool &is_variadic,
      std::vector<lldb_private::CompilerType> &function_param_types);

  size_t ParseChildMembers(const DWARFDIE &die,
                           lldb_private::CompilerType &class_compiler_type);
};

#endif // SymbolFileDWARF_DWARFASTParserLegacy_h_
