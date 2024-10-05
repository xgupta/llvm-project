//===-- DWARFASTParserLegacy.h---------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef SymbolFileDWARF_DWARFASTParserLegacy_h_
#define SymbolFileDWARF_DWARFASTParserLegacy_h_

#include "DWARFASTParser.h"
#include "DWARFDIE.h"
#include "DWARFDefines.h"
#include "LogChannelDWARF.h"
#include "Plugins/TypeSystem/Legacy/TypeSystemLegacy.h"
#include "lldb/Core/PluginInterface.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"

namespace lldb_private {
class CompileUnit;
}

class DWARFDebugInfoEntry;
class DWARFDIECollection;

class DWARFASTParserLegacy
    : public lldb_private::plugin::dwarf::DWARFASTParser {
public:
  DWARFASTParserLegacy(lldb_private::TypeSystemLegacy &ast);

  ~DWARFASTParserLegacy() override;

  lldb::TypeSP
  ParseTypeFromDWARF(const lldb_private::SymbolContext &sc,
                     const lldb_private::plugin::dwarf::DWARFDIE &die,
                     bool *type_is_new_ptr) override;

  lldb_private::Function *
  ParseFunctionFromDWARF(lldb_private::CompileUnit &comp_unit,
                         const lldb_private::plugin::dwarf::DWARFDIE &die,
                         const lldb_private::AddressRange &func_range) override;

  bool
  CompleteTypeFromDWARF(const lldb_private::plugin::dwarf::DWARFDIE &die,
                        lldb_private::Type *type,
                        const lldb_private::CompilerType &legacy_type) override;

  lldb_private::CompilerDeclContext GetDeclContextForUIDFromDWARF(
      const lldb_private::plugin::dwarf::DWARFDIE &die) override {
    return lldb_private::CompilerDeclContext();
  }

  lldb_private::CompilerDeclContext GetDeclContextContainingUIDFromDWARF(
      const lldb_private::plugin::dwarf::DWARFDIE &die) override {
    return lldb_private::CompilerDeclContext();
  }

  lldb_private::CompilerDecl GetDeclForUIDFromDWARF(
      const lldb_private::plugin::dwarf::DWARFDIE &die) override {
    return lldb_private::CompilerDecl();
  }

  void EnsureAllDIEsInDeclContextHaveBeenParsed(
      lldb_private::CompilerDeclContext decl_context) override {}

  std::string GetDIEClassTemplateParams(
      lldb_private::plugin::dwarf::DWARFDIE die) override {
    return {};
  }
  lldb_private::ConstString ConstructDemangledNameFromDWARF(
      const lldb_private::plugin::dwarf::DWARFDIE &die) override {
    return lldb_private::ConstString();
  }

private:
  lldb_private::TypeSystemLegacy &m_ast;

  size_t ParseChildParameters(
      lldb_private::CompileUnit &comp_unit,
      const lldb_private::plugin::dwarf::DWARFDIE &parent_die,
      bool &is_variadic,
      std::vector<lldb_private::CompilerType> &function_param_types);

  size_t
  ParseChildMembers(const lldb_private::plugin::dwarf::DWARFDIE &die,
                    const lldb_private::CompilerType &class_compiler_type);
};

#endif // SymbolFileDWARF_DWARFASTParserLegacy_h_
