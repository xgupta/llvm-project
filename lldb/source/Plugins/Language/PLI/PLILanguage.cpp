//===--   PLILanguage.cpp ---------------------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "llvm/ADT/StringRef.h"
#include "llvm/Support/Threading.h"

#include "PLILanguage.h"

#include "lldb/Core/DumpDataExtractor.h"
#include "lldb/Core/PluginManager.h"
#include "lldb/DataFormatters/DataVisualization.h"
#include "lldb/DataFormatters/FormattersHelpers.h"
#include "lldb/Utility/ConstString.h"

#include "Plugins/TypeSystem/Legacy/TypeSystemLegacy.h"

using namespace llvm;
using namespace lldb;
using namespace lldb_private;
using namespace lldb_private::formatters;

LLDB_PLUGIN_DEFINE(PLILanguage)

void PLILanguage::Initialize() {
  PluginManager::RegisterPlugin(GetPluginNameStatic(), "PLI Language",
                                CreateInstance);
}

void PLILanguage::Terminate() {
  PluginManager::UnregisterPlugin(CreateInstance);
}

llvm::StringRef PLILanguage::GetPluginNameStatic() {
  static llvm::StringRef g_name("PLI");
  return g_name;
}

//------------------------------------------------------------------
// PluginInterface protocol
//------------------------------------------------------------------
llvm::StringRef PLILanguage::GetPluginName() { return GetPluginNameStatic(); }

uint32_t PLILanguage::GetPluginVersion() { return 1; }

//------------------------------------------------------------------
// Static Functions
//------------------------------------------------------------------
Language *PLILanguage::CreateInstance(lldb::LanguageType language) {
  if (language == eLanguageTypePLI) {
    return new PLILanguage();
  }
  return nullptr;
}

lldb::TypeCategoryImplSP PLILanguage::GetFormatters() {
  static llvm::once_flag g_initialize;
  static TypeCategoryImplSP g_category;

  llvm::call_once(g_initialize, [this]() -> void {
    DataVisualization::Categories::GetCategory(ConstString(GetPluginName()),
                                               g_category);
    if (g_category) {
      LoadPLIFormatters(g_category);
    }
  });
  return g_category;
}

HardcodedFormatters::HardcodedSummaryFinder
PLILanguage::GetHardcodedSummaries() {
  static HardcodedFormatters::HardcodedSummaryFinder g_formatters;
  return g_formatters;
}

bool PLILanguage::DemangledNameContainsPath(llvm::StringRef path,
                                            ConstString demangled) const {
  return demangled.GetStringRef().contains_insensitive(path);
}

HardcodedFormatters::HardcodedSyntheticFinder
PLILanguage::GetHardcodedSynthetics() {
  static HardcodedFormatters::HardcodedSyntheticFinder g_formatters;
  return g_formatters;
}

bool PLILanguage::IsSourceFile(llvm::StringRef file_path) const {
  const auto suffixes = {".pli", ".plirc"};
  for (auto suffix : suffixes) {
    if (file_path.ends_with_insensitive(suffix))
      return true;
  }
  return false;
}

void PLILanguage::LoadPLIFormatters(lldb::TypeCategoryImplSP category_sp) {
  if (!category_sp)
    return;

  SyntheticChildren::Flags synth_flags;
  synth_flags.SetCascades(true)
      .SetSkipPointers(false)
      .SetSkipReferences(false)
      .SetFrontEndWantsDereference(true);

  TypeFormatImpl::Flags bit_flags;
  bit_flags.SetCascades(true).SetSkipPointers(true).SetSkipReferences(true);

  AddFormat(category_sp, lldb::eFormatUnsigned, ConstString("BIT"), bit_flags);

  AddCXXSynthetic(category_sp,
                  lldb_private::formatters::PLIBitsetSyntheticFrontEndCreator,
                  "pli bit synthetic children", ConstString("^BIT [[0-9]+]$"),
                  synth_flags, true);

  TypeSummaryImpl::Flags summary_flags;
  summary_flags.SetCascades(true)
      .SetSkipPointers(false)
      .SetSkipReferences(false)
      .SetDontShowChildren(true)
      .SetDontShowValue(true)
      .SetShowMembersOneLiner(false)
      .SetHideItemNames(false);

  AddCXXSummary(category_sp,
                lldb_private::formatters::RaincodeStringSummaryProvider,
                "string summary provider", ConstString("char \\[[0-9]+\\]"),
                summary_flags, true);

  return;
}
