//===-- CobolLanguage.cpp ---------------------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

// Other libraries and framework includes
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/Threading.h"

#include "CobolLanguage.h"

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

LLDB_PLUGIN_DEFINE(CobolLanguage)

void CobolLanguage::Initialize() {
  PluginManager::RegisterPlugin(GetPluginNameStatic(), "Cobol Language",
                                CreateInstance);
}

void CobolLanguage::Terminate() {
  PluginManager::UnregisterPlugin(CreateInstance);
}

lldb_private::ConstString CobolLanguage::GetPluginNameStatic() {
  static ConstString g_name("Cobol");
  return g_name;
}

//------------------------------------------------------------------
// PluginInterface protocol
//------------------------------------------------------------------
lldb_private::ConstString CobolLanguage::GetPluginName() {
  return GetPluginNameStatic();
}

uint32_t CobolLanguage::GetPluginVersion() { return 1; }

//------------------------------------------------------------------
// Static Functions
//------------------------------------------------------------------
Language *CobolLanguage::CreateInstance(lldb::LanguageType language) {
  if (language == eLanguageTypeCobol85)
    return new CobolLanguage();
  return nullptr;
}

lldb::TypeCategoryImplSP CobolLanguage::GetFormatters() {
  static llvm::once_flag g_initialize;
  static TypeCategoryImplSP g_category;

  llvm::call_once(g_initialize, [this]() -> void {
    DataVisualization::Categories::GetCategory(GetPluginName(), g_category);
    if (g_category) {
      LoadCobolFormatters(g_category);
    }
  });
  return g_category;
}

HardcodedFormatters::HardcodedSummaryFinder
CobolLanguage::GetHardcodedSummaries() {
  static HardcodedFormatters::HardcodedSummaryFinder g_formatters;
  return g_formatters;
}

HardcodedFormatters::HardcodedSyntheticFinder
CobolLanguage::GetHardcodedSynthetics() {
  static HardcodedFormatters::HardcodedSyntheticFinder g_formatters;
  return g_formatters;
}

bool CobolLanguage::IsSourceFile(llvm::StringRef file_path) const {
  return file_path.endswith(".cob");
}

void CobolLanguage::LoadCobolFormatters(lldb::TypeCategoryImplSP category_sp) {
  if (!category_sp)
    return;

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
}

bool formatters::RaincodeStringSummaryProvider(ValueObject &valobj,
                                               Stream &stream,
                                               const TypeSummaryOptions &opts) {
  ProcessSP process_sp = valobj.GetProcessSP();
  if (!process_sp)
    return false;

  uint32_t length = 0;
  if (!valobj.GetCompilerType().IsCStringType(length))
    return false;

  if (length == 0) {
    stream.Printf("\"\"");
    return true;
  }

  lldb::addr_t valobj_addr = valobj.GetAddressOf();
  if (valobj_addr == LLDB_INVALID_ADDRESS)
    return false;

  Status error;
  length++; // null terminated
  ExecutionContext exe_ctx(valobj.GetExecutionContextRef());
  lldb::DataBufferSP buffer_sp(new DataBufferHeap(length, 0));
  char *buffer = reinterpret_cast<char *>(buffer_sp->GetBytes());

  process_sp->ReadStringFromMemory(valobj_addr, buffer, length, error, 1);

  // this is for PLI var string.
  const Flags type_flags(valobj.GetTypeInfo());
  if (type_flags.Test(eTypeIsVarString)) {
    uint32_t ele = buffer[0] << 8 | buffer[1];
    length = ele < length ? ele + 1 : length;
    buffer += 2;
  }

  ExecutionContextScope *exe_scope = process_sp.get();
  TargetCharsetReader Conv(exe_scope->CalculateTarget());
  if (!Conv.IsValid()) {
    Host::SystemLog(Host::eSystemLogWarning,
                    "WARNING: Invalid target charset %s.\n",
                    Conv.getTargetFormat().GetCString());
    return false;
  }

  Conv.convert(buffer, length);
  DataExtractor data(buffer, length, exe_ctx.GetByteOrder(),
                     process_sp->GetAddressByteSize());
  return DumpDataExtractor(data, &stream, 0, eFormatCString, 1, length,
                           UINT32_MAX, LLDB_INVALID_ADDRESS, 0, 0, exe_scope);
}
