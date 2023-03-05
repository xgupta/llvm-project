//===--   PLILanguage.h -----------------------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef liblldb_PLILanguage_h_
#define liblldb_PLILanguage_h_

// Other libraries and framework includes
#include "llvm/ADT/StringRef.h"

// Project includes
#include "lldb/Target/Language.h"
#include "lldb/Utility/ConstString.h"
#include "lldb/lldb-private.h"

namespace lldb_private {

class PLILanguage : public Language {
public:
  PLILanguage() = default;
  ~PLILanguage() override = default;

  lldb::LanguageType GetLanguageType() const override {
    return lldb::eLanguageTypePLI;
  }

  HardcodedFormatters::HardcodedSummaryFinder GetHardcodedSummaries() override;

  HardcodedFormatters::HardcodedSyntheticFinder
  GetHardcodedSynthetics() override;

  bool IsSourceFile(llvm::StringRef file_path) const override;

  lldb::TypeCategoryImplSP GetFormatters() override;

  //------------------------------------------------------------------
  // Static Functions
  //------------------------------------------------------------------
  static void Initialize();

  static void Terminate();

  static lldb_private::Language *CreateInstance(lldb::LanguageType language);

  static llvm::StringRef GetPluginNameStatic();

  static void LoadPLIFormatters(lldb::TypeCategoryImplSP category_sp);

  //------------------------------------------------------------------
  // PluginInterface protocol
  //------------------------------------------------------------------
  llvm::StringRef GetPluginName() override;

  uint32_t GetPluginVersion();
};

namespace formatters {

SyntheticChildrenFrontEnd *
PLIBitsetSyntheticFrontEndCreator(CXXSyntheticChildren *,
                                  lldb::ValueObjectSP valobj_sp);

bool RaincodeStringSummaryProvider(ValueObject &valobj, Stream &stream,
                                   const TypeSummaryOptions &options);

} // namespace formatters
} // namespace lldb_private

#endif // liblldb_PLILanguage_h_
