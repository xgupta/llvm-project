//===-- CobolLanguage.h -----------------------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef liblldb_CobolLanguage_h_
#define liblldb_CobolLanguage_h_

// Other libraries and framework includes
#include "llvm/ADT/StringRef.h"

// Project includes
#include "lldb/Target/Language.h"
#include "lldb/Utility/ConstString.h"
#include "lldb/lldb-private.h"

namespace lldb_private {

class CobolLanguage : public Language {
public:
  CobolLanguage() = default;
  ~CobolLanguage() override = default;

  lldb::LanguageType GetLanguageType() const override {
    return lldb::eLanguageTypeCobol85;
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

  static lldb_private::ConstString GetPluginNameStatic();

  static void LoadCobolFormatters(lldb::TypeCategoryImplSP category_sp);

  //------------------------------------------------------------------
  // PluginInterface protocol
  //------------------------------------------------------------------
  ConstString GetPluginName() override;

  uint32_t GetPluginVersion() override;
};

namespace formatters {

bool RaincodeStringSummaryProvider(ValueObject &valobj, Stream &stream,
                                   const TypeSummaryOptions &options);

} // namespace formatters
} // namespace lldb_private

#endif // liblldb_CobolLanguage_h_
