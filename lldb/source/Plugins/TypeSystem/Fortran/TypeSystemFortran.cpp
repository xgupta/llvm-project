//===-- TypeSystemFortran.cpp -----------------------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
#include "TypeSystemFortran.h"

#include "lldb/Core/PluginManager.h"
#include "lldb/Symbol/SymbolFile.h"

using namespace lldb;
using namespace lldb_private;
using namespace llvm;
using namespace lldb_private::plugin::dwarf;

LLDB_PLUGIN_DEFINE(TypeSystemFortran)

namespace lldb_private {

// Temporary Type class to represent scalar types for prototype
class FortranType {
public:
  enum TypeKind { KIND_INTEGER, KIND_LOGICAL, KIND_REAL, KIND_UNKNOWN };
  FortranType(int kind, const lldb_private::ConstString &name, uint64_t bitsize)
      : m_kind(kind), m_type_name(name), m_bitsize(bitsize) {}
  int GetKind() { return m_kind; }
  uint64_t GetBitSize() { return m_bitsize; }
  lldb_private::ConstString GetName() { return m_type_name; }

private:
  int m_kind;
  uint64_t m_bitsize;
  lldb_private::ConstString m_type_name;
};
}; // namespace lldb_private

namespace {
bool IsLanguageSupported(lldb::LanguageType language) {
  if (language == lldb::LanguageType::eLanguageTypeFortran77 ||
      language == lldb::LanguageType::eLanguageTypeFortran90 ||
      language == lldb::LanguageType::eLanguageTypeFortran95 ||
      language == lldb::LanguageType::eLanguageTypeFortran03 ||
      language == lldb::LanguageType::eLanguageTypeFortran08 ||
      language == lldb::LanguageType::eLanguageTypeFortran18)
    return true;

  return false;
}
} // namespace
char TypeSystemFortran::ID;

TypeSystemFortran::~TypeSystemFortran() = default;
TypeSystemFortran::TypeSystemFortran() = default;

void TypeSystemFortran::Initialize() {
  PluginManager::RegisterPlugin(
      GetPluginNameStatic(), "fortran AST context plug-in", CreateInstance,
      GetSupportedLanguagesForTypes(), GetSupportedLanguagesForExpressions());
}

void TypeSystemFortran::Terminate() {
  PluginManager::UnregisterPlugin(CreateInstance);
}

LanguageSet TypeSystemFortran::GetSupportedLanguagesForTypes() {
  LanguageSet languages;
  languages.Insert(eLanguageTypeFortran77);
  languages.Insert(eLanguageTypeFortran90);
  languages.Insert(eLanguageTypeFortran95);
  languages.Insert(eLanguageTypeFortran03);
  languages.Insert(eLanguageTypeFortran08);
  languages.Insert(eLanguageTypeFortran18);
  return languages;
}

LanguageSet TypeSystemFortran::GetSupportedLanguagesForExpressions() {
  LanguageSet languages;
  languages.Insert(eLanguageTypeFortran77);
  languages.Insert(eLanguageTypeFortran90);
  languages.Insert(eLanguageTypeFortran95);
  languages.Insert(eLanguageTypeFortran03);
  languages.Insert(eLanguageTypeFortran08);
  languages.Insert(eLanguageTypeFortran18);
  return languages;
}

bool TypeSystemFortran::IsFloatingPointType(lldb::opaque_compiler_type_t type) {
  int kind = static_cast<FortranType *>(type)->GetKind();
  if (kind == FortranType::KIND_REAL)
    return true;
  return false;
}

ConstString TypeSystemFortran::GetTypeName(lldb::opaque_compiler_type_t type,
                                           bool BaseOnly) {
  if (!type)
    return ConstString();
  FortranType *fortran_type = static_cast<FortranType *>(type);
  uint64_t bytes = fortran_type->GetBitSize() / 8;
  if (BaseOnly)
    return fortran_type->GetName();
  switch (fortran_type->GetKind()) {
  case FortranType::KIND_INTEGER:
    if (fortran_type->GetBitSize() != 32)
      return ConstString(llvm::formatv("INTEGER(KIND={0})", bytes).str());
    else
      return fortran_type->GetName();
    break;
  case FortranType::KIND_LOGICAL:
    return fortran_type->GetName();
  case FortranType::KIND_REAL:
    if (bytes == 4)
      return ConstString("REAL");
    if (bytes == 8)
      return ConstString("DOUBLE PRECISION");
    return ConstString(llvm::formatv("REAL(KIND={0})", bytes).str());
  }
  return ConstString("Unsupported");
}

CompilerType TypeSystemFortran::GetOrCreateFortranType(int kind,
                                                       uint64_t bitsize,
                                                       ConstString name) {
  FortranType *type = m_type_map[{kind, bitsize}].get();
  if (type)
    return CompilerType(weak_from_this(), (void *)type);
  auto new_type_up = std::make_unique<FortranType>(kind, name, bitsize);
  FortranType *raw_ptr = new_type_up.get();

  m_type_map[{kind, bitsize}] = std::move(new_type_up);

  return CompilerType(weak_from_this(), (void *)raw_ptr);
}

// FIXME: Needs a map to index this
CompilerType
TypeSystemFortran::GetBasicTypeFromAST(lldb::BasicType basic_type) {
  switch (basic_type) {
  case lldb::eBasicTypeInt:
    return GetOrCreateFortranType(FortranType::KIND_INTEGER, 32,
                                  ConstString("INTEGER"));
  case lldb::eBasicTypeFloat:
    return GetOrCreateFortranType(FortranType::KIND_REAL, 32,
                                  ConstString("REAL"));
  case lldb::eBasicTypeDouble:
    return GetOrCreateFortranType(FortranType::KIND_REAL, 64,
                                  ConstString("DOUBLE PRECISION"));
  case lldb::eBasicTypeBool:
    return GetOrCreateFortranType(FortranType::KIND_LOGICAL, 32,
                                  ConstString("LOGICAL"));
  default:
    return CompilerType();
  }
}

CompilerType
TypeSystemFortran::GetBuiltinTypeForEncodingAndBitSize(lldb::Encoding encoding,
                                                       size_t bit_size) {
  switch (encoding) {
  case eEncodingSint:
    return GetOrCreateFortranType(FortranType::KIND_INTEGER, bit_size,
                                  ConstString("INTEGER"));
  case eEncodingIEEE754:
    return GetOrCreateFortranType(FortranType::KIND_REAL, bit_size,
                                  ConstString("REAL"));
  default:
    return CompilerType();
  }
}

plugin::dwarf::DWARFASTParser *TypeSystemFortran::GetDWARFParser() {
  if (!m_dwarf_ast_parser_up)
    m_dwarf_ast_parser_up = std::make_unique<DWARFASTParserFortran>(*this);
  return m_dwarf_ast_parser_up.get();
}

// TODO: Process Target and architecture for pointers and Expression Evaluation,
// if module and target have different typesystems like clang, we would have to
// account for that here
lldb::TypeSystemSP
TypeSystemFortran::CreateInstance(lldb::LanguageType language, Module *module,
                                  Target *target) {
  if (IsLanguageSupported(language)) {
    return std::make_shared<TypeSystemFortran>();
  }
  return TypeSystemSP();
}

llvm::Expected<uint64_t>
TypeSystemFortran::GetBitSize(lldb::opaque_compiler_type_t type,
                              ExecutionContextScope *exe_scope) {
  if (!type)
    return 0;
  FortranType *fortran_type = static_cast<FortranType *>(type);
  return fortran_type->GetBitSize();
}

lldb::Encoding
TypeSystemFortran::GetEncoding(lldb::opaque_compiler_type_t type) {
  if (!type)
    return lldb::eEncodingInvalid;
  FortranType *fortran_type = static_cast<FortranType *>(type);
  switch (fortran_type->GetKind()) {
  case FortranType::KIND_REAL:
    return lldb::eEncodingIEEE754;
  case FortranType::KIND_INTEGER:
    return lldb::eEncodingSint;
  case FortranType::KIND_LOGICAL:
    return lldb::eEncodingUint;
  default:
    return lldb::eEncodingInvalid;
  }
}

lldb::Format TypeSystemFortran::GetFormat(lldb::opaque_compiler_type_t type) {
  if (!type)
    return lldb::eFormatDefault;
  FortranType *fortran_type = static_cast<FortranType *>(type);
  switch (fortran_type->GetKind()) {
  case FortranType::KIND_INTEGER:
    return lldb::eFormatDecimal;
  case FortranType::KIND_REAL:
    return lldb::eFormatFloat;
  case FortranType::KIND_LOGICAL:
    return lldb::eFormatBoolean;
  default:
    return lldb::eFormatDefault;
  }
}

bool IsIntegerType(lldb::opaque_compiler_type_t type, bool &is_signed) {
  if (!type)
    return false;
  FortranType *fortran_type = static_cast<FortranType *>(type);
  if (fortran_type->GetKind() == FortranType::KIND_INTEGER) {
    is_signed = true;
    return true;
  }
  return false;
}