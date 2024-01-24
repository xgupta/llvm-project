//===-- TypeSystemLegacy.cpp -------------------------------------*- C++
//-*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "TypeSystemLegacy.h"
#include "lldb/Core/Module.h"
#include "lldb/Core/PluginManager.h"
#include "lldb/Target/ExecutionContext.h"
#include "lldb/Target/Language.h"
#include "lldb/Target/Process.h"
#include "lldb/Target/Target.h"

#include "Plugins/ExpressionParser/Cobol/CobolUserExpression.h"
#include "Plugins/ExpressionParser/PLI/PLIUserExpression.h"
#include "Plugins/LanguageRuntime/ObjC/ObjCLanguageRuntime.h"
#include "Plugins/SymbolFile/DWARF/DWARFASTParserLegacy.h"
#include "lldb/Core/DumpDataExtractor.h"
#include "lldb/Host/StreamFile.h"
#include "lldb/Utility/DataExtractor.h"
#include "lldb/Utility/Endian.h"
#include "lldb/Utility/Log.h"

#include <mutex>
#include <string>
#include <vector>

using namespace lldb;
using namespace lldb_private;
using namespace llvm;
using namespace lldb_private::dwarf;

LLDB_PLUGIN_DEFINE(TypeSystemLegacy)

namespace lldb_private {
class LegacyFunction;
class LegacyArray;
class LegacyStruct;
class LegacyDynamicArray;

class LegacyType {
public:
  enum LLVMTypeKind {
    KIND_INT,
    KIND_UINT,
    KIND_FLOAT,
    KIND_DOUBLE,

    KIND_DISPLAY,
    KIND_DECIMAL,
    KIND_EDITED,

    KIND_ARRAY,
    KIND_STRUCT,
    KIND_PTR,
    KIND_FUNC,

    KIND_DYNAMIC,
    KIND_INVALID,

    KIND_L88,

    KIND_MASK = (1 << 5) - 1,
    KIND_MAX = (1 << 5),

  };

  enum LLVMSign {
    eUnsigned,
    eLeadingOverpunch,
    eTrailingOverpunch,
    eLeadingSeparate,
    eTrailingSeparate,
  };

  LegacyType(int kind, const ConstString &name)
      : m_kind(kind & KIND_MASK), m_name(name), m_bitsize(0), m_sign(eUnsigned),
        m_digit_count(0), m_byte_order(eByteOrderBig), bin_scale(false) {}
  LegacyType(int kind, const ConstString &name, const ConstString &pic_string,
             uint32_t bitsize, int sign, int scale, uint32_t digit_count,
             ByteOrder byte_order, bool bin_scale)
      : m_kind(kind & KIND_MASK), m_name(name), m_pic_string(pic_string),
        m_bitsize(bitsize), m_sign(sign), m_scale(scale),
        m_digit_count(digit_count), m_byte_order(byte_order),
        bin_scale(bin_scale) {}

  virtual ~LegacyType() {}

  int GetLegacyKind() const { return m_kind; }
  const ConstString &GetTypeName() const { return m_name; }
  const ConstString &GetPicString() const { return m_pic_string; }
  ByteOrder GetTypeByteOrder() const { return m_byte_order; }
  uint32_t GetTypeBitSize() const { return m_bitsize; }
  uint32_t GetSign() const { return m_sign; }
  int32_t GetScale() const { return m_scale; }
  uint32_t GetDigitCount() const { return m_digit_count; }
  bool is_binary_scale() const { return bin_scale; }
  void EditString(std::string &data);
  virtual CompilerType GetElementType() const { return CompilerType(); }
  LegacyFunction *GetFunction();
  LegacyArray *GetArray();
  LegacyStruct *GetStruct();
  LegacyDynamicArray *GetDynamicArray();

private:
  int m_kind;
  ConstString m_name;
  ConstString m_pic_string;
  uint32_t m_bitsize;
  uint32_t m_sign;
  int32_t m_scale;
  uint32_t m_digit_count;
  ByteOrder m_byte_order;
  const bool bin_scale;
  LegacyType(const LegacyType &) = delete;
  const LegacyType &operator=(const LegacyType &) = delete;
};

class LegacyElem : public LegacyType {
public:
  LegacyElem(int kind, const ConstString &name, const CompilerType &elem)
      : LegacyType(kind, name), m_elem(elem) {}
  virtual CompilerType GetElementType() const override { return m_elem; }

private:
  CompilerType m_elem;

  LegacyElem(const LegacyElem &) = delete;
  const LegacyElem &operator=(const LegacyElem &) = delete;
};

class LegacyArray : public LegacyElem {
public:
  LegacyArray(const ConstString array_type_name, const ConstString &name,
              const CompilerType &elem, uint64_t length,
              bool isVarString = false)
      : LegacyElem(KIND_ARRAY, name, elem), m_array_type_name(array_type_name),
        m_length(length), m_var_string(isVarString) {}

  static bool classof(const LegacyType *type) {
    return type->GetLegacyKind() == LegacyType::KIND_ARRAY;
  }

  uint64_t GetLength() const { return m_length; }
  const ConstString &GetArrayTypeName() const { return m_array_type_name; }

  bool isVarString() const { return m_var_string; }
  virtual bool isDynamic() const { return false; }

protected:
  void SetLength(uint64_t length) { m_length = length; }

private:
  ConstString m_array_type_name;
  uint64_t m_length;
  bool m_var_string;
  LegacyArray(const LegacyArray &) = delete;
  const LegacyArray &operator=(const LegacyArray &) = delete;
};

class LegacyDynamicArray : public LegacyArray {
public:
  LegacyDynamicArray(const ConstString array_type_name, const ConstString &name,
                     const CompilerType &elem, DWARFExpressionList count_exp,
                     bool isVarString = false)
      : LegacyArray(array_type_name, name, elem, 0, isVarString),
        m_count_exp(count_exp) {}

  static bool classof(const LegacyType *type) {
    return type->GetLegacyKind() == LegacyType::KIND_ARRAY &&
           (static_cast<const LegacyArray *>(type)->isDynamic());
  }

  DWARFExpressionList GetCountExp() const { return m_count_exp; }

  void UpdateLength(uint64_t length) { SetLength(length); }

  bool isDynamic() const override { return true; }

private:
  const DWARFExpressionList m_count_exp;
  LegacyDynamicArray(const LegacyDynamicArray &) = delete;
  const LegacyDynamicArray &operator=(const LegacyDynamicArray &) = delete;
};

class LegacyFunction : public LegacyType {
public:
  LegacyFunction(const ConstString &name, const CompilerType *params,
                 uint32_t numargs, bool is_variadic)
      : LegacyType(KIND_FUNC, name), m_is_variadic(is_variadic),
        m_params(params, params + numargs) {}

  bool IsVariadic() const { return m_is_variadic; }
  CompilerType GetArgumentAtIndex(size_t index) const {
    if (index >= m_params.size())
      return CompilerType();
    return m_params[index];
  }

private:
  bool m_is_variadic;
  std::vector<CompilerType> m_params;
  LegacyFunction(const LegacyFunction &) = delete;
  const LegacyFunction &operator=(const LegacyFunction &) = delete;
};

class LegacyDynamic : public LegacyType {
public:
  LegacyDynamic(const CompilerType &base_type, DWARFExpressionList location,
                DWARFExpressionList allocated)
      : LegacyType(KIND_DYNAMIC, ConstString()), m_type(base_type),
        m_location(location), m_allocated(allocated) {}

  CompilerType GetBaseType() const { return m_type; }
  DWARFExpressionList getAllocated() const { return m_allocated; }
  DWARFExpressionList getLocation() const { return m_location; }

private:
  CompilerType m_type;
  DWARFExpressionList m_location;
  DWARFExpressionList m_allocated;

  LegacyDynamic(const LegacyDynamic &) = delete;
  const LegacyDynamic &operator=(const LegacyDynamic &) = delete;
};

class LegacyStruct : public LegacyType {
public:
  struct Member {
    Member(const ConstString &name, const CompilerType &type,
           uint64_t offset_in_bits)
        : m_name(name), m_type(type), m_offset_in_bits(offset_in_bits) {}
    ConstString m_name;
    CompilerType m_type;
    uint32_t m_offset_in_bits;

    ConstString GetMemberName() const { return m_name; }
  };

  LegacyStruct(const ConstString &name, int64_t byte_size)
      : LegacyType(KIND_STRUCT, name), m_byte_size(byte_size),
        m_is_complete(false) {}

  uint32_t GetNumMembers() const { return m_members.size(); }
  const Member *GetMemberAtIndex(uint32_t i) const {
    return i < GetNumMembers() ? &m_members[i] : nullptr;
  }
  bool IsComplete() const { return m_is_complete; }

  void SetComplete() { m_is_complete = true; }
  int64_t GetByteSize() const { return m_byte_size; }
  void AddMember(const ConstString &name, const CompilerType &type,
                 uint64_t offset_in_bits) {
    m_members.push_back(Member(name, type, offset_in_bits));
  }

private:
  int64_t m_byte_size;
  bool m_is_complete;
  std::vector<Member> m_members;

  LegacyStruct(const LegacyStruct &) = delete;
  const LegacyStruct &operator=(const LegacyStruct &) = delete;
};

LegacyArray *LegacyType::GetArray() {
  if (m_kind == KIND_ARRAY)
    return static_cast<LegacyArray *>(this);
  return nullptr;
}

LegacyDynamicArray *LegacyType::GetDynamicArray() {
  if (m_kind == KIND_ARRAY) {
    LegacyArray *array = static_cast<LegacyArray *>(this);
    if (array->isDynamic())
      return static_cast<LegacyDynamicArray *>(array);
  }
  return nullptr;
}

LegacyFunction *LegacyType::GetFunction() {
  if (m_kind == KIND_FUNC)
    return static_cast<LegacyFunction *>(this);
  return nullptr;
}

LegacyStruct *LegacyType::GetStruct() {
  if (m_kind == KIND_STRUCT)
    return static_cast<LegacyStruct *>(this);
  return nullptr;
}

} // namespace lldb_private

namespace {
static inline bool TypeSystemLegacySupportsLanguage(LanguageType language) {
  return language == eLanguageTypePLI || language == eLanguageTypeCobol74 ||
         language == eLanguageTypeCobol85 || language == eLanguageTypeFortran90;
}

static inline uint8_t EncodePackedSign(const uint8_t in) {
  switch (in) {
  default:
    return in - '0';
  case '-':
    return 0xD; // preferred
    return 0xB;
  case '+':
  case ' ':
    return 0xC; // preferred
    return 0xF;
  }
}

static inline char EncodeOverpunchDigit(char in, bool isPositive) {
  switch (in) {
  case '0':
    return isPositive ? '{' : '}';
  case '1':
    return isPositive ? 'A' : 'J';
  case '2':
    return isPositive ? 'B' : 'K';
  case '3':
    return isPositive ? 'C' : 'L';
  case '4':
    return isPositive ? 'D' : 'M';
  case '5':
    return isPositive ? 'E' : 'N';
  case '6':
    return isPositive ? 'F' : 'O';
  case '7':
    return isPositive ? 'G' : 'P';
  case '8':
    return isPositive ? 'H' : 'Q';
  case '9':
    return isPositive ? 'I' : 'R';
  }
  return 0;
}

static inline uint8_t DecodePackedSignToString(const uint8_t in) {
  switch (in) {
  default:
    return in + '0';
  case 0xA:
  case 0xC:
  case 0xE:
    return '+';
  case 0xB:
  case 0xD:
    return '-';
  case 0xF:
    return ' ';
  }
}

static inline char DecodeOverpunchDigit(char &in) {
  switch (in) {
  case '}':
    in = '0';
    return '-';
  case 'J':
    in = '1';
    return '-';
  case 'K':
    in = '2';
    return '-';
  case 'L':
    in = '3';
    return '-';
  case 'M':
    in = '4';
    return '-';
  case 'N':
    in = '5';
    return '-';
  case 'O':
    in = '6';
    return '-';
  case 'P':
    in = '7';
    return '-';
  case 'Q':
    in = '8';
    return '-';
  case 'R':
    in = '9';
    return '-';
  case '{':
    in = '0';
    return '+';
  case 'A':
    in = '1';
    return '+';
  case 'B':
    in = '2';
    return '+';
  case 'C':
    in = '3';
    return '+';
  case 'D':
    in = '4';
    return '+';
  case 'E':
    in = '5';
    return '+';
  case 'F':
    in = '6';
    return '+';
  case 'G':
    in = '7';
    return '+';
  case 'H':
    in = '8';
    return '+';
  case 'I':
    in = '9';
    return '+';
  }
  return 0;
}

} // namespace

//----------------------------------------------------------------------
// Tests
//----------------------------------------------------------------------

char TypeSystemLegacy::ID;

#ifndef NDEBUG
bool TypeSystemLegacy::Verify(lldb::opaque_compiler_type_t type) {
  // TODO add more checks.
  return type;
}
#endif

void LegacyType::EditString(std::string &data) {
  if (m_kind != KIND_EDITED)
    return;
  // TODO
  std::string buffer(data);

  return;
}

bool TypeSystemLegacy::IsArrayType(opaque_compiler_type_t type,
                                   CompilerType *element_type, uint64_t *size,
                                   bool *is_incomplete) {
  if (element_type)
    element_type->Clear();
  if (size)
    *size = 0;
  if (is_incomplete)
    *is_incomplete = false;

  if (LegacyArray *array = static_cast<LegacyType *>(type)->GetArray()) {
    if (element_type)
      *element_type = array->GetElementType();
    if (size)
      *size = array->GetLength();
    return true;
  }

  if (static_cast<LegacyType *>(type)->GetLegacyKind() ==
      LegacyType::KIND_DYNAMIC) {
    const auto dyn_base_type =
        static_cast<LegacyDynamic *>(type)->GetBaseType();
    return dyn_base_type.IsArrayType(element_type, size, is_incomplete);
  }

  return false;
}

bool TypeSystemLegacy::IsIntegerType(opaque_compiler_type_t type,
                                     bool &is_signed) {
  is_signed = false;

  if (type) {
    int kind = static_cast<LegacyType *>(type)->GetLegacyKind();
    switch (kind) {
    case LegacyType::KIND_INT:
      is_signed = true;
      return true;
    case LegacyType::KIND_UINT:
    case LegacyType::KIND_DECIMAL:
      return true;
    }
  }

  if (static_cast<LegacyType *>(type)->GetLegacyKind() ==
      LegacyType::KIND_DYNAMIC) {
    const auto dyn_base_type =
        static_cast<LegacyDynamic *>(type)->GetBaseType();
    return dyn_base_type.IsIntegerType(is_signed);
  }

  return false;
}

bool TypeSystemLegacy::IsFloatingPointType(opaque_compiler_type_t type,
                                           uint32_t &count, bool &is_complex) {
  count = 0;
  is_complex = false;
  if (type) {
    int kind = static_cast<LegacyType *>(type)->GetLegacyKind();
    switch (kind) {
    case LegacyType::KIND_FLOAT:
    case LegacyType::KIND_DOUBLE:
      count = 1;
      return true;
    }
  }

  if (static_cast<LegacyType *>(type)->GetLegacyKind() ==
      LegacyType::KIND_DYNAMIC) {
    const auto dyn_base_type =
        static_cast<LegacyDynamic *>(type)->GetBaseType();
    return dyn_base_type.IsFloatingPointType(count, is_complex);
  }
  return false;
}

bool TypeSystemLegacy::IsPointerType(opaque_compiler_type_t type,
                                     CompilerType *pointee_type) {
  if (!type)
    return false;

  LegacyType *t = static_cast<LegacyType *>(type);
  if (pointee_type) {
    *pointee_type = t->GetElementType();
  }

  switch (t->GetLegacyKind()) {
  case LegacyType::KIND_PTR:
    return true;
  case LegacyType::KIND_DYNAMIC: {
    const auto dyn_base_type =
        static_cast<LegacyDynamic *>(type)->GetBaseType();
    return dyn_base_type.IsPointerType(pointee_type);
  }
  default:
    return false;
  }
}

bool TypeSystemLegacy::IsAggregateType(opaque_compiler_type_t type) {
  if (!type)
    return false;

  int kind = static_cast<LegacyType *>(type)->GetLegacyKind();
  switch (kind) {
  case LegacyType::KIND_STRUCT:
  case LegacyType::KIND_ARRAY:
    return true;
  case LegacyType::KIND_DYNAMIC: {
    const auto dyn_base_type =
        static_cast<LegacyDynamic *>(type)->GetBaseType();
    return dyn_base_type.IsAggregateType();
  }
  }
  return false;
}

bool TypeSystemLegacy::IsCharType(opaque_compiler_type_t type) {
  if (static_cast<LegacyType *>(type)->GetLegacyKind() ==
      LegacyType::KIND_DISPLAY) {
    return true;
  }

  if (static_cast<LegacyType *>(type)->GetLegacyKind() ==
      LegacyType::KIND_DYNAMIC) {
    const auto dyn_base_type =
        static_cast<LegacyDynamic *>(type)->GetBaseType();
    return dyn_base_type.IsCharType();
  }
  return false;
}

unsigned TypeSystemLegacy::GetPtrAuthKey(lldb::opaque_compiler_type_t type) {
  // Since TypeSystemLegacy do not handle pointer authentication, return a
  // default value.
  return 0;
}

unsigned
TypeSystemLegacy::GetPtrAuthDiscriminator(lldb::opaque_compiler_type_t type) {
  // Provide a default implementation.
  return 0;
}

bool TypeSystemLegacy::GetPtrAuthAddressDiversity(
    lldb::opaque_compiler_type_t type) {
  // Return a default value.
  return false;
}

bool TypeSystemLegacy::IsFunctionType(opaque_compiler_type_t type) {
  switch (static_cast<LegacyType *>(type)->GetLegacyKind()) {
  default:
    break;
  case LegacyType::KIND_FUNC:
    return true;
  case LegacyType::KIND_DYNAMIC: {
    const auto dyn_base_type =
        static_cast<LegacyDynamic *>(type)->GetBaseType();
    return dyn_base_type.IsFunctionType();
  }
  }
  return false;
}

bool TypeSystemLegacy::IsPointerOrReferenceType(opaque_compiler_type_t type,
                                                CompilerType *pointee_type) {
  return IsPointerType(type, pointee_type);
}

uint32_t TypeSystemLegacy::IsHomogeneousAggregate(opaque_compiler_type_t type,
                                                  CompilerType *base_type_ptr) {
  return false;
}

bool TypeSystemLegacy::IsBeingDefined(opaque_compiler_type_t type) {
  return false;
}

bool TypeSystemLegacy::IsPolymorphicClass(opaque_compiler_type_t type) {
  return false;
}

bool TypeSystemLegacy::IsVectorType(opaque_compiler_type_t type,
                                    CompilerType *element_type,
                                    uint64_t *size) {
  if (element_type)
    element_type->Clear();
  if (size)
    *size = 0;
  return false;
}

bool TypeSystemLegacy::IsConst(opaque_compiler_type_t type) { return false; }

bool TypeSystemLegacy::IsCStringType(opaque_compiler_type_t type,
                                     uint32_t &length) {
  if (!type)
    return false;

  LegacyType *base_type = static_cast<LegacyType *>(type);
  if (LegacyArray *array = base_type->GetArray()) {
    if (array->GetElementType().IsCharType()) {
      length = array->GetLength() + (array->isVarString() ? 2UL : 0UL);
      return true;
    }
  }
  if (static_cast<LegacyType *>(type)->GetLegacyKind() ==
      LegacyType::KIND_DYNAMIC) {
    const auto dyn_base_type =
        static_cast<LegacyDynamic *>(type)->GetBaseType();
    return dyn_base_type.IsCStringType(length);
  }
  return false;
}

bool TypeSystemLegacy::IsTypedefType(opaque_compiler_type_t type) {
  return false;
}

bool TypeSystemLegacy::IsReferenceType(opaque_compiler_type_t type,
                                       CompilerType *pointee_type,
                                       bool *is_rvalue) {
  return false;
}

CompilerType TypeSystemLegacy::MutateBaseTypeSize(opaque_compiler_type_t type,
                                                  uint64_t sizeInBits) {
  if (!type)
    return CompilerType();
  if (IsAggregateType(type))
    return CompilerType();
  ConstString dst_type_name(GetTypeName(type, true).GetCString() +
                            std::string(".m.") + std::to_string(sizeInBits));
  LegacyType *dst_type = (*m_mutated_types)[dst_type_name].get();
  if (dst_type)
    return CompilerType(weak_from_this(), dst_type);

  LegacyType *src_type = static_cast<LegacyType *>(type);
  if (src_type->GetTypeBitSize() == sizeInBits)
    return CompilerType(weak_from_this(), type);

  dst_type =
      new LegacyType(src_type->GetLegacyKind(), src_type->GetTypeName(),
                     src_type->GetPicString(), sizeInBits, src_type->GetSign(),
                     src_type->GetScale(), src_type->GetDigitCount(),
                     src_type->GetTypeByteOrder(), src_type->is_binary_scale());
  (*m_mutated_types)[dst_type_name].reset(dst_type);
  return CompilerType(weak_from_this(), dst_type);
}

CompilerType
TypeSystemLegacy::GetFunctionReturnType(opaque_compiler_type_t type) {
  if (!type)
    return CompilerType();
  if (auto func = static_cast<LegacyFunction *>(type))
    return func->GetArgumentAtIndex(0);
  return CompilerType();
}

CompilerType TypeSystemLegacy::GetPointeeType(opaque_compiler_type_t type) {
  if (!type)
    return CompilerType();
  return static_cast<LegacyType *>(type)->GetElementType();
}

CompilerType TypeSystemLegacy::GetPointerType(opaque_compiler_type_t type) {
  if (!type)
    return CompilerType();

  LegacyType *base_type = static_cast<LegacyType *>(type);
  LegacyType *pointer = (*m_types)[base_type].get();
  if (pointer == nullptr) {
    ConstString type_name = GetTypeName(type, true);
    ConstString pointer_name(std::string("*") + type_name.GetCString());
    pointer = new LegacyElem(LegacyType::KIND_PTR, pointer_name,
                             CompilerType(weak_from_this(), type));
    (*m_types)[base_type].reset(pointer);
  }
  return CompilerType(weak_from_this(), pointer);
}

CompilerType
TypeSystemLegacy::GetLValueReferenceType(opaque_compiler_type_t type) {
  if (!type)
    return CompilerType();

  // TODOa
  Host::SystemLog(lldb::eSeverityInfo,
                  "Warning: DW_TAG_reference_type not supported.");
  return CompilerType();
}

CompilerType TypeSystemLegacy::GetTypedefedType(opaque_compiler_type_t type) {
  return CompilerType();
}

CompilerType
TypeSystemLegacy::GetNonReferenceType(opaque_compiler_type_t type) {
  return CompilerType(weak_from_this(), type);
}

bool TypeSystemLegacy::GetCompleteType(opaque_compiler_type_t type) {
  if (!type)
    return false;

  LegacyType *t = static_cast<LegacyType *>(type);
  if (t->GetArray())
    return t->GetElementType().GetCompleteType();

  if (LegacyStruct *s = t->GetStruct()) {
    if (s->IsComplete())
      return true;
    CompilerType compiler_type(weak_from_this(), s);
    SymbolFile *symbols = GetSymbolFile();
    return symbols && symbols->CompleteType(compiler_type);
  }

  if (static_cast<LegacyType *>(type)->GetLegacyKind() ==
      LegacyType::KIND_DYNAMIC) {
    const auto dyn_base_type =
        static_cast<LegacyDynamic *>(type)->GetBaseType();
    return dyn_base_type.GetCompleteType();
  }
  return true;
}

ConstString TypeSystemLegacy::GetTypeName(opaque_compiler_type_t type,
                                          bool baseType) {
  if (!type)
    return ConstString();

  LegacyType *base_type = static_cast<LegacyType *>(type);
  if (LegacyArray *array = base_type->GetArray()) {
    ConstString type_name = array->GetArrayTypeName();
    if (!type_name.IsEmpty())
      return type_name;

    ConstString array_base_type_name = array->GetElementType().GetTypeName();
    size_t length = array->GetLength();
    if (array_base_type_name == "BIT")
      length *= 8;
    ConstString array_type_name(array_base_type_name.GetCString() +
                                std::string(" [") + std::to_string(length) +
                                std::string("]"));
    return array_type_name;
  }

  if (static_cast<LegacyType *>(type)->GetLegacyKind() ==
      LegacyType::KIND_DYNAMIC) {
    const auto dyn_base_type =
        static_cast<LegacyDynamic *>(type)->GetBaseType();
    return dyn_base_type.GetTypeName();
  }
  return base_type->GetTypeName();
}

uint32_t
TypeSystemLegacy::GetTypeInfo(lldb::opaque_compiler_type_t type,
                              CompilerType *pointee_or_element_compiler_type) {
  if (pointee_or_element_compiler_type)
    pointee_or_element_compiler_type->Clear();

  if (!type)
    return 0;

  uint32_t flags = eTypeHasValue | eTypeIsBuiltIn | eTypeIsScalar;
  auto legacy_type = static_cast<LegacyType *>(type);
  auto type_kind = legacy_type->GetLegacyKind();
  switch (type_kind) {
  case LegacyType::KIND_FLOAT:
  case LegacyType::KIND_DOUBLE:
    flags |= eTypeIsFloat;
    break;
  case LegacyType::KIND_UINT:
    flags |= eTypeIsInteger;
    break;
  case LegacyType::KIND_INT:
    flags |= eTypeIsInteger | eTypeIsSigned;
    break;
  case LegacyType::KIND_ARRAY: {
    if (pointee_or_element_compiler_type)
      *pointee_or_element_compiler_type = legacy_type->GetElementType();
    flags = eTypeHasChildren | eTypeIsArray;

    const auto array_type = legacy_type->GetArray();
    if (array_type->isVarString())
      flags |= eTypeIsVarString;

    if (array_type->isDynamic())
      flags |= eTypeIsDynamic;

    // FIXME
    uint32_t length;
    if (IsCStringType(type, length))
      flags |= eTypeHasValue;
  } break;
  case LegacyType::KIND_PTR:
    flags = eTypeIsPointer | eTypeHasValue | eTypeHasChildren;
    break;
  case LegacyType::KIND_STRUCT:
    flags = eTypeHasChildren | eTypeIsStructUnion;
    break;
  case LegacyType::KIND_DYNAMIC: {
    const auto dyn_base_ty = static_cast<LegacyDynamic *>(type)->GetBaseType();
    flags = eTypeIsDynamic | (dyn_base_ty.GetTypeInfo());
    break;
  }
  case LegacyType::KIND_DECIMAL:
  case LegacyType::KIND_DISPLAY:
  case LegacyType::KIND_EDITED:
  default:
    break;
  }
  return flags;
}

bool TypeSystemLegacy::SupportsLanguage(LanguageType language) {
  return TypeSystemLegacySupportsLanguage(language);
}

LanguageType TypeSystemLegacy::GetMinimumLanguage(opaque_compiler_type_t type) {
  return GetLanguage();
}

static inline std::string
getStringFromDataExtractor(const lldb_private::DataExtractor &data,
                           uint32_t src_tyk, int32_t scale,
                           bool is_binscale = false) {
  offset_t offset = 0;
  switch (src_tyk) {
  case LegacyType::KIND_UINT:
  case LegacyType::KIND_INT: {
    const size_t length = data.GetByteSize();
    auto value = data.GetMaxS64(&offset, length);
    if (is_binscale && scale != 0)
      value /= std::pow(2, scale);
    return std::to_string(value);
  }
  case LegacyType::KIND_FLOAT: {
    auto value = data.GetFloat(&offset);
    if (is_binscale && scale != 0)
      value /= std::pow(2, scale);
    return std::to_string(value);
  }
  case LegacyType::KIND_DOUBLE: {
    auto value = data.GetDouble(&offset);
    if (is_binscale && scale != 0)
      value /= std::pow(2, scale);
    return std::to_string(value);
  }
  default:
    break;
  }
  return std::string(data.PeekCStr(0), data.GetByteSize());
}

bool TypeSystemLegacy::VerifyEncodeType(opaque_compiler_type_t src_type,
                                        opaque_compiler_type_t dst_type) {
  if (!src_type || !dst_type)
    return false;

  const auto dst_tyk = static_cast<LegacyType *>(dst_type)->GetLegacyKind();
  const auto src_tyk = static_cast<LegacyType *>(src_type)->GetLegacyKind();
  switch (dst_tyk) {
  case LegacyType::KIND_PTR:
  case LegacyType::KIND_INT:
  case LegacyType::KIND_UINT:
    return (src_tyk == LegacyType::KIND_UINT) ||
           (src_tyk == LegacyType::KIND_INT);
  case LegacyType::KIND_DOUBLE:
  case LegacyType::KIND_FLOAT:
    return (src_tyk == LegacyType::KIND_FLOAT) ||
           (src_tyk == LegacyType::KIND_DOUBLE);
  case LegacyType::KIND_DECIMAL:
  case LegacyType::KIND_EDITED:
  case LegacyType::KIND_DISPLAY: {
    return true;
  }
  case LegacyType::KIND_ARRAY: {
    uint32_t ty_len;
    return IsCStringType(src_type, ty_len);
  }
  case LegacyType::KIND_STRUCT:
  case LegacyType::KIND_FUNC:
  default:
    break;
  }
  return false;
}

bool TypeSystemLegacy::EncodeDataToType(ExecutionContext &exe_scope,
                                        opaque_compiler_type_t src_type,
                                        const DataExtractor &src_data,
                                        opaque_compiler_type_t dest_type,
                                        DataExtractor &dest_data,
                                        const LanguageType lang) {
  if (!VerifyEncodeType(src_type, dest_type)) {
    Host::SystemLog(lldb::eSeverityError, "Wrong assignment type.\n");
    return false;
  }

  LegacyType *lty = static_cast<LegacyType *>(dest_type);
  LegacyType *src_ty = static_cast<LegacyType *>(src_type);
  const auto type_kind = lty->GetLegacyKind();
  const auto src_type_kind = src_ty->GetLegacyKind();
  const auto type_sign = lty->GetSign();
  const auto type_scale = lty->GetScale();
  const auto type_digits_count = lty->GetDigitCount();
  const auto type_byte_order = lty->GetTypeByteOrder();
  const bool is_binscale = lty->is_binary_scale();
  const auto TypeLengthInBits = lty->GetTypeBitSize();
  const auto ty_length = (TypeLengthInBits + 7) / 8;
  const auto src_byte_order = endian::InlHostByteOrder();
  DataBufferSP dest_buffer = dest_data.GetSharedDataBuffer();
  dest_data.SetByteOrder(type_byte_order);
  const bool isPackedDecimal = (type_kind == LegacyType::KIND_DECIMAL);

  offset_t offset = 0;
  switch (type_kind) {
  case LegacyType::KIND_STRUCT:
  case LegacyType::KIND_FUNC:
    return false;
  default:
  case LegacyType::KIND_PTR:
  case LegacyType::KIND_INT:
  case LegacyType::KIND_UINT: {
    union {
      int8_t i8;
      int16_t i16;
      int32_t i32;
      int64_t i64;
    } iu;
    const size_t length = src_data.GetByteSize();
    switch (ty_length) {
    case 1:
      iu.i8 = src_data.GetMaxS64(&offset, length);
      break;
    case 2:
      iu.i16 = src_data.GetMaxS64(&offset, length);
      break;
    case 4:
      iu.i32 = src_data.GetMaxS64(&offset, length);
      break;
    case 8:
      iu.i64 = src_data.GetMaxS64(&offset, length);
      break;
    }
    DataExtractor temp(&iu, ty_length, src_byte_order, GetPointerByteSize());
    temp.CopyByteOrderedData(0, temp.GetByteSize(),
                             (void *)dest_buffer->GetBytes(), ty_length,
                             type_byte_order);
    return true;
  }
  case LegacyType::KIND_FLOAT: {
    float value;
    if (src_type_kind == LegacyType::KIND_FLOAT)
      value = src_data.GetFloat(&offset);
    else if (src_type_kind == LegacyType::KIND_DOUBLE)
      value = src_data.GetDouble(&offset);
    else {
      Host::SystemLog(lldb::eSeverityError, "Invalid float src data type.\n");
      return false;
    }

    if (type_scale) {
      auto exp = std::pow(is_binscale ? 2 : 10, type_scale);
      value /= exp;
    }
    DataExtractor temp(&value, sizeof(value), src_byte_order,
                       GetPointerByteSize());
    temp.CopyByteOrderedData(0, temp.GetByteSize(),
                             (void *)dest_buffer->GetBytes(), sizeof(value),
                             type_byte_order);
    return true;
  }
  case LegacyType::KIND_DOUBLE: {
    double value;
    if (src_type_kind == LegacyType::KIND_FLOAT)
      value = src_data.GetFloat(&offset);
    else if (src_type_kind == LegacyType::KIND_DOUBLE)
      value = src_data.GetDouble(&offset);
    else {
      Host::SystemLog(lldb::eSeverityError, "Invalid float src data type.\n");
      return false;
    }
    if (type_scale) {
      auto exp = std::pow(is_binscale ? 2 : 10, type_scale);
      value /= exp;
    }
    DataExtractor temp(&value, sizeof(value), src_byte_order,
                       GetPointerByteSize());
    temp.CopyByteOrderedData(0, temp.GetByteSize(),
                             (void *)dest_buffer->GetBytes(), sizeof(value),
                             type_byte_order);
    return true;
  }
  case LegacyType::KIND_ARRAY: {
    const auto array_type = lty->GetArray();
    const auto arr_len = array_type->GetLength();
    std::string val_str(getStringFromDataExtractor(src_data, src_type_kind,
                                                   type_scale, is_binscale));
    uint16_t str_length = val_str.size();
    if (str_length > arr_len) {
      Host::SystemLog(lldb::eSeverityError,
                      "Assignment string longer then available.\n");
      val_str.erase(arr_len, 100);
      str_length = val_str.size();
    }

    if (!array_type->isVarString()) {
      val_str.append(arr_len - str_length, ' ');
    }

    TargetCharsetReader Conv(exe_scope.GetTargetSP(), true);
    if (!Conv.IsValid()) {
      Host::SystemLog(lldb::eSeverityError,
                      "Assignment charset encoding failure.\n");
      return false;
    }

    if (!Conv.convert(val_str)) {
      Host::SystemLog(lldb::eSeverityError,
                      "Assignment charset encoding convertion failure.\n");
      return false;
    }

    if (array_type->isVarString()) {
      const uint8_t lsb = str_length & 0xff;
      const uint8_t msb = (str_length >> 8) & 0xff;

      // big endian str length at start in 2 bytes.
      val_str.insert(0, 1, lsb);
      val_str.insert(0, 1, msb);
    }
    DataExtractor temp(val_str.c_str(), val_str.size(), type_byte_order,
                       GetPointerByteSize());
    temp.CopyByteOrderedData(0, temp.GetByteSize(),
                             (void *)dest_buffer->GetBytes(), val_str.size(),
                             type_byte_order);
    return true;
  } break;
  case LegacyType::KIND_DECIMAL:
  case LegacyType::KIND_EDITED:
  case LegacyType::KIND_DISPLAY:
    // Encode Display Type
    std::string val_str(getStringFromDataExtractor(src_data, src_type_kind,
                                                   type_scale, is_binscale));
    char sign;
    const auto no_digits = isPackedDecimal ? type_digits_count : ty_length;
    if (val_str[0] == '-') {
      sign = '-';
      val_str.erase(0, 1);
    } else {
      sign = '+';
      if (val_str[0] == '+')
        val_str.erase(0, 1);
    }
    val_str.erase(0, val_str.find_first_not_of('0'));
    if (val_str.size() == 0) {
      val_str = std::string(no_digits, '0');
    } else {
      const int digit_len = no_digits + type_scale;
      auto pos = val_str.find('.');
      if (pos != std::string::npos) {
        val_str.erase(pos, 1);
      } else {
        pos = val_str.size();
      }

      // FIXME
      if (digit_len > (int)pos)
        val_str.insert(0, digit_len - pos, '0');
      else
        val_str.erase(0, pos - digit_len);

      if (val_str.size() > no_digits)
        val_str.erase(no_digits, 100);
      else
        val_str.append(no_digits - val_str.size(), '0');
    }

    switch (type_sign) {
    case LegacyType::eTrailingSeparate:
      val_str.push_back(sign);
      break;
    case LegacyType::eLeadingSeparate:
      val_str = sign + val_str;
      break;
    case LegacyType::eTrailingOverpunch:
      val_str.back() = EncodeOverpunchDigit(val_str.back(), sign == '+');
      break;
    case LegacyType::eLeadingOverpunch:
      val_str.front() = EncodeOverpunchDigit(val_str.front(), sign == '+');
      break;
    case LegacyType::eUnsigned:
    default:
      break;
    }

    uint8_t buffer[128];
    size_t count = 0;
    if (isPackedDecimal) {
      if (val_str.size() % 2)
        val_str.insert(0, 1, '0');

      for (size_t i = 0; i < val_str.size(); i += 2) {
        buffer[count++] = (EncodePackedSign(val_str[i]) << 4) |
                          (EncodePackedSign(val_str[i + 1]));
      }
    } else {
      TargetCharsetReader Conv(exe_scope.GetTargetSP(), true);
      if (!Conv.IsValid()) {
        Host::SystemLog(lldb::eSeverityError,
                        "Assignment charset encoding failure.\n");
        return false;
      }
      if (!Conv.convert(val_str)) {
        Host::SystemLog(lldb::eSeverityError,
                        "Assignment charset encoding convertion failure.\n");
        return false;
      }
      count = val_str.size();
      val_str.copy((char *)buffer, count);
    }
    DataExtractor temp(buffer, count, src_byte_order, GetPointerByteSize());
    temp.CopyByteOrderedData(0, temp.GetByteSize(),
                             (void *)dest_buffer->GetBytes(), count,
                             type_byte_order);
    return true;
  }
  return false;
}

//----------------------------------------------------------------------
// Exploring the type
//----------------------------------------------------------------------
std::optional<uint64_t>
TypeSystemLegacy::GetBitSize(opaque_compiler_type_t type,
                             ExecutionContextScope *exe_scope) {
  if (!type)
    return std::nullopt;

  LegacyType *base_type = static_cast<LegacyType *>(type);
  int kind = base_type->GetLegacyKind();

  switch (kind) {
  default:
    break;
  case LegacyType::KIND_DECIMAL:
  case LegacyType::KIND_DISPLAY:
  case LegacyType::KIND_EDITED:
  case LegacyType::KIND_FLOAT:
  case LegacyType::KIND_DOUBLE:
  case LegacyType::KIND_INT:
  case LegacyType::KIND_UINT: {
    return static_cast<LegacyType *>(type)->GetTypeBitSize();
  }
  case LegacyType::KIND_ARRAY: {
    LegacyArray *array = base_type->GetArray();
    if (std::optional<uint64_t> bit_size =
            array->GetElementType().GetBitSize(exe_scope))
      return (array->GetLength() + (array->isVarString() ? 2UL : 0UL)) *
             (*bit_size);
    return std::nullopt;
  }
  case LegacyType::KIND_PTR:
    return m_pointer_byte_size * 8;
  case LegacyType::KIND_STRUCT:
    return base_type->GetStruct()->GetByteSize() * 8;
  case LegacyType::KIND_DYNAMIC: {
    const auto dyn_base_type =
        static_cast<LegacyDynamic *>(type)->GetBaseType();
    return dyn_base_type.GetBitSize(exe_scope);
  }
  case LegacyType::KIND_L88:
    return 1UL;
  }
  return std::nullopt;
}

Format TypeSystemLegacy::GetFormat(opaque_compiler_type_t type) {
  if (!type)
    return eFormatDefault;

  switch (static_cast<LegacyType *>(type)->GetLegacyKind()) {
  case LegacyType::KIND_DECIMAL:
  case LegacyType::KIND_DISPLAY:
  case LegacyType::KIND_EDITED:
    return eFormatChar;
  case LegacyType::KIND_FLOAT:
  case LegacyType::KIND_DOUBLE:
    return eFormatFloat;
  case LegacyType::KIND_INT:
    return eFormatDecimal;
  case LegacyType::KIND_UINT:
    return eFormatUnsigned;
  case LegacyType::KIND_PTR:
    return eFormatHex;
  case LegacyType::KIND_ARRAY:
    return eFormatVoid;
  case LegacyType::KIND_DYNAMIC: {
    const auto dyn_base_type =
        static_cast<LegacyDynamic *>(type)->GetBaseType();
    return dyn_base_type.GetFormat();
  }
  case LegacyType::KIND_L88:
    return eFormatBoolean;
  default:
    return eFormatBytes;
  }
}

Encoding TypeSystemLegacy::GetEncoding(opaque_compiler_type_t type,
                                       uint64_t &count) {
  bool is_signed;
  if (IsIntegerType(type, is_signed))
    return is_signed ? eEncodingSint : eEncodingUint;
  count = 1;

  bool is_complex;
  uint32_t complex_count;
  if (IsFloatingPointType(type, complex_count, is_complex)) {
    count = complex_count;
    return eEncodingIEEE754;
  }

  if (IsPointerType(type))
    return eEncodingUint;

  if (static_cast<LegacyType *>(type)->GetLegacyKind() ==
      LegacyType::KIND_DYNAMIC) {
    const auto dyn_base_type =
        static_cast<LegacyDynamic *>(type)->GetBaseType();
    return dyn_base_type.GetEncoding(count);
  }

  count = 0;
  return eEncodingInvalid;
}

llvm::Expected<uint32_t>
TypeSystemLegacy::GetNumChildren(opaque_compiler_type_t type,
                                 bool omit_empty_base_classes,
                                 const ExecutionContext *exe_ctx) {
  if (!type)
    return 0;

  if (!GetCompleteType(type))
    return 0;

  LegacyType *base_type = static_cast<LegacyType *>(type);
  if (LegacyArray *array = base_type->GetArray())
    return array->GetLength();

  if (LegacyStruct *struct_type = base_type->GetStruct())
    return struct_type->GetNumMembers();

  if (static_cast<LegacyType *>(type)->GetLegacyKind() ==
      LegacyType::KIND_DYNAMIC) {
    const auto dyn_base_type =
        static_cast<LegacyDynamic *>(type)->GetBaseType();
    return dyn_base_type.GetNumChildren(omit_empty_base_classes, exe_ctx);
  }

  return 0;
}

std::optional<size_t>
TypeSystemLegacy::GetTypeBitAlign(opaque_compiler_type_t type,
                                  ExecutionContextScope *exe_scope) {
  return 0;
}

CompilerType TypeSystemLegacy::GetBasicTypeFromAST(BasicType basic_type) {
  LegacyType *type = (*m_basic_types)[basic_type].get();
  if (type)
    return CompilerType(weak_from_this(), type);

  switch (basic_type) {
  default:
    return CompilerType();
  case eBasicTypeUnsignedInt: {
    type = new LegacyType(LegacyType::KIND_UINT, ConstString("unsigned int"),
                          ConstString(), 64, 0, 0, 0,
                          endian::InlHostByteOrder(), false);
    (*m_basic_types)[basic_type].reset(type);
    break;
  }
  case eBasicTypeInt: {
    type =
        new LegacyType(LegacyType::KIND_INT, ConstString("int"), ConstString(),
                       64, 0, 0, 0, endian::InlHostByteOrder(), false);
    (*m_basic_types)[basic_type].reset(type);
    break;
  }
  case eBasicTypeFloat: {
    type = new LegacyType(LegacyType::KIND_FLOAT, ConstString("float"),
                          ConstString(), 32, 0, 0, 0,
                          endian::InlHostByteOrder(), false);
    (*m_basic_types)[basic_type].reset(type);
    break;
  }
  case eBasicTypeDouble: {
    type = new LegacyType(LegacyType::KIND_DOUBLE, ConstString("double"),
                          ConstString(), 64, 0, 0, 0,
                          endian::InlHostByteOrder(), false);
    (*m_basic_types)[basic_type].reset(type);
    break;
  }
  case eBasicTypeChar: {
    type = new LegacyType(LegacyType::KIND_DISPLAY, ConstString("char"),
                          ConstString(), 8, 0, 0, 0, endian::InlHostByteOrder(),
                          false);
    (*m_basic_types)[basic_type].reset(type);
    break;
  }
  case eBasicTypeBool:
    type =
        new LegacyType(LegacyType::KIND_UINT, ConstString("bit"), ConstString(),
                       8, 0, 0, 0, endian::InlHostByteOrder(), false);
    (*m_basic_types)[basic_type].reset(type);
    break;
  case eBasicTypeOther:
    type = new LegacyType(LegacyType::KIND_L88, ConstString("Level88"),
                          ConstString(), 0, 0, 0, 0, endian::InlHostByteOrder(),
                          false);
  }
  return CompilerType(weak_from_this(), type);
}

CompilerType
TypeSystemLegacy::DynGetBaseType(lldb::opaque_compiler_type_t type) const {
  if (!type)
    return CompilerType();

  int kind = static_cast<LegacyType *>(type)->GetLegacyKind();
  if (kind != LegacyType::KIND_DYNAMIC)
    return CompilerType();

  const auto dyn = static_cast<LegacyDynamic *>(type);
  return dyn->GetBaseType();
}

DWARFExpressionList
TypeSystemLegacy::DynGetLocation(lldb::opaque_compiler_type_t type) const {
  if (!type)
    return DWARFExpressionList();

  int kind = static_cast<LegacyType *>(type)->GetLegacyKind();
  if (kind != LegacyType::KIND_DYNAMIC)
    return DWARFExpressionList();

  const auto dyn = static_cast<LegacyDynamic *>(type);
  return dyn->getLocation();
}

DWARFExpressionList
TypeSystemLegacy::DynGetAllocated(lldb::opaque_compiler_type_t type) const {
  if (!type)
    return DWARFExpressionList();

  int kind = static_cast<LegacyType *>(type)->GetLegacyKind();
  if (kind != LegacyType::KIND_DYNAMIC)
    return DWARFExpressionList();

  const auto dyn = static_cast<LegacyDynamic *>(type);
  return dyn->getAllocated();
}

DWARFExpressionList
TypeSystemLegacy::DynArrGetCountExp(lldb::opaque_compiler_type_t type) const {
  if (!type)
    return DWARFExpressionList();

  int kind = static_cast<LegacyType *>(type)->GetLegacyKind();
  if (kind != LegacyType::KIND_ARRAY ||
      !static_cast<LegacyArray *>(type)->isDynamic())
    return DWARFExpressionList();

  const auto dyn_arr = static_cast<LegacyDynamicArray *>(type);
  return dyn_arr->GetCountExp();
}

bool TypeSystemLegacy::DynArrUpdateLength(lldb::opaque_compiler_type_t type,
                                          uint64_t length) {
  if (!type)
    return false;

  int kind = static_cast<LegacyType *>(type)->GetLegacyKind();
  if (kind != LegacyType::KIND_ARRAY ||
      !static_cast<LegacyArray *>(type)->isDynamic())
    return false;

  const auto dyn_array = static_cast<LegacyDynamicArray *>(type);
  dyn_array->UpdateLength(length);
  return true;
}

lldb::BasicType
TypeSystemLegacy::GetBasicTypeEnumeration(opaque_compiler_type_t type) {
  return eBasicTypeUnsignedInt;
}

CompilerType TypeSystemLegacy::GetFieldAtIndex(opaque_compiler_type_t type,
                                               size_t idx, std::string &name,
                                               uint64_t *bit_offset_ptr,
                                               uint32_t *bitfield_bit_size_ptr,
                                               bool *is_bitfield_ptr) {
  if (bit_offset_ptr)
    *bit_offset_ptr = 0;
  if (bitfield_bit_size_ptr)
    *bitfield_bit_size_ptr = 0;
  if (is_bitfield_ptr)
    *is_bitfield_ptr = false;

  if (!type || !GetCompleteType(type))
    return CompilerType();

  if (LegacyStruct *st = static_cast<LegacyType *>(type)->GetStruct()) {
    const auto *field = st->GetMemberAtIndex(idx);
    if (field) {
      name = std::string(field->m_name.GetStringRef());
      if (bit_offset_ptr)
        *bit_offset_ptr = field->m_offset_in_bits;
      return field->m_type;
    }
  }

  if (static_cast<LegacyType *>(type)->GetLegacyKind() ==
      LegacyType::KIND_DYNAMIC) {
    const auto dyn_base_type =
        static_cast<LegacyDynamic *>(type)->GetBaseType();
    return dyn_base_type.GetFieldAtIndex(
        idx, name, bit_offset_ptr, bitfield_bit_size_ptr, is_bitfield_ptr);
  }
  // TODO struct
  return CompilerType();
}

llvm::Expected<CompilerType> TypeSystemLegacy::GetChildCompilerTypeAtIndex(
    opaque_compiler_type_t type, ExecutionContext *exe_ctx, size_t idx,
    bool transparent_pointers, bool omit_empty_base_classes,
    bool ignore_array_bounds, std::string &child_name,
    uint32_t &child_byte_size, int32_t &child_byte_offset,
    uint32_t &child_bitfield_bit_size, uint32_t &child_bitfield_bit_offset,
    bool &child_is_base_class, bool &child_is_deref_of_parent,
    ValueObject *valobj, uint64_t &language_flags) {

  auto num_children_or_error =
      GetNumChildren(type, omit_empty_base_classes, exe_ctx);
  if (!num_children_or_error) {
    return num_children_or_error.takeError();
  }

  uint32_t num_children = *num_children_or_error;
  const bool idx_is_valid = idx < num_children;

  auto get_exe_scope = [&exe_ctx]() {
    return exe_ctx ? exe_ctx->GetBestExecutionContextScope() : nullptr;
  };

  int kind = static_cast<LegacyType *>(type)->GetLegacyKind();

  switch (kind) {
  default:
    break;
  case LegacyType::KIND_PTR: {
    CompilerType pointee_type(GetPointeeType(type));

    if (transparent_pointers && pointee_type.IsAggregateType()) {
      // TODO
      return CompilerType();
    } else {
      child_is_deref_of_parent = true;

      const char *parent_name = valobj ? valobj->GetName().GetCString() : NULL;
      if (parent_name) {
        child_name.assign(1, '*');
        child_name += parent_name;
      }

      if (idx == 0) {
        if (std::optional<uint64_t> size =
                pointee_type.GetByteSize(get_exe_scope())) {
          child_byte_size = *size;
          child_byte_offset = 0;
          return pointee_type;
        }
      }
    }
  } break;
  case LegacyType::KIND_ARRAY: {
    LegacyArray *array = static_cast<LegacyType *>(type)->GetArray();
    if (ignore_array_bounds || idx_is_valid) {
      CompilerType element_type = array->GetElementType();
      if (element_type.GetCompleteType()) {
        child_name = llvm::formatv("[{0}]", idx);
        if (auto byte_size = element_type.GetByteSize(
                exe_ctx ? exe_ctx->GetBestExecutionContextScope() : NULL))
          child_byte_size = *byte_size;
        child_byte_offset = (int32_t)idx * (int32_t)child_byte_size;
        if (array->isVarString())
          child_byte_offset += 2;
        return element_type;
      }
    }
  } break;
  case LegacyType::KIND_STRUCT: {
    uint64_t bit_offset;
    CompilerType ret =
        GetFieldAtIndex(type, idx, child_name, &bit_offset, nullptr, nullptr);
    if (std::optional<uint64_t> byte_size = ret.GetByteSize(
            exe_ctx ? exe_ctx->GetBestExecutionContextScope() : nullptr))
      child_byte_size = *byte_size;
    child_byte_offset = bit_offset / 8;
    return ret;
  } break;
  case LegacyType::KIND_DYNAMIC: {
    const auto dyn_base_type =
        static_cast<LegacyDynamic *>(type)->GetBaseType();
    return dyn_base_type.GetChildCompilerTypeAtIndex(
        exe_ctx, idx, transparent_pointers, omit_empty_base_classes,
        ignore_array_bounds, child_name, child_byte_size, child_byte_offset,
        child_bitfield_bit_size, child_bitfield_bit_offset, child_is_base_class,
        child_is_deref_of_parent, valobj, language_flags);
  }
  }
  return CompilerType();
}

uint32_t
TypeSystemLegacy::GetIndexOfChildWithName(lldb::opaque_compiler_type_t type,
                                          llvm::StringRef name,
                                          bool omit_empty_base_classes) {
  if (!type)
    return UINT32_MAX;

  if (!GetCompleteType(type))
    return UINT32_MAX;

  LegacyType *t = static_cast<LegacyType *>(type);
  if (LegacyStruct *stype = t->GetStruct()) {
    for (size_t i = 0; i < stype->GetNumMembers(); ++i) {
      if (const auto member = stype->GetMemberAtIndex(i)) {
        if (ConstString::Equals(member->GetMemberName(), ConstString(name),
                                false)) {
          return i;
        }
      }
    }
  }
  if (static_cast<LegacyType *>(type)->GetLegacyKind() ==
      LegacyType::KIND_DYNAMIC) {
    const auto dyn_base_type =
        static_cast<LegacyDynamic *>(type)->GetBaseType();
    return dyn_base_type.GetIndexOfChildWithName(name, omit_empty_base_classes);
  }
  return UINT32_MAX;
}

size_t TypeSystemLegacy::GetIndexOfChildMemberWithName(
    lldb::opaque_compiler_type_t type, llvm::StringRef name,
    bool omit_empty_base_classes, std::vector<uint32_t> &child_indexes) {
  uint32_t index = GetIndexOfChildWithName(type, name, omit_empty_base_classes);
  if (index == UINT32_MAX)
    return 0;
  child_indexes.push_back(index);
  return 1;
}

lldb::TypeClass TypeSystemLegacy::GetTypeClass(opaque_compiler_type_t type) {
  if (!type)
    return eTypeClassInvalid;

  if (IsPointerType(type))
    return eTypeClassPointer;

  int kind = static_cast<LegacyType *>(type)->GetLegacyKind();

  if (kind == LegacyType::KIND_FUNC)
    return eTypeClassFunction;

  if (kind == LegacyType::KIND_DYNAMIC) {
    const auto dyn_base_ty = static_cast<LegacyDynamic *>(type)->GetBaseType();
    return dyn_base_ty.GetTypeClass();
  }

  switch (kind) {
  case LegacyType::KIND_FUNC:
    return eTypeClassFunction;
  case LegacyType::KIND_DISPLAY:
  case LegacyType::KIND_EDITED:
  case LegacyType::KIND_DECIMAL:
    return eTypeClassBuiltin;
  case LegacyType::KIND_ARRAY:
    return eTypeClassArray;
  case LegacyType::KIND_STRUCT:
    return eTypeClassStruct;
  }
  return eTypeClassInvalid;
}

int TypeSystemLegacy::GetFunctionArgumentCount(opaque_compiler_type_t type) {
  // add param type list
  return 0;
}

unsigned TypeSystemLegacy::GetTypeQualifiers(opaque_compiler_type_t type) {
  return 0;
}

CompilerType
TypeSystemLegacy::GetFullyUnqualifiedType(opaque_compiler_type_t type) {
  return CompilerType(weak_from_this(), type);
}

//----------------------------------------------------------------------
// Creating Types
//----------------------------------------------------------------------

CompilerType TypeSystemLegacy::CreateArrayType(
    const ConstString &array_type_name, const ConstString &name,
    const CompilerType &element_type, size_t element_count, bool isVarString) {
  if (!element_type.IsValid())
    return CompilerType();

  if (element_count == 0) {
    char buffer[128];
    sprintf(buffer,
            "Warning: need to add support for "
            "DW_TAG_array_type '%s' with dynamic size",
            name.GetCString());
    Host::SystemLog(lldb::eSeverityWarning, std::string(buffer));
    return CompilerType();
  }

  if (isVarString && (element_count > 2))
    element_count -= 2;

  LegacyType *array_type = new LegacyArray(array_type_name, name, element_type,
                                           element_count, isVarString);
  return CompilerType(weak_from_this(), array_type);
}

CompilerType TypeSystemLegacy::CreateArrayType(
    const ConstString &array_type_name, const ConstString &name,
    const CompilerType &element_type, DWARFExpressionList element_count,
    bool isVarString) {
  if (!element_type.IsValid())
    return CompilerType();

  LegacyType *array_type = new LegacyDynamicArray(
      array_type_name, name, element_type, element_count, isVarString);
  return CompilerType(weak_from_this(), array_type);
}

CompilerType
TypeSystemLegacy::CreateDynamicType(const CompilerType &base_type,
                                    const DWARFExpressionList &dw_location,
                                    const DWARFExpressionList &dw_allocated) {
  if (!base_type)
    return CompilerType();

  LegacyType *type = static_cast<LegacyType *>(base_type.GetOpaqueQualType());
  if (!type)
    return CompilerType();

  if (type->GetLegacyKind() == LegacyType::KIND_DYNAMIC)
    return CompilerType();

  LegacyType *dyn_type =
      new LegacyDynamic(base_type, dw_location, dw_allocated);
  return CompilerType(weak_from_this(), dyn_type);
}

CompilerType TypeSystemLegacy::GetArrayType(lldb::opaque_compiler_type_t type,
                                            uint64_t size) {
  if (!type)
    return CompilerType();

  if (size == 0)
    return CompilerType();

  LegacyType *element_type = static_cast<LegacyType *>(type);
  LegacyType *array_type = new LegacyArray(
      ConstString(), ConstString(""),
      CompilerType(weak_from_this(), element_type), size, false);
  return CompilerType(weak_from_this(), array_type);
}

void TypeSystemLegacy::AddFieldToStruct(const CompilerType &struct_type,
                                        const ConstString &member_name,
                                        const CompilerType &member_type,
                                        uint32_t offset_in_bits) {

  if (!struct_type)
    return;

  LegacyType *type = static_cast<LegacyType *>(struct_type.GetOpaqueQualType());
  if (!type)
    return;

  if (LegacyStruct *stype = type->GetStruct())
    stype->AddMember(member_name, member_type, offset_in_bits);
}

void TypeSystemLegacy::CompleteStructType(const CompilerType &struct_type) {
  if (!struct_type)
    return;

  LegacyType *type = static_cast<LegacyType *>(struct_type.GetOpaqueQualType());
  if (!type)
    return;

  if (LegacyStruct *stype = type->GetStruct())
    stype->SetComplete();
}

CompilerType TypeSystemLegacy::CreateStructType(const ConstString &name,
                                                uint32_t byte_size) {
  LegacyStruct *type = new LegacyStruct(name, byte_size);
  return CompilerType(weak_from_this(), type);
}

CompilerType TypeSystemLegacy::CreateFunctionType(const ConstString &name,
                                                  CompilerType *params,
                                                  size_t params_count,
                                                  bool is_variadic) {
  LegacyType *type =
      new LegacyFunction(name, params, params_count, is_variadic);
  return CompilerType(weak_from_this(), type);
}

CompilerType TypeSystemLegacy::CreateBaseType(
    const ConstString &name, const ConstString &pic_string, uint32_t dw_ate,
    uint32_t bitsize, uint32_t dw_sign, int32_t scale, uint32_t digit_count,
    ByteOrder byte_order, bool bin_scale) {
  int kind = LegacyType::KIND_INVALID;

  switch (dw_ate) {
  case DW_ATE_signed:
  case DW_ATE_signed_fixed:
    kind = LegacyType::KIND_INT;
    break;
  case DW_ATE_unsigned:
  case DW_ATE_unsigned_fixed:
    kind = LegacyType::KIND_UINT;
    break;
  case DW_ATE_signed_char:
  case DW_ATE_unsigned_char:
  case DW_ATE_numeric_string:
    kind = LegacyType::KIND_DISPLAY;
    break;
  case DW_ATE_edited:
    kind = LegacyType::KIND_EDITED;
    break;
  case DW_ATE_packed_decimal:
    kind = LegacyType::KIND_DECIMAL;
    break;
  case DW_ATE_float: {
    if (bitsize == 64)
      kind = LegacyType::KIND_DOUBLE;
    else
      kind = LegacyType::KIND_FLOAT;
  } break;
  case DW_ATE_boolean:
    kind = LegacyType::KIND_L88;
    break;
  }

  uint32_t sign;
  switch (dw_sign) {
  case DW_DS_leading_overpunch:
    sign = LegacyType::eLeadingOverpunch;
    break;
  case DW_DS_trailing_overpunch:
    sign = LegacyType::eTrailingOverpunch;
    break;
  case DW_DS_leading_separate:
    sign = LegacyType::eLeadingSeparate;
    break;
  case DW_DS_trailing_separate:
    sign = LegacyType::eTrailingSeparate;
    break;
  default:
    sign = LegacyType::eUnsigned;
  }

  if (kind == LegacyType::KIND_INVALID) {
    char buffer[256];
    sprintf(buffer,
            "Warning: need to add support for "
            "DW_TAG_base_type '%s' encoded with "
            "DW_ATE = 0x%x, bit_size = %u\n, "
            "sign = %u, scale = %d, byte_order = %u.\n",
            name.GetCString(), dw_ate, bitsize, dw_sign, scale, byte_order);
    std::string message(buffer);
    Host::SystemLog(lldb::eSeverityWarning, message);
    return CompilerType();
  }

  LegacyType *type = new LegacyType(kind, name, pic_string, bitsize, sign,
                                    scale, digit_count, byte_order, bin_scale);
  return CompilerType(weak_from_this(), type);
}

//----------------------------------------------------------------------
// Creating related types
//----------------------------------------------------------------------
CompilerType
TypeSystemLegacy::GetArrayElementType(opaque_compiler_type_t type,
                                      ExecutionContextScope *exe_scope) {
  LegacyArray *array = static_cast<LegacyType *>(type)->GetArray();
  if (array) {
    return array->GetElementType();
  }
  if (static_cast<LegacyType *>(type)->GetLegacyKind() ==
      LegacyType::KIND_DYNAMIC) {
    const auto dyn_base_type =
        static_cast<LegacyDynamic *>(type)->GetBaseType();
    return dyn_base_type.GetArrayElementType(exe_scope);
  }
  return CompilerType();
}

TypeMemberFunctionImpl
TypeSystemLegacy::GetMemberFunctionAtIndex(opaque_compiler_type_t type,
                                           size_t idx) {
  return TypeMemberFunctionImpl();
}

CompilerType TypeSystemLegacy::GetCanonicalType(opaque_compiler_type_t type) {
  return CompilerType(weak_from_this(), type);
}

CompilerType
TypeSystemLegacy::GetEnumerationIntegerType(opaque_compiler_type_t type) {
  return CompilerType(weak_from_this(), type);
}

TypeSystemLegacy::TypeSystemLegacy(TargetSP target)
    : m_pointer_byte_size(0), m_target_wp(target), m_types(new TypeMap),
      m_basic_types(new BasicTypeMap), m_mutated_types(new MutatedTypeMap) {}

TypeSystemLegacy::~TypeSystemLegacy() {}

//------------------------------------------------------------------
// PluginInterface functions
//------------------------------------------------------------------

uint32_t TypeSystemLegacy::GetPluginVersion() { return 1; }
llvm::StringRef TypeSystemLegacy::GetPluginNameStatic() {
  return llvm::StringRef("legacy");
}

llvm::StringRef TypeSystemLegacy::GetPluginName() {
  return TypeSystemLegacy::GetPluginNameStatic();
}

TypeSystemSP TypeSystemLegacy::CreateInstance(lldb::LanguageType language,
                                              lldb_private::Module *module,
                                              Target *target) {
  if (TypeSystemLegacySupportsLanguage(language)) {
    ArchSpec arch;
    std::shared_ptr<TypeSystemLegacy> ast_sp;

    if (module) {
      arch = module->GetArchitecture();
      ast_sp = std::make_shared<TypeSystemLegacy>();
    } else if (target) {
      arch = target->GetArchitecture();
      ast_sp = std::make_shared<TypeSystemLegacy>(target->shared_from_this());
    }

    ast_sp->SetLanguage(language);

    if (arch.IsValid())
      ast_sp->SetAddressByteSize(arch.GetAddressByteSize());

    return ast_sp;
  }
  return TypeSystemSP();
}

LanguageSet TypeSystemLegacy::GetSupportedLanguagesForTypes() {
  LanguageSet languages;
  languages.Insert(eLanguageTypePLI);
  languages.Insert(eLanguageTypeCobol74);
  languages.Insert(eLanguageTypeCobol85);
  return languages;
}

LanguageSet TypeSystemLegacy::GetSupportedLanguagesForExpressions() {
  LanguageSet languages;
  languages.Insert(eLanguageTypePLI);
  languages.Insert(eLanguageTypeCobol74);
  languages.Insert(eLanguageTypeCobol85);
  return languages;
}

void TypeSystemLegacy::Initialize() {
  PluginManager::RegisterPlugin(GetPluginNameStatic(), "AST context plug-in",
                                CreateInstance, GetSupportedLanguagesForTypes(),
                                GetSupportedLanguagesForExpressions());
}

void TypeSystemLegacy::Terminate() {
  PluginManager::UnregisterPlugin(CreateInstance);
}

void TypeSystemLegacy::Finalize() {}

lldb_private::plugin::dwarf::DWARFASTParser *
TypeSystemLegacy::GetDWARFParser() {
  if (!m_dwarf_ast_parser_ap)
    m_dwarf_ast_parser_ap.reset(new DWARFASTParserLegacy(
        *std::static_pointer_cast<TypeSystemLegacy>(weak_from_this().lock())));

  return m_dwarf_ast_parser_ap.get();
}

//----------------------------------------------------------------------
// Dumping types
//----------------------------------------------------------------------
#define DEPTH_INCREMENT 2

#ifndef NDEBUG
LLVM_DUMP_METHOD void
TypeSystemLegacy::dump(lldb::opaque_compiler_type_t type) const {
  // FIXME
  return;
}
#endif

bool TypeSystemLegacy::DumpTypeValue(opaque_compiler_type_t type, Stream &s,
                                     Format format, const DataExtractor &data,
                                     offset_t byte_offset, size_t byte_size,
                                     uint32_t bitfield_bit_size,
                                     uint32_t bitfield_bit_offset,
                                     ExecutionContextScope *exe_scope) {
  if (!type)
    return false;

  if (IsAggregateType(type)) {
    /*
    // FIXME
    ExecutionContext exe_ctx(exe_scope);
    DumpValue(type, &exe_ctx, s, format, data, byte_offset, byte_size,
              bitfield_bit_size, bitfield_bit_offset, true, true, true, 0);
    */
    return false;
  }

  auto type_kind = static_cast<LegacyType *>(type)->GetLegacyKind();
  auto type_sign = static_cast<LegacyType *>(type)->GetSign();
  auto type_scale = static_cast<LegacyType *>(type)->GetScale();
  auto byte_order = static_cast<LegacyType *>(type)->GetTypeByteOrder();
  const bool is_binscale = static_cast<LegacyType *>(type)->is_binary_scale();
  const bool skipIconv = (type_kind == LegacyType::KIND_DECIMAL);
  const bool skipFormatting = (type_kind == LegacyType::KIND_EDITED);

  switch (type_kind) {
  case LegacyType::KIND_INT:
  case LegacyType::KIND_UINT:
  case LegacyType::KIND_FLOAT:
  case LegacyType::KIND_DOUBLE: {
    DataExtractor format_data;
    format_data.SetData(data, 0, data.GetByteSize());
    format_data.SetAddressByteSize(data.GetAddressByteSize());
    format_data.SetByteOrder(byte_order);
    if (type_scale) {
      offset_t offset = 0;
      // TODO - Fix me
      auto data = std::pow(is_binscale ? 2 : 10, type_scale);
      if (type_kind == LegacyType::KIND_INT) {
        data *= format_data.GetMaxS64(&offset, byte_size);
      } else {
        data *= format_data.GetMaxU64(&offset, byte_size);
      }
      format_data.SetData((void *)&data, sizeof(data),
                          endian::InlHostByteOrder());
      return DumpDataExtractor(format_data, &s, byte_offset, eFormatFloat,
                               sizeof(data), 1 /*item_count*/, UINT32_MAX,
                               LLDB_INVALID_ADDRESS, bitfield_bit_size,
                               bitfield_bit_offset, exe_scope);
    }
    return DumpDataExtractor(format_data, &s, byte_offset, format, byte_size,
                             1 /*item_count*/, UINT32_MAX, LLDB_INVALID_ADDRESS,
                             bitfield_bit_size, bitfield_bit_offset, exe_scope);
  } break;
  case LegacyType::KIND_DECIMAL:
  case LegacyType::KIND_EDITED:
  case LegacyType::KIND_DISPLAY: {

    switch (format) {
    default:
      return DumpDataExtractor(data, &s, byte_offset, format, 1, byte_size,
                               UINT32_MAX, LLDB_INVALID_ADDRESS,
                               bitfield_bit_size, bitfield_bit_offset,
                               exe_scope);
      break;
    case eFormatCString:
    case eFormatCharArray:
    case eFormatCharPrintable:
    case eFormatChar: {
      DataExtractor format_data;
      std::string format_string;

      uint8_t buffer[64];

      if (byte_size > 64) {
        Host::SystemLog(lldb::eSeverityError,
                        "Error: Display/Decimal type with invalid width"
                        ", Displaying only first 64 bytes.\n");
        byte_size = 64;
      }
      data.CopyByteOrderedData(0, byte_size, buffer, byte_size, byte_order);

      if (!skipIconv && exe_scope) {
        TargetCharsetReader Conv(exe_scope->CalculateTarget());
        if (!Conv.IsValid()) {
          char buffer[64];
          sprintf(buffer, "WARNING: Invalid target charset %s.\n",
                  Conv.getTargetFormat().GetCString());
          Host::SystemLog(lldb::eSeverityWarning, std::string(buffer));
        } else
          Conv.convert(reinterpret_cast<char *>(buffer), byte_size);
      }

      if (type_kind == LegacyType::KIND_DECIMAL) {
        for (offset_t i = 0; i < byte_size; ++i) {
          format_string += std::to_string((buffer[i] & 0xf0) >> 4);
          format_string += DecodePackedSignToString((buffer[i] & 0x0f));
          type_sign = LegacyType::eTrailingSeparate;
        }
      } else {
        format_string.assign(buffer, buffer + byte_size);
      }

      /// extract sign and decode over-punch digit
      char sign = ' ';
      switch (type_sign) {
      case LegacyType::eTrailingSeparate: {
        sign = format_string.back();
        format_string.pop_back();
      } break;
      case LegacyType::eTrailingOverpunch: {
        sign = DecodeOverpunchDigit(format_string.back());
      } break;
      case LegacyType::eLeadingOverpunch: {
        sign = DecodeOverpunchDigit(format_string.front());
      } break;
      case LegacyType::eLeadingSeparate: {
        sign = format_string.front();
        format_string.erase(0, 1);
      } break;
      case LegacyType::eUnsigned:
      default:
        break;
      }

      if (!skipFormatting) {
        /// remove leading zeros
        format_string.erase(0, format_string.find_first_not_of('0'));

        /// check if value is zero then nothing needs to be done furthur
        if (format_string.length() == 0)
          format_string = '0';
        /// scale the value, add trailing/leading zeros if needed with decimal
        /// point
        else {
          if (type_scale > 0) {
            format_string.append(std::string('0', type_scale));
          } else if (type_scale < 0) {
            int pos = format_string.length() + type_scale;
            if (pos > 0) {
              format_string.insert(pos, 1, '.');
            } else {
              format_string.insert(0, (-pos + 1), '0');
              format_string.insert(1, 1, '.');
            }
          }
          if (type_sign != LegacyType::eUnsigned)
            format_string.insert(0, 1, sign);
        }
      }
      format_data.SetData((const void *)format_string.c_str(),
                          format_string.length(), endian::InlHostByteOrder());
      format_data.SetAddressByteSize(data.GetAddressByteSize());

      return DumpDataExtractor(format_data, &s, byte_offset, format, 1,
                               format_string.length(), UINT32_MAX,
                               LLDB_INVALID_ADDRESS, bitfield_bit_size,
                               bitfield_bit_offset, exe_scope);
    }
    }
  }
  case LegacyType::KIND_PTR:
    return DumpDataExtractor(data, &s, byte_offset, format, byte_size, 1,
                             UINT32_MAX, LLDB_INVALID_ADDRESS,
                             bitfield_bit_size, bitfield_bit_offset, exe_scope);
  case LegacyType::KIND_DYNAMIC: {
    auto dyn_base_ty = static_cast<LegacyDynamic *>(type)->GetBaseType();
    return dyn_base_ty.DumpTypeValue(&s, format, data, byte_offset, byte_size,
                                     bitfield_bit_size, bitfield_bit_offset,
                                     exe_scope);
  }
  case LegacyType::KIND_L88: {
    return DumpDataExtractor(data, &s, byte_offset, eFormatCharPrintable,
                             byte_size, data.GetDataEnd() - data.GetDataStart(),
                             UINT32_MAX, LLDB_INVALID_ADDRESS,
                             bitfield_bit_size, bitfield_bit_offset, exe_scope);
  }
  }
  Host::SystemLog(lldb::eSeverityError,
                  "Error: DumpTypeValue not handled yet.\n");
  return false;
}

void TypeSystemLegacy::DumpTypeDescription(opaque_compiler_type_t type,
                                           DescriptionLevel level) {
  // Dump to stdout
  StreamFile s(stdout, false);
  DumpTypeDescription(type, s, level);
}

void TypeSystemLegacy::DumpTypeDescription(opaque_compiler_type_t type,
                                           Stream &s, DescriptionLevel level) {
  if (!type)
    return;

  ConstString name = GetTypeName(type, true);
  s.PutCString(name.AsCString());
}

UserExpression *TypeSystemLegacy::GetUserExpression(
    StringRef expr, StringRef prefix, SourceLanguage language,
    Expression::ResultType desired_type,
    const EvaluateExpressionOptions &options, ValueObject *ctx_obj) {
  TargetSP target = m_target_wp.lock();
  if (target) {
    Log *log = GetLog(LLDBLog::Expressions);

    LLDB_LOGF(log, "LegacyTypeSystem: UserExpression %s for %s.\n",
              expr.str().c_str(), language.GetDescription().data());

    switch (language) {
    default:
      char buffer[64];
      sprintf(
          buffer,
          "LegacyTypeSystem: UserExpression for language %s not supported.\n",
          language.GetDescription().data());
      Host::SystemLog(lldb::eSeverityError, std::string(buffer));
      break;
    case eLanguageTypeCobol74:
    case eLanguageTypeCobol85:
      return new CobolUserExpression(*target, expr, prefix, language,
                                     desired_type, options);
    case eLanguageTypePLI:
      return new PLIUserExpression(*target, expr, prefix, language,
                                   desired_type, options);
    }
  }

  Host::SystemLog(lldb::eSeverityError,
                  "LegacyTypeSystem: UserExpression error.\n");
  return nullptr;
}

PersistentExpressionState *TypeSystemLegacy::GetPersistentExpressionState() {
  return nullptr;
}
