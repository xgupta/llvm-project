//===-- TypeSystemLegacy.h ------------------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef liblldb_TypeSystemLegacy_h_
#define liblldb_TypeSystemLegacy_h_

// C Includes
// C++ Includes
#include <map>
#include <memory>
#include <set>
#include <string>
#include <vector>

#if defined(__linux__)
#include <iconv.h>
#define USE_BUILTIN_ICONV
#endif

// Other libraries and framework includes
// Project includes
#include "lldb/Symbol/CompilerType.h"
#include "lldb/Symbol/TypeSystem.h"
#include "lldb/Target/Target.h"
#include "lldb/Utility/ConstString.h"

namespace lldb_private {

class LegacyType;

class TypeSystemLegacy : public TypeSystem {
  // LLVM RTTI support
  static char ID;

public:
  // llvm casting support
  bool isA(const void *ClassID) const override { return ClassID == &ID; }
  static bool classof(const TypeSystem *ts) { return ts->isA(&ID); }

  //------------------------------------------------------------------
  // Constructors and Destructors
  //------------------------------------------------------------------
  TypeSystemLegacy(lldb::TargetSP target = nullptr);

  ~TypeSystemLegacy() override;

  void Finalize() override;

  //------------------------------------------------------------------
  // PluginInterface functions
  //------------------------------------------------------------------
  ConstString GetPluginName() override;

  static ConstString GetPluginNameStatic();

  uint32_t GetPluginVersion() override;

  DWARFASTParser *GetDWARFParser() override;

  static lldb::TypeSystemSP CreateInstance(lldb::LanguageType language,
                                           Module *module, Target *target);

  static LanguageSet GetSupportedLanguagesForTypes();

  static LanguageSet GetSupportedLanguagesForExpressions();

  static void Initialize();

  static void Terminate();

  void SetAddressByteSize(int byte_size) { m_pointer_byte_size = byte_size; }

  //----------------------------------------------------------------------
  // CompilerDecl functions
  //----------------------------------------------------------------------
  ConstString DeclGetName(void *opaque_decl) override { return ConstString(); };

  CompilerType GetTypeForDecl(void *opaque_decl) override {
    return CompilerType();
  }

  //----------------------------------------------------------------------
  // CompilerDeclContext functions
  //----------------------------------------------------------------------

  ConstString DeclContextGetName(void *opaque_decl_ctx) override {
    return ConstString();
  }

  ConstString DeclContextGetScopeQualifiedName(void *opaque_decl_ctx) override {
    return ConstString();
  }

  bool
  DeclContextIsClassMethod(void *opaque_decl_ctx,
                           lldb::LanguageType *language_ptr,
                           bool *is_instance_method_ptr,
                           ConstString *language_object_name_ptr) override {
    return false;
  }

  bool DeclContextIsContainedInLookup(void *opaque_decl_ctx,
                                      void *other_opaque_decl_ctx) override {
    return false;
  }

  //----------------------------------------------------------------------
  // Tests
  //----------------------------------------------------------------------
#ifndef NDEBUG
  bool Verify(lldb::opaque_compiler_type_t type) override;
#endif

  bool IsArrayType(lldb::opaque_compiler_type_t type,
                   CompilerType *element_type, uint64_t *size,
                   bool *is_incomplete) override;

  bool IsAggregateType(lldb::opaque_compiler_type_t type) override;

  bool IsAnonymousType(lldb::opaque_compiler_type_t type) override {
    return false;
  }

  bool IsCharType(lldb::opaque_compiler_type_t type) override;

  bool IsCompleteType(lldb::opaque_compiler_type_t type) override {
    return true;
  }

  bool IsDefined(lldb::opaque_compiler_type_t type) override {
    return type != nullptr;
  }

  bool IsFloatingPointType(lldb::opaque_compiler_type_t type, uint32_t &count,
                           bool &is_complex) override;

  bool IsFunctionType(lldb::opaque_compiler_type_t type,
                      bool *is_variadic_ptr = nullptr) override;

  size_t
  GetNumberOfFunctionArguments(lldb::opaque_compiler_type_t type) override {
    return 0;
  }

  CompilerType GetFunctionArgumentAtIndex(lldb::opaque_compiler_type_t type,
                                          const size_t index) override {
    return CompilerType();
  }

  bool IsFunctionPointerType(lldb::opaque_compiler_type_t type) override {
    return IsFunctionType(type);
  }

  bool IsBlockPointerType(lldb::opaque_compiler_type_t type,
                          CompilerType *function_pointer_type_ptr) override {
    return false;
  }

  bool IsIntegerType(lldb::opaque_compiler_type_t type,
                     bool &is_signed) override;

  bool IsEnumerationType(lldb::opaque_compiler_type_t type,
                         bool &is_signed) override {
    return false;
  }

  bool IsPossibleDynamicType(lldb::opaque_compiler_type_t type,
                             CompilerType *target_type, // Can pass NULL
                             bool check_cplusplus, bool check_objc) override {
    return false;
  }

  bool IsPointerType(lldb::opaque_compiler_type_t type,
                     CompilerType *pointee_type = nullptr) override;

  bool IsScalarType(lldb::opaque_compiler_type_t type) override {
    return !IsAggregateType(type);
  }

  bool IsVoidType(lldb::opaque_compiler_type_t type) override { return false; }

  bool CanPassInRegisters(const CompilerType &type) override { return false; }

  // TypeSystems can support more than one language
  bool SupportsLanguage(lldb::LanguageType language) override;

  //----------------------------------------------------------------------
  // Type Completion
  //----------------------------------------------------------------------

  bool GetCompleteType(lldb::opaque_compiler_type_t type) override;

  //----------------------------------------------------------------------
  // AST related queries
  //----------------------------------------------------------------------

  uint32_t GetPointerByteSize() override { return m_pointer_byte_size; }

  //----------------------------------------------------------------------
  // Accessors
  //----------------------------------------------------------------------

  ConstString GetTypeName(lldb::opaque_compiler_type_t type) override;

  ConstString GetDisplayTypeName(lldb::opaque_compiler_type_t type) override {
    return GetTypeName(type);
  }

  uint32_t GetTypeInfo(lldb::opaque_compiler_type_t type,
                       CompilerType *pointee_or_element_compiler_type) override;

  lldb::LanguageType
  GetMinimumLanguage(lldb::opaque_compiler_type_t type) override;

  lldb::TypeClass GetTypeClass(lldb::opaque_compiler_type_t type) override;

  //----------------------------------------------------------------------
  // Creating related types
  //----------------------------------------------------------------------

  CompilerType GetArrayElementType(lldb::opaque_compiler_type_t type,
                                   uint64_t *stride,
                                   ExecutionContextScope *exe_scope) override;

  // Returns -1 if this isn't a function of if the function doesn't have a
  // prototype Returns a value >= 0 if there is a prototype.
  int GetFunctionArgumentCount(lldb::opaque_compiler_type_t type) override;

  CompilerType GetFunctionArgumentTypeAtIndex(lldb::opaque_compiler_type_t type,
                                              size_t idx) override {
    assert(0);
    return CompilerType();
  }

  CompilerType
  GetFunctionReturnType(lldb::opaque_compiler_type_t type) override;

  size_t GetNumMemberFunctions(lldb::opaque_compiler_type_t type) override {
    return 0;
  }

  CompilerType GetPointeeType(lldb::opaque_compiler_type_t type) override;

  CompilerType GetPointerType(lldb::opaque_compiler_type_t type) override;

  CompilerType
  GetLValueReferenceType(lldb::opaque_compiler_type_t type) override;

  CompilerType
  GetRValueReferenceType(lldb::opaque_compiler_type_t type) override {
    assert(0);
    return CompilerType();
  }

  bool VerifyEncodeType(lldb::opaque_compiler_type_t src_type,
                        lldb::opaque_compiler_type_t dst_type);

  bool EncodeDataToType(
      ExecutionContext &exe_scope, lldb::opaque_compiler_type_t src_type,
      const DataExtractor &src_data, lldb::opaque_compiler_type_t dest_type,
      DataExtractor &dest_data,
      const lldb::LanguageType lang = lldb::eLanguageTypeUnknown);
  /*
    CompilerType AddConstModifier(lldb::opaque_compiler_type_t type) override;

    CompilerType AddVolatileModifier(lldb::opaque_compiler_type_t type)
    override;

    CompilerType AddRestrictModifier(lldb::opaque_compiler_type_t type)
    override;

    CompilerType CreateTypedef(lldb::opaque_compiler_type_t type,
                               const char *name,
                               const CompilerDeclContext &decl_ctx) override;
  */
  //----------------------------------------------------------------------
  // Exploring the type
  //----------------------------------------------------------------------
  const llvm::fltSemantics &GetFloatTypeSemantics(size_t byte_size) override {
    return llvm::APFloatBase::Bogus();
  }

  llvm::Optional<uint64_t> GetByteSize(lldb::opaque_compiler_type_t type,
                                       ExecutionContextScope *exe_scope) {
    if (llvm::Optional<uint64_t> bit_size = GetBitSize(type, exe_scope))
      return (*bit_size + 7) / 8;
    return llvm::None;
  }

  llvm::Optional<uint64_t>
  GetBitSize(lldb::opaque_compiler_type_t type,
             ExecutionContextScope *exe_scope) override;

  lldb::Encoding GetEncoding(lldb::opaque_compiler_type_t type,
                             uint64_t &count) override;

  lldb::Format GetFormat(lldb::opaque_compiler_type_t type) override;

  uint32_t GetNumChildren(lldb::opaque_compiler_type_t type,
                          bool omit_empty_base_classes,
                          const ExecutionContext *exe_ctx) override;

  lldb::BasicType
  GetBasicTypeEnumeration(lldb::opaque_compiler_type_t type) override;

  uint32_t GetNumFields(lldb::opaque_compiler_type_t type) override {
    // TODO struct members calculation
    return 0;
  }

  CompilerType GetFieldAtIndex(lldb::opaque_compiler_type_t type, size_t idx,
                               std::string &name, uint64_t *bit_offset_ptr,
                               uint32_t *bitfield_bit_size_ptr,
                               bool *is_bitfield_ptr) override;

  uint32_t GetNumDirectBaseClasses(lldb::opaque_compiler_type_t type) override {
    return 0;
  }

  uint32_t
  GetNumVirtualBaseClasses(lldb::opaque_compiler_type_t type) override {
    return 0;
  }

  CompilerType GetDirectBaseClassAtIndex(lldb::opaque_compiler_type_t type,
                                         size_t idx,
                                         uint32_t *bit_offset_ptr) override {
    return CompilerType();
  }

  CompilerType GetVirtualBaseClassAtIndex(lldb::opaque_compiler_type_t type,
                                          size_t idx,
                                          uint32_t *bit_offset_ptr) override {
    return CompilerType();
  }

  TypeMemberFunctionImpl
  GetMemberFunctionAtIndex(lldb::opaque_compiler_type_t type,
                           size_t idx) override;

  CompilerType GetCanonicalType(lldb::opaque_compiler_type_t type) override;

  CompilerType GetChildCompilerTypeAtIndex(
      lldb::opaque_compiler_type_t type, ExecutionContext *exe_ctx, size_t idx,
      bool transparent_pointers, bool omit_empty_base_classes,
      bool ignore_array_bounds, std::string &child_name,
      uint32_t &child_byte_size, int32_t &child_byte_offset,
      uint32_t &child_bitfield_bit_size, uint32_t &child_bitfield_bit_offset,
      bool &child_is_base_class, bool &child_is_deref_of_parent,
      ValueObject *valobj, uint64_t &language_flags) override;

  // Lookup a child given a name. This function will match base class names and
  // member member names in "clang_type" only, not descendants.
  uint32_t GetIndexOfChildWithName(lldb::opaque_compiler_type_t type,
                                   const char *name,
                                   bool omit_empty_base_classes) override;

  // Lookup a child member given a name. This function will match member names
  // only and will descend into "clang_type" children in search for the first
  // member in this class, or any base class that matches "name".
  // TODO: Return all matches for a given name by returning a
  // vector<vector<uint32_t>>
  // so we catch all names that match a given child name, not just the first.
  size_t
  GetIndexOfChildMemberWithName(lldb::opaque_compiler_type_t type,
                                const char *name, bool omit_empty_base_classes,
                                std::vector<uint32_t> &child_indexes) override;

  //----------------------------------------------------------------------
  // Creating Types
  //----------------------------------------------------------------------

  CompilerType CreateArrayType(const ConstString &array_type_name,
                               const ConstString &name,
                               const CompilerType &element_type,
                               size_t element_count, bool isVarString);

  CompilerType GetArrayType(lldb::opaque_compiler_type_t type,
                            uint64_t size) override;

  CompilerType CreateStructType(const ConstString &name, uint32_t byte_size);

  void AddFieldToStruct(const CompilerType &struct_type,
                        const ConstString &member_name,
                        const CompilerType &member_type, uint32_t byte_offset);

  void CompleteStructType(const CompilerType &struct_type);

  CompilerType CreateFunctionType(const ConstString &name, CompilerType *params,
                                  size_t params_count, bool is_variadic);

  CompilerType CreateBaseType(const ConstString &name,
                              const ConstString &pic_string, uint32_t dw_ate,
                              uint32_t bitsize, uint32_t sign, int32_t scale,
                              uint32_t digit_count, lldb::ByteOrder byte_order,
                              bool bin_scale);

  //----------------------------------------------------------------------
  // Dumping types
  //----------------------------------------------------------------------
#ifndef NDEBUG
  /// Convenience LLVM-style dump method for use in the debugger only.
  /// In contrast to the other \p Dump() methods this directly invokes
  /// \p clang::QualType::dump().
  LLVM_DUMP_METHOD void dump(lldb::opaque_compiler_type_t type) const override;
#endif

  void DumpValue(lldb::opaque_compiler_type_t type, ExecutionContext *exe_ctx,
                 Stream *s, lldb::Format format, const DataExtractor &data,
                 lldb::offset_t data_offset, size_t data_byte_size,
                 uint32_t bitfield_bit_size, uint32_t bitfield_bit_offset,
                 bool show_types, bool show_summary, bool verbose,
                 uint32_t depth) override;

  bool DumpTypeValue(lldb::opaque_compiler_type_t type, Stream *s,
                     lldb::Format format, const DataExtractor &data,
                     lldb::offset_t data_offset, size_t data_byte_size,
                     uint32_t bitfield_bit_size, uint32_t bitfield_bit_offset,
                     ExecutionContextScope *exe_scope) override;

  void DumpTypeDescription(
      lldb::opaque_compiler_type_t type,
      lldb::DescriptionLevel level =
          lldb::eDescriptionLevelFull) override; // Dump to stdout

  void DumpTypeDescription(
      lldb::opaque_compiler_type_t type, Stream *s,
      lldb::DescriptionLevel level = lldb::eDescriptionLevelFull) override;

  //----------------------------------------------------------------------
  // TODO: These methods appear unused. Should they be removed?
  //----------------------------------------------------------------------

  bool IsRuntimeGeneratedType(lldb::opaque_compiler_type_t type) override {
    return false;
  }

  void DumpSummary(lldb::opaque_compiler_type_t type, ExecutionContext *exe_ctx,
                   Stream *s, const DataExtractor &data,
                   lldb::offset_t data_offset, size_t data_byte_size) override;

  //----------------------------------------------------------------------
  // TODO: Determine if these methods should move to ClangASTContext.
  //----------------------------------------------------------------------

  bool IsPointerOrReferenceType(lldb::opaque_compiler_type_t type,
                                CompilerType *pointee_type) override;

  unsigned GetTypeQualifiers(lldb::opaque_compiler_type_t type) override;

  bool IsCStringType(lldb::opaque_compiler_type_t type,
                     uint32_t &length) override;

  llvm::Optional<size_t>
  GetTypeBitAlign(lldb::opaque_compiler_type_t type,
                  ExecutionContextScope *exe_scope) override;

  CompilerType GetBasicTypeFromAST(lldb::BasicType basic_type) override;

  CompilerType GetBuiltinTypeForEncodingAndBitSize(lldb::Encoding encoding,
                                                   size_t bit_size) override {
    return CompilerType();
  }

  bool IsBeingDefined(lldb::opaque_compiler_type_t type) override;

  bool IsConst(lldb::opaque_compiler_type_t type) override;

  uint32_t IsHomogeneousAggregate(lldb::opaque_compiler_type_t type,
                                  CompilerType *base_type_ptr) override;

  bool IsPolymorphicClass(lldb::opaque_compiler_type_t type) override;

  bool IsTypedefType(lldb::opaque_compiler_type_t type) override;

  // If the current object represents a typedef type, get the underlying type
  CompilerType GetTypedefedType(lldb::opaque_compiler_type_t type) override;

  bool IsVectorType(lldb::opaque_compiler_type_t type,
                    CompilerType *element_type, uint64_t *size) override;

  CompilerType
  GetFullyUnqualifiedType(lldb::opaque_compiler_type_t type) override;

  CompilerType GetNonReferenceType(lldb::opaque_compiler_type_t type) override;

  bool IsReferenceType(lldb::opaque_compiler_type_t type,
                       CompilerType *pointee_type, bool *is_rvalue) override;
  CompilerType MutateBaseTypeSize(lldb::opaque_compiler_type_t type,
                                  uint64_t sizeInBits) override;

  UserExpression *GetUserExpression(llvm::StringRef expr,
                                    llvm::StringRef prefix,
                                    lldb::LanguageType language,
                                    Expression::ResultType desired_type,
                                    const EvaluateExpressionOptions &options,
                                    ValueObject *ctx_obj) override;

  FunctionCaller *GetFunctionCaller(const CompilerType &return_type,
                                    const Address &function_address,
                                    const ValueList &arg_value_list,
                                    const char *name) override {
    assert(0);
    return nullptr;
  }

  UtilityFunction *GetUtilityFunction(const char *text,
                                      const char *name) override;

  PersistentExpressionState *GetPersistentExpressionState() override;

  lldb::LanguageType GetLanguage() const { return m_lang; }
  void SetLanguage(lldb::LanguageType lang) { m_lang = lang; }

private:
  typedef std::map<LegacyType *, std::unique_ptr<LegacyType>> TypeMap;
  typedef std::map<lldb::BasicType, std::unique_ptr<LegacyType>> BasicTypeMap;
  typedef std::map<ConstString, std::unique_ptr<LegacyType>> MutatedTypeMap;
  int m_pointer_byte_size;
  std::unique_ptr<DWARFASTParser> m_dwarf_ast_parser_ap;
  lldb::TargetWP m_target_wp;
  std::unique_ptr<TypeMap> m_types;
  std::unique_ptr<BasicTypeMap> m_basic_types;
  std::unique_ptr<MutatedTypeMap> m_mutated_types;
  lldb::LanguageType m_lang = lldb::eLanguageTypeUnknown;

  TypeSystemLegacy(const TypeSystemLegacy &) = delete;
  const TypeSystemLegacy &operator=(const TypeSystemLegacy &) = delete;
};

/// TODO cleanup, seperate iconv
class TargetCharsetReader {
public:
  TargetCharsetReader(lldb::TargetSP target_sp, bool encode = false)
      : fromcode("") {
#if defined(USE_BUILTIN_ICONV)
    icd = (iconv_t)-1;
    if (target_sp) {
      fromcode.assign(target_sp->GetTargetCharset().str());
      icd = encode ? iconv_open(fromcode.c_str(), "")
                   : iconv_open("", fromcode.c_str());
    }
#endif
  }

  ~TargetCharsetReader() {
#if defined(USE_BUILTIN_ICONV)
    iconv_close(icd);
#endif
  }

  bool convert(std::string &inp) {
    if (!IsValid())
      return false;

    bool Converted = false;
#if defined(USE_BUILTIN_ICONV)
    size_t in_size = inp.length();
    char *src_ptr = const_cast<char *>(inp.c_str());
    std::vector<char> buffer(buffer_size);
    std::string dst;
    while (in_size) {
      char *dst_ptr = &buffer[0];
      size_t out_size = buffer_size;
      size_t result = iconv(icd, &src_ptr, &in_size, &dst_ptr, &out_size);
      if (result == (size_t)-1)
        return false;

      size_t out_len = buffer_size - out_size;
      if (out_len) {
        dst.append(&buffer[0], out_len);
        Converted |= true;
      }
    }
    if (Converted) {
      inp.erase();
      inp = dst;
    }
#endif
    return Converted;
  }

  bool convert(char *inp, size_t length) {
    if (!IsValid())
      return false;
#if defined(USE_BUILTIN_ICONV)
    size_t in_size = length;
    char *src_ptr = inp;
    std::vector<char> buffer(buffer_size);
    while (in_size) {
      char *dst_ptr = &buffer[0];
      size_t out_size = buffer_size;
      size_t result = iconv(icd, &src_ptr, &in_size, &dst_ptr, &out_size);
      if (result == (size_t)-1)
        return false;
      size_t out_len = buffer_size - out_size;
      if (out_len) {
        memcpy(inp, &buffer[0], out_len);
        return true;
      }
    }
#endif
    return false;
  }

  bool IsValid() const {
#if defined(USE_BUILTIN_ICONV)
    return icd != (iconv_t)-1;
#else
    return false;
#endif
  }

  ConstString getTargetFormat() const { return ConstString(fromcode); }

private:
  TargetCharsetReader(const TargetCharsetReader &) = delete;
  const TargetCharsetReader &operator=(const TargetCharsetReader &) = delete;

  static const size_t buffer_size = 1024;
  std::string fromcode;
#if defined(USE_BUILTIN_ICONV)
  iconv_t icd;
#endif
};

} // namespace lldb_private

#endif // liblldb_TypeSystemLegacy_h_
