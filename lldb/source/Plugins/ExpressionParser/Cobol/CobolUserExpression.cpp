//===-- CobolUserExpression.cpp -------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "llvm/ADT/StringRef.h"

#include "CobolUserExpression.h"

#include "Plugins/TypeSystem/Legacy/TypeSystemLegacy.h"
#include "lldb/Core/Module.h"
#include "lldb/Expression/DiagnosticManager.h"
#include "lldb/Expression/ExpressionVariable.h"
#include "lldb/Host/StreamFile.h"
#include "lldb/Symbol/SymbolFile.h"
#include "lldb/Symbol/TypeList.h"
#include "lldb/Symbol/VariableList.h"
#include "lldb/Target/Process.h"
#include "lldb/Target/StackFrame.h"
#include "lldb/Target/Target.h"
#include "lldb/Utility/DataBufferHeap.h"
#include "lldb/Utility/DataEncoder.h"
#include "lldb/Utility/DataExtractor.h"
#include "lldb/Utility/LLDBLog.h"
#include "lldb/Utility/Log.h"
#include "lldb/ValueObject/ValueObjectConstResult.h"
#include "lldb/ValueObject/ValueObjectRegister.h"

#include "llvm/Support/Casting.h"

using namespace lldb_private;
using namespace lldb;

char CobolUserExpression::ID;

static bool SearchCompilerTypeForMemberWithName(CompilerType *comp_type,
                                                llvm::Twine name) {

  if (!comp_type)
    return false;

  CompilerType elem_or_pointee_compiler_type;
  const Flags type_flags(
      comp_type->GetTypeInfo(&elem_or_pointee_compiler_type));
  if (!type_flags.Test(eTypeIsStructUnion))
    return false;

  std::vector<uint32_t> child_indexes;
  const size_t index = comp_type->GetIndexOfChildMemberWithName(
      name.str().c_str(), true, child_indexes);
  if (index != 0)
    return true;

  // const llvm::Expected<uint32_t> total_count =
  // comp_type->GetNumChildren(true, nullptr);
  uint32_t total_count = comp_type->GetNumChildren(true, nullptr).get();
  for (uint32_t i = 0; i < total_count; ++i) {
    std::string child_name;
    uint32_t child_byte_size;
    int32_t child_byte_offset = 0;
    uint32_t child_bitfield_bit_size;
    uint32_t child_bitfield_bit_offset;
    bool child_is_base_class;
    bool child_is_deref_of_parent;
    uint64_t language_flags;

    llvm::Expected<CompilerType> expected_child_type =
        comp_type->GetChildCompilerTypeAtIndex(
            nullptr, i, true, true, true, child_name, child_byte_size,
            child_byte_offset, child_bitfield_bit_size,
            child_bitfield_bit_offset, child_is_base_class,
            child_is_deref_of_parent, nullptr, language_flags);

    if (!expected_child_type) {
      // Handle the error, e.g., by returning or logging it.
      llvm::consumeError(expected_child_type.takeError());
      return false; // or handle accordingly
    }
    CompilerType child_type =
        *expected_child_type; // Extract the actual value safely

    if (SearchCompilerTypeForMemberWithName(&child_type, name))
      return true;
  }
  return false;
}

/// TODO: improve performance
static VariableSP SearchMemberByName(TargetSP target, llvm::Twine name) {
  VariableList variable_list;
  if (!target) {
    return nullptr;
  }
  target->GetImages().FindGlobalVariables(
      RegularExpression(llvm::StringRef("^.*$")), 100, variable_list);

  for (size_t i = 0; i < variable_list.GetSize(); ++i) {
    VariableSP result = variable_list.GetVariableAtIndex(i);
    if (!result)
      continue;

    lldb::LanguageType lang = result->GetLanguage();
    if (!((lang == eLanguageTypeCobol85) || (lang == eLanguageTypeCobol74)))
      variable_list.RemoveVariableAtIndex(i);

    if (!result->GetType()->IsAggregateType())
      variable_list.RemoveVariableAtIndex(i);
  }

  for (size_t i = 0; i < variable_list.GetSize(); ++i) {
    VariableSP result = variable_list.GetVariableAtIndex(i);
    auto comp_type = result->GetType()->GetForwardCompilerType();

    if (SearchCompilerTypeForMemberWithName(&comp_type, name))
      return result;
  }
  return nullptr;
}

static VariableSP FindGlobalVariable(TargetSP target, llvm::Twine name,
                                     bool &isMember) {
  VariableList variable_list;
  if (!target) {
    return nullptr;
  }
  target->GetImages().FindGlobalVariables(
      RegularExpression(llvm::StringRef("^" + name.str() + "$"),
                        llvm::Regex::NoFlags, true /* case-insensitive */),
      1, variable_list);

  const auto match_count = variable_list.GetSize();
  for (uint32_t i = 0; i < match_count; ++i) {
    VariableSP result = variable_list.GetVariableAtIndex(i);
    if (!result)
      continue;

    lldb::LanguageType lang = result->GetLanguage();
    if ((lang == eLanguageTypeCobol85) || (lang == eLanguageTypeCobol74))
      return result;
  }

  if (match_count > 0)
    return variable_list.GetVariableAtIndex(0);

  isMember = true;
  return SearchMemberByName(target, name);
}

char CobolPersistentExpressionState::ID = 0;
CobolPersistentExpressionState::CobolPersistentExpressionState() {}

void CobolPersistentExpressionState::RemovePersistentVariable(
    lldb::ExpressionVariableSP var) {
  RemoveVariable(var);
}

ConstString
CobolPersistentExpressionState::GetNextPersistentVariableName(bool is_error) {
  llvm::SmallString<64> name;
  {
    llvm::raw_svector_ostream os(name);
    os << GetPersistentVariablePrefix(is_error)
       << m_next_persistent_variable_id++;
  }
  return ConstString(name);
}

CobolInterpreter::CobolInterpreter(ExecutionContext &exe_ctx, const char *expr)
    : m_exe_ctx(exe_ctx), m_frame(exe_ctx.GetFrameSP()), m_parser(expr) {}

bool CobolInterpreter::Parse() {
  for (std::unique_ptr<CobolASTStmt> stmt(m_parser.Statement()); stmt;
       stmt.reset(m_parser.Statement())) {
    if (m_parser.Failed())
      break;
    m_statements.emplace_back(std::move(stmt));
  }

  if (m_parser.Failed() || !m_parser.AtEOF())
    m_parser.GetError(m_error);

  return m_error.Success();
}

lldb::ValueObjectSP CobolInterpreter::Evaluate(ExecutionContext &exe_ctx) {
  m_exe_ctx = exe_ctx;
  ValueObjectSP result;
  for (auto &stmt : m_statements) {
    result = EvaluateStatement(stmt.get());
    if (m_error.Fail())
      return nullptr;
  }
  return result;
}

lldb::ValueObjectSP
CobolInterpreter::EvaluateStatement(const lldb_private::CobolASTStmt *stmt) {
  // Handle other
  switch (stmt->GetKind()) {
  default:
    m_error = Status::FromErrorStringWithFormat("%s node not supported",
                                                stmt->GetKindName());
    break;
  case CobolASTNode::eExprStmt:
    const CobolASTExprStmt *expr = llvm::cast<CobolASTExprStmt>(stmt);
    return EvaluateExpr(expr->GetExpr());
  }
  return nullptr;
}

lldb::ValueObjectSP CobolInterpreter::EvaluateExpr(const CobolASTExpr *expr) {
  if (expr)
    return expr->Visit<ValueObjectSP>(this);
  return ValueObjectSP();
}

lldb::ValueObjectSP CobolInterpreter::VisitIdent(const CobolASTIdent *ident) {
  ValueObjectSP result;
  llvm::StringRef var_name = ident->GetName().m_text;
  if (m_frame) {
    VariableSP var_sp;
    if (var_name[0] == '$') {
      m_error.Clear();
      m_error =
          Status::FromErrorString("Consistent var lookup not implemented yet");
      return nullptr;
    }

    VariableListSP var_list_sp(m_frame->GetInScopeVariableList(true));
    VariableList *var_list = var_list_sp.get();
    if (var_list) {
      var_sp = var_list->FindVariable(ConstString(var_name),
                                      /* include_static_members */ true,
                                      /* case_sensitive */ false);
      if (var_sp) {
        result = m_frame->GetValueObjectForFrameVariable(var_sp, m_use_dynamic);
      }
    }

    if (!result && var_list) {
      for (size_t i = 0; i < var_list->GetSize(); i++) {
        VariableSP variable_sp = var_list->GetVariableAtIndex(i);
        if (!variable_sp)
          continue;

        Type *var_type = variable_sp->GetType();
        if (!var_type)
          continue;

        ValueObjectSP valobj_sp =
            m_frame->GetValueObjectForFrameVariable(variable_sp, m_use_dynamic);
        if (!valobj_sp)
          return valobj_sp;

        valobj_sp = m_frame->GetValueObjectForFrameAggregateVariable(
            ConstString(var_name), valobj_sp, m_use_dynamic);
        if (valobj_sp) {
          result = valobj_sp;
          break;
        }
      }
    }

    // search global if available
    if (!result) {
      TargetSP target = m_frame->CalculateTarget();
      if (!target) {
        m_error.Clear();
        m_error = Status::FromErrorString("No target");
        return nullptr;
      }

      bool isMember = false;
      var_sp = FindGlobalVariable(target, var_name, isMember);
      if (var_sp) {
        ValueObjectSP valobj_sp =
            m_frame->TrackGlobalVariable(var_sp, m_use_dynamic);
        if (isMember)
          valobj_sp = m_frame->GetValueObjectForFrameAggregateVariable(
              ConstString(var_name), valobj_sp, m_use_dynamic);
        return valobj_sp;
      }
    }
  }
  if (!result)
    m_error = Status::FromErrorStringWithFormat("Unknown variable %s",
                                                var_name.str().c_str());
  return result;
}

ValueObjectSP CobolInterpreter::VisitUnaryExpr(const CobolASTUnaryExpr *expr) {
  ValueObjectSP var = EvaluateExpr(expr->GetExpr());
  if (!var)
    return nullptr;

  switch (expr->GetOp()) {
  default:
    return nullptr;
  case CobolLexer::OP_AMP:
    CompilerType comp_type = var->GetCompilerType().GetPointerType();
    uint64_t address = var->GetAddressOf();

    ByteOrder byte_order = m_exe_ctx.GetByteOrder();
    DataBufferSP buffer(new DataBufferHeap(&address, sizeof(address)));
    DataExtractor data(buffer, byte_order, sizeof(address));
    return ValueObject::CreateValueObjectFromData(llvm::StringRef(), data,
                                                  m_exe_ctx, comp_type);
  }
}

ValueObjectSP CobolInterpreter::VisitBasicLit(const CobolASTBasicLit *expr) {
  llvm::StringRef value_string = expr->GetValue().m_text;

  lldb::BasicType base_type = eBasicTypeInvalid;
  size_t data_length = 0;
  int64_t iValue;
  double dValue;
  bool isArray = false;
  size_t array_length = 0;
  const void *data_ptr = nullptr;
  switch (expr->GetValue().m_type) {
  default:
    m_error = Status::FromErrorStringWithFormat("Non-Const lexical type for %s",
                                                value_string.str().c_str());
    return nullptr;
  case CobolLexer::LIT_INTEGER:
    if (value_string.front() == '+')
      value_string = value_string.drop_front(1);
    if (value_string.getAsInteger(0, iValue)) {
      m_error = Status::FromErrorStringWithFormat("integer conversion error %s",
                                                  value_string.str().c_str());
      return nullptr;
    }
    data_length = sizeof(iValue);
    base_type = eBasicTypeInt;
    data_ptr = &iValue;
    break;
  case CobolLexer::LIT_FLOAT:
    if (value_string.getAsDouble(dValue)) {
      m_error = Status::FromErrorStringWithFormat("double conversion error %s",
                                                  value_string.str().c_str());
      return nullptr;
    }
    data_length = sizeof(dValue);
    base_type = eBasicTypeDouble;
    data_ptr = &dValue;
    break;
  case CobolLexer::LIT_STRING:
    base_type = eBasicTypeChar;
    isArray = true;
    data_ptr = value_string.data();
    array_length = value_string.size();
    data_length = array_length;
    break;
  }

  TargetSP target = m_exe_ctx.GetTargetSP();
  if (!target)
    return nullptr;

  auto type_sys = target->GetScratchTypeSystemForLanguage(eLanguageTypeCobol85);
  if (!type_sys)
    return nullptr;

  TypeSystemLegacy *legacy_type_system =
      llvm::dyn_cast_or_null<TypeSystemLegacy>(type_sys->get());
  if (!legacy_type_system)
    return nullptr;

  ByteOrder byte_order = endian::InlHostByteOrder();
  uint8_t addr_size = target->GetArchitecture().GetAddressByteSize();

  DataBufferSP buffer(new DataBufferHeap(data_length, 0));
  DataEncoder enc(buffer->GetBytes(), data_length, byte_order, addr_size);
  enc.PutData(0, data_ptr, data_length);
  DataExtractor data(enc.GetDataBuffer(), byte_order, addr_size);

  CompilerType comp_type = legacy_type_system->GetBasicTypeFromAST(base_type);
  if (isArray) {
    static ConstString array_name("char []");
    static ConstString empty_name;
    comp_type = legacy_type_system->CreateArrayType(
        array_name, empty_name, comp_type, array_length, false /*isVarString*/);
  }
  return ValueObject::CreateValueObjectFromData(llvm::StringRef(), data,
                                                m_exe_ctx, comp_type);
}

ValueObjectSP
CobolInterpreter::VisitSelectorExpr(const CobolASTSelectorExpr *expr) {
  ValueObjectSP target = EvaluateExpr(expr->GetExpr());
  if (target) {
    if (target->GetCompilerType().IsPointerType()) {
      target = target->Dereference(m_error);
      if (m_error.Fail())
        return nullptr;
    }
    ConstString field(expr->GetSel()->GetName().m_text);
    ValueObjectSP result = target->GetChildMemberWithName(field, true);
    if (!result)
      m_error = Status::FromErrorStringWithFormat("Unknown child %s",
                                                  field.AsCString());
    return result;
  }
  if (const CobolASTIdent *package =
          llvm::dyn_cast<CobolASTIdent>(expr->GetExpr())) {
    bool isMember = false;
    if (VariableSP global = FindGlobalVariable(
            m_exe_ctx.GetTargetSP(),
            package->GetName().m_text + "." + expr->GetSel()->GetName().m_text,
            isMember)) {
      if (m_frame) {
        m_error.Clear();
        return m_frame->GetValueObjectForFrameVariable(global, m_use_dynamic);
      }
    }
  }
  if (auto ref = llvm::dyn_cast<CobolASTRefModifierExpr>(expr->GetExpr())) {
    if (auto ident = llvm::dyn_cast<const CobolASTIdent>(ref->GetExpr())) {
      auto varName = ident->GetName().m_text;
      auto isMember = false;
      auto target =
          FindGlobalVariable(m_exe_ctx.GetTargetSP(), varName, isMember);
      if (!target || !m_frame)
        return nullptr;
      auto targetSP =
          m_frame->GetValueObjectForFrameVariable(target, m_use_dynamic);
      auto indices = llvm::cast<CobolASTIndexExpr>(ref->GetStartExpr());
      auto memberName = expr->GetSel()->GetName().m_text;
      auto result = m_frame->GetValueObjectForFrameAggregateVariable(
          ConstString(memberName), targetSP, m_use_dynamic, true);

      if (!result) {
        m_error = Status::FromErrorStringWithFormat("Unknown child %s",
                                                    memberName.str().c_str());
        return nullptr;
      }

      return GetIndexedExpression(result, indices, memberName);
    }
  }
  return nullptr;
}

// Gets an element from array where `var` is shared pointer to array type
ValueObjectSP CobolInterpreter::GetElementAtIndex(lldb::ValueObjectSP var,
                                                  uint32_t start,
                                                  uint32_t len) {
  CompilerType elem_type;
  uint64_t max_elem;
  bool is_incomplete;
  bool substring_select = !var->GetCompilerType().IsArrayType(
      &elem_type, &max_elem, &is_incomplete);
  DataExtractor var_data;

  if (substring_select) {
    Status err;
    max_elem = var->GetData(var_data, err);
  }

  if ((start < 1) || (start > max_elem)) {
    m_error =
        Status::FromErrorStringWithFormat("Out of bound index: %d.", start);
    return nullptr;
  }
  start -= 1; // convert 1-based indexing to 0-based.

  if ((start + len) > max_elem)
    len = max_elem - start;

  DataExtractor result_data;
  CompilerType result_type;
  if (substring_select) {
    TargetSP target = m_exe_ctx.GetTargetSP();
    if (!target)
      return nullptr;

    auto type_sys =
        target->GetScratchTypeSystemForLanguage(eLanguageTypeCobol85);
    if (!type_sys)
      return nullptr;

    // FIXME: mutate type with proper size
    result_type = type_sys->get()->MutateBaseTypeSize(
        var->GetCompilerType().GetOpaqueQualType(), len * 8);
    result_data.SetData(var_data, start, len);
  } else {
    uint32_t child_byte_size = 0;
    int32_t child_byte_offset = 0;
    uint32_t child_bitfield_bit_size = 0;
    uint32_t child_bitfield_bit_offset = 0;
    uint64_t ut1 = 0;
    bool bt1 = false;
    bool bt2 = false;
    std::string empty_str;

    if (!(var->GetCompilerType().GetChildCompilerTypeAtIndex(
            &m_exe_ctx, 0, true, true, false, empty_str, child_byte_size,
            child_byte_offset, child_bitfield_bit_size,
            child_bitfield_bit_offset, bt1, bt2, var.get(), ut1)))
      return nullptr;

    const uint64_t offset = start * child_byte_size;
    AddressType address_type = eAddressTypeInvalid;
    auto addr = var->GetAddressOf(true, &address_type);
    result_type = len == 1 ? elem_type : elem_type.GetArrayType(len);
    return ValueObject::CreateValueObjectFromAddress(
        llvm::StringRef(), addr + offset, m_exe_ctx, result_type);
  }
  return ValueObject::CreateValueObjectFromData(llvm::StringRef(), result_data,
                                                m_exe_ctx, result_type);
}

ValueObjectListSP CobolInterpreter::FindAllCandidates(ConstString var_name) {
  ValueObjectListSP candidates(new ValueObjectList);

  VariableListSP var_list_sp(m_frame->GetInScopeVariableList(true));
  VariableList *var_list = var_list_sp.get();

  if (var_list) {
    for (size_t i = 0; i < var_list->GetSize(); i++) {
      VariableSP variable_sp = var_list->GetVariableAtIndex(i);
      if (!variable_sp)
        continue;

      Type *var_type = variable_sp->GetType();
      if (!var_type)
        continue;

      ValueObjectSP valobj_sp =
          m_frame->GetValueObjectForFrameVariable(variable_sp, m_use_dynamic);
      if (!valobj_sp)
        continue;

      valobj_sp = m_frame->GetValueObjectForFrameAggregateVariable(
          var_name, valobj_sp, m_use_dynamic, true);
      if (valobj_sp) {
        candidates->Append(valobj_sp);
      }
    }
  }

  return candidates;
}

/*
result: Shared pointer to a value corresponding to element with all indices 1.
indices: Index expression
var_name: Name of the field
*/
ValueObjectSP CobolInterpreter::GetIndexedExpression(
    lldb::ValueObjectSP result, const lldb_private::CobolASTIndexExpr *indices,
    llvm::StringRef var_name) {
  for (size_t i = 0; i < indices->GetNumberOfIndices(); ++i) {
    auto index =
        GetUIntFromValueObjectSP(EvaluateExpr(indices->GetIndices()[i].get()));
    if (m_error.Fail()) {
      result = nullptr;
      break;
    }

    auto level = indices->GetNumberOfIndices() - i;
    auto top_level_parent =
        result->GetParent()->FollowParentChain([&level](ValueObject *par) {
          if (par->GetCompilerType().IsArrayType())
            level--;
          return (level != 0);
        });

    if (!top_level_parent) {
      result = nullptr;
      break;
    }

    result = GetElementAtIndex(top_level_parent->GetSP(), index);
    if (!result)
      break;

    result = m_frame->GetValueObjectForFrameAggregateVariable(
        ConstString(var_name), result, m_use_dynamic, true);
  }
  return result;
}

ValueObjectSP
CobolInterpreter::FindFieldInStructArray(const CobolASTRefModifierExpr *expr) {
  auto ident = llvm::cast<const CobolASTIdent>(expr->GetExpr());
  auto indices = llvm::cast<const CobolASTIndexExpr>(expr->GetStartExpr());

  llvm::StringRef var_name = ident->GetName().m_text;

  auto candidates_sp = FindAllCandidates(ConstString(var_name));

  if (candidates_sp->GetSize() == 0) {
    m_error = Status::FromErrorStringWithFormat("Unknown variable %s",
                                                var_name.str().c_str());
    return nullptr;
  }

  for (auto result : candidates_sp->GetObjects()) {
    m_error.Clear();
    result = GetIndexedExpression(result, indices, var_name);
    if (result)
      return result;
  }

  return nullptr;
}

uint32_t CobolInterpreter::GetUIntFromValueObjectSP(ValueObjectSP var) {
  uint32_t index;
  llvm::StringRef index_string(var->GetValueAsCString());
  if (index_string.getAsInteger(10, index)) {
    m_error = Status::FromErrorStringWithFormat(
        "ref modifier invalid index %s.", index_string.str().c_str());
    return 0;
  }
  return index;
}

ValueObjectSP
CobolInterpreter::VisitRefModExpr(const CobolASTRefModifierExpr *expr) {
  auto index_expr = llvm::cast<CobolASTIndexExpr>(expr->GetStartExpr());
  ValueObjectSP var = EvaluateExpr(expr->GetExpr());
  if (!var || index_expr->GetNumberOfIndices() != 1) {
    m_error.Clear();
    switch (expr->GetExpr()->GetKind()) {
    case CobolASTNode::eIdent:
      return FindFieldInStructArray(expr);
    case CobolASTNode::eSelectorExpr: {
      auto selector = llvm::cast<CobolASTSelectorExpr>(expr->GetExpr());
      auto target = EvaluateExpr(selector->GetExpr());
      if (!target)
        return nullptr;
      auto memberName = selector->GetSel()->GetName().m_text;
      auto result = m_frame->GetValueObjectForFrameAggregateVariable(
          ConstString(memberName), target, m_use_dynamic, true);
      return GetIndexedExpression(
          result, llvm::cast<CobolASTIndexExpr>(expr->GetStartExpr()),
          memberName);
    }
    default:
      return nullptr;
    }

    auto start_var = EvaluateExpr(index_expr->GetIndices()[0].get());
    auto start = GetUIntFromValueObjectSP(start_var);
    if (m_error.Fail())
      return nullptr;
    auto len_var = EvaluateExpr(expr->GetLenExpr());
    uint32_t len;
    if (len_var) {
      len = GetUIntFromValueObjectSP(len_var);
      if (m_error.Fail())
        return nullptr;
      return GetElementAtIndex(var, start, len);
    }
    return GetElementAtIndex(var, start);
  }
}

ValueObjectSP
CobolInterpreter::VisitFuncCallExpr(const CobolASTFuncCallExpr *expr) {
  llvm::StringRef funcName = expr->GetFuncName().m_text;
  // if (!funcName.equals(llvm::StringRef("sizeof")))
  if (funcName == (llvm::StringRef("sizeof")))
    // TODO
    return nullptr;

  if (expr->getTotalNumParams() != 1) {
    m_error =
        Status::FromErrorString("wrong number of params for sizeof operator.");
    return nullptr;
  }

  ValueObjectSP param = EvaluateExpr(expr->getParamAtIndex(0));
  if (!param)
    return nullptr;

  auto data_size = param->GetByteSize().value();
  DataBufferSP buffer(new DataBufferHeap(sizeof(data_size), 0));
  TargetSP target = m_exe_ctx.GetTargetSP();
  if (!target)
    return nullptr;

  auto type_sys = target->GetScratchTypeSystemForLanguage(eLanguageTypeCobol85);
  if (!type_sys)
    return nullptr;

  ByteOrder byte_order = target->GetArchitecture().GetByteOrder();
  uint8_t addr_size = target->GetArchitecture().GetAddressByteSize();
  DataEncoder enc(buffer->GetBytes(), sizeof(data_size), byte_order, addr_size);
  enc.PutData(0, &data_size, sizeof(data_size));
  DataExtractor data(enc.GetDataBuffer(), byte_order, addr_size);

  CompilerType comp_type =
      type_sys->get()->GetBasicTypeFromAST(eBasicTypeUnsignedInt);
  return ValueObject::CreateValueObjectFromData(llvm::StringRef(), data,
                                                m_exe_ctx, comp_type);
}

ValueObjectSP
CobolInterpreter::VisitCompareExpr(const CobolASTCompareExpr *expr) {
  Value valF(Scalar(0));
  ValueObjectSP ValFalse = ValueObjectConstResult::Create(
      m_exe_ctx.GetBestExecutionContextScope(), valF, ConstString(""));
  TargetSP target = m_exe_ctx.GetTargetSP();
  if (!target)
    return ValFalse;

  auto type_sys = target->GetScratchTypeSystemForLanguage(eLanguageTypeCobol85);
  if (!type_sys)
    return ValFalse;

  TypeSystemLegacy *legacy_ts =
      llvm::dyn_cast_or_null<TypeSystemLegacy>(type_sys->get());
  if (!legacy_ts)
    return ValFalse;

  Value valT(Scalar(UINT8_MAX));
  ValueObjectSP ValTrue = ValueObjectConstResult::Create(
      m_exe_ctx.GetBestExecutionContextScope(), valT, ConstString(""));
  ValueObjectSP lhsVal = EvaluateExpr(expr->GetlhsExpr());
  ValueObjectSP rhsVal = EvaluateExpr(expr->GetrhsExpr());

  DataExtractor lData, rData;
  Status error;
  lhsVal->GetData(lData, error);
  rhsVal->GetData(rData, error);
  auto lhsCompTy = lhsVal->GetCompilerType();
  auto rhsCompTy = rhsVal->GetCompilerType();
  if (lhsCompTy == rhsCompTy) {
    if (lData.Compare(rData))
      return ValTrue;
  }
  return ValFalse;
}

ValueObjectSP
CobolInterpreter::VisitAssignmentExpr(const CobolASTAssignmentExpr *expr) {
  ValueObjectSP lhsRef = EvaluateExpr(expr->GetlhsExpr());
  if (!lhsRef)
    return nullptr;

  ValueObjectSP rhsVal = EvaluateExpr(expr->GetrhsExpr());
  if (!rhsVal)
    return nullptr;

  DataExtractor data;
  Status error;
  rhsVal->GetData(data, error);

  if (error.Fail())
    return nullptr;

  TargetSP target = m_exe_ctx.GetTargetSP();
  if (!target)
    return nullptr;

  auto type_sys = target->GetScratchTypeSystemForLanguage(eLanguageTypeCobol85);
  if (!type_sys)
    return nullptr;

  TypeSystemLegacy *legacy_ts =
      llvm::dyn_cast_or_null<TypeSystemLegacy>(type_sys->get());
  if (!legacy_ts)
    return nullptr;

  const uint8_t addr_size = target->GetArchitecture().GetAddressByteSize();
  const auto dst_type = lhsRef->GetCompilerType().GetOpaqueQualType();
  const auto src_type = rhsVal->GetCompilerType().GetOpaqueQualType();
  const auto data_size = lhsRef->GetByteSize();
  DataBufferSP buffer(new DataBufferHeap(*data_size, 0));
  DataExtractor dest_data(buffer, ByteOrder::eByteOrderInvalid, addr_size);
  if (!legacy_ts->EncodeDataToType(m_exe_ctx, src_type, data, dst_type,
                                   dest_data))
    return nullptr;

  lhsRef->SetData(dest_data, error);
  return lhsRef;
}

CobolUserExpression::CobolUserExpression(
    ExecutionContextScope &exe_scope, llvm::StringRef expr,
    llvm::StringRef prefix, SourceLanguage language, ResultType desired_type,
    const EvaluateExpressionOptions &options)
    : UserExpression(exe_scope, expr, prefix, language, desired_type, options) {
}

bool CobolUserExpression::Parse(DiagnosticManager &diagnostic_manager,
                                ExecutionContext &exe_ctx,
                                ExecutionPolicy execution_policy,
                                bool keep_result_in_memory,
                                bool generate_debug_info) {
  Log *log = GetLog(LLDBLog::Expressions);
  InstallContext(exe_ctx);
  m_interpreter.reset(new CobolInterpreter(exe_ctx, GetUserText()));
  if (m_interpreter->Parse())
    return true;

  LLDB_LOGF(log, "error while parsing the following code:%s\n", GetUserText());
  diagnostic_manager.Printf(lldb::eSeverityError,
                            "cobol expression can't be interpreted");
  return false;
}

lldb::ExpressionResults
CobolUserExpression::DoExecute(DiagnosticManager &diagnostic_manager,
                               ExecutionContext &exe_ctx,
                               const EvaluateExpressionOptions &options,
                               lldb::UserExpressionSP &shared_ptr_to_me,
                               lldb::ExpressionVariableSP &result) {

  Log *log = GetLog(LLDBLog::Expressions);

  lldb_private::ExecutionPolicy execution_policy = options.GetExecutionPolicy();
  lldb::ExpressionResults execution_results = lldb::eExpressionSetupError;

  Process *process = exe_ctx.GetProcessPtr();
  Target *target = exe_ctx.GetTargetPtr();

  if (target == nullptr || process == nullptr ||
      process->GetState() != lldb::eStateStopped) {
    if (execution_policy == eExecutionPolicyAlways) {
      if (log)
        log->Printf(
            "== [CobolUserExpression::Evaluate] Expression may not run, "
            "but is not constant ==");

      diagnostic_manager.PutString(lldb::eSeverityError,
                                   "expression needed to run but couldn't");

      return execution_results;
    }
  }

  SourceLanguage language = target->GetLanguage();

  m_interpreter->set_use_dynamic(options.GetUseDynamic());
  ValueObjectSP result_val_sp = m_interpreter->Evaluate(exe_ctx);
  Status err = std::move(m_interpreter->error());
  m_interpreter.reset();

  if (!result_val_sp) {
    const char *error_cstr = err.AsCString();
    if (error_cstr && error_cstr[0]) {
      diagnostic_manager.PutString(lldb::eSeverityError, error_cstr);
      if (log)
        LLDB_LOGF(log, "expression interpretation error: %s\n.", error_cstr);
    } else {
      diagnostic_manager.PutString(lldb::eSeverityError,
                                   "expression can't be interpreted or run");
      if (log)
        LLDB_LOGF(log, "expression interpretation Unknown error\n.");
    }
    return lldb::eExpressionDiscarded;
  }

  result_val_sp->UpdateValueIfNeeded();
  result.reset(new ExpressionVariable());
  result->m_live_sp = result->m_frozen_sp = result_val_sp;
  result->m_flags |= ExpressionVariable::EVIsProgramReference;
  PersistentExpressionState *pv =
      target->GetPersistentExpressionStateForLanguage(
          language.AsLanguageType());
  if (pv != nullptr) {
    if (result_val_sp->GetName().IsEmpty())
      result->SetName(pv->GetNextPersistentVariableName());
    pv->AddVariable(result);
  }
  return lldb::eExpressionCompleted;
}
