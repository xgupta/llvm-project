//===-  CobolUserExpression.h ---------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef liblldb_CobolUserExpression_h_
#define liblldb_CobolUserExpression_h_

#include "lldb/Expression/ExpressionVariable.h"
#include "lldb/Expression/UserExpression.h"
#include "lldb/Target/ExecutionContext.h"
#include "lldb/Utility/Status.h"
#include "lldb/lldb-forward.h"
#include "lldb/lldb-private.h"

#include "Plugins/ExpressionParser/Cobol/CobolAST.h"
#include "Plugins/ExpressionParser/Cobol/CobolParser.h"
#include <memory>

namespace lldb_private {
/// TODO - not used yet.
class CobolPersistentExpressionState
    : public llvm::RTTIExtends<CobolPersistentExpressionState,
                               PersistentExpressionState> {
public:
  // LLVM RTTI support
  static char ID;
  CobolPersistentExpressionState();

  void RemovePersistentVariable(lldb::ExpressionVariableSP variable) override;

  ConstString GetNextPersistentVariableName(bool is_error = false) override;

protected:
  llvm::StringRef
  GetPersistentVariablePrefix(bool is_error = false) const override {
    return "$Cobol";
  }

private:
  uint32_t m_next_persistent_variable_id = 0;
};

class CobolInterpreter {
public:
  CobolInterpreter(ExecutionContext &exe_ctx, const char *expr);

  bool Parse();
  lldb::ValueObjectSP Evaluate(ExecutionContext &exe_ctx);
  lldb::ValueObjectSP EvaluateStatement(const lldb_private::CobolASTStmt *s);
  lldb::ValueObjectSP EvaluateExpr(const lldb_private::CobolASTExpr *e);

  lldb::ValueObjectSP VisitBasicLit(const lldb_private::CobolASTBasicLit *e);
  lldb::ValueObjectSP VisitIdent(const lldb_private::CobolASTIdent *e);
  lldb::ValueObjectSP VisitUnaryExpr(const lldb_private::CobolASTUnaryExpr *e);
  lldb::ValueObjectSP
  VisitSelectorExpr(const lldb_private::CobolASTSelectorExpr *e);
  lldb::ValueObjectSP
  VisitRefModExpr(const lldb_private::CobolASTRefModifierExpr *expr);
  lldb::ValueObjectSP
  VisitFuncCallExpr(const lldb_private::CobolASTFuncCallExpr *expr);
  lldb::ValueObjectSP
  VisitAssignmentExpr(const lldb_private::CobolASTAssignmentExpr *expr);
  lldb::ValueObjectSP
  VisitCompareExpr(const lldb_private::CobolASTCompareExpr *expr);

  void set_use_dynamic(lldb::DynamicValueType use_dynamic) {
    m_use_dynamic = use_dynamic;
  }

  Status &error() { return m_error; }

private:
  ExecutionContext m_exe_ctx;
  lldb::StackFrameSP m_frame;
  Status m_error;
  lldb::DynamicValueType m_use_dynamic;
  CobolParser m_parser;
  std::vector<std::unique_ptr<CobolASTStmt>> m_statements;
  lldb::ValueObjectSP
  FindFieldInStructArray(const lldb_private::CobolASTRefModifierExpr *expr);
  lldb::ValueObjectSP GetElementAtIndex(lldb::ValueObjectSP var, uint32_t start,
                                        uint32_t len = 1);
  uint32_t GetUIntFromValueObjectSP(lldb::ValueObjectSP var);
  uint32_t GetUIntFromValueObjectSPReportIndex(lldb::ValueObjectSP var, bool reportIndex);
  lldb::ValueObjectListSP FindAllCandidates(ConstString var_name);
  lldb::ValueObjectSP
  GetIndexedExpression(lldb::ValueObjectSP result,
                       const lldb_private::CobolASTIndexExpr *indices,
                       llvm::StringRef var_name);
  lldb::ValueObjectSP GetLevel88(llvm::StringRef var_name,
                                 lldb::ValueObjectSP &result,
                                 CompilerType comp_type, int index);
};

class CobolUserExpression : public UserExpression {
  // LLVM RTTI support
  static char ID;

public:
  bool isA(const void *ClassID) const override {
    return ClassID == &ID || UserExpression::isA(ClassID);
  }
  static bool classof(const Expression *obj) { return obj->isA(&ID); }

  CobolUserExpression(ExecutionContextScope &exe_scope, llvm::StringRef expr,
                      llvm::StringRef prefix, SourceLanguage language,
                      ResultType desired_type,
                      const EvaluateExpressionOptions &options);

  bool Parse(DiagnosticManager &diagnostic_manager, ExecutionContext &exe_ctx,
             lldb_private::ExecutionPolicy execution_policy,
             bool keep_result_in_memory, bool generate_debug_info) override;

  bool CanInterpret() override { return true; }
  bool FinalizeJITExecution(
      DiagnosticManager &diagnostic_manager, ExecutionContext &exe_ctx,
      lldb::ExpressionVariableSP &result,
      lldb::addr_t function_stack_bottom = LLDB_INVALID_ADDRESS,
      lldb::addr_t function_stack_top = LLDB_INVALID_ADDRESS) override {
    return true;
  }

protected:
  lldb::ExpressionResults
  DoExecute(DiagnosticManager &diagnostic_manager, ExecutionContext &exe_ctx,
            const EvaluateExpressionOptions &options,
            lldb::UserExpressionSP &shared_ptr_to_me,
            lldb::ExpressionVariableSP &result) override;

private:
  std::unique_ptr<CobolInterpreter> m_interpreter;
};

} // namespace lldb_private

#endif // liblldb_CobolUserExpression_h_
