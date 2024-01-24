//===--   PLIUserExpression.h -----------------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef liblldb_PLIUserExpression_h_
#define liblldb_PLIUserExpression_h_

#include <memory>

#include "lldb/Expression/ExpressionVariable.h"
#include "lldb/Expression/UserExpression.h"
#include "lldb/Target/ExecutionContext.h"
#include "lldb/Utility/Status.h"
#include "lldb/lldb-forward.h"
#include "lldb/lldb-private.h"

#include "Plugins/ExpressionParser/PLI/PLIAST.h"
#include "Plugins/ExpressionParser/PLI/PLIParser.h"

namespace lldb_private {
/// TODO - not used yet.
class PLIPersistentExpressionState
    : public llvm::RTTIExtends<PLIPersistentExpressionState,
                               PersistentExpressionState> {
public:
  // LLVM RTTI support
  static char ID;
  PLIPersistentExpressionState();

  void RemovePersistentVariable(lldb::ExpressionVariableSP variable) override;

  ConstString GetNextPersistentVariableName(bool is_error = false) override;

protected:
  llvm::StringRef
  GetPersistentVariablePrefix(bool is_error = false) const override {
    return "$PLI";
  }

private:
  uint32_t m_next_persistent_variable_id = 0;
};

class PLIInterpreter {
public:
  PLIInterpreter(ExecutionContext &exe_ctx, const char *expr);

  bool Parse();
  lldb::ValueObjectSP Evaluate(ExecutionContext &exe_ctx);
  lldb::ValueObjectSP EvaluateStatement(const lldb_private::PLIASTStmt *s);
  lldb::ValueObjectSP EvaluateExpr(const lldb_private::PLIASTExpr *e);

  lldb::ValueObjectSP VisitBasicLit(const lldb_private::PLIASTBasicLit *e);
  lldb::ValueObjectSP VisitIdent(const lldb_private::PLIASTIdent *e);
  lldb::ValueObjectSP VisitUnaryExpr(const lldb_private::PLIASTUnaryExpr *e);
  lldb::ValueObjectSP
  VisitSelectorExpr(const lldb_private::PLIASTSelectorExpr *e);
  lldb::ValueObjectSP
  VisitRefModExpr(const lldb_private::PLIASTRefModifierExpr *expr);
  lldb::ValueObjectSP
  VisitFuncCallExpr(const lldb_private::PLIASTFuncCallExpr *expr);
  lldb::ValueObjectSP
  VisitAssignmentExpr(const lldb_private::PLIASTAssignmentExpr *expr);

  void set_use_dynamic(lldb::DynamicValueType use_dynamic) {
    m_use_dynamic = use_dynamic;
  }

  Status &error() { return m_error; }

private:
  ExecutionContext m_exe_ctx;
  lldb::StackFrameSP m_frame;
  Status m_error;
  lldb::DynamicValueType m_use_dynamic;
  PLIParser m_parser;
  std::vector<std::unique_ptr<PLIASTStmt>> m_statements;
};

class PLIUserExpression : public UserExpression {
  // LLVM RTTI support
  static char ID;

public:
  bool isA(const void *ClassID) const override {
    return ClassID == &ID || UserExpression::isA(ClassID);
  }
  static bool classof(const Expression *obj) { return obj->isA(&ID); }

  PLIUserExpression(ExecutionContextScope &exe_scope, llvm::StringRef expr,
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
  std::unique_ptr<PLIInterpreter> m_interpreter;
};

} // namespace lldb_private

#endif // liblldb_PLIUserExpression_h_
