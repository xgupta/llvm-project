//===--   PLIAST.h --------------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef liblldb_PLIAST_h
#define liblldb_PLIAST_h

#include "Plugins/ExpressionParser/PLI/PLILexer.h"
#include "lldb/lldb-forward.h"
#include "lldb/lldb-private.h"
#include "llvm/Support/Casting.h"

namespace lldb_private {

class PLIASTNode {
public:
  enum NodeKind {
    eBasicLit,
    eUnaryExpr,
    eEmptyStmt,
    eExprStmt,
    eIdent,
    eRefModExpr,
    eSelectorExpr,
    eFuncCallExpr,
    eAssignmentExpr,
  };

  virtual ~PLIASTNode() = default;
  virtual const char *GetKindName() const = 0;
  NodeKind GetKind() const { return m_kind; }

protected:
  explicit PLIASTNode(NodeKind kind) : m_kind(kind) {}

private:
  const NodeKind m_kind;

  PLIASTNode(const PLIASTNode &) = delete;
  const PLIASTNode &operator=(const PLIASTNode &) = delete;
};

class PLIASTExpr : public PLIASTNode {
public:
  template <typename R, typename V> R Visit(V *v) const;

  static bool classof(const PLIASTNode *node) {
    return node->GetKind() == eIdent;
  }

protected:
  explicit PLIASTExpr(NodeKind kind) : PLIASTNode(kind) {}

private:
  PLIASTExpr(const PLIASTExpr &) = delete;
  const PLIASTExpr &operator=(const PLIASTExpr &) = delete;
};

class PLIASTIdent : public PLIASTExpr {
public:
  explicit PLIASTIdent(PLILexer::Token name)
      : PLIASTExpr(eIdent), m_name(name) {}
  ~PLIASTIdent() override = default;

  const char *GetKindName() const override { return "Identifier"; }
  static bool classof(const PLIASTNode *node) {
    return node->GetKind() == eIdent;
  }
  PLILexer::Token GetName() const { return m_name; }
  void SetName(PLILexer::Token name) { m_name = name; }

private:
  friend class PLIASTNode;
  PLILexer::Token m_name;
  PLIASTIdent(const PLIASTIdent &) = delete;
  const PLIASTIdent &operator=(const PLIASTIdent &) = delete;
};

class PLIASTStmt : public PLIASTNode {
public:
  template <typename R, typename V> R Visit(V *v) const;
  static bool classof(const PLIASTNode *node) {
    return node->GetKind() == eEmptyStmt;
  }

protected:
  explicit PLIASTStmt(NodeKind kind) : PLIASTNode(kind) {}

private:
  PLIASTStmt(const PLIASTStmt &) = delete;
  const PLIASTNode &operator=(const PLIASTNode &) = delete;
};

class PLIASTEmptyStmt : public PLIASTStmt {
public:
  PLIASTEmptyStmt() : PLIASTStmt(eEmptyStmt) {}
  ~PLIASTEmptyStmt() override = default;

  const char *GetKindName() const override { return "Empty Statement"; }
  static bool classof(const PLIASTNode *node) {
    return node->GetKind() == eEmptyStmt;
  }

  PLIASTEmptyStmt(const PLIASTEmptyStmt &) = delete;
  const PLIASTEmptyStmt &operator=(const PLIASTEmptyStmt &) = delete;
};

class PLIASTExprStmt : public PLIASTStmt {
public:
  explicit PLIASTExprStmt(PLIASTExpr *expr)
      : PLIASTStmt(eExprStmt), m_expr(expr) {}
  ~PLIASTExprStmt() override = default;

  const char *GetKindName() const override { return "Expr Statement"; }
  static bool classof(const PLIASTNode *n) { return n->GetKind() == eExprStmt; }
  const PLIASTExpr *GetExpr() const { return m_expr.get(); }
  void SetExpr(PLIASTExpr *expr) { m_expr.reset(expr); }

private:
  friend class PLIASTNode;
  std::unique_ptr<PLIASTExpr> m_expr;
  PLIASTExprStmt(const PLIASTExprStmt &) = delete;
  const PLIASTExprStmt &operator=(const PLIASTExprStmt &) = delete;
};

class PLIASTBasicLit : public PLIASTExpr {
public:
  explicit PLIASTBasicLit(PLILexer::Token T)
      : PLIASTExpr(eBasicLit), m_value(T) {}
  ~PLIASTBasicLit() override = default;

  const char *GetKindName() const override { return "Basic Literal"; }
  static bool classof(const PLIASTNode *node) {
    return node->GetKind() == eBasicLit;
  }

  PLILexer::Token GetValue() const { return m_value; }
  void SetValue(PLILexer::Token T) { m_value = T; }

private:
  friend class PLIASTNode;
  PLILexer::Token m_value;

  PLIASTBasicLit(const PLIASTBasicLit &) = delete;
  const PLIASTBasicLit &operator=(const PLIASTBasicLit &) = delete;
};

class PLIASTUnaryExpr : public PLIASTExpr {
public:
  PLIASTUnaryExpr(PLILexer::PLILexer::TokenType tok, PLIASTExpr *expr)
      : PLIASTExpr(eUnaryExpr), m_tt(tok), m_expr(expr) {}
  ~PLIASTUnaryExpr() override = default;

  const char *GetKindName() const override { return "Unary Expression"; }
  static bool classof(const PLIASTNode *node) {
    return node->GetKind() == eUnaryExpr;
  }

  PLILexer::PLILexer::TokenType GetOp() const { return m_tt; }
  void SetOp(PLILexer::PLILexer::TokenType tt) { m_tt = tt; }

  const PLIASTExpr *GetExpr() const { return m_expr.get(); }
  void Setexpr(PLIASTExpr *expr) { m_expr.reset(expr); }

private:
  friend class PLIASTNode;
  PLILexer::PLILexer::TokenType m_tt;
  std::unique_ptr<PLIASTExpr> m_expr;

  PLIASTUnaryExpr(const PLIASTUnaryExpr &) = delete;
  const PLIASTUnaryExpr &operator=(const PLIASTUnaryExpr &) = delete;
};

class PLIASTSelectorExpr : public PLIASTExpr {
public:
  PLIASTSelectorExpr(PLIASTExpr *expr, PLIASTIdent *sel)
      : PLIASTExpr(eSelectorExpr), m_expr_up(expr), m_sel_up(sel) {}
  ~PLIASTSelectorExpr() override = default;

  const char *GetKindName() const override { return "Selector Expression"; }

  static bool classof(const PLIASTNode *n) {
    return n->GetKind() == eSelectorExpr;
  }

  const PLIASTExpr *GetExpr() const { return m_expr_up.get(); }
  void SetExpr(PLIASTExpr *expr) { m_expr_up.reset(expr); }

  const PLIASTIdent *GetSel() const { return m_sel_up.get(); }
  void SetSel(PLIASTIdent *sel) { m_sel_up.reset(sel); }

private:
  friend class PLIASTNode;
  std::unique_ptr<PLIASTExpr> m_expr_up;
  std::unique_ptr<PLIASTIdent> m_sel_up;

  PLIASTSelectorExpr(const PLIASTSelectorExpr &) = delete;
  const PLIASTSelectorExpr &operator=(const PLIASTSelectorExpr &) = delete;
};

class PLIASTRefModifierExpr : public PLIASTExpr {
public:
  PLIASTRefModifierExpr(PLIASTExpr *expr, PLIASTExpr *start, PLIASTExpr *len)
      : PLIASTExpr(eRefModExpr), m_expr(expr), m_expr_start(start),
        m_expr_len(len) {}
  ~PLIASTRefModifierExpr() override = default;

  static bool classof(const PLIASTNode *n) {
    return n->GetKind() == eRefModExpr;
  }

  const PLIASTExpr *GetExpr() const { return m_expr.get(); }
  void SetExpr(PLIASTExpr *expr) { m_expr.reset(expr); }

  const PLIASTExpr *GetStartExpr() const { return m_expr_start.get(); }
  void SetStartExpr(PLIASTExpr *expr) { m_expr_start.reset(expr); }

  const PLIASTExpr *GetLenExpr() const { return m_expr_len.get(); }
  void SetLenExpr(PLIASTExpr *expr) { m_expr_len.reset(expr); }

  const char *GetKindName() const override {
    return "reference modifier Expression";
  }

private:
  std::unique_ptr<PLIASTExpr> m_expr;
  std::unique_ptr<PLIASTExpr> m_expr_start;
  std::unique_ptr<PLIASTExpr> m_expr_len;

  PLIASTRefModifierExpr(const PLIASTRefModifierExpr &) = delete;
  const PLIASTRefModifierExpr &
  operator=(const PLIASTRefModifierExpr &) = delete;
};

class PLIASTFuncCallExpr : public PLIASTExpr {
public:
  PLIASTFuncCallExpr(PLILexer::Token funcName)
      : PLIASTExpr(eFuncCallExpr), funcName(funcName) {}

  static bool classof(const PLIASTNode *n) {
    return n->GetKind() == eFuncCallExpr;
  }

  PLILexer::Token GetFuncName() const { return funcName; }
  const PLIASTExpr *getParamAtIndex(size_t index) const {
    if (index < paramList.size())
      return paramList[index];
    return nullptr;
  }

  void addParam(PLIASTExpr *param) { paramList.push_back(param); }

  const char *GetKindName() const override {
    return "function call expression";
  }

  size_t getTotalNumParams() const { return paramList.size(); }

private:
  std::vector<PLIASTExpr *> paramList;
  PLILexer::Token funcName;

  PLIASTFuncCallExpr(const PLIASTFuncCallExpr &) = delete;
  const PLIASTFuncCallExpr &operator=(const PLIASTFuncCallExpr &) = delete;
};

class PLIASTAssignmentExpr : public PLIASTExpr {
public:
  PLIASTAssignmentExpr(PLIASTExpr *lhs, PLIASTExpr *rhs)
      : PLIASTExpr(eAssignmentExpr), m_lhsExpr(lhs), m_rhsExpr(rhs) {}

  static bool classof(const PLIASTNode *n) {
    return n->GetKind() == eAssignmentExpr;
  }

  const PLIASTExpr *GetlhsExpr() const { return m_lhsExpr.get(); }
  void SetlhsExpr(PLIASTExpr *expr) { m_lhsExpr.reset(expr); }

  const PLIASTExpr *GetrhsExpr() const { return m_rhsExpr.get(); }
  void SetrhsExpr(PLIASTExpr *expr) { m_rhsExpr.reset(expr); }

  const char *GetKindName() const override { return "assignment expression"; }

private:
  std::unique_ptr<PLIASTExpr> m_lhsExpr;
  std::unique_ptr<PLIASTExpr> m_rhsExpr;

  PLIASTAssignmentExpr(const PLIASTAssignmentExpr &) = delete;
  const PLIASTAssignmentExpr &operator=(const PLIASTAssignmentExpr &) = delete;
};

template <typename R, typename V> R PLIASTExpr::Visit(V *v) const {
  switch (GetKind()) {
  default:
    return R();
  case eBasicLit:
    return v->VisitBasicLit(llvm::cast<const PLIASTBasicLit>(this));
  case eIdent:
    return v->VisitIdent(llvm::cast<const PLIASTIdent>(this));
  case eUnaryExpr:
    return v->VisitUnaryExpr(llvm::cast<const PLIASTUnaryExpr>(this));
  case eSelectorExpr:
    return v->VisitSelectorExpr(llvm::cast<const PLIASTSelectorExpr>(this));
  case eRefModExpr:
    return v->VisitRefModExpr(llvm::cast<const PLIASTRefModifierExpr>(this));
  case eFuncCallExpr:
    return v->VisitFuncCallExpr(llvm::cast<const PLIASTFuncCallExpr>(this));
  case eAssignmentExpr:
    return v->VisitAssignmentExpr(llvm::cast<const PLIASTAssignmentExpr>(this));
  }
}

} // namespace lldb_private

#endif // liblldb_PLIAST_h
