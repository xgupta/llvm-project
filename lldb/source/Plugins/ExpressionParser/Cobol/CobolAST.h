//===-- CobolAST.h ----------------------------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef liblldb_CobolAST_h
#define liblldb_CobolAST_h

#include "Plugins/ExpressionParser/Cobol/CobolLexer.h"
#include "lldb/lldb-forward.h"
#include "lldb/lldb-private.h"
#include "llvm/Support/Casting.h"

namespace lldb_private {

class CobolASTNode {
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

  virtual ~CobolASTNode() = default;
  virtual const char *GetKindName() const = 0;
  NodeKind GetKind() const { return m_kind; }

protected:
  explicit CobolASTNode(NodeKind kind) : m_kind(kind) {}

private:
  const NodeKind m_kind;

  CobolASTNode(const CobolASTNode &) = delete;
  const CobolASTNode &operator=(const CobolASTNode &) = delete;
};

class CobolASTExpr : public CobolASTNode {
public:
  template <typename R, typename V> R Visit(V *v) const;

  static bool classof(const CobolASTNode *node) {
    return node->GetKind() == eIdent;
  }

protected:
  explicit CobolASTExpr(NodeKind kind) : CobolASTNode(kind) {}

private:
  CobolASTExpr(const CobolASTExpr &) = delete;
  const CobolASTExpr &operator=(const CobolASTExpr &) = delete;
};

class CobolASTIdent : public CobolASTExpr {
public:
  explicit CobolASTIdent(CobolLexer::Token name)
      : CobolASTExpr(eIdent), m_name(name) {}
  ~CobolASTIdent() override = default;

  const char *GetKindName() const override { return "Identifier"; }
  static bool classof(const CobolASTNode *node) {
    return node->GetKind() == eIdent;
  }
  CobolLexer::Token GetName() const { return m_name; }
  void SetName(CobolLexer::Token name) { m_name = name; }

private:
  friend class CobolASTNode;
  CobolLexer::Token m_name;
  CobolASTIdent(const CobolASTIdent &) = delete;
  const CobolASTIdent &operator=(const CobolASTIdent &) = delete;
};

class CobolASTStmt : public CobolASTNode {
public:
  template <typename R, typename V> R Visit(V *v) const;
  static bool classof(const CobolASTNode *node) {
    return node->GetKind() == eEmptyStmt;
  }

protected:
  explicit CobolASTStmt(NodeKind kind) : CobolASTNode(kind) {}

private:
  CobolASTStmt(const CobolASTStmt &) = delete;
  const CobolASTNode &operator=(const CobolASTNode &) = delete;
};

class CobolASTEmptyStmt : public CobolASTStmt {
public:
  CobolASTEmptyStmt() : CobolASTStmt(eEmptyStmt) {}
  ~CobolASTEmptyStmt() override = default;

  const char *GetKindName() const override { return "Empty Statement"; }
  static bool classof(const CobolASTNode *node) {
    return node->GetKind() == eEmptyStmt;
  }

  CobolASTEmptyStmt(const CobolASTEmptyStmt &) = delete;
  const CobolASTEmptyStmt &operator=(const CobolASTEmptyStmt &) = delete;
};

class CobolASTExprStmt : public CobolASTStmt {
public:
  explicit CobolASTExprStmt(CobolASTExpr *expr)
      : CobolASTStmt(eExprStmt), m_expr(expr) {}
  ~CobolASTExprStmt() override = default;

  const char *GetKindName() const override { return "Expr Statement"; }
  static bool classof(const CobolASTNode *n) {
    return n->GetKind() == eExprStmt;
  }
  const CobolASTExpr *GetExpr() const { return m_expr.get(); }
  void SetExpr(CobolASTExpr *expr) { m_expr.reset(expr); }

private:
  friend class CobolASTNode;
  std::unique_ptr<CobolASTExpr> m_expr;
  CobolASTExprStmt(const CobolASTExprStmt &) = delete;
  const CobolASTExprStmt &operator=(const CobolASTExprStmt &) = delete;
};

class CobolASTBasicLit : public CobolASTExpr {
public:
  explicit CobolASTBasicLit(CobolLexer::Token T)
      : CobolASTExpr(eBasicLit), m_value(T) {}
  ~CobolASTBasicLit() override = default;

  const char *GetKindName() const override { return "Basic Literal"; }
  static bool classof(const CobolASTNode *node) {
    return node->GetKind() == eBasicLit;
  }

  CobolLexer::Token GetValue() const { return m_value; }
  void SetValue(CobolLexer::Token T) { m_value = T; }

private:
  friend class CobolASTNode;
  CobolLexer::Token m_value;

  CobolASTBasicLit(const CobolASTBasicLit &) = delete;
  const CobolASTBasicLit &operator=(const CobolASTBasicLit &) = delete;
};

class CobolASTUnaryExpr : public CobolASTExpr {
public:
  CobolASTUnaryExpr(CobolLexer::CobolLexer::TokenType tok, CobolASTExpr *expr)
      : CobolASTExpr(eUnaryExpr), m_tt(tok), m_expr(expr) {}
  ~CobolASTUnaryExpr() override = default;

  const char *GetKindName() const override { return "Unary Expression"; }
  static bool classof(const CobolASTNode *node) {
    return node->GetKind() == eUnaryExpr;
  }

  CobolLexer::CobolLexer::TokenType GetOp() const { return m_tt; }
  void SetOp(CobolLexer::CobolLexer::TokenType tt) { m_tt = tt; }

  const CobolASTExpr *GetExpr() const { return m_expr.get(); }
  void Setexpr(CobolASTExpr *expr) { m_expr.reset(expr); }

private:
  friend class CobolASTNode;
  CobolLexer::CobolLexer::TokenType m_tt;
  std::unique_ptr<CobolASTExpr> m_expr;

  CobolASTUnaryExpr(const CobolASTUnaryExpr &) = delete;
  const CobolASTUnaryExpr &operator=(const CobolASTUnaryExpr &) = delete;
};

class CobolASTSelectorExpr : public CobolASTExpr {
public:
  CobolASTSelectorExpr(CobolASTExpr *expr, CobolASTIdent *sel)
      : CobolASTExpr(eSelectorExpr), m_expr_up(expr), m_sel_up(sel) {}
  ~CobolASTSelectorExpr() override = default;

  const char *GetKindName() const override { return "Selector Expression"; }

  static bool classof(const CobolASTNode *n) {
    return n->GetKind() == eSelectorExpr;
  }

  const CobolASTExpr *GetExpr() const { return m_expr_up.get(); }
  void SetExpr(CobolASTExpr *expr) { m_expr_up.reset(expr); }
  void ReplaceExpr(CobolASTExpr *expr) {
    m_expr_up.release();
    m_expr_up.reset(expr);
  }

  const CobolASTIdent *GetSel() const { return m_sel_up.get(); }
  void SetSel(CobolASTIdent *sel) { m_sel_up.reset(sel); }

private:
  friend class CobolASTNode;
  std::unique_ptr<CobolASTExpr> m_expr_up;
  std::unique_ptr<CobolASTIdent> m_sel_up;

  CobolASTSelectorExpr(const CobolASTSelectorExpr &) = delete;
  const CobolASTSelectorExpr &operator=(const CobolASTSelectorExpr &) = delete;
};

class CobolASTRefModifierExpr : public CobolASTExpr {
public:
  CobolASTRefModifierExpr(CobolASTExpr *expr, CobolASTExpr *start,
                          CobolASTExpr *len)
      : CobolASTExpr(eRefModExpr), m_expr(expr), m_expr_start(start),
        m_expr_len(len) {}
  ~CobolASTRefModifierExpr() override = default;

  static bool classof(const CobolASTNode *n) {
    return n->GetKind() == eRefModExpr;
  }

  const CobolASTExpr *GetExpr() const { return m_expr.get(); }
  void SetExpr(CobolASTExpr *expr) { m_expr.reset(expr); }
  void ReplaceExpr(CobolASTExpr *expr) {
    m_expr.release();
    m_expr.reset(expr);
  }

  const CobolASTExpr *GetStartExpr() const { return m_expr_start.get(); }
  void SetStartExpr(CobolASTExpr *expr) { m_expr_start.reset(expr); }

  const CobolASTExpr *GetLenExpr() const { return m_expr_len.get(); }
  void SetLenExpr(CobolASTExpr *expr) { m_expr_len.reset(expr); }

  const char *GetKindName() const override {
    return "reference modifier Expression";
  }

private:
  std::unique_ptr<CobolASTExpr> m_expr;
  std::unique_ptr<CobolASTExpr> m_expr_start;
  std::unique_ptr<CobolASTExpr> m_expr_len;

  CobolASTRefModifierExpr(const CobolASTRefModifierExpr &) = delete;
  const CobolASTRefModifierExpr &
  operator=(const CobolASTRefModifierExpr &) = delete;
};

class CobolASTFuncCallExpr : public CobolASTExpr {
public:
  CobolASTFuncCallExpr(CobolLexer::Token funcName)
      : CobolASTExpr(eFuncCallExpr), funcName(funcName) {}

  static bool classof(const CobolASTNode *n) {
    return n->GetKind() == eFuncCallExpr;
  }

  CobolLexer::Token GetFuncName() const { return funcName; }
  const CobolASTExpr *getParamAtIndex(size_t index) const {
    if (index < paramList.size())
      return paramList[index];
    return nullptr;
  }

  void addParam(CobolASTExpr *param) { paramList.push_back(param); }

  const char *GetKindName() const override {
    return "function call expression";
  }

  size_t getTotalNumParams() const { return paramList.size(); }

private:
  std::vector<CobolASTExpr *> paramList;
  CobolLexer::Token funcName;

  CobolASTFuncCallExpr(const CobolASTFuncCallExpr &) = delete;
  const CobolASTFuncCallExpr &operator=(const CobolASTFuncCallExpr &) = delete;
};

class CobolASTAssignmentExpr : public CobolASTExpr {
public:
  CobolASTAssignmentExpr(CobolASTExpr *lhs, CobolASTExpr *rhs)
      : CobolASTExpr(eAssignmentExpr), m_lhsExpr(lhs), m_rhsExpr(rhs) {}

  static bool classof(const CobolASTNode *n) {
    return n->GetKind() == eAssignmentExpr;
  }

  const CobolASTExpr *GetlhsExpr() const { return m_lhsExpr.get(); }
  void SetlhsExpr(CobolASTExpr *expr) { m_lhsExpr.reset(expr); }

  const CobolASTExpr *GetrhsExpr() const { return m_rhsExpr.get(); }
  void SetrhsExpr(CobolASTExpr *expr) { m_rhsExpr.reset(expr); }

  const char *GetKindName() const override { return "assignment expression"; }

private:
  std::unique_ptr<CobolASTExpr> m_lhsExpr;
  std::unique_ptr<CobolASTExpr> m_rhsExpr;

  CobolASTAssignmentExpr(const CobolASTAssignmentExpr &) = delete;
  const CobolASTAssignmentExpr &
  operator=(const CobolASTAssignmentExpr &) = delete;
};

template <typename R, typename V> R CobolASTExpr::Visit(V *v) const {
  switch (GetKind()) {
  default:
    return R();
  case eBasicLit:
    return v->VisitBasicLit(llvm::cast<const CobolASTBasicLit>(this));
  case eIdent:
    return v->VisitIdent(llvm::cast<const CobolASTIdent>(this));
  case eUnaryExpr:
    return v->VisitUnaryExpr(llvm::cast<const CobolASTUnaryExpr>(this));
  case eSelectorExpr:
    return v->VisitSelectorExpr(llvm::cast<const CobolASTSelectorExpr>(this));
  case eRefModExpr:
    return v->VisitRefModExpr(llvm::cast<const CobolASTRefModifierExpr>(this));
  case eFuncCallExpr:
    return v->VisitFuncCallExpr(llvm::cast<const CobolASTFuncCallExpr>(this));
  case eAssignmentExpr:
    return v->VisitAssignmentExpr(
        llvm::cast<const CobolASTAssignmentExpr>(this));
  }
}

} // namespace lldb_private

#endif // liblldb_CobolAST_h
