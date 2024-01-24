//===-- CobolParser.cpp -----------------------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "Plugins/ExpressionParser/Cobol/CobolParser.h"
#include "Plugins/ExpressionParser/Cobol/CobolAST.h"
#include "lldb/Utility/Status.h"

using namespace lldb_private;
using namespace llvm;

CobolParser::CobolParser(const char *expr)
    : m_lexer(expr), m_pos(0), m_last_tok(CobolLexer::TOK_INVALID),
      m_failed(false) {}

void CobolParser::GetError(Status &error) {
  if (!m_failed)
    return;

  size_t rem = m_lexer.BytesLeft();
  error = Status::FromErrorStringWithFormat(
      "Syntex error: expected with bytes remaining %lu.", rem);
}

CobolASTStmt *CobolParser::Statement() {
  Rule r("Statement", this);

  // TODO Add other rules
  CobolASTExpr *expr = Expression();
  if (!expr)
    return r.error();

  if (auto ret = ExpressionStmt(expr))
    return ret;
  return r.error();
}

CobolASTStmt *CobolParser::ExpressionStmt(CobolASTExpr *expr) {
  Rule r("ExpressionStmt", this);
  if (Semicolon())
    return new CobolASTExprStmt(expr);
  return r.error();
}

CobolASTExpr *CobolParser::Expression() {
  Rule r("Expression", this);
  if (CobolASTExpr *result = UnaryExpr())
    return result;
  return r.error();
}

CobolASTExpr *CobolParser::PrimaryExpr() {
  CobolASTExpr *l = Operand();
  CobolASTExpr *r;
  while ((r = Selector(l)) || (r = SelectorOf(l)) || (r = RefModifier(l)) ||
         (r = Assignment(l)) || (r = Compare(l)))
    l = r;
  return l;
}

CobolASTExpr *CobolParser::Operand() {
  CobolLexer::Token *lit;
  if ((lit = match(CobolLexer::LIT_INTEGER)) ||
      (lit = match(CobolLexer::LIT_FLOAT)) ||
      (lit = match(CobolLexer::LIT_STRING)))
    return new CobolASTBasicLit(*lit);
  return Identifier();
}

CobolASTIdent *CobolParser::Identifier() {
  if (auto *tok = match(CobolLexer::TOK_IDENTIFIER)) {
    return new CobolASTIdent(*tok);
  }
  return nullptr;
}

CobolASTExpr *CobolParser::UnaryExpr() {
  switch (peek()) {
  default:
    return PrimaryExpr();
  case CobolLexer::OP_AMP:
  case CobolLexer::OP_STAR: {
    const CobolLexer::Token t = next();
    if (CobolASTExpr *expr = UnaryExpr()) {
      return new CobolASTUnaryExpr(t.m_type, expr);
    }
  } break;
  case CobolLexer::OP_SIZEOF: {
    next(); // Flush operator token
    if (!match(CobolLexer::OP_LPAREN))
      return nullptr;
    auto func = new CobolASTFuncCallExpr(
        CobolLexer::Token(CobolLexer::OP_SIZEOF, "sizeof"));
    func->addParam(PrimaryExpr());
    if (!match(CobolLexer::OP_RPAREN))
      return nullptr;
    return func;
  }
  }
  return nullptr;
}

CobolASTExpr *CobolParser::ModifySelectorASTTree(CobolASTExpr *var,
                                                 CobolASTExpr *mem) {
  switch (llvm::cast<CobolASTNode>(mem)->GetKind()) {
  case CobolASTNode::eRefModExpr: {
    auto leftSel = llvm::cast<CobolASTRefModifierExpr>(mem);
    auto lSel = ModifySelectorASTTree(
        var, const_cast<CobolASTExpr *>(leftSel->GetExpr()));
    leftSel->ReplaceExpr(lSel);
    return leftSel;
  }
  case CobolASTNode::eSelectorExpr: {
    auto leftSel = llvm::cast<CobolASTSelectorExpr>(mem);
    auto lSel = ModifySelectorASTTree(
        var, const_cast<CobolASTExpr *>(leftSel->GetExpr()));
    leftSel->ReplaceExpr(lSel);
    return leftSel;
  }
  case CobolASTNode::eIdent: {
    auto iIdent = llvm::cast<CobolASTIdent>(mem);
    return new CobolASTSelectorExpr(var, iIdent);
  }
  default:
    break;
  }
  return nullptr;
}

CobolASTExpr *CobolParser::SelectorOf(CobolASTExpr *expr) {
  if (match(CobolLexer::KW_OF)) {
    if (auto *e = PrimaryExpr()) {
      return ModifySelectorASTTree(e, expr);
    }
  }
  return nullptr;
}

CobolASTExpr *CobolParser::Selector(CobolASTExpr *expr) {
  if (match(CobolLexer::OP_DOT)) {
    if (auto *name = Identifier())
      return new CobolASTSelectorExpr(expr, name);
  }
  return nullptr;
}

CobolASTExpr *CobolParser::Indices() {
  Rule r("Indices", this);
  std::vector<std::unique_ptr<CobolASTExpr>> indices_vec;

  while (true) {
    indices_vec.push_back(std::unique_ptr<CobolASTExpr>(PrimaryExpr()));
    auto next_type = peek();
    if (next_type != CobolLexer::OP_COMMA)
      break;
    next();
  }

  return new CobolASTIndexExpr(std::move(indices_vec));
}

CobolASTExpr *CobolParser::RefModifier(CobolASTExpr *expr) {
  Rule r("MoveRef", this);
  CobolASTExpr *start = nullptr;
  CobolASTExpr *len = nullptr;
  if (match(CobolLexer::OP_LPAREN)) {
    start = Indices();
    CobolLexer::Token t = next();
    if (t.m_type == CobolLexer::OP_COLON) {
      len = PrimaryExpr();
      t = next();
    }
    if (t.m_type != CobolLexer::OP_RPAREN)
      return r.error();
  }

  if (start == nullptr)
    return nullptr;

  return new CobolASTRefModifierExpr(expr, start, len);
}

CobolASTExpr *CobolParser::FuncCall(CobolASTExpr *expr) {
  Rule r("funcCall", this);
  if (match(CobolLexer::OP_SIZEOF)) {
    if (!match(CobolLexer::OP_LPAREN))
      return r.error();
    auto func = new CobolASTFuncCallExpr(
        CobolLexer::Token(CobolLexer::OP_SIZEOF, "sizeof"));
    func->addParam(PrimaryExpr());
    if (!match(CobolLexer::OP_RPAREN))
      return r.error();
    return func;
  }
  return nullptr;
}

CobolASTExpr *CobolParser::Compare(CobolASTExpr *expr) {
  Rule r("Compare", this);
  bool is_not = false;
  if (match(CobolLexer::KW_IS)) {
    CobolLexer::Token t = next();
    if (t.m_type == CobolLexer::KW_NOT) {
      is_not = true;
      t = next();
    }

    auto rhsVal = PrimaryExpr();
    switch (t.m_type) {
    default:
      break;
    case CobolLexer::OP_COMP_GE:
    case CobolLexer::OP_COMP_GT:
    case CobolLexer::OP_COMP_LE:
    case CobolLexer::OP_COMP_LT:
    case CobolLexer::OP_EQ:
      return new CobolASTCompareExpr(expr, rhsVal, t.m_type, is_not);
    }
  }
  return nullptr;
}

CobolASTExpr *CobolParser::Assignment(CobolASTExpr *expr) {
  Rule r("Assignment", this);
  if (match(CobolLexer::KW_MV)) {
    auto rhsVal = PrimaryExpr();
    if (rhsVal == nullptr)
      return nullptr;
    if (!match(CobolLexer::KW_TO))
      return nullptr;
    auto lhsRef = PrimaryExpr();
    if (lhsRef == nullptr)
      return nullptr;
    return new CobolASTAssignmentExpr(lhsRef, rhsVal);
  } else if (match(CobolLexer::KW_SET)) {
    auto lhsRef = PrimaryExpr();
    if (lhsRef == nullptr)
      return nullptr;
    if (!match(CobolLexer::KW_TO))
      return nullptr;
    auto rhsVal = PrimaryExpr();
    if (rhsVal == nullptr)
      return nullptr;
    return new CobolASTAssignmentExpr(lhsRef, rhsVal);
  }
  return nullptr;
}

bool CobolParser::Semicolon() {
  if (match(CobolLexer::OP_SEMICOLON))
    return true;

  switch (peek()) {
  default:
    break;
  case CobolLexer::TOK_EOF:
    return true;
  }
  return false;
}
