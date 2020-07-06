//===--   PLIParser.cpp -----------------------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "Plugins/ExpressionParser/PLI/PLIParser.h"
#include "Plugins/ExpressionParser/PLI/PLIAST.h"
#include "lldb/Utility/Status.h"

using namespace lldb_private;
using namespace llvm;

PLIParser::PLIParser(const char *expr)
    : m_lexer(expr), m_pos(0), m_last_tok(PLILexer::TOK_INVALID),
      m_failed(false) {}

void PLIParser::GetError(Status &error) {
  if (!m_failed)
    return;

  size_t rem = m_lexer.BytesLeft();
  error.SetErrorStringWithFormat(
      "Syntex error: expected with bytes remaining %lu.", rem);
}

PLIASTStmt *PLIParser::Statement() {
  Rule r("Statement", this);

  // TODO Add other rules
  PLIASTExpr *expr = Expression();
  if (!expr)
    return r.error();

  if (auto ret = ExpressionStmt(expr))
    return ret;
  return r.error();
}

PLIASTStmt *PLIParser::ExpressionStmt(PLIASTExpr *expr) {
  Rule r("ExpressionStmt", this);
  if (Semicolon())
    return new PLIASTExprStmt(expr);
  return r.error();
}

PLIASTExpr *PLIParser::Expression() {
  Rule r("Expression", this);
  if (PLIASTExpr *result = UnaryExpr())
    return result;
  return r.error();
}

PLIASTExpr *PLIParser::PrimaryExpr() {
  PLIASTExpr *l = Operand();
  PLIASTExpr *r;
  while ((r = Selector(l)) || (r = SelectorOf(l)) || (r = RefModifier(l)) ||
         (r = Assignment(l)))
    l = r;
  return l;
}

PLIASTExpr *PLIParser::Operand() {
  PLILexer::Token *lit;
  if ((lit = match(PLILexer::LIT_INTEGER)) ||
      (lit = match(PLILexer::LIT_FLOAT)) || (lit = match(PLILexer::LIT_STRING)))
    return new PLIASTBasicLit(*lit);
  return Identifier();
}

PLIASTIdent *PLIParser::Identifier() {
  if (auto *tok = match(PLILexer::TOK_IDENTIFIER))
    return new PLIASTIdent(*tok);
  return nullptr;
}

PLIASTExpr *PLIParser::UnaryExpr() {
  switch (peek()) {
  default:
    return PrimaryExpr();
  case PLILexer::OP_AMP:
  case PLILexer::OP_STAR: {
    const PLILexer::Token t = next();
    if (PLIASTExpr *expr = UnaryExpr()) {
      return new PLIASTUnaryExpr(t.m_type, expr);
    }
  } break;
  case PLILexer::OP_SIZEOF: {
    next(); // Flush operator token
    if (!match(PLILexer::OP_LPAREN))
      return nullptr;
    auto func =
        new PLIASTFuncCallExpr(PLILexer::Token(PLILexer::OP_SIZEOF, "sizeof"));
    func->addParam(PrimaryExpr());
    if (!match(PLILexer::OP_RPAREN))
      return nullptr;
    return func;
  }
  }
  return nullptr;
}

PLIASTExpr *PLIParser::SelectorOf(PLIASTExpr *expr) {
  if (match(PLILexer::KW_OF)) {
    if (auto *e = PrimaryExpr()) {
      auto sel = llvm::cast<PLIASTIdent>(expr);
      return new PLIASTSelectorExpr(e, sel);
    }
  }
  return nullptr;
}

PLIASTExpr *PLIParser::Selector(PLIASTExpr *expr) {
  if (match(PLILexer::OP_DOT)) {
    if (auto *name = Identifier())
      return new PLIASTSelectorExpr(expr, name);
  }
  return nullptr;
}

PLIASTExpr *PLIParser::RefModifier(PLIASTExpr *expr) {
  Rule r("MoveRef", this);
  PLIASTExpr *start = nullptr;
  PLIASTExpr *len = nullptr;
  if (match(PLILexer::OP_LPAREN)) {
    start = PrimaryExpr();
    PLILexer::Token t = next();
    if (t.m_type == PLILexer::OP_COLON) {
      len = PrimaryExpr();
      t = next();
    }
    if (t.m_type != PLILexer::OP_RPAREN)
      return r.error();
  }

  if (start == nullptr)
    return nullptr;

  return new PLIASTRefModifierExpr(expr, start, len);
}

PLIASTExpr *PLIParser::FuncCall(PLIASTExpr *expr) {
  Rule r("funcCall", this);
  if (match(PLILexer::OP_SIZEOF)) {
    if (!match(PLILexer::OP_LPAREN))
      return r.error();
    auto func =
        new PLIASTFuncCallExpr(PLILexer::Token(PLILexer::OP_SIZEOF, "sizeof"));
    func->addParam(PrimaryExpr());
    if (!match(PLILexer::OP_RPAREN))
      return r.error();
    return func;
  }
  return nullptr;
}

PLIASTExpr *PLIParser::Assignment(PLIASTExpr *expr) {
  Rule r("assignment", this);
  if (match(PLILexer::OP_EQ)) {
    PLIASTExpr *rhsVal = PrimaryExpr();
    if (!rhsVal)
      return nullptr;
    return new PLIASTAssignmentExpr(expr, rhsVal);
  }
  return nullptr;
}

bool PLIParser::Semicolon() {
  if (match(PLILexer::OP_SEMICOLON))
    return true;

  switch (peek()) {
  default:
    break;
  case PLILexer::TOK_EOF:
    return true;
  }
  return false;
}
