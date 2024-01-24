//===--   PLILexer.cpp ------------------------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include <cstring>

#include "PLILexer.h"

using namespace lldb_private;
using namespace llvm;

const PLILexer::KeywordMap PLILexer::m_keywords = {
    {"&", OP_AMP},    {":", OP_COLON},    {"(", OP_LPAREN},
    {"[", OP_LBRACK}, {"{", OP_LBRACE},   {")", OP_RPAREN},
    {"]", OP_RBRACK}, {"}", OP_RBRACE},   {"*", OP_STAR},
    {"+", OP_PLUS},   {"-", OP_MINUS},    {".", OP_DOT},
    {"OF", KW_OF},    {"STG", OP_SIZEOF}, {"STORAGE", OP_SIZEOF},
    {"=", OP_EQ},     {">", OP_GT},       {"<", OP_LT},
    {">=", OP_GE},    {"<=", OP_LE},      {"~", OP_NOT},
    {"^", OP_NOT},    {"<>", OP_NE}};

PLILexer::PLILexer(const char *expr)
    : m_start(expr), m_end(expr + strlen(expr)), m_last_token(TOK_INVALID, "") {
}

const PLILexer::Token &PLILexer::Lex() {
  bool newline = false;
  SkipWhiteSpace(newline);

  if (m_start >= m_end) {
    m_last_token.m_text = llvm::StringRef("");
    m_last_token.m_type = TOK_EOF;
    return m_last_token;
  }

  const char *start = m_start;
  PLILexer::TokenType lastTok = m_last_token.m_type;
  m_last_token.m_type = TOK_INVALID;
  size_t offset = 0;
  if (newline) {
    switch (lastTok) {
    default:
      m_start++;
      start++;
      break;
    case TOK_IDENTIFIER:
    case LIT_FLOAT:
    case LIT_INTEGER:
    case LIT_STRING:
      m_last_token.m_text = llvm::StringRef("");
      m_last_token.m_type = OP_SEMICOLON;
      return m_last_token;
    }
  }

  if (IsDigit(*m_start))
    m_last_token.m_type = DoNumber();
  else if (*m_start == '-' && IsDigit(m_start[1]))
    m_last_token.m_type = DoNumber();
  else if (*m_start == '+' && IsDigit(m_start[1]))
    m_last_token.m_type = DoNumber();
  else if (IsOperator(*m_start))
    m_last_token.m_type = DoOperator();
  else {
    switch (*m_start) {
    // persistant variables
    case '$':
    default: {
      if (IsLetterOrDigit(*m_start))
        m_last_token.m_type = DoIdent();
    } break;
    case '.': {
      if (IsDigit(m_start[1]))
        m_last_token.m_type = DoNumber();
      else
        m_last_token.m_type = DoOperator();
    } break;
    case '"':
    case '`':
      m_last_token.m_type = DoString();
      // skip quotes
      start++;
      offset = 1;
      break;
    }
  }
  size_t length = m_start - start;
  if (length)
    length -= offset;
  m_last_token.m_text = llvm::StringRef(start, length);
  return m_last_token;
}

PLILexer::TokenType PLILexer::DoString() {
  if (*m_start == '`') {
    while (++m_start < m_end) {
      if (*m_start == '`')
        ++m_start;
      return LIT_STRING;
    }
    return TOK_INVALID;
  }

  while (++m_start < m_end) {
    switch (*m_start) {
    // We already have matched openning double quotation
    case '"':
      ++m_start;
      return LIT_STRING;
    case '\n':
      return TOK_INVALID;
    case '\\':
      if (m_start[1] == '\n')
        return TOK_INVALID;
      m_start++;
    }
  }
  return TOK_INVALID;
}

PLILexer::TokenType PLILexer::DoNumber() {
  if (m_start[0] == '0' && (m_start[1] == 'x' || m_start[1] == 'X')) {
    m_start += 2;
    while (IsHexChar(*m_start))
      m_start++;
    return LIT_INTEGER;
  }

  if (m_start[0] == '+' || m_start[0] == '-') {
    m_start++;
  }

  bool dot_ok = true;
  bool e_ok = true;
  while (true) {
    while (IsDigit(*m_start))
      ++m_start;
    switch (*m_start) {
    case '.':
      if (!dot_ok)
        return LIT_FLOAT;
      ++m_start;
      dot_ok = false;
      break;
    case 'e':
    case 'E':
      if (!e_ok)
        return LIT_FLOAT;
      dot_ok = e_ok = false;
      ++m_start;
      if (*m_start == '+' || *m_start == '-')
        ++m_start;
      break;
    default:
      if (dot_ok)
        return LIT_INTEGER;
      return LIT_FLOAT;
    }
  }
}

PLILexer::TokenType PLILexer::DoIdent() {
  const char *start = m_start++;
  while (m_start < m_end && IsLetterOrDigit(*m_start))
    ++m_start;
  TokenType kw = LookupKeyword(llvm::StringRef(start, m_start - start));
  if (kw != TOK_INVALID)
    return kw;
  return TOK_IDENTIFIER;
}

PLILexer::TokenType PLILexer::DoOperator() {
  size_t longestOp = (m_end - m_start);
  if (longestOp > 8)
    longestOp = 7;

  for (size_t i = longestOp; i > 0; --i) {
    TokenType TT = LookupKeyword(StringRef(m_start, i));
    if (TT != TOK_INVALID) {
      m_start += i;
      return TT;
    }
  }
  return TOK_INVALID;
}

PLILexer::TokenType PLILexer::DoWord() {
  const char *start = m_start;
  while ((m_start < m_end) && IsLetterOrDigit(*m_start))
    m_start++;

  TokenType TT = LookupKeyword(StringRef(m_start, m_start - start));
  return (TT == TOK_INVALID) ? TOK_IDENTIFIER : TT;
}

PLILexer::TokenType PLILexer::LookupKeyword(StringRef KW) {
  const auto &it = m_keywords.find(KW.upper());
  if (it != m_keywords.end())
    return it->second;
  return TOK_INVALID;
}

StringRef PLILexer::LookupToken(PLILexer::TokenType TT) {
  for (const auto &entry : m_keywords)
    if (entry.getValue() == TT)
      return entry.getKey();
  return "";
}
