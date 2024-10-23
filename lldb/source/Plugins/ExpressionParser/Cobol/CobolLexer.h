//===-- CobolLexer.h -----------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef liblldb_CobolLexer_h
#define liblldb_CobolLexer_h

#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/StringRef.h"

namespace lldb_private {

class CobolLexer {
public:
  explicit CobolLexer(const char *expr);

  enum TokenType {
    KW_OF,
    KW_MV,
    KW_SET,
    KW_TO,
    KW_IF,
    KW_IS,
    KW_EQUALS,
    KW_NOT,
    LIT_INTEGER,
    LIT_FLOAT,
    LIT_STRING,
    OP_AMP,
    OP_COLON,
    OP_COMMA,
    OP_DOT,
    OP_LPAREN,
    OP_LBRACK,
    OP_LBRACE,
    OP_MINUS,
    OP_PLUS,
    OP_RPAREN,
    OP_RBRACK,
    OP_RBRACE,
    OP_SEMICOLON,
    OP_STAR,
    OP_SIZEOF,
    OP_EQ,
    OP_COMPARE,
    OP_COMP_GE,
    OP_COMP_GT,
    OP_COMP_LE,
    OP_COMP_LT,
    TOK_EOF,
    TOK_INVALID,
    TOK_IDENTIFIER,
  };

  struct Token {
    explicit Token(TokenType TT, llvm::StringRef text)
        : m_type(TT), m_text(text) {}
    TokenType m_type;
    llvm::StringRef m_text;
    bool IsLiteral() const {
      return (m_type == LIT_INTEGER) || (m_type == LIT_FLOAT) ||
             (m_type == LIT_STRING);
    }
  };

  const Token &Lex();

  size_t BytesLeft() const { return m_end - m_start; }
  llvm::StringRef GetText(int len) const {
    return llvm::StringRef(m_start, len);
  }

  static TokenType LookupKeyword(llvm::StringRef KW);
  static llvm::StringRef LookupToken(TokenType TT);

private:
  bool IsDigit(char c) { return c >= '0' && c <= '9'; }
  bool IsHexChar(char c) {
    return IsDigit(c) || (c >= 'A' && c <= 'F') || (c >= 'a' && c <= 'f');
  }
  bool IsOperator(char c) {
    switch (c) {
    default:
      return false;
    case '&':
    case '*':
    case '+':
    case '-':
    case '[':
    case ']':
    case ':':
    case '(':
    case ')':
    case '=':
    case '>':
    case '<':
    case ',':
      break;
    }
    return true;
  }
  bool IsLetterOrDigit(char c) {
    if (IsDigit(c))
      return true;
    if (c >= 'a' && c <= 'z')
      return true;
    if (c >= 'A' && c <= 'Z')
      return true;
    switch (c) {
    default:
      break;
    case '_':
    case '-':
      return true;
    }
    return false;
  }
  bool IsWhiteSpace(char c) { return (c == ' ') || (c == '\t') || (c == '\r'); }
  void SkipWhiteSpace(bool &newline) {
    for (; m_start < m_end; ++m_start) {
      if (*m_start == '\n')
        newline = true;
      if (!IsWhiteSpace(*m_start))
        break;
    }
  }

  TokenType DoOperator();
  TokenType DoWord();
  TokenType DoString();
  TokenType DoNumber();
  TokenType DoIdent();

  using KeywordMap = llvm::StringMap<TokenType>;
  static const KeywordMap m_keywords;
  const char *m_start;
  const char *m_end;
  Token m_last_token;
};

} // namespace lldb_private

#endif // liblldb_CobolLexer_h
