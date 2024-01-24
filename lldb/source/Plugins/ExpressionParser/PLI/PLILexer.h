//===--   PLILexer.h --------------------------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef liblldb_PLILexer_h
#define liblldb_PLILexer_h

#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/StringRef.h"

namespace lldb_private {

class PLILexer {
public:
  explicit PLILexer(const char *expr);

  enum TokenType {
    KW_OF,
    LIT_INTEGER,
    LIT_FLOAT,
    LIT_STRING,
    OP_AMP,
    OP_COLON,
    OP_DOT,
    OP_EQ,
    OP_GE,
    OP_GT,
    OP_LPAREN,
    OP_LBRACK,
    OP_LBRACE,
    OP_LE,
    OP_LT,
    OP_MINUS,
    OP_NE,
    OP_NOT,
    OP_PLUS,
    OP_RPAREN,
    OP_RBRACK,
    OP_RBRACE,
    OP_SEMICOLON,
    OP_STAR,
    OP_SIZEOF,
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
    case '~':
    case '^':
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

#endif // liblldb_PLILexer_h
