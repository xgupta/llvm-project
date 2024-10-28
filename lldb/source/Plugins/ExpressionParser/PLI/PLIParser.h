//===--   PLIParser.h -------------------------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef liblldb_PLIParser_h_
#define liblldb_PLIParser_h_

#include "Plugins/ExpressionParser/PLI/PLIAST.h"
#include "Plugins/ExpressionParser/PLI/PLILexer.h"
#include "lldb/lldb-private.h"
#include "llvm/ADT/StringRef.h"

namespace lldb_private {

class PLIParser {
public:
  explicit PLIParser(const char *expr);

  PLIASTStmt *Statement();

  bool Failed() const { return m_failed; }
  bool AtEOF() const {
    return m_lexer.BytesLeft() == 0 && m_pos == m_tokens.size();
  }
  void GetError(Status &error);

private:
  class Rule {
  public:
    Rule(llvm::StringRef name, PLIParser *p)
        : m_name(name), m_parser(p), m_pos(p->m_pos) {}

    std::nullptr_t error() {
      if (!m_parser->m_failed) {
        // Set m_error in case this is the top level.
        if (m_parser->m_last_tok == PLILexer::TOK_INVALID)
          // And set m_last in case it isn't.
          m_parser->m_last_tok = PLILexer::TOK_INVALID;
        m_parser->m_pos = m_pos;
      }
      return nullptr;
    }

  private:
    llvm::StringRef m_name;
    PLIParser *m_parser;
    size_t m_pos;
  };
  friend class Rule;

  PLILexer::Token &next() {
    if (m_pos >= m_tokens.size()) {
      if (m_pos && (m_tokens.back().m_type == PLILexer::TOK_EOF ||
                    m_tokens.back().m_type == PLILexer::TOK_INVALID))
        return m_tokens.back();

      m_pos = m_tokens.size();
      m_tokens.push_back(m_lexer.Lex());
    }
    return m_tokens[m_pos++];
  }

  PLILexer::TokenType peek() {
    PLILexer::Token &tok = next();
    --m_pos;
    return tok.m_type;
  }

  PLILexer::Token *match(PLILexer::TokenType TT) {
    PLILexer::Token &tok = next();
    if (tok.m_type == TT)
      return &tok;
    --m_pos;
    m_last_tok = TT;
    return nullptr;
  }
  bool Semicolon();
  PLIASTIdent *Identifier();
  PLIASTStmt *ExpressionStmt(PLIASTExpr *expr);
  PLIASTExpr *Expression();
  PLIASTExpr *PrimaryExpr();
  PLIASTExpr *Operand();
  PLIASTExpr *UnaryExpr();
  PLIASTExpr *Selector(PLIASTExpr *expr);
  PLIASTExpr *SelectorOf(PLIASTExpr *expr);
  PLIASTExpr *RefModifier(PLIASTExpr *expr);
  PLIASTExpr *FuncCall(PLIASTExpr *expr);
  PLIASTExpr *Assignment(PLIASTExpr *expr);

  PLILexer m_lexer;
  std::vector<PLILexer::Token> m_tokens;
  size_t m_pos;
  PLILexer::TokenType m_last_tok;
  llvm::StringRef m_error;
  bool m_failed;
};

} // namespace lldb_private

#endif // liblldb_PLIParser_h_
