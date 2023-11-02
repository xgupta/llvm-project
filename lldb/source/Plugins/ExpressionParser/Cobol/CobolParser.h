//===-- CobolParser.h -------------------------------------------*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#ifndef liblldb_CobolParser_h_
#define liblldb_CobolParser_h_

#include "Plugins/ExpressionParser/Cobol/CobolAST.h"
#include "Plugins/ExpressionParser/Cobol/CobolLexer.h"
#include "lldb/lldb-private.h"
#include "llvm/ADT/StringRef.h"

namespace lldb_private {

class CobolParser {
public:
  explicit CobolParser(const char *expr);

  CobolASTStmt *Statement();

  bool Failed() const { return m_failed; }
  bool AtEOF() const {
    return m_lexer.BytesLeft() == 0 && m_pos == m_tokens.size();
  }
  void GetError(Status &error);

private:
  class Rule {
  public:
    Rule(llvm::StringRef name, CobolParser *p)
        : m_name(name), m_parser(p), m_pos(p->m_pos) {}

    std::nullptr_t error() {
      if (!m_parser->m_failed) {
        // Set m_error in case this is the top level.
        if (m_parser->m_last_tok == CobolLexer::TOK_INVALID)
          m_parser->m_error =
              llvm::StringRef("Error with Rule " + m_name.str());
        // And set m_last in case it isn't.
        m_parser->m_last_tok = CobolLexer::TOK_INVALID;
        m_parser->m_pos = m_pos;
      }
      return nullptr;
    }

  private:
    llvm::StringRef m_name;
    CobolParser *m_parser;
    size_t m_pos;
  };
  friend class Rule;

  CobolLexer::Token &next() {
    if (m_pos >= m_tokens.size()) {
      if (m_pos && (m_tokens.back().m_type == CobolLexer::TOK_EOF ||
                    m_tokens.back().m_type == CobolLexer::TOK_INVALID))
        return m_tokens.back();

      m_pos = m_tokens.size();
      m_tokens.push_back(m_lexer.Lex());
    }
    return m_tokens[m_pos++];
  }

  CobolLexer::TokenType peek() {
    CobolLexer::Token &tok = next();
    --m_pos;
    return tok.m_type;
  }

  CobolLexer::Token *match(CobolLexer::TokenType TT) {
    CobolLexer::Token &tok = next();
    if (tok.m_type == TT)
      return &tok;
    --m_pos;
    m_last_tok = TT;
    return nullptr;
  }
  bool Semicolon();
  CobolASTIdent *Identifier();
  CobolASTStmt *ExpressionStmt(CobolASTExpr *expr);
  CobolASTExpr *Expression();
  CobolASTExpr *PrimaryExpr();
  CobolASTExpr *Operand();
  CobolASTExpr *UnaryExpr();
  CobolASTExpr *ModifySelectorASTTree(CobolASTExpr *var, CobolASTExpr *mem);
  CobolASTExpr *Selector(CobolASTExpr *expr);
  CobolASTExpr *SelectorOf(CobolASTExpr *expr);
  CobolASTExpr *RefModifier(CobolASTExpr *expr);
  CobolASTExpr *FuncCall(CobolASTExpr *expr);
  CobolASTExpr *Assignment(CobolASTExpr *expr);
  CobolASTExpr *Compare(CobolASTExpr *expr);
  CobolASTExpr *Indices();

  CobolLexer m_lexer;
  std::vector<CobolLexer::Token> m_tokens;
  size_t m_pos;
  CobolLexer::TokenType m_last_tok;
  llvm::StringRef m_error;
  bool m_failed;
};

} // namespace lldb_private

#endif // liblldb_CobolParser_h_
