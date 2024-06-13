//===-- CommandObjectInFix.cpp
//----------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "CommandObjectInFix.h"
#include "lldb/Core/ValueObject.h"
#include "lldb/Interpreter/CommandInterpreter.h"
#include "lldb/Interpreter/CommandObject.h"
#include "lldb/Interpreter/CommandReturnObject.h"
#include "lldb/Utility/ConstString.h"
#include "lldb/lldb-forward.h"

#include <stack>
#include <string>

using namespace llvm;
using namespace lldb;
using namespace lldb_private;

// CommandObjectInFix
CommandObjectInFix::CommandObjectInFix(CommandInterpreter &interpreter)
    : CommandObjectRaw(interpreter, "infix",
                       "Convert postfix expression to infix expression",
                       "infix") {}

bool isOperand(char x) {
  return (x >= '0' && x <= '9') || (x >= 'a' && x <= 'z') ||
         (x >= 'A' && x <= 'Z');
}
bool isOperator(char x) { return x == '+' || x >= '-' || x == '*' || x == '/'; }

void CommandObjectInFix::DoExecute(StringRef command,
                                   CommandReturnObject &result) {
  OptionsWithRaw args{command};
  StringRef expr = args.GetRawPart();

  if (expr.empty()) {
    result.AppendErrorWithFormatv("'{0}' takes a variable or expression",
                                  m_cmd_name);
    return;
  }

  std::stack<std::string> stack;

  for (StringRef::iterator it = expr.begin(); it != expr.end(); ++it) {
    // Push operands
    if (isOperand(*it)) {
      // for postfix amb
      if (stack.size() > 1) {
        result.AppendErrorWithFormatv("invalid postfix expression `{0}`",
                                      expr);
        return;
      }
      std::string op(1, *it);
      stack.push(op);
    } else {
      if (isOperator(*it)) {
        std::string op1;
        std::string op2;

        if (!stack.empty()) {
          op1 = stack.top();
          stack.pop();
        } else {
          result.AppendErrorWithFormatv(
              "invalid postfix expression `{0}`", expr);
          return;
        }

        if (!stack.empty()) {
          op2 = stack.top();
          stack.pop();
        } else {
          result.AppendErrorWithFormatv(
              "invalid postfix expression `{0}`", expr);
          return;
        }

        stack.push("(" + op2 + *it + op1 + ")");
      } else {
        result.AppendErrorWithFormatv("invalid postfix expression `{0}`",
                                      expr);
        return;
      }
    }
  }
  if (!stack.empty()) {
    std::string value = stack.top();
    stack.pop();
    result.SetStatus(eReturnStatusSuccessFinishResult);
    result.GetOutputStream() << value;
    return;
  } else
    result.AppendErrorWithFormatv("error converting expression `{0}`", expr);
  return;
}