//===-- CommandObjectPostFix.cpp ------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "CommandObjectPostFix.h"
#include "lldb/Core/ValueObject.h"
#include "lldb/Interpreter/CommandInterpreter.h"
#include "lldb/Interpreter/CommandObject.h"
#include "lldb/Interpreter/CommandReturnObject.h"
#include "lldb/Utility/ConstString.h"
#include "lldb/lldb-forward.h"

#include <stack>

using namespace llvm;
using namespace lldb;
using namespace lldb_private;

// CommandObjectPostFix
CommandObjectPostFix::CommandObjectPostFix(CommandInterpreter &interpreter)
    : CommandObjectRaw(interpreter, "postfix",
                       "Evaluate the postfix expression", "postfix") {}

void CommandObjectPostFix::DoExecute(StringRef command,
                                     CommandReturnObject &result) {
  OptionsWithRaw args{command};
  StringRef expr = args.GetRawPart();

  if (expr.empty()) {
    result.AppendErrorWithFormatv("'{0}' takes a variable or expression",
                                  m_cmd_name);
    return;
  }

  std::stack<int> stack;

  // Scan all characters one by one
  for (StringRef::iterator it = expr.begin(); it != expr.end(); ++it) {
    if (isdigit(*it))
      // for postfix 123
      if (stack.size() > 1) {
        result.AppendErrorWithFormatv("invalid postfix expression `{0}`", expr);
        return;
      } else {
        stack.push(*it - '0');
      }
    else {
      int val1;
      int val2;
      if (!stack.empty()) {
        val1 = stack.top();
        stack.pop();
      } else {
        result.AppendErrorWithFormatv("invalid postfix expression `{0}`", expr);
        return;
      }

      if (!stack.empty()) {
        val2 = stack.top();
        stack.pop();
      } else {
        result.AppendErrorWithFormatv("invalid postfix expression `{0}`", expr);
        return;
      }

      switch (*it) {
      case '+':
        stack.push(val2 + val1);
        break;
      case '-':
        stack.push(val2 - val1);
        break;
      case '*':
        stack.push(val2 * val1);
        break;
      case '/':
        if (val1 == 0) {
          result.AppendErrorWithFormatv(
              "divide by zero in postfix expression `{0}`", expr);
          return;
        }
        stack.push(val2 / val1);
        break;
      }
    }
  }

  if (!stack.empty()) {
    int value = stack.top();
    stack.pop();
    result.SetStatus(eReturnStatusSuccessFinishResult);
    result.GetOutputStream().Printf("%d", value);
    return;
  } else
    result.AppendErrorWithFormatv("error evaluating expression `{0}`", expr);
  return;
}
