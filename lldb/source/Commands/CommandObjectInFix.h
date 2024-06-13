//===-- CommandObjectInFix.h --------------------------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLDB_SOURCE_COMMANDS_COMMANDOBJECTINFIX_H
#define LLDB_SOURCE_COMMANDS_COMMANDOBJECTINFIX_H

#include "lldb/Interpreter/CommandObject.h"

namespace lldb_private {

// CommandObjectInFix

class CommandObjectInFix : public CommandObjectRaw {
public:
  CommandObjectInFix(CommandInterpreter &interpreter);
  ~CommandObjectInFix() override = default;


protected:
  void DoExecute(llvm::StringRef command, CommandReturnObject &result) override;
};

} // namespace lldb_private

#endif // LLDB_SOURCE_COMMANDS_COMMANDOBJECTINFIX_H
