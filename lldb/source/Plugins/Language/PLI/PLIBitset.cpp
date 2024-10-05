//===--    PLIBitset.cpp --------------------------------------------------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "PLILanguage.h"
#include "lldb/DataFormatters/FormattersHelpers.h"
#include "lldb/Target/Target.h"

#include "llvm/ADT/StringRef.h"

using namespace lldb;
using namespace lldb_private;

namespace {

class BitsetFrontEnd : public SyntheticChildrenFrontEnd {
public:
  BitsetFrontEnd(ValueObject &valobj);

  size_t GetIndexOfChildWithName(ConstString name) override {
    return formatters::ExtractIndexFromString(name.GetCString());
  }

  bool MightHaveChildren() override { return true; }
  lldb::ChildCacheState Update() override;
  llvm::Expected<uint32_t> CalculateNumChildren() override {
    return m_elements.size();
  }
  ValueObjectSP GetChildAtIndex(uint32_t idx) override;

private:
  std::vector<ValueObjectSP> m_elements;
  CompilerType m_bool_type;
  ByteOrder m_byte_order = eByteOrderInvalid;
  uint8_t m_byte_size = 0;
};

} // namespace

BitsetFrontEnd::BitsetFrontEnd(ValueObject &valobj)
    : SyntheticChildrenFrontEnd(valobj) {
  m_bool_type = valobj.GetCompilerType().GetBasicTypeFromAST(eBasicTypeBool);
  if (auto target_sp = m_backend.GetTargetSP()) {
    m_byte_order = target_sp->GetArchitecture().GetByteOrder();
    m_byte_size = target_sp->GetArchitecture().GetAddressByteSize();
    Update();
  }
}

lldb::ChildCacheState BitsetFrontEnd::Update() {
  m_elements.clear();

  TargetSP target_sp = m_backend.GetTargetSP();
  if (!target_sp)
    return lldb::ChildCacheState::eRefetch;
  uint32_t capping_size = target_sp->GetMaximumNumberOfChildrenToDisplay();

  if (!m_backend.IsArrayType())
    return lldb::ChildCacheState::eRefetch;

  const llvm::StringRef type_name(
      m_backend.GetCompilerType().GetTypeName().GetStringRef());
  const llvm::StringRef size_str = type_name.split('[').second.split(']').first;
  uint32_t num_elements = 0;
  if (size_str.getAsInteger(10, num_elements))
    num_elements = *m_backend.GetNumChildren(capping_size / 8) * 8;
  m_elements.assign(num_elements, ValueObjectSP());
  return lldb::ChildCacheState::eRefetch;
}

ValueObjectSP BitsetFrontEnd::GetChildAtIndex(uint32_t idx) {
  if (idx >= m_elements.size())
    return ValueObjectSP();

  if (m_elements[idx])
    return m_elements[idx];

  size_t chunk_idx = idx / 8;
  size_t bit_pos = 7 - (idx % 8); // we have zero bit on 7th pos
  m_elements[idx] = m_backend.GetChildAtIndex(chunk_idx, true)
                        ->GetSyntheticBitFieldChild(bit_pos, bit_pos, true);
  m_elements[idx]->SetName(ConstString(llvm::formatv("[{0}]", idx).str()));
  return m_elements[idx];
}

SyntheticChildrenFrontEnd *
formatters::PLIBitsetSyntheticFrontEndCreator(CXXSyntheticChildren *,
                                              lldb::ValueObjectSP valobj_sp) {
  if (valobj_sp)
    return new BitsetFrontEnd(*valobj_sp);
  return nullptr;
}
