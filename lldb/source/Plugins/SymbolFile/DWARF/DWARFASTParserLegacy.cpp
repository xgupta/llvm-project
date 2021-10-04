//===-- DWARFASTParserLegacy.cpp ---------------------------------*- C++
//-*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "DWARFASTParserLegacy.h"

#include "DWARFDIE.h"
#include "DWARFDebugInfo.h"
#include "DWARFDeclContext.h"
#include "DWARFDefines.h"
#include "SymbolFileDWARF.h"
#include "SymbolFileDWARFDebugMap.h"
#include "UniqueDWARFASTType.h"

#include "lldb/Core/Module.h"
#include "lldb/Core/Value.h"
#include "lldb/Symbol/CompileUnit.h"
#include "lldb/Symbol/Function.h"
#include "lldb/Symbol/ObjectFile.h"
#include "lldb/Symbol/TypeList.h"

#include "Plugins/SymbolFile/DWARF/DWARFASTParserLegacy.h"

#include <memory>

//#define ENABLE_DEBUG_PRINTF // COMMENT OUT THIS LINE PRIOR TO CHECKIN

#ifdef ENABLE_DEBUG_PRINTF
#include <stdio.h>
#define DEBUG_PRINTF(fmt, ...) printf(fmt, __VA_ARGS__)
#else
#define DEBUG_PRINTF(fmt, ...)
#endif

using namespace lldb;
using namespace lldb_private;

DWARFASTParserLegacy::DWARFASTParserLegacy(TypeSystemLegacy &ast)
    : m_ast(ast) {}
DWARFASTParserLegacy::~DWARFASTParserLegacy() {}

TypeSP
DWARFASTParserLegacy::ParseTypeFromDWARF(const lldb_private::SymbolContext &sc,
                                         const DWARFDIE &die,
                                         bool *type_is_new_ptr) {
  TypeSP type_sp;

  if (type_is_new_ptr)
    *type_is_new_ptr = false;

  Log *log(LogChannelDWARF::GetLogIfAny(DWARF_LOG_TYPE_COMPLETION |
                                        DWARF_LOG_LOOKUPS));
  if (die) {
    SymbolFileDWARF *dwarf = die.GetDWARF();
    if (log) {
      dwarf->GetObjectFile()->GetModule()->LogMessage(
          log,
          "DWARFASTParserLegacy::ParseTypeFromDWARF (die = 0x%8.8x) %s name = "
          "'%s')",
          die.GetOffset(), DW_TAG_value_to_name(die.Tag()), die.GetName());
    }

    Type *type_ptr = dwarf->m_die_to_type.lookup(die.GetDIE());

    if (!type_ptr) {
      if (type_is_new_ptr)
        *type_is_new_ptr = true;

      const dw_tag_t tag = die.Tag();
      CompilerType compiler_type;
      DWARFAttributes attributes;
      DWARFFormValue form_value;
      dw_attr_t attr;
      Declaration decl;
      const char *type_name_cstr = nullptr;
      const char *pic_cstr = nullptr;
      ConstString type_name_const_str;
      ConstString pic_const_str;
      ByteOrder byte_order = endian::InlHostByteOrder();
      Type::ResolveState resolve_state = Type::ResolveState::Unresolved;
      Type::EncodingDataType encoding_data_type = Type::eEncodingIsUID;

      switch (tag) {
      case DW_TAG_pointer_type:
      case DW_TAG_reference_type:
      case DW_TAG_base_type: {
        dwarf->m_die_to_type[die.GetDIE()] = DIE_IS_BEING_PARSED;
        DWARFFormValue encoding_uid;
        uint32_t bit_size = 0;
        uint32_t encoding = 0;
        uint32_t sign = 0;
        uint32_t digit_count = 0;
        int32_t scale = 0;
        bool bin_scale = false;

        const size_t num_attributes = die.GetAttributes(attributes);
        for (size_t i = 0; i < num_attributes; ++i) {
          attr = attributes.AttributeAtIndex(i);
          if (attributes.ExtractFormValueAtIndex(i, form_value)) {
            switch (attr) {
            case DW_AT_decl_file:
              decl.SetFile(sc.comp_unit->GetSupportFiles().GetFileSpecAtIndex(
                  form_value.Unsigned()));
              break;
            case DW_AT_decl_line:
              decl.SetLine(form_value.Unsigned());
              break;
            case DW_AT_decl_column:
              decl.SetColumn(form_value.Unsigned());
              break;
            case DW_AT_name:
              type_name_cstr = form_value.AsCString();
              if (type_name_cstr)
                type_name_const_str.SetCString(type_name_cstr);
              break;
            case DW_AT_byte_size:
              bit_size = form_value.Unsigned() * 8;
              break;
            case DW_AT_bit_size:
              bit_size = form_value.Unsigned();
              break;
            case DW_AT_encoding:
              encoding = form_value.Unsigned();
              break;
            case DW_AT_endianity:
              switch (form_value.Unsigned()) {
              case DW_END_big:
                byte_order = eByteOrderBig;
                break;
              case DW_END_little:
                byte_order = eByteOrderLittle;
                break;
              }
              break;
            case DW_AT_decimal_sign:
              sign = form_value.Unsigned();
              break;
            case DW_AT_decimal_scale:
              scale = form_value.Signed();
              break;
            case DW_AT_binary_scale:
              scale = form_value.Signed();
              bin_scale = true;
              break;
            case DW_AT_type:
              encoding_uid = form_value;
              break;
            case DW_AT_picture_string:
              pic_cstr = form_value.AsCString();
              if (pic_cstr)
                pic_const_str.SetCString(pic_cstr);
              break;
            case DW_AT_digit_count:
              digit_count = form_value.Unsigned();
              break;
            default:
              break;
            }
          }
        }

        DEBUG_PRINTF("0x%8.8" PRIx64 ": %s (\"%s\") type => 0x%8.8lx\n",
                     die.GetID(), DW_TAG_value_to_name(tag), type_name_cstr,
                     encoding_uid.Reference());

        switch (tag) {
        default:
        case DW_TAG_base_type:
          compiler_type = m_ast.CreateBaseType(
              type_name_const_str, pic_const_str, encoding, bit_size, sign,
              scale, digit_count, byte_order, bin_scale);
          resolve_state = Type::ResolveState::Full;
          break;
        case DW_TAG_pointer_type:
          encoding_data_type = Type::eEncodingIsPointerUID;
          break;
        case DW_TAG_reference_type:
          encoding_data_type = Type::eEncodingIsLValueReferenceUID;
          break;
        }

        type_sp = std::make_shared<Type>(
            die.GetID(), dwarf, type_name_const_str, (bit_size + 7) / 8,
            nullptr, dwarf->GetUID(encoding_uid.Reference()),
            encoding_data_type, &decl, compiler_type, resolve_state);

        assert(type_sp.get());
      } break;
      case DW_TAG_array_type: {
        // Set a bit that lets us know that we are currently parsing this
        dwarf->GetDIEToType()[die.GetDIE()] = DIE_IS_BEING_PARSED;

        DWARFFormValue type_die_form;
        uint32_t byte_stride = 0;
        uint32_t bit_stride = 0;
        bool isVarString = false;

        const size_t num_attributes = die.GetAttributes(attributes);
        for (size_t i = 0; i < num_attributes; ++i) {
          attr = attributes.AttributeAtIndex(i);
          if (attributes.ExtractFormValueAtIndex(i, form_value)) {
            switch (attr) {
            case DW_AT_decl_file:
              decl.SetFile(sc.comp_unit->GetSupportFiles().GetFileSpecAtIndex(
                  form_value.Unsigned()));
              break;
            case DW_AT_decl_line:
              decl.SetLine(form_value.Unsigned());
              break;
            case DW_AT_decl_column:
              decl.SetColumn(form_value.Unsigned());
              break;
            case DW_AT_name:
              type_name_cstr = form_value.AsCString();
              type_name_const_str.SetCString(type_name_cstr);
              break;
            case DW_AT_type:
              type_die_form = form_value;
              break;
            case DW_AT_byte_stride:
              byte_stride = form_value.Unsigned();
              break;
            case DW_AT_bit_stride:
              bit_stride = form_value.Unsigned();
              break;
            case DW_AT_RAINCODE_str_header:
              isVarString = true;
              break;
            default:
              break;
            }
          }
        }

        DEBUG_PRINTF("0x%8.8" PRIx64 ": %s (\"%s\")\n", die.GetID(),
                     DW_TAG_value_to_name(tag), type_name_cstr);

        Type *element_type =
            dwarf->ResolveTypeUID(type_die_form.Reference(), true);

        if (element_type) {
          auto array_info = ParseChildArrayInfo(die);
          if (array_info) {
            byte_stride = array_info->byte_stride;
            bit_stride = array_info->bit_stride;
          }

          if ((byte_stride == 0) && (bit_stride == 0))
            byte_stride = element_type->GetByteSize(nullptr).getValueOr(0);

          ConstString empty_name;
          CompilerType array_element_type =
              element_type->GetForwardCompilerType();
          if (array_element_type.GetCompleteType()) {
            uint64_t array_element_bit_stride = byte_stride * 8 + bit_stride;

            if (array_info && array_info->element_orders.size() > 0) {
              auto end = array_info->element_orders.rend();
              uint64_t num_elements = 0;
              for (auto pos = array_info->element_orders.rbegin(); pos != end;
                   ++pos) {
                num_elements = *pos;
                compiler_type = m_ast.CreateArrayType(
                    type_name_const_str, empty_name, array_element_type,
                    num_elements, isVarString);
                array_element_type = compiler_type;
                if (num_elements)
                  array_element_bit_stride *= num_elements;
              }
            } else
              compiler_type =
                  m_ast.CreateArrayType(type_name_const_str, empty_name,
                                        array_element_type, 0, isVarString);

            type_sp = std::make_shared<Type>(
                die.GetID(), dwarf, empty_name, array_element_bit_stride / 8,
                nullptr, dwarf->GetUID(type_die_form.Reference()),
                Type::eEncodingIsUID, &decl, compiler_type,
                Type::ResolveState::Full);

            type_sp->SetEncodingType(element_type);
          } else
            dwarf->GetObjectFile()->GetModule()->LogMessage(
                log,
                "DWARFASTParserLegacy::ParseTypeFromDWARF (die = 0x%8.8x) %s "
                "name "
                "= '%s'), incomplete type array element not supported, yet!.",
                die.GetOffset(), DW_TAG_value_to_name(die.Tag()),
                die.GetName());
        }
      } break;
      case DW_TAG_union_type:
      case DW_TAG_structure_type: {
        dwarf->m_die_to_type[die.GetDIE()] = DIE_IS_BEING_PARSED;
        bool byte_size_valid = false;
        unsigned byte_size = 0;

        const size_t num_attributes = die.GetAttributes(attributes);
        for (size_t i = 0; i < num_attributes; ++i) {
          attr = attributes.AttributeAtIndex(i);
          if (attributes.ExtractFormValueAtIndex(i, form_value)) {
            switch (attr) {
            default:
              break;
            case DW_AT_name:
              type_name_cstr = form_value.AsCString();
              type_name_const_str.SetCString(type_name_cstr);
              break;
            case DW_AT_byte_size:
              byte_size = form_value.Unsigned();
              byte_size_valid = true;
              break;
            }
          }
        }

        std::unique_ptr<UniqueDWARFASTType> unique_ast_entry_ap(
            new UniqueDWARFASTType());

        if (type_name_const_str &&
            dwarf->GetUniqueDWARFASTTypeMap().Find(
                type_name_const_str, die, decl,
                byte_size_valid ? byte_size : -1, *unique_ast_entry_ap)) {
          type_sp = unique_ast_entry_ap->m_type_sp;
          if (type_sp) {
            dwarf->m_die_to_type[die.GetDIE()] = type_sp.get();
            return type_sp;
          }
        }

        DEBUG_PRINTF("0x%8.8" PRIx64 ": %s (\"%s\")\n", die.GetID(),
                     DW_TAG_value_to_name(tag), type_name_cstr);

        bool compiler_type_was_created = false;
        compiler_type.SetCompilerType(
            &m_ast, dwarf->GetForwardDeclDieToClangType().lookup(die.GetDIE()));
        if (!compiler_type) {
          compiler_type_was_created = true;
          compiler_type =
              m_ast.CreateStructType(type_name_const_str, byte_size);
        }

        type_sp = std::make_shared<Type>(
            die.GetID(), dwarf, type_name_const_str, byte_size, nullptr,
            LLDB_INVALID_UID, Type::eEncodingIsUID, &decl, compiler_type,
            Type::ResolveState::Forward);

        unique_ast_entry_ap->m_type_sp = type_sp;
        unique_ast_entry_ap->m_die = die;
        unique_ast_entry_ap->m_declaration = decl;
        unique_ast_entry_ap->m_byte_size = byte_size;
        dwarf->GetUniqueDWARFASTTypeMap().Insert(type_name_const_str,
                                                 *unique_ast_entry_ap);

        if (!die.HasChildren())
          m_ast.CompleteStructType(compiler_type);
        else if (compiler_type_was_created) {
          dwarf->GetForwardDeclDieToClangType()[die.GetDIE()] =
              compiler_type.GetOpaqueQualType();
          dwarf->GetForwardDeclClangTypeToDie().try_emplace(
           compiler_type.GetOpaqueQualType(),
          *die.GetDIERef());
        }
      } break;
      case DW_TAG_dynamic_type: {
        dwarf->m_die_to_type[die.GetDIE()] = DIE_IS_BEING_PARSED;
        const size_t num_attributes = die.GetAttributes(attributes);
        DWARFFormValue type_die_form;
        DWARFExpression dw_location, dw_allocated;
        ModuleSP module(die.GetModule());
        ConstString empty_name;

        for (size_t i = 0; i < num_attributes; i++) {
          attr = attributes.AttributeAtIndex(i);
          if (attributes.ExtractFormValueAtIndex(i, form_value)) {
            switch (attr) {
            case DW_AT_type:
              type_die_form = form_value;
              break;
            case DW_AT_data_location: {
              if (!DWARFFormValue::IsBlockForm(form_value.Form())) {
                dwarf->GetObjectFile()->GetModule()->ReportError(
                    "{0x%8.8x}: dynamic type tag 0x%4.4x (%s), has invalid"
                    " DW_AT_data_location expression type."
                    "please file a bug and attach the file at the "
                    "start of this error message",
                    die.GetOffset(), tag, DW_TAG_value_to_name(tag));
              }

              auto data = die.GetData();
              uint32_t offset = form_value.BlockData() - data.GetDataStart();
              uint32_t length = form_value.Unsigned();
              dw_location = DWARFExpression(
                  module, DataExtractor(data, offset, length), die.GetCU());
            } break;
            case DW_AT_allocated: {
              if (!DWARFFormValue::IsBlockForm(form_value.Form())) {
                dwarf->GetObjectFile()->GetModule()->ReportError(
                    "{0x%8.8x}: dynamic type tag 0x%4.4x (%s), has invalid"
                    " DW_AT_allocated expression type."
                    "please file a bug and attach the file at the "
                    "start of this error message",
                    die.GetOffset(), tag, DW_TAG_value_to_name(tag));
              }

              auto data = die.GetData();
              uint32_t offset = form_value.BlockData() - data.GetDataStart();
              uint32_t length = form_value.Unsigned();
              dw_allocated = DWARFExpression(
                  module, DataExtractor(data, offset, length), die.GetCU());
            } break;
            }
          }
        }
        DEBUG_PRINTF("0x%8.8" PRIx64 ": %s (\"%s\")\n", die.GetID(),
                     DW_TAG_value_to_name(tag), type_name_cstr);

        Type *base_type =
            dwarf->ResolveTypeUID(type_die_form.Reference(), true);
        compiler_type = m_ast.CreateDynamicType(
            base_type->GetForwardCompilerType(), dw_location, dw_allocated);
        type_sp = std::make_shared<Type>(
            die.GetID(), dwarf, empty_name, 0, nullptr,
            dwarf->GetUID(type_die_form.Reference()), Type::eEncodingIsUID,
            &decl, compiler_type, Type::ResolveState::Full);
        type_sp->SetEncodingType(base_type);
      } break;
      case DW_TAG_typedef:
      case DW_TAG_unspecified_type:
        dwarf->GetObjectFile()->GetModule()->LogMessage(
            log,
            "DWARFASTParserLegacy::ParseTypeFromDWARF (die = 0x%8.8x) %s name "
            "= '%s'), not supported, yet!.",
            die.GetOffset(), DW_TAG_value_to_name(die.Tag()), die.GetName());
        break;
      case DW_TAG_subprogram:
      case DW_TAG_subroutine_type: {
        dwarf->m_die_to_type[die.GetDIE()] = DIE_IS_BEING_PARSED;

        bool is_variadic = false;

        const size_t num_attr = die.GetAttributes(attributes);
        for (size_t i = 0; i < num_attr; ++i) {
          if (attributes.ExtractFormValueAtIndex(i, form_value)) {
            switch (attributes.AttributeAtIndex(i)) {
            case DW_AT_name:
              type_name_cstr = form_value.AsCString();
              type_name_const_str.SetCString(type_name_cstr);
              break;
              // todo external
            }
          }
        }

        DEBUG_PRINTF("0x%8.8" PRIx64 ": %s (\"%s\")\n", die.GetID(),
                     DW_TAG_value_to_name(tag), type_name_cstr);

        std::vector<CompilerType> function_params_types;
        if (die.HasChildren())
          ParseChildParameters(*sc.comp_unit, die, is_variadic,
                               function_params_types);

        compiler_type = m_ast.CreateFunctionType(
            type_name_const_str, function_params_types.data(),
            function_params_types.size(), is_variadic);

        type_sp = std::make_shared<Type>(
            die.GetID(), dwarf, type_name_const_str, 0, nullptr,
            LLDB_INVALID_UID, Type::eEncodingIsUID, &decl, compiler_type,
            Type::ResolveState::Full);

        assert(type_sp.get());
      } break;
      default:
        dwarf->GetObjectFile()->GetModule()->ReportError(
            "{0x%8.8x}: unhandled type tag 0x%4.4x (%s), "
            "please file a bug and attach the file at the "
            "start of this error message",
            die.GetOffset(), tag, DW_TAG_value_to_name(tag));
        break;
      }

      if (type_sp.get()) {
        DWARFDIE sc_parent_die =
            SymbolFileDWARF::GetParentSymbolContextDIE(die);
        dw_tag_t sc_parent_tag = sc_parent_die.Tag();

        SymbolContextScope *symbol_context_scope = nullptr;
        if (sc_parent_tag == DW_TAG_compile_unit ||
            sc_parent_tag == DW_TAG_partial_unit) {
          symbol_context_scope = sc.comp_unit;
        } else if (sc.function != nullptr && sc_parent_die) {
          symbol_context_scope =
              sc.function->GetBlock(true).FindBlockByID(sc_parent_die.GetID());
          if (symbol_context_scope == nullptr)
            symbol_context_scope = sc.function;
        }

        if (symbol_context_scope) {
          type_sp->SetSymbolContextScope(symbol_context_scope);
        }

        dwarf->m_die_to_type[die.GetDIE()] = type_sp.get();
      }
    } else if (type_ptr != DIE_IS_BEING_PARSED) {
      type_sp = type_ptr->shared_from_this();
    }
  }
  return type_sp;
}

Function *DWARFASTParserLegacy::ParseFunctionFromDWARF(CompileUnit &comp_unit,
                                                       const DWARFDIE &die) {
  if (die.Tag() != DW_TAG_subprogram)
    return nullptr;

  DWARFRangeList func_ranges;
  const char *name = nullptr;
  const char *mangled = nullptr;
  int decl_file = 0;
  int decl_line = 0;
  int decl_column = 0;
  int call_file = 0;
  int call_line = 0;
  int call_column = 0;
  DWARFExpression frame_base;
  DWARFExpression rc_frame_base;
  DWARFExpression static_link;

  if (die.GetDIENamesAndRanges(name, mangled, func_ranges, decl_file, decl_line,
                               decl_column, call_file, call_line, call_column,
                               &frame_base, &static_link, &rc_frame_base)) {
    // Union of all ranges in the function DIE (if the function is
    // discontiguous)
    AddressRange func_range;
    lldb::addr_t lowest_func_addr = func_ranges.GetMinRangeBase(0);
    lldb::addr_t highest_func_addr = func_ranges.GetMaxRangeEnd(0);
    if (lowest_func_addr != LLDB_INVALID_ADDRESS &&
        lowest_func_addr <= highest_func_addr) {
      ModuleSP module_sp(die.GetModule());
      func_range.GetBaseAddress().ResolveAddressUsingFileSections(
          lowest_func_addr, module_sp->GetSectionList());
      if (func_range.GetBaseAddress().IsValid())
        func_range.SetByteSize(highest_func_addr - lowest_func_addr);
    }

    if (func_range.GetBaseAddress().IsValid()) {
      Mangled func_name;
      func_name.SetValue(ConstString(name), false);

      FunctionSP func_sp;
      std::unique_ptr<Declaration> decl_ap;
      if (decl_file != 0 || decl_line != 0 || decl_column != 0)
        decl_ap.reset(new Declaration(
            comp_unit.GetSupportFiles().GetFileSpecAtIndex(decl_file),
            decl_line, decl_column));

      SymbolFileDWARF *dwarf = die.GetDWARF();
      // Supply the type _only_ if it has already been parsed
      Type *func_type = dwarf->m_die_to_type.lookup(die.GetDIE());

      assert(func_type == NULL || func_type != DIE_IS_BEING_PARSED);

      if (dwarf->FixupAddress(func_range.GetBaseAddress())) {
        const user_id_t func_user_id = die.GetID();
        func_sp = std::make_shared<Function>(
            &comp_unit, func_user_id, // UserID is the DIE offset
            func_user_id, func_name, func_type,
            func_range); // first address range

        if (func_sp.get() != nullptr) {
          if (frame_base.IsValid())
            func_sp->GetFrameBaseExpression() = frame_base;
          if (rc_frame_base.IsValid())
            func_sp->GetRCFrameBaseExpression() = rc_frame_base;
          if (static_link.IsValid())
            func_sp->GetStaticLinkExpression() = static_link;
          comp_unit.AddFunction(func_sp);
          return func_sp.get();
        }
      }
    }
  }
  return nullptr;
}

bool DWARFASTParserLegacy::CompleteTypeFromDWARF(const DWARFDIE &die,
                                                 Type *type,
                                                 CompilerType &comp_type) {

  if (!die)
    return false;

  if (!comp_type)
    return false;

  const dw_tag_t tag = die.Tag();
  if (tag == DW_TAG_structure_type || tag == DW_TAG_union_type) {
    if (die.HasChildren())
      ParseChildMembers(die, comp_type);

    m_ast.CompleteStructType(comp_type);
    return true;
  }
  return false;
}

size_t DWARFASTParserLegacy::ParseChildParameters(
    CompileUnit &comp_unit, const DWARFDIE &parent_die, bool &is_variadic,
    std::vector<CompilerType> &function_param_types) {

  if (!parent_die)
    return 0;

  size_t arg_idx = 0;
  for (DWARFDIE die = parent_die.GetFirstChild(); die.IsValid();
       die = die.GetSibling()) {
    dw_tag_t tag = die.Tag();
    switch (tag) {
    case DW_TAG_formal_parameter:
      assert(0 && "Funtions with parameters pending, yet!");
    default:
      break;
    }
  }
  return arg_idx;
}

size_t
DWARFASTParserLegacy::ParseChildMembers(const DWARFDIE &parent_die,
                                        CompilerType &struct_compiler_type) {
  size_t member_idx = 0;

  for (DWARFDIE die = parent_die.GetFirstChild(); die.IsValid();
       die = die.GetSibling()) {
    dw_tag_t tag = die.Tag();

    if (tag == DW_TAG_member) {
      DWARFAttributes attributes;
      const char *name = NULL;
      DWARFFormValue encoding_uid;
      const size_t num_attributes = die.GetAttributes(attributes);
      uint32_t member_offset_in_bits = UINT32_MAX;
      for (size_t i = 0; i < num_attributes; ++i) {
        const dw_attr_t attr = attributes.AttributeAtIndex(i);
        DWARFFormValue form_value;

        if (attributes.ExtractFormValueAtIndex(i, form_value)) {
          switch (attr) {
          default:
            break;
          case DW_AT_name:
            name = form_value.AsCString();
            break;
          case DW_AT_type:
            encoding_uid = form_value;
            break;
          case DW_AT_data_member_location:
            if (form_value.BlockData()) {
              assert(0 && "Data member location expr pending.");
              return 0;
            } else {
              member_offset_in_bits = form_value.Unsigned() * 8;
            }
            break;
          case DW_AT_data_bit_offset:
            member_offset_in_bits = form_value.Unsigned();
            break;
          }
        }
      }

      Type *member_type = die.ResolveTypeUID(encoding_uid.Reference());
      if (member_type) {
        CompilerType member_full_type = member_type->GetFullCompilerType();
        m_ast.AddFieldToStruct(struct_compiler_type, ConstString(name),
                               member_full_type, member_offset_in_bits);
      }
      ++member_idx;
    }
  }

  return 0;
}
