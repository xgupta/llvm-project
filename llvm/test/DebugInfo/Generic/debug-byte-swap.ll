; RUN: llc -filetype=obj -o %t.o %s
; RUN: llvm-dwarfdump -debug-info %t.o

; ModuleID = 'debug_byte_swap_test'
source_filename = "debug_byte_swap_test.c"

; Define the function
define dso_local i32 @test_function() !dbg !6 {
entry:
  %x = alloca i32, align 4
  store i32 42, ptr %x, align 4
  call void @llvm.dbg.value(metadata ptr %x, metadata !5, metadata !10), !dbg !9
  ret i32 42
}

; Declare intrinsic
declare void @llvm.dbg.value(metadata, metadata, metadata)

; Debug info
!llvm.dbg.cu = !{!0}
!llvm.module.flags = !{!3}

!0 = distinct !DICompileUnit(language: DW_LANG_C, file: !1, producer: "LLVM", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, enums: !2)
!1 = !DIFile(filename: "debug_byte_swap_test.c", directory: "/")
!2 = !{}
!3 = !{i32 2, !"Debug Info Version", i32 3}

!4 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
!5 = !DILocalVariable(name: "x", scope: !6, file: !1, line: 3, type: !4)
!6 = distinct !DISubprogram(name: "test_function", linkageName: "test_function", scope: !1, file: !1, line: 2, type: !7, scopeLine: 2, unit: !0, retainedNodes: !8)
!7 = !DISubroutineType(types: !2)
!8 = !{!5}

!9 = !DILocation(line: 3, column: 5, scope: !6)
!10 = !DIExpression(DW_OP_deref, DW_OP_RC_byte_swap)

