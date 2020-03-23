; ModuleID = 'forArray.c'
source_filename = "forArray.c"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

@__const.main.a = private unnamed_addr constant [3 x i32] [i32 1, i32 2, i32 3], align 4
@__const.main.b = private unnamed_addr constant [3 x i32] [i32 2, i32 4, i32 6], align 4
@.str = private unnamed_addr constant [20 x i8] c"value of c[%d]: %d\0A\00", align 1

; Function Attrs: nounwind ssp uwtable
define i32 @main(i32, i8** nocapture readnone) local_unnamed_addr #0 {
  %3 = icmp sgt i32 %0, 0
  br i1 %3, label %4, label %6

; <label>:4:                                      ; preds = %2
  %5 = zext i32 %0 to i64
  br label %7

; <label>:6:                                      ; preds = %7, %2
  ret i32 0

; <label>:7:                                      ; preds = %7, %4
  %8 = phi i64 [ 0, %4 ], [ %16, %7 ]
  %9 = getelementptr inbounds [3 x i32], [3 x i32]* @__const.main.a, i64 0, i64 %8
  %10 = load i32, i32* %9, align 4, !tbaa !4
  %11 = getelementptr inbounds [3 x i32], [3 x i32]* @__const.main.b, i64 0, i64 %8
  %12 = load i32, i32* %11, align 4, !tbaa !4
  %13 = add nsw i32 %12, %10
  %14 = trunc i64 %8 to i32
  %15 = tail call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([20 x i8], [20 x i8]* @.str, i64 0, i64 0), i32 %14, i32 %13)
  %16 = add nuw nsw i64 %8, 1
  %17 = icmp eq i64 %16, %5
  br i1 %17, label %6, label %7
}

; Function Attrs: nounwind
declare i32 @printf(i8* nocapture readonly, ...) local_unnamed_addr #1

attributes #0 = { nounwind ssp uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "darwin-stkchk-strong-link" "disable-tail-calls"="false" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "probe-stack"="___chkstk_darwin" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nounwind "correctly-rounded-divide-sqrt-fp-math"="false" "darwin-stkchk-strong-link" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "probe-stack"="___chkstk_darwin" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.module.flags = !{!0, !1, !2}
!llvm.ident = !{!3}

!0 = !{i32 2, !"SDK Version", [2 x i32] [i32 10, i32 15]}
!1 = !{i32 1, !"wchar_size", i32 4}
!2 = !{i32 7, !"PIC Level", i32 2}
!3 = !{!"Apple clang version 11.0.0 (clang-1100.0.33.17)"}
!4 = !{!5, !5, i64 0}
!5 = !{!"int", !6, i64 0}
!6 = !{!"omnipotent char", !7, i64 0}
!7 = !{!"Simple C/C++ TBAA"}
