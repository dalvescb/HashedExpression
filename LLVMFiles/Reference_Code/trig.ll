; ModuleID = 'trig.c'
source_filename = "trig.c"
target datalayout = "e-m:o-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-apple-macosx10.15.0"

; Function Attrs: noinline nounwind optnone ssp uwtable
define i32 @main() #0 {
  %1 = alloca i32, align 4
  %2 = alloca i32, align 4
  %3 = alloca i32, align 4
  store i32 0, i32* %1, align 4
  %4 = load i32, i32* %2, align 4
  %5 = sitofp i32 %4 to double
  %6 = call double @llvm.sin.f64(double %5)
  %7 = fptosi double %6 to i32
  store i32 %7, i32* %3, align 4
  %8 = load i32, i32* %2, align 4
  %9 = sitofp i32 %8 to double
  %10 = call double @llvm.cos.f64(double %9)
  %11 = fptosi double %10 to i32
  store i32 %11, i32* %3, align 4
  %12 = load i32, i32* %2, align 4
  %13 = sitofp i32 %12 to double
  %14 = call double @tan(double %13) #3
  %15 = fptosi double %14 to i32
  store i32 %15, i32* %3, align 4
  %16 = load i32, i32* %2, align 4
  %17 = sitofp i32 %16 to double
  %18 = call double @llvm.exp.f64(double %17)
  %19 = fptosi double %18 to i32
  store i32 %19, i32* %3, align 4
  %20 = load i32, i32* %2, align 4
  %21 = sitofp i32 %20 to double
  %22 = call double @llvm.log.f64(double %21)
  %23 = fptosi double %22 to i32
  store i32 %23, i32* %3, align 4
  %24 = load i32, i32* %2, align 4
  %25 = sitofp i32 %24 to double
  %26 = call double @sinh(double %25) #3
  %27 = fptosi double %26 to i32
  store i32 %27, i32* %3, align 4
  %28 = load i32, i32* %2, align 4
  %29 = sitofp i32 %28 to double
  %30 = call double @cosh(double %29) #3
  %31 = fptosi double %30 to i32
  store i32 %31, i32* %3, align 4
  %32 = load i32, i32* %2, align 4
  %33 = sitofp i32 %32 to double
  %34 = call double @tanh(double %33) #3
  %35 = fptosi double %34 to i32
  store i32 %35, i32* %3, align 4
  %36 = load i32, i32* %2, align 4
  %37 = sitofp i32 %36 to double
  %38 = call double @asin(double %37) #3
  %39 = fptosi double %38 to i32
  store i32 %39, i32* %3, align 4
  %40 = load i32, i32* %2, align 4
  %41 = sitofp i32 %40 to double
  %42 = call double @acos(double %41) #3
  %43 = fptosi double %42 to i32
  store i32 %43, i32* %3, align 4
  %44 = load i32, i32* %2, align 4
  %45 = sitofp i32 %44 to double
  %46 = call double @atan(double %45) #3
  %47 = fptosi double %46 to i32
  store i32 %47, i32* %3, align 4
  %48 = load i32, i32* %2, align 4
  %49 = sitofp i32 %48 to double
  %50 = call double @asinh(double %49) #3
  %51 = fptosi double %50 to i32
  store i32 %51, i32* %3, align 4
  %52 = load i32, i32* %2, align 4
  %53 = sitofp i32 %52 to double
  %54 = call double @acosh(double %53) #3
  %55 = fptosi double %54 to i32
  store i32 %55, i32* %3, align 4
  %56 = load i32, i32* %2, align 4
  %57 = sitofp i32 %56 to double
  %58 = call double @atanh(double %57) #3
  %59 = fptosi double %58 to i32
  store i32 %59, i32* %3, align 4
  ret i32 0
}

; Function Attrs: nounwind readnone speculatable
declare double @llvm.sin.f64(double) #1

; Function Attrs: nounwind readnone speculatable
declare double @llvm.cos.f64(double) #1

; Function Attrs: nounwind readnone
declare double @tan(double) #2

; Function Attrs: nounwind readnone speculatable
declare double @llvm.exp.f64(double) #1

; Function Attrs: nounwind readnone speculatable
declare double @llvm.log.f64(double) #1

; Function Attrs: nounwind readnone
declare double @sinh(double) #2

; Function Attrs: nounwind readnone
declare double @cosh(double) #2

; Function Attrs: nounwind readnone
declare double @tanh(double) #2

; Function Attrs: nounwind readnone
declare double @asin(double) #2

; Function Attrs: nounwind readnone
declare double @acos(double) #2

; Function Attrs: nounwind readnone
declare double @atan(double) #2

; Function Attrs: nounwind readnone
declare double @asinh(double) #2

; Function Attrs: nounwind readnone
declare double @acosh(double) #2

; Function Attrs: nounwind readnone
declare double @atanh(double) #2

attributes #0 = { noinline nounwind optnone ssp uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "darwin-stkchk-strong-link" "disable-tail-calls"="false" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "probe-stack"="___chkstk_darwin" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nounwind readnone speculatable }
attributes #2 = { nounwind readnone "correctly-rounded-divide-sqrt-fp-math"="false" "darwin-stkchk-strong-link" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "probe-stack"="___chkstk_darwin" "stack-protector-buffer-size"="8" "target-cpu"="penryn" "target-features"="+cx16,+fxsr,+mmx,+sahf,+sse,+sse2,+sse3,+sse4.1,+ssse3,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #3 = { nounwind readnone }

!llvm.module.flags = !{!0, !1, !2}
!llvm.ident = !{!3}

!0 = !{i32 2, !"SDK Version", [2 x i32] [i32 10, i32 15]}
!1 = !{i32 1, !"wchar_size", i32 4}
!2 = !{i32 7, !"PIC Level", i32 2}
!3 = !{!"Apple clang version 11.0.0 (clang-1100.0.33.17)"}
