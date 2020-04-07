; ModuleID = 'basic'
source_filename = "Main.hs"
target triple = "x86_64-apple-macosx10.15.0"

define double @test1.ll(double %x0, double %x1, double %x2, double %x3, double %x4, double %x5, double %x6, double %x7, double %x8, double %x9, double %y) {
entry:
  %t1113937649807536 = fmul double %x0, %x1
  %t1023105356512890 = fmul double %t1113937649807536, %x2
  %t1182468901041066 = fmul double %t1023105356512890, %x3
  %t1080036217880891 = fmul double %t1182468901041066, %x4
  %t1181002989139170 = fmul double %t1080036217880891, %x5
  %t1121034805635147 = fmul double %t1181002989139170, %x6
  %t1234921583972606 = fmul double %t1121034805635147, %x7
  %t1267684798096778 = fmul double %t1234921583972606, %x8
  %t1160933160265921 = fmul double %t1267684798096778, %x9
  %t860006610424745 = fadd double %x0, %x1
  %t829093578103334 = fadd double %t860006610424745, %x2
  %t888171448350914 = fadd double %t829093578103334, %x3
  %t816582238519895 = fadd double %t888171448350914, %x4
  %t800408162321345 = fadd double %t816582238519895, %x5
  %t762056333570421 = fadd double %t800408162321345, %x6
  %t918791009645843 = fadd double %t762056333570421, %x7
  %t806733041855021 = fadd double %t918791009645843, %x8
  %t840533988101340 = fadd double %t806733041855021, %x9
  %t1743508550768458 = fsub double 0.000000e+00, %t840533988101340
  %t801925215886683 = fadd double %t1160933160265921, %t1743508550768458
  %t888871445490635 = fadd double %x0, %y
  %t1501979649663258 = call double @llvm.pow.f64(double %t888871445490635, double -1.000000e+00)
  %t1211997463717641 = fmul double %x0, %t1501979649663258
  %t1000216311837998 = fadd double %t801925215886683, %t1211997463717641
  %t1337447513054473 = call double @llvm.pow.f64(double %t1000216311837998, double 2.000000e+00)
  ret double %t1337447513054473
}

; Function Attrs: nounwind readnone speculatable
declare double @llvm.sin.f64(double) #0

; Function Attrs: nounwind readnone speculatable
declare double @llvm.cos.f64(double) #0

; Function Attrs: nounwind readnone
declare double @tan(double) #1

; Function Attrs: nounwind readnone
declare double @sinh(double) #1

; Function Attrs: nounwind readnone
declare double @cosh(double) #1

; Function Attrs: nounwind readnone
declare double @tanh(double) #1

; Function Attrs: nounwind readnone
declare double @asin(double) #1

; Function Attrs: nounwind readnone
declare double @acos(double) #1

; Function Attrs: nounwind readnone
declare double @atan(double) #1

; Function Attrs: nounwind readnone
declare double @asinh(double) #1

; Function Attrs: nounwind readnone
declare double @acosh(double) #1

; Function Attrs: nounwind readnone
declare double @atanh(double) #1

; Function Attrs: nounwind readnone speculatable
declare double @llvm.exp.f64(double) #0

; Function Attrs: nounwind readnone speculatable
declare double @llvm.log.f64(double) #0

; Function Attrs: nounwind readnone speculatable
declare double @llvm.pow.f64(double, double) #0

; Function Attrs: nounwind readnone speculatable
declare double @llvm.sqrt.f64(double) #0

attributes #0 = { nounwind readnone speculatable }
attributes #1 = { nounwind readnone }
