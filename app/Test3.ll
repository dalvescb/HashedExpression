; ModuleID = 'basic'
source_filename = "Main.hs"

define double @test3() {
entry:
  %t761793118134727 = fadd double 0.000000e+00, 1.000000e+00
  %t3225223544315303 = call double @tan(double %t761793118134727)
  ret double %t3225223544315303
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
