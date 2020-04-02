; ModuleID = 'basic'
source_filename = "Main.hs"

define double @test26(double %w, double %x, double %y) {
entry:
  %t1245166095583728 = fmul double %w, %x
  %t1513173470626744 = call double @llvm.pow.f64(double %y, double -1.000000e+00)
  %t1207004219852244 = fmul double %t1245166095583728, %t1513173470626744
  ret double %t1207004219852244
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
