; ModuleID = 'basic'
source_filename = "<string>"

define double @test1(double %x) {
entry:
  %t1405989648669474 = call double @llvm.pow.f64(double %x, double 2.000000e+00)
  ret double %t1405989648669474
}

; Function Attrs: nounwind readnone speculatable
declare double @llvm.sin.f64(double) #0

; Function Attrs: nounwind readnone speculatable
declare double @llvm.cos.f64(double) #0

; Function Attrs: nounwind readnone
declare double @llvm.tan.f64(double) #1

; Function Attrs: nounwind readnone
declare double @llvm.sinh.f64(double) #1

; Function Attrs: nounwind readnone
declare double @llvm.cosh.f64(double) #1

; Function Attrs: nounwind readnone
declare double @llvm.tanh.f64(double) #1

; Function Attrs: nounwind readnone
declare double @llvm.asin.f64(double) #1

; Function Attrs: nounwind readnone
declare double @llvm.acos.f64(double) #1

; Function Attrs: nounwind readnone
declare double @llvm.atan.f64(double) #1

; Function Attrs: nounwind readnone
declare double @llvm.asinh.f64(double) #1

; Function Attrs: nounwind readnone
declare double @llvm.acosh.f64(double) #1

; Function Attrs: nounwind readnone
declare double @llvm.atanh.f64(double) #1

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
