; ModuleID = 'basic'
source_filename = "<string>"

define double @test3(double %x, double %y) {
entry:
  %t1142802484873426 = fmul double %x, %y
  ret double %t1142802484873426
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
