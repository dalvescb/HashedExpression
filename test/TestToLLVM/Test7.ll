; ModuleID = 'basic'
source_filename = "<string>"

define double @test7(double %x) {
entry:
  %t2539310632263636 = call double @llvm.sin.f64(double %x)
  ret double %t2539310632263636
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
