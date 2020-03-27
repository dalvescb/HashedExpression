; ModuleID = 'basic'
source_filename = "<string>"

define double @func(double %a, double %b) {
entry:
  %t761793117067181 = fadd double 0.000000e+00, 4.900000e+01
  %t2445913513075420 = call double @llvm.sqrt.f64(double %t761793117067181)
  ret double %t2445913513075420
}

; Function Attrs: nounwind readnone speculatable
declare double @llvm.sqrt.f64(double) #0

attributes #0 = { nounwind readnone speculatable }
