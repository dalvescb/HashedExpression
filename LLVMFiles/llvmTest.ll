; ModuleID = 'basic'
source_filename = "<string>"

define i32 @add(i32 %a, i32 %b) {
entry:
  %result = add i32 %a, %b
  ret i32 %result
}

define i32 @sub(i32 %a, i32 %b) {
entry:
  %result = sub i32 %a, %b
  ret i32 %result
}

define i32 @mul(i32 %a, i32 %b) {
entry:
  %result = mul i32 %a, %b
  ret i32 %result
}

define i32 @div(i32 %a, i32 %b) {
entry:
  %result = sdiv i32 %a, %b
  ret i32 %result
}

define i32 @mod(i32 %a, i32 %b) {
entry:
  %result = srem i32 %a, %b
  ret i32 %result
}

define i32 @shl(i32 %a, i32 %b) {
entry:
  %result = shl i32 %a, %b
  ret i32 %result
}

define i32 @lshr(i32 %a, i32 %b) {
entry:
  %result = lshr i32 %a, %b
  ret i32 %result
}

define i32 @ashr(i32 %a, i32 %b) {
entry:
  %result = ashr i32 %a, %b
  ret i32 %result
}

define i32 @add.1(i32 %a, i32 %b) {
entry:
  %result = add i32 %a, %b
  ret i32 %result
}

define i32 @or(i32 %a, i32 %b) {
entry:
  %result = or i32 %a, %b
  ret i32 %result
}

define i32 @xor(i32 %a, i32 %b) {
entry:
  %result = xor i32 %a, %b
  ret i32 %result
}

define float @fadd(float %a, float %b) {
entry:
  %result = fadd float %a, %b
  ret float %result
}

define i32 @count(i32 %n) {
entry:
   br label %loop
loop:
   %i = phi i32 [ 1, %entry ], [ %nextvar, %loop ]
   %nextvar = add i32 %i, 1
   %cmptmp = icmp ult i32 %i, %n
   %booltmp = zext i1 %cmptmp to i32
   %loopcond = icmp ne i32 %booltmp, 0

   br i1 %loopcond, label %loop, label %afterloop
afterloop:
   ret i32 %i
}
