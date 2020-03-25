; ModuleID = 'exampleModule'
source_filename = "<string>"

define i32 @add(i32 %x, i32 %y) {
entry:
  %0 = sub i32 0, %x
  %1 = add i32 %0, %0
  %2 = mul i32 %1, %0
  ret i32 %2
}

define i32 @swap(i32 %x, i32 %y) {
entry:
  %0 = add i32 %x, %y
  %1 = sub i32 %0, %y
  %2 = sub i32 %0, %1
  ret i32 0
}
