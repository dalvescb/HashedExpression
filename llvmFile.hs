; ModuleID = 'basic'
source_filename = "<string>"

define i32 @add(i32 %a, i32 %b) {
entry:
  %result = add i32 %a, %b
  ret i32 %result
}
