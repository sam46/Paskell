; ModuleID = 'MainModule'
source_filename = "test/comments.pcl"

declare i32 @printf(i8*, ...)

define i32 @main(i32 %main) {
main.entry:
  %0 = alloca i32
  store i32 %main, i32* %0
  %1 = load i32, i32* %0
  ret i32 %1
}
