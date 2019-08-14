; ModuleID = 'MainModule'
source_filename = "test/variable.pcl"

@i = external global i32

declare i32 @printf(i8*, ...)

define i32 @main(i32 %main) {
main.entry:
  %0 = alloca i32
  store i32 %main, i32* %0
  store i32 1, i32* @i
  %1 = load i32, i32* %0
  ret i32 %1
}
