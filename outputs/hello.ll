; ModuleID = 'MainModule'
source_filename = "test/hello.pcl"

@c = common global i8 0
@i = common global i32 0
@f = common global double 0.000000e+00
@str.main.entry.3 = private unnamed_addr constant [5 x i8] c"%s%s\00"
@str.main.entry.4 = private unnamed_addr constant [6 x i8] c"hello\00"
@str.main.entry.5 = private unnamed_addr constant [2 x i8] c"\0A\00"

declare i32 @printf(i8*, ...)

define i32 @main(i32 %main) {
main.entry:
  %0 = alloca i32
  store i32 %main, i32* %0
  %1 = add i32 1, 5
  store i32 %1, i32* @i
  %2 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @str.main.entry.3, i32 0, i32 0), i8* getelementptr inbounds ([6 x i8], [6 x i8]* @str.main.entry.4, i32 0, i32 0), i8* getelementptr inbounds ([2 x i8], [2 x i8]* @str.main.entry.5, i32 0, i32 0))
  %3 = load i32, i32* %0
  ret i32 %3
}
