; ModuleID = 'MainModule'
source_filename = "test/str_variable.pcl"

@s = common global i8*
@str.main.entry.2 = private unnamed_addr constant [13 x i8] c"Hello World!\00"
@str.main.entry.3 = private unnamed_addr constant [5 x i8] c"%s%s\00"
@str.main.entry.5 = private unnamed_addr constant [2 x i8] c"\0A\00"

declare i32 @printf(i8*, ...)

define i32 @main(i32 %main) {
main.entry:
  %0 = alloca i32
  store i32 %main, i32* %0
  store i8* getelementptr inbounds ([13 x i8], [13 x i8]* @str.main.entry.2, i32 0, i32 0), i8** @s
  %1 = load i8*, i8** @s
  %2 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @str.main.entry.3, i32 0, i32 0), i8* %1, i8* getelementptr inbounds ([2 x i8], [2 x i8]* @str.main.entry.5, i32 0, i32 0))
  %3 = load i32, i32* %0
  ret i32 %3
}
