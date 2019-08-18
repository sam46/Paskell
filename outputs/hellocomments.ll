; ModuleID = 'MainModule'
source_filename = "test/hellocomments.pcl"

@str.main.entry.2 = private unnamed_addr constant [5 x i8] c"%s%s\00"
@str.main.entry.3 = private unnamed_addr constant [13 x i8] c"Hello World!\00"
@str.main.entry.4 = private unnamed_addr constant [2 x i8] c"\0A\00"

declare i32 @printf(i8*, ...)

define i32 @main(i32 %main) {
main.entry:
  %0 = alloca i32
  store i32 %main, i32* %0
  %1 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @str.main.entry.2, i32 0, i32 0), i8* getelementptr inbounds ([13 x i8], [13 x i8]* @str.main.entry.3, i32 0, i32 0), i8* getelementptr inbounds ([2 x i8], [2 x i8]* @str.main.entry.4, i32 0, i32 0))
  %2 = load i32, i32* %0
  ret i32 %2
}
