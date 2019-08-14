; ModuleID = 'MainModule'
source_filename = "test/proctest.pcl"

@str.hi.entry.1 = private unnamed_addr constant [5 x i8] c"%s%s\00"
@str.hi.entry.2 = private unnamed_addr constant [9 x i8] c"ProcTest\00"
@str.hi.entry.3 = private unnamed_addr constant [2 x i8] c"\0A\00"
@str.hi.entry.5 = private unnamed_addr constant [5 x i8] c"%s%s\00"
@str.hi.entry.6 = private unnamed_addr constant [6 x i8] c"Hello\00"
@str.hi.entry.7 = private unnamed_addr constant [2 x i8] c"\0A\00"

declare i32 @printf(i8*, ...)

define void @hi() {
hi.entry:
  %0 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @str.hi.entry.1, i32 0, i32 0), i8* getelementptr inbounds ([9 x i8], [9 x i8]* @str.hi.entry.2, i32 0, i32 0), i8* getelementptr inbounds ([2 x i8], [2 x i8]* @str.hi.entry.3, i32 0, i32 0))
  %1 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @str.hi.entry.5, i32 0, i32 0), i8* getelementptr inbounds ([6 x i8], [6 x i8]* @str.hi.entry.6, i32 0, i32 0), i8* getelementptr inbounds ([2 x i8], [2 x i8]* @str.hi.entry.7, i32 0, i32 0))
  ret void
}

define i32 @main(i32 %main) {
main.entry:
  %0 = alloca i32
  store i32 %main, i32* %0
  call void @hi()
  %1 = load i32, i32* %0
  ret i32 %1
}
