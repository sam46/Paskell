; ModuleID = 'MainModule'
source_filename = "test/functest.pcl"

@str.main.entry.2 = private unnamed_addr constant [5 x i8] c"%s%s\00"
@str.main.entry.3 = private unnamed_addr constant [9 x i8] c"FuncTest\00"
@str.main.entry.4 = private unnamed_addr constant [2 x i8] c"\0A\00"
@str.main.entry.6 = private unnamed_addr constant [5 x i8] c"%d%s\00"
@str.main.entry.8 = private unnamed_addr constant [2 x i8] c"\0A\00"

declare i32 @printf(i8*, ...)

define i32 @add(i32 %add, i32 %a, i32 %b) {
add.entry:
  %0 = alloca i32
  store i32 %add, i32* %0
  %1 = alloca i32
  store i32 %a, i32* %1
  %2 = alloca i32
  store i32 %b, i32* %2
  %3 = load i32, i32* %1
  %4 = load i32, i32* %2
  %5 = add i32 %3, %4
  store i32 %5, i32* %0
  %6 = load i32, i32* %0
  ret i32 %6
}

define i32 @main(i32 %main) {
main.entry:
  %0 = alloca i32
  store i32 %main, i32* %0
  %1 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @str.main.entry.2, i32 0, i32 0), i8* getelementptr inbounds ([9 x i8], [9 x i8]* @str.main.entry.3, i32 0, i32 0), i8* getelementptr inbounds ([2 x i8], [2 x i8]* @str.main.entry.4, i32 0, i32 0))
  %2 = call i32 @add(i32 0, i32 1, i32 2)
  %3 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @str.main.entry.6, i32 0, i32 0), i32 %2, i8* getelementptr inbounds ([2 x i8], [2 x i8]* @str.main.entry.8, i32 0, i32 0))
  %4 = load i32, i32* %0
  ret i32 %4
}
