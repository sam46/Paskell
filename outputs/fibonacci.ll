; ModuleID = 'MainModule'
source_filename = "test/fibonacci.pcl"

@0 = private unnamed_addr constant [5 x i8] c"%d%s\00"
@1 = private unnamed_addr constant [2 x i8] c"\0A\00"

declare i32 @printf(i8*, ...)

define i32 @fib(i32, i32) {
  %3 = add i32 %1, -1
  %4 = tail call i32 @fib(i32 0, i32 %3)
  %5 = add i32 %1, -2
  %6 = tail call i32 @fib(i32 0, i32 %5)
  %7 = add i32 %4, %6
  ret i32 %7
}

define i32 @main(i32) {
  %2 = tail call i32 @fib(i32 0, i32 2)
  %3 = tail call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @0, i64 0, i64 0), i32 %2, i8* getelementptr inbounds ([2 x i8], [2 x i8]* @1, i64 0, i64 0))
  ret i32 %0
}
