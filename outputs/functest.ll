; ModuleID = 'MainModule'
source_filename = "test/functest.pcl"

@0 = private unnamed_addr constant [5 x i8] c"%s%s\00"
@1 = private unnamed_addr constant [9 x i8] c"FuncTest\00"
@2 = private unnamed_addr constant [2 x i8] c"\0A\00"
@3 = private unnamed_addr constant [5 x i8] c"%d%s\00"
@4 = private unnamed_addr constant [2 x i8] c"\0A\00"

declare i32 @printf(i8*, ...)

define i32 @add(i32, i32, i32) {
  %4 = add i32 %1, %2
  ret i32 %4
}

define i32 @main(i32) {
  %2 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @0, i64 0, i64 0), i8* getelementptr inbounds ([9 x i8], [9 x i8]* @1, i64 0, i64 0), i8* getelementptr inbounds ([2 x i8], [2 x i8]* @2, i64 0, i64 0))
  %3 = call i32 @add(i32 0, i32 1, i32 2)
  %4 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @3, i64 0, i64 0), i32 %3, i8* getelementptr inbounds ([2 x i8], [2 x i8]* @4, i64 0, i64 0))
  ret i32 %0
}
