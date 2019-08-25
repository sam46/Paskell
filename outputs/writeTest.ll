; ModuleID = 'MainModule'
source_filename = "test/writeTest.pcl"

@a = common global i8* null
@b = common global i32 0
@c = common global double 0.000000e+00
@0 = private unnamed_addr constant [12 x i8] c"Hello World\00"
@1 = private unnamed_addr constant [5 x i8] c"%s%s\00"
@2 = private unnamed_addr constant [2 x i8] c"\0A\00"

declare i32 @printf(i8*, ...)

define i32 @main(i32) {
  store double 1.200000e+00, double* @c, align 8
  store i32 2, i32* @b, align 4
  store i8* getelementptr inbounds ([12 x i8], [12 x i8]* @0, i64 0, i64 0), i8** @a, align 8
  %2 = tail call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @1, i64 0, i64 0), i8* getelementptr inbounds ([12 x i8], [12 x i8]* @0, i64 0, i64 0), i8* getelementptr inbounds ([2 x i8], [2 x i8]* @2, i64 0, i64 0))
  ret i32 %0
}
