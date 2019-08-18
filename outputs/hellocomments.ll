; ModuleID = 'MainModule'
source_filename = "test/hellocomments.pcl"

@0 = private unnamed_addr constant [5 x i8] c"%s%s\00"
@1 = private unnamed_addr constant [13 x i8] c"Hello World!\00"
@2 = private unnamed_addr constant [2 x i8] c"\0A\00"

declare i32 @printf(i8*, ...)

define i32 @main(i32) {
  %2 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @0, i64 0, i64 0), i8* getelementptr inbounds ([13 x i8], [13 x i8]* @1, i64 0, i64 0), i8* getelementptr inbounds ([2 x i8], [2 x i8]* @2, i64 0, i64 0))
  ret i32 %0
}
