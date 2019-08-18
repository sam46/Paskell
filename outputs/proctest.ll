; ModuleID = 'MainModule'
source_filename = "test/proctest.pcl"

@0 = private unnamed_addr constant [5 x i8] c"%s%s\00"
@1 = private unnamed_addr constant [9 x i8] c"ProcTest\00"
@2 = private unnamed_addr constant [2 x i8] c"\0A\00"
@3 = private unnamed_addr constant [5 x i8] c"%s%s\00"
@4 = private unnamed_addr constant [6 x i8] c"Hello\00"
@5 = private unnamed_addr constant [2 x i8] c"\0A\00"

declare i32 @printf(i8*, ...)

define void @hi() {
  %1 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @0, i64 0, i64 0), i8* getelementptr inbounds ([9 x i8], [9 x i8]* @1, i64 0, i64 0), i8* getelementptr inbounds ([2 x i8], [2 x i8]* @2, i64 0, i64 0))
  %2 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @3, i64 0, i64 0), i8* getelementptr inbounds ([6 x i8], [6 x i8]* @4, i64 0, i64 0), i8* getelementptr inbounds ([2 x i8], [2 x i8]* @5, i64 0, i64 0))
  ret void
}

define i32 @main(i32) {
  call void @hi()
  ret i32 %0
}
