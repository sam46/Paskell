; ModuleID = 'MainModule'
source_filename = "test/embeddedFun.pcl"

@0 = private unnamed_addr constant [3 x i8] c"%s\00"
@1 = private unnamed_addr constant [18 x i8] c"Calling say() ..\0A\00"
@2 = private unnamed_addr constant [5 x i8] c"%s%s\00"
@3 = private unnamed_addr constant [2 x i8] c"\0A\00"
@4 = private unnamed_addr constant [20 x i8] c"Inside say(): Hello\00"

declare i32 @printf(i8*, ...)

define void @say(i8*) {
  %2 = tail call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @0, i64 0, i64 0), i8* getelementptr inbounds ([18 x i8], [18 x i8]* @1, i64 0, i64 0))
  %3 = tail call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @2, i64 0, i64 0), i8* %0, i8* getelementptr inbounds ([2 x i8], [2 x i8]* @3, i64 0, i64 0))
  ret void
}

define i32 @main(i32) {
  tail call void @say(i8* getelementptr inbounds ([20 x i8], [20 x i8]* @4, i64 0, i64 0))
  ret i32 %0
}
