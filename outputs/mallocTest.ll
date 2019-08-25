; ModuleID = 'MainModule'
source_filename = "test/mallocTest.pcl"

@p = common global i32* null
@i = common global i32 0
@0 = private unnamed_addr constant [5 x i8] c"%p%s\00"
@1 = private unnamed_addr constant [2 x i8] c"\0A\00"

declare i32 @printf(i8*, ...)

define i32 @main(i32) {
  store i32 0, i32* @i, align 4
  %2 = load i32*, i32** @p, align 8
  %3 = tail call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @0, i64 0, i64 0), i32* dereferenceable(4) %2, i8* getelementptr inbounds ([2 x i8], [2 x i8]* @1, i64 0, i64 0))
  ret i32 %0
}
