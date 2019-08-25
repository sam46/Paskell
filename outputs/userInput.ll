; ModuleID = 'MainModule'
source_filename = "test/userInput.pcl"

@i = common global i32 0
@c = common global i8 0
@0 = private unnamed_addr constant [3 x i8] c"%d\00"
@1 = private unnamed_addr constant [5 x i8] c"%d%s\00"
@2 = private unnamed_addr constant [2 x i8] c"\0A\00"
@3 = private unnamed_addr constant [4 x i8] c" %c\00"
@4 = private unnamed_addr constant [5 x i8] c"%c%s\00"
@5 = private unnamed_addr constant [2 x i8] c"\0A\00"

declare i32 @printf(i8*, ...)

declare i32 @scanf(i8*, ...)

define i32 @main(i32) {
  %2 = tail call i32 (i8*, ...) @scanf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @0, i64 0, i64 0), i32* nonnull dereferenceable(4) @i)
  %3 = load i32, i32* @i, align 4
  %4 = tail call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @1, i64 0, i64 0), i32 %3, i8* getelementptr inbounds ([2 x i8], [2 x i8]* @2, i64 0, i64 0))
  %5 = tail call i32 (i8*, ...) @scanf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @3, i64 0, i64 0), i8* nonnull dereferenceable(1) @c)
  %6 = load i8, i8* @c, align 1
  %7 = tail call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @4, i64 0, i64 0), i8 %6, i8* getelementptr inbounds ([2 x i8], [2 x i8]* @5, i64 0, i64 0))
  ret i32 %0
}
