; ModuleID = 'MainModule'
source_filename = "test/int_variable.pcl"

@i = common global i32 0
@0 = private unnamed_addr constant [5 x i8] c"%d%s\00"
@1 = private unnamed_addr constant [2 x i8] c"\0A\00"
@2 = private unnamed_addr constant [3 x i8] c"%s\00"
@3 = private unnamed_addr constant [5 x i8] c"YES\0A\00"
@4 = private unnamed_addr constant [5 x i8] c"%s%s\00"
@5 = private unnamed_addr constant [4 x i8] c"NO\0A\00"
@6 = private unnamed_addr constant [2 x i8] c"\0A\00"
@7 = private unnamed_addr constant [5 x i8] c"%s%s\00"
@8 = private unnamed_addr constant [17 x i8] c"Integer Variable\00"
@9 = private unnamed_addr constant [2 x i8] c"\0A\00"

declare i32 @printf(i8*, ...)

define i32 @main(i32) {
  store i32 42, i32* @i, align 4
  %2 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @0, i64 0, i64 0), i32 42, i8* getelementptr inbounds ([2 x i8], [2 x i8]* @1, i64 0, i64 0))
  %3 = load i32, i32* @i, align 4
  %4 = icmp sgt i32 %3, 0
  br i1 %4, label %5, label %7

; <label>:5:                                      ; preds = %1
  %6 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @2, i64 0, i64 0), i8* getelementptr inbounds ([5 x i8], [5 x i8]* @3, i64 0, i64 0))
  br label %9

; <label>:7:                                      ; preds = %1
  %8 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @4, i64 0, i64 0), i8* getelementptr inbounds ([4 x i8], [4 x i8]* @5, i64 0, i64 0), i8* getelementptr inbounds ([2 x i8], [2 x i8]* @6, i64 0, i64 0))
  br label %9

; <label>:9:                                      ; preds = %7, %5
  %10 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @7, i64 0, i64 0), i8* getelementptr inbounds ([17 x i8], [17 x i8]* @8, i64 0, i64 0), i8* getelementptr inbounds ([2 x i8], [2 x i8]* @9, i64 0, i64 0))
  ret i32 %0
}
