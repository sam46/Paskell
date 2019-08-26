; ModuleID = 'MainModule'
source_filename = "test/arrayTest.pcl"

@i = common global [5 x i32] zeroinitializer
@j = common global i32 0
@0 = private unnamed_addr constant [12 x i8] c"Array: %d%s\00"
@1 = private unnamed_addr constant [2 x i8] c"\0A\00"

declare i32 @printf(i8*, ...)

define i32 @main(i32) {
  br label %2

; <label>:2:                                      ; preds = %5, %1
  %3 = phi i32 [ %11, %5 ], [ 0, %1 ]
  store i32 %3, i32* @j, align 4
  %4 = icmp slt i32 %3, 5
  br i1 %4, label %5, label %12

; <label>:5:                                      ; preds = %2
  %6 = load i32, i32* @j, align 4
  %7 = sext i32 %6 to i64
  %8 = getelementptr inbounds [5 x i32], [5 x i32]* @i, i64 0, i64 %7
  %9 = tail call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([12 x i8], [12 x i8]* @0, i64 0, i64 0), i32* nonnull %8, i8* getelementptr inbounds ([2 x i8], [2 x i8]* @1, i64 0, i64 0))
  %10 = load i32, i32* @j, align 4
  %11 = add i32 %10, 1
  br label %2

; <label>:12:                                     ; preds = %2
  ret i32 %0
}
