; ModuleID = 'MainModule'
source_filename = "test/fibonacci.pcl"

@i = common global i32 0
@0 = private unnamed_addr constant [5 x i8] c"%s%s\00"
@1 = private unnamed_addr constant [32 x i8] c"Generating fibonacci sequence: \00"
@2 = private unnamed_addr constant [2 x i8] c"\0A\00"
@3 = private unnamed_addr constant [5 x i8] c"%d%s\00"
@4 = private unnamed_addr constant [2 x i8] c"\0A\00"

declare i32 @printf(i8*, ...)

define i32 @fib(i32, i32) {
  br label %tailrecurse

tailrecurse:                                      ; preds = %4, %2
  %accumulator.tr = phi i32 [ 1, %2 ], [ %8, %4 ]
  %.tr = phi i32 [ %0, %2 ], [ 0, %4 ]
  %.tr1 = phi i32 [ %1, %2 ], [ %7, %4 ]
  %3 = icmp sgt i32 %.tr1, 2
  br i1 %3, label %4, label %9

; <label>:4:                                      ; preds = %tailrecurse
  %5 = add i32 %.tr1, -1
  %6 = tail call i32 @fib(i32 0, i32 %5)
  %7 = add i32 %.tr1, -2
  %8 = add i32 %6, %accumulator.tr
  br label %tailrecurse

; <label>:9:                                      ; preds = %tailrecurse
  ret i32 %accumulator.tr
}

define i32 @main(i32) {
  %2 = tail call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @0, i64 0, i64 0), i8* getelementptr inbounds ([32 x i8], [32 x i8]* @1, i64 0, i64 0), i8* getelementptr inbounds ([2 x i8], [2 x i8]* @2, i64 0, i64 0))
  br label %3

; <label>:3:                                      ; preds = %6, %1
  %4 = load i32, i32* @i, align 4
  %5 = icmp slt i32 %4, 30
  br i1 %5, label %6, label %11

; <label>:6:                                      ; preds = %3
  %7 = load i32, i32* @i, align 4
  %8 = add i32 %7, 1
  store i32 %8, i32* @i, align 4
  %9 = tail call i32 @fib(i32 0, i32 %8)
  %10 = tail call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @3, i64 0, i64 0), i32 %9, i8* getelementptr inbounds ([2 x i8], [2 x i8]* @4, i64 0, i64 0))
  br label %3

; <label>:11:                                     ; preds = %3
  ret i32 %0
}
