; ModuleID = 'MainModule'
source_filename = "test/arrayTest.pcl"

@i = common global [5 x i32] zeroinitializer
@s = common global [5 x i8] zeroinitializer
@f = common global [5 x double] zeroinitializer
@j = common global i32 0
@0 = private unnamed_addr constant [2 x i8] c"a\00"

define i32 @main(i32) {
  store i32 42, [5 x i32]* @i, align 4
  store i8* getelementptr inbounds ([2 x i8], [2 x i8]* @0, i64 0, i64 0), [5 x i8]* @s, align 8
  store double 4.200000e+01, [5 x double]* @f, align 8
  br label %2

; <label>:2:                                      ; preds = %5, %1
  %3 = load i32, i32* @j, align 4
  %4 = icmp slt i32 %3, 5
  br i1 %4, label %5, label %8

; <label>:5:                                      ; preds = %2
  %6 = load i32, i32* @j, align 4
  %7 = add i32 %6, 1
  store i32 %7, i32* @j, align 4
  br label %2

; <label>:8:                                      ; preds = %2
  ret i32 %0
}
