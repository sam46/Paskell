; ModuleID = 'MainModule'
source_filename = "test/real_variable.pcl"

@f = common global double 0.000000e+00
@0 = private unnamed_addr constant [6 x i8] c"%lf%s\00"
@1 = private unnamed_addr constant [2 x i8] c"\0A\00"
@2 = private unnamed_addr constant [5 x i8] c"%s%s\00"
@3 = private unnamed_addr constant [24 x i8] c"Double Variable Example\00"
@4 = private unnamed_addr constant [2 x i8] c"\0A\00"

declare i32 @printf(i8*, ...)

define i32 @main(i32) {
  store double 1.300000e+00, double* @f, align 8
  %2 = tail call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([6 x i8], [6 x i8]* @0, i64 0, i64 0), double 1.300000e+00, i8* getelementptr inbounds ([2 x i8], [2 x i8]* @1, i64 0, i64 0))
  %3 = load double, double* @f, align 8
  %4 = fcmp olt double %3, 2.000000e+00
  br i1 %4, label %5, label %7

; <label>:5:                                      ; preds = %1
  %6 = tail call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @2, i64 0, i64 0), i8* getelementptr inbounds ([24 x i8], [24 x i8]* @3, i64 0, i64 0), i8* getelementptr inbounds ([2 x i8], [2 x i8]* @4, i64 0, i64 0))
  br label %7

; <label>:7:                                      ; preds = %1, %5
  ret i32 %0
}
