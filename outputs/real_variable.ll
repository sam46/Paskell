; ModuleID = 'MainModule'
source_filename = "test/real_variable.pcl"

@f = common global double 0.000000e+00
@str.main.entry.2 = private unnamed_addr constant [6 x i8] c"%lf%s\00"
@str.main.entry.4 = private unnamed_addr constant [2 x i8] c"\0A\00"
@str.main.if.then.8 = private unnamed_addr constant [5 x i8] c"%s%s\00"
@str.main.if.then.9 = private unnamed_addr constant [24 x i8] c"Double Variable Example\00"
@str.main.if.then.10 = private unnamed_addr constant [2 x i8] c"\0A\00"

declare i32 @printf(i8*, ...)

define i32 @main(i32 %main) {
main.entry:
  %0 = alloca i32
  store i32 %main, i32* %0
  store double 1.300000e+00, double* @f
  %1 = load double, double* @f
  %2 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([6 x i8], [6 x i8]* @str.main.entry.2, i32 0, i32 0), double %1, i8* getelementptr inbounds ([2 x i8], [2 x i8]* @str.main.entry.4, i32 0, i32 0))
  %3 = load double, double* @f
  %4 = fcmp olt double %3, 2.000000e+00
  br i1 %4, label %main.if.then, label %main.if.else

main.if.then:                                     ; preds = %main.entry
  %5 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @str.main.if.then.8, i32 0, i32 0), i8* getelementptr inbounds ([24 x i8], [24 x i8]* @str.main.if.then.9, i32 0, i32 0), i8* getelementptr inbounds ([2 x i8], [2 x i8]* @str.main.if.then.10, i32 0, i32 0))
  br label %main.if.exit

main.if.else:                                     ; preds = %main.entry
  br label %main.if.exit

main.if.exit:                                     ; preds = %main.if.else, %main.if.then
  %6 = load i32, i32* %0
  ret i32 %6
}
