; ModuleID = 'MainModule'
source_filename = "test/int_variable.pcl"

@i = common global i32 0
@str.main.entry.2 = private unnamed_addr constant [5 x i8] c"%d%s\00"
@str.main.entry.4 = private unnamed_addr constant [2 x i8] c"\0A\00"
@str.main.if.then.8 = private unnamed_addr constant [3 x i8] c"%s\00"
@str.main.if.then.9 = private unnamed_addr constant [5 x i8] c"YES\0A\00"
@str.main.if.else.11 = private unnamed_addr constant [5 x i8] c"%s%s\00"
@str.main.if.else.12 = private unnamed_addr constant [4 x i8] c"NO\0A\00"
@str.main.if.else.13 = private unnamed_addr constant [2 x i8] c"\0A\00"
@str.main.if.exit.15 = private unnamed_addr constant [5 x i8] c"%s%s\00"
@str.main.if.exit.16 = private unnamed_addr constant [17 x i8] c"Integer Variable\00"
@str.main.if.exit.17 = private unnamed_addr constant [2 x i8] c"\0A\00"

declare i32 @printf(i8*, ...)

define i32 @main(i32 %main) {
main.entry:
  %0 = alloca i32
  store i32 %main, i32* %0
  store i32 42, i32* @i
  %1 = load i32, i32* @i
  %2 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @str.main.entry.2, i32 0, i32 0), i32 %1, i8* getelementptr inbounds ([2 x i8], [2 x i8]* @str.main.entry.4, i32 0, i32 0))
  %3 = load i32, i32* @i
  %4 = icmp sgt i32 %3, 0
  br i1 %4, label %main.if.then, label %main.if.else

main.if.then:                                     ; preds = %main.entry
  %5 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @str.main.if.then.8, i32 0, i32 0), i8* getelementptr inbounds ([5 x i8], [5 x i8]* @str.main.if.then.9, i32 0, i32 0))
  br label %main.if.exit

main.if.else:                                     ; preds = %main.entry
  %6 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @str.main.if.else.11, i32 0, i32 0), i8* getelementptr inbounds ([4 x i8], [4 x i8]* @str.main.if.else.12, i32 0, i32 0), i8* getelementptr inbounds ([2 x i8], [2 x i8]* @str.main.if.else.13, i32 0, i32 0))
  br label %main.if.exit

main.if.exit:                                     ; preds = %main.if.else, %main.if.then
  %7 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @str.main.if.exit.15, i32 0, i32 0), i8* getelementptr inbounds ([17 x i8], [17 x i8]* @str.main.if.exit.16, i32 0, i32 0), i8* getelementptr inbounds ([2 x i8], [2 x i8]* @str.main.if.exit.17, i32 0, i32 0))
  %8 = load i32, i32* %0
  ret i32 %8
}
