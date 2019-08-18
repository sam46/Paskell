; ModuleID = 'MainModule'
source_filename = "test/embeddedFun.pcl"

@str.say.entry.2 = private unnamed_addr constant [5 x i8] c"%s%s\00"
@str.say.entry.4 = private unnamed_addr constant [2 x i8] c"\0A\00"
@str.main.entry.2 = private unnamed_addr constant [20 x i8] c"Inside say(): Hello\00"

declare i32 @printf(i8*, ...)

define void @say(i8* %s) {
say.entry:
  %0 = alloca i8*
  store i8* %s, i8** %0
  %1 = load i8*, i8** %0
  %2 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([5 x i8], [5 x i8]* @str.say.entry.2, i32 0, i32 0), i8* %1, i8* getelementptr inbounds ([2 x i8], [2 x i8]* @str.say.entry.4, i32 0, i32 0))
  ret void
}

define i32 @main(i32 %main) {
main.entry:
  %0 = alloca i32
  store i32 %main, i32* %0
  call void @say(i8* getelementptr inbounds ([20 x i8], [20 x i8]* @str.main.entry.2, i32 0, i32 0))
  %1 = load i32, i32* %0
  ret i32 %1
}
