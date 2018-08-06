#include <stdio.h> 


char* c;

char* z = "world";
int x = 0;

void whatever() {
    char* s;
    s = "hello";
    printf("%s%d%f\n",s, 123, 1.2);
    c = "ok";
}


@.str = private unnamed_addr constant [6 x i8] c"world\00", align 1
@z = global i8* getelementptr inbounds ([6 x i8], [6 x i8]* @.str, i32 0, i32 0), align 8
@x = global i32 0, align 4
@.str.1 = private unnamed_addr constant [6 x i8] c"hello\00", align 1
@.str.2 = private unnamed_addr constant [8 x i8] c"%s%d%f\0A\00", align 1
@.str.3 = private unnamed_addr constant [3 x i8] c"ok\00", align 1
@c = common global i8* null, align 8

; Function Attrs: noinline nounwind optnone uwtable
define void @whatever() #0 {
  %1 = alloca i8*, align 8
  store i8* getelementptr inbounds ([6 x i8], [6 x i8]* @.str.1, i32 0, i32 0), i8** %1, align 8
  %2 = load i8*, i8** %1, align 8
  %3 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([8 x i8], [8 x i8]* @.str.2, i32 0, i32 0), i8* %2, i32 123, double 1.200000e+00)
  store i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str.3, i32 0, i32 0), i8** @c, align 8
  ret void
}

declare i32 @printf(i8*, ...) #1