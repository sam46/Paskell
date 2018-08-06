; ModuleID = 'src/m1.c'
source_filename = "src/m1.c"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

@myg = common global i32 0, align 4
@.str = private unnamed_addr constant [3 x i8] c"%d\00", align 1

; Function Attrs: noinline nounwind optnone uwtable
define i32 @foo1(i32, i32) #0 {
  %3 = alloca i32, align 4
  %4 = alloca i32, align 4
  store i32 %0, i32* %3, align 4
  store i32 %1, i32* %4, align 4
  %5 = load i32, i32* %3, align 4
  %6 = load i32, i32* %4, align 4
  %7 = add nsw i32 %5, %6
  ret i32 %7
}

; Function Attrs: noinline nounwind optnone uwtable
define i32 @foo2(i32, i32) #0 {
  %3 = alloca i32, align 4
  %4 = alloca i32, align 4
  %5 = alloca i32, align 4
  store i32 %0, i32* %3, align 4
  store i32 %1, i32* %4, align 4
  %6 = load i32, i32* %3, align 4
  %7 = load i32, i32* %4, align 4
  %8 = add nsw i32 %6, %7
  store i32 %8, i32* %5, align 4
  %9 = load i32, i32* %5, align 4
  ret i32 %9
}

; Function Attrs: noinline nounwind optnone uwtable
define i32 @foo3(i32, i32) #0 {
  %3 = alloca i32, align 4
  %4 = alloca i32, align 4
  %5 = alloca i32, align 4
  %6 = alloca i32, align 4
  store i32 %0, i32* %3, align 4
  store i32 %1, i32* %4, align 4
  %7 = load i32, i32* %3, align 4
  %8 = load i32, i32* %4, align 4
  %9 = add nsw i32 %7, %8
  store i32 %9, i32* %5, align 4
  %10 = load i32, i32* %3, align 4
  %11 = load i32, i32* %4, align 4
  %12 = call i32 @foo2(i32 %10, i32 %11)
  %13 = load i32, i32* %5, align 4
  %14 = add nsw i32 %12, %13
  store i32 %14, i32* %6, align 4
  %15 = load i32, i32* %6, align 4
  ret i32 %15
}

; Function Attrs: noinline nounwind optnone uwtable
define i32 @pbr(i32, i32*) #0 {
  %3 = alloca i32, align 4
  %4 = alloca i32*, align 8
  store i32 %0, i32* %3, align 4
  store i32* %1, i32** %4, align 8
  %5 = load i32, i32* %3, align 4
  %6 = load i32*, i32** %4, align 8
  store i32 %5, i32* %6, align 4
  %7 = load i32, i32* %3, align 4
  ret i32 %7
}

; Function Attrs: noinline nounwind optnone uwtable
define i32 @foo4() #0 {
  %1 = alloca i32, align 4
  %2 = alloca i32, align 4
  store i32 1, i32* %1, align 4
  store i32 2, i32* %2, align 4
  %3 = load i32, i32* %1, align 4
  %4 = call i32 @pbr(i32 %3, i32* %2)
  %5 = load i32, i32* %1, align 4
  %6 = add nsw i32 %4, %5
  %7 = load i32, i32* %2, align 4
  %8 = add nsw i32 %6, %7
  ret i32 %8
}

; Function Attrs: noinline nounwind optnone uwtable
define i32 @foo5() #0 {
  %1 = alloca i32, align 4
  %2 = alloca i32*, align 8
  store i32 1, i32* %1, align 4
  %3 = load i32*, i32** %2, align 8
  store i32 2, i32* %3, align 4
  %4 = load i32, i32* %1, align 4
  %5 = load i32*, i32** %2, align 8
  %6 = call i32 @pbr(i32 %4, i32* %5)
  %7 = load i32, i32* %1, align 4
  %8 = add nsw i32 %6, %7
  %9 = load i32*, i32** %2, align 8
  %10 = load i32, i32* %9, align 4
  %11 = add nsw i32 %8, %10
  ret i32 %11
}

; Function Attrs: noinline nounwind optnone uwtable
define void @foo() #0 {
  %1 = alloca i32, align 4
  %2 = alloca i32, align 4
  %3 = alloca i32, align 4
  %4 = alloca i32, align 4
  %5 = alloca i32, align 4
  %6 = alloca i32, align 4
  %7 = alloca float, align 4
  %8 = alloca float, align 4
  %9 = alloca i32, align 4
  %10 = alloca i32, align 4
  store i32 7, i32* %1, align 4
  store i32 1, i32* %2, align 4
  store i32 0, i32* %3, align 4
  store i32 0, i32* %4, align 4
  %11 = load i32, i32* %2, align 4
  %12 = icmp ne i32 %11, 0
  br i1 %12, label %16, label %13

; <label>:13:                                     ; preds = %0
  %14 = load i32, i32* %3, align 4
  %15 = icmp ne i32 %14, 0
  br label %16

; <label>:16:                                     ; preds = %13, %0
  %17 = phi i1 [ true, %0 ], [ %15, %13 ]
  %18 = zext i1 %17 to i32
  store i32 %18, i32* %5, align 4
  %19 = load i32, i32* %2, align 4
  %20 = icmp ne i32 %19, 0
  br i1 %20, label %21, label %24

; <label>:21:                                     ; preds = %16
  %22 = load i32, i32* %1, align 4
  %23 = icmp sgt i32 %22, 2
  br label %24

; <label>:24:                                     ; preds = %21, %16
  %25 = phi i1 [ false, %16 ], [ %23, %21 ]
  %26 = zext i1 %25 to i32
  store i32 %26, i32* %6, align 4
  %27 = load i32, i32* %1, align 4
  %28 = sitofp i32 %27 to double
  %29 = fadd double %28, 2.500000e+00
  %30 = fptrunc double %29 to float
  store float %30, float* %7, align 4
  %31 = load i32, i32* %1, align 4
  %32 = sitofp i32 %31 to double
  %33 = fmul double %32, 2.500000e+00
  %34 = fptrunc double %33 to float
  store float %34, float* %8, align 4
  %35 = load i32, i32* %1, align 4
  store i32 %35, i32* %9, align 4
  %36 = load i32, i32* %1, align 4
  %37 = sub nsw i32 0, %36
  store i32 %37, i32* %10, align 4
  ret void
}

; Function Attrs: noinline nounwind optnone uwtable
define void @f6() #0 {
  %1 = alloca i32, align 4
  store i32 1, i32* %1, align 4
  %2 = load i32, i32* %1, align 4
  %3 = icmp sgt i32 %2, 1
  br i1 %3, label %4, label %5

; <label>:4:                                      ; preds = %0
  store i32 1, i32* %1, align 4
  br label %6

; <label>:5:                                      ; preds = %0
  store i32 2, i32* %1, align 4
  br label %6

; <label>:6:                                      ; preds = %5, %4
  ret void
}

; Function Attrs: noinline nounwind optnone uwtable
define void @ff() #0 {
  %1 = alloca i32, align 4
  store i32 1, i32* %1, align 4
  br label %2

; <label>:2:                                      ; preds = %5, %0
  %3 = load i32, i32* %1, align 4
  %4 = icmp sgt i32 %3, 1
  br i1 %4, label %5, label %6

; <label>:5:                                      ; preds = %2
  store i32 2, i32* %1, align 4
  br label %2

; <label>:6:                                      ; preds = %2
  ret void
}

; Function Attrs: noinline nounwind optnone uwtable
define void @ffor() #0 {
  %1 = alloca i32, align 4
  %2 = alloca i32, align 4
  store i32 0, i32* %2, align 4
  br label %3

; <label>:3:                                      ; preds = %7, %0
  %4 = load i32, i32* %2, align 4
  %5 = icmp slt i32 %4, 10
  br i1 %5, label %6, label %10

; <label>:6:                                      ; preds = %3
  store i32 1, i32* %1, align 4
  br label %7

; <label>:7:                                      ; preds = %6
  %8 = load i32, i32* %2, align 4
  %9 = add nsw i32 %8, 1
  store i32 %9, i32* %2, align 4
  br label %3

; <label>:10:                                     ; preds = %3
  ret void
}

; Function Attrs: noinline nounwind optnone uwtable
define void @bar() #0 {
  store i32 12, i32* @myg, align 4
  ret void
}

; Function Attrs: noinline nounwind optnone uwtable
define void @printer() #0 {
  %1 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str, i32 0, i32 0), i32 123)
  ret void
}

declare i32 @printf(i8*, ...) #1

attributes #0 = { noinline nounwind optnone uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }

!llvm.module.flags = !{!0}
!llvm.ident = !{!1}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{!"clang version 5.0.2 (tags/RELEASE_502/final)"}
