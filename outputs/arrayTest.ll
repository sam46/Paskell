; ModuleID = 'MainModule'
source_filename = "test/arrayTest.pcl"

@i = common global [5 x i32] zeroinitializer
@s = common global [5 x i8] zeroinitializer
@f = common global [5 x double] zeroinitializer
@j = common global i32 0

declare i32 @printf(i8*, ...)

define i32 @main(i32 %main) {
main.entry:
  %0 = alloca i32
  store i32 %main, i32* %0
  br label %main.while.test

main.while.test:                                  ; preds = %main.while.body, %main.entry
  %1 = load i32, i32* @j
  %2 = icmp slt i32 %1, 5
  br i1 %2, label %main.while.body, label %main.while.exit

main.while.body:                                  ; preds = %main.while.test
  store i32 1, [5 x i32]* @i
  %3 = load i32, i32* @j
  %4 = add i32 %3, 1
  store i32 %4, i32* @j
  br label %main.while.test

main.while.exit:                                  ; preds = %main.while.test
  %5 = load i32, i32* %0
  ret i32 %5
}