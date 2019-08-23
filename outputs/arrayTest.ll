; ModuleID = 'MainModule'
source_filename = "test/arrayTest.pcl"

@i = common global [5 x i32] zeroinitializer

define i32 @main(i32) {
  store i32 4, [5 x i32]* @i, align 4
  ret i32 %0
}
