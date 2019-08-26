; ModuleID = 'MainModule'
source_filename = "test/malloc-free.pcl"

@ip = common global i32* null

declare void @free(i8*)

define i32 @main(i32) {
  tail call void @free(i8* bitcast (i32** @ip to i8*))
  ret i32 %0
}
