
// alloca: reserves memory and returns a pointer (like int*, not a value)
// store v p: stores v at memory address p 
// load takes a memory address and stores a variable into it. 
//   When reading a value at a pointer, one needs to create a variable to hold it,
//   and that's the only way to read the value in that memory location
//   e.g. int* a;  int x = load a;


////////////////////////////////////////////

int foo1 (int a, int b) {
    return a+b;
}

// ; Function Attrs: noinline nounwind optnone uwtable
// define i32 @foo1(i32, i32) #0 {
//   %3 = alloca i32, align 4        // a pointer
//   %4 = alloca i32, align 4        // b pointer
//   store i32 %0, i32* %3, align 4  // store %0 at %3
//   store i32 %1, i32* %4, align 4  // store %1 at %4
//   %5 = load i32, i32* %3, align 4 // load the value from memory location  pointed to by %3 into %5
//   %6 = load i32, i32* %4, align 4 // load the value from memory location  pointed to by %3 into %5
//   %7 = add nsw i32 %5, %6
//   ret i32 %7
// }

////////////////////////////////////////////

int foo2 (int a, int b) {
    int c = a + b;
    return c;
}

// define @foo (i32, i32) {
//     %3 = alloca i32, // int* %3 
//     %4 = alloca i32, // int* %4
//     %5 = alloca i32, // int* %5

//     store i32, i32 %0, i32* %3 // *(%3) = %0
//     store i32, i32 %1, i32* %4 // *(%4) = %1

//     %6 = load i32, i32* %3     // %6 = load %3 
//     %7 = load i32, i32* %4     // %7 = load %4 

//     %8 = add nsw i32 %6, %7    // %8 = %6 + %7
//     store i32, i32 %8, i32* %5 // *(%5) = %8

//     %9 = load i32, i32* %5     // %9 = load %5
//     ret i32 %9

// }


// ; Function Attrs: noinline nounwind optnone uwtable
// define i32 @foo2(i32, i32) #0 {
//   %3 = alloca i32, align 4
//   %4 = alloca i32, align 4
//   %5 = alloca i32, align 4
//   store i32 %0, i32* %3, align 4
//   store i32 %1, i32* %4, align 4
//   %6 = load i32, i32* %3, align 4
//   %7 = load i32, i32* %4, align 4
//   %8 = add nsw i32 %6, %7
//   store i32 %8, i32* %5, align 4
//   %9 = load i32, i32* %5, align 4
//   ret i32 %9
// }


////////////////////////////////////////////

int foo3 (int a, int b) {
    int c = a + b;
    int d = foo2(a,b) + c;
    return d;
}




// define i32 @foo3 (i32, i32) {
//     %3 = alloca i32, // int* %3 
//     %4 = alloca i32, // int* %4
//     %5 = alloca i32, // int* %5

//     store i32, i32 %0, i32* %3 // *(%3) = %0
//     store i32, i32 %1, i32* %4 // *(%4) = %1

//     %6 = load i32, i32* %3     // %6 = load %3   a value
//     %7 = load i32, i32* %4     // %7 = load %4   b value

//     %8 = add nsw i32 %6, %7    // %8 = %6 + %7
//     store i32, i32 %8, i32* %5 // *(%5) = %8

//     ///////////////////////////

//     %9 = alloca i32, // int* %9   for d


//     %10 = load i32, i32* %3     // %10 = load %3   a value
//     %11 = load i32, i32* %4     // %11 = load %4   b value

//     %12 = call i32 @foo2(i32 %10, i32 %11)
//     %13 = load i32* %5           // c value

//     %14 = add i32, i32 %12, i32 %13
//     store i32, i32 %14, i32* %9

//     %15 = load i32, i32* %9
//     ret i32 %15
// }




// ; Function Attrs: noinline nounwind optnone uwtable
// define i32 @foo3(i32, i32) #0 {
//   %3 = alloca i32, align 4
//   %4 = alloca i32, align 4
//   %5 = alloca i32, align 4
//   %6 = alloca i32, align 4
//   store i32 %0, i32* %3, align 4
//   store i32 %1, i32* %4, align 4
//   %7 = load i32, i32* %3, align 4
//   %8 = load i32, i32* %4, align 4
//   %9 = add nsw i32 %7, %8
//   store i32 %9, i32* %5, align 4
//   %10 = load i32, i32* %3, align 4
//   %11 = load i32, i32* %4, align 4
//   %12 = call i32 @foo2(i32 %10, i32 %11)
//   %13 = load i32, i32* %5, align 4
//   %14 = add nsw i32 %12, %13
//   store i32 %14, i32* %6, align 4
//   %15 = load i32, i32* %6, align 4
//   ret i32 %15
// }


int pbr(int a, int* b) {
    *b = a;
    return a; 
}

// define i32 @pbr (i32, i32*) {
//     %3 = alloca i32
//     %4 = alloca i32*

//     store  i32 %0, i32* %3
//     store  i32* %1, i32** %4

//     %5 = load i32, i32* %3 // a value
//     %x = load i32*, i32** %4
//     store i32, i32 %5, i32* %x

//     %6 = load i32, i32* %3
//     ret i32 %6
// }



// ; Function Attrs: noinline nounwind optnone uwtable
// define i32 @pbr(i32, i32*) #0 {
//   %3 = alloca i32, align 4
//   %4 = alloca i32*, align 8

//   store i32 %0, i32* %3, align 4
//   store i32* %1, i32** %4, align 8

//   %5 = load i32, i32* %3, align 4
//   %6 = load i32*, i32** %4, align 8
//   store i32 %5, i32* %6, align 4

//   %7 = load i32, i32* %3, align 4
//   ret i32 %7
// }


//////////////////////////////////////////////


int foo4() {
    int a = 1;
    int b = 2;
    return pbr(a,&b) + a + b;
}

// define i32 @foo4() {
//     %1 = alloca i32
//     %2 = alloca i32
//     store i32, i32 1, i32* %1
//     store i32, i32 2, i32* %2
    

//     %7 = load i32, i32* %1
//     %8 = call i32 @pbr(i32 %7, i32* %2)

//     %4 = load i32, i32* %1
//     %5 = load i32, i32* %2
//     %6 = add i32, i32 %4, i32 %5

    

//     %9 = add i32, i32 %6, i32 %8
//     ret %9
// }



// ; Function Attrs: noinline nounwind optnone uwtable
// define i32 @foo4() #0 {
//   %1 = alloca i32, align 4
//   %2 = alloca i32, align 4
//   store i32 1, i32* %1, align 4
//   store i32 2, i32* %2, align 4

//   %3 = load i32, i32* %1, align 4
//   %4 = call i32 @pbr(i32 %3, i32* %2)

//   %5 = load i32, i32* %1, align 4
//   %6 = add nsw i32 %4, %5
//   %7 = load i32, i32* %2, align 4
//   %8 = add nsw i32 %6, %7
//   ret i32 %8
// }


//////////////////////////////////////////

int foo5() {
    int a = 1;
    int* b;
    *b = 2;
    return pbr(a,b) + a + (*b);
}


// define i32 @foo4() {
//     %0 = alloca i32
//     %1 = alloca i32*
//     store i32, i32 1, i32* %0
//     store i32, i32 2, i32* %1
    
//     %2 = alloca i32
//     %3 = load i32, i32* %0
//     %4 = load i32, i32* %1
//     %5 = add i32, i32 %3, i32 %4

    
//     %6 = load i32, i32* %0
//     %7 = call i32 @pbr(i32 %6, i32* %1)

//     %8 = add i32, i32 %5, i32 %7
//     ret %8
// }


void foo(){
    int x = 2.5*3;
    int y = 1 && 0 || 1;
    int z = 1 > 2;
    int w = 1.5 > 2;

    int c = y || z;
    int d = y && (x>2);

    float g = x + 2.5;
    float h = x * 2.5;

    int u = +x;
    int v = -x;
}

// ; Function Attrs: noinline nounwind optnone uwtable
// define void @foo() #0 {
//   %1 = alloca i32, align 4
//   %2 = alloca i32, align 4
//   %3 = alloca i32, align 4
//   %4 = alloca i32, align 4
//   %5 = alloca i32, align 4
//   %6 = alloca i32, align 4
//   %7 = alloca float, align 4
//   %8 = alloca float, align 4
//   %9 = alloca i32, align 4
//   %10 = alloca i32, align 4
//   store i32 7, i32* %1, align 4
//   store i32 1, i32* %2, align 4
//   store i32 0, i32* %3, align 4
//   store i32 0, i32* %4, align 4
//   %11 = load i32, i32* %2, align 4
//   %12 = icmp ne i32 %11, 0
//   br i1 %12, label %16, label %13

// ; <label>:13:                                     ; preds = %0
//   %14 = load i32, i32* %3, align 4
//   %15 = icmp ne i32 %14, 0
//   br label %16

// ; <label>:16:                                     ; preds = %13, %0
//   %17 = phi i1 [ true, %0 ], [ %15, %13 ]
//   %18 = zext i1 %17 to i32
//   store i32 %18, i32* %5, align 4
//   %19 = load i32, i32* %2, align 4
//   %20 = icmp ne i32 %19, 0
//   br i1 %20, label %21, label %24

// ; <label>:21:                                     ; preds = %16
//   %22 = load i32, i32* %1, align 4
//   %23 = icmp sgt i32 %22, 2
//   br label %24

// ; <label>:24:                                     ; preds = %21, %16
//   %25 = phi i1 [ false, %16 ], [ %23, %21 ]
//   %26 = zext i1 %25 to i32
//   store i32 %26, i32* %6, align 4
//   %27 = load i32, i32* %1, align 4
//   %28 = sitofp i32 %27 to double
//   %29 = fadd double %28, 2.500000e+00
//   %30 = fptrunc double %29 to float
//   store float %30, float* %7, align 4
//   %31 = load i32, i32* %1, align 4
//   %32 = sitofp i32 %31 to double
//   %33 = fmul double %32, 2.500000e+00
//   %34 = fptrunc double %33 to float
//   store float %34, float* %8, align 4
//   %35 = load i32, i32* %1, align 4
//   store i32 %35, i32* %9, align 4
//   %36 = load i32, i32* %1, align 4
//   %37 = sub nsw i32 0, %36
//   store i32 %37, i32* %10, align 4
//   ret void
// }


///////////////////////////////////////////////////


void f6() {
    int x = 1;
    if (x > 1)
        x = 1;
    else x = 2; 
}

// ; Function Attrs: noinline nounwind optnone uwtable
// define void @f6() #0 {
  
//   %1 = alloca i32, align 4
//   store i32 1, i32* %1, align 4

//   // if (x > 1)
//   %2 = load i32, i32* %1, align 4
//   %3 = icmp sgt i32 %2, 1
//   br i1 %3, label %4, label %5

// // then
// ; <label>:4:                                      ; preds = %0
//   store i32 1, i32* %1, align 4
//   br label %6

// // else
// ; <label>:5:                                      ; preds = %0
//   store i32 2, i32* %1, align 4
//   br label %6

// ; <label>:6:                                      ; preds = %5, %4
//   ret void
// }

void ff() {
  int x = 1;
  while (x > 1) 
    x = 2;
}

// ; Function Attrs: noinline nounwind optnone uwtable
// define void @ff() #0 {
//   %1 = alloca i32, align 4
//   store i32 1, i32* %1, align 4
//   br label %2

// ; <label>:2:                                      ; preds = %5, %0
//   %3 = load i32, i32* %1, align 4
//   %4 = icmp sgt i32 %3, 1
//   br i1 %4, label %5, label %6

// ; <label>:5:                                      ; preds = %2
//   store i32 2, i32* %1, align 4
//   br label %2

// ; <label>:6:                                      ; preds = %2
//   ret void
// }

///////////////////////////////////////////////

void ffor() {
  int y;
  for (int x = 0; x<10; x++) 
    y = 1;
}

// ; Function Attrs: noinline nounwind optnone uwtable
// define void @ffor() #0 {
//   %1 = alloca i32, align 4

//   %2 = alloca i32, align 4   // x
//   store i32 0, i32* %2, align 4 // x = 0

//   br label %3

//     // if x < 10
// ; <label>:3:                                      ; preds = %7, %0
//   %4 = load i32, i32* %2, align 4
//   %5 = icmp slt i32 %4, 10      
//   br i1 %5, label %6, label %10  

//  // then y = 1
// ; <label>:6:                                      ; preds = %3
//   store i32 1, i32* %1, align 4
//   br label %7

//   // x++
// ; <label>:7:                                      ; preds = %6
//   %8 = load i32, i32* %2, align 4
//   %9 = add nsw i32 %8, 1
//   store i32 %9, i32* %2, align 4
//   br label %3

// ; <label>:10:                                     ; preds = %3
//   ret void
// }

int myg;

void bar() {
    myg = 12;
}

#include <stdio.h> 

void printer() {
    printf("%d", 123);
}
