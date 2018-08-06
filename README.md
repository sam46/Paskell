# Paskell
A (reduced) Pascal compiler in Haskell that compiles to LLVM  
Work in Progress, mostly finished!  

### Features:   
- Declarations: var 
- Types: integer, boolean, string, char, real
- Control Flow: if, while, for    
- Functions/Procedures 
- Nested Functions/Procedures (Not finished yet) 
- I/O: Write/Writeln

### Progress  
- [x] Lexing/Parsing 
- [x] Semantic Analysis/Type-Checking
- [x] Type-Annotated IR AST
- [x] IR pretty-printer
- [x] LLVM Code generation 

### Demo
#### A REPL demonstrating the parser:  

![parserepl.gif](https://github.com/sam46/Paskell/blob/master/parserepl.PNG)
  
#### Pretty printer for type-annotated AST:  

Input source File:
```Pascal
program p;

var x,y,a:real;

function f1 (x,t:integer):boolean;

    function f2 (x,a:boolean):boolean;
    begin
        t:=5;
        x:=true;
        a:=false;
        y:=2.5
    end;
begin
    x:=1;
    t:=2;
    y:=4.5;
    if f2(true,false) then a:=5.5
    else if 2>3 then x:=1 else
end;

function f3 (c,x:integer):real;
var d : boolean;
begin
    x:=1;
    c:=3;
    y:=3.4;
    d:= f1(x,1);
end;

begin 
    x:=f3(1,2);
    x:=f3(1+2*3,-1*2+3)
end.
```

Output AST:

```
Program p {
    Var ("x",TYreal) ("y",TYreal) ("a",TYreal);
    Func f1:TYbool (("x",TYint), ("t",TYint)) {
        Func f2:TYbool (("x",TYbool), ("a",TYbool)) {

            t:TYint := 5:TYint;
            x:TYbool := True:TYbool;
            a:TYbool := False:TYbool;
            y:TYreal := 2.5:TYreal;

        }

        x:TYint := 1:TYint;
        t:TYint := 2:TYint;
        y:TYreal := 4.5:TYreal;
        if f2(True:TYbool, False:TYbool):TYbool
        then
            a:TYreal := 5.5:TYreal;
        else
            if (2:TYint>3:TYint):TYbool
            then
                x:TYint := 1:TYint;
            else
                ;
        ;

    }
    Func f3:TYreal (("c",TYint), ("x",TYint)) {
        Var ("d",TYbool);

        x:TYint := 1:TYint;
        c:TYint := 3:TYint;
        y:TYreal := 3.4:TYreal;
        d:TYbool := f1(x:TYint:TYint, 1:TYint):TYbool;
        ;

    }

    x:TYreal := f3(1:TYint, 2:TYint):TYreal;
    x:TYreal := f3((1:TYint+(2:TYint*3:TYint):TYint):TYint, (-((1:TYint*2:TYint):TYint+3:TYint):TYint):TYint):TYreal;
}
```



#### Reference 
- [Language grammar](http://courses.washington.edu/css448/zander/Project/grammar.pdf)
