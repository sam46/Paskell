# Paskell
A (reduced) Pascal compiler in Haskell that compiles to LLVM  
Work in Progress, mostly finished!  

### Features:   
- Declarations: var, type (aliases)
- Types: integer, boolean, string, char, real
- Control Flow: if, while, for    
- Functions/Procedures 
- Pass by reference
- Nested Functions/Procedures (Not finished yet) 
- I/O: Write/Writeln

### Progress  
- [x] Lexing/Parsing 
- [x] Semantic Analysis/Type-Checking
- [x] Type-Annotated IR AST
- [x] IR pretty-printer
- [x] LLVM Code generation 


### Usage
Once the executable is built, it can be used to compile Pascal source files to llvm-ir, or internal IR used by the compiler:  

  `Paskell -c src`      compile to llvm-ir  
  `Paskell -c src dest` compile to llvm-ir and save in dest  
  `Paskell -ir src`     produce internal IR   
  `Paskell -x src`      execute pascal source. Equivalent to 
                        `Paskell -c src | lli`  
  `Paskell -h`          (for help)  
  
Example:

```
> ./Paskell -c fib.pas fib.ll
```

 Since the output is llvm-ir, we can leverage the many tools LLVM provide to:
 - execute it using the llvm interpreter  
    `> lli fib.ll`
 - convert it to bitcode llvm assembly (.bc)  
    `> llvm-as fib.ll -o fib.bc`
 - optimize the code using various settings, for example  
    `> opt -mem2reg fib.bc` 
 - translate it to a native assembly executable of a specific architecture (x86, ARM, etc)  
   `> llc -march=x86-64 fib.bc -o fib.s`
 - link many modules into one program 
 
### Building the compiler from source:
You need to have llvm installed
```
> sudo apt-get install llvm
```

Then, you can use Cabal or Stack.  
To build using Cabal:

```
> cd Paskell/
> cabal install -j
```
this will install all dependencies and produce an executable in 
`dist/build/Paskell/`
  
You can also build using Stack.

### Tests
Various tests, testing utilities and sample Pascal programs are   
at [src/tests.hs](https://github.com/sam46/Paskell/blob/master/src/tests.hs)  
All can be run from GHCI.

### TODO's
- finish nested functions/procedures:  
  this only requires pulling nested functions to global scope  
  and renaming them during the type-annotation pass
- constants: trivial to implement
- Read/Readln IO statements
- records
- arrays
- case statements

### Implementation
This is a 4-pass compiler:  

**pass 1**: lex/parsing  
**pass 2**: type checking  
**pass 3**: Constructing IR: type-annotation, type resolution, (future: identifier-renaming, nested-function extraction)  
**pass 4**: code gen  
  
### References
- [Language grammar](http://courses.washington.edu/css448/zander/Project/grammar.pdf)
- Stephen Diehl's Haskell [llvm-tutorial](http://www.stephendiehl.com/llvm/)
