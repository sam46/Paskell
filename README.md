# Paskell
[![Build Status](https://travis-ci.org/sam46/Paskell.svg?branch=master)](https://travis-ci.org/sam46/Paskell)
A (reduced) Pascal compiler in Haskell that compiles to LLVM

- [Paskell](#paskell)
    + [Features](#features)
    + [Progress](#progress)
    + [Usage](#usage)
    + [Demo](#demo)
    + [Building](#building)
        * [With docker](#with-docker)
        * [Without docker](#without-docker)
    + [Tests](#tests)
    + [Implementation](#implementation)
    + [TODO](#todo)
    + [Contributions](#contributions)
    + [References](#references)
  
### Features   
- Declarations: var, type (aliases)
- Types: integer, boolean, string, char, real
- Control Flow: if, while, for    
- Functions/Procedures 
- Pass by reference
- Basic Typecasting
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

  `paskell -c src`      compile to llvm-ir  
  `paskell -c src dest` compile to llvm-ir and save in dest  
  `paskell -ir src`     produce internal IR   
  `paskell -x src`      execute pascal source. Equivalent to 
                        `paskell -c src | lli`  
  `paskell -h`          (for help)  
  
### Demo:
The compiler is complemented with the `llvm` utilities 
```
$ paskell -c fib.pas fib.ll
```

 Since the output is llvm-ir, we can leverage the many tools LLVM provide to:
 - execute it using the llvm interpreter  
    `$ lli fib.ll`
 - convert it to bitcode llvm assembly (.bc)  
    `$ llvm-as fib.ll -o fib.bc`
 - optimize the code using various settings, for example  
    `$ opt -mem2reg fib.bc` 
 - translate it to a native assembly executable of a specific architecture (x86, ARM, etc)  
   `$ llc -march=x86-64 fib.bc -o fib.s`
 - link many modules into one program 

### Building

##### With docker
```
$ make bash
```
to build the compiler and launch a shell session where the compiler and llvm utitlies are in `$PATH` and ready out-of-the-box.

###### Alternatively
```
$ make build
```
will build the same image tagged `paskell`
which can be used with `docker run` and volumes.
For example:
```
$ docker run -v /path/to/original_file.pas:/path/to/file.pas paskell paskell -c /path/to/file.pas
```

##### Without docker
You need to have llvm installed
```
$ sudo apt-get install llvm-5.0
```
`lli` should be in `$PATH` to be able to execute Pascal programs

Then, you can use Cabal or Stack.  
To build using Cabal:

```
$ cd Paskell/
$ cabal install -j
```
this will install all dependencies and produce an executable in 
`dist/build/Paskell/`
  
You can also build using Stack.

### Tests
```
$ make test
```
to run the test suite using docker.

### Implementation
This is a 4-pass compiler:  

**pass 1**: lex/parsing  
**pass 2**: type checking  
**pass 3**: constructing IR: type-annotation, type resolution, (future: identifier-renaming, nested-function extraction)  
**pass 4**: code generation  
  
### TODO
- finish nested functions/procedures:  
  this only requires pulling nested functions to global scope  
  and renaming them during the type-annotation pass
- constants: trivial to implement
- Read/Readln IO statements
- records
- arrays
- case statements
- forward declaration
  
### Contributions    
Bug reports, added features, etc are welcome  

### References
- [Language grammar](http://courses.washington.edu/css448/zander/Project/grammar.pdf)
- Stephen Diehl's Haskell [llvm-tutorial](http://www.stephendiehl.com/llvm/)
