#!/usr/bin/env bash
echo 'FuncTest'
.stack-work/dist/x86_64-linux-tinfo6/Cabal-2.4.0.1/build/Paskell/Paskell -c test/functest.pcl outputs/functest.ll
echo ''

echo 'ProcTest'
.stack-work/dist/x86_64-linux-tinfo6/Cabal-2.4.0.1/build/Paskell/Paskell -c test/proctest.pcl outputs/proctest.ll
echo ''

echo 'Hello'
.stack-work/dist/x86_64-linux-tinfo6/Cabal-2.4.0.1/build/Paskell/Paskell -c test/hello.pcl outputs/hello.ll
echo ''

echo 'Int Variable'
.stack-work/dist/x86_64-linux-tinfo6/Cabal-2.4.0.1/build/Paskell/Paskell -c test/int_variable.pcl outputs/int_variable.ll
echo ''

echo 'Real Variable'
.stack-work/dist/x86_64-linux-tinfo6/Cabal-2.4.0.1/build/Paskell/Paskell -c test/str_variable.pcl outputs/str_variable.ll
echo ''
