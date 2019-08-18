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
.stack-work/dist/x86_64-linux-tinfo6/Cabal-2.4.0.1/build/Paskell/Paskell -c test/real_variable.pcl outputs/real_variable.ll
echo ''

echo 'Array Test'
.stack-work/dist/x86_64-linux-tinfo6/Cabal-2.4.0.1/build/Paskell/Paskell -c test/arrayTest.pcl outputs/arrayTest.ll
echo ''

echo 'Embedded Fun'
.stack-work/dist/x86_64-linux-tinfo6/Cabal-2.4.0.1/build/Paskell/Paskell -c test/embeddedFun.pcl outputs/embeddedFun.ll
echo ''

echo 'Comment'
.stack-work/dist/x86_64-linux-tinfo6/Cabal-2.4.0.1/build/Paskell/Paskell -c test/comments.pcl outputs/comments.ll
echo ''

echo 'Hello Comment'
.stack-work/dist/x86_64-linux-tinfo6/Cabal-2.4.0.1/build/Paskell/Paskell -c test/hellocomments.pcl outputs/hellocomments.ll
echo ''
