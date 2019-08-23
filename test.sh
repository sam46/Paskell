#!/usr/bin/env bash

cd test/
files=`ls *.pcl | cut -d '.' -f1`
cd ..

compile=".stack-work/dist/x86_64-linux-tinfo6/Cabal-2.4.0.1/build/Paskell/Paskell"
for i in $files
do
  echo 'Compiling' $i
  $compile -c test/$i.pcl outputs/$i.ll
done
