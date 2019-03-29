#!/bin/sh
mkdir -p test-results
for x in 0 1 2 3 4 5 6 7 8 9
do
  for y in 0 1 2 3 4 5 6 7 8 9
  do
    ./build/bin/agrippa-driver --random-seed=$x$y > test-results/test-$x$y.log 2> test-results/error-$x$y.log
  done
done
