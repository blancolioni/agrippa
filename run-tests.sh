#!/bin/sh
mkdir -p test-results
for seed in 1 2 3 4 5 6 7 8 9
do
   ./build/bin/agrippa-driver --random-seed=$seed > test-results/test-$seed.log 2> test-results/error-$seed.log
done
