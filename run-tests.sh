#!/bin/sh
mkdir -p test-results
for x in 0 1 2 3 4 5 6 7 8 9
do
  for y in 0 1 2 3 4 5 6 7 8 9
  do
    for z in 0 1 2 3 4 5 6 7 8 9
    do
        seed=$x$y$z
        output=test-results/test-$seed.log
        error=test-results/error-$seed.log
        ./build/bin/agrippa-driver --random-seed=$seed > $output 2> $error
        if [ $(stat -c %s $error) -eq 0 ]
        then rm $error
        else echo Test $seed failed
        fi
    done
  done
done
