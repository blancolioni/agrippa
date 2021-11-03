#!/bin/sh
mkdir -p test-results
seed=$1
output=test-results/test-$seed.log
error=test-results/error-$seed.log
./build/bin/agrippa-driver --text-ui --random-seed=$seed > $output 2> $error
if [ $(stat -c %s $error) -eq 0 ]
then rm $error
else echo Test $seed failed
fi
