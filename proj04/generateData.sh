#!/bin/bash

timeArray=( 480 1440 10080 43200 86400 )

# for time in ${timeArray[@]}
# do
#     echo time = $time
#     ./mieds -e $time -r 10 example.sim 
# done

echo
echo Varying run data
echo

runArray=( 2 5 10 20 30 )

for run in ${runArray[@]}
do
    echo run = $run
    ./mieds -e 43200 -r $run example.sim
done