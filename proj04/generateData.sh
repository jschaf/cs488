#!/bin/bash

timeArray = ( 480 1440 10080 43200 86400 )

for time in ${timeArray[@]}
do
    ./mieds -e $time -r 10 example.sim 
done

runArray = ( 2 5 10 20 30 )

for run in ${runArray[@]}
do
    ./mieds -e 86400 -r run example.sim
done