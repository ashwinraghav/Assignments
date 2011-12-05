 #!/bin/bash

# CS 6444 Homework 5
#
# Bash script to submit test jobs to the queue
#

ROWS=10000
COLS=10000
STEPS=10000

OUTPUT_DIR=ppm
LOG_DIR=log
STDOUT_DIR=stdout
STDERR_DIR=stderr

#PBS -l walltime=00:30:00
#PBS -N OpenCLCUDA
#PBS -o $STDOUT_DIR
#PBS -e $STDERR_DIR

cd $PBS_O_WORKDIR

mkdir -p $OUTPUT_DIR
mkdir -p $LOG_DIR
mkdir -p $STDOUT_DIR
mkdir -p $STDERR_DIR

# Test different configurations

for IMP in opencl cuda
do
       EXEC=./heated_plate_$IMP

       # "Warm up" run
       $EXEC 1000 1000 1000 > /dev/null 2>&1
       rm -f *.ppm

       for THREADS in 32 16 8
       do
               JOBNAME="$IMP-$THREADS"
               LOG_FILE="$JOBNAME-`hostname`.log"

               $EXEC $ROWS $COLS $STEPS $THREADS > log/$LOG_FILE 2>&1
               hostname >> log/$LOG_FILE
               mv snapshot_$IMP.$STEPS.ppm $OUTPUT_DIR/
       done
done
