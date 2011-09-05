#!/bin/sh
#PBS -l nodes=5:ppn=1
#PBS -l walltime=12:00:00
#PBS -o output.txt
#PBS -j oe
#PBS -m ea
#PBS -M am2qa@virginia.edu
 
cd $PBS_O_WORKDIR
./a.out data25k.txt 0.5
