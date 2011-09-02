#!/bin/sh
#PBS -l nodes=1:ppn=1
#PBS -l walltime=12:00:00
#PBS -o output_filename
#PBS -j oe
#PBS -m bea
#PBS -M userid@virginia.edu
 
cd $PBS_O_WORKDIR
./a.out
