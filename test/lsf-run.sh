#!/bin/bash 
# The total number of processes:
# 64 MPI processes * 2 OpenMP threads = 128 cores
#BSUB -n 16
#BSUB -oo job_%J.out
#BSUB -eo job_%J.err
# It will allocate 1 mpi proc per node:
#BSUB -R"span[ptile=16]"
#BSUB -x
#BSUB -J parsisim
#BSUB -W 01:00
##BSUB -q bsc_debug
#BSUB -q bsc_case

# You can choose the parallel environment through
# modules
#module load intel
#module load openmpi
module unload intel
module load gcc

#export EXTRAE_CONFIG_FILE=extrae.xml
#export EXTRAE_HOME=/apps/CEPBATOOLS/extrae/2.5.1/openmpi/64
##export LD_PRELOAD=${EXTRAE_HOME}/lib/libomptrace.so
#source ${EXTRAE_HOME}/etc/extrae.sh

#for i in 1 2 4 8 16 
for i in 1 
do

export OMP_NUM_THREADS=$i

#SIZE=200x200x200
SIZE=100x100x100
CAT=2c
ID=`date +%s`

echo '--| LSF: JOB SUBMITTED WITH SRUN AT: ' `date`
#/usr/bin/time mpirun /home/bsc21/bsc21021/Alya_repsol/Alya_MN/Executables/unix/Alya.x Invo4 > /gpfs/scratch/bsc21/bsc21021/outputs/output-MN-${ID}.txt
#/usr/bin/time /home/bsc21/bsc21021/parblusim/blusim.exe blusim-large.par > /home/bsc21/bsc21021/parblusim/output-MN-${ID}-${OMP_NUM_THREADS}.txt
for i in 1
do
/usr/bin/time /home/bsc21/bsc21021/mps-stencil/test/testFitness.exe params_${SIZE}_${CAT}.dat > /home/bsc21/bsc21021/mps-stencil/test/output-fix-${SIZE}-${CAT}-${OMP_NUM_THREADS}-${ID}.txt 2>&1
done
#mpirun -v /gpfs/apps/MN3/VALGRIND/3.8.1/bin/valgrind --error-limit=no --leak-check=full /home/bsc21/bsc21021/Alya_merge/Executables/unix/Alya.g Invo4
echo '--| LSF: JOB FINISHED AT: ' `date`


done

