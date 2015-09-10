#!/bin/bash

export EXTRAE_CONFIG_FILE=extrae.xml
export EXTRAE_HOME=/home/operedo/utils/extrae
export PAPI_HOME=/home/operedo/utils/papi
export UNWIND_HOME=/usr 
source ${EXTRAE_HOME}/etc/extrae.sh

export OMP_NUM_THREADS=2

#make clean
#make MODE='STENCIL=125' REGSIZE='REGISTER=8'
./testFitness.exe
