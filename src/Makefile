#
# Usage:
# > make clean;make STENCIL=0 [COMPRESSED=1]   
# > make clean;make STENCIL=125 REGISTER={8,16,32} [COMPRESSED=1] 
#


COMPRESSEDFLAG=
STENCILFLAG=
REGISTERFLAG=

ifdef STENCIL
STENCILFLAG= -DSTENCIL=$(STENCIL)
endif

ifdef REGISTER
REGISTERFLAG= -DREGISTER=$(REGISTER)
endif

ifdef COMPRESSED
COMPRESSEDFLAG= -DCOMPRESSED
endif

CC=gcc
CCFLAGS=-O3 $(STENCILFLAG) $(REGISTERFLAG) $(COMPRESSEDFLAG)
#CCFLAGS=-O3 -D$(MODE) -D$(REGSIZE) 

F90= gfortran
#F90FLAGS= -cpp -g -O3 -march=native -ffast-math -funroll-loops -ffree-form -D$(MODE) -D$(REGSIZE)
#ifdef COMPRESSED
#F90FLAGS= -cpp -O3 -ffree-form -D$(MODE) -D$(REGSIZE) -DCOMPRESSED
#else
#F90FLAGS= -cpp -O3 -ffree-form -D$(MODE) -D$(REGSIZE)
#endif
F90FLAGS= -cpp -O3 -ffree-form $(STENCILFLAG) $(REGISTERFLAG) $(COMPRESSEDFLAG)  
OPENMPFLAGS= -fopenmp
#OPENMPFLAGS= 
SERIALFLAGS= 

NVCC=/usr/local/cuda-5.5/bin/nvcc 
NVCCFLAGS=  -arch=sm_20 $(STENCILFLAG) $(REGISTERFLAG) $(COMPRESSEDFLAG) -lcuda 
#NVCCFLAGS= -arch=compute_20 --ptxas-options=-v
#F90= ifort
#F90FLAGS= -fpp -g -O3 -D$(MODE) -D$(REGSIZE)
#OPENMPFLAGS= -openmp

AR= ar
ARFLAGS= cr
RANLIB= ranlib

MPSGALIB= libmps-stencil.a

MPSGA_OBJECTS= \
	patternOperations.o 	\
	loadHistogram.o \
	fitnessFunction.o \
	fitnessFunctionGPU.o

IO_OBJECTS=
#AUX_OBJECTS= initRandomSeed.o qsort.o rng.o
#AUX_OBJECTS= rng.o fitnessFunctionCwrapper.o fitnessFunctionCUDAwrapper.o 
AUX_OBJECTS= rng.o fitnessFunctionCwrapper.o
MOD_OBJECTS= \
	patternoperations.mod \
	rng.mod

par: lib-par

lib-par: $(AUX_OBJECTS) $(IO_OBJECTS) $(MPSGA_OBJECTS)  
	$(AR) $(ARFLAGS) \
		$(MPSGALIB) \
		$(MPSGA_OBJECTS) \
		$(AUX_OBJECTS) \
		$(MOD_OBJECTS) 
	$(RANLIB) $(MPSGALIB)	
	cp $(MPSGALIB) ../lib

clean :
	        rm -f *.o *.mod *.a ../lib/*.a 

#.SUFFIXES: .o .f90 .fpp .c .cu
.SUFFIXES: .o .f90 .fpp .c

.f90.o : ; $(F90) -c $(F90FLAGS) $(OPENMPFLAGS) $*.f90

.fpp.o : ; $(F90) -c $(F90FLAGS) $(OPENMPFLAGS) $*.fpp

.c.o : ; $(CC) -c $(CCFLAGS) $(OPENMPFLAGS) $*.c

.cu.o : ; $(NVCC) -c $(NVCCFLAGS) $*.cu 
