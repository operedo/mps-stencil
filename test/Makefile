#
# Usage:
# > make clean;make MODE='BASELINE' REGSIZE='REGISTER=32' 
# > make clean;make MODE='STENCIL=125' REGSIZE='REGISTER=8' 
# > make clean;make MODE='STENCIL=125' REGSIZE='REGISTER=16' 
# > make clean;make MODE='STENCIL=125' REGSIZE='REGISTER=32' 
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



F90= gfortran
#ifdef COMPRESSED
#F90FLAGS= -cpp -O3 -ffree-form -D$(MODE) -D$(REGSIZE) -DCOMPRESSED
#else
#F90FLAGS= -cpp -O3 -ffree-form -D$(MODE) -D$(REGSIZE)
#endif

F90FLAGS= -cpp -O3 -ffree-form $(STENCILFLAG) $(REGISTERFLAG) $(COMPRESSEDFLAG)  

NVCCFLAGS=
#NVCCFLAGS= -L /usr/local/cuda/lib64 -lcuda -lcudart
OPENMPFLAGS= -fopenmp 
#OPENMPFLAGS=

#F90= ifort
#F90FLAGS= -fpp -O3 -D$(MODE) -D$(REGSIZE)
#OPENMPFLAGS= -openmp 

#MPSGALIB= ../lib/libmps-stencil.a -lgomp -lm 
MPSGALIB= ../lib/libmps-stencil.a -lm -lstdc++ 

all: testFitness

testFitness: testFitness.o
	$(F90) $(F90FLAGS) $(OPENMPFLAGS) testFitness.o \
		-o testFitness.exe \
		$(MPSGALIB) $(NVCCFLAGS)  
#	cd ../src; make clean;make MODE='$(MODE)' REGISTER='$(REGSIZE)'; cd -

instr:
	$(F90) $(F90FLAGS) $(OPENMPFLAGS) -g -c extrae_module.f90
	$(F90) $(F90FLAGS) $(OPENMPFLAGS) -DTRACE -g -c -I. -I../src testFitness.fpp
	$(F90) $(F90FLAGS) $(OPENMPFLAGS) -DTRACE -g -I. -I../src -I/home/operedo/utils/extrae/include testFitness.o \
               -o testFitness.exe -L/home/operedo/utils/extrae/lib64 -lomptrace -L/usr/lib64 -lunwind \
               -L/home/operedo/utils/papi/lib64 -lpapi -L/usr/lib64 -lbfd -L/usr/lib64 -liberty -lxml2 /lib64/librt-2.15.so $(MPSGALIB)

clean :
	        rm -f *.o *.exe *.mod

.SUFFIXES: .o .f90 .fpp

.f90.o : ; $(F90) -c $(F90FLAGS) $(OPENMPFLAGS) $*.f90 -I ../src

.fpp.o : ; $(F90) -c $(F90FLAGS) $(OPENMPFLAGS) $*.fpp -I ../src

