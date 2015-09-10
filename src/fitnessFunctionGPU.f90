
#include "header.h"

subroutine fitnessFunctionGPU(realization_matrix,rows,cols,slides,&
                         tem_rows, tem_cols, tem_slides, value, tem_cells, &
                         tem_cells_rows,tem_cells_cols,tem_cells_slides)

  use patternOperations
#ifdef _OPENMP
  use omp_lib
#endif

  integer(4), intent(in)    :: rows, cols, slides
  integer(4), intent(in)    :: realization_matrix(rows,cols,slides)
  integer(4), intent(in)    :: tem_rows, tem_cols, tem_slides
  real(8), intent(inout)    :: value
  integer(4), intent(in)    :: tem_cells
  integer(4), intent(in)    :: tem_cells_rows(tem_cells),&
                               tem_cells_cols(tem_cells),&
                               tem_cells_slides(tem_cells)
  integer(4)                :: ii,jj,kk,irow,icol,islide,jrow,jcol,jslide,pos
  integer(4)                :: localPattern(tem_rows*tem_cols*tem_slides)
  integer(4)                :: freq_realization(npatterns),freq_realization_byid(npatterns,12)
  integer(4)                :: idthread,numthreads
  integer(8)                 :: clock_start, clock_end, clock_rate

  integer :: fitnessFunctionCwrapper, fitnessFunctionCUDAwrapper, valueGPU,valueFortran

! begin: test cuda kernel in fortran 90
  real(4)   :: valueSingle
  real(8)   :: timeFortran, timeCUDA

#if defined(MODE) && MODE==1

  freq_realization_byid=0

  do ii=1,npatterns
     freq_realization(ii)=frequency(ii)
  end do

  value=0.0
  pos=-1

!!$OMP PARALLEL FIRSTPRIVATE(localPattern,pos,icol,irow,islide,idthread)
!#ifdef _OPENMP
!  idthread=omp_get_thread_num()
!  numthreads=omp_get_num_threads()
!#else
  idthread=0
  numthreads=1
!#endif
  idthread=idthread+1


  !!$OMP DO SCHEDULE(STATIC) 
  !do islide = 0,slides-tem_slides
  !   do icol = 0,cols-tem_cols
  !      do irow = 0,rows-tem_rows
  !         do icell = 1, tem_cells
  !            localPattern(icell)=realization_matrix(irow+tem_cells_rows(icell),&
  !                                                 icol+tem_cells_cols(icell),&
  !                                                 islide+tem_cells_slides(icell))  
  !         end do
  !         call patternSearch(tem_cells,localPattern,pos)
  !         if(pos/=-1) then
  !            freq_realization_byid(pos,idthread)=freq_realization_byid(pos,idthread)+1
  !         end if
  !      end do
  !   end do
  !end do
  !!$OMP END DO
  !!$OMP END PARALLEL



  !do jj=1,numthreads
  !   do ii=1,npatterns
  !      freq_realization(ii)=freq_realization(ii)-freq_realization_byid(ii,jj)
  !   end do
  !end do

  !do ii=1,npatterns
  !   value=value + real(freq_realization(ii))*real(freq_realization(ii))
  !end do

  call system_clock(COUNT_RATE=clock_rate)

  value=0.0
  call system_clock(COUNT=clock_start)
  valueFortran = fitnessFunction(realization_matrix,rows,cols,slides,&
                         tem_rows, tem_cols, tem_slides, value, tem_cells, &
                         tem_cells_rows,tem_cells_cols,tem_cells_slides)

  call system_clock(COUNT=clock_end)
  print *,'Fortran90:   ',valueFortran,value
  timeFortran = (real(clock_end,kind=8)-real(clock_start,kind=8))/real(clock_rate,kind=8) 
  !print *, "CLOCK-fitnessFunctionFortran=",(real(clock_end,kind=8)-real(clock_start,kind=8))/real(clock_rate,kind=8)

 ! do ii=1,npatterns
 !    freq_realization(ii)=frequency(ii)
 ! end do

  !value=0.0
  !call system_clock(COUNT=clock_start)
  !valueGPU =fitnessfunctionCwrapper(slides,cols,rows,tem_cells,tem_slides,tem_cols,tem_rows,&
  !                          tem_cells_slides,tem_cells_cols,tem_cells_rows,&
  !                          npatterns,freq_realization,patternArray,realization_matrix,value)
  !
  !call system_clock(COUNT=clock_end)
  !!print *,'C-wrapper:   ',valueGPU,value
  !print *, "CLOCK-fitnessFunctionCwrapper=",(real(clock_end,kind=8)-real(clock_start,kind=8))/real(clock_rate,kind=8)
  
!  do ii=1,npatterns
!     freq_realization(ii)=frequency(ii)
!  end do

!  valueGPU=0.0
!  call system_clock(COUNT=clock_start)
!  valueGPU = fitnessfunctionCUDAwrapper(slides,cols,rows,tem_cells,tem_slides,tem_cols,tem_rows,&
!                           tem_cells_slides,tem_cells_cols,tem_cells_rows,&
!                            npatterns,freq_realization,patternArray,realization_matrix,value)
!
!  call system_clock(COUNT=clock_end)
!  print *,'CUDA-wrapper:',valueGPU,value
! timeCUDA=(real(clock_end,kind=8)-real(clock_start,kind=8))/real(clock_rate,kind=8) 
!  !print *, "CLOCK-fitnessFunctionCUDAwrapper=",(real(clock_end,kind=8)-real(clock_start,kind=8))/real(clock_rate,kind=8)
!
!  print *,timeFortran,timeCUDA
  !print *,valueFortran,valueGPU
#endif

end subroutine fitnessFunctionGPU
