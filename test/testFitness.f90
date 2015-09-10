program testFitness

  use patternOperations

#ifdef TRACE
  use extrae_module
#endif

  implicit none

  type individual
     integer(4), allocatable :: matrix(:,:,:)
     integer(4), allocatable :: histogram(:)
  end type individual
  type(individual)           :: indivTI, indivRE
  integer(4)                 :: number, rows, cols, slides, tem_rows, tem_cols, tem_slides
  integer(4)                 :: irow,icol,islide,ii,jj,kk
  integer(4)                 :: tem_cells
  real(8)                    :: value,realrand
  integer(4),allocatable     :: tem_cells_rows(:),tem_cells_cols(:),tem_cells_slides(:)
  integer(8)                 :: clock_start, clock_end, clock_rate
  integer(4)                 ::ierr,i ,tests
  character(len=32)          :: arg
  character(len=200)         :: paramfile
  character(len=200)         :: datafile
  character(len=200)         :: datafilemodif

#ifdef TRACE
  call extrae_init()
#endif

  call getarg(1, arg)
  read(arg,*) paramfile

  !print *,arg

  call system_clock(COUNT_RATE=clock_rate)

  call system_clock(COUNT=clock_start)
  !open (unit = 13, file = "../data/params/params_100x100x300_125stencil.txt")
  open (unit = 13, file = "../data/params/5x5x5/"//paramfile)
  !open (unit = 13, file = "../data/params/10x10x10/"//paramfile)

  !print *, "../data/params/"//paramfile 

  !open (unit = 13, file = "../data/params/params_400x400x600_125stencil.txt")
  !open (unit = 13, file = "../data/params/params_400x400x100_125stencil.txt")
  read (13,*) datafile

  !print *,datafile

  read (13,*) datafilemodif

  !print *,datafilemodif

  read (13,*) rows, cols, slides, tem_rows, tem_cols, tem_slides
  !print *, rows, cols, slides, tem_rows, tem_cols, tem_slides
  read (13,*) tem_cells
  !print *,tem_cells
  if(tem_cells>0) then
     allocate(tem_cells_rows(tem_cells))
     allocate(tem_cells_cols(tem_cells))
     allocate(tem_cells_slides(tem_cells))
     do ii=1,tem_cells
        read (13,*) tem_cells_rows(ii), tem_cells_cols(ii), tem_cells_slides(ii)
        !print *, tem_cells_rows(ii), tem_cells_cols(ii), tem_cells_slides(ii)
     end do
  end if
  close(13)

  allocate(indivTI%matrix(rows,cols,slides))
  allocate(indivRE%matrix(rows,cols,slides))

  !open (unit = 13, file = "../data/images/channels100x100x300.dat")
  !open (unit = 13, file = "../data/images/channels100x100x300_4C.dat")
  !open (unit = 13, file = "../data/images/channels100x100x300_5C.dat")
  open (unit = 13, file = "../data/images/"//datafile)
  !print *, "../data/images/"//datafile 
  !open (unit = 13, file = "../data/images/channels400x400x600_5C.dat")
  !open (unit = 13, file = "../data/images/channels400x400x100_2C.dat")
  do ii=1,slides
     read (13,*) indivTI%matrix(:,:,ii)
  end do
  close(13)
  
  !open (unit = 13, file = "../data/images/channels100x100x300_modif.dat")
  !open (unit = 13, file = "../data/images/channels100x100x300_modif_4C.dat")
  !open (unit = 13, file = "../data/images/channels100x100x300_modif_5C.dat")
  open (unit = 13, file = "../data/images/"//datafilemodif)
  !print *, "../data/images/"//datafilemodif 
  !open (unit = 13, file = "../data/images/channels400x400x600_modif_5C.dat")
  !open (unit = 13, file = "../data/images/channels400x400x100_modif_2C.dat")
  do ii=1,slides
     read (13,*) indivRE%matrix(:,:,ii)
  end do
  close(13)

  call system_clock(COUNT=clock_end)
  print *, "CLOCK-readParams=",real((real(clock_end,kind=8)-real(clock_start,kind=8))/real(clock_rate,kind=8),kind=8)

  call system_clock(COUNT=clock_start)
  call loadHistogram(indivTI%matrix,rows,cols,slides,tem_rows,tem_cols,tem_slides, tem_cells, &
                                   tem_cells_rows, tem_cells_cols,tem_cells_slides,0)
  call system_clock(COUNT=clock_end)
  print *, "CLOCK-loadHistogram=",real((real(clock_end,kind=8)-real(clock_start,kind=8))/real(clock_rate,kind=8),kind=8)


  tests=1

  call system_clock(COUNT=clock_start)
  do i=1,tests
     value=0.0
     call fitnessFunctionGPU(indivRE%matrix,rows,cols,slides,tem_rows,tem_cols,tem_slides,&
                          value, tem_cells, tem_cells_rows,tem_cells_cols,tem_cells_slides)
!     call fitnessFunction(indivRE%matrix,rows,cols,slides,tem_rows,tem_cols,tem_slides,&
!                          value, tem_cells, tem_cells_rows,tem_cells_cols,tem_cells_slides)
  end do
  call system_clock(COUNT=clock_end)
  print *, "CLOCK-fitnessFunction=",real((real(clock_end,kind=8)-real(clock_start,kind=8))/real(clock_rate,kind=8),kind=8)/&
                            real(tests,kind=8)
  !print *, "rate=",real(clock_rate/100.0)," diff=",((clock_end)-(clock_start)) 
  !print *, 'fitnessValue=', value
  
  deallocate(indivTI%matrix)
  deallocate(indivRE%matrix)
  deallocate(tem_cells_rows)
  deallocate(tem_cells_cols)
  deallocate(tem_cells_slides)

#ifdef TRACE
  call extrae_fini()
#endif

end program testFitness
