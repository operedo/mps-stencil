
#include "header.h"

subroutine fitnessFunction(realization_matrix,rows,cols,slides,&
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
  integer(4)                :: ii,jj,kk,irow,icol,islide,jrow,jcol,jslide,pos,counter,i
  integer(4)                :: localPattern(tem_rows*tem_cols*tem_slides)
  integer(4)                :: freq_realization(npatterns),freq_realization_byid(npatterns,12)
  integer(4)                :: idthread,numthreads
#ifdef COMPRESSED
  integer(4)                :: localPatternChunk(int((tem_cells+CHUNK-1)/CHUNK)), &
                               chunkCounter, chunkValue, chunkIndex, tem_cells_chunk,&
                               chunkPowers(CHUNK)
#endif

#if defined(MODE) && ( MODE==0 || MODE==1 )

  freq_realization_byid=0

#if MODE==0
  do ii=1,npatterns
     freq_realization(ii)=patternArray(ii)%frequency
  end do
#elif MODE==1
  do ii=1,npatterns
     freq_realization(ii)=frequency(ii)
  end do
  print *,size(freq_realization),npatterns
#endif

  value=0.0
  pos=-1

#ifdef COMPRESSED
  chunkCounter=0
  chunkValue=0
  chunkIndex=0
  tem_cells_chunk=int((tem_cells+CHUNK-1)/CHUNK)
  do i=1,CHUNK
     chunkPowers(i)=10**(i-1)
     !chunkPowers(i)=CHUNK**(i-1)
  end do
#endif


!!$OMP PARALLEL FIRSTPRIVATE(localPattern,pos,icol,irow,islide,idthread)
!#ifdef _OPENMP
!  idthread=omp_get_thread_num()
!  numthreads=omp_get_num_threads()
!#else
  idthread=0
  numthreads=1
!#endif
  idthread=idthread+1
  counter=1
  !!$OMP DO SCHEDULE(STATIC) 
  do islide = 0,slides-tem_slides
     do icol = 0,cols-tem_cols
        do irow = 0,rows-tem_rows
#ifdef COMPRESSED
           chunkIndex=0
           chunkValue=0
           chunkCounter=0
#endif
           do icell = 1, tem_cells
#ifdef COMPRESSED
              chunkValue = chunkValue + realization_matrix(irow+tem_cells_rows(icell),&
                                        icol+tem_cells_cols(icell),&
                                        islide+tem_cells_slides(icell))*(chunkPowers(chunkCounter+1))
                                                   !islide+tem_cells_slides(icell))*(CHUNK**(chunkCounter))
              chunkCounter=chunkCounter+1

              if(chunkCounter==CHUNK .or. icell==tem_cells)then
                 chunkIndex=chunkIndex+1
                 !print *,icol,irow,'chunkIndex=',chunkIndex,'size(localPatternChunk)=',size(localPatternChunk),'chunkValue=',chunkValue
                 localPatternChunk(chunkIndex)=chunkValue
                 chunkValue=0
                 chunkCounter=0
              end if
#else
              localPattern(icell)=realization_matrix(irow+tem_cells_rows(icell),&
                                                   icol+tem_cells_cols(icell),&
                                                   islide+tem_cells_slides(icell))
#endif
           end do
#ifdef COMPRESSED
           call patternSearch(tem_cells_chunk,localPatternChunk,pos)
#else
           call patternSearch(tem_cells,localPattern,pos)
#endif

           !if(counter.le.2000 .and. counter.ge.1900) then
           !   write(*,*)pos
           !   do ii=1,tem_cells-1
           !      write(*,'(I1)',advance='no') localPattern(ii)
           !   end do
           !   write(*,'(I1)') localPattern(tem_cells)
           !end if
           !counter=counter+1
           if(pos/=-1) then
              freq_realization_byid(pos,idthread)=freq_realization_byid(pos,idthread)+1
           end if
        end do
     end do
  end do
  !!$OMP END DO
  !!$OMP END PARALLEL

  do jj=1,numthreads
     do ii=1,npatterns
        freq_realization(ii)=freq_realization(ii)-freq_realization_byid(ii,jj)
     end do
  end do

  do ii=1,npatterns
     value=value + real(freq_realization(ii))*real(freq_realization(ii))
  end do

#elif defined(MODE) && MODE==125

  integer(4) :: rowplus1
  integer(4) :: rowplus2
  integer(4) :: rowplus3
  integer(4) :: rowplus4
  integer(4) :: rowplus5
  integer(4) :: colplus1
  integer(4) :: colplus2
  integer(4) :: colplus3
  integer(4) :: colplus4
  integer(4) :: colplus5
  integer(4) :: slideplus1
  integer(4) :: slideplus2
  integer(4) :: slideplus3
  integer(4) :: slideplus4
  integer(4) :: slideplus5


  freq_realization_byid=0

  do ii=1,npatterns
     freq_realization(ii)=frequency(ii)
  end do

  value=0.0
  pos=-1

!$OMP PARALLEL FIRSTPRIVATE(localPattern,pos,icol,irow,islide,&
!$OMP rowplus1,&
!$OMP rowplus2,&
!$OMP rowplus3,&
!$OMP rowplus4,&
!$OMP rowplus5,&
!$OMP colplus1,&
!$OMP colplus2,&
!$OMP colplus3,&
!$OMP colplus4,&
!$OMP colplus5,&
!$OMP slideplus1,&
!$OMP slideplus2,&
!$OMP slideplus3,&
!$OMP slideplus4,&
!$OMP slideplus5,&
!$OMP idthread)
#ifdef _OPENMP
  idthread=omp_get_thread_num()
  numthreads=omp_get_num_threads()
#else
  idthread=0
  numthreads=1
#endif
  idthread=idthread+1
  !$OMP DO SCHEDULE(STATIC) 
  do islide = 0,slides-tem_slides

     slideplus1 = islide + 1
     slideplus2 = islide + 2
     slideplus3 = islide + 3
     slideplus4 = islide + 4
     slideplus5 = islide + 5
     do icol = 0,cols-tem_cols
        colplus1 = icol + 1
        colplus2 = icol + 2
        colplus3 = icol + 3
        colplus4 = icol + 4
        colplus5 = icol + 5
        do irow = 0,rows-tem_rows
           rowplus1 = irow + 1
           rowplus2 = irow + 2
           rowplus3 = irow + 3
           rowplus4 = irow + 4
           rowplus5 = irow + 5

           localPattern(1)=realization_matrix(rowplus1,colplus1,slideplus1)
           localPattern(2)=realization_matrix(rowplus2,colplus1,slideplus1)
           localPattern(3)=realization_matrix(rowplus3,colplus1,slideplus1)
           localPattern(4)=realization_matrix(rowplus4,colplus1,slideplus1)
           localPattern(5)=realization_matrix(rowplus5,colplus1,slideplus1)
           localPattern(6)=realization_matrix(rowplus1,colplus2,slideplus1)
           localPattern(7)=realization_matrix(rowplus2,colplus2,slideplus1)
           localPattern(8)=realization_matrix(rowplus3,colplus2,slideplus1)
           localPattern(9)=realization_matrix(rowplus4,colplus2,slideplus1)
           localPattern(10)=realization_matrix(rowplus5,colplus2,slideplus1)
           localPattern(11)=realization_matrix(rowplus1,colplus3,slideplus1)
           localPattern(12)=realization_matrix(rowplus2,colplus3,slideplus1)
           localPattern(13)=realization_matrix(rowplus3,colplus3,slideplus1)
           localPattern(14)=realization_matrix(rowplus4,colplus3,slideplus1)
           localPattern(15)=realization_matrix(rowplus5,colplus3,slideplus1)
           localPattern(16)=realization_matrix(rowplus1,colplus4,slideplus1)
           localPattern(17)=realization_matrix(rowplus2,colplus4,slideplus1)
           localPattern(18)=realization_matrix(rowplus3,colplus4,slideplus1)
           localPattern(19)=realization_matrix(rowplus4,colplus4,slideplus1)
           localPattern(20)=realization_matrix(rowplus5,colplus4,slideplus1)
           localPattern(21)=realization_matrix(rowplus1,colplus5,slideplus1)
           localPattern(22)=realization_matrix(rowplus2,colplus5,slideplus1)
           localPattern(23)=realization_matrix(rowplus3,colplus5,slideplus1)
           localPattern(24)=realization_matrix(rowplus4,colplus5,slideplus1)
           localPattern(25)=realization_matrix(rowplus5,colplus5,slideplus1)
           localPattern(26)=realization_matrix(rowplus1,colplus1,slideplus2)
           localPattern(27)=realization_matrix(rowplus2,colplus1,slideplus2)
           localPattern(28)=realization_matrix(rowplus3,colplus1,slideplus2)
           localPattern(29)=realization_matrix(rowplus4,colplus1,slideplus2)
           localPattern(30)=realization_matrix(rowplus5,colplus1,slideplus2)
           localPattern(31)=realization_matrix(rowplus1,colplus2,slideplus2)
           localPattern(32)=realization_matrix(rowplus2,colplus2,slideplus2)
           localPattern(33)=realization_matrix(rowplus3,colplus2,slideplus2)
           localPattern(34)=realization_matrix(rowplus4,colplus2,slideplus2)
           localPattern(35)=realization_matrix(rowplus5,colplus2,slideplus2)
           localPattern(36)=realization_matrix(rowplus1,colplus3,slideplus2)
           localPattern(37)=realization_matrix(rowplus2,colplus3,slideplus2)
           localPattern(38)=realization_matrix(rowplus3,colplus3,slideplus2)
           localPattern(39)=realization_matrix(rowplus4,colplus3,slideplus2)
           localPattern(40)=realization_matrix(rowplus5,colplus3,slideplus2)
           localPattern(41)=realization_matrix(rowplus1,colplus4,slideplus2)
           localPattern(42)=realization_matrix(rowplus2,colplus4,slideplus2)
           localPattern(43)=realization_matrix(rowplus3,colplus4,slideplus2)
           localPattern(44)=realization_matrix(rowplus4,colplus4,slideplus2)
           localPattern(45)=realization_matrix(rowplus5,colplus4,slideplus2)
           localPattern(46)=realization_matrix(rowplus1,colplus5,slideplus2)
           localPattern(47)=realization_matrix(rowplus2,colplus5,slideplus2)
           localPattern(48)=realization_matrix(rowplus3,colplus5,slideplus2)
           localPattern(49)=realization_matrix(rowplus4,colplus5,slideplus2)
           localPattern(50)=realization_matrix(rowplus5,colplus5,slideplus2)
           localPattern(51)=realization_matrix(rowplus1,colplus1,slideplus3)
           localPattern(52)=realization_matrix(rowplus2,colplus1,slideplus3)
           localPattern(53)=realization_matrix(rowplus3,colplus1,slideplus3)
           localPattern(54)=realization_matrix(rowplus4,colplus1,slideplus3)
           localPattern(55)=realization_matrix(rowplus5,colplus1,slideplus3)
           localPattern(56)=realization_matrix(rowplus1,colplus2,slideplus3)
           localPattern(57)=realization_matrix(rowplus2,colplus2,slideplus3)
           localPattern(58)=realization_matrix(rowplus3,colplus2,slideplus3)
           localPattern(59)=realization_matrix(rowplus4,colplus2,slideplus3)
           localPattern(60)=realization_matrix(rowplus5,colplus2,slideplus3)
           localPattern(61)=realization_matrix(rowplus1,colplus3,slideplus3)
           localPattern(62)=realization_matrix(rowplus2,colplus3,slideplus3)
           localPattern(63)=realization_matrix(rowplus3,colplus3,slideplus3)
           localPattern(64)=realization_matrix(rowplus4,colplus3,slideplus3)
           localPattern(65)=realization_matrix(rowplus5,colplus3,slideplus3)
           localPattern(66)=realization_matrix(rowplus1,colplus4,slideplus3)
           localPattern(67)=realization_matrix(rowplus2,colplus4,slideplus3)
           localPattern(68)=realization_matrix(rowplus3,colplus4,slideplus3)
           localPattern(69)=realization_matrix(rowplus4,colplus4,slideplus3)
           localPattern(70)=realization_matrix(rowplus5,colplus4,slideplus3)
           localPattern(71)=realization_matrix(rowplus1,colplus5,slideplus3)
           localPattern(72)=realization_matrix(rowplus2,colplus5,slideplus3)
           localPattern(73)=realization_matrix(rowplus3,colplus5,slideplus3)
           localPattern(74)=realization_matrix(rowplus4,colplus5,slideplus3)
           localPattern(75)=realization_matrix(rowplus5,colplus5,slideplus3)
           localPattern(76)=realization_matrix(rowplus1,colplus1,slideplus4)
           localPattern(77)=realization_matrix(rowplus2,colplus1,slideplus4)
           localPattern(78)=realization_matrix(rowplus3,colplus1,slideplus4)
           localPattern(79)=realization_matrix(rowplus4,colplus1,slideplus4)
           localPattern(80)=realization_matrix(rowplus5,colplus1,slideplus4)
           localPattern(81)=realization_matrix(rowplus1,colplus2,slideplus4)
           localPattern(82)=realization_matrix(rowplus2,colplus2,slideplus4)
           localPattern(83)=realization_matrix(rowplus3,colplus2,slideplus4)
           localPattern(84)=realization_matrix(rowplus4,colplus2,slideplus4)
           localPattern(85)=realization_matrix(rowplus5,colplus2,slideplus4)
           localPattern(86)=realization_matrix(rowplus1,colplus3,slideplus4)
           localPattern(87)=realization_matrix(rowplus2,colplus3,slideplus4)
           localPattern(88)=realization_matrix(rowplus3,colplus3,slideplus4)
           localPattern(89)=realization_matrix(rowplus4,colplus3,slideplus4)
           localPattern(90)=realization_matrix(rowplus5,colplus3,slideplus4)
           localPattern(91)=realization_matrix(rowplus1,colplus4,slideplus4)
           localPattern(92)=realization_matrix(rowplus2,colplus4,slideplus4)
           localPattern(93)=realization_matrix(rowplus3,colplus4,slideplus4)
           localPattern(94)=realization_matrix(rowplus4,colplus4,slideplus4)
           localPattern(95)=realization_matrix(rowplus5,colplus4,slideplus4)
           localPattern(96)=realization_matrix(rowplus1,colplus5,slideplus4)
           localPattern(97)=realization_matrix(rowplus2,colplus5,slideplus4)
           localPattern(98)=realization_matrix(rowplus3,colplus5,slideplus4)
           localPattern(99)=realization_matrix(rowplus4,colplus5,slideplus4)
           localPattern(100)=realization_matrix(rowplus5,colplus5,slideplus4)
           localPattern(101)=realization_matrix(rowplus1,colplus1,slideplus5)
           localPattern(102)=realization_matrix(rowplus2,colplus1,slideplus5)
           localPattern(103)=realization_matrix(rowplus3,colplus1,slideplus5)
           localPattern(104)=realization_matrix(rowplus4,colplus1,slideplus5)
           localPattern(105)=realization_matrix(rowplus5,colplus1,slideplus5)
           localPattern(106)=realization_matrix(rowplus1,colplus2,slideplus5)
           localPattern(107)=realization_matrix(rowplus2,colplus2,slideplus5)
           localPattern(108)=realization_matrix(rowplus3,colplus2,slideplus5)
           localPattern(109)=realization_matrix(rowplus4,colplus2,slideplus5)
           localPattern(110)=realization_matrix(rowplus5,colplus2,slideplus5)
           localPattern(111)=realization_matrix(rowplus1,colplus3,slideplus5)
           localPattern(112)=realization_matrix(rowplus2,colplus3,slideplus5)
           localPattern(113)=realization_matrix(rowplus3,colplus3,slideplus5)
           localPattern(114)=realization_matrix(rowplus4,colplus3,slideplus5)
           localPattern(115)=realization_matrix(rowplus5,colplus3,slideplus5)
           localPattern(116)=realization_matrix(rowplus1,colplus4,slideplus5)
           localPattern(117)=realization_matrix(rowplus2,colplus4,slideplus5)
           localPattern(118)=realization_matrix(rowplus3,colplus4,slideplus5)
           localPattern(119)=realization_matrix(rowplus4,colplus4,slideplus5)
           localPattern(120)=realization_matrix(rowplus5,colplus4,slideplus5)
           localPattern(121)=realization_matrix(rowplus1,colplus5,slideplus5)
           localPattern(122)=realization_matrix(rowplus2,colplus5,slideplus5)
           localPattern(123)=realization_matrix(rowplus3,colplus5,slideplus5)
           localPattern(124)=realization_matrix(rowplus4,colplus5,slideplus5)
           localPattern(125)=realization_matrix(rowplus5,colplus5,slideplus5)

           call patternSearch(tem_cells,localPattern,pos)

           if(pos/=-1) then
              freq_realization_byid(pos,idthread)=freq_realization_byid(pos,idthread)+1
           end if
        end do

     end do
  end do
  !$OMP END DO
  !$OMP END PARALLEL

  do jj=1,numthreads
     do ii=1,npatterns
        freq_realization(ii)=freq_realization(ii)-freq_realization_byid(ii,jj)
     end do
  end do

  do ii=1,npatterns
     value=value + real(freq_realization(ii))*real(freq_realization(ii))
  end do

#endif

end subroutine fitnessFunction
