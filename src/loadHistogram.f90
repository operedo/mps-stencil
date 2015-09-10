
#include "header.h"

subroutine loadHistogram(matrix,rows,cols,slides,tem_rows,tem_cols,tem_slides,tem_cells,&
                                       tem_cells_rows,tem_cells_cols,tem_cells_slides,clock_rate)

  use patternOperations
#ifdef _OPENMP
  use omp_lib
#endif

  implicit none

  integer(4), intent(in)                   :: rows, cols, slides
  integer(4), intent(in)                   :: matrix(rows,cols,slides)
  integer(4), intent(in)                   :: tem_rows, tem_cols, tem_slides, tem_cells
  integer(4), intent(in)                   :: clock_rate
  !type(patternType), intent(inout), pointer  :: histogram(:)
  integer(4)                               :: irow,icol,islide,jrow,jcol,jslide,ipattern, icell, ithread,&
                                              threadId,numThreads,npatternsGlobal,i,pos,j,freq,leadDim,leadLim,&
                                              numThreadsLog
  integer(4)                               :: decimalNumber,nchunks,counter,total,id,clockIni,clockEnd
  integer(4)                               :: localPattern(tem_rows*tem_cols*tem_slides), &
                                              tem_cells_rows(tem_cells), tem_cells_cols(tem_cells), &
                                              tem_cells_slides(tem_cells)
#ifdef COMPRESSED
  integer(4)                               :: localPatternChunk(int((tem_cells+CHUNK-1)/CHUNK)), &
                                              chunkCounter, chunkValue, chunkIndex,tem_cells_chunk,&
                                              chunkPowers(CHUNK)
#endif
  real(8)                                  :: totalinv    


  integer(4)                               :: sourceData,targetData
#ifdef _OPENMP
  integer(4), parameter                    :: MAXTHREADS=16
  type(patternType)                          :: localHistogram(MAXTHREADS)
#endif  

#ifdef _OPENMP
  !$OMP PARALLEL
  numThreads = OMP_get_num_threads()
  !$OMP END PARALLEL
  localHistogram(:)%npatterns=0
#endif
  npatterns=0
  counter=1
  total=(rows-tem_rows+1)*(cols-tem_cols+1)*(slides-tem_slides+1)
  totalinv=1.0/real(total)


#ifdef COMPRESSED
  chunkCounter=0
  chunkValue=0
  chunkIndex=0
  tem_cells_chunk=int((tem_cells+CHUNK-1)/CHUNK)
  do i=1,CHUNK
     chunkPowers(i)=int(10**(i-1))
     !chunkPowers(i)=int(CHUNK**(i-1))
     !print *,'chunkPowers(',i,')',chunkPowers(i)
  end do
#endif   

  !$OMP PARALLEL default(shared) &
  !$OMP          firstprivate(islide,icol,irow,counter,icell,localpattern,leadDim,leadLim,&
#ifdef COMPRESSED
  !$OMP          chunkValue,chunkCounter,chunkIndex,localPatternChunk,&
#endif   
  !$OMP          threadId)
#ifdef _OPENMP
  threadId = OMP_get_thread_num()
#else
  threadId = 0 
#endif
  threadId = threadId + 1
  !!$OMP DO schedule(static,8) 
  !$OMP DO schedule(dynamic,1) 
  do islide = 1,slides-tem_slides+1

     leadDim=islide
     leadLim=slides-tem_slides+1
 
#ifdef _OPENMP
     if(threadId==numThreads)then
        print *,'loading histogram(',threadId,')... ',leadDim,'/',leadLim,' (',&
                                                (real(leadDim)/real(leadLim))*100,'%)'
     end if
#else
     print *,'loading histogram(',threadId,')... ',leadDim,'/',leadLim,' (',&
                                                (real(leadDim)/real(leadLim))*100,'%)'
#endif

     do icol = 1,cols-tem_cols+1
        do irow = 1,rows-tem_rows+1
#ifdef COMPRESSED
           chunkIndex=0
           chunkValue=0
           chunkCounter=0
#endif
           do icell = 1, tem_cells 
#ifdef COMPRESSED

                 chunkValue = chunkValue + matrix(irow+tem_cells_rows(icell)-1,&
                                          icol+tem_cells_cols(icell)-1,&
                                          islide+tem_cells_slides(icell)-1)*(chunkPowers(chunkCounter+1))

                 chunkCounter=chunkCounter+1

                 if(chunkCounter==CHUNK .or. icell==tem_cells)then
                    chunkIndex=chunkIndex+1
                    localPatternChunk(chunkIndex)=chunkValue
                    chunkValue=0
                    chunkCounter=0
                 end if
#else
                 localPattern(icell)=matrix(irow+tem_cells_rows(icell)-1,&
                                          icol+tem_cells_cols(icell)-1,&
                                          islide+tem_cells_slides(icell)-1) 
#endif
           end do

#ifdef _OPENMP

#ifdef COMPRESSED
           if(threadId==1)then
              call patternInsertion(tem_cells_chunk,localPatternChunk)
           else
              call patternInsertionLocal(tem_cells_chunk,localPatternChunk,&
                   localHistogram(threadId)%patternArray,&
                   localHistogram(threadId)%frequency,&
                   localHistogram(threadId)%npatterns,&
                   localHistogram(threadId)%memusage,&
                   localHistogram(threadId)%rootPos)
           end if
#else
           if(threadId==1)then
              call patternInsertion(tem_cells,localPattern)
           else
              call patternInsertionLocal(tem_cells,localPattern,&
                   localHistogram(threadId)%patternArray,&
                   localHistogram(threadId)%frequency,&
                   localHistogram(threadId)%npatterns,&
                   localHistogram(threadId)%memusage,&
                   localHistogram(threadId)%rootPos)
           end if
#endif

#else

#ifdef COMPRESSED
           call patternInsertion(tem_cells_chunk,localPatternChunk)
#else
           call patternInsertion(tem_cells,localPattern)
#endif

#endif

           counter=counter+1
        end do
     end do
  end do
  !$OMP END DO
  !$OMP END PARALLEL

#ifdef _OPENMP

!  do ithread=2,numThreads
!     npatternsGlobal =localHistogram(ithread)%npatterns
!     do j=1,npatternsGlobal 
!#ifdef COMPRESSED
!        localPatternChunk(:)=localHistogram(ithread)%patternArray(:,j)
!        freq=localHistogram(ithread)%frequency(j) 
!        call patternInsertion(tem_cells_chunk,&
!           localPatternChunk,&
!           freq)
!           !localHistogram(ithread)%patternArray(:,j),&
!           !localHistogram(ithread)%frequency(j))
!#else
!        localPattern(:)=localHistogram(ithread)%patternArray(:,j)
!        freq=localHistogram(ithread)%frequency(j) 
!        call patternInsertion(tem_cells,&
!           localPattern,&
!           freq)
!           !localHistogram(ithread)%patternArray(:,j),&
!           !localHistogram(ithread)%frequency(j))
!#endif
!     end do
!  end do

  numThreadsLog=int(log(real(numThreads))/log(2.0))

  !print *,'numThreadsLog=',numThreadsLog
  
!
!  do ithread=numThreadsLog,1,-1
!     !print *,'threads=',ithread
!!$OMP PARALLEL num_threads(2**(ithread-1)) default(shared) &
!!$OMP          private(threadId,sourceData,targetData,npatternsGlobal,j)
!     threadId = OMP_get_thread_num()+1
!     sourceData = 2**(ithread-1)+threadId
!     targetData = threadId
!     !print *,'threadId=',threadId,' numThreads=',OMP_get_num_threads()
!     print *,'source=',sourceData,'target=',targetData
!
!     npatternsGlobal =localHistogram(sourceData)%npatterns
!
!     if(targetData==1)then
!        do j=1,npatternsGlobal 
!           !print *,sourceData,'->',targetData,'-',j,'/',npatternsGlobal,' INS'
!#ifdef COMPRESSED
!           call patternInsertion(tem_cells_chunk,&
!              localHistogram(sourceData)%patternArray(:,j),&
!              localHistogram(sourceData)%frequency(j))
!#else
!           call patternInsertion(tem_cells,&
!              localHistogram(sourceData)%patternArray(:,j),&
!              localHistogram(sourceData)%frequency(j))
!#endif
!        end do
!     else
!        do j=1,npatternsGlobal 
!           !print *,sourceData,'->',targetData,'-',j,'/',npatternsGlobal,' INS'
!#ifdef COMPRESSED
!           call patternInsertionLocal(tem_cells_chunk,&
!              localHistogram(sourceData)%patternArray(:,j),&
!              localHistogram(targetData)%patternArray,&
!              localHistogram(targetData)%frequency,&
!              localHistogram(targetData)%npatterns,&
!              localHistogram(targetData)%memusage,&
!              localHistogram(targetData)%rootPos,&
!              localHistogram(sourceData)%frequency(j)&
!           )
!#else
!           call patternInsertionLocal(tem_cells,&
!              localHistogram(sourceData)%patternArray(:,j),&
!              localHistogram(targetData)%patternArray,&
!              localHistogram(targetData)%frequency,&
!              localHistogram(targetData)%npatterns,&
!              localHistogram(targetData)%memusage,&
!              localHistogram(targetData)%rootPos,&
!              localHistogram(sourceData)%frequency(j)&
!           )
!#endif
!        end do
!
!     end if
!
!!$OMP END PARALLEL
!
!  end do
!


!  do ithread=2,numThreads
!     npatternsGlobal =localHistogram(ithread)%npatterns
!
!     !$OMP PARALLEL default(shared) firstprivate(pos,j,freq)
!     !$OMP DO schedule(static,32)
!     do j=1,npatternsGlobal 
!!        pos=-1
!        !localPattern(:)=localHistogram(ithread)%patternArray(:,j)
!        freq=localHistogram(ithread)%frequency(j) 
!!        call patternSearch(tem_cells,localHistogram(ithread)%patternArray(:,j),pos)
!        !call patternSearch(tem_cells,localPattern,pos)
!!        if(pos/=-1)then
!           !print *,ithread,'/',numThreads,'-',j,'/',npatternsGlobal,' HIT'
!!           !$OMP CRITICAL (FREQ)
!           !frequency(pos)=frequency(pos)+localHistogram(ithread)%frequency(j)
!!           frequency(pos)=frequency(pos)+freq
!!           !$OMP END CRITICAL (FREQ)
!!        else
!           !print *,ithread,'/',numThreads,'-',jj,'/',npatternsGlobal,' MISS'
!           !$OMP CRITICAL (INS)
!#ifdef COMPRESSED
!           call patternInsertion(tem_cells_chunk,&
!              localHistogram(ithread)%patternArray(:,j),&
!              !localPattern,&
!              !localHistogram(ithread)%frequency(j))
!              freq)
!#else
!           call patternInsertion(tem_cells,&
!              localHistogram(ithread)%patternArray(:,j),&
!              !localPattern,&
!              !localHistogram(ithread)%frequency(j))
!              freq)
!#endif
!           !$OMP END CRITICAL (INS)
!!        end if
!     end do
!     !$OMP END DO
!     !$OMP END PARALLEL
!  end do


#endif

  !call patternBuildPivots(2,4) ! (facies,levels)

  !print *,npatterns

  !do icell=1,tem_cells/CHUNK
  !   do irow=1,npatterns
  !      !write(*,"(I3,522I1)") icell,patternArray(icell,:)
  !      !write(*,*) patternArray(icell,:)
  !      write(*,"(25I3)") patternArray(:,irow)
  !   end do
  !end do

  !stop

  print *,npatterns
  do j=1,npatterns
     !print *,frequency(j),patternArray(:,j)
     write(*,"(125I1)")patternArray(:,j)
  end do


end subroutine loadHistogram
