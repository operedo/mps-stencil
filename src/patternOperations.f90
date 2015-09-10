
#include "header.h"

module patternOperations


  implicit none

#if MODE == 0
  integer(4) :: npatterns
  type patternType
     integer(4), pointer :: pattern(:)
     integer(4)          :: frequency 
  end type patternType
  type(patternType), pointer :: patternArray(:) 
  integer(4) :: rootPos
#else
  type patternType
     integer(4)          :: npatterns
     integer(4)          :: memusage
     integer(4), pointer :: patternArray(:,:)
     integer(4), pointer :: frequency(:) 
     integer(4)          :: rootPos
  end type patternType
  integer(4) :: npatterns
  integer(4) :: memusage
  integer(4), pointer :: patternArray(:,:)
  integer(4), pointer :: frequency(:)
  integer(4) :: rootPos

  !integer(4), pointer :: pivot(:)
  !type record
  !   integer(4) :: generation
  !   integer(4) :: value
  !   type ( record ), pointer :: parent
  !   type ( record ), pointer :: left
  !   type ( record ), pointer :: right
  !end type record
  !type(record),pointer :: head


#endif

contains
  
  subroutine patternComparation(nrows,onePattern1,onePattern2,value)
    !
    ! Compare two patterns.
    ! If value = 0, both patterns are equals
    ! If value = 1, the first pattern is bigger
    ! If value = 2, the second pattern is bigger
    !
    integer(4), intent(in)   :: nrows
    integer(4), intent(in)   :: onePattern1(nrows),onePattern2(nrows)
    integer(4), intent(out)  :: value
    integer(4)               :: irow

#if (MODE == 0) || (MODE == 1)

    value = 0
    irow = 0

    do while ( value == 0 .and. irow < nrows )
       irow = irow + 1       
       if (     onePattern1(irow) > onePattern2(irow) ) then
          value = 1        
       elseif ( onePattern1(irow) < onePattern2(irow) ) then
          !value = 2
          value = -1
       end if
    end do

#elif MODE==125

    !print *,'patternComparation: STENCIL=125'

    value=onePattern1(1) - onePattern2(1)
    if(value/=0)return
    value=onePattern1(2) - onePattern2(2)
    if(value/=0)return
    value=onePattern1(3) - onePattern2(3)
    if(value/=0)return
    value=onePattern1(4) - onePattern2(4)
    if(value/=0)return
    value=onePattern1(5) - onePattern2(5)
    if(value/=0)return
    value=onePattern1(6) - onePattern2(6)
    if(value/=0)return
    value=onePattern1(7) - onePattern2(7)
    if(value/=0)return
    value=onePattern1(8) - onePattern2(8)
    if(value/=0)return
    value=onePattern1(9) - onePattern2(9)
    if(value/=0)return
    value=onePattern1(10) - onePattern2(10)
    if(value/=0)return
    value=onePattern1(11) - onePattern2(11)
    if(value/=0)return
    value=onePattern1(12) - onePattern2(12)
    if(value/=0)return
    value=onePattern1(13) - onePattern2(13)
    if(value/=0)return
    value=onePattern1(14) - onePattern2(14)
    if(value/=0)return
    value=onePattern1(15) - onePattern2(15)
    if(value/=0)return
    value=onePattern1(16) - onePattern2(16)
    if(value/=0)return
    value=onePattern1(17) - onePattern2(17)
    if(value/=0)return
    value=onePattern1(18) - onePattern2(18)
    if(value/=0)return
    value=onePattern1(19) - onePattern2(19)
    if(value/=0)return
    value=onePattern1(20) - onePattern2(20)
    if(value/=0)return
    value=onePattern1(21) - onePattern2(21)
    if(value/=0)return
    value=onePattern1(22) - onePattern2(22)
    if(value/=0)return
    value=onePattern1(23) - onePattern2(23)
    if(value/=0)return
    value=onePattern1(24) - onePattern2(24)
    if(value/=0)return
    value=onePattern1(25) - onePattern2(25)
    if(value/=0)return
    value=onePattern1(26) - onePattern2(26)
    if(value/=0)return
    value=onePattern1(27) - onePattern2(27)
    if(value/=0)return
    value=onePattern1(28) - onePattern2(28)
    if(value/=0)return
    value=onePattern1(29) - onePattern2(29)
    if(value/=0)return
    value=onePattern1(30) - onePattern2(30)
    if(value/=0)return
    value=onePattern1(31) - onePattern2(31)
    if(value/=0)return
    value=onePattern1(32) - onePattern2(32)
    if(value/=0)return
    value=onePattern1(33) - onePattern2(33)
    if(value/=0)return
    value=onePattern1(34) - onePattern2(34)
    if(value/=0)return
    value=onePattern1(35) - onePattern2(35)
    if(value/=0)return
    value=onePattern1(36) - onePattern2(36)
    if(value/=0)return
    value=onePattern1(37) - onePattern2(37)
    if(value/=0)return
    value=onePattern1(38) - onePattern2(38)
    if(value/=0)return
    value=onePattern1(39) - onePattern2(39)
    if(value/=0)return
    value=onePattern1(40) - onePattern2(40)
    if(value/=0)return
    value=onePattern1(41) - onePattern2(41)
    if(value/=0)return
    value=onePattern1(42) - onePattern2(42)
    if(value/=0)return
    value=onePattern1(43) - onePattern2(43)
    if(value/=0)return
    value=onePattern1(44) - onePattern2(44)
    if(value/=0)return
    value=onePattern1(45) - onePattern2(45)
    if(value/=0)return
    value=onePattern1(46) - onePattern2(46)
    if(value/=0)return
    value=onePattern1(47) - onePattern2(47)
    if(value/=0)return
    value=onePattern1(48) - onePattern2(48)
    if(value/=0)return
    value=onePattern1(49) - onePattern2(49)
    if(value/=0)return
    value=onePattern1(50) - onePattern2(50)
    if(value/=0)return
    value=onePattern1(51) - onePattern2(51)
    if(value/=0)return
    value=onePattern1(52) - onePattern2(52)
    if(value/=0)return
    value=onePattern1(53) - onePattern2(53)
    if(value/=0)return
    value=onePattern1(54) - onePattern2(54)
    if(value/=0)return
    value=onePattern1(55) - onePattern2(55)
    if(value/=0)return
    value=onePattern1(56) - onePattern2(56)
    if(value/=0)return
    value=onePattern1(57) - onePattern2(57)
    if(value/=0)return
    value=onePattern1(58) - onePattern2(58)
    if(value/=0)return
    value=onePattern1(59) - onePattern2(59)
    if(value/=0)return
    value=onePattern1(60) - onePattern2(60)
    if(value/=0)return
    value=onePattern1(61) - onePattern2(61)
    if(value/=0)return
    value=onePattern1(62) - onePattern2(62)
    if(value/=0)return
    value=onePattern1(63) - onePattern2(63)
    if(value/=0)return
    value=onePattern1(64) - onePattern2(64)
    if(value/=0)return
    value=onePattern1(65) - onePattern2(65)
    if(value/=0)return
    value=onePattern1(66) - onePattern2(66)
    if(value/=0)return
    value=onePattern1(67) - onePattern2(67)
    if(value/=0)return
    value=onePattern1(68) - onePattern2(68)
    if(value/=0)return
    value=onePattern1(69) - onePattern2(69)
    if(value/=0)return
    value=onePattern1(70) - onePattern2(70)
    if(value/=0)return
    value=onePattern1(71) - onePattern2(71)
    if(value/=0)return
    value=onePattern1(72) - onePattern2(72)
    if(value/=0)return
    value=onePattern1(73) - onePattern2(73)
    if(value/=0)return
    value=onePattern1(74) - onePattern2(74)
    if(value/=0)return
    value=onePattern1(75) - onePattern2(75)
    if(value/=0)return
    value=onePattern1(76) - onePattern2(76)
    if(value/=0)return
    value=onePattern1(77) - onePattern2(77)
    if(value/=0)return
    value=onePattern1(78) - onePattern2(78)
    if(value/=0)return
    value=onePattern1(79) - onePattern2(79)
    if(value/=0)return
    value=onePattern1(80) - onePattern2(80)
    if(value/=0)return
    value=onePattern1(81) - onePattern2(81)
    if(value/=0)return
    value=onePattern1(82) - onePattern2(82)
    if(value/=0)return
    value=onePattern1(83) - onePattern2(83)
    if(value/=0)return
    value=onePattern1(84) - onePattern2(84)
    if(value/=0)return
    value=onePattern1(85) - onePattern2(85)
    if(value/=0)return
    value=onePattern1(86) - onePattern2(86)
    if(value/=0)return
    value=onePattern1(87) - onePattern2(87)
    if(value/=0)return
    value=onePattern1(88) - onePattern2(88)
    if(value/=0)return
    value=onePattern1(89) - onePattern2(89)
    if(value/=0)return
    value=onePattern1(90) - onePattern2(90)
    if(value/=0)return
    value=onePattern1(91) - onePattern2(91)
    if(value/=0)return
    value=onePattern1(92) - onePattern2(92)
    if(value/=0)return
    value=onePattern1(93) - onePattern2(93)
    if(value/=0)return
    value=onePattern1(94) - onePattern2(94)
    if(value/=0)return
    value=onePattern1(95) - onePattern2(95)
    if(value/=0)return
    value=onePattern1(96) - onePattern2(96)
    if(value/=0)return
    value=onePattern1(97) - onePattern2(97)
    if(value/=0)return
    value=onePattern1(98) - onePattern2(98)
    if(value/=0)return
    value=onePattern1(99) - onePattern2(99)
    if(value/=0)return
    value=onePattern1(100) - onePattern2(100)
    if(value/=0)return
    value=onePattern1(101) - onePattern2(101)
    if(value/=0)return
    value=onePattern1(102) - onePattern2(102)
    if(value/=0)return
    value=onePattern1(103) - onePattern2(103)
    if(value/=0)return
    value=onePattern1(104) - onePattern2(104)
    if(value/=0)return
    value=onePattern1(105) - onePattern2(105)
    if(value/=0)return
    value=onePattern1(106) - onePattern2(106)
    if(value/=0)return
    value=onePattern1(107) - onePattern2(107)
    if(value/=0)return
    value=onePattern1(108) - onePattern2(108)
    if(value/=0)return
    value=onePattern1(109) - onePattern2(109)
    if(value/=0)return
    value=onePattern1(110) - onePattern2(110)
    if(value/=0)return
    value=onePattern1(111) - onePattern2(111)
    if(value/=0)return
    value=onePattern1(112) - onePattern2(112)
    if(value/=0)return
    value=onePattern1(113) - onePattern2(113)
    if(value/=0)return
    value=onePattern1(114) - onePattern2(114)
    if(value/=0)return
    value=onePattern1(115) - onePattern2(115)
    if(value/=0)return
    value=onePattern1(116) - onePattern2(116)
    if(value/=0)return
    value=onePattern1(117) - onePattern2(117)
    if(value/=0)return
    value=onePattern1(118) - onePattern2(118)
    if(value/=0)return
    value=onePattern1(119) - onePattern2(119)
    if(value/=0)return
    value=onePattern1(120) - onePattern2(120)
    if(value/=0)return
    value=onePattern1(121) - onePattern2(121)
    if(value/=0)return
    value=onePattern1(122) - onePattern2(122)
    if(value/=0)return
    value=onePattern1(123) - onePattern2(123)
    if(value/=0)return
    value=onePattern1(124) - onePattern2(124)
    if(value/=0)return
    value=onePattern1(125) - onePattern2(125)
    if(value/=0)return

#endif

  end subroutine patternComparation


  subroutine patternInsertion(nrows,onePattern,freq)
    !
    ! Insert a pattern into an array. The array is sorted in increasing order, i.e., array(1) < array(2) < ...
    !
    integer(4), intent(in)                          :: nrows
    integer(4), intent(in)                          :: onePattern(nrows)
    integer(4), intent(in), optional                :: freq
    integer(4)                                      :: isNew, value
    integer(4)                                      :: pos,ipattern,irow,newipattern,istat,poslocal
    
#if MODE==0
    type(patternType), pointer  :: tempPatternArray(:) 
#else
    integer(4),pointer        :: tempPatternArray(:,:)    
    integer(4),pointer        :: tempFrequency(:)    
#endif

    if (npatterns == 0) then
       memusage=1024
#if MODE==0
       allocate(patternArray(1))
       allocate(patternArray(1)%pattern(nrows))
       do irow = 1,nrows
          patternArray(1)%pattern(irow) = onePattern(irow)
       end do
       if(present(freq)) then
          patternArray(1)%frequency = freq
       else
          patternArray(1)%frequency = 1
       end if
#else
       !allocate(patternArray(nrows,1))
       !allocate(frequency(1))
       allocate(patternArray(nrows,memusage))
       allocate(frequency(memusage))
       patternArray(1:nrows,1) = onePattern(1:nrows)
       if(present(freq)) then
          frequency(1) = freq
       else
          frequency(1) = 1
       end if
#endif
       npatterns = 1
    else              

       pos = 0

       isNew    = 0
       do while (isNew == 0 .and. pos < npatterns )
          pos = pos + 1

#if MODE==0         
          call patternComparation(nrows,patternArray(pos)%pattern(1:nrows),onePattern,value)
#else
          call patternComparation(nrows,patternArray(1:nrows,pos),onePattern,value)
#endif

          if (value == 1) then
             pos = pos -1
             isNew = 1
          elseif (value == 0) then
#if MODE==0         
             if(present(freq)) then
                patternArray(pos)%frequency = patternArray(pos)%frequency + freq
             else
                patternArray(pos)%frequency = patternArray(pos)%frequency + 1
             end if
#else
             if(present(freq)) then
                frequency(pos) = frequency(pos) + freq
             else
                frequency(pos) = frequency(pos) + 1
             end if
#endif
             isNew = -1
          end if          
       end do

       if ( isNew /= -1 ) then

#if MODE==0
          allocate(tempPatternArray(npatterns), stat=istat)
          do ipattern = 1, npatterns
             allocate(tempPatternArray(ipattern)%pattern(nrows), stat=istat)
          end do

          do ipattern = 1,npatterns
             tempPatternArray(ipattern)%pattern(1:nrows) = patternArray(ipattern)%pattern(1:nrows)
             tempPatternArray(ipattern)%frequency = patternArray(ipattern)%frequency
          end do

          do ipattern = 1, npatterns
             deallocate(patternArray(ipattern)%pattern)
          end do
          deallocate(patternArray)

          allocate(patternArray(npatterns+1), stat = istat)
          do ipattern = 1, npatterns+1
             allocate(patternArray(ipattern)%pattern(nrows), stat=istat)
          end do


          do newipattern = 1,pos
             patternArray(newipattern)%pattern(1:nrows) = tempPatternArray(newipattern)%pattern(1:nrows)
             patternArray(newipattern)%frequency = tempPatternArray(newipattern)%frequency
          end do


          do irow = 1,nrows
             patternArray(pos+1)%pattern(irow) = onePattern(irow)
          end do

          if(present(freq)) then
             patternArray(pos+1)%frequency = freq
          else
             patternArray(pos+1)%frequency = 1
          end if


          do newipattern = pos+2,npatterns+1
             patternArray(newipattern)%pattern(1:nrows) = tempPatternArray(newipattern-1)%pattern(1:nrows)
             patternArray(newipattern)%frequency = tempPatternArray(newipattern-1)%frequency
          end do


          do ipattern = 1, npatterns
             deallocate(tempPatternArray(ipattern)%pattern, stat=istat)
          end do
          deallocate(tempPatternArray)

#else
          if(memusage==npatterns)then

             memusage=2*memusage

             allocate(tempPatternArray(nrows,npatterns))
             allocate(tempFrequency(npatterns))

             tempPatternArray(1:nrows,1:npatterns) = patternArray(1:nrows,1:npatterns)
             tempFrequency(1:npatterns) = frequency(1:npatterns)

             deallocate(patternArray)
             deallocate(frequency)

             !allocate(patternArray(nrows,npatterns+1), stat = istat)
             !allocate(frequency(npatterns+1), stat = istat)
             allocate(patternArray(nrows,memusage), stat = istat)
             allocate(frequency(memusage), stat = istat)

             patternArray(1:nrows,1:pos) = tempPatternArray(1:nrows,1:pos)
             frequency(1:pos) = tempFrequency(1:pos) 

             patternArray(1:nrows,pos+1) = onePattern(1:nrows)

             if(present(freq)) then
                frequency(pos+1) = freq
             else
                frequency(pos+1) = 1
             end if

             patternArray(1:nrows,pos+2:npatterns+1) = tempPatternArray(1:nrows,pos+1:npatterns)
             frequency(pos+2:npatterns+1) = tempFrequency(pos+1:npatterns)

             deallocate(tempPatternArray)
             deallocate(tempFrequency)

          else
             !patternArray(1:nrows,1:pos) = tempPatternArray(1:nrows,1:pos)
             !frequency(1:pos) = tempFrequency(1:pos) 

             patternArray(1:nrows,pos+2:npatterns+1) = patternArray(1:nrows,pos+1:npatterns)
             frequency(pos+2:npatterns+1) = frequency(pos+1:npatterns)
          
             patternArray(1:nrows,pos+1) = onePattern(1:nrows)
             if(present(freq)) then
                frequency(pos+1) = freq
             else
                frequency(pos+1) = 1
             end if


          end if

#endif
          npatterns = npatterns + 1
       end if                    
    endif
    rootPos = int(real(npatterns) * 0.5)
    !rootPos = shiftr(npatterns,1)


  end subroutine patternInsertion

  subroutine patternInsertionLocal(nrows,onePattern,patternArrayLocal,frequencyLocal,&
                                  npatternsLocal,memusageLocal, rootPosLocal,freq)
    !
    ! Insert a pattern into an array. The array is sorted in increasing order, i.e., array(1) < array(2) < ...
    !
    integer(4), intent(in)                          :: nrows
    integer(4), intent(in)                          :: onePattern(nrows)
    integer(4), pointer, intent(inout)    :: patternArrayLocal(:,:)
    integer(4), pointer, intent(inout)    :: frequencyLocal(:)
    integer(4), intent(inout)    :: npatternsLocal
    integer(4), intent(inout)    :: memusageLocal
    integer(4), intent(inout)    :: rootPosLocal
    integer(4), intent(in), optional                :: freq
 
    integer(4)                                      :: isNew, value, lastvalue
    integer(4)                                      :: pos,ipattern,irow,newipattern,istat,poslocal
    
#if MODE==0
    type(patternType), pointer  :: tempPatternArray(:) 
#else
    integer(4),pointer        :: tempPatternArray(:,:)    
    integer(4),pointer        :: tempFrequency(:)    
#endif

#if MODE==0         
    print *,'MODE==0 (BASELINE) not supported.'
    stop
#endif

    if (npatternsLocal == 0) then

       memusageLocal=1024

       !allocate(patternArrayLocal(nrows,1))
       !allocate(frequencyLocal(1))
       allocate(patternArrayLocal(nrows,memusageLocal))
       allocate(frequencyLocal(memusageLocal))
       patternArrayLocal(1:nrows,1) = onePattern(1:nrows)
       if(present(freq)) then
          frequencyLocal(1) = freq
       else
          frequencyLocal(1) = 1
       end if
       npatternsLocal = 1
    else              

!       minn = 1
!       maxx = npatterns
!       isFound=0
!       counter=0
!       do while ( minn <= maxx .and. isFound == 0 )
!          !pos = int(real(minn+maxx)* 0.5)
!          pos = shiftr(minn+maxx,1)
!          call patternComparation(nrows,patternArray(1:nrows,pos),onePattern,value)
!          if (value == 0) then
!             isFound = 1
!          elseif (value == 1) then
!             maxx = pos - 1
!          else
!             minn = pos + 1
!          end if
!       end do
!



       pos = 0
       isNew    = 0
       lastValue=0
       do while (isNew == 0 .and. pos < npatternsLocal )
          pos = pos + 1

          call patternComparation(nrows,patternArrayLocal(1:nrows,pos),onePattern,value)

          !if (value == 1) then
          !   pos = pos -1
          !   isNew = 1
          !elseif (value == 0) then
          if (value == 0) then

             if(present(freq)) then
                frequencyLocal(pos) = frequencyLocal(pos) + freq
             else
                frequencyLocal(pos) = frequencyLocal(pos) + 1
             end if

             isNew = -1
          else
             if(value>0 .and. lastValue<=0)then !  patternArrayLocal(:,pos)   > onePattern .and.
                pos=pos-1                      !  patternArrayLocal(:,pos-1) < onePattern  
                isNew = 1
             end if

          end if          
          lastvalue=value
       end do
       if(isNew/=-1 .and. isNew/=1 .and. pos==npatternsLocal) then
          
          isNew = 1
       end if

!
!
!       pos=0
!       isNew=0
!       do while (isNew == 0 .and. pos < npatternsLocal )
!          pos = pos + 1
!
!          call patternComparation(nrows,patternArrayLocal(1:nrows,pos),onePattern,value)
!
!          if (value == 1) then
!             pos = pos -1
!             isNew = 1
!          elseif (value == 0) then
!
!             if(present(freq)) then
!                frequencyLocal(pos) = frequencyLocal(pos) + freq
!             else
!                frequencyLocal(pos) = frequencyLocal(pos) + 1
!             end if
!
!             isNew = -1
!          end if          
!       end do


       if ( isNew == 1 ) then

          if(memusageLocal==npatternsLocal)then

             memusageLocal=2*memusageLocal
             allocate(tempPatternArray(nrows,npatternsLocal))
             allocate(tempFrequency(npatternsLocal))

             tempPatternArray(1:nrows,1:npatternsLocal) = patternArrayLocal(1:nrows,1:npatternsLocal)
             tempFrequency(1:npatternsLocal) = frequencyLocal(1:npatternsLocal)

             deallocate(patternArrayLocal)
             deallocate(frequencyLocal)

             !allocate(patternArrayLocal(nrows,npatternsLocal+1), stat = istat)
             !allocate(frequencyLocal(npatternsLocal+1), stat = istat)
             allocate(patternArrayLocal(nrows,memusageLocal), stat = istat)
             allocate(frequencyLocal(memusageLocal), stat = istat)

             !patternArrayLocal(1:nrows,1:pos) = tempPatternArray(1:nrows,1:pos)
             !frequencyLocal(1:pos) = tempFrequency(1:pos) 

             if(pos>=1)then
                patternArrayLocal(1:nrows,1:pos) = tempPatternArray(1:nrows,1:pos)
                frequencyLocal(1:pos) = tempFrequency(1:pos) 
             end if

             patternArrayLocal(1:nrows,pos+1) = onePattern(1:nrows)

             if(present(freq)) then
                frequencyLocal(pos+1) = freq
             else
                frequencyLocal(pos+1) = 1
             end if

             if(pos<=npatternsLocal-1)then
             patternArrayLocal(1:nrows,pos+2:npatternsLocal+1) = &
                                            tempPatternArray(1:nrows,pos+1:npatternsLocal)
                                            !tempPatternArray(1:nrows,pos+1:npatternsLocal)
             frequencyLocal(pos+2:npatternsLocal+1) = tempFrequency(pos+1:npatternsLocal)
             end if

             deallocate(tempPatternArray)
             deallocate(tempFrequency)

          else !if(isNew==-1)then

             if(pos<=npatternsLocal-1)then
             patternArrayLocal(1:nrows,pos+2:npatternsLocal+1) = &
                                            patternArrayLocal(1:nrows,pos+1:npatternsLocal)
             frequencyLocal(pos+2:npatternsLocal+1) = frequencyLocal(pos+1:npatternsLocal)
             end if          

             patternArrayLocal(1:nrows,pos+1) = onePattern(1:nrows)
             if(present(freq)) then
                frequencyLocal(pos+1) = freq
             else
                frequencyLocal(pos+1) = 1
             end if
     
          end if

          npatternsLocal = npatternsLocal + 1
       end if                    
    endif
    rootPosLocal = int(real(npatternsLocal) * 0.5)
    !rootPosLocal = shiftr(npatternsLocal,1)

  end subroutine patternInsertionLocal


!  subroutine patternBuildPivots(facies,maxlevel)
!    integer(4), intent(in) :: facies,maxlevel
!    integer(4) :: i,level,counter,pivotsize,offset,limit
!
!
!    pivotsize=0
!    do i=0,maxlevel-1
!       pivotsize=pivotsize+(facies-1)*(facies**i)
!    end do
!
!    print *,'pivotsize=',pivotsize
!
!    allocate(pivot(pivotsize))
!
!    pivot=0   
! 
!    offset = 0
!    limit = 0
!    counter=0
!#if MODE==0
!    !pivot(counter)=patternArray(1)%pattern(1)
!#else
!    !pivot(counter)=patternArray(1,1)
!#endif 
!    do level=1,maxlevel
!       
!       if(level>1)then
!          if(counter<facies-1)then
!             do i=counter+1,facies-1
!                pivot(offset + i)=npatterns
!             end do
!          end if
!          offset=(facies-1)*(facies**(level-2)) 
!          limit=(facies-1)*(facies**(level-1)) 
!       else
!          offset=0
!          limit=(facies-1) 
!       end if
!
!       counter=0
!       
!
!       do i=2,npatterns
!#if MODE==0
!          if(patternArray(i-1)%pattern(level)/=patternArray(i)%pattern(level))then
!#else
!          if(patternArray(level,i-1)/=patternArray(level,i))then
!#endif
!             if(.not.any(pivot.eq.i))then
!                counter=counter+1
!                pivot(offset + counter)=i
!                !pivot((facies-1)*pow(facies,level) - 1 + counter)=i
!                !if(counter==facies-1)then
!                if(counter==limit)then
!                   exit 
!                end if
!             end if
!          end if
!       end do
!       print *,'offset=',offset,'couter=',counter,'limit=',limit,'npatterns=',npatterns
!    end do
!
!
!    do i=1,pivotsize
!       print *,'pivot(',i,')=',pivot(i)
!    end do
!
!
!  end subroutine patternBuildPivots


  subroutine printPatternArray(nrows)
     integer(4),intent(in)  :: nrows
     integer(4)             :: ipattern,irow

     !print *,npatterns
#if MODE==0
     do ipattern=1,npatterns
        print *,patternArray(ipattern)%pattern(1:nrows),' -- ',patternArray(ipattern)%frequency
     end do
#else
     do ipattern=1,npatterns
        print *,patternArray(1:nrows,ipattern),' -- ',frequency(ipattern)
     end do
#endif

  end subroutine 

  subroutine patternSearch(nrows,onePattern,pos)

    integer(4), intent(in)   :: nrows
    integer(4), intent(in)   :: onePattern(nrows)
    integer(4), intent(out)  :: pos
    integer(4)               :: isFound,minn,maxx,value,ii,jj
    integer(4)               :: counter,levels,facies

#if MODE==0 

    minn = 1
    maxx = npatterns
   
    isFound=0

    do while ( minn <= maxx .and. isFound == 0 )
       !pos = int(real(minn+maxx)* 0.5)
       pos = shiftr(minn+maxx,1)
       call patternComparation(nrows,patternArray(pos)%pattern(1:nrows),onePattern,value)
       if (value == 0) then
          isFound = 1
       elseif (value == 1) then
          maxx = pos - 1
       else
          minn = pos + 1
       end if
    end do

    if (isFound == 0) pos = -1

#elif MODE==1

    minn = 1
    maxx = npatterns
   
    isFound=0
    counter=0

    do while ( minn <= maxx .and. isFound == 0 )
       !pos = int(real(minn+maxx)* 0.5)
       pos = shiftr(minn+maxx,1)
       call patternComparation(nrows,patternArray(1:nrows,pos),onePattern,value)
       if (value == 0) then
          isFound = 1
       elseif (value == 1) then
          maxx = pos - 1
       else
          minn = pos + 1
       end if
    end do

    if (isFound == 0) pos = -1



!    minn = 1
!    maxx = npatterns
!   
!    isFound=0
!    counter=1
!    levels=0
!    facies=2
!
!    do while ( minn <= maxx .and. isFound == 0 )
!       !pos = int(real(minn+maxx)* 0.5)
!       levels=levels+1
!       if(levels>2)then
!          pos = shiftr(minn+maxx,1)
!          call patternComparation(nrows,patternArray(1:nrows,pos),onePattern,value)
!          if (value == 0) then
!             isFound = 1
!          elseif (value == 1) then
!             maxx = pos - 1
!          else
!             minn = pos + 1
!          end if
!       else
!          pos = pivot(counter)
!          call patternComparation(nrows,patternArray(1:nrows,pos),onePattern,value)
!          if (value == 0) then
!             isFound = 1
!          elseif (value == 1) then
!             maxx = pivot(counter)-1
!             counter=counter+1
!          else
!             minn = pivot(counter)
!             counter=counter+2
!          end if
!          endif
!
!    end do
!
!    if (isFound == 0) pos = -1




#elif MODE==125

#if REGSIZE==8

    integer(4)  :: irow

    integer(4)  :: oc1
    integer(4)  :: oc2
    integer(4)  :: oc3
    integer(4)  :: oc4
    integer(4)  :: oc5
    integer(4)  :: oc6
    integer(4)  :: oc7
    integer(4)  :: oc8


    !print *,'patternSearch: STENCIL=125 REGISTER=8'

    oc1=onePattern(1)
    oc2=onePattern(2)
    oc3=onePattern(3)
    oc4=onePattern(4)
    oc5=onePattern(5)
    oc6=onePattern(6)
    oc7=onePattern(7)
    oc8=onePattern(8)

    minn = 1
    maxx = npatterns
    isFound=0
    value=-1
    pos=-1 
    do while ( minn <= maxx )
       !pos = int(real(minn+maxx) * 0.5)
       pos = shiftr(minn+maxx,1)
       do while (.true.)
          value=patternArray(1,pos) - oc1
          if(value/=0)exit
          value=patternArray(2,pos) - oc2
          if(value/=0)exit
          value=patternArray(3,pos) - oc3
          if(value/=0)exit
          value=patternArray(4,pos) - oc4
          if(value/=0)exit
          value=patternArray(5,pos) - oc5
          if(value/=0)exit
          value=patternArray(6,pos) - oc6
          if(value/=0)exit
          value=patternArray(7,pos) - oc7
          if(value/=0)exit
          value=patternArray(8,pos) - oc8
          if(value/=0)exit

          irow = 8
          do while ( value == 0 .and. irow < 125 )
             irow = irow + 1
             value=patternArray(irow,pos) - onePattern(irow)
          end do
      
          !if(value/=0)exit
 
          if(value==0) then
             isFound = 1
             return  ! value=0
          end if

          exit

       end do
       if (value > 0) then
          maxx = pos - 1
       else
          minn = pos + 1
       end if
    end do
    pos = -1

#elif REGSIZE==16

    integer(4)  :: irow

    integer(4)  :: oc1
    integer(4)  :: oc2
    integer(4)  :: oc3
    integer(4)  :: oc4
    integer(4)  :: oc5
    integer(4)  :: oc6
    integer(4)  :: oc7
    integer(4)  :: oc8
    integer(4)  :: oc9
    integer(4)  :: oc10
    integer(4)  :: oc11
    integer(4)  :: oc12
    integer(4)  :: oc13
    integer(4)  :: oc14
    integer(4)  :: oc15
    integer(4)  :: oc16

    !print *,'patternSearch: STENCIL=125 REGISTER_NUM=32'

    oc1=onePattern(1)
    oc2=onePattern(2)
    oc3=onePattern(3)
    oc4=onePattern(4)
    oc5=onePattern(5)
    oc6=onePattern(6)
    oc7=onePattern(7)
    oc8=onePattern(8)
    oc9=onePattern(9)
    oc10=onePattern(10)
    oc11=onePattern(11)
    oc12=onePattern(12)
    oc13=onePattern(13)
    oc14=onePattern(14)
    oc15=onePattern(15)
    oc16=onePattern(16)

    minn = 1
    maxx = npatterns
    isFound=0
    value=-1
    pos=-1 
    do while ( minn <= maxx )
       !pos = int(real(minn+maxx) * 0.5)
       pos = shiftr(minn+maxx,1)
       do while (.true.)
          value=patternArray(1,pos) - oc1
          if(value/=0)exit
          value=patternArray(2,pos) - oc2
          if(value/=0)exit
          value=patternArray(3,pos) - oc3
          if(value/=0)exit
          value=patternArray(4,pos) - oc4
          if(value/=0)exit
          value=patternArray(5,pos) - oc5
          if(value/=0)exit
          value=patternArray(6,pos) - oc6
          if(value/=0)exit
          value=patternArray(7,pos) - oc7
          if(value/=0)exit
          value=patternArray(8,pos) - oc8
          if(value/=0)exit
          value=patternArray(9,pos) - oc9
          if(value/=0)exit
          value=patternArray(10,pos) - oc10
          if(value/=0)exit
          value=patternArray(11,pos) - oc11
          if(value/=0)exit
          value=patternArray(12,pos) - oc12
          if(value/=0)exit
          value=patternArray(13,pos) - oc13
          if(value/=0)exit
          value=patternArray(14,pos) - oc14
          if(value/=0)exit
          value=patternArray(15,pos) - oc15
          if(value/=0)exit
          value=patternArray(16,pos) - oc16
          if(value/=0)exit

          irow = 16
          do while ( value == 0 .and. irow < 125 )
             irow = irow + 1
             value=patternArray(irow,pos) - onePattern(irow)
          end do
      
          !if(value/=0)exit
 
          if(value==0) then
             isFound = 1
             return  ! value=0
          end if

          exit

       end do
       if (value > 0) then
          maxx = pos - 1
       else
          minn = pos + 1
       end if
    end do
    pos = -1

#elif REGSIZE==32
   
    integer(4)  :: irow

    integer(4)  :: oc1
    integer(4)  :: oc2
    integer(4)  :: oc3
    integer(4)  :: oc4
    integer(4)  :: oc5
    integer(4)  :: oc6
    integer(4)  :: oc7
    integer(4)  :: oc8
    integer(4)  :: oc9
    integer(4)  :: oc10
    integer(4)  :: oc11
    integer(4)  :: oc12
    integer(4)  :: oc13
    integer(4)  :: oc14
    integer(4)  :: oc15
    integer(4)  :: oc16
    integer(4)  :: oc17
    integer(4)  :: oc18
    integer(4)  :: oc19
    integer(4)  :: oc20
    integer(4)  :: oc21
    integer(4)  :: oc22
    integer(4)  :: oc23
    integer(4)  :: oc24
    integer(4)  :: oc25
    integer(4)  :: oc26
    integer(4)  :: oc27
    integer(4)  :: oc28
    integer(4)  :: oc29
    integer(4)  :: oc30
    integer(4)  :: oc31

    !print *,'patternSearch: STENCIL=125 REGISTER_NUM=32'

    oc1=onePattern(1)
    oc2=onePattern(2)
    oc3=onePattern(3)
    oc4=onePattern(4)
    oc5=onePattern(5)
    oc6=onePattern(6)
    oc7=onePattern(7)
    oc8=onePattern(8)
    oc9=onePattern(9)
    oc10=onePattern(10)
    oc11=onePattern(11)
    oc12=onePattern(12)
    oc13=onePattern(13)
    oc14=onePattern(14)
    oc15=onePattern(15)
    oc16=onePattern(16)
    oc17=onePattern(17)
    oc18=onePattern(18)
    oc19=onePattern(19)
    oc20=onePattern(20)
    oc21=onePattern(21)
    oc22=onePattern(22)
    oc23=onePattern(23)
    oc24=onePattern(24)
    oc25=onePattern(25)
    oc26=onePattern(26)
    oc27=onePattern(27)
    oc28=onePattern(28)
    oc29=onePattern(29)
    oc30=onePattern(30)
    oc31=onePattern(31)
    minn = 1
    maxx = npatterns
    isFound=0
    value=-1
    pos=-1 
    do while ( minn <= maxx )
       !pos = int(real(minn+maxx) * 0.5)
       pos = shiftr(minn+maxx,1)
       do while (.true.)
          value=patternArray(1,pos) - oc1
          if(value/=0)exit
          value=patternArray(2,pos) - oc2
          if(value/=0)exit
          value=patternArray(3,pos) - oc3
          if(value/=0)exit
          value=patternArray(4,pos) - oc4
          if(value/=0)exit
          value=patternArray(5,pos) - oc5
          if(value/=0)exit
          value=patternArray(6,pos) - oc6
          if(value/=0)exit
          value=patternArray(7,pos) - oc7
          if(value/=0)exit
          value=patternArray(8,pos) - oc8
          if(value/=0)exit
          value=patternArray(9,pos) - oc9
          if(value/=0)exit
          value=patternArray(10,pos) - oc10
          if(value/=0)exit
          value=patternArray(11,pos) - oc11
          if(value/=0)exit
          value=patternArray(12,pos) - oc12
          if(value/=0)exit
          value=patternArray(13,pos) - oc13
          if(value/=0)exit
          value=patternArray(14,pos) - oc14
          if(value/=0)exit
          value=patternArray(15,pos) - oc15
          if(value/=0)exit
          value=patternArray(16,pos) - oc16
          if(value/=0)exit
          value=patternArray(17,pos) - oc17
          if(value/=0)exit
          value=patternArray(18,pos) - oc18
          if(value/=0)exit
          value=patternArray(19,pos) - oc19
          if(value/=0)exit
          value=patternArray(20,pos) - oc20
          if(value/=0)exit
          value=patternArray(21,pos) - oc21
          if(value/=0)exit
          value=patternArray(22,pos) - oc22
          if(value/=0)exit
          value=patternArray(23,pos) - oc23
          if(value/=0)exit
          value=patternArray(24,pos) - oc24
          if(value/=0)exit
          value=patternArray(25,pos) - oc25
          if(value/=0)exit
          value=patternArray(26,pos) - oc26
          if(value/=0)exit
          value=patternArray(27,pos) - oc27
          if(value/=0)exit
          value=patternArray(28,pos) - oc28
          if(value/=0)exit
          value=patternArray(29,pos) - oc29
          if(value/=0)exit
          value=patternArray(30,pos) - oc30
          if(value/=0)exit
          value=patternArray(31,pos) - oc31
          if(value/=0)exit
          irow = 31
          do while ( value == 0 .and. irow < 125 )
             irow = irow + 1
             value=patternArray(irow,pos) - onePattern(irow)
          end do
      
          !if(value/=0)exit
 
          if(value==0) then
             isFound = 1
             return  ! value=0
          end if

          exit

       end do
       if (value > 0) then
          maxx = pos - 1
       else
          minn = pos + 1
       end if
    end do
    pos = -1
#else
    print *,'ERROR: REGSIZE not defined at compilation time.'
#endif

#endif

  end subroutine patternSearch

end module patternOperations
