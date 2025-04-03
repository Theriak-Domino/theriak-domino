! flags.f90  Version: 2023.06.11
! -----------------------------------------------------------------------
! Copyright (C) 2022-2023  Doug Tinkham
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <https://www.gnu.org/licenses/>.
! -----------------------------------------------------------------------
!
! Module with various run-time specified values to be read in theriak.ini
! in a $CALC-FLAGS section. Can contain numeric, boolean or string values
! Many are not parameters/constants; as of now, none are adjusted after
! they are read from theriak.ini.

MODULE FLAGS

  REAL(8), PARAMETER :: TINEE=TINY(0.0D0)
  REAL(8), PARAMETER :: LZERO=DLOG(TINEE)  
  REAL(8), PARAMETER :: EPSI =EPSILON(0.0D0) 

  LOGICAL(4) :: OUTFLAGS  = .FALSE.   !!prints some num calc params on dominograms
  
  INTEGER(4) :: DOSTP     = 2         !!steep version:
                                      !!0 = original. hard-coded vals
                                      !!1 = original+ STPZAL,STPZNN,STPVMIN
                                      !!2 = no bing business. 

  INTEGER(4) :: DOACT     = 2         !!sr activi version
                                      !!0 = sr activi, rver
                                      !!2 = sr activi2 lver
  
  REAL(8)    :: STPZAL    = 2D-24     !!for DOSTP=1;  link to ZEROEM?
  REAL(8)    :: STPZNN    = 3D-55     !!for DOSTP=1;  link to PMINXXX?
  REAL(8)    :: STPVMIN   = 1D-20     !!for DOSTP=1;  def 1.0D-20
  
  REAL(8)    :: ZEROEM    = 2D-24     !!
  REAL(8)    :: PMINXXX   = 2D-24     !!may be in/decreased at times
  REAL(8)    :: PMINXELSI = 0.0D0     !!
  REAL(8)    :: PMINAAA   = TINEE*1D1 !!2.22507D-305    !>tiny;
  
  REAL(8)    :: RIDICUT   = 1D5       !!sr newph: newph created if grad < val
  REAL(8)    :: MINGRADPPN= EPSI      !!sr steep2: mue set 0 if ppn < val
  
  REAL(8)    :: EMDXSCALE = 2D-1      !!sr marmin: scale dxstart when starting
                                      !!min from em (dec to help get wide misc
                                      !!limb mins at low T)
  INTEGER(4) :: EMSTARTMOD = 10       !!sr addph: start min from em comp every X 
                                      !!loops; def is 10, 25 works a bit faster

  INTEGER(4) :: L1SCANMAX = 5         !!sr addph

  REAL(8)    :: VAFFSCALE = 3D-02     !!sr vecadd
  
  LOGICAL :: DOEXTRAPPNSVA = .FALSE.  !!sr vecadd
  
  INTEGER(4) :: L1NEWSEED    = 12     !!sr theria; LOO1 to add seeds; def was 2, inc to # sys comp
        
  INTEGER(4) :: NLOOPSDOSEED = 1      !!sr theria; #loops to keep adding seeds 
  
  LOGICAL    :: DOAONNSEED   = .TRUE. !!sr theria; do addph in loop adding seends

  !REAL(8)    :: RLOWEXP       = -706.0D0 !!not used
  !INTEGER(4) :: ILOWEXP       = -706     !!not used
  !LOGICAL    :: DOMINMAXSIELST= .FALSE.  !!not used; to add to sr spacetest
  !REAL(8)    :: ETCSCALE      = 0.5D0    !!not used; to add to sr etc
  !for SR CLEAN to remove exceeding CALMAX error when 
  !keeping too many soln comps in memory
  !INTEGER(4) :: LOO1ISTABCUT  = 20  !!not used; cdc default is 20.

  CONTAINS
    !
    SUBROUTINE CALCFLAGS
        IMPLICIT NONE
        !include 'theriak.cmn'
        INCLUDE 'files.cmn'
        !
        INTEGER(4)    :: istat,spos,epos,dpos,bpos,lenofstr
        CHARACTER(500) linestr
        CHARACTER(1000) datstr,tdatstr
        CHARACTER(len=16) :: tstr1,tstr2

        datstr = ''

        !Load all lines of params and concatenate into formatted datstr.
        do
          READ(UNIT=ini,FMT='(A500)',IOSTAT=istat) linestr
          if(istat /= 0) print *, 'CALCFLAGS read gave IOSTAT = ',istat
          if(linestr(1:4) == '') EXIT
          linestr = ADJUSTL(linestr)
          spos = INDEX(linestr,'!')
          if(spos .NE. 0) linestr = linestr(1:spos-1)
          tdatstr = linestr
          !loop here to remove all spaces, insert ;
          do
              !get key
              epos = INDEX(tdatstr,'=')
              if(epos == 0) EXIT
              tstr1 = TRIM(ADJUSTL(tdatstr(1:epos-1)))  !holds key
              tdatstr = TRIM(ADJUSTL(tdatstr(epos+1:))) !value and rest of line
              lenofstr = LEN_TRIM(tdatstr,kind=4)
              if(lenofstr .NE. 0) then
                !key= is stripped off, so get value
                bpos = INDEX(tdatstr,' ')               !space b/w value and remaining string
                if(bpos == 0) then
                  !must be last one, and no space? Never hit this case afaict.
                  tstr2 = tdatstr(1:LEN(tstr2))                   !holds value in this case
                  tdatstr = TRIM(ADJUSTL(tdatstr))      !is needed? 
                  print*,'ERROR in SR CALCFLAGS. This value is not getting set:',tdatstr
                else
                  tstr2 = tdatstr(1:bpos-1)  !holds value
                  tdatstr = TRIM(ADJUSTL(tdatstr(bpos+1:)))
                  datstr = TRIM(ADJUSTL(datstr))//TRIM(tstr1)//'='//TRIM(tstr2)//';'
                endif
              else
                !error ToDo
                print *, 'ERROR: lenofstr==0 in SR CALCFLAGS: check ',tstr1
                EXIT
              endif
          end do
        end do

        tdatstr = datstr

        do
          epos = INDEX(tdatstr,"=")
          dpos = INDEX(tdatstr,";")
          if(epos == 0) EXIT
          if(dpos == 0) then
            print *, "ERROR in CALCFLAGS... no trailing ;"
            EXIT
          endif
          tstr1 = tdatstr(1:epos-1)
          tstr2 = tdatstr(epos+1:dpos-1) !/ drops the trailing ; from tstr2
          tdatstr = tdatstr(dpos+1:)
          !keep CALL LOWUP for boolean flags tRuE/FalSE 
          SELECT case(tstr1)
            case("EMSTARTMOD")
              if(tstr2 /= '') then
                READ(tstr2,*) EMSTARTMOD
            endif
            case("DOSTP")
              if(tstr2 /= '') then
                READ(tstr2,*) DOSTP
            endif
            case("DOACT")
              if(tstr2 /= '') then
                READ(tstr2,*) DOACT
            endif
            case("STPZAL")
              if(tstr2 /= '') then
                READ(tstr2,*) STPZAL
              endif
            case("STPZNN")
              if(tstr2 /= '') then
                READ(tstr2,*) STPZNN
              endif
            case("STPVMIN")
              if(tstr2 /= '') then
                READ(tstr2,*) STPVMIN
              endif
            case("ZEROEM")
              if(tstr2 /= '') then
                READ(tstr2,*) ZEROEM
              endif
            case("PMINXELSI")
              if(tstr2 /= '') then
                READ(tstr2,*) PMINXELSI
              endif
            case("PMINXXX")
              if(tstr2 /= '') then
                READ(tstr2,*) PMINXXX
              endif
            case("PMINAAA")
              if(tstr2 /= '') then
                READ(tstr2,*) PMINAAA
              endif
            case("RIDICUT")
              if(tstr2 /= '') then
                READ(tstr2,*) RIDICUT
              endif
            case("MINGRADPPN")
              if(tstr2 /= '') then
                READ(tstr2,*) MINGRADPPN
              endif
            case("EMDXSCALE")
              if(tstr2 /= '') then
                READ(tstr2,*) EMDXSCALE
              endif
            case("L1SCANMAX")
              if(tstr2 /= '') then
                READ(tstr2,*) L1SCANMAX
              endif
            case("VAFFSCALE")
              if(tstr2 /= '') then
                READ(tstr2,*) VAFFSCALE
              endif
            case("L1NEWSEED")
              if(tstr2 /= '') then
                READ(tstr2,*) L1NEWSEED
              endif
            case("NLOOPSDOSEED")
              if(tstr2 /= '') then
                READ(tstr2,*) NLOOPSDOSEED
              endif
            case("DOAONNSEED")
              CALL LOWUP(tstr2)
              if(tstr2 == 'TRUE') DOAONNSEED = .TRUE.
              if(tstr2 == 'FALSE') DOAONNSEED = .FALSE.
            case("OUTFLAGS")
              CALL LOWUP(tstr2)
              if(tstr2 == 'TRUE') OUTFLAGS = .TRUE.
            case("DOEXTRAPPNSVA")
              CALL LOWUP(tstr2)
              if(tstr2 == 'TRUE') DOEXTRAPPNSVA = .TRUE.
              if(tstr2 == 'FALSE') DOEXTRAPPNSVA = .FALSE.
            !case("RLOWEXP")
            !  if(tstr2 /= '') then
            !    READ(tstr2,*) RLOWEXP
            !  endif
            !case("ILOWEXP")
            !  if(tstr2 /= '') then
            !    READ(tstr2,*) ILOWEXP
            !  endif
            !case("DOMINMAXSIELST")
            !  print *, "Found DOMINMAXSIELST, but not implemented"
            !  CALL LOWUP(tstr2)
            !  if(tstr2 == 'TRUE') DOMINMAXSIELST = .TRUE.
            !case("LOO1ISTABCUT") !2022-10-07: for SR CLEAN
            !  print *, "Found LOO1ISTABCUT, but not implemented"
            !  if(tstr2 /= '') then
            !    READ(tstr2,*) LOO1ISTABCUT
            !  endif
            !case("ETCSCALE") !2021-12-19: for SR ETC
            !  print *, "Found ETCSCALE, but not implemented"
            !  if(tstr2 /= '') then
            !    READ(tstr2,*) ETCSCALE
            !  endif
            case default
              print *, 'Calc Flag '//tstr1//' not recognized.'
          END SELECT
        end do
        return
    end subroutine CALCFLAGS

END MODULE FLAGS
