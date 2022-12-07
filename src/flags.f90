! flags.f90  Version: 29.11.2022-dev
! -----------------------------------------------------------------------
! Copyright (C) 2022  Doug Tinkham
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
! Some values below are not implemented yet in 2022 version of source, but
! are in 2020 version of src. They are not parameters/constants, but as of
! now, none are adjusted after they are read from theriak.ini. Default 
! values below seem to work well. OUTFLAGS controls whether or not some of  
! these are printed along the right side of graphics diagrams produced by 
! domino. Not thread safe. save attribut is redundant since in module.

MODULE FLAGS
    !
    !declare and set default values
    !
    LOGICAL(4), SAVE :: OUTFLAGS = .FALSE.
    !
    INTEGER(4)    :: DOSTP = 2    !steep version:
                                  !0 = original. hard-coded vals
                                  !1 = original+ STPZAL,STPZNN,STPVMIN, array syntax
                                  !2 = no neg/zero checks. fast, 
                                  !    perhaps less stable when several neg ppns
    INTEGER(4)    :: DOACT = 2    !sr activi version
                                  !0 = sr activi, rver
                                  !2 = sr activi2 lver
    !
    REAL(8), SAVE :: STPZAL    = 2.22D-24      !for DOSTP=1  link to ZEROEM
    REAL(8), SAVE :: STPZNN    = 3.333333D-55  !for DOSTP=1  link to PMINXXX
    REAL(8), SAVE :: STPVMIN   = 1.0D-20       !for DOSTP=1; def 1.0D-20
    !
    REAL(8), SAVE :: ZEROEM    = 2.22044D-24
    REAL(8), SAVE :: PMINXXX   = 2.44249D-15     !>eps; may be decreased at times
    REAL(8), SAVE :: PMINAAA   = 2.22507D-305    !>tiny;
    REAL(8), SAVE :: PMINXELSI = 0.0D0 
    !
    REAL(8), SAVE :: VAFFSCALE  = 3.0D-02
    REAL(8), SAVE :: RLOWEXP    = -706.0D0
    INTEGER(4), SAVE :: ILOWEXP = -706
    LOGICAL, SAVE :: DOEXTRAPPNSVA  = .FALSE.
    !
    !not all implemented yet
    LOGICAL, SAVE :: DOMINMAXSIELST = .FALSE.
    LOGICAL, SAVE :: DOBINGVA       = .FALSE.
    !
    !2021-12-19 - below flags not implemented
    LOGICAL, SAVE    :: SWITCHGCMAX = .FALSE.
    INTEGER(4), SAVE :: LOO1GCMAX   = 7
    INTEGER(4), SAVE :: GCMAXBIG    = 250
    INTEGER(4), SAVE :: GCMAXSMALL  = 15 !3
    REAL(8), SAVE    :: ETCSCALE    = 0.5D0
    !2022-01-07 for SR CLEAN to remove exceeding CALMAX error when 
    !keeping too many soln comps in memory
    INTEGER(4), SAVE :: LOO1ISTABCUT = 20  !cdc default is 20.
    ! These are not flags, but counters..
    INTEGER(4), SAVE :: nbadstinvecadd = 0 !# bad st in sr vecadd
    !
    REAL(8), PARAMETER :: TINEE=TINY(0.0D0)
    REAL(8), PARAMETER :: LZERO=DLOG(TINEE)
!    REAL(8), PARAMETER :: PMINAAA   = TINEE  !2.22507D-305    !>tiny;
!    REAL(8), PARAMETER :: LMINAAA=DLOG(PMINAAA)
!    REAL(8), PARAMETER :: PMINXELSI = 0.0D0
!    REAL(8), PARAMETER :: LMINXELSI=LZERO !DLOG(PMINXELSI)


  CONTAINS
    !
    SUBROUTINE CALCFLAGS
        IMPLICIT NONE
        !include 'theriak.cmn'
        INCLUDE 'files.cmn'
        !
        INTEGER(4)    :: istat,spos,epos,dpos,bpos,lenofstr
        CHARACTER*500 linestr
        CHARACTER*1000 datstr,tdatstr
        CHARACTER(len=16) :: tstr1,tstr2
        !
        ! read (CALC-FLAGS)  2019-08-03
        !
        !
        datstr = ''
        !
        !Load all lines of params and concatenate into formatted datstr.
        do
          READ(UNIT=ini,FMT='(A500)',IOSTAT=istat) linestr
          if(istat /= 0) print *, 'CALCFLAGS read gave IOSTAT = ',istat
          if(linestr(1:4) == '') EXIT
          linestr = ADJUSTL(linestr)
          spos = INDEX(linestr,'!')
          if(spos .NE. 0) linestr = linestr(1:spos-1)
          !print *, trim(linestr)
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
                  tstr2 = tdatstr(1:)                   !holds value in this case
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
        !print*,TRIM(ADJUSTL(tdatstr))

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
            !case("STPNMUECUT")
            !  if(tstr2 /= '') then
            !    READ(tstr2,*) STPNMUECUT
            !  endif
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
            case("VAFFSCALE")
              if(tstr2 /= '') then
                READ(tstr2,*) VAFFSCALE
              endif
            case("RLOWEXP")
              if(tstr2 /= '') then
                READ(tstr2,*) RLOWEXP
              endif
            case("ILOWEXP")
              if(tstr2 /= '') then
                READ(tstr2,*) ILOWEXP
              endif
            case("OUTFLAGS")
              CALL LOWUP(tstr2)
              if(tstr2 == 'TRUE') OUTFLAGS = .TRUE.
            case("DOBINGVA")
              CALL LOWUP(tstr2)
              if(tstr2 == 'TRUE') DOBINGVA = .TRUE.
            case("DOEXTRAPPNSVA")
              CALL LOWUP(tstr2)
              if(tstr2 == 'TRUE') DOEXTRAPPNSVA = .TRUE.
              if(tstr2 == 'FALSE') DOEXTRAPPNSVA = .FALSE.
              !
            case("DOMINMAXSIELST")
              print *, "Found DOMINMAXSIELST, but not implemented"
              CALL LOWUP(tstr2)
              if(tstr2 == 'TRUE') DOMINMAXSIELST = .TRUE.
            case("SWITCHGCMAX")  !2021-12-19: for SR ETC
              print *, "Found SWITCHGCMAX, but not implemented"
              CALL LOWUP(tstr2)
              if(tstr2 == 'TRUE') SWITCHGCMAX = .TRUE.
              if(tstr2 == 'FALSE') SWITCHGCMAX = .FALSE.
            case("LOO1GCMAX") !2021-12-19: for SR ETC
              print *, "Found LOO1GCMAX, but not implemented"
              if(tstr2 /= '') then
                READ(tstr2,*) LOO1GCMAX
              endif
            case("GCMAXBIG") !2021-12-19: for SR ETC
              print *, "Found GCMAXBIG, but not implemented"
              if(tstr2 /= '') then
                READ(tstr2,*) GCMAXBIG
              endif
            case("GCMAXSMALL") !2021-12-19: for SR ETC
              print *, "Found GCMAXSMALL, but not implemented"
              if(tstr2 /= '') then
                READ(tstr2,*) GCMAXSMALL
              endif
            case("LOO1ISTABCUT") !2022-10-07: for SR CLEAN
              print *, "Found LOO1ISTABCUT, but not implemented"
              if(tstr2 /= '') then
                READ(tstr2,*) LOO1ISTABCUT
              endif
            case("ETCSCALE") !2021-12-19: for SR ETC
              print *, "Found ETCSCALE, but not implemented"
              if(tstr2 /= '') then
                READ(tstr2,*) ETCSCALE
              endif
            case default
              print *, 'Calc Flag '//tstr1//' not recognized.'
          END SELECT
        end do
        return
    end subroutine CALCFLAGS


END MODULE FLAGS
