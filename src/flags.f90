!dkt Module with various run-time specified values to be read in theriak.ini
!    in a $CALC-FLAGS section. Can contain numeric, boolean or string values
!    Some values below are not implemented yet in 2022 version of source, but
!    are in 2000 version of src. They are not parameters/constants, but as of
!    now, none are adjusted after they are read from theriak.ini. Default values
!    below seem to work very well. PMINXXX, PMINAAA and PMINXELSI are the 
!    important keys. OUTFLAGS controls whether or not some of these are printed 
!    along the right side of graphics diagrams produced by domino.

module flags
    !
    !declare and set default values
    !
    logical(4), save :: OUTFLAGS = .false.
    !
    integer(4)    :: DOSTP = 1    !steep version:
                                  !0 = original. hard-coded vals
                                  !1 = original with STPZAL, STPZNN, STPVMIN, array math. def
                                  !2 = Straight downhill. fast, slow at lowT/highP, 
                                  !    less stable when several neg ppns.
    !
    real(8), save :: STPZAL    = 2.22D-24      !for DOSTP=2,3  link to ZEROEM
    real(8), save :: STPZNN    = 3.333333D-55  !for DOSTP=2,3  link to PMINXXX
    real(8), save :: STPVMIN   = 1.0D-20       !def 1.0D-20, DOSTP=2,3
    !
    real(8), save :: ZEROEM    = 2.22D-24
    real(8), save :: PMINXXX   = 3.333333D-55
    real(8), save :: PMINAAA   = 2.22D-307
    real(8), save :: PMINXELSI = 2.22D-16 
    !
    real(8), save :: VAFFSCALE  = 3.33D-02
    real(8), save :: RLOWEXP    = -706.0D0
    integer(4), save :: ILOWEXP = -706
    !
    !not all implemented yet
    logical, save :: DOMINMAXSIELST = .false.
    logical, save :: DOEXTRAPPNSVA  = .false.
    logical, save :: DOBINGVA       = .false.
    !
    !2021-12-19 - below flags not implemented
    logical, save    :: SWITCHGCMAX = .FALSE.
    integer(4), save :: LOO1GCMAX  = 7
    integer(4), save :: GCMAXBIG   = 250
    integer(4), save :: GCMAXSMALL = 15 !3
    real(8), save    :: ETCSCALE   = 0.5D0
    !2022-01-07 for SR CLEAN to remove exceeding CALMAX error when keeping too many soln comps in memory
    integer(4), save :: LOO1ISTABCUT = 20  !cdc default is 20.
    ! These are not flags, but counters..
    integer(4), save :: nbadstinvecadd = 0 !# of bad spacetests in sr vecadd
    !
  CONTAINS
    !
    subroutine CALCFLAGS
        implicit none
        !include 'theriak.cmn'
        include 'files.cmn'
        !
        integer(kind=4) :: istat,spos,epos,dpos,bpos,lenofstr
        character*500 linestr
        character*1000 datstr,tdatstr
        character(len=16) :: tstr1,tstr2
        !
        ! read (CALC-FLAGS)  2019-08-03
        !
        !
        datstr = ''
        !
        !Load all lines of params and concatenate into formatted datstr. To simplify.
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


end module flags
