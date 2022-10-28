!-----Version: 28.05.2022
! This file contains a collection of subroutines that depend on the operating
! system and the compiler used. The implementations shown below match those of
! the GNU-G77 unix and windows compilers. Adjust the significant statements in
! each subroutine to your operating system and compiler.
!
!*****************************************************************************

        subroutine GetEnvVar(tpath, itpath, ierr, dir, ext, os)
!
!       This soubroutine asks the operating system if the environment
!       variable THERMDOM is set. This variable defines the absolute path
!       of the directory where the program executables and their associated
!       files reside. If the variable is set, then it reads the absolute path
!       into the character string "tpath". The subroutine assumes that the
!       absolute path contains already the appropriate directory delimiter "dir" that,
!       depending on your operating system might be / (e.g. for Unix) or \
!       (e.g. for DOS/Win). By default, dir="/" in thblock.cmn.
!       It also checks if this absolute path ends with this directory delimiter.
!       If the delimiter is not set at the end of the absolute path, this sub-
!       routine sets it. After this, the subroutine finds out the position
!       "itpath" of the last significant (e.g. non-blank) character (that in any case
!       would be the directory delimiter!) in the string "tpath".
!       The integer "ierr" takes the value 0 if THERDOM is set or 1
!       in case it is not.
!
        implicit NONE
        character *(*) tpath, dir, ext, os
        integer itpath, ierr
!
        itpath=0
        ierr=0
!
! G77 version
!
        dir='\\'
        ext='.bat'
        os='Windows, gfortran'
!
        call GetEnv('THERDOM',tpath)
!
        if(tpath.eq.' ') then
           ierr=1
           return
        end if
        call LASTCHAR(tpath, itpath)
        if(tpath(itpath:itpath).ne.dir) then
           itpath=itpath+1
           tpath(itpath:itpath)=dir
        end if
        return
        end
!
!*********************************************************************
!
        subroutine getwork (wpath, iwpath, ierr, dir)
!
!       This soubroutine finds out the absolute path of the working
!       directory. It checks and does the same things as described in
!       in soubroutine GetEnvVar
!
        implicit none
        character *(*) wpath, dir
        integer iwpath, ierr
!
!%%%%   g77 version
!
       ierr=0
       call getcwd(wpath,ierr)
!
       iwpath=0
       call LASTCHAR(wpath, iwpath)
       if(wpath(iwpath:iwpath).ne.dir) then
            iwpath=iwpath+1
            wpath(iwpath:iwpath)=dir
       end if
!        write(*,*) wpath
        return
        end
!
!************************************************************************************
!
        subroutine ClearScreen
!
!       This soubroutine clears the screen. If you don't need clearing the
!       screen comment out the apprpriate statements except "subroutine ...",
!       "return" and "end".
!
!       Sort description of statements
!       ------------------------------
!       The system call invokes an operating system call. The argument
!       (character-string) is a valid operating system statement.
!
!$$$$    Windows/DOS version
!
        call system ('cls')
!
!$$$$   Unix version
!
!        call system ('clear')
!
        return
        end
!
!****************************************************************
!
        subroutine getdate (sdate)
!
!       This subroutine gets the current date and time and writes both
!       into the character string "sdate".
!
        implicit NONE
!
!%%%%   The G77 version fetches the current date and time using integer arrays
!       as arguments. Depending on your operating system locale,
!       this might be DD MM JJJJ or MM DD JJJJ (DD=day between 01 and 31; MM=
!       month between 01 and 12; JJJJ= four digit year number) and
!       hh mm  ss (hh= hour between 0 and 24; mm= minute between 0 and 60; ss=
!       seconds between 0 and 60). The main point in this subroutine is
!       that the date and time arguments should be writen into character string
!       "sdate" and passed to the calling program units. Format of date and time
!       within sdate may be as you like.
!

        character *(*) sdate
!
!$$$$$$ g77 compiler
!
        integer time(3), date(3)
        call idate (date)
        call itime(time)
        write(sdate,1) date, time
1       format(2(i2.2,'/'),i4.4,1x,2(i2.2,':'),i2.2)
!
        return
        end
!
!*************************************************************************
!
      subroutine cputime(ch)
      implicit none
      character *(*) ch
      real hour, minute, sec, rest, runtime
!
      call Second(runtime)
      hour=int(runtime/3600.)
      runtime=mod(runtime,3600.0)
      minute=int(runtime/60)
      sec=mod(runtime,60.0)
      rest=(sec-int(sec))*1000
      write(ch,806) int(hour), int(minute), int(sec),int(rest)
806   format('CPU-Time (hhh:mm:sec): ',i3.3,':',i2.2,':',i2.2, &
      '.',i3.3)
!
!      write(*,806)  int(hour), int(minute), int(sec),int(rest)
!      pause
!
      return
      end

!*********************************************************************
!
        subroutine GetLineArgs (i,largum,ierr)
!
!*****  It is very convinient, if programs GUZZLER and EXPLOT could be run
!       with command line arguments corresponding exactly to the input
!       needed by them interactively. This subroutine makes use of the
!       G77 intinsic function that checks the command line for arguments.
!
!       Variables description:
!       i is the number of argument following the program call that is
!       argument number 0.
!       largum is the argument at position i
!       ierr controls the possibility that this subroutine becomes actually
!       inactive and returns with no actions
!
!       if your compiler does not offer a similar function call like the one
!       given below for G77 you should, set the ierr variable to a non zero value.
!       If your compiler offers such a possibility, that sound differently,
!       commend out the the G77 block below and edit your own appropriately.
!
        implicit none
        integer i, ierr
        character*(*) largum
!
        ierr=0
        if(ierr.ne.0) return
!
!%%%%   g77 version
!
        call GetArg (i,largum)
!
        return
        end
!
!*********************************************************************
!
      SUBROUTINE MAKEEXEC(A001)
      CHARACTER*(*) A001
!-----
      RETURN
      END
!
!*********************************************************************
!
      SUBROUTINE RMCOMMAND(A001)
      CHARACTER*(*) A001
      A001='del'

!-----
      RETURN
      END
!
!*********************************************************************
!
      SUBROUTINE EXEXT(A001)
      CHARACTER*(*) A001
      A001='.bat'
!-----
      RETURN
      END
!
!*********************************************************************
!
      SUBROUTINE CALLABAT(A001)
      CHARACTER*(*) A001
      A001='CALL'
!-----
      RETURN
      END
!
!*********************************************************************
!
      SUBROUTINE MAKEFOLDER(A001)
      CHARACTER*(*) A001
      CHARACTER*(132) A002
      INTEGER*4 I
      CALL lastchar(A001,I)
      IF (I.EQ.0) RETURN
      A002='mkdir '//A001
      CALL lastchar(A002,I)
      CALL SYSTEM (A002(1:I))
!-----
      RETURN
      END
!
!*********************************************************************
!
      SUBROUTINE REMOFOLDER(A001)
      CHARACTER*(*) A001
      CHARACTER*(132) A002
      INTEGER*4 I
      CALL lastchar(A001,I)
      IF (I.EQ.0) RETURN
      A002='rmdir /S /Q '//A001
      CALL lastchar(A002,I)
      CALL SYSTEM (A002(1:I))
!-----
      RETURN
      END
!
!*********************************************************************
!
      SUBROUTINE DIRLIST
      CALL SYSTEM ('dir /A:D /B > dirlist.txt')
      OPEN(UNIT=99, FILE='dirlist.txt')
!-----
      RETURN
      END
!
!*********************************************************************
!
      SUBROUTINE FILEDELETE
      CLOSE (UNIT=99)
      CALL SYSTEM ('del dirlist.txt')
!-----
      RETURN
      END

