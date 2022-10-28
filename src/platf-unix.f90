!-----Version: 28.05.2022   unix
! This file contains a collection of subroutines that depend on the operating
! system and the compiler used.
!
!***************************************************************************
!
      SUBROUTINE clearscreen
!---- operating system call to clear the screen.
!      CALL SYSTEM ('clear')
      RETURN
      END
!
!****************************************************************
!
      SUBROUTINE getdate (sdate)
!---- gets the current date and time and writes both
!---- into the character string "sdate".
      IMPLICIT NONE
      CHARACTER *(*) sdate
      CHARACTER*8 DATUM
      CHARACTER*10 ZEIT
      CALL Date_and_Time(DATUM,ZEIT)
      WRITE (UNIT=sdate,FMT=200) DATUM(7:8),DATUM(5:6),DATUM(1:4), &
      ZEIT(1:2),ZEIT(3:4),ZEIT(5:6)
  200 FORMAT (A2,'.',A2,'.',A4,' - ',A2,':',A2,':',A2)
!-----
      RETURN
      END
!
!****************************************************************
!
      SUBROUTINE CPUTIME(CH)
!---- gets the used CPU-time and writes it into the character string "sdate".
      IMPLICIT NONE
      CHARACTER*(*) CH
      INTEGER*4 EHRS,EMIN,ESEC
      REAL*4 ETOTAL,EVALUE,ETIME
      REAL*4 EARRAY(2)
!-----
      EVALUE=ETIME(EARRAY)
!      ETOTAL=EARRAY(2)
      ETOTAL=EVALUE
      EHRS=DINT(ETOTAL/3600.0D0)
      ETOTAL=ETOTAL-3600.0D0*EHRS
      EMIN=INT(ETOTAL/60.0D0)
      ETOTAL=ETOTAL-60.0D0*EMIN
      ESEC=INT(ETOTAL)
!-----
      IF (ETOTAL.GT.10.0) THEN
      WRITE(UNIT=CH,FMT=1010) EHRS,EMIN,ETOTAL
 1010 FORMAT ('CPU time: ',I4,'h ',I2.2,'m ',F5.2,'s')
      ELSE
      WRITE(UNIT=CH,FMT=1012) EHRS,EMIN,ETOTAL
 1012 FORMAT ('CPU time: ',I4, 'h ',I2.2,'m 0',F4.2,'s')
      END IF
!-----
      RETURN
      END
!
!***********************************************************
!
      SUBROUTINE GetEnvVar(mpath,impath,ierr, dir, ext, os)
!---- This soubroutine determines the program's directory.
!---- In the case of doubble clicking the icon, the 0th argument of
!---- GETARG is: path+programname. From this the path is extracted.
!---- If the program is run from a shell, there is no path attached to
!---- programname, and the environment variable THERDOM is used as path.
!---- dir: (CHARACTER*1) directory delimiter character (UNIX: '/')
!---- ext: (CHARACTER*10) extension of domjob-file
!---- os: (CHARACTER*20) Text that identifies the operating system
!
      IMPLICIT NONE
      CHARACTER *(*) mpath, dir, ext, os
      CHARACTER*120 CH,wdir
      INTEGER*4 I1,I
      INTEGER impath, ierr, ICH, iwdir
      LOGICAL*4 IPR
      IPR=.TRUE.
      IF (ierr.EQ.9) IPR=.FALSE.
!-----
      dir='/'
      ext=' '
      os='UNIX'
      CH=' '
      mpath=' '
      impath=0
      ierr=0
!-----try argument no.0 beginning with "/"
      CALL getarg(0,CH)
      CALL LABLA(CH,ICH)
      IF (CH(1:1).EQ.dir) THEN
      IF (IPR) WRITE (*,1002) CH(1:ICH)
 1002 FORMAT (/,' Program (getarg 0):  ',A)
      GOTO 888
      END IF
!-----try THERDOM
      CALL GetEnv('THERDOM',CH)
      CALL LABLA(CH,ICH)
      IF (ICH.NE.0) THEN
      IF (CH(ICH:ICH).NE.dir) THEN
      ICH=ICH+1
      CH(ICH:ICH)=dir
      END IF
      IF (IPR) WRITE (*,1004) CH(1:ICH)
 1004 FORMAT (/,' Program (THERDOM):   ',A)
      I1=ICH
      GOTO 999
      END IF
!-----try cwd
      CALL getcwd(CH)
      CALL LABLA(CH,ICH)
      IF (ICH.NE.0) THEN
      IF (CH(ICH:ICH).NE.dir) THEN
      ICH=ICH+1
      CH(ICH:ICH)=dir
      END IF
      IF (IPR) WRITE (*,1006) CH(1:ICH)
 1006 FORMAT (/,' Program (getcwd):    ',A)
      I1=ICH
      GOTO 999
      END IF
!=====
  888 I1=0
      DO 500,I=ICH,1,-1
      IF (CH(I:I).EQ.dir) THEN
      I1=I
      GOTO 501
      END IF
  500 CONTINUE
  501 CONTINUE
!=====
  999 IF (I1.EQ.0) I1=1
      mpath=CH(1:I1)
      impath=I1
      IF (IPR) WRITE (*,2000) mpath(1:impath)
 2000 FORMAT (/,' path for program:    ',A)
!-----
      RETURN
      END
!
!*********************************************************************
!
      SUBROUTINE getwork (wdir,iwdir, ierr, dir)
!---- finds the path of the working directory.
!---- dir: (CHARACTER*1) directory delimiter character (UNIX: '/')
      IMPLICIT NONE
      CHARACTER*(*) wdir, dir
      INTEGER ierr, iwdir, getcwd
      LOGICAL*4 IPR
      IPR=.TRUE.
      IF (ierr.EQ.9) IPR=.FALSE.
!-----
      wdir=' '
      ierr=0
      ierr=getcwd(wdir)
      CALL LABLA(wdir,iwdir)
      IF (wdir(iwdir:iwdir).NE.dir) THEN
      iwdir=iwdir+1
      wdir(iwdir:iwdir)=dir
      END IF
      IF (IPR) WRITE (*,1000) wdir(1:iwdir)
 1000 FORMAT (/,' Working directory: ',A)
!-----
      RETURN
      END
!
!*********************************************************************
!
      SUBROUTINE GetLineArgs (i,largum,ierr)
!---- gets the ith argument from the command line
!---- (arg 0 = program name (includes path if program invoked by double clicking))
      IMPLICIT NONE
      INTEGER i, ierr
      CHARACTER*(*) largum
      CALL GetArg (i,largum)
!-----
      RETURN
      END
!
!*********************************************************************
!
      SUBROUTINE MAKEEXEC(A001)
      CHARACTER*(*) A001
      CHARACTER*(132) A002
      INTEGER*4 I
      CALL LABLA(A001,I)
      IF (I.EQ.0) RETURN
      A002='chmod a+x '//A001
      CALL LABLA(A002,I)
      CALL SYSTEM (A002(1:I))
!-----
      RETURN
      END
!
!*********************************************************************
!
      SUBROUTINE RMCOMMAND(A001)
      CHARACTER*(*) A001
      A001='rm'
!-----
      RETURN
      END
!
!*********************************************************************
!
      SUBROUTINE EXEXT(A001)
      CHARACTER*(*) A001
      A001=' '
!-----
      RETURN
      END
!
!*********************************************************************
!
      SUBROUTINE CALLABAT(A001)
      CHARACTER*(*) A001
      A001=' '
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
      CALL LABLA(A001,I)
      IF (I.EQ.0) RETURN
      A002='mkdir '//A001
      CALL LABLA(A002,I)
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
      CALL LABLA(A001,I)
      IF (I.EQ.0) RETURN
      A002='rm -r '//A001
      CALL LABLA(A002,I)
      CALL SYSTEM (A002(1:I))
!-----
      RETURN
      END
!
!*********************************************************************
!
      SUBROUTINE DIRLIST
      CALL SYSTEM ('ls -F > dirlist')
      OPEN(UNIT=99, FILE='dirlist')
!-----
      RETURN
      END
!
!*********************************************************************
!
      SUBROUTINE FILEDELETE
      CLOSE (UNIT=99)
      CALL SYSTEM ('rm -r dirlist')
!-----
      RETURN
      END
