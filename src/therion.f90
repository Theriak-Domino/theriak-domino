!therion.f90  Version: 2023.06.11
! -----------------------------------------------------------------------
! Copyright (C) 1984-2022  Christian de Capitani
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
!               ***********
!               * THERION *
!               ***********
!
!     Program written by Christian de Capitani
!     based on Theriak
!
!     Any suggestions, complaints or comments are greatly appreciated
!     by the author and should be sent to:
!          Christian de CapitaniX
!          Mineralogisch-Petrographisches Institut
!          Universitaet Basel
!          Bernoullistrasse 30
!          CH-4056 BASEL
!
!
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
      include 'checkdb.cmn'
!*****
!
!-----END OF COMMON VARIABLES
      INTEGER(4) I001,I002,I,I1,COMAY,ierr,j, &
      LARG
      REAL(8) FF
      CHARACTER(16) CH16
      CHARACTER(500) CH001,CH002,SYREC,CHIN(3),ZEITSTRING
!      CHARACTER(120) DIRNAME
!-    DIRNAME is local (modified in Subroutines)
!-    The global variable for the directory is NEWDIR
!*****
      progname='THERION'
      vers=BUILDVERSION
      task='"check consistency of databases"'
      ierr=0
      call initialize('$THERION-FILES',ierr)
      if(ierr.ne.0) CALL EXIT
!
!-----
      LARG=0
      IERR=0
      DO I=1,5
      CALL GetLineArgs (I,LARGUM(I),IERR)
      IF(IERR.NE.0.OR.LARGUM(I).EQ.' ')THEN
      GOTO 399
      ELSE
      LARG=LARG+1
      END IF
      END DO
  399 CONTINUE
      IFNR=5
      IF (LARG.GT.0) THEN
      INFILE=LARGUM(1)
      CALL LABLA(INFILE,I1)
      IFNR=39
      OPEN (UNIT=IFNR,FILE=INFILE(1:I1),STATUS='UNKNOWN')
      END IF
      IF (LARG.GT.1) THEN
      filename(dat)=LARGUM(2)
      CALL LABLA(filename(dat),fnl(dat))
      END IF
!-----
!      WRITE (UNIT=6,FMT='(''larg: '',i2)') larg
!      WRITE (UNIT=6,FMT='(''arg: '',i2,1x,a)') &
!      ((I,largum(I)),I=1,larg)
!      WRITE (UNIT=6,FMT='(''filename: '',a)') filename(dat) 
!-----
!-----
!
!*****
      CHIN(1)=' '
      CHIN(2)=' '
      CHIN(3)=' '
      KEYW='loin'
      NKEYWS=0
      NEWDIR=' '
      LDIR=0
!------------------
!     Open UNIT=log
!------------------
      j=log
      line=filename(j)(1:fnl(j))//ext(j)
      path=wpath
      akzess=' '
      state=' '
      call openfile(j,ierr)
      if(ierr.ne.0) CALL EXIT
!-----
      DO I=1,3
        READ (UNIT=log,FMT='(A500)',END=411) CHIN(I)
      END DO
  411 CONTINUE
      WRITE (6,100)
  100 FORMAT (/ &
      ' -------------------'/ &
      ' database definition'/ &
      ' -------------------')
      CALL LABLA(CHIN(1),I002)
      IF (I002.EQ.0) I002=1
      CH002=' Enter [ "?" | CR | "files" | database filename ] <'// &
      CHIN(1)(1:I002)//'>?'
!-----
  412 CONTINUE
      CALL PUST (6,CH002)
      READ (IFNR,FMT='(A500)') CH001
      IF (CH001.EQ.'?') THEN
       CALL helpme('$THK-START')
       GOTO 412
      END IF
      IF (VERGL(CH001,'files')) THEN
       CALL listfiles
       GOTO 412
      END IF
      IF (CH001.EQ.' ') THEN
       CH001=CHIN(1)
       I001=I002
      ELSE
       CHIN(1)=CH001
      END IF
      CH002=CH001
      CALL TAXI(CH002,DBNAME)
      CALL LABLA(DBNAME,I001)
!===== for therion
      filename(dbs)=DBNAME
!------------------
!     open UNIT=dbs
!------------------
      j=dbs
      line=DBNAME
      path=wpath
      akzess=' '
      state='old'
      call openfile(j,ierr)
      if(ierr.ne.0) CALL EXIT
!------------------
!     open UNIT=dat
!------------------
      j=dat
      line=filename(j)(1:fnl(j))//ext(j)
      path=wpath
      akzess=' '
      state='old'
      call openfile(j,ierr)
      if(ierr.ne.0) CALL EXIT
!-----
      WRITE (scr,101) DBNAME(1:I001)
  101 FORMAT (/' database for this run: ',A/)
!*****
      COMAY=COMAX
      CALL PROREAD(SYREC)
!-- pick nach proread! da dort NPICK=0
!   do not do this in therion
!      NPICK=0
!      DO I=1,COMAX
!       CALL TAXI(CH002,CH16)
!       IF (CH16.NE.' ') THEN
!       NPICK=NPICK+1
!       PICK(NPICK)=CH16
!       ELSE
!       GOTO 5
!       END IF
!      END DO
!    5 CONTINUE
!--
!=====
!     after PROREAD the dat file (THERIN) can be closed in therion
      CLOSE (UNIT=dat)
!=====
      Call LABLA(line,I1)
      WRITE (scr,102) line(1:I1)
  102 FORMAT (/,' Input from file',1x,a)
      write(scr,104) ('-',I=1,I1+16)
  104 format(1x,130a1,:)
      write(scr,106) TC,P
  106 format(' T =',F8.2,' C     P =',F9.2,' Bar')
      CALL PUST (scr,' '//SYREC)
!*****
!-----check for consistency (type of calculation = cdc)
!!      READ (UNIT=IFNR,FMT='(A500)') CH001

!*****
    6 WRITE (scr,115)
  115 FORMAT (/ &
      ' -----------------'/ &
      ' check consistency'/ &
      ' -----------------')
      CALL LABLA(CHIN(2),I002)
      IF (I002.EQ.0) I002=1
      CH002=' Enter [ "?" | CR | experimental database ] <'//CHIN(2)(1:I002)//'>?'
      CALL PUST (scr,CH002)
      CH001=' '
      READ (IFNR,FMT='(A500)') CH001
!---
      IF (CH001.EQ.'?') THEN
      CALL helpme('$THK-KONS')
      GOTO 6
      END IF
!---
      CALL LABLA(CH001,I001)
      IF (I001.EQ.0) THEN
      CH001=CHIN(2)
      I001=I002
      ELSE
      CHIN(2)=CH001
      END IF
!----for therion
      CALL LABLA(CH001,I001)
      filename(drv)=CH001(1:I001)
      DRIVENAME=CH001(1:I001)
      CALL PUST(6,' drivename: '//CH001)
!------------------
!     open UNIT=drv
!------------------
      j=drv
      line=CH001
      path=wpath
      akzess=' '
      state='old'
      call openfile(j,ierr)
      if(ierr.ne.0) CALL EXIT

!*****
!*****
    7 WRITE (scr,118)
  118 FORMAT (/ &
      ' ------------------'/ &
      ' choose experiments'/ &
      ' ------------------')
      CALL LABLA(CHIN(3),I002)
      IF (I002.EQ.0) I002=1
      CH002=' Enter [ "?" | CR | all | sys | pha | exp ] <' &
      //CHIN(3)(1:I002)//'>?'
      CALL PUST (scr,CH002)
      CH001=' '
      READ (IFNR,FMT='(A500)') CH001
!---
      IF (CH001.EQ.'?') THEN
      CALL helpme('$THK-KONS')
      GOTO 7
      END IF
!---
      CALL LABLA(CH001,I001)
      IF (I001.EQ.0) THEN
      CH001=CHIN(3)
      I001=I002
      ELSE
      CHIN(3)=CH001
      END IF
!----for therion
      CALL UPLOW2(CH001)
      CALL TAXI(CH001,KEYW)
      CALL LABLA(KEYW,I1)
      IF (KEYW.EQ.' ') KEYW='all'
      IF (KEYW.NE.'all'.AND.KEYW.NE.'exp'.AND.KEYW.NE.'pha') goto 7
!=====
      IF (.NOT.VERGL(KEYW,'all')) THEN
        CALL PUST(6,' choose: '//CH001)
        NKEYWS=0
   31   CALL TAXI(CH001,CH16)
        IF (CH16.NE.' ') THEN
          NKEYWS=NKEYWS+1
          KEYWS(NKEYWS)=CH16
          GOTO 31
        END IF
      END IF
!=====
      IF (KEYW.EQ.'all') THEN
!!      DIRNAME='all'
      NKEYWS=0
      END IF

      IF (KEYW.EQ.'exp') THEN
      IF (NKEYWS.EQ.0) GOTO 7
      NKEYWS=1
!!      WRITE (UNIT=DIRNAME,FMT=1200) KEYW(1:I1),(KEYWS(J),J=1,NKEYWS)
!! 1200 FORMAT (A,'_',20A)
      END IF

      IF (KEYW.EQ.'pha') THEN
      IF (NKEYWS.EQ.0) GOTO 7
!!      WRITE (UNIT=DIRNAME,FMT=1202) KEYW(1:I1),(KEYWS(J),J=1,NKEYWS)
!! 1202 FORMAT (A,'_',20A3)
      END IF







!      CALL COLLAPS(DIRNAME,IL)
!      CALL PREMAKEF(DIRNAME,ILMAX,IL)
!      WRITE (UNIT=6,FMT='(''MAKE A FOLDER'',A)') DIRNAME
!      CALL MAKEFOLDER(DIRNAME)
!      NEWDIR=DIRNAME(1:IL)
!      LDIR=IL

!        OPEN (UNIT=72,FILE=NEWDIR(1:LDIR)//'00ini',STATUS='UNKNOWN')
!        WRITE (UNIT=72,FMT='(''directory: '',A)') DIRNAME(1:LDIR)
!
!        IF (KEYW.EQ.'pha') WRITE (UNIT=72,FMT='(''Phases:'')')
!        IF (KEYW.EQ.'exp') WRITE (UNIT=72,FMT='(''Experiment:'')')
!        DO I=1,NKEYWS
!         CALL LABLA(KEYWS(I),I1)
!         WRITE (UNIT=72,FMT='(I3,2X,A)') I,KEYWS(I)(1:I1)
!        END DO
!        CLOSE (UNIT=72)







!------------------
!-----store terminal input
      CLOSE (UNIT=log)
!------------------
!     Open UNIT=log
!------------------
      j=log
      line=filename(j)(1:fnl(j))//ext(j)
      path=wpath
      akzess=' '
      state='old '
      call openfile(j,ierr)
      if(ierr.ne.0) CALL EXIT
!-----
      DO I=1,3
        CALL PUST(log,CHIN(I))
      END DO
      CLOSE (UNIT=log)
!*****
!----- here jump to KONSI

      CALL KONSI
!-----------------------------------------------------------------
!-----AT THIS POINT THE PROGRAM MAY BE CHANGED TO PERFORM REPEATED
!-----CALCULATIONS WITH VARYING T,P OR BULK COMPOSITION
!-----------------------------------------------------------------
!-----CALCULATE THE EQUILIBRIUM ASSEMBLAGE:
!*****
!      CALL CALSTR
!      CALL PRININ       (if you want anything printed before calculation)
!      CALL THERIA
!*****
!-----------------------------------------------------------------
!-----CHANGE T OR P
!*****
!     TC=.....              (Temperature in deg. C)
!     P=.....               (Pressure in Bars)
!     CALL NURVONPT
!*****
!-----------------------------------------------------------------
!-----CHANGE THE BULK COMPOSITION:
!-----(THIS IS SLIGHTLY MORE COMPLICATED. THE FOLLOWING IS AN EXAMPLE.)
!*****
!     READ (UNIT=5,FMT='(A170)') FORMUL
!     CALL CHEMIE(COMAY,NC,OXYDE,OXANZ,FORMUL,CHE)
!     MORE=.FALSE.
!     DO 601,I=1,NC
!     IF (CHE(I).EQ.0.0D0.NEQV.CHEM(I).EQ.0.0D0) MORE=.TRUE.
!     CHEM(I)=CHE(I)
! 601 CONTINUE
!     IF (MORE) THEN
!     CALL DBREAD
!     CALL NURVONPT
!     CALL GIBBSTEST(...,...) 
!     ELSE
!     DO 602,I=1,NUN
! 602 BULK(I)=CHE(CHMCOD(I))
!     END IF
!*****
!-----------------------------------------------------------------
!-----CONTROL THE AMOUNT OF OUTPUT PRODUCED:
!-----IF PRTLOG(n) IS SET .FALSE. THEN THE CORRESPONDING OUTPUT
!-----IS OMITTED.
!*****
!*****BEFORE THE CALCULATION:
!*****
!     PRTLOG(1)=.TRUE.     (stop after reading database)
!     PRTLOG(2)=.TRUE.     (Print bulk composition)
!     PRTLOG(3)=.TRUE.     (Print list of considered phases)
!     PRTLOG(4)=.TRUE.     (Print a summary of the solution models)
!     PRTLOG(5)=.TRUE.     (Print the parameters)
!*****
!*****AFTER THE CALCULATION:
!*****
!     PRTLOG(6)=.TRUE.OR.PRTLOG(7)=.TRUE.OR.PRTLOG(8)=.TRUE.
!                          (Print stable assemblage)
!     PRTLOG(6)=.TRUE.     (Print volumes and densities)
!     PRTLOG(7)=.TRUE.     (Print compositions of all stable phases)
!     PRTLOG(8)=.TRUE.     (Print activities of all considered phases)
!     PRTLOG(9)=.TRUE.     (used to print table in loop)
!     PRTLOG(10)=.TRUE.    (used to print image for pixelmaps)
!     PRTLOG(11)=.TRUE.    (used to print short table (e.g. theriaq))
!*****
!-----------------------------------------------------------------
!     CALL NURVONPT
!     CALL CALSTR
!     IF (PRTLOG(2).OR.PRTLOG(3).OR.PRTLOG(4)) CALL PRININ
!     CALL THERIA
      CALL CPUTIME(ZEITSTRING)
      CALL CPU_TIME(FF)
      CALL LABLA(ZEITSTRING,I001)
      WRITE (scr,150) ZEITSTRING(1:I001)
  150 FORMAT (/,' exit THERIAK',/,1X,A)
      END
!-----
!*************************************************************
!*************************************************************
      SUBROUTINE KONSI
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
      include 'checkdb.cmn'
!
!-----END OF COMMON VARIABLES
      LOGICAL(4) L001,L002,L003,L004,L005
      CHARACTER(1) TABTAB,CH1
      CHARACTER(32) TEXT,CH16
      CHARACTER(80) CH80,FOLDERLIST(1000)
      CHARACTER(16) KEYWORD,TABPNR,BATEX
      CHARACTER(100) TABREA,TABAUT,TABBUL,TABPHA,HTXT1,HTXT2,HTXT3
      CHARACTER(250) CH001,CH002,BININ,TEXTQ
      CHARACTER(500) CH500
      INTEGER(4) COMAY,I,J,I1,I2,I001,I002,IC,NFEHL,FICOM,NCOMPS, &
      EXONE,I3,I4,I5,PLNR,ISU,IO,NFOLD
      REAL(8) FF,FF2,YPOS, &
      ZERO,F1,F2,F3,F4,PLCONST,MODE(3),CHEM2(COMAX),CHEM3(3,COMAX)
!-----
!---- UNIT=1-25 reserviert
!---- UNIT=39 (achtung ist fuer IFNR line argument reserviert)
!---- UNIT=40 (achtung ist fuer ini_.. schon verwendet)
!----
!---- UNIT=33
!             trans_xxx | translation file for database
!             (makeplot not called by program anymore, create file "makeplot" and call by command line)
!---- UNIT=30
!             reac_database_TYP_0exp | experiments for explot, with scale, fill: none and gray
!---- UNIT=32
!             reac_database_TYP_exn | experiments for explot, without scale, fill: none and gray
!---- UNIT=34
!             reac_database_TYP_exn2 | experiments2 for explot, without scale, inconsistent data: black
!---- UNIT=31
!             reac_database_TYP_txt | domino script file
!             reac_database_job | job-file (calls domino)
!---- UNIT=35
!             check_database_file (detailed Output)
!---- UNIT=36
!             checksummary_database (short Output) (not used)
!---- UNIT=37
!             reatable_database | list of all reactions (may depend on database) (not used)
!---- UNIT=38
!             checkshort_database | VERY short summary (not used)
!---- UNIT=39
!             checkquiteshort_database | quite short summary
!---- UNIT=51
!             checkphases_database | lists phases not found in database
!---- UNIT=55
!             check_missing-phases | summary of all phases not found in database
!----

!     make a list of all folders beginning with "_"
      NFOLD=0
      CALL DIRLIST
      OPEN (UNIT=52,FILE='dirlist',STATUS='UNKNOWN')
      DO I=1,1000
        READ(UNIT=52,FMT='(A)',END=777) CH001
        CALL TAXI(CH001,CH80)
        IF (CH80(1:1).EQ.'_') THEN
          NFOLD=NFOLD+1
          FOLDERLIST(NFOLD)=CH80
        END IF
      END DO
  777 CONTINUE
      WRITE (UNIT=6,FMT='(/,'' the following experiment folders exist:'',I4)') NFOLD
      DO I=1,NFOLD
        CALL PUST(6,'  '//FOLDERLIST(I))
      END DO
      

      INCSTR='------------------- '
      BLAN=' '
      NELEDB=NC
      DO I=1,NC
        CHEM(I)=1.0D0
        ELEDB(I)=OXYDE(I)
!        WRITE (UNIT=6,FMT='(''EL '',A)') OXYDE(I)
      END DO
      USE='*'
      LUSE=1
      CALL DBREAD
      NPHADB=NPHA+NSOL
      DO I=1,NPHA
        PHADB(I)=NAME(I)
!        WRITE (UNIT=6,FMT='(''PH '',A)') NAME(I)
      END DO
        DO I=1,NSOL
        PHADB(NPHA+I)=SOLNAM(I)
!        WRITE (UNIT=6,FMT='(''SO '',A)') SOLNAM(I)
      END DO
!----
! does some additional printing
      PRTEST=.TRUE.
      PTSYMB=1
!
      TANG=0.0D0
      FICOM=0
!----
!----
      DO I=1,19
        FDELETE(I)=.FALSE.
      END DO
      OPEN (UNIT=72,FILE='keepdelete',STATUS='UNKNOWN')
       DO I=1,100
         READ (UNIT=72,FMT='(A)',END=111) CH001
         CALL TAXI(CH001,CH16)
         CALL GELI(CH001,FF)
         I001=IDINT(FF)
         IF (CH16.EQ.'_exn'.AND.I001.EQ.0) FDELETE(1)=.TRUE.
         IF (CH16.EQ.'_exn2'.AND.I001.EQ.0) FDELETE(2)=.TRUE.
         IF (CH16.EQ.'_0exp'.AND.I001.EQ.0) FDELETE(3)=.TRUE.
         IF (CH16.EQ.'_0exp.ps'.AND.I001.EQ.0) FDELETE(4)=.TRUE.
         IF (CH16.EQ.'_0exp.svg'.AND.I001.EQ.0) FDELETE(5)=.TRUE.
         IF (CH16.EQ.'_com'.AND.I001.EQ.0) FDELETE(6)=.TRUE.
         IF (CH16.EQ.'_com.ps'.AND.I001.EQ.0) FDELETE(7)=.TRUE.
         IF (CH16.EQ.'_com.svg'.AND.I001.EQ.0) FDELETE(8)=.TRUE.
         IF (CH16.EQ.'_com2'.AND.I001.EQ.0) FDELETE(9)=.TRUE.
         IF (CH16.EQ.'_com2.ps'.AND.I001.EQ.0) FDELETE(10)=.TRUE.
         IF (CH16.EQ.'_com2.svg'.AND.I001.EQ.0) FDELETE(11)=.TRUE.
         IF (CH16.EQ.'_job'.AND.I001.EQ.0) FDELETE(12)=.TRUE.
         IF (CH16.EQ.'.txt'.AND.I001.EQ.0) FDELETE(13)=.TRUE.
         IF (CH16.EQ.'.plt'.AND.I001.EQ.0) FDELETE(14)=.TRUE.
         IF (CH16.EQ.'.rxn'.AND.I001.EQ.0) FDELETE(15)=.TRUE.
         IF (CH16.EQ.'.cln'.AND.I001.EQ.0) FDELETE(16)=.TRUE.
         IF (CH16.EQ.'_bin_loop'.AND.I001.EQ.0) FDELETE(17)=.TRUE.
         IF (CH16.EQ.'_fun_loop'.AND.I001.EQ.0) FDELETE(18)=.TRUE.
         IF (CH16.EQ.'_rea_loop'.AND.I001.EQ.0) FDELETE(19)=.TRUE.
       END DO
  111 CONTINUE
      CLOSE (UNIT=72)


!      DO I=1,19
!        WRITE (UNIT=6,FMT='(''FDELETE'',I3,2X,L4)') I,FDELETE(I)
!      END DO

! make a list of colors

      NCOLORS=6
      DO I=1,20
        LCOLOR(I)=' '
        FCOLOR(I)=' '
      END DO
      FGRAU='LCOLOR  0.5  0.5  0.5'
      LCOLOR(1)='LCOLOR   0   0   0'
      FCOLOR(1)='FCOLOR   0   0   0'
      LCOLOR(2)='LCOLOR   1   0   0'
      FCOLOR(2)='FCOLOR   1   0   0'
      LCOLOR(3)='LCOLOR   0   1   0'
      FCOLOR(3)='FCOLOR   0   1   0'
      LCOLOR(3)='LCOLOR   0   0   1'
      FCOLOR(3)='FCOLOR   0   0   1'
      LCOLOR(4)='LCOLOR   0   1   1'
      FCOLOR(4)='FCOLOR   0   1   1'
      LCOLOR(5)='LCOLOR   1   0.5   0'
      FCOLOR(5)='FCOLOR   1   0.5   0'
      LCOLOR(6)='LCOLOR   1   0   1'
      FCOLOR(6)='FCOLOR   1   0   1'
!----
      DO I=1,6
       CHLINE(I)=' '
      END DO
      XLINE=' '
      YLINE=' '
      WASLINE=' '
      LABLINE=' '
      PIXFNAME=' '
      TSIM=0.0D0
      PSIM=0.0D0
      ZERO=0.0D0
      CCODE=0
      MCODE=0
      NINC=0
      ISINC=0
      EXONE=0
      NCOMPS=0
      TABTAB=CHAR(9)
      NPLOTS=0
      PLOTTYP=' '
      ALLP=.FALSE.
      USEDFOR=.TRUE.
      PICKSTRING=' '
      COMAY=COMAX
      DO I=1,11
       PRTLOG(I)=.FALSE.
      END DO
      DRU=.FALSE.
      DO I=1,COMAX
       PASS(I)=' '
       MASS(I)=' '
       PMULT(I)=0
       MMULT(I)=0
      END DO
      NTRANS=0
      SHOW=0
      SHOWALL=.FALSE.
      IF (SHOWALL) SHOW=1
      TITLE='no TITLE'
      CALL CHECKNAME1(DBNAME,DBUSED)
      REAOK=.FALSE.
!--
      CALL EXEXT(BATEX)
      IF (BATEX.EQ.' ') THEN
        JONAME='_job'
      ELSE
        CALL LABLA(BATEX,I1)
        JONAME='_job'//BATEX(1:I1)
      END IF
!--
      CALL LABLA(DBUSED,I2)
      OPEN (UNIT=55,FILE='check_missing_phases_'//DBUSED(1:I2),STATUS='UNKNOWN')
!!-open reatable_database
!      CALL LABLA(DBNAME,I1)
!      OPFILE=NEWDIR(1:LDIR)//'reatable_'//DBNAME(1:I1)
!!      CALL PUST(6,'Open file 37: '//OPFILE)
!      CALL LABLA(OPFILE,IO)
!      OPEN (UNIT=37,FILE=OPFILE(1:IO),STATUS='UNKNOWN')
!      CALL PUST(37,'reatable')
!      CALL PUST(37,' ')
!!--
!!-open checkshort_database
!      CALL LABLA(DBNAME,I1)
!      OPFILE=NEWDIR(1:LDIR)//'checkshort_'//DBNAME(1:I1)
!!      CALL PUST(6,'Open file 38: '//OPFILE)
!      CALL LABLA(OPFILE,IO)
!      OPEN (UNIT=38,FILE=OPFILE(1:IO),STATUS='UNKNOWN')
!      CALL PUST(38,'short '//DBNAME(1:I1))
!      CALL PUST(38,' ')
!!--
!!-open checkquiteshort_database
!      CALL LABLA(DBNAME,I1)
!      OPFILE=NEWDIR(1:LDIR)//'checkquiteshort_'//DBNAME(1:I1)
!!      CALL PUST(6,'Open file 39: '//OPFILE)
!      CALL LABLA(OPFILE,IO)
!      OPEN (UNIT=39,FILE=OPFILE(1:IO),STATUS='UNKNOWN')
!      CALL PUST(39,'quiteshort '//DBNAME(1:I1))
!      CALL PUST(39,' ')
!!
!!-open check_database_reafile
!      CALL LABLA(filename(drv),I1)
!      CALL LABLA(DBUSED,I2)
!      CH001='check_'//DBUSED(1:I2)//'_'//filename(drv)(1:I1)
!      CALL LABLA(CH001,I1)
!      OPFILE=NEWDIR(1:LDIR)//CH001(1:I1)
!!      CALL PUST(6,'Open file 35: '//OPFILE)
!      CALL LABLA(OPFILE,IO)
!      OPEN (UNIT=35,FILE=OPFILE(1:IO),STATUS='UNKNOWN')
!      CALL PUST(35,CH001)
!      CALL PUST(35,' ')
!!
!!-- open checksummary_database_reafile
!      CH001='checksummary_'//DBUSED(1:I2)//'_'//filename(drv)(1:I1)
!      CALL LABLA(CH001,I1)
!      OPFILE=NEWDIR(1:LDIR)//CH001(1:I1)
!!      CALL PUST(6,'Open file 36: '//OPFILE)
!      CALL LABLA(OPFILE,IO)
!      OPEN (UNIT=36,FILE=OPFILE(1:IO),STATUS='UNKNOWN')
!      CALL PUST(36,CH001)
!      CALL PUST(36,' ')
!!
!!-- open checkphases_database_reafile
!      CH001='checkphases_'//DBUSED(1:I2)//'_'//filename(drv)(1:I1)
!      CALL LABLA(CH001,I1)
!      OPFILE=NEWDIR(1:LDIR)//CH001(1:I1)
!!      CALL PUST(6,'Open file 51: '//OPFILE)
!      CALL LABLA(OPFILE,IO)
!      OPEN (UNIT=51,FILE=OPFILE(1:IO),STATUS='UNKNOWN')
!      CALL PUST(51,CH001)
!      CALL PUST(51,' ')
!!
!
!      CALL PUST(35,' drivename: '//CH001)
!----------------------------------------------------------------
!--    read translation for database
!----------------------------------------------------------------
      CALL LABLA(DBNAME,I1)
      OPFILE='trans_'//DBNAME(1:I1)
!      CALL PUST(6,'Open file 33: '//OPFILE)
      CALL LABLA(OPFILE,IO)
!-- open trans_database
      OPEN (UNIT=33,FILE=OPFILE(1:IO),STATUS='OLD',ERR=15)
!----
      NTRANS=0
      DO I=1,1000
      READ (UNIT=33,FMT='(A)',END=14) CH001
      NTRANS=NTRANS+1
      CALL TAXI(CH001,TRANS(NTRANS,1))
      CALL TAXI(CH001,TRANS(NTRANS,2))
      END DO
   14 CLOSE (UNIT=33)
!----

!      CALL LABLA(filename(drv),I1)
!      CALL LABLA(DBUSED,I2)
!      CH001='translate_'//DBUSED(1:I2)//'_'//filename(drv)(1:I1)
!      CALL LABLA(CH001,I1)
!      OPFILE=CH001(1:I1)
!!      CALL PUST(6,'Open file 35: '//OPFILE)
!      CALL LABLA(OPFILE,IO)
!      OPEN (UNIT=35,FILE=OPFILE(1:IO),STATUS='UNKNOWN')
!      CALL PUST(35,CH001)
!      CALL PUST(35,' ')

!!      WRITE (UNIT=scr,FMT='('' '')')
!!      DO J=1,NTRANS
!!       WRITE (scr,fmt='('' translate: '',a,a)') TRANS(J,1),TRANS(J,2)
!!!       WRITE (35,fmt='('' translate: '',a,a)') TRANS(J,1),TRANS(J,2)
!!      END DO
      GOTO 16
   15 WRITE (scr,1000) 'trans_'//DBNAME(1:I1)
!      WRITE (35,1000) 'trans_'//DBNAME(1:I1)
 1000 FORMAT (' no translation file: ',A)
   16 CONTINUE

!      CLOSE (UNIT=35)

      IF (KEYW.EQ.'pha') THEN
      DO I=1,NKEYWS
        CALL TRANSL(KEYWS(I))
      END DO
!!      WRITE (UNIT=DIRNAME,FMT=1202) KEYW(1:I1),(KEYWS(J),J=1,NKEYWS)
!! 1202 FORMAT (A,'_',20A3)
      END IF

!----------------------------------------------------------------
!--    initialize some variables
!----------------------------------------------------------------
       INFO1=' '
       COMM1=' '
       COMM2=' '
       NICREAC=' '
       PRISUM=.TRUE.
!----------------------------------------------------------------
!--    search START and begin reading experiments UNIT=drv (not anymore)
!----------------------------------------------------------------
!    2 READ (UNIT=drv,FMT='(A)',END=998) CH001
!      IF (CH001(1:5).NE.'START') GOTO 2
!====
      ISU=1
      DO I=1,4
        ENO(I)=0
      END DO
      ENO(4)=1
      ENO(1)=1
!
!--   ENO(1)=1 if REAC (or ev synonym EXPTXT) not used
!     ENO(2)=1 if REAID
!     ENO(3)=1 if PHASES
!     ENO(4)=1 if SYSEL (elements defined in BULK or COMP0, COMPS etc) not used
!=====================================================================
!
    1 READ (UNIT=drv,FMT='(A)',END=999) CH001
!!      write (UNIT=6,FMT='(//,''NEW LINE  :'',A)') CH001(1:20)
!------------------------------------------
! as soon as all ENOs are 1, make directory
      I001=0
      DO I=1,4
        IF (ENO(I).GT.0) I001=I001+1
      END DO

      CALL LABLA(REAID,I1)

!      IF (I001.EQ.4) THEN
!      WRITE (UNIT=6,FMT='(''ENOS AT THE MOMENT: '',I4)') I001
!      WRITE (UNIT=6,FMT='(''NEWDIR AT THE MOMENT: '',A)') NEWDIR(1:LDIR)
!      WRITE (UNIT=6,FMT='(''REAID AT THE MOMENT: '',A)') REAID(1:I1)
!      END IF

      IF (I001.EQ.4) THEN
        WRITE (UNIT=6,FMT='(''     REAID '',a,''  new dir '',a)') REAID(1:I1),NEWDIR(1:LDIR)
      END IF


      IF ('_'//REAID(1:I1)//dir(1:1).NE.NEWDIR(1:LDIR)) THEN

!      check if directoy exists (is in FOLDERLIST)
        IF (I001.EQ.4) THEN
        CH80='_'//REAID
        CALL LABLA(CH80,I1)
          WRITE (UNIT=6,FMT='('' LOOKING FOR '',A)') CH80(1:I1)
          I002=0
          DO I=1,NFOLD
            IF (CH80(1:I1).EQ.FOLDERLIST(I)(1:I1)) I002=I
!            write (unit=6,fmt='('' searching '',A,i4)') CH80(1:I1),I002
          END DO
          IF (I002.GT.0) THEN
            WRITE (UNIT=6,FMT='('' folder already exists: '',A,A)') CH80(1:I1),FOLDERLIST(I002)(1:LDIR)
            NEWDIR=CH80(1:I1)//dir(1:1)
            LDIR=I1+1
!           call newreadir anyway
            CALL NEWREADIR
          ELSE
            WRITE (UNIT=6,FMT='('' folder must be made: '',A)') CH80(1:I1)
            NFOLD=NFOLD+1
            FOLDERLIST(NFOLD)=NEWDIR(1:LDIR)
            NEWDIR=CH80(1:I1)
            LDIR=I1
            CALL NEWREADIR
          END IF
        END IF

      ELSE
        WRITE (UNIT=6,FMT='(''     REAID is equal'',a,''  new dir '',a)') REAID(1:I1),NEWDIR(1:LDIR)
      END IF
!!      WRITE (UNIT=6,FMT='(//,'' NEWDIR IS '',A,//)') NEWDIR(1:LDIR)
!------------------------------------------
!
      IF (ISU.EQ.0) THEN
!!!        CALL PUST(6,' input : '//CH001)
!!!        CALL PUST(35,' input : '//CH001)
      ELSE
!!!        CALL PUST(6,' waiting for TITLE : '//CH001)
!!!        CALL PUST(35,' waiting for TITLE : '//CH001)
      END IF

      IF (CH001(1:1).EQ.'!'.OR.CH001.EQ.' ') GOTO 1
      IF (CH001(1:6).EQ.'FERTIG') GOTO 999
      CALL TAXI(CH001,KEYWORD)
      CALL LABLA(KEYWORD,I1)
      IF (KEYWORD(I1:I1).EQ.':') KEYWORD(I1:I1)=' '
!!      IF (.NOT.VERGL(KEYWORD(1:6),'TITLE').AND.ISU.EQ.1) GOTO 1
      L001=.NOT.VERGL(KEYWORD(1:6),'TITLE')
      L002=.NOT.VERGL(KEYWORD(1:7),'TITLE2')
      IF (L001.AND.L002.AND.ISU.EQ.1) GOTO 1
!
      IF (PRTEST) WRITE (UNIT=6,FMT='(/,'' keyword = '',A)') KEYWORD
!
!======================================================================
!======================================================================
!      ISU: used for subroutines to return if phase or element not found in dataabase
!           ISU=0: OK    ISU=1: not found
!           also used to test that all ENO(I)=1
!      NCOMPS: number of components defining bulk in key=COMPS (and COMP0)
!      BUFORMUL0: bulk formula as read in BULK
!      BUFORMUL: used to define plots. (May differ from BUFORMUL0 with added H2O and CO2
!      FLUFORMUL: formula for fluid (is e.g. added to BUFORMUL)
!      NCHOOSE: number of chosen phases (= pick)
!      NPICK: number of chosen phases
!      ALLP: if true read all phases from database (by setting all elemets n=1)
!
    3 IF (VERGL(KEYWORD,'TITLE')) THEN
       IF (NPLOTS.GT.0) THEN
         CALL MAKEPLOT
       END IF
       CALL MAKESHORT
       TITLE=CH001(1:200)
       DO I=1,4
         ENO(I)=0
       END DO
       ENO(4)=1
       ENO(1)=1
       ISU=0
       NCOMPS=0
       BUFORMUL=' '
       BUFORMUL0=' '
       FLUFORMUL=' '
       NCHOOSE=0
       NPICK=0
       PICKSTRING=' '
       ALLP=.FALSE.
       INFO1=' '
       COMM1=' '
       COMM2=' '
       NICREAC=' '
       REAID=' '
       TABPNR='Nr.'
       TABREA='no reaction'
       TABAUT='no author'
       REFER=TABAUT
       EXONE=0
       CALL FIBLA(CH001,I1)
       IF (I1.EQ.0) I1=1
       CH002=CH001(I1:)
       CALL LABLA(CH002,I2)
       IF (I2.EQ.0) I2=1
       I3=INDEX(CH002,'  ')
       IF (I3.GT.1) I2=I3-1
       TABAUT=CH002(1:I2)
       REFER=TABAUT
       NPLAUT=1
       PLAUT(1)=REFER
       TABBUL=' '
       TABPHA=' '
       NPLOTS=0
       PLOTTYP=' '
       KEY1=' '
       KEY2=' '
       CTUNIT='NN'
       NCOEFF=0
       REAOK=.FALSE.
       NEXP=0
       NINC=0
       EXPER='no experiments'
!
       WRITE (UNIT=scr,FMT='(//,130A1)') ('=',J=1,130)
!!!       WRITE (UNIT=35,FMT='(//,130A1)') ('=',J=1,130)
      END IF
!======================================================================
!======================================================================
      IF (VERGL(KEYWORD,'TITLE2')) THEN
       CALL MAKESHORT
       TITLE=CH001(1:200)
       DO I=1,4
         ENO(I)=0
       END DO
       ENO(4)=1
       ENO(1)=1
       BUFORMUL=' '
       BUFORMUL0=' '
       FLUFORMUL=' '
       NCHOOSE=0
       NPICK=0
       PICKSTRING=' '
       ALLP=.FALSE.
       INFO1=' '
       COMM1=' '
       NICREAC=' '
       REAID=' '
       TABREA='no reaction'
       TABAUT='no author'
       REFER=TABAUT
       EXONE=0
       CALL FIBLA(CH001,I1)
       IF (I1.EQ.0) I1=1
       CH002=CH001(I1:)
       CALL LABLA(CH002,I2)
       IF (I2.EQ.0) I2=1
       I3=INDEX(CH002,'  ')
       IF (I3.GT.1) I2=I3-1
       TABAUT=CH002(1:I2)
       REFER=TABAUT
       NPLAUT=NPLAUT+1
       PLAUT(NPLAUT)=REFER
       TABBUL=' '
       TABPHA=' '
       REAOK=.FALSE.
       NEXP=0
       NINC=0
       EXPER='no experiments'
!
       IF (PLAUT(NPLAUT).EQ.PLAUT(NPLAUT-1)) THEN
       NPLAUT=NPLAUT-1
       ELSE
         CALL PLOTAUT
       END IF
!
      END IF
!======================================================================
!======================================================================
      L001=VERGL(KEYWORD,'-TITLE')
      L002=VERGL(KEYWORD,'-TITLE2')
      IF (L001.OR.L002) THEN
       WRITE (UNIT=6,FMT='('' -TIT found'')')
!
    5  READ (UNIT=drv,FMT='(A)',END=999) CH001
       IF (CH001(1:1).EQ.'!'.OR.CH001.EQ.' ') GOTO 5
!       IF (CH001(1:6).EQ.'FERTIG') GOTO 999
       CALL TAXI(CH001,KEYWORD)
       CALL LABLA(KEYWORD,I1)
       IF (KEYWORD(I1:I1).EQ.':') KEYWORD(I1:I1)=' '
!
       IF (.NOT.VERGL(KEYWORD(1:6),'TITLE')) GOTO 5
!
       GOTO 3
      END IF
!======================================================================
!======================================================================
      IF (VERGL(KEYWORD,'INFO')) THEN
       INFO1=CH001(1:200)
      END IF
!======================================================================
!======================================================================
      IF (VERGL(KEYWORD,'REAC')) THEN
       NICREAC=CH001(1:200)
       ENO(1)=1
       CALL FIBLA(NICREAC,I1)
       TABREA=NICREAC(I1:)
       NICREAC=TABREA
      END IF
!======================================================================
!======================================================================
      L001=VERGL(KEYWORD,'REAID')
      L002=VERGL(KEYWORD,'REACID')
      IF (L001.OR.L002) THEN         

        CALL TAXI(CH001,REAID)

        CALL LABLA(REAID,I1)
        CH80='_'//REAID(1:I1)
        IF ('_'//REAID(1:I1)//dir(1:1).NE.NEWDIR(1:LDIR)) THEN

          WRITE (UNIT=6,FMT='('' new directory name would be '',a)') '_'//REAID(1:I1)//dir(1:1)
!          DO NOT CALL YET READIR, WAIT FOR 'EXP' OR ALL ENO'S=1
!          CALL NEWREADIR

        END IF
        I1=I1+1
!===
        ENO(2)=1
        IF (VERGL(KEYW,'exp')) THEN
          ENO(2)=0
          IF (NKEYWS.GT.0) THEN
            CALL PUST(6,' REAID    : '//REAID)
            CALL PUST(6,' KEYWS(1) : '//KEYWS(1))
            IF (.NOT.VERGL(REAID,KEYWS(1))) THEN
              WRITE (UNIT=6,FMT='('' -- REAID does not match'')')
              ISU=1
            ELSE
              WRITE (UNIT=6,FMT='('' ++ REAID does match'')')
              ENO(2)=1
            END IF
          END IF
        END IF
!===
        IF (VERGL(KEYW,'all')) THEN
          ENO(2)=1
          WRITE (UNIT=6,FMT='('' -- KEYW = all'')')
        END IF
!===
      END IF
!+
!======================================================================
!======================================================================
      IF (VERGL(KEYWORD,'PHASES')) THEN
      NCHOOSE0=0
      NCHOOSE=0
      I1=1
      TABPHA=' '
   30 CALL TAXI(CH001,CH16)
      TABPHA(I1:)=CH16
      CALL LABLA(TABPHA,I1)
      I1=I1+2
      IF (CH16.EQ.' ') GOTO 31
      NCHOOSE0=NCHOOSE0+1
      NCHOOSE=NCHOOSE+1
      CALL TRANSL(CH16)
      CHOOSE(NCHOOSE)=CH16(1:16)
      CHOOSE0(NCHOOSE0)=CH16(1:16)
      GOTO 30
   31 CONTINUE
      PICKSTRING=' '
      I1=3
      DO I=1,NCHOOSE
      PICKSTRING(I1:)=CHOOSE(I)
      CALL LABLA(PICKSTRING,I2)
      I1=I2+3
      END DO
!+++
      CALL PHAINDB(ISU)
      IF (ISU.EQ.1) GOTO 1
!+++
      IF (PRTEST) THEN
        CALL LABLA(PICKSTRING,I1)
        WRITE (UNIT=6,FMT='('' new phases: (all OK)'',A)') PICKSTRING(1:I1)
      END IF

!      ENO(3)=1

       DO I1=1,NKEYWS
         WRITE (UNIT=6,FMT='(I3,''  PHA: KEYWS(I)'',A)') I1,KEYWS(I1)
       END DO

      ENO(3)=1
      IF (VERGL(KEYW,'pha')) THEN
       ENO(3)=0
       IC=0
       DO I1=1,NKEYWS
        DO I2=1,NCHOOSE
         IF (VERGL(CHOOSE(I2),KEYWS(I1))) IC=IC+1
        END DO
       END DO
!       IF (IC.NE.NKEYWS) THEN
!             ISU=1
!             GOTO 1
!       END IF
       IF (IC.EQ.0) THEN
             ISU=1
             GOTO 1
       END IF
       IF (IC.GT.0) ENO(3)=1
      END IF
!---
      IF (ENO(3).EQ.1) THEN
        WRITE (UNIT=6,FMT='('' PHA: this reaction will be used'')')
      ELSE
        WRITE (UNIT=6,FMT='('' PHA: this reaction will not be used'')')
      END IF
!===
        IF (VERGL(KEYW,'all')) THEN
          ENO(3)=1
          WRITE (UNIT=6,FMT='('' -- KEYW = all'')')
        END IF
!===
!+++
      END IF
!
!======================================================================
!======================================================================
      IF (VERGL(KEYWORD,'COMM1')) THEN
       COMM1=CH001(1:200)
      END IF
!
!======================================================================
!======================================================================
      IF (VERGL(KEYWORD,'COMM2')) THEN
       COMM2=CH001(1:200)
      END IF
!
!======================================================================
!======================================================================
      IF (VERGL(KEYWORD,'RCOEFF')) THEN
      CALL CHECKENO
      NCOEFF=0
   24 CALL GELI(CH001,FF)
      I001=IDINT(FF)
      IF (I001.EQ.0) GOTO 25
      NCOEFF=NCOEFF+1
      COEFF(NCOEFF)=FF
      CALL TAXI(CH001,RPHA(NCOEFF))
      CALL TRANSL(RPHA(NCOEFF))
      GOTO 24
   25 CONTINUE
      END IF
!
!======================================================================
!======================================================================
      IF (VERGL(KEYWORD,'PLOTEXCH')) THEN
      CALL CHECKENO
       CALL TAXI(CH001,PLFILE)
       TABPNR=PLFILE
       IF (REAID.EQ.' ') REAID=PLFILE
!--
!----  read input needed for bin++ and plot
!----  read plot file name
       PLOTTYP='EXC'
       NPLOTS=NPLOTS+1
       WRITE (UNIT=CH16,FMT='(I2.2)') NPLOTS
       CALL LABLA(CH16,I3)
       CALL LABLA(PLFILE,I1)
       CALL LABLA(DBUSED,I2)
       PLFPLUS=PLFILE(1:I1)//'_'//DBUSED(1:I2)//'_'//CH16(1:I3)// &
       '_'//PLOTTYP
       AUTOTYP(NPLOTS)=PLOTTYP
       AUTOPLOT(NPLOTS)=PLFPLUS
       TUNIT(NPLOTS)='NN'
!      read compositions
       DO I=1,2
        READ (UNIT=drv,FMT='(A)',END=999) BINCH(I)
       END DO
!      read RAT lines (get info for plot)
       READ (UNIT=drv,FMT='(A)',END=999) BININ
       CALL GETRAT(BININ,XCPH,XCEL1,XCEL1DIV,XCEL1CH, &
       XCEL2,XCEL2DIV,XCEL2CH,PLXMIN,PLXMAX,PLBR)
       BINV(1)=BININ
       READ (UNIT=drv,FMT='(A)',END=999) BININ
       CALL GETRAT(BININ,YCPH,YCEL1,YCEL1DIV,YCEL1CH, &
       YCEL2,YCEL2DIV,YCEL2CH,PLYMIN,PLYMAX,PLHO)
       BINV(2)=BININ
!----
        EXPLOT=.TRUE.
        CALL LABLA(XCPH,I1)
        CALL LABLA(XCEL1CH,I2)
        CALL LABLA(XCEL2CH,I3)
        XVARI=XCEL1CH(1:I2)//'/('//XCEL1CH(1:I2)// &
        '+'//XCEL2CH(1:I3)//') ('//XCPH(1:I1)//')'
        CALL LABLA(YCPH,I1)
        CALL LABLA(YCEL1CH,I2)
        CALL LABLA(YCEL2CH,I3)
        YVARI=YCEL1CH(1:I2)//'/('//YCEL1CH(1:I2)// &
        '+'//YCEL2CH(1:I3)//') ('//YCPH(1:I1)//')'
        XKEY(NPLOTS)='RAT'
        YKEY(NPLOTS)='RAT'
!
      CALL PREPPLOT
      END IF
!
!======================================================================
!======================================================================
      IF (VERGL(KEYWORD,'PLOTEXC2')) THEN
      CALL CHECKENO
       CALL TAXI(CH001,PLFILE)
       TABPNR=PLFILE
       IF (REAID.EQ.' ') REAID=PLFILE
!--
!----  read input needed for bin++ and plot
!----  read plot file name
       PLOTTYP='EX2'
       NPLOTS=NPLOTS+1
       WRITE (UNIT=CH16,FMT='(I2.2)') NPLOTS
       CALL LABLA(CH16,I3)
       CALL LABLA(PLFILE,I1)
       CALL LABLA(DBUSED,I2)
       PLFPLUS=PLFILE(1:I1)//'_'//DBUSED(1:I2)//'_'//CH16(1:I3)// &
       '_'//PLOTTYP
       AUTOTYP(NPLOTS)=PLOTTYP
       AUTOPLOT(NPLOTS)=PLFPLUS
       TUNIT(NPLOTS)='NN'
!      read compositions
       DO I=1,2
        READ (UNIT=drv,FMT='(A)',END=999) BINCH(I)
       END DO
!
!      read RAT lines (get info for plot)
       READ (UNIT=drv,FMT='(A)',END=999) BININ
       CALL GETRAT(BININ,XRPH,XREL1,XREL1DIV,XREL1CH, &
       XREL2,XREL2DIV,XREL2CH,PLXMIN,PLXMAX,PLBR)
       BINV(1)=BININ
       READ (UNIT=drv,FMT='(A)',END=999) BININ
       CALL TAXI(BININ,KEY2)
       CALL TAXI(BININ,XCPH)
       CALL TRANSL(XCPH)
       CALL FUNTAXI(BININ,XCEL1,XCEL1DIV,XCEL1CH)
       CALL FUNTAXI(BININ,XCEL2,XCEL2DIV,XCEL2CH)
       CALL TAXI(BININ,YCPH)
       CALL TRANSL(YCPH)
       CALL FUNTAXI(BININ,YCEL1,YCEL1DIV,YCEL1CH)
       CALL FUNTAXI(BININ,YCEL2,YCEL2DIV,YCEL2CH)
       CALL GELI(BININ,PLYMIN)
       CALL GELI(BININ,PLYMAX)
       CALL GELI(BININ,PLHO)
       CALL LABLA(KEY2,I1)
       CALL LABLA(XCPH,I2)
       CALL LABLA(XCEL1CH,I3)
       CALL LABLA(XCEL2CH,I4)
       HTXT1=KEY2(1:I1)//'   '//XCPH(1:I2)//'   '//XCEL1CH(1:I3)// &
       '   '//XCEL2CH(1:I4)
       CALL LABLA(YCPH,I2)
       CALL LABLA(YCEL1CH,I3)
       CALL LABLA(YCEL2CH,I4)
       HTXT2=YCPH(1:I2)//'   '//YCEL1CH(1:I3)// &
       '   '//YCEL2CH(1:I4)
       WRITE (UNIT=HTXT3,FMT='(3(3X,F7.2))') PLYMIN,PLYMAX,PLHO
       CALL LABLA(HTXT1,I1)
       CALL LABLA(HTXT2,I2)
       CALL LABLA(HTXT3,I3)
       BINV(2)=HTXT1(1:I1)//'   '//HTXT2(1:I2)//'   '//HTXT3(1:I3)
!----
        EXPLOT=.TRUE.
        CALL LABLA(XRPH,I1)
        CALL LABLA(XREL1CH,I2)
        CALL LABLA(XREL2CH,I3)
        XVARI=XREL1CH(1:I2)//'/('//XREL1CH(1:I2)// &
        '+'//XREL2CH(1:I3)//') ('//XRPH(1:I1)//')'

        YVARI='LNKD'
        XKEY(NPLOTS)='RAT'
        YKEY(NPLOTS)='LNKD'
!
      CALL PREPPLOT
      END IF
!
!
!======================================================================
!======================================================================
      IF (VERGL(KEYWORD,'PLOTBIVA')) THEN
       CALL CHECKENO
       CALL TAXI(CH001,PLFILE)
       TABPNR=PLFILE
       IF (REAID.EQ.' ') REAID=PLFILE
!--
!----  read input needed for bin++ and plot
!----  read plot file name
       PLOTTYP='BIV'
       NPLOTS=NPLOTS+1
       WRITE (UNIT=CH16,FMT='(I2.2)') NPLOTS
       CALL LABLA(CH16,I3)
       CALL LABLA(PLFILE,I1)
       CALL LABLA(DBUSED,I2)
       PLFPLUS=PLFILE(1:I1)//'_'//DBUSED(1:I2)//'_'//CH16(1:I3)// &
       '_'//PLOTTYP
       AUTOTYP(NPLOTS)=PLOTTYP
       AUTOPLOT(NPLOTS)=PLFPLUS
       TUNIT(NPLOTS)='NN'
!      read compositions
       DO I=1,2
        READ (UNIT=drv,FMT='(A)',END=999) BINCH(I)
       END DO


      DO I=1,2
       CH001=BINCH(I)
       CALL TAXI(CH001,FORMUL)
       CALL CHEMIE(COMAY,NC,OXYDE,OXANZ,FORMUL,CHE)
       DO I1=1,NC
         CHEBI(I,I1)=CHE(I1)
       END DO
      END DO
!===
!      read VAL (or other?) lines (get info for plot)
       READ (UNIT=drv,FMT='(A)',END=999) BININ
       CALL TAXI(BININ,KEY1)
       CALL TAXI(BININ,XCPH)
       CALL TRANSL(XCPH)
       CALL FUNTAXI(BININ,XCEL1,XCEL1DIV,XCEL1CH)
       CALL GELI(BININ,PLXMIN)
       CALL GELI(BININ,PLXMAX)
       CALL GELI(BININ,PLBR)
       BIXPH=XCPH
       BIXEL=XCEL1
       BIXELDIV=XCEL1DIV
       BIXELCH=XCEL1CH
!!       BINV(1)=BININ
!
       READ (UNIT=drv,FMT='(A)',END=999) BININ
       CALL TAXI(BININ,KEY2)
       CALL TAXI(BININ,YCPH)
       CALL TRANSL(YCPH)
       CALL FUNTAXI(BININ,YCEL1,YCEL1DIV,YCEL1CH)
       CALL GELI(BININ,PLYMIN)
       CALL GELI(BININ,PLYMAX)
       CALL GELI(BININ,PLHO)
       BIYPH=YCPH
       BIYEL=YCEL1
       BIYELDIV=YCEL1DIV
       BIYELCH=YCEL1CH
!
       CALL LABLA(KEY1,I1)
       CALL LABLA(XCPH,I2)
       CALL LABLA(XCEL1CH,I3)
       HTXT1=KEY1(1:I1)//'   '//XCPH(1:I2)//'   '//XCEL1CH(1:I3)
       WRITE (UNIT=HTXT3,FMT='(3(3X,F7.2))') PLXMIN,PLXMAX,PLBR
       CALL LABLA(HTXT1,I1)
       CALL LABLA(HTXT3,I3)
       BINV(1)=HTXT1(1:I1)//'   '//HTXT3(1:I3)

       CALL LABLA(KEY2,I1)
       CALL LABLA(YCPH,I2)
       CALL LABLA(YCEL1CH,I3)
       HTXT1=KEY2(1:I1)//'   '//YCPH(1:I2)//'   '//YCEL1CH(1:I3)
       WRITE (UNIT=HTXT3,FMT='(3(3X,F7.2))') PLYMIN,PLYMAX,PLHO
       CALL LABLA(HTXT1,I1)
       CALL LABLA(HTXT3,I3)
       BINV(2)=HTXT1(1:I1)//'   '//HTXT3(1:I3)
!----
        EXPLOT=.TRUE.
        CALL LABLA(XCPH,I1)
        CALL LABLA(XCEL1CH,I2)
        XVARI=XCPH(1:I1)//' '//XCEL1CH(1:I2)
        CALL LABLA(YCPH,I1)
        CALL LABLA(YCEL1CH,I2)
        YVARI=YCPH(1:I1)//' '//YCEL1CH(1:I2)

        XKEY(NPLOTS)=KEY1
        YKEY(NPLOTS)=KEY2
!
      CALL PREPPLOT
      END IF
!
!======================================================================
!======================================================================
      IF (VERGL(KEYWORD,'PLOTVAL')) THEN
      CALL CHECKENO
       CALL TAXI(CH001,PLFILE)
       TABPNR=PLFILE
       IF (REAID.EQ.' ') REAID=PLFILE
!--
!----  read input needed for fun++ and plot
!----  read plot file name
       PLOTTYP='VAL'
       NPLOTS=NPLOTS+1
       WRITE (UNIT=CH16,FMT='(I2.2)') NPLOTS
       CALL LABLA(CH16,I3)
       CALL LABLA(PLFILE,I1)
       CALL LABLA(DBUSED,I2)
       PLFPLUS=PLFILE(1:I1)//'_'//DBUSED(1:I2)//'_'//CH16(1:I3)// &
       '_'//PLOTTYP
       AUTOTYP(NPLOTS)=PLOTTYP
       AUTOPLOT(NPLOTS)=PLFPLUS
!      read composition
       READ (UNIT=drv,FMT='(A)',END=999) BINCH(1)
       READ (UNIT=drv,FMT='(A)',END=999) BININ
       BINV(1)=BININ
       CALL TAXI(BININ,KEY1)
       CALL GELI(BININ,PLXMIN)
       CALL GELI(BININ,PLXMAX)
       CALL GELI(BININ,PLBR)
       READ (UNIT=drv,FMT='(A)',END=999) BININ
       CALL TAXI(BININ,KEY2)

      CALL TAXI(BININ,XCPH)
      CALL TRANSL(XCPH)
       CH002=BININ
      CALL FUNTAXI(BININ,XCEL1,XCEL1DIV,XCEL1CH)

       CALL GELI(BININ,PLYMIN)
       CALL GELI(BININ,PLYMAX)
       CALL GELI(BININ,PLHO)

       CALL LABLA(KEY2,I1)
       CALL LABLA(XCPH,I2)
       CALL LABLA(CH002,I3)
       BINV(2)=KEY2(1:I1)//'   '//XCPH(1:I2)//'   '//CH002(1:I3)

        XVARI=KEY1
        IF (KEY1.EQ.'TC') THEN
          XVARI='T[C]'
          TUNIT(NPLOTS)='TC'
        END IF
        IF (KEY1.EQ.'TK') THEN
          XVARI='T[K]'
          TUNIT(NPLOTS)='TK'
        END IF
        IF (KEY1.EQ.'P') THEN
          XVARI='P[Bar]'
          TUNIT(NPLOTS)='NN'
        END IF
        CALL LABLA(XCPH,I1)
        CALL LABLA(XCEL1CH,I2)
        YVARI=XCEL1CH(1:I2)//' ('//XCPH(1:I1)//')'
        XKEY(NPLOTS)=KEY1
        YKEY(NPLOTS)=KEY2
!
      CALL PREPPLOT
      END IF
!
!======================================================================
! type = LKD
!======================================================================
      IF (VERGL(KEYWORD,'PLOTKD')) THEN
      CALL CHECKENO
       CALL TAXI(CH001,PLFILE)
       TABPNR=PLFILE
       IF (REAID.EQ.' ') REAID=PLFILE
!--
!----  read input needed for fun++ and plot
!----  read plot file name
       PLOTTYP='LKD'
       NPLOTS=NPLOTS+1
       WRITE (UNIT=CH16,FMT='(I2.2)') NPLOTS
       CALL LABLA(CH16,I3)
       CALL LABLA(PLFILE,I1)
       CALL LABLA(DBUSED,I2)
       PLFPLUS=PLFILE(1:I1)//'_'//DBUSED(1:I2)//'_'//CH16(1:I3)// &
       '_'//PLOTTYP
       AUTOTYP(NPLOTS)=PLOTTYP
       AUTOPLOT(NPLOTS)=PLFPLUS
!      read composition
       READ (UNIT=drv,FMT='(A)',END=999) BINCH(1)
       READ (UNIT=drv,FMT='(A)',END=999) BININ
       BINV(1)=BININ
       CALL TAXI(BININ,KEY1)
       CALL GELI(BININ,PLXMIN)
       CALL GELI(BININ,PLXMAX)
       CALL GELI(BININ,PLBR)
       READ (UNIT=drv,FMT='(A)',END=999) BININ
       CALL TAXI(BININ,KEY2)

       CALL TAXI(BININ,XCPH)
       CALL TRANSL(XCPH)
       CALL FUNTAXI(BININ,XCEL1,XCEL1DIV,XCEL1CH)
       CALL FUNTAXI(BININ,XCEL2,XCEL2DIV,XCEL2CH)
       CALL TAXI(BININ,YCPH)
       CALL TRANSL(YCPH)
       CALL FUNTAXI(BININ,YCEL1,YCEL1DIV,YCEL1CH)
       CALL FUNTAXI(BININ,YCEL2,YCEL2DIV,YCEL2CH)

       CALL GELI(BININ,PLYMIN)
       CALL GELI(BININ,PLYMAX)
       CALL GELI(BININ,PLHO)

       CALL LABLA(KEY2,I1)
       CALL LABLA(XCPH,I2)
       CALL LABLA(XCEL1CH,I3)
       CALL LABLA(XCEL2CH,I4)
       HTXT1=KEY2(1:I1)//'   '//XCPH(1:I2)//'   '//XCEL1CH(1:I3)// &
       '   '//XCEL2CH(1:I4)
       CALL LABLA(YCPH,I2)
       CALL LABLA(YCEL1CH,I3)
       CALL LABLA(YCEL2CH,I4)
       HTXT2=YCPH(1:I2)//'   '//YCEL1CH(1:I3)// &
       '   '//YCEL2CH(1:I4)
       WRITE (UNIT=HTXT3,FMT='(3(3X,F7.2))') PLYMIN,PLYMAX,PLHO
       CALL LABLA(HTXT1,I1)
       CALL LABLA(HTXT2,I2)
       CALL LABLA(HTXT3,I3)
       BINV(2)=HTXT1(1:I1)//'   '//HTXT2(1:I2)//'   '//HTXT3(1:I3)

        XVARI=KEY1
        IF (KEY1.EQ.'TC') THEN
          XVARI='T[C]'
          TUNIT(NPLOTS)='TC'
        END IF
        IF (KEY1.EQ.'TK') THEN
          XVARI='T[K]'
          TUNIT(NPLOTS)='TK'
        END IF
        IF (KEY1.EQ.'P') THEN
          XVARI='P[Bar]'
          TUNIT(NPLOTS)='NN'
        END IF
        YVARI='LNKD'
        XKEY(NPLOTS)=KEY1
        YKEY(NPLOTS)=KEY2
!
      CALL PREPPLOT
      END IF
!
!======================================================================
! type = LKR
!======================================================================
! plot the ln K of a reaction (calculated from endmember reaction)
! example: n1 A + n2 B = n3 C +n4 D
! K = (act(C)^n3 * act(D)^n4) / (act(A)^n1 * act(B)^n2)
! 
      IF (VERGL(KEYWORD,'PLOTLNKR')) THEN
      CALL CHECKENO
       CALL TAXI(CH001,PLFILE)
       TABPNR=PLFILE
       IF (REAID.EQ.' ') REAID=PLFILE
!--
      IF (NCOEFF.EQ.0) THEN
       WRITE (UNIT=6,FMT='(//''reaction coefficients not defined'')')
       WRITE (UNIT=35,FMT='(//''reaction coefficients not defined'')')
       GOTO 1
      END IF
!----  read input needed for rea++ and plot
!----  read plot file name
       PLOTTYP='LKR'
       NPLOTS=NPLOTS+1
       WRITE (UNIT=CH16,FMT='(I2.2)') NPLOTS
       CALL LABLA(CH16,I3)
       CALL LABLA(PLFILE,I1)
       CALL LABLA(DBUSED,I2)
       PLFPLUS=PLFILE(1:I1)//'_'//DBUSED(1:I2)//'_'//CH16(1:I3)// &
       '_'//PLOTTYP
       AUTOTYP(NPLOTS)=PLOTTYP
       AUTOPLOT(NPLOTS)=PLFPLUS
!      read composition
       READ (UNIT=drv,FMT='(A)',END=999) BINCH(1)
       READ (UNIT=drv,FMT='(A)',END=999) BININ
       BINV(1)=BININ
       CALL TAXI(BININ,KEY1)
       CALL GELI(BININ,PLXMIN)
       CALL GELI(BININ,PLXMAX)
       CALL GELI(BININ,PLBR)
!
       READ (UNIT=drv,FMT='(A)',END=999) BININ
       BINV(2)=BININ
       CALL TAXI(BININ,KEY2)
       CALL GELI(BININ,PLYMIN)
       CALL GELI(BININ,PLYMAX)
       CALL GELI(BININ,PLHO)
!
       BININ=' '
       DO I=1,NCOEFF
         CALL LABLA(BININ,I1)
         WRITE (UNIT=CH16,FMT='(F7.2)') COEFF(I)
         CALL FIBLA(CH16,I2)
         CALL LABLA(CH16,I3)
         CALL LABLA(RPHA(I),I4)
         BININ(I1+3:)=CH16(I2:I3)//'  '//RPHA(I)(1:I4)
       END DO
       BINV(3)=BININ
!
        XVARI=KEY1
        IF (KEY1.EQ.'TC') THEN
          XVARI='T[C]'
          TUNIT(NPLOTS)='TC'
        END IF
        IF (KEY1.EQ.'TK') THEN
          XVARI='T[K]'
          TUNIT(NPLOTS)='TK'
        END IF
        IF (KEY1.EQ.'P') THEN
          XVARI='P[Bar]'
          TUNIT(NPLOTS)='NN'
        END IF
        YVARI='LNKD'
        XKEY(NPLOTS)=KEY1
        YKEY(NPLOTS)=KEY2
!
      CALL PREPPLOT
      END IF
!
!======================================================================
!======================================================================
      L001=VERGL(KEYWORD,'PLOT')
      L002=VERGL(KEYWORD,'PLOTXCO2')
      L003=VERGL(KEYWORD,'PLOTISO')
      L004=VERGL(KEYWORD,'PLOTPX')
      L005=VERGL(KEYWORD,'PLOTTX')
      IF (L001.OR.L002.OR.L003.OR.L004.OR.L005) THEN
      CALL CHECKENO
      
       WRITE (UNIT=6,FMT='(''preparing plots, newdir='',a)') NEWDIR
       WRITE (UNIT=6,FMT='(''preparing plots, REAID='',a)') REAID

       CALL TAXI(CH001,PLFILE)
       TABPNR=PLFILE
       IF (REAID.EQ.' ') REAID=PLFILE
       IF (VERGL(KEYWORD,'PLOT')) PLOTTYP='PT'
       IF (VERGL(KEYWORD,'PLOTPX')) PLOTTYP='PX'
       IF (VERGL(KEYWORD,'PLOTTX')) PLOTTYP='TX'
       IF (VERGL(KEYWORD,'PLOTXCO2')) PLOTTYP='TXC'
       IF (VERGL(KEYWORD,'PLOTISO')) PLOTTYP='ISO'
!----------------------------------------------------------------
!--    NPLOTS   = number of plots for one set of experiments
!--    AUTOTYP  = PLOTTYP of plot NPLOTS
!--    AUTOPLOT = first part of all file names for plot NPLOTS
!----------------------------------------------------------------
       NPLOTS=NPLOTS+1
       WRITE (UNIT=CH16,FMT='(I2.2)') NPLOTS
       CALL LABLA(CH16,I3)
       CALL LABLA(PLFILE,I1)
       CALL LABLA(DBUSED,I2)
       PLFPLUS=PLFILE(1:I1)//'_'//DBUSED(1:I2)//'_'//CH16(1:I3)// &
       '_'//PLOTTYP
       AUTOTYP(NPLOTS)=PLOTTYP
       AUTOPLOT(NPLOTS)=PLFPLUS
       TUNIT(NPLOTS)='TC'
!=====
        CALL GELI(CH001,PLXMIN)
        CALL GELI(CH001,PLXMAX)
        CALL GELI(CH001,PLBR)
        CALL GELI(CH001,PLYMIN)
        CALL GELI(CH001,PLYMAX)
        CALL GELI(CH001,PLHO)
!====================================================================
!=== read compositions and RAT lines if P-X or T-X
        L001=VERGL(KEYWORD,'PLOTPX')
        L002=VERGL(KEYWORD,'PLOTTX')
        IF (L001.OR.L002) THEN
          DO I=1,2
            READ (UNIT=drv,FMT='(A)',END=999) BINCH(I)
            CALL PUST(6,' input: '//BINCH(I))
            CALL PUST(35,' input: '//BINCH(I))
          END DO
          CALL GELI(CH001,PLCONST)
!--- read RAT
          READ (UNIT=drv,FMT='(A)',END=999) BININ
            CALL PUST(6,' input: '//BININ)
            CALL PUST(35,' input: '//BININ)
          CALL GETRATX(BININ,XCPH,XCEL1,XCEL1DIV,XCEL1CH, &
          XCEL2,XCEL2DIV,XCEL2CH)
          BINV(1)=BININ
          READ (UNIT=drv,FMT='(A)',END=999) BININ
            CALL PUST(6,' input: '//BININ)
            CALL PUST(35,' input: '//BININ)
          CALL GETRATX(BININ,YCPH,YCEL1,YCEL1DIV,YCEL1CH, &
          YCEL2,YCEL2DIV,YCEL2CH)
          BINV(2)=BININ
!----
          EXPLOT=.TRUE.
          CALL LABLA(XCPH,I1)
          CALL LABLA(XCEL1CH,I2)
          CALL LABLA(XCEL2CH,I3)
          XVARI=XCEL1CH(1:I2)//'/('//XCEL1CH(1:I2)// &
          '+'//XCEL2CH(1:I3)//') ('//XCPH(1:I1)//')'
          CALL LABLA(YCPH,I1)
          CALL LABLA(YCEL1CH,I2)
          CALL LABLA(YCEL2CH,I3)
          YVARI=YCEL1CH(1:I2)//'/('//YCEL1CH(1:I2)// &
          '+'//YCEL2CH(1:I3)//') ('//YCPH(1:I1)//')'
!
        END IF
!----------------------------------------------------------------
!--    write header of experimental plotfile file 30   _0exp
!--    write header of experimental plotfile file 32   _exn
!--    write header of experimental plotfile file 34   _exn2
!----------------------------------------------------------------
      CALL LABLA(AUTOPLOT(NPLOTS),I1)
      OPFILE=NEWDIR(1:LDIR)//AUTOPLOT(NPLOTS)(1:I1)//'_0exp'
!      CALL PUST(6,'Open file 30: '//OPFILE)
      CALL LABLA(OPFILE,IO)
!-- open _0exp (experiments with scale, fill: none and gray)
       OPEN (UNIT=30,FILE=OPFILE(1:IO),STATUS='UNKNOWN')
       WRITE (30,2010) PLXMIN,PLXMAX,PLYMIN,PLYMAX,PLBR,PLHO
 2010  FORMAT ( &
       'X-variable',/, &
       'Y-variable',/, &
       F10.2,F10.2,F10.1,F10.1,F10.4,F10.4,'   0  0',/,' ',/, &
       'FCOLOR  0.5  0.5  0.5',/,'FAT    0.01')
!=====
       CALL LABLA(AUTOPLOT(NPLOTS),I1)
      OPFILE=NEWDIR(1:LDIR)//AUTOPLOT(NPLOTS)(1:I1)//'_exn'
      CALL LABLA(OPFILE,IO)
!-- open _exn (experiments no scale, fill: none and gray)
       OPEN (UNIT=32,FILE=OPFILE(1:IO),STATUS='UNKNOWN')
!!       WRITE (UNIT=32,FMT='(/,''FGRAY    0.5'')') 
       WRITE (UNIT=32,FMT='(/,A)') FGRAU
       WRITE (UNIT=32,FMT='(/,''FAT    0.01'')') 
      OPFILE=NEWDIR(1:LDIR)//AUTOPLOT(NPLOTS)(1:I1)//'_exn2'
      CALL LABLA(OPFILE,IO)
!-- open _exn2 (experiments no scale, inconsstencies: black)
       OPEN (UNIT=34,FILE=OPFILE(1:IO),STATUS='UNKNOWN')
!!       WRITE (UNIT=34,FMT='(/,''FGRAY    0.0'')') 
       WRITE (UNIT=34,FMT='(/,A)') FCOLOR(1)
       WRITE (UNIT=34,FMT='(/,''FAT    0.01'')') 
!
       IF (NICREAC.EQ.' ') NICREAC='X'
       CALL FIBLA(NICREAC,I1)
       CALL LABLA(NICREAC,I2)
!
       CALL LABLA(REAID,I3)
       WRITE (30,2012) REAID(1:I3),NICREAC(I1:I2),ZERO,PLHO
       WRITE (32,2012) REAID(1:I3),NICREAC(I1:I2),ZERO,PLHO
       WRITE (34,2012) REAID(1:I3),NICREAC(I1:I2),ZERO,PLHO
  2012 FORMAT ('PSYM   (plot: ',A,') ',A,2X,F8.3,2X,F8.3, &
       '   0.3    0   0   1   0')
       CALL LABLA(DRIVENAME,I3)
       WRITE (30,2013) DRIVENAME(1:I3),PLBR+1.0
       WRITE (32,2013) DRIVENAME(1:I3),PLBR+1.0
       WRITE (34,2013) DRIVENAME(1:I3),PLBR+1.0
  2013 FORMAT ('PSYM    data: ',A,2X,F8.3, &
       '   1.5   0.3   0   0   -0.5   90')
       
! first author here
       WRITE (UNIT=30,FMT='(A)') LCOLOR(NPLAUT)
       WRITE (UNIT=30,FMT='(A)') FGRAU
       WRITE (UNIT=32,FMT='(A)') LCOLOR(NPLAUT)
       WRITE (UNIT=32,FMT='(A)') FGRAU
       WRITE (UNIT=34,FMT='(A)') LCOLOR(NPLAUT)
       WRITE (UNIT=34,FMT='(A)') FCOLOR(NPLAUT)
       YPOS=PLHO-0.45D0
       CALL LABLA(PLAUT(NPLAUT),I3)
       WRITE (30,2055) PLAUT(NPLAUT)(1:I3),ZERO+0.2D0,YPOS
       WRITE (32,2055) PLAUT(NPLAUT)(1:I3),ZERO+0.2D0,YPOS
       WRITE (34,2055) PLAUT(NPLAUT)(1:I3),ZERO+0.2D0,YPOS
  2055 FORMAT ('PSYM    ',A,2X,F8.3,2X,F8.3, &
       '   0.3    0   0   0   0')
!
!
      CLOSE (UNIT=30)
      CLOSE (UNIT=32)
      CLOSE (UNIT=34)
!----------------------------------------------------------------
!--    prepare domino script file 31   .txt
!----------------------------------------------------------------
       DO I=1,6
         CHLINE(I)=' '
       END DO
       XLINE=' '
       YLINE=' '
! plot isolines 
       IF (VERGL(KEYWORD,'PLOTISO')) THEN
         PLOTTYP='ISO'
         CHLINE(1)=BUFORMUL
         READ (UNIT=drv,FMT='(A)',END=999) CH001
         CALL TAXI(CH001,CH16)
         CALL TRANSL(CH16)
         CALL LABLA(CH16,I1)
         CALL LABLA(CH001,I2)
         WASLINE=CH16(1:I1)//'   '//CH001(1:I2)
         WRITE (XLINE,2020) PLXMIN,PLXMAX,ZERO,PLBR
 2020    FORMAT ('TC  ',4(2X,1PE15.8))
         WRITE (YLINE,2022) PLYMIN,PLYMAX,ZERO,PLHO
 2022    FORMAT ('P  ',4(2X,1PE15.8))
         TSIM=0.0D0
         PSIM=0.0D0
        XKEY(NPLOTS)='TC'
        YKEY(NPLOTS)='P'
       END IF
!plot P-T phase diagram
       IF (VERGL(KEYWORD,'PLOT')) THEN
         PLOTTYP='PT'
         CHLINE(1)=BUFORMUL
         WASLINE='.'
         WRITE (XLINE,2024) PLXMIN,PLXMAX,ZERO,PLBR
 2024    FORMAT ('TC  ',4(2X,1PE15.8))
         WRITE (YLINE,2026) PLYMIN,PLYMAX,ZERO,PLHO
 2026    FORMAT ('P  ',4(2X,1PE15.8))
         TSIM=0.0D0
         PSIM=0.0D0
        XKEY(NPLOTS)='TC'
        YKEY(NPLOTS)='P'
       END IF
!plot P-X phase diagram
       IF (VERGL(KEYWORD,'PLOTPX')) THEN
         PLOTTYP='PX'
         CHLINE(1)=BUFORMUL
         CHLINE(2)=BINCH(1)
         CHLINE(3)=BINCH(2)
         WASLINE='.'
         WRITE (XLINE,2028) PLXMIN,PLXMAX,ZERO,PLBR
 2028    FORMAT ('BIN  ',4(2X,1PE15.8))
         WRITE (YLINE,2030) PLYMIN,PLYMAX,ZERO,PLHO
 2030    FORMAT ('P  ',4(2X,1PE15.8))
         TSIM=PLCONST
         PSIM=0.0D0
        XKEY(NPLOTS)='BIN'
        YKEY(NPLOTS)='P'
       END IF
!plot T-X phase diagram
       IF (VERGL(KEYWORD,'PLOTTX')) THEN
         PLOTTYP='TX'
         CHLINE(1)=BUFORMUL
         CHLINE(2)=BINCH(1)
         CHLINE(3)=BINCH(2)
         WASLINE='.'
         WRITE (XLINE,2032) PLXMIN,PLXMAX,ZERO,PLBR
 2032    FORMAT ('BIN  ',4(2X,1PE15.8))
         WRITE (YLINE,2034) PLYMIN,PLYMAX,ZERO,PLHO
 2034    FORMAT ('TC  ',4(2X,1PE15.8))
         TSIM=0.0D0
         PSIM=PLCONST
        XKEY(NPLOTS)='BIN'
        YKEY(NPLOTS)='TC'
       END IF
! plot T-XCO2 diagram
       IF (VERGL(KEYWORD,'PLOTXCO2')) THEN
         PLOTTYP='TXC'
         CALL GELI(CH001,PLCONST)
         CALL LABLA(BUFORMUL0,I2)
         CHLINE(1)=BUFORMUL0(1:I2)//'H(2)O(1)C(1)O(2)'
         CHLINE(2)=BUFORMUL0(1:I2)//'H(20000000)O(10000000)'
         CHLINE(3)=BUFORMUL0(1:I2)//'C(10000000)O(20000000)'
         WASLINE='.'
         WRITE (XLINE,2036) PLXMIN,PLXMAX,ZERO,PLBR
 2036    FORMAT ('BIN  ',4(2X,1PE15.8))
         WRITE (YLINE,2038) PLYMIN,PLYMAX,ZERO,PLHO
 2038    FORMAT ('TC  ',4(2X,1PE15.8))
         TSIM=0.0D0
         PSIM=PLCONST
        XKEY(NPLOTS)='BIN'
        YKEY(NPLOTS)='TC'
       END IF
!====
       LABLINE='1'
!----------------------------------------------------------------
!--    make domino script
!----------------------------------------------------------------
       CALL DOMSCRIPT
!====
      END IF
!======================================================================
!======================================================================
      IF (VERGL(KEYWORD,'EXP')) THEN
       CALL CHECKENO
       NEXP=NEXP+1

       IF (ISINC.EQ.1) NINC=NINC+1

       WRITE (UNIT=6,FMT='(''CCODE IN EXP='',I4)') CCODE
!!         WRITE (UNIT=39,FMT='(''ISINC='',I3)') ISINC
!!         WRITE (UNIT=39,FMT='(''NINC= '',I3)') NINC

!       EXPER=CH001(1:200)
       CALL TAXI(CH001,EXPER)
       CALL FIBLA(CH001,I1)
       IF (I1.EQ.0) I1=1
       CH002=CH001(I1:)
       CALL LABLA(EXPER,I2)
       CH001=EXPER(1:I2)//'     '//CH002
       EXPER=CH001(1:200)
       NFEHL=-1
       CCODE=0
       MCODE=0
       FICOM=0
       ISINC=0
       PEXP=0
       COMM2=' '
       NLOGA=0
       XCO2=0.0D0
       XCO2ERR=0.0D0
       USEDFOR=.TRUE.
!       IF (NCOMPS.GT.0) NCOMPS=0
       FLUFORMUL=' '
       BUFORMUL=BUFORMUL0
       EXONE=EXONE+1
       IF (EXONE.EQ.1) THEN
!       IF (REAOK.OR.PHAOK.OR.ELOK.OR.(KEYW.EQ.'all')) THEN
!
!        CALL LABLA(REAID,I1)
!        IF ('_'//REAID(1:I1)//dir(1:1).NE.NEWDIR) THEN
!          WRITE (UNIT=6,FMT='(''newreadir '',a,a)') '_'//REAID(1:I1)//dir(1:1),NEWDIR
!!          CALL NEWREADIR
!        END IF

!        CALL LABLA(TABPNR,I1)
        CALL LABLA(REAID,I1)
        CALL LABLA(TABREA,I2)
        CALL LABLA(TABAUT,I3)
        CALL LABLA(TABBUL,I4)
        CALL LABLA(TABPHA,I5)
!        CALL PUST(37,REAID(1:I1)//TABTAB//TABREA(1:I2)//TABTAB &
!        //TABAUT(1:I3))
!!!!        //TABTAB//TABBUL(1:I4)//TABTAB//TABPHA(1:I5))
        WRITE (UNIT=39,FMT='('' '')')
        CALL PUSTU(39,REAID(1:I1)//'     '//TABREA(1:I2)//'     ' &
        //TABAUT(1:I3))
!       END IF
!       ELSE
!!!         WRITE (UNIT=39,FMT='(''ISINC='',I3)') ISINC
!!!         WRITE (UNIT=39,FMT='(''NINC= '',I3)') NINC
       END IF
      END IF
!======================================================================
!======================================================================
      IF (VERGL(KEYWORD,'not used')) THEN
       USEDFOR=.FALSE.
      END IF
!======================================================================
!======================================================================
      IF (VERGL(KEYWORD,'LOGA')) THEN
       NLOGA=NLOGA+1
       CALL TAXI(CH001,CH16)
       CALL TRANSL(CH16)
       ALOGA(NLOGA)=CH16(1:16)
       CALL GELI(CH001,XLOGA(NLOGA))
       I001=IDINT(XLOGA(NLOGA))
       IF (ALOGA(NLOGA).EQ.' '.OR.I001.EQ.0) NLOGA=NLOGA-1
      END IF
!======================================================================
!======================================================================
      IF (VERGL(KEYWORD,'XCO2')) THEN
       CALL GELI(CH001,XCO2)
       CALL GELI(CH001,XCO2ERR)
       FF=1000.0D0
       F1=FF*2.0D0*(1.0D0-XCO2)
       F2=FF*(1.0D0-XCO2)
       F3=FF*XCO2
       F4=FF*2.0D0*XCO2
       WRITE (UNIT=FLUFORMUL,FMT=2050) F1,F2,F3,F4
 2050  FORMAT ('H(',F8.1,')O(',F8.1,')C(',F8.1,')O(',F8.1,')')
       CALL COLLAPS(FLUFORMUL,I1)
!       WRITE (UNIT=6,FMT='(''FLU='',A,''='')') FLUFORMUL(1:I1)
      CALL LABLA(BUFORMUL0,I2)
      BUFORMUL=BUFORMUL0(1:I2)//FLUFORMUL(1:I1)
      END IF
!======================================================================
!======================================================================
      IF (VERGL(KEYWORD,'SHOW')) THEN
       SHOW=1
      END IF
!======================================================================
!======================================================================
      IF (VERGL(KEYWORD,'SHOWALL')) THEN
       SHOWALL=.TRUE.
       SHOW=1
      END IF
!======================================================================
!======================================================================
!----- this is just for testing (makes a "nice" bulk-line)
      IF (VERGL(KEYWORD,'BUBU')) THEN
      CALL BUBU(CH002)
!      CALL PUST(6,CH002)
      END IF
!======================================================================
!======================================================================
      IF (VERGL(KEYWORD,'BULK')) THEN
      ALLP=.FALSE.
      NCOMPS=0
      CALL TAXI(CH001,FORMUL)
!--- check if element in database
      CH500=FORMUL
      CALL ELEINDB(CH500,ISU)
      IF (ISU.EQ.1) GOTO 1
!
      BUFORMUL0=FORMUL
      TABBUL=FORMUL(1:100)
      CALL TAXI(CH001,USE)
      BULKUSE=USE
      CALL CHEMIE(COMAY,NC,OXYDE,OXANZ,FORMUL,CHEM)
      CALL LABLA(USE,LUSE)
      CALL DBREAD
!+++
      IF (PRTEST) THEN
      WRITE (scr,2052) ('-',J=1,130)
!!!      WRITE (35,2052) ('-',J=1,130)
 2052 FORMAT (/130A1)
      CALL LABLA(BUFORMUL0,I1)
      WRITE (UNIT=6,FMT='('' new bulk: '',A)') BUFORMUL0(1:I1)
      END IF
      XCO2=0.0D0
      XCO2ERR=0.0D0
      BUFORMUL=BUFORMUL0

!+++
      END IF
!======================================================================
!======================================================================
      L001=VERGL(KEYWORD,'COMPS')
      L002=VERGL(KEYWORD,'COMP0')
      IF (L001.OR.L002) THEN
      IF (VERGL(KEYWORD,'COMP0')) THEN
        NCOMPS=0
      ELSE
        IF (NCOMPS.EQ.0) THEN
          WRITE (UNIT=6,FMT='('' use COMP0 first'')')
          CALL EXIT
        END IF
      END IF
      ALLP=.FALSE.
      NCOMPS=NCOMPS+1
      CALL GELI(CH001,MODE(NCOMPS))
      CALL TAXI(CH001,FORMUL)
      USE='*'
      BULKUSE=USE
      CALL LABLA(USE,LUSE)
      CALL CHEMIE(COMAY,NC,OXYDE,OXANZ,FORMUL,CHEM2)
!      WRITE (UNIT=6,FMT='(I2,F10.2)') (I,CHEM2(I),I=1,NC)
      DO I=1,NC
       CHEM3(NCOMPS,I)=CHEM2(I)
      END DO
      IF (NCOMPS.EQ.1) THEN
      DO I=1,NC
      CHEM(I)=CHEM3(NCOMPS,I)*MODE(NCOMPS)
      END DO
      ELSE
      DO I=1,NC
       CHEM(I)=CHEM(I)+CHEM3(NCOMPS,I)*MODE(NCOMPS)
      END DO
      END IF
      CALL DBREAD
      CALL BUBU(FORMUL)
      BUFORMUL0=FORMUL
      TABBUL=FORMUL(1:100)
!+++
      IF (PRTEST) THEN
      CALL LABLA(BUFORMUL0,I1)
      WRITE (UNIT=6,FMT='('' new bulk: '',A)') BUFORMUL0(1:I1)
      END IF
!
      XCO2=0.0D0
      XCO2ERR=0.0D0
      BUFORMUL=BUFORMUL0

!
      END IF
!
!======================================================================
!======================================================================
      IF (VERGL(KEYWORD,'CHECKVAL')) THEN
      CALL TAXI(CH001,CH16)
      CALL TRANSL(CH16)
      CHKPH=CH16(1:16)
      CALL FUNTAXI(CH001,CHKEL,CHKELDIV,CHKELCH)
      CALL GELI(CH001,FF)
      CALL TAXI(CH001,CH16)
      CALL MINMAX(FF,CH16,CHKMIN,CHKMAX)
      CALL CHECKVAL
      GOTO 1
      END IF
!
!======================================================================
!======================================================================
      IF (VERGL(KEYWORD,'CHECKBIVA')) THEN
      CALL GELI(CH001,FF)
      CALL TAXI(CH001,CH16)
      CALL MINMAX(FF,CH16,F1,F2)
      CALL GELI(CH001,CHKSTART)
      CALL GELI(CH001,FF)
      CALL TAXI(CH001,CH16)
      CALL MINMAX(FF,CH16,CHKMIN,CHKMAX)
      CALL CHECKBIVA(F1,F2)
!---
      GOTO 1
      END IF
!
!======================================================================
!======================================================================
      IF (VERGL(KEYWORD,'CHECKBIN')) THEN

      CALL CHECKBIN
!---
      GOTO 1
      END IF
!======================================================================
!======================================================================
      IF (VERGL(KEYWORD,'LNK')) THEN
      CALL GELI(CH001,FF)
      CALL TAXI(CH001,CH16)
      CALL MINMAX(FF,CH16,CHKMIN,CHKMAX)
      CALL CHECKKREA
!==
      GOTO 1
      END IF
!======================================================================
!======================================================================
      IF (VERGL(KEYWORD,'STABILITY')) THEN
      CALL TAXI(CH001,CH16)
      CALL TRANSL(CH16)
      CALL TAXI(CH001,CH1)
      CALL STABILITY(CH16,CH1)
!==
      GOTO 1
      END IF
!======================================================================
!======================================================================
      IF (VERGL(KEYWORD,'CHECKASS')) THEN
      CTUNIT='TC'
      CCODE=0
!      MCODE=0
      NPASS=0
      NMASS=0
      EXPEC=CH001
  40  CALL TAXI(CH001,TEXT)
      IF (TEXT.EQ.' ') GOTO 41
      IF (TEXT(1:1).EQ.'+') THEN
       NPASS=NPASS+1
       CH16=TEXT(2:)
       CALL TRANSL(CH16)
       PASS(NPASS)=CH16(1:16)
       PMULT(NPASS)=1
      END IF
     IF (TEXT(1:1).EQ.'-') THEN
       NMASS=NMASS+1
       CH16=TEXT(2:)
       CALL TRANSL(CH16)
       MASS(NMASS)=CH16(1:16)
       MMULT(NMASS)=1
      END IF
      GOTO 40
   41 CONTINUE
      CALL CHECKASS(NFEHL)
      SHOW=0
      IF (SHOWALL) SHOW=1
!==
      DO I=1,NPLOTS
        IF (AUTOTYP(I).EQ.'PT') THEN
          PLNR=I
          CALL PLOTEXP(PLNR)
        END IF
        IF (AUTOTYP(I).EQ.'TXC') THEN
          PLNR=I
          CALL PLOTEXP(PLNR)
        END IF
      END DO
!----
      END IF
!======================================================================
!======================================================================
      L001=VERGL(KEYWORD,'CHECKCOM')
      L002=VERGL(KEYWORD,'SHOWCOM')
      L003=VERGL(KEYWORD,'CHECKCOM+')
      IF (L001.OR.L002.OR.L003) & 
      THEN
!TEST       IF (VERGL(KEYWORD,'SHOWCOM')) PRISUM=.FALSE.
!       FICOM=FICOM+1
!       IF (FICOM.EQ.1) THEN
!       WRITE (scr,FMT='(5X,49A1)') ('-',I=1,19)
!       WRITE (35,FMT='(5X,49A1)') ('-',I=1,19)
!       END IF
       CALL TAXI(CH001,CH16)
       CALL TRANSL(CH16)
       CHKPH=CH16(1:16)
       CALL FUNTAXI(CH001,CHKEL,CHKELDIV,CHKELCH)
!
       CALL GELI(CH001,FF)
       CALL TAXI(CH001,CH16)
       CALL MINMAX(FF,CH16,CHKMIN,CHKMAX)
       
       CHKLIM=0
       IF (L003)THEN
!!       CALL GELI(CH001,FF2)
!!       CHKLIM=IDINT(FF2)
         FF2=(CHKMIN+CHKMAX)/2.0D0
         CALL GELI(CH001,CHKSTART)
         IF (CHKSTART.LE.FF2) CHKLIM=1
         IF (CHKSTART.GT.FF2) CHKLIM=-1
       END IF

!
       IF (NFEHL.EQ.-1) THEN
        CALL PRTFIRST
        CH002='     '//CHKPH//' check assemblage first <'
        CALL LABLA(CH002,I1)
        DO J=I1+1,129
         CH002(J:J)='-'
        END DO
        CALL PUST(scr,CH002)
        CALL PUST(35,CH002)
        GOTO 1
       END IF
       IF (NFEHL.GT.0) THEN
        CH002='     '//CHKPH//' assemblage not stable <'
        CALL LABLA(CH002,I1)
        DO J=I1+1,129
         CH002(J:J)='-'
        END DO
        CALL PUST(scr,CH002)
        CALL PUST(35,CH002)
        MCODE=1
!        GOTO 1
       END IF
       CCODE=0
!==
        KEY1='TC'
        CTUNIT='TC'
        CALL CHECKVALPT(TEXTQ,I001)

!-----q-summary

      CALL LABLA(CHKELCH,I1)
      I2=INDEX(EXPER,'  ')
      CALL LABLA(CHKPH,I4)
      CALL LABLA(TEXTQ,I5)
      CH002=' '
      WRITE (UNIT=CH002,FMT='(A,2X,A)') CHKPH(1:16),CHKELCH(1:8)
      CALL LABLA(CH002,I2)
      WRITE (39,2015) CH002(1:39),TEXTQ(1:I5)
 2015 FORMAT (15X,'value: ',2(2X,A))



        PRISUM=.TRUE.
!==
      END IF
!
!======================================================================
!======================================================================
      L001=VERGL(KEYWORD,'EXCH')
      L002=VERGL(KEYWORD,'CHECKKD')
      IF (L001.OR.L002) THEN
!       IF (NPLOTS.EQ.0) GOTO 1
!- read X-values
       CALL GELI(CH001,XCSTART)
       CALL GELI(CH001,FF)
       CALL TAXI(CH001,CH16)
       CALL MINMAX(FF,CH16,XCMIN,XCMAX)
!- read Y-values
       CALL GELI(CH001,YCSTART)
       CALL GELI(CH001,FF)
       CALL TAXI(CH001,CH16)
       CALL MINMAX(FF,CH16,YCMIN,YCMAX)
!--
       IF (NFEHL.EQ.-1) THEN
       CALL PRTFIRST
        CH002='     '//CHKPH//' check assemblage first <'
        CALL LABLA(CH002,I1)
        DO J=I1+1,129
         CH002(J:J)='-'
        END DO
        CALL PUST(scr,CH002)
        CALL PUST(35,CH002)
        GOTO 1
       END IF
       IF (NFEHL.GT.0) THEN
        CH002='     '//CHKPH//' assemblage not stable <'
        CALL LABLA(CH002,I1)
        DO J=I1+1,129
         CH002(J:J)='-'
        END DO
        CALL PUST(scr,CH002)
        CALL PUST(35,CH002)
        MCODE=1
!        GOTO 1
       END IF
!--
       F1=PLYMIN
       F2=PLYMAX
       CALL CHECKEXCH
!--
      END IF
!======================================================================
!======================================================================
      IF (VERGL(KEYWORD,'CHECKTAB')) THEN
      CALL CHECKENO

      CALL CHECKTAB(CH001)
      CH001=' '
      GOTO 1
      END IF
!======================================================================
!======================================================================
!----------------------------------------------------
!     print concentration of one element of one phase.
!     input: PRTVAL:   phasename    element
!            PRTVAL    phasename    element/value
!
      IF (VERGL(KEYWORD,'PRTVAL')) THEN
       FICOM=FICOM+1
       IF (FICOM.EQ.1) THEN
       WRITE (scr,FMT='(5X,49A1)') ('-',I=1,19)
       WRITE (35,FMT='(5X,49A1)') ('-',I=1,19)
       END IF
       CALL TAXI(CH001,CH16)
       CALL TRANSL(CH16)
       CHKPH=CH16(1:16)
       CALL FUNTAXI(CH001,CHKEL,CHKELDIV,CHKELCH)
!--
       CALL PRTVAL
      END IF
!======================================================================
!======================================================================
      L001=VERGL(KEYWORD,'TP')
      L002=VERGL(KEYWORD,'PT')
      IF (L001.OR.L002) THEN
      
       CALL GELI(CH001,FF)
       CALL TAXI(CH001,CH16)
       CALL MINMAX(FF,CH16,F1,F2)
       TEM=(F1+F2)/2.0D0
       TERR0=DABS(F1-F2)/2.0D0
       
       CALL GELI(CH001,FF)
       CALL TAXI(CH001,CH16)
       CALL MINMAX(FF,CH16,F1,F2)
       PRE=(F1+F2)/2.0D0
       PERR0=DABS(F1-F2)/2.0D0

       IF (TEM.LT.TSHMIN) TSHMIN=TEM
       IF (TEM.GT.TSHMAX) TSHMAX=TEM
       IF (PRE.LT.PSHMIN) PSHMIN=PRE
       IF (PRE.GT.PSHMAX) PSHMAX=PRE

!====
!!       CALL GELI(CH001,TERR0)
!       CALL TAXI(CH001,CH16)
!       CALL LABLA(CH16,I1)
!       IF (I1.EQ.0) I1=1
!       IF (CH16(I1:I1).EQ.'%') THEN
!        CH16(I1:I1)=' '
!        CALL GELI(CH16,FF)
!        TERR0=FF*TEM/100.0D0
!       ELSE
!        CALL GELI(CH16,TERR0)
!       END IF
!!
!!       CALL GELI(CH001,PERR0)
!       CALL TAXI(CH001,CH16)
!       CALL LABLA(CH16,I1)
!       IF (I1.EQ.0) I1=1
!       IF (CH16(I1:I1).EQ.'%') THEN
!        CH16(I1:I1)=' '
!        CALL GELI(CH16,FF)
!        PERR0=FF*PRE/100.0D0
!       ELSE
!        CALL GELI(CH16,PERR0)
!       END IF
!====
       IF (DABS(TERR0).LT.1D-12) TERR0=10.0D0
       IF (DABS(PERR0).LT.1D-12) PERR0=20.0D0
      END IF
!======================================================================
!======================================================================
      GOTO 1
!--
!  998 CONTINUE
!      WRITE (UNIT=6,FMT='(/,'' START not found'')')
  999 CONTINUE
      CALL MAKESHORT
!===
       IF (NPLOTS.GT.0) THEN
         CALL MAKEPLOT
       END IF
!===

      WRITE (UNIT=6,FMT='('' now closing 35'')')

      CLOSE (UNIT=35)
!!      CLOSE (UNIT=36)
!!      CLOSE (UNIT=51)
!!      CLOSE (UNIT=37)
!!      CLOSE (UNIT=38)
      CLOSE (UNIT=39)
      CLOSE (UNIT=55)
!===
      RETURN
      END
!
!-----
!*************************************************************
!*************************************************************
      SUBROUTINE MAKESHORT
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
      include 'checkdb.cmn'
!-----END OF COMMON VARIABLES
      REAL(8) PRZ
!
      IF (PRTEST) WRITE (UNIT=6,FMT='(''->MAKESHORT'')')
      WRITE (UNIT=6,FMT='('' NEXP    ='',I4)') NEXP
      CALL PUST(6,' REAID   = '//REAID)
      CALL PUST(6,' NICREAC = '//NICREAC)
      CALL PUST(6,' REFER   = '//REFER)
!      WRITE (UNIT=6,FMT='(''NINC='',I4)') NINC
!      WRITE (UNIT=6,FMT='(''TSHMIN='',E20.10)') TSHMIN
!      WRITE (UNIT=6,FMT='(''TSHMAX='',E20.10)') TSHMAX
!      WRITE (UNIT=6,FMT='(''PSHMIN='',E20.10)') PSHMIN
!      WRITE (UNIT=6,FMT='(''PSHMAX='',E20.10)') PSHMAX

      IF (ISINC.EQ.1) NINC=NINC+1
!!         WRITE (UNIT=39,FMT='(''ISINC='',I3)') ISINC
!!         WRITE (UNIT=39,FMT='(''NINC= '',I3)') NINC

      IF (NEXP.GT.0) THEN
        PRZ=100.0D0*REAL(NEXP-NINC)/REAL(NEXP)
!        WRITE (38,1000) REAID,NICREAC,REFER,TSHMIN,TSHMAX,PSHMIN,PSHMAX,NEXP,PRZ
! 1000   FORMAT(A6,2X,A20,2X,A30,2X,F5.0,' - ',F5.0,2X,F7.0,' - ',F7.0,2X,I4,2X,F6.2,' %')
        WRITE (39,1010) NEXP,(NEXP-NINC),PRZ
 1010   FORMAT (/,3X,'summary: ',2X,'number of experiments:',I4,'   consistent:', &
        I4,11X,'(',F6.2,' %)')
      END IF

      NINC=0
      ISINC=0
      TSHMIN=1D20
      TSHMAX=-1D20
      PSHMIN=1D20
      PSHMAX=-1D20
!===
      RETURN
      END
!
!-----
!*************************************************************
!*************************************************************
      SUBROUTINE MAKEPLOT
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
      include 'checkdb.cmn'
!-----END OF COMMON VARIABLES
      INTEGER(4) I,IK,I1,j,ierr
      CHARACTER(16) DELRM
      CHARACTER(500) CH500
      LOGICAL(4) DOREM
!
!!      IF (PRTEST) WRITE (UNIT=6,FMT='(''-> MAKEPLOT'',I4)') NPLOTS
!!      IF (PRTEST) WRITE (UNIT=35,FMT='(''-> MAKEPLOT'',I4)') NPLOTS
!-- set DOREM to false to NOT remove files *_com etc.
      DOREM=.TRUE.
!
!===  because of microsoft serfs, the dbs file (database) has to be closed
!===  before the system call .._job with DOMINO or THERIAK
      CLOSE (UNIT=dbs)
!===
!
      DO I=1,NPLOTS
       CALL LABLA(AUTOPLOT(I),I1)
       CALL SYSTEM(NEWDIR(1:LDIR)//AUTOPLOT(I)(1:I1)//'_job')
!!       CALL EXECUTE_COMMAND_LINE(AUTOPLOT(I)(1:I1)//'_job',WAIT=.TRUE.)
      END DO
!------------------
!     open UNIT=dbs
!------------------
      j=dbs
      line=DBNAME
      path=wpath
      akzess=' '
      state='old'
      call openfile(j,ierr)
      if(ierr.ne.0)  CALL EXIT
!===
!===
      DO I=1,NPLOTS
      CALL LABLA(AUTOPLOT(I),I1)
      WRITE (6,2000) AUTOPLOT(I)(1:I1)//'_xxx'
 2000 FORMAT (' files to be deleted: ',A)
      END DO
!===
!===
      CALL RMCOMMAND(DELRM)
      CALL LABLA(DELRM,IK)

      IF (DOREM) THEN
      DO I=1,NPLOTS
!      CALL LABLA(AUTOPLOT(I),I1)
      CH500=NEWDIR(1:LDIR)//AUTOPLOT(I)
      CALL LABLA(CH500,I1)
      IF (FDELETE(1)) CALL SYSTEM(DELRM(1:IK)//' '//CH500(1:I1)//'_exn')
      IF (FDELETE(2)) CALL SYSTEM(DELRM(1:IK)//' '//CH500(1:I1)//'_exn2')
      IF (FDELETE(3)) CALL SYSTEM(DELRM(1:IK)//' '//CH500(1:I1)//'_0exp')
      IF (FDELETE(4)) CALL SYSTEM(DELRM(1:IK)//' '//CH500(1:I1)//'_0exp.ps')
      IF (FDELETE(5)) CALL SYSTEM(DELRM(1:IK)//' '//CH500(1:I1)//'_0exp.svg')
      IF (FDELETE(6)) CALL SYSTEM(DELRM(1:IK)//' '//CH500(1:I1)//'_com')
      IF (FDELETE(7)) CALL SYSTEM(DELRM(1:IK)//' '//CH500(1:I1)//'_com.ps')
      IF (FDELETE(8)) CALL SYSTEM(DELRM(1:IK)//' '//CH500(1:I1)//'_com.svg')
      IF (FDELETE(9)) CALL SYSTEM(DELRM(1:IK)//' '//CH500(1:I1)//'_com2')
      IF (FDELETE(10)) CALL SYSTEM(DELRM(1:IK)//' '//CH500(1:I1)//'_com2.ps')
      IF (FDELETE(11)) CALL SYSTEM(DELRM(1:IK)//' '//CH500(1:I1)//'_com2.svg')
      IF (FDELETE(12)) CALL SYSTEM(DELRM(1:IK)//' '//CH500(1:I1)//'_job')
      IF (FDELETE(13)) CALL SYSTEM(DELRM(1:IK)//' '//CH500(1:I1)//'.txt')
      IF (FDELETE(14)) CALL SYSTEM(DELRM(1:IK)//' '//CH500(1:I1)//'.cln')
      IF (FDELETE(15)) CALL SYSTEM(DELRM(1:IK)//' '//CH500(1:I1)//'.plt')
      IF (FDELETE(16)) CALL SYSTEM(DELRM(1:IK)//' '//CH500(1:I1)//'.rxn')
      IF (FDELETE(17)) CALL SYSTEM(DELRM(1:IK)//' '//CH500(1:I1)//'_fun_loop')
      IF (FDELETE(18)) CALL SYSTEM(DELRM(1:IK)//' '//CH500(1:I1)//'_bin_loop')
      IF (FDELETE(19)) CALL SYSTEM(DELRM(1:IK)//' '//CH500(1:I1)//'_rea_loop')
      END DO
      END IF
!



      END
!-----
!*************************************************************
!*************************************************************
      SUBROUTINE CHECKASS(NFEHL)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
      include 'checkdb.cmn'
!
!-----END OF COMMON VARIABLES
      CHARACTER(250) CH001,CHTAG(10),PHASTR,PHASTR0,LISTCOM,PLUSCOM, &
      MINUSCOM
      CHARACTER(250) ASSTEXT
      CHARACTER(40) CH40
      CHARACTER(32) TEXT
      INTEGER(4) IP,IM,I,NFEHL,NPLUS,NMINUS,I1,I2,J,DURCH,COMAY,NTAG, &
      IS,NP,I3,NNPICK,IFKEY,EINS,ALLES,ISSTAB
      REAL(8) FF
!=====
      IF (PRTEST) WRITE (UNIT=6,FMT='(''-> CHECKASS nfehl ='',I4)')NFEHL
      IF (PRTEST) WRITE (UNIT=35,FMT='(''-> CHECKASS nfehl ='',I4)')NFEHL
!
      CTUNIT='TC'
      IF (BUFORMUL.EQ.' ') THEN
      CALL PRTFIRST
!
      CH001=' '
      WRITE (CH001,3002)
 3002 FORMAT (5X,' Bulk missing <')
      CALL LABLA(CH001,I1)
      DO J=I1+1,120
       CH001(J:J)='-'
      END DO
      CALL PUST(scr,CH001)
      CALL PUST(35,CH001)
      RETURN
      END IF
!=====
      COMAY=COMAX
      EINS=1
      NTAG=0
      NP=0
      NNPICK=NPICK
      ISSTAB=0
!=====
      DO DURCH=1,2
!----D1 bis D1, nur im 1. Durchgang
!======================
!==== DURCH=1, was sowieso gemacht werden muss
!======================
      IF (DURCH.EQ.1) THEN
      FORMUL=BUFORMUL
      USE=BULKUSE
      CALL CHEMIE(COMAY,NC,OXYDE,OXANZ,FORMUL,CHEM)
      CALL LABLA(USE,LUSE)
      NPICK=0
      ALLP=.FALSE.
      CALL DBREAD
!+++
      PHASTR='     '
      PHASTR0='     reduced database:'
      DO J=1,NCHOOSE
      CALL LABLA(PHASTR,I2)
      IF (J.EQ.1) THEN
      PHASTR(I2+3:)=CHOOSE(J)
      ELSE
      PHASTR(I2+2:)=CHOOSE(J)
      END IF
      END DO
      DO J=1,NCHOOSE0
      CALL LABLA(PHASTR0,I2)
      IF (J.EQ.1) THEN
      PHASTR0(I2+3:)=CHOOSE0(J)
      ELSE
      PHASTR0(I2+2:)=CHOOSE0(J)
      END IF
      END DO
!+++
      IF (NCHOOSE.GT.0) THEN
       DO I=1,NCHOOSE
        I1=0
        DO IP=1,NPHA
         IF (VERGL(NAME(IP),CHOOSE(I))) THEN
          IF (NULL(IP)) THEN 
           I1=-1
          ELSE
           I1=1
          END IF
         END IF
        END DO
        IF (I1.EQ.0) THEN
         DO IS=1,NSOL
          IF (VERGL(SOLNAM(IS),CHOOSE(I))) THEN
           IF (EXSOL(IS)) THEN
            I1=-1
           ELSE
            I1=1
           END IF
          END IF
         END DO
        END IF
!==
      IF (I1.EQ.0) THEN
      CALL LABLA(CHOOSE(I),I2)
      CALL PRTFIRST
!
      CH001=' '
      WRITE (CH001,3012) CHOOSE(I)(1:I2)
 3012 FORMAT (5X, A,' .')
      CALL LABLA(CH001,I1)
      DO J=I1+1,107
       CH001(J:J)='.'
      END DO
      CH001(109:)='phase not in database'
      CALL PUST(scr,CH001)
      CALL PUST(35,CH001)
      J=0
      DO I1=1,NPHNOT
       IF (VERGL(CHOOSE(I),PHNOT(I1))) J=I1
      END DO
      IF (J.EQ.0) THEN
       NPHNOT=NPHNOT+1
       PHNOT(NPHNOT)=CHOOSE(I)
!       CALL PUST(51,CH001)
      END IF
      RETURN
      END IF
!==
      IF (I1.EQ.-1) THEN
      CALL LABLA(CHOOSE(I),I2)
      NTAG=NTAG+1
      CHTAG(NTAG)=' '
      WRITE (CHTAG(NTAG),3022) CHOOSE(I)(1:I2)
 3022 FORMAT (5X,'Warning: phase ',A,' is excluded in database ')
      CALL LABLA(CHTAG(NTAG),I1)
!      DO J=I1+1,130
!       CHTAG(NTAG)(J:J)='.'
!      END DO
      END IF
!==
       END DO
      END IF
!==
      TC=TEM
      P=PRE
      CALL NURVONPT
      CALL MACHACT
      CALL CALSTR
      CALL THERIA
      NFEHL=0
!D1------
      END IF
!=====================
!==== DURCH=2
!=====================
      IF (DURCH.EQ.2) THEN
!       IF (NCHOOSE.EQ.0.OR. &
!       (NFEHL.EQ.0.AND.NP.EQ.0.AND.SHOW.EQ.0)) RETURN
      IF (NCHOOSE.EQ.0) RETURN
       NPICK=NCHOOSE
       DO I=1,NCHOOSE
        PICK(I)=CHOOSE(I)
       END DO
!---- to print assemblage etc. (nur im 2. Durchgang)
      IF (SHOW.EQ.1) THEN
       PRTLOG(3)=.TRUE.
       PRTLOG(6)=.TRUE.
       PRTLOG(8)=.TRUE.
!       PRTLOG(4)=.TRUE.
       DRU=.TRUE.
      END IF
!=====================
!==== DURCH=1 und 2
!=====================
      ALLP=.FALSE.
      CALL DBREAD
      TC=TEM
      P=PRE
      CALL NURVONPT
      CALL MACHACT
      CALL CALSTR
      CALL PRININ
      IF (.NOT.GIBBSFLAG) THEN
        ALLES=NSOL
        CALL GIBBSTEST(EINS,ALLES)
      END IF
      CALL THERIA
!---- to stop print assemblage etc.
      IF (SHOW.EQ.1) THEN
       DO I=1,11
        PRTLOG(I)=.FALSE.
       END DO
       DRU=.FALSE.
      END IF
      END IF
!====
      ASSTEXT=' '
      DO I=1,NUN2
       I1=I
       CALL GETNAME(I1,IS,TEXT)
       FF=NN(I)
       CH40=' '
       WRITE (UNIT=CH40,FMT='(F10.3)') NN(I)
       CALL FIBLA(CH40,I2)
       IF (I2.EQ.0) I2=1
       CALL LABLA(CH40,I3)
       IF (I3.EQ.0) I3=1
       CALL LABLA(ASSTEXT,I1)
       IF (I.EQ.1) THEN
       ASSTEXT(I1+2:)=CH40(I2:I3)//' '//TEXT
       ELSE
       ASSTEXT(I1+2:)='+ '//CH40(I2:I3)//' '//TEXT
       END IF
      END DO
!==
      NFEHL=0
      LISTCOM=' '
      PLUSCOM=' '
      MINUSCOM=' '
!-------------------------------------------------
!---- check if all stable phases are from the list
!-------------------------------------------------
!      DO IP=1,NCHOOSE0
!       WRITE (UNIT=6,FMT='('' list: '',A)') CHOOSE0(IP)
!      END DO
      NP=0
      DO I=1,NUN2
       I1=I
       CALL GETNAME(I1,IS,TEXT)
       I1=0
       DO IP=1,NCHOOSE0
        IF (VERGL(TEXT,CHOOSE0(IP))) THEN
         I1=I1+1
        END IF
       END DO
       IF (I1.EQ.0) THEN
        NP=NP+1
        CALL LABLA(LISTCOM,I3)
        LISTCOM(I3+2:)=TEXT
       END IF
      END DO
!-----------------------------------------------
!---- check if an EXPEC phase is missing
!-----------------------------------------------
!      DO IP=1,NPASS
!       WRITE (UNIT=6,FMT='('' plus: '',A)') PASS(IP)
!      END DO
      DO IP=1,NPASS
      NPLUS=0
      IPASS(IP)=0
      DO I=1,NUN2
       I1=I
       CALL GETNAME(I1,IS,TEXT)
       IF (VERGL(TEXT,PASS(IP))) THEN
        NPLUS=NPLUS+1
        IPASS(IP)=I
       END IF
      END DO
!!!!!!!!!!       IF (NPLUS.NE.PMULT(IP)) THEN
       IF (NPLUS.EQ.0.AND.PMULT(IP).GT.0) THEN
       NFEHL=NFEHL+1
        CALL LABLA(PLUSCOM,I3)
        PLUSCOM(I3+2:)=PASS(IP)
       END IF
      END DO
!-----------------------------------------------
!---- check if a not EXPEC phase is present
!-----------------------------------------------
!      DO IM=1,NMASS
!       WRITE (UNIT=6,FMT='('' minus: '',A)') MASS(IM)
!      END DO
      DO IM=1,NMASS
      NMINUS=0
      DO I=1,NUN2
       I1=I
       CALL GETNAME(I1,IS,TEXT)
       IF (VERGL(TEXT,MASS(IM))) NMINUS=NMINUS+1
      END DO
       IF (NMINUS.NE.0) THEN
       NFEHL=NFEHL+1
        CALL LABLA(MINUSCOM,I3)
        MINUSCOM(I3+2:)=MASS(IM)
       END IF
      END DO
!====
!======================
!==== DURCH=1
!======================
      IF (DURCH.EQ.1) THEN
      CALL PRTFIRST
!
!      IF (NPLOTS.GT.0) THEN
!      CALL LABLA(AUTOPLOT(NPLOTS),I1)
!      CH001='plotfile: '//AUTOPLOT(NPLOTS)(1:I1)//'  typ: '//PLOTTYP
!      CALL PUST(scr,CH001)
!      CALL PUST(35,CH001)
!      END IF
      DO I=1,NTAG
       CALL FIBLA(CHTAG(I),I1)
       CALL PUST(scr,CHTAG(I)(I1:))
       CALL PUST(35,CHTAG(I)(I1:))
      END DO
      CH001='bulk: '//BUFORMUL(1:244)
      CALL PUSTCOL(scr,'35:'//CH001,6,129)
      CALL PUSTCOL(35,CH001,6,129)
!      CH001='     database used: '//filename(dbs)
!      CALL PUST(scr,CH001)
!      CALL PUST(35,CH001)
      CH001=' '
      CALL LABLA(filename(dbs),I1)
      CALL LABLA(ASSTEXT,I2)
      WRITE (CH001,1002) TC,P,filename(dbs)(1:I1)
 1002 FORMAT (5X,'TC =',F8.2,'  P =',F9.2,5X, &
      'database  :  ',A)
      CALL PUST(scr,CH001)
      CALL PUST(35,CH001)
      CH001='assemblage: '//ASSTEXT(1:I2)
      CALL LABLA(CH001,I1)
      WRITE (scr,FMT='(5X,''+-'',2X,F8.2,5X,F9.2,5X,A)') &
      TERR0,PERR0,CH001(1:I1)
      WRITE (35,FMT='(5X,''+-'',2X,F8.2,5X,F9.2,5X,A)') &
      TERR0,PERR0,CH001(1:I1)
       WRITE (scr,FMT='(5X,26A1)') ('-',I=1,26)
       WRITE (35,FMT='(5X,26A1)') ('-',I=1,26)
      CALL LABLA(EXPEC,I1)
      WRITE (CH001,FMT='(5X,A)') 'check: '//EXPEC(1:I1)//' .'
      CALL LABLA(CH001,I1)
!-----------------------------------------------
!---- assemblage check for complete database
!-----------------------------------------------
      IF (NFEHL.GT.0) THEN
      DO J=I1+1,107
       CH001(J:J)='.'
      END DO
      CH001(109:)='assemblage not stable'
      MCODE=1
      ELSE
       CH001(I1:)='(OK)'
      END IF
      CALL PUST(scr,CH001)
      CALL PUST(35,CH001)
!--
      IF (LISTCOM.NE.' ') THEN
       CH001='     unexpected : '//LISTCOM(1:232)
       CALL PUST(scr,CH001)
       CALL PUST(35,CH001)
      END IF
      IF (PLUSCOM.NE.' ') THEN
       CH001='     should be stable : '//PLUSCOM(1:226)
       CALL PUST(scr,CH001)
       CALL PUST(35,CH001)
      END IF
      IF (MINUSCOM.NE.' ') THEN
       CH001='     should not be stable : '//MINUSCOM(1:222)
       CALL PUST(scr,CH001)
       CALL PUST(35,CH001)
      END IF
!--
      CALL PUST(scr,PHASTR0)
      CALL PUST(35,PHASTR0)
!-----------------------------------------------
!---- assemblage check OK for complete database
!-----------------------------------------------
      IF (NFEHL.EQ.0.AND.LISTCOM.EQ.' ') THEN
      CH001=' '
      WRITE (CH001,FMT='(5X,A)') 'assemblage is stable (OK)'
      CALL PUST(scr,CH001)
      CALL PUST(35,CH001)
      ISSTAB=1
      CH001=' '
      CALL LABLA(EXPER,I1)
      IF (I1.LT.10) I1=10
      IF (PEXP.EQ.0) THEN
        WRITE (39,1007) EXPER(1:I1),TEM,PRE
 1007    FORMAT (/,3X,A10,2X,'assemblage:  T = ',F8.1,'  P =',F10.1,10X,'OK(stable)')
      ELSE
        WRITE (39,1008) BLAN(1:I1),TEM,PRE
 1008   FORMAT (3X,A10,2X,'assemblage:  T = ',F8.1,'  P =',F10.1,10X,'OK(stable)')
      END IF
      PEXP=PEXP+1
      END IF
      END IF
!====
!=====================
!==== DURCH=2
!=====================
      IF (DURCH.EQ.2) THEN
      CH001=' '
      CALL LABLA(ASSTEXT,I1)
      WRITE (CH001,1012) ASSTEXT(1:I1)
 1012 FORMAT (5X,'reduced assemblage: ',A)
      CALL PUST(scr,CH001)
      CALL PUST(35,CH001)
!-----------------------------------------------
!---- assemblage check OK for reduced database
!-----------------------------------------------
      IF (NFEHL.EQ.0.AND.ISSTAB.EQ.0) THEN
      CH001=' '
      WRITE (CH001,FMT='(5X,A)') 'assemblage is metastable (OK)'
      CALL PUST(scr,CH001)
      CALL PUST(35,CH001)
!cdcmay2021
      MCODE=0

      CH001=' '
      CALL LABLA(EXPER,I1)
      IF (I1.LT.10) I1=10
      IF (PEXP.EQ.0) THEN
        WRITE (39,1015) EXPER(1:I1),TEM,PRE
 1015   FORMAT (/,3X,A10,2X,'assemblage:  T = ',F8.1,'  P =',F10.1,10X,'OK(metastable)')
      ELSE
        WRITE (39,1016) BLAN(1:I1),TEM,PRE
 1016   FORMAT (3X,A10,2X,'assemblage:  T = ',F8.1,'  P =',F10.1,10X,'OK(metastable)')
      END IF
      PEXP=PEXP+1

      END IF
      END IF
!=====================
!==== DURCH=2 und NFEHL>0
!=====================
!-----------------------------------------------
!---- if NFEHL>0, print G of unstable phases (unstable: may be dG=0!!)
!-----------------------------------------------
      IF (DURCH.EQ.2.AND.NFEHL.GT.0) THEN
      WRITE (UNIT=scr,FMT='('' '')')
      WRITE (UNIT=35,FMT='('' '')')
      DO I=NUN2+1,NMAX
       I1=I
       CALL GETNAME(I1,IS,TEXT)
!
!       IF ((IS.EQ.0.AND.SUGG(I).GT.0.AND.EMSOL(NUMMER(I)).EQ.0).OR. &
!       (IS.GT.0.AND.SUGG(I).EQ.MINISUG(IS))) THEN
!
        IFKEY=0
        IF (IS.EQ.0) THEN
         IF (SUGG(I).GT.0.AND.EMSOL(NUMMER(I)).EQ.0) IFKEY=1
        ELSE
         IF (SUGG(I).EQ.MINISUG(IS)) IFKEY=1
        END IF
!
       IF (IFKEY.EQ.1) THEN
        IF (DABS(G(I)).GT.-1D-5) THEN
        FF=0.0D0
        DO J=1,NUN
         FF=FF+X(I,J)
        END DO
        IF (DABS(G(I)).GT.1D-5) THEN
        WRITE (scr,2000) TEXT,G(I),G(I)/FF
        WRITE (35,2000) TEXT,G(I),G(I)/FF
 2000   FORMAT (5X,A,'G = ',1PE12.5,3X,' G/atom = ',1PE12.5)
        ELSE
        WRITE (scr,2002) TEXT,G(I)
        WRITE (35,2002) TEXT,G(I)
 2002   FORMAT (5X,A,'G = ',1PE12.5)
        END IF
        END IF
       END IF
      END DO
!----------------------------------------------------------
!---- keep reduced database and search for assemblage in PT
!---- or search +/- err in P-T-XCO2
!----------------------------------------------------------
      IF (DABS(XCO2).LT.1D-12) CALL SEARCHASS(NFEHL)
      IF (DABS(XCO2).GT.1D-12) CALL SEARCHPTX(NFEHL)
      END IF
!=====DURCH
      END DO


!==
!      DO I=1,NPLOTS
!        IF (AUTOTYP(I).EQ.'PT') THEN
!          PLNR=I
!          CALL PLOTEXP(PLNR)
!        END IF
!        IF (AUTOTYP(I).EQ.'TXC') THEN
!          PLNR=I
!          CALL PLOTEXP(PLNR)
!        END IF
!      END DO
!==
      RETURN
      END
!
!-----
!*************************************************************
!*************************************************************
      SUBROUTINE CHECKENO
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
      include 'checkdb.cmn'
      LOGICAL(4) L001
      INTEGER(4) I
!
      L001=.FALSE.
      DO I=1,4
        IF (ENO(I).EQ.0) L001=.TRUE.
      END DO
      IF (L001) THEN
        IF (ENO(1).EQ.0) WRITE (UNIT=6,FMT='('' eno: missing REAC'')') 
        IF (ENO(2).EQ.0) WRITE (UNIT=6,FMT='('' eno: missing REAID'')') 
        IF (ENO(3).EQ.0) WRITE (UNIT=6,FMT='('' eno: missing PHASES'')') 
        IF (ENO(4).EQ.0) WRITE (UNIT=6,FMT='('' eno: missing SYSEL'')')
        CALL EXIT
      END IF
!
!==
      RETURN
      END
!
!-----
!*************************************************************
!*************************************************************
      SUBROUTINE STABILITY(CH16,CH1)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
      include 'checkdb.cmn'
!
!-----END OF COMMON VARIABLES
      CHARACTER(250) ASSTEXT
      CHARACTER(130) CH001,TEX
      CHARACTER(40) CH40,TEXT
      CHARACTER(16) CH16
      CHARACTER(1) CH1
      INTEGER(4) I,IP,IS,II,J,I1,I2,I3
      REAL(8) WERT,FS,FF
!
      IF (PRTEST) WRITE (UNIT=6,FMT='(''-> STABILITY '',A,1X,A)') CH16,CH1
      IF (PRTEST) WRITE (UNIT=35,FMT='(''-> STABILITY '',A,1X,A)') CH16,CH1
!
      NPICK=NCHOOSE
      DO I=1,NCHOOSE
       PICK(I)=CHOOSE(I)
      END DO
      CALL DBREAD
      WERT=0.0D0
!===
!==== exclude phase CH16
      CH001=' '
      DO J=6,31
      CH001(J:J)='-'
      END DO
      CALL PUST(6,CH001)
      CALL PUST(35,CH001)
      IF (CH1.EQ.'+') THEN
        TEX='(stable, G < 0)'
        FS=-1.0D0
      ELSE
        TEX='(unstable, G > 0)'
        FS=1.0D0
      END IF
      CALL LABLA(CH16,I)
      CALL LABLA(TEX,J)
      WRITE (6,1010) CH16(1:I),TEX(1:J)
 1010 FORMAT (6X,'check stability of: ',A,1X,A)
!
      DO IP=1,NPHA
        IF (NAME(IP).EQ.CH16) THEN
          PEXCL=PEXCL+1
          NULL(IP)=.TRUE.
          NAME(IP)='$'//CH16(1:15)
        END IF
      END DO
      DO IS=1,NSOL
        IF (SOLNAM(IS).EQ.CH16) THEN
          SEXCL=SEXCL+1
          EXSOL(IS)=.TRUE.
          DO II=1,NEND(IS)
            NULL(EM(IS,II))=.TRUE.
          END DO
        END IF
      END DO
!===
      TC=TEM
      P=PRE
      IF (SHOW.EQ.1) THEN
       PRTLOG(3)=.TRUE.
       PRTLOG(6)=.TRUE.
       PRTLOG(8)=.TRUE.
!       PRTLOG(4)=.TRUE.
       DRU=.TRUE.
      END IF
        CALL NURVONPT
        CALL CALSTR
        CALL THERIA

       PRTLOG(3)=.FALSE.
       PRTLOG(6)=.FALSE.
       PRTLOG(8)=.FALSE.
!       PRTLOG(4)=.FALSE.
       DRU=.FALSE.
       SHOW=0
!---- no need to set back, because database will be read for next exp. ?
!---- set back in theriminos in GETF
!====
      ASSTEXT=' '
      DO I=1,NUN2
       I1=I
       CALL GETNAME(I1,IS,TEXT)
       FF=NN(I)
       CH40=' '
       WRITE (UNIT=CH40,FMT='(F10.3)') NN(I)
       CALL FIBLA(CH40,I2)
       IF (I2.EQ.0) I2=1
       CALL LABLA(CH40,I3)
       IF (I3.EQ.0) I3=1
       CALL LABLA(ASSTEXT,I1)
       IF (I.EQ.1) THEN
       ASSTEXT(I1+2:)=CH40(I2:I3)//' '//TEXT
       ELSE
       ASSTEXT(I1+2:)='+ '//CH40(I2:I3)//' '//TEXT
       END IF
      END DO
!==
      CALL LABLA(ASSTEXT,I1)
      CALL LABLA(CH16,I2)
      WRITE (6,1070) CH16(1:I2),ASSTEXT(1:I1)
 1070 FORMAT (6X,'assemblage with ',A,' excluded: ',A)
!==
!----- solution phases
      IF (SEXCL.GT.0) THEN
      DO I=1,NMAX
       IF (NUMMER(I).EQ.0) THEN
        IS=EMCODE(I)
        IF (EXSOL(IS).AND.SUGG(I).EQ.MINISUG(IS)) THEN
          WERT=G(I)
          IF (WERT*FS.LT.0.0D0) THEN
            TEX=' '
            DO J=2,84
              TEX(J:J)='.'
            END DO
            IF (WERT.LT.0.AND.FS.GT.0) THEN
            TEX(86:)='should be positive'
            ELSE
            TEX(86:)='should be negative'
            END IF
          ELSE
            TEX='(OK)'
          END IF
          CALL LABLA(TEX,J)
      WRITE (6,1050) WERT,TEX(1:J)
 1050 FORMAT (6X,'G = ',F15.4,2X,A)
        END IF
       END IF
      END DO
      END IF
!----- non-solution phases
      IF (PEXCL.GT.0) THEN
      DO I=1,NMAX
       IF (NUMMER(I).NE.0) THEN
        IF (NAME(NUMMER(I))(1:1).EQ.'$') THEN
!         WRITE (UNIT=scr,FMT='('' '')')
!         WRITE (scr,162) NUMMER(I),EMCODE(I), &
!                NAME(NUMMER(I)),G(I),VV(I)*10.0D0
!                NAME(NUMMER(I)),G(I),VV(I)*10.0D0
          WERT=G(I)
          IF (WERT*FS.LT.0.0D0) THEN
            TEX=' '
            DO J=2,84
              TEX(J:J)='.'
            END DO
            IF (WERT.LT.0.AND.FS.GT.0) THEN
            TEX(86:)='should be positive'
            ELSE
            TEX(86:)='should be negative'
            END IF
          ELSE
            TEX='(OK)'
          END IF
          CALL LABLA(TEX,J)
      WRITE (6,1060) WERT,TEX(1:J)
 1060 FORMAT (6X,'G = ',F15.4,2X,A)
!-----
!         IP=I
!         CALL MINIREA(IP)
!-----
        END IF
       END IF
      END DO
      END IF
!==
      RETURN
      END
!--
!-----
!*************************************************************
!*************************************************************
      SUBROUTINE CHECKEXCH
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
      include 'checkdb.cmn'
!
!-----END OF COMMON VARIABLES
      CHARACTER(250) CH001,TEXT1,TEXT2
      CHARACTER(80) CHE2,CHE1,CROSSES,TEXTQ,TEXTQ1,TEXTQ2
      CHARACTER(32) CH16
      CHARACTER(7) EBAR1,EBAR2
      CHARACTER(4) V1
      INTEGER(4) IP,IE,IS,I1,I2,I3,I4,I5,I,J, &
      ITT,IPP,IC,BARSHIFT,FEA,FEA1,BPOS,PLNR,INKD,CCODE1
      REAL(8) F1,F2,TOFMIN,POFMIN,TOFMAX,POFMAX, &
      FF,PROZ,MAXPROZ,PTPROZ,WERTMIN,WERTMAX, &
      WERT,XAMIN,XAMAX,YAMIN,YAMAX
!--
      IF (PRTEST) WRITE (UNIT=6,FMT='(''-> exchange reaction (CHECKEXCH)'')')
      IF (PRTEST) WRITE (UNIT=35,FMT='(''-> exchange reaction (CHECKEXCH)'')')
      CTUNIT='TC'
!+++
!      IF (PRTEST) THEN
!      WRITE (UNIT=6,FMT= &
!      '('' checking exchange reaction (CHECKEXCH)'')')
!      END IF
!+++
      CCODE1=CCODE
      INKD=0
! ignore if unstable assembage
      ISINC=0
      MCODE=0
!====
      KDMIN=(XCMIN/(1.0D0-XCMIN))/(YCMAX/(1.0D0-YCMAX))
      KDMAX=(XCMAX/(1.0D0-XCMAX))/(YCMIN/(1.0D0-YCMIN))

      IF (DABS(XCSTART).LT.1D-12.AND.DABS(YCSTART).LT.1D-12) THEN
        KD0=(KDMIN+KDMAX)/2.0D0
        CHKLIM=0
        XCSTART=(XCMIN+XCMAX)/2.0D0
        YCSTART=(YCMIN+YCMAX)/2.0D0
      ELSE
        IF(XCSTART.LT.0.000001D0) XCSTART=0.000001D0
        IF(XCSTART.GT.0.999999D0) XCSTART=0.999999D0
        IF(YCSTART.LT.0.000001D0) YCSTART=0.000001D0
        IF(YCSTART.GT.0.999999D0) YCSTART=0.999999D0
        KD0=(XCSTART/(1.0D0-XCSTART))/(YCSTART/(1.0D0-YCSTART))
        F1=(KDMIN+KDMAX)/2.0D0
        IF (KD0.GT.F1) CHKLIM=1
        IF (KD0.LT.F1) CHKLIM=-1
      END IF
!====
      XAMIN=1D20
      XAMAX=-1D20
      YAMIN=1D20
      YAMAX=-1D20
      WERTMIN=1D20
      WERTMAX=-1D20
      DO ITT=-1,1,2
      DO IPP=-1,1,2
      TC=TEM+DFLOAT(ITT)*TERR0
      P=PRE+DFLOAT(IPP)*PERR0
      CALL NURVONPT
      CALL CALSTR
      CALL THERIA
      CALL GETVAL(XCPH,XCEL1,XCEL1DIV,IP,IS,IE,F1)
      CALL GETVAL(XCPH,XCEL2,XCEL2DIV,IP,IS,IE,F2)
      XCWERT=F1/(F1+F2)
      IF (XCWERT.GT.XAMAX) XAMAX=XCWERT
      IF (XCWERT.LT.XAMIN) XAMIN=XCWERT
      CALL GETVAL(YCPH,YCEL1,YCEL1DIV,IP,IS,IE,F1)
      CALL GETVAL(YCPH,YCEL2,YCEL2DIV,IP,IS,IE,F2)
      YCWERT=F1/(F1+F2)
      IF (YCWERT.GT.YAMAX) YAMAX=YCWERT
      IF (YCWERT.LT.YAMIN) YAMIN=YCWERT
      KDC=(XCWERT/(1.0D0-XCWERT))/(YCWERT/(1.0D0-YCWERT))
      IF (KDC.LT.WERTMIN) THEN
        WERTMIN=KDC
        TOFMIN=TC
        POFMIN=P
      END IF
      IF (KDC.GT.WERTMAX) THEN
        WERTMAX=KDC
        TOFMAX=TC
        POFMAX=P
      END IF
      END DO
      END DO
!====
      TC=TEM
      P=PRE
      CALL NURVONPT
      CALL CALSTR
      CALL THERIA
       CALL GETVAL(XCPH,XCEL1,XCEL1DIV,IP,IS,IE,F1)
       IF (IP.EQ.0.OR.IE.EQ.0) THEN
         CALL ETWASFEHLT(IP,IE)
       END IF
!--
       CALL GETVAL(XCPH,XCEL2,XCEL2DIV,IP,IS,IE,F2)
       IF (IP.EQ.0.OR.IE.EQ.0) THEN
         CALL ETWASFEHLT(IP,IE)
       END IF
!--
       XCWERT=F1/(F1+F2)
       CALL GETVAL(YCPH,YCEL1,YCEL1DIV,IP,IS,IE,F1)
       IF (IP.EQ.0.OR.IE.EQ.0) THEN
         CALL ETWASFEHLT(IP,IE)
       END IF
!--
       CALL GETVAL(YCPH,YCEL2,YCEL2DIV,IP,IS,IE,F2)
       IF (IP.EQ.0.OR.IE.EQ.0) THEN
         CALL ETWASFEHLT(IP,IE)
       END IF
!--
       YCWERT=F1/(F1+F2)
       WERT=(XCWERT/(1.0D0-XCWERT))/(YCWERT/(1.0D0-YCWERT))
!====
      KDC=WERT
!====
       TEXT1=' '
       IC=0
       FF=(KDMIN+KDMAX)/2.0D0
       PROZ=(KDC-FF)/FF*100.0D0
       MAXPROZ=(KDMAX-FF)/FF*100.0D0
       FF=(WERTMIN+WERTMAX)/2.0D0
       PTPROZ=(WERTMAX-FF)/FF*100.0D0

!====
!====
      BPOS=19
      EBAR1='x--|--x'
      EBAR2='x--0--x'
      CHE1='|'
      CHE1(BPOS:)=EBAR1
      BARSHIFT=BPOS
      FEA=0
      FEA1=0
      IF (WERT.LE.KDMAX.AND.WERT.GE.KDMIN) FEA1=1
      IF (WERTMIN.LE.KDMAX.AND.WERTMAX.GE.KDMIN) FEA=1
      IF (WERT.LT.KDMIN) BARSHIFT=BPOS-9
      IF (WERT.GT.KDMAX) BARSHIFT=BPOS+9
      IF (FEA.EQ.1.AND.WERT.LE.KDMIN) BARSHIFT=BPOS-5
      IF (FEA.EQ.1.AND.WERT.GT.KDMAX) BARSHIFT=BPOS+5
      IF (FEA1.EQ.1.AND.WERT.LE.(KDMIN+KDMAX)/2.0D0) BARSHIFT=BPOS-2
      IF (FEA1.EQ.1.AND.WERT.GT.(KDMIN+KDMAX)/2.0D0) BARSHIFT=BPOS+2
      IF (DABS(PROZ).LT.0.001) BARSHIFT=BPOS
      INKD=1
      IF (FEA.EQ.1) INKD=0
      CHE2='|'
      CHE2(BARSHIFT:)=EBAR2
!
       CH16=' '
       WRITE (UNIT=CH16,FMT='(F7.2)') PROZ
       CALL FIBLA(CH16,I1)
       CALL LABLA(CH16,I2)
       TEXT1(IC+2:)='( '//CH16(I1:I2)//' % )'
       CALL LABLA(TEXT1,IC)
!
       IF (FEA.EQ.0.AND.WERT.LE.KDMIN) THEN
        IF (CHKLIM.GT.0) THEN
        TEXT1(IC+2:)='KD upper limit OK'
        TEXTQ='KD upper limit OK'
        CCODE=0
        INKD=1
        ELSE
        DO J=IC+1,53
         TEXT1(J:J)='+'
        END DO
        TEXT1(55:)='KD too small'
        TEXTQ='------------------ KD too small'
        INKD=2
        IF (ISINC.EQ.0) THEN
          ISINC=1
!!           NINC=NINC+1
           CCODE=1
        END IF
       END IF
       END IF
!
       IF (FEA.EQ.0.AND.WERT.GT.KDMAX) THEN
        IF (CHKLIM.LT.0) THEN
        TEXT1(IC+2:)='KD lower limit OK'
        TEXTQ='KD lower limit OK'
        CCODE=0
        INKD=1
        ELSE
        DO J=IC+1,53
         TEXT1(J:J)='+'
        END DO
        TEXT1(55:)='KD too large'
        TEXTQ='------------------ KD too large'
        INKD=2
        IF (ISINC.EQ.0) THEN
          ISINC=1
!!           NINC=NINC+1
           CCODE=1
        END IF
       END IF
       END IF
       CALL LABLA(TEXT1,IC)
!
       IF (FEA.EQ.1) THEN
         CCODE=0
         INKD=0
         TEXT1(IC+2:)='KD OK'
         TEXTQ='KD OK'
       END IF
       CALL LABLA(TEXT1,IC)

!
!       CALL PUST(scr,'check'//TEXT1)
!       CALL PUST(35,'check'//TEXT1)
!==
        CALL LABLA(XCPH,I1)
        CALL LABLA(XCEL1CH,I2)
        CALL LABLA(XCEL2CH,I3)
!cdcmai2021        XVARI=XCEL1CH(1:I2)//'/('//XCEL1CH(1:I2)// &
!cdcmai2021        '+'//XCEL2CH(1:I3)//') ('//XCPH(1:I1)//')'
        XVARI=XCEL1CH(1:I2)//'/'//XCEL2CH(1:I3)//' ('//XCPH(1:I1)//')'
        CALL LABLA(YCPH,I1)
        CALL LABLA(YCEL1CH,I2)
        CALL LABLA(YCEL2CH,I3)
!cdcmai2021        YVARI=YCEL1CH(1:I2)//'/('//YCEL1CH(1:I2)// &
!cdcmai2021        '+'//YCEL2CH(1:I3)//') ('//YCPH(1:I1)//')'
        YVARI=YCEL1CH(1:I2)//'/'//YCEL2CH(1:I3)//' ('//YCPH(1:I1)//')'
       CALL LABLA(XVARI,I1)
       CALL LABLA(YVARI,I2)
       CH001='  KD = '//XVARI(1:I1)//' / '//YVARI(1:I2)
!       CALL PUST(scr,CH001)
!       CALL PUST(35,CH001)
!
      WRITE(UNIT=CH16,FMT='(F15.5)') KDC
      IF (CHKLIM.LT.0) TEXT2='lower limit'
      IF (CHKLIM.GT.0) TEXT2='upper limit'
      IF (CHKLIM.EQ.0) TEXT2='bracket'
      V1='T[C]'
      WRITE (scr,FMT='(5X,55A1)') ('-',I=1,55)
      WRITE (35,FMT='(5X,55A1)') ('-',I=1,55)
!!      IF (CCODE.NE.0) WRITE (36,FMT='(5X,55A1)') ('-',I=1,55)

!      CALL LABLA(CHKELCH,I1)
      I1=INDEX(EXPER,'  ')
      CALL LABLA(TEXT2,I3)
      CALL LABLA(CH001,I4)
      WRITE (6,1102) EXPER(1:I1),CH001(1:I4),TEXT2(1:I3)
      WRITE (35,1102) EXPER(1:I1),CH001(1:I4),TEXT2(1:I3)
!!      IF (CCODE.NE.0) WRITE (36,1102) EXPER(1:I1),CH001(1:I4), &
!!      TEXT2(1:I3)
 1102 FORMAT (5X,A,A,3X,A)

      WRITE (UNIT=CH16,FMT='(F15.5)') KD0
      IF (KD0.GT.1000.0D0) CH16='   > 1000.00000'
      WRITE (6,1010) CH16
      WRITE (35,1010) CH16
!!      IF (CCODE.NE.0) WRITE (36,1010) CH16
 1010 FORMAT (5X,'initial: KD =',3X,A)
      WRITE (6,1100) 
      WRITE (35,1100) 
!!      IF (CCODE.NE.0) WRITE (36,1100) 
 1100 FORMAT (9X,'T[C]',4X,'P[Bar]',7X,'KD',10X, &
      'min',9X,'max',10X,'+/-')

      CALL LABLA(CHE1,I1)
      IF (CHKLIM.LT.0) CHE1(I1+1:I1+9)='---------'
      IF (CHKLIM.GT.0) CHE1(I1-15:I1-7)='---------'

      CALL LABLA(CHE1,I1)
      WRITE (6,1000) TEM,PRE,(KDMIN+KDMAX)/2.0D0,KDMIN, &
      KDMAX,MAXPROZ,CHE1(1:I1)
      WRITE (35,1000) TEM,PRE,(KDMIN+KDMAX)/2.0D0,KDMIN, &
      KDMAX,MAXPROZ,CHE1(1:I1)
!!      IF (CCODE.NE.0) WRITE (36,1000) TEM,PRE, &
!!     (KDMIN+KDMAX)/2.0D0,KDMIN,KDMAX,MAXPROZ,CHE1(1:I1)
 1000 FORMAT (5X,F8.2,F10.1,1X,F12.5,F12.5,F12.5,4X,F7.2,' %',A)

      CALL LABLA(CHE2,I1)
      WRITE (6,1002) WERT,PTPROZ,CHE2(1:I1)
      WRITE (35,1002) WERT,PTPROZ,CHE2(1:I1)
!!      IF (CCODE.NE.0) WRITE (36,1002) WERT,PTPROZ,CHE2(1:I1)
 1002 FORMAT (24X,F12.5,28X,F7.2,' %',A)

      WRITE (6,1004) TOFMIN,POFMIN,WERTMIN
      WRITE (35,1004) TOFMIN,POFMIN,WERTMIN
!!      IF (CCODE.NE.0) WRITE (36,1004) TOFMIN,POFMIN,WERTMIN
 1004 FORMAT (5X,F8.2,F10.1,13X,F12.5)

      WRITE (6,1008) TOFMAX,POFMAX,WERTMAX,TEXT1(1:IC)
      WRITE (35,1008) TOFMAX,POFMAX,WERTMAX,TEXT1(1:IC)
!!      IF (CCODE.NE.0) WRITE (36,1008) TOFMAX,POFMAX,WERTMAX, &
!!      TEXT1(1:IC)
 1008 FORMAT (5X,F8.2,F10.1,25X,F12.5,3X,A)

! q-summary
!      CALL LABLA(CH001,I4)
      CALL LABLA(TEXTQ,I5)
      WRITE (39,1112) CH001(1:50),TEXTQ(1:I5)
 1112 FORMAT (13X,A,2X,A,3X,A)
       

!====================================================================
!
!====================================================================
!
      WRITE (scr,FMT='(5X,55A1)') ('-',I=1,55)
      WRITE (35,FMT='(5X,55A1)') ('-',I=1,55)
      
      
      
!---  MCODE is set to 1 if assemblage is not stable
!---  at the moment this does not make the experiment inconsistent.
      IF (MCODE.GT.0) THEN
        CALL PUST  &
        (6,'     experiment inconsistent, assemblage not stable ')
      END IF
!====
!      WRITE (scr,FMT='(5X,55A1)') ('-',I=1,55)
!      WRITE (35,FMT='(5X,55A1)') ('-',I=1,55)
!      WRITE (UNIT=6,FMT='(''CCODE='',I4)') CCODE
!      WRITE (UNIT=35,FMT='(''CCODE='',I4)') CCODE
!==
!-------------------------------------------------------------
!---- make plot
!-------------------------------------------------------------
      IF (INKD.LE.1) THEN
        CCODE=0
      ELSE
         CCODE=1
      END IF
      
      IF (NCHOOSE.GT.2) THEN
          CCODE=CCODE1
      END IF
      
!!      WRITE (UNIT=35,FMT='(''in CHECKEXCH, INKD,CCODE='',2i4)') INKD,CCODE

      DO I=1,NPLOTS
        IF (AUTOTYP(I).EQ.'EXC') THEN
          PLNR=I
          CALL PLOTEXP(PLNR)
        END IF
        IF (AUTOTYP(I).EQ.'EX2') THEN
          PLNR=I
          CALL PLOTEXP(PLNR)
        END IF
        IF (AUTOTYP(I).EQ.'LKD') THEN
          PLNR=I
          CALL PLOTEXP(PLNR)
        END IF
!        IF (AUTOTYP(I).EQ.'PX') THEN
!          PLNR=I
!          CALL PLOTEXP(PLNR)
!        END IF
!        IF (AUTOTYP(I).EQ.'TX') THEN
!          PLNR=I
!          CALL PLOTEXP(PLNR)
!        END IF
      END DO
!===
      RETURN
      END
!-----
!*************************************************************
!*************************************************************
      SUBROUTINE ETWASFEHLT(IP,IE)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
      include 'checkdb.cmn'
!      CHARACTER*(*) PHA,ELE
      CHARACTER(150) TEXT1
      INTEGER(4) IP,IE,IC,J,I1,I2
!--
      IF (IP.EQ.0) THEN
       CCODE=10
       CALL LABLA(XCPH,I1)
       TEXT1='     '//XCPH(1:I1)
       CALL LABLA(TEXT1,IC)
       DO J=IC+2,112
        TEXT1(J:J)='+'
       END DO
       TEXT1(114:)='phase not stable 1'
       CALL PUST(6,TEXT1)
       CALL PUST(35,TEXT1)
       CALL SHORTSUM
       RETURN
      END IF
!--
      IF (IE.EQ.0.AND.IP.NE.0) THEN
       CCODE=5
       CALL LABLA(XCPH,I1)
       CALL LABLA(XCEL1,I2)
       TEXT1='     '//XCPH(1:I1)//' : '//XCEL1(1:I2)
       CALL LABLA(TEXT1,IC)
       DO J=IC+2,113
        TEXT1(J:J)='-'
       END DO
       TEXT1(115:)='unknown variable 1'
       CALL PUST(6,TEXT1)
       CALL PUST(35,TEXT1)
       RETURN
      END IF

!----
      RETURN
      END
!-----
!*************************************************************
!*************************************************************
      SUBROUTINE PREPPLOT
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
      include 'checkdb.cmn'
!
!-----END OF COMMON VARIABLES
!
      INTEGER(4) I1,I2,I3,I,NCH,NV,IJ,IO
      CHARACTER(250) CH002,BININ
      CHARACTER(3) CH3
      REAL(8) ZERO,YPOS,FF
!
      IF (PRTEST) WRITE (UNIT=6,FMT='(''-> PREPPLOT'')')
      IF (PRTEST) WRITE (UNIT=35,FMT='(''-> PREPPLOT'')')
!
      XUNTEN(NPLOTS)=PLXMIN
      XOBEN(NPLOTS)=PLXMAX
      YUNTEN(NPLOTS)=PLYMIN
      YOBEN(NPLOTS)=PLYMAX
!----------------------------------------------------------------
!--    BEGIN experimental data file 30   _0exp
!----------------------------------------------------------------
       CALL LABLA(AUTOPLOT(NPLOTS),I1)
      OPFILE=NEWDIR(1:LDIR)//AUTOPLOT(NPLOTS)(1:I1)//'_0exp'
!      CALL PUST(6,'Open file 30: '//OPFILE)
      CALL LABLA(OPFILE,IO)
!-- open _0exp (experiments with scale, fill: none and gray)
!       WRITE (UNIT=6,FMT='(''OPEN 3742 '',A)')OPFILE(1:IO)
       OPEN (UNIT=30,FILE=OPFILE(1:IO),STATUS='UNKNOWN')
       CALL LABLA(XVARI,I1)
       CALL LABLA(YVARI,I2)
       WRITE (30,1048) XVARI(1:I1),YVARI(1:I2), &
       PLXMIN,PLXMAX,PLYMIN,PLYMAX,PLBR,PLHO
 1048  FORMAT ( &
       A,/, &
       A,/, &
       F10.2,F10.2,F10.1,F10.1,F10.4,F10.4,'   0  0',/,' ',/, &
       'FCOLOR   0.5   0.5   0.5',/,'FAT    0.01')
!----------------------------------------------------------------
!--    make THERIAK input file 31   .txt  (close unit 31)
!----------------------------------------------------------------
       IF (VERGL(AUTOTYP(NPLOTS),'EXC')) THEN
         CH3='bin'
         NCH=2
         NV=2
       END IF
       IF (VERGL(AUTOTYP(NPLOTS),'EX2')) THEN
         CH3='bin'
         NCH=2
         NV=2
       END IF
       IF (VERGL(AUTOTYP(NPLOTS),'BIV')) THEN
         CH3='bin'
         NCH=2
         NV=2
       END IF
       IF (VERGL(AUTOTYP(NPLOTS),'VAL')) THEN
         CH3='fun'
         NCH=1
         NV=2
       END IF
       IF (VERGL(AUTOTYP(NPLOTS),'LKD')) THEN
         CH3='fun'
         NCH=1
         NV=2
       END IF
       IF (VERGL(AUTOTYP(NPLOTS),'LKR')) THEN
         CH3='rea'
         NCH=1
         NV=3
       END IF
       CALL LABLA(AUTOPLOT(NPLOTS),I1)
      OPFILE=NEWDIR(1:LDIR)//AUTOPLOT(NPLOTS)(1:I1)//'.txt'
!      CALL PUST(6,'Open file 31: '//OPFILE)
      CALL LABLA(OPFILE,IO)
!-- open .txt script for theriak with bin++, fun++ or rea++
!   theriak will produce bin_loop, fun_loop or rea_loop
!       WRITE (UNIT=6,FMT='(''OPEN 3791 '',A)')OPFILE(1:IO)
       OPEN (UNIT=31,FILE=OPFILE(1:IO),STATUS='UNKNOWN')
       CALL LABLA(filename(dbs),I1)
       CALL LABLA(PICKSTRING,I2)
       CH002=filename(dbs)(1:I1)//'   '//PICKSTRING(1:I2)
       CALL PUST(31,CH002)
       CH002=CH3//'++    50'
       CALL PUST(31,CH002)
!
       DO I=1,NCH
        CALL LABLA(BINCH(I),I1)
        CALL PUST(31,'0  '//BINCH(I)(1:I1)//'   *')
!!        WRITE (UNIT=6,FMT='(''I BINCH(I)'',I3,2X,A)') i,BINCH(I)(1:I1)
       END DO
!
!!       WRITE (UNIT=6,FMT='(''AUTOPLOT='',A)') AUTOTYP(NPLOTS)
       DO I=1,NV
        CALL LABLA(BINV(I),I1)
        CALL PUST(31,BINV(I)(1:I1))
!!        WRITE (UNIT=6,FMT='(''I BINV(I)'',I3,2X,A)') i,BINV(I)(1:I1)
       END DO
!
!      read const (multiple)
    30 READ (UNIT=drv,FMT='(A)',END=31) BININ
       IF (BININ.EQ.' ') GOTO 31
       CALL PUST(31,BININ)
       GOTO 30
    31 CONTINUE
       CLOSE (UNIT=31)
!----------------------------------------------------------------
!--    make job file 31   _txt
!----------------------------------------------------------------
       CALL LABLA(AUTOPLOT(NPLOTS),I1)
       CALL LABLA(JONAME,IJ)
       CH002=AUTOPLOT(NPLOTS)(1:I1)//JONAME(1:IJ)
       CALL LABLA(CH002,IJ)

!      CALL PUST(6,'HUCH '//CH002)

      OPFILE=NEWDIR(1:LDIR)//CH002(1:IJ)
!      CALL PUST(6,'Open file 31: '//OPFILE)
      CALL LABLA(OPFILE,IO)
!-- open _txt for theriak  (to make _0exp.ps _com.ps _com2.ps)
       OPEN (UNIT=31,FILE=OPFILE(1:IO),STATUS='UNKNOWN')
       CH002='theriak '//NEWDIR(1:LDIR)//AUTOPLOT(NPLOTS)(1:I1)// &
       '.txt  '
       CALL PUST(31,CH002)
       CH002='explot  '//NEWDIR(1:LDIR)//AUTOPLOT(NPLOTS)(1:I1)// &
       '_0exp  '//NEWDIR(1:LDIR)//AUTOPLOT(NPLOTS)(1:I1)//'_0exp.ps'
       CALL PUST(31,CH002)
!
!!       CH002='cat  '//CH3//'_loop ' &
!!       //AUTOPLOT(NPLOTS)(1:I1)//'_exn  > ' &
!
       CH002='cat2  '// &
       NEWDIR(1:LDIR)//AUTOPLOT(NPLOTS)(1:I1)//'_'//CH3//'_loop ' &
       //NEWDIR(1:LDIR)//AUTOPLOT(NPLOTS)(1:I1)//'_exn  ' &
       //NEWDIR(1:LDIR)//AUTOPLOT(NPLOTS)(1:I1)//'_com'
       CALL PUST(31,CH002)
       CH002='explot  '// &
       NEWDIR(1:LDIR)//AUTOPLOT(NPLOTS)(1:I1)//'_com  '// &
       NEWDIR(1:LDIR)//AUTOPLOT(NPLOTS)(1:I1)//'_com.ps'
       CALL PUST(31,CH002)
!
!!       CH002='cat  '//CH3//'_loop ' &
!!       //AUTOPLOT(NPLOTS)(1:I1)//'_exn2  > '// &
!
       CH002='cat2  '// &
       NEWDIR(1:LDIR)//AUTOPLOT(NPLOTS)(1:I1)//'_'//CH3//'_loop ' &
       //NEWDIR(1:LDIR)//AUTOPLOT(NPLOTS)(1:I1)//'_exn2  '// &
       NEWDIR(1:LDIR)//AUTOPLOT(NPLOTS)(1:I1)//'_com2'
       CALL PUST(31,CH002)
       CH002='explot  '// &
       NEWDIR(1:LDIR)//AUTOPLOT(NPLOTS)(1:I1)//'_com2  '// &
       NEWDIR(1:LDIR)//AUTOPLOT(NPLOTS)(1:I1)//'_com2.ps'
       CALL PUST(31,CH002)
       CLOSE (UNIT=31)
!
       CALL LABLA(AUTOPLOT(NPLOTS),I1)
       CALL MAKEEXEC(NEWDIR(1:LDIR)//AUTOPLOT(NPLOTS)(1:I1)//'_job')
!----------------------------------------------------------------
      OPFILE=NEWDIR(1:LDIR)//AUTOPLOT(NPLOTS)(1:I1)//'_exn'
!      CALL PUST(6,'Open file 32: '//OPFILE)
      CALL LABLA(OPFILE,IO)
!-- open _exn (experiments no scale, fill: none and gray)
       OPEN (UNIT=32,FILE=OPFILE(1:IO),STATUS='UNKNOWN')
!!       WRITE (UNIT=32,FMT='(/,''FGRAY    0.5'')') 
       WRITE (UNIT=32,FMT='(/,A)') FGRAU
       WRITE (UNIT=32,FMT='(/,''FAT    0.01'')') 
      OPFILE=NEWDIR(1:LDIR)//AUTOPLOT(NPLOTS)(1:I1)//'_exn2'
!      CALL PUST(6,'Open file 34: '//OPFILE)
      CALL LABLA(OPFILE,IO)
!-- open _exn2 (experiments no scale, inconsistencies: black)
       OPEN (UNIT=34,FILE=OPFILE(1:IO),STATUS='UNKNOWN')
!!       WRITE (UNIT=34,FMT='(/,''FGRAY    0.0'')') 
       WRITE (UNIT=34,FMT='(/,A)') FCOLOR(NPLAUT)
       WRITE (UNIT=34,FMT='(/,''FAT    0.01'')') 
!--
       ZERO=0.0D0
       IF (NICREAC.EQ.' ') NICREAC='XXX'
       CALL FIBLA(NICREAC,I1)
       CALL LABLA(NICREAC,I2)
!       CALL LABLA(PLFILE,I3)
!
!      write reaction on top
       CALL LABLA(REAID,I3)
       WRITE (30,2051) REAID(1:I3),NICREAC(I1:I2),ZERO,PLHO
       WRITE (32,2051) REAID(1:I3),NICREAC(I1:I2),ZERO,PLHO
       WRITE (34,2051) REAID(1:I3),NICREAC(I1:I2),ZERO,PLHO
  2051 FORMAT ('PSYM   (plot: ',A,') ',A,2X,F8.3,2X,F8.3, &
       '   0.3    0   0   1   0')
       CALL LABLA(DRIVENAME,I3)
       WRITE (30,2053) DRIVENAME(1:I3),PLBR+1.0
       WRITE (32,2053) DRIVENAME(1:I3),PLBR+1.0
       WRITE (34,2053) DRIVENAME(1:I3),PLBR+1.0
  2053 FORMAT ('PSYM    data: ',A,2X,F8.3, &
       '   1.5   0.3   0   0   -0.5   90')
       
! first author here
       WRITE (UNIT=30,FMT='(A)') LCOLOR(NPLAUT)
       WRITE (UNIT=30,FMT='(A)') FGRAU
       WRITE (UNIT=32,FMT='(A)') LCOLOR(NPLAUT)
       WRITE (UNIT=32,FMT='(A)') FGRAU
       WRITE (UNIT=34,FMT='(A)') LCOLOR(NPLAUT)
       WRITE (UNIT=34,FMT='(A)') FCOLOR(NPLAUT)
       YPOS=PLHO-0.45D0
       CALL LABLA(PLAUT(NPLAUT),I3)
       WRITE (30,2055) PLAUT(NPLAUT)(1:I3),ZERO+0.2D0,YPOS
       WRITE (32,2055) PLAUT(NPLAUT)(1:I3),ZERO+0.2D0,YPOS
       WRITE (34,2055) PLAUT(NPLAUT)(1:I3),ZERO+0.2D0,YPOS
  2055 FORMAT ('PSYM    ',A,2X,F8.3,2X,F8.3, &
       '   0.3    0   0   0   0')
!
!------
       CLOSE (UNIT=30)
       CLOSE (UNIT=32)
       CLOSE (UNIT=34)
!
      RETURN
      END
!-----
!*************************************************************
!*************************************************************
      SUBROUTINE PLOTAUT
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
      include 'checkdb.cmn'
!-----END OF COMMON VARIABLES
      INTEGER(4) I,I3,IPL,IO,I1
      REAL(8) FF,YPOS,ZERO
!      plot the author from TITLE2, NPLAUT
      ZERO=0.0D0

      DO IPL=1,NPLOTS

      CALL LABLA(AUTOPLOT(IPL),I1)
      OPFILE=NEWDIR(1:LDIR)//AUTOPLOT(IPL)(1:I1)//'_0exp'
      CALL PUST(6,'Open file 30: '//OPFILE)
      CALL LABLA(OPFILE,IO)
!-- open _0exp (experiments with scale, fill: none and gray)
      OPEN (UNIT=30,FILE=OPFILE(1:IO),STATUS='OLD',POSITION='APPEND')
      OPFILE=NEWDIR(1:LDIR)//AUTOPLOT(IPL)(1:I1)//'_exn'
!      CALL PUST(6,'Open file 32: '//OPFILE)
      CALL LABLA(OPFILE,IO)
!-- open _exn (experiments no scale, fill: none and gray)
      OPEN (UNIT=32,FILE=OPFILE(1:IO),STATUS='OLD',POSITION='APPEND')
      OPFILE=NEWDIR(1:LDIR)//AUTOPLOT(IPL)(1:I1)//'_exn2'
!      CALL PUST(6,'Open file 34: '//OPFILE)
      CALL LABLA(OPFILE,IO)
!-- open _exn2 (experiments no scale, inconsstencies: black)
      OPEN (UNIT=34,FILE=OPFILE(1:IO),STATUS='OLD',POSITION='APPEND')


      I=NPLAUT
       WRITE (UNIT=30,FMT='(A)') LCOLOR(I)
       WRITE (UNIT=30,FMT='(A)') FGRAU
       WRITE (UNIT=32,FMT='(A)') LCOLOR(I)
       WRITE (UNIT=32,FMT='(A)') FGRAU
       WRITE (UNIT=34,FMT='(A)') LCOLOR(I)
       WRITE (UNIT=34,FMT='(A)') FCOLOR(I)
      FF=DFLOAT(I-1)
      YPOS=PLHO-0.45D0-FF*0.45D0
      CALL LABLA(PLAUT(NPLAUT),I3)
      WRITE (30,2055) PLAUT(I)(1:I3),ZERO+0.2D0,YPOS
      WRITE (32,2055) PLAUT(I)(1:I3),ZERO+0.2D0,YPOS
      WRITE (34,2055) PLAUT(I)(1:I3),ZERO+0.2D0,YPOS
 2055 FORMAT ('PSYM    ',A,2X,F8.3,2X,F8.3, &
       '   0.3    0   0   0   0')
!
      END DO
!------
       CLOSE (UNIT=30)
       CLOSE (UNIT=32)
       CLOSE (UNIT=34)
!-----
!
      RETURN
      END
!-----
!*************************************************************
!*************************************************************
      SUBROUTINE PLOTEXP(PLNR)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
      include 'checkdb.cmn'
!
!-----END OF COMMON VARIABLES
!
      INTEGER(4) I1,I2,J,JJ,PLNR,IE,IP,IS,IO
      REAL(8) F1,F2,XVAL,XERR,YU,YO,YRE,YERR,YC,TEMNOW, &
      GRSVAL,GRSBIV,GRSEXC,GRSPTX,GRSEX2,GRSLKD,GRSLKR,GRSPT,GRSTXC,GRSISO
      CHARACTER(32) CH20
!
      GRSVAL=0.03D0
      GRSBIV=0.1D0
      GRSEXC=0.1D0
      GRSPTX=0.1D0
      GRSEX2=0.1D0
      GRSLKD=0.1D0
      GRSLKR=0.1D0
      GRSPT=0.1D0
      GRSTXC=0.1D0
      GRSISO=0.1D0
      YERR=0.0D0
!------
      IF (PRTEST) THEN
      WRITE (UNIT=6,FMT='(''-> PLOTEXP plot Nr and type:'',I4,2X,A4)') &
      PLNR,AUTOTYP(PLNR)
      WRITE (UNIT=35,FMT='(''-> PLOTEXP plot Nr and type:'',I4,2X,A4)') &
      PLNR,AUTOTYP(PLNR)
!      WRITE (UNIT=6,FMT='(''   CCODE = '',I4)') CCODE
!      WRITE (UNIT=35,FMT='(''   CCODE = '',I4)') CCODE
      END IF
!

!     statt ACCESS='APPEND':  POSITION='APPEND'

      CALL LABLA(AUTOPLOT(PLNR),I1)
      OPFILE=NEWDIR(1:LDIR)//AUTOPLOT(PLNR)(1:I1)//'_0exp'
      CALL PUST(6,'Open file 30: '//OPFILE)
      CALL LABLA(OPFILE,IO)
!-- open _0exp (experiments with scale, fill: none and gray)
      OPEN (UNIT=30,FILE=OPFILE(1:IO),STATUS='OLD',POSITION='APPEND')
      OPFILE=NEWDIR(1:LDIR)//AUTOPLOT(PLNR)(1:I1)//'_exn'
!      CALL PUST(6,'Open file 32: '//OPFILE)
      CALL LABLA(OPFILE,IO)
!-- open _exn (experiments no scale, fill: none and gray)
      OPEN (UNIT=32,FILE=OPFILE(1:IO),STATUS='OLD',POSITION='APPEND')
      OPFILE=NEWDIR(1:LDIR)//AUTOPLOT(PLNR)(1:I1)//'_exn2'
!      CALL PUST(6,'Open file 34: '//OPFILE)
      CALL LABLA(OPFILE,IO)
!-- open _exn2 (experiments no scale, inconsstencies: black)
      OPEN (UNIT=34,FILE=OPFILE(1:IO),STATUS='OLD',POSITION='APPEND')
!======
      TEMNOW=TEM
      IF (TUNIT(PLNR).EQ.'TC'.AND.CTUNIT.EQ.'TK') THEN
        TEM=TEM-273.15D0
      END IF
      IF (TUNIT(PLNR).EQ.'TK'.AND.CTUNIT.EQ.'TC') THEN
        TEM=TEM+273.15
      END IF
!=================================================================
!======
      XERR=0.0D0
      IF (AUTOTYP(PLNR).EQ.'VAL') THEN
        WRITE (UNIT=6,FMT='(''-> plot typ = VAL'')')
        IF (XKEY(PLNR).EQ.'TC') THEN
           XVAL=TEM
          XERR=TERR0
        END IF
        IF (XKEY(PLNR).EQ.'TK') THEN
!        XVAL=TEM+273.15D0
          XVAL=TEM
          XERR=TERR0
        END IF
        IF (XKEY(PLNR).EQ.'P') THEN
          XVAL=PRE
          XERR=PERR0
        END IF
        IF (XKEY(PLNR).EQ.'1000/T') THEN
          XVAL=1000.0D0/(TEM+273.15D0)
          XERR=1000.0D0/(TEM+273.15D0)-1000.0D0/(TEM+TERR0+273.15D0)
        END IF
!
        YU=CHKMIN
        YO=CHKMAX
        YERR=(YO-YU)/2.0D0
        IF (YO.LT.YUNTEN(PLNR)) THEN
          YU=YUNTEN(PLNR)-YERR
          YO=YUNTEN(PLNR)
        END IF
        IF (YU.GT.YOBEN(PLNR)) THEN
          YO=YOBEN(PLNR)+YERR
          YU=YOBEN(PLNR)
        END IF
        YC=YCWERT
        IF (YC.LT.YUNTEN(PLNR)) YC=YUNTEN(PLNR)
        IF (YC.GT.YOBEN(PLNR)) YC=YOBEN(PLNR)
!
        J=0
        JJ=0
       
        WRITE (UNIT=35,FMT='('' in plotexp(VAL): CCODE='',I4)') CCODE
        
        CALL FIBLA(EXPEC,I1)
        IF (EXPEC(I1:I1).EQ.'+') J=2
        IF (CCODE.GT.0) JJ=2
        
! draw box around observed point
        WRITE (30,1010) J,XVAL-XERR,YU,XVAL+XERR,YU, &
        XVAL+XERR,YO,XVAL-XERR,YO
        WRITE (32,1010) J,XVAL-XERR,YU,XVAL+XERR,YU, &
        XVAL+XERR,YO,XVAL-XERR,YO
        WRITE (34,1010) JJ,XVAL-XERR,YU,XVAL+XERR,YU, &
        XVAL+XERR,YO,XVAL-XERR,YO
  1010  FORMAT ( &
        'LINIEN    ',I2,2X,'1  0   0',8(2X,1PE15.8),'   999  999  0')
! draw cross if experiment not used
        IF (.NOT.USEDFOR) THEN
          WRITE (30,1012) XVAL-XERR,YU,XVAL+XERR,YO
          WRITE (30,1012) XVAL-XERR,YO,XVAL+XERR,YU
          WRITE (32,1012) XVAL-XERR,YU,XVAL+XERR,YO
          WRITE (32,1012) XVAL-XERR,YO,XVAL+XERR,YU
          WRITE (34,1012) XVAL-XERR,YU,XVAL+XERR,YO
          WRITE (34,1012) XVAL-XERR,YO,XVAL+XERR,YU
  1012    FORMAT ( &
          'LINIEN     0  0  0   0',4(2X,1PE15.8),'   999  999  0')
        END IF
! draw label
        I2=INDEX(EXPER,'  ')
        WRITE (30,1014) EXPER(1:I2),XVAL+XERR,(YU+YO)/2.0D0,GRSVAL
        WRITE (32,1014) EXPER(1:I2),XVAL+XERR,(YU+YO)/2.0D0,GRSVAL
        WRITE (34,1014) EXPER(1:I2),XVAL+XERR,(YU+YO)/2.0D0,GRSVAL
  1014  FORMAT ( &
        'TEXT    ',A,2(2X,1PE15.8),2X,0P,F6.2,'  0.5  0  -0.5  0')
! draw dotted line
        F1=XVAL
        F2=(YU+YO)/2.0D0
        J=0
!         WRITE (30,1016) J,XVAL,YC,F1,F2
        WRITE (32,1016) J,XVAL,YC,F1,F2
        WRITE (34,1016) J,XVAL,YC,F1,F2
  1016  FORMAT ( &
        'STYLE   0.05   0.05   0.05   0.05',/, &
        'LINIEN   ',I2,2X,'0  0   0',4(2X,1PE15.8),'  999  999  0',/, &
        'STYLE   0.0   0.0   0.0   0.0')

!       WRITE (30,1018) PTSYMB,XVAL,YC
        WRITE (32,1018) PTSYMB,XVAL,YC
        WRITE (34,1018) PTSYMB,XVAL,YC
  1018  FORMAT ( &
       'PUNKTE    ',2X,I3,2X,'0.1',2(2X,1PE15.8),'   999  999  0')
      END IF
!=================================================================
!======
      XERR=0.0D0
      IF (AUTOTYP(PLNR).EQ.'BIV') THEN
        WRITE (UNIT=6,FMT='(''-> plot typ = BIV'')')
        XKEY(PLNR)='bi'
        XVAL=(XCHKMIN+XCHKMAX)/2.0D0
        XERR=DABS(XCHKMIN-XCHKMAX)/2.0D0
        YU=CHKMIN
        YO=CHKMAX
        YERR=DABS(YO-YU)/2.0D0
        IF (YO.LT.YUNTEN(PLNR)) THEN
          YU=YUNTEN(PLNR)-YERR
          YO=YUNTEN(PLNR)
        END IF
        IF (YU.GT.YOBEN(PLNR)) THEN
          YO=YOBEN(PLNR)+YERR
          YU=YOBEN(PLNR)
        END IF

        YC=(CHKMIN+CHKMAX)/2.0D0
        IF (YC.LT.YUNTEN(PLNR)) YC=YUNTEN(PLNR)
        IF (YC.GT.YOBEN(PLNR)) YC=YOBEN(PLNR)
!
        J=0
        JJ=0
       
        WRITE (UNIT=35,FMT='('' in plotexp(BIV): CCODE='',I4)') CCODE
        
        CALL FIBLA(EXPEC,I1)
        IF (EXPEC(I1:I1).EQ.'+') J=2
        IF (CCODE.GT.0) JJ=2
! draw box around observed point
        WRITE (30,2010) J,XVAL-XERR,YU,XVAL+XERR,YU, &
        XVAL+XERR,YO,XVAL-XERR,YO
        WRITE (32,2010) J,XVAL-XERR,YU,XVAL+XERR,YU, &
        XVAL+XERR,YO,XVAL-XERR,YO
        WRITE (34,2010) JJ,XVAL-XERR,YU,XVAL+XERR,YU, &
        XVAL+XERR,YO,XVAL-XERR,YO
  2010  FORMAT ( &
        'LINIEN    ',I2,2X,'1  0   0',8(2X,1PE15.8),'   999  999  0')
! draw cross if experiment not used
        IF (.NOT.USEDFOR) THEN
        WRITE (30,2012) XVAL-XERR,YU,XVAL+XERR,YO
        WRITE (30,2012) XVAL-XERR,YO,XVAL+XERR,YU
        WRITE (32,2012) XVAL-XERR,YU,XVAL+XERR,YO
        WRITE (32,2012) XVAL-XERR,YO,XVAL+XERR,YU
        WRITE (34,2012) XVAL-XERR,YU,XVAL+XERR,YO
        WRITE (34,2012) XVAL-XERR,YO,XVAL+XERR,YU
  2012  FORMAT ( &
        'LINIEN     0  0  0   0',4(2X,1PE15.8),'   999  999  0')
        END IF
! draw line from starting point to observed point
         F1=(CHKMIN+CHKMAX)/2.0D0
         WRITE (30,2013) J,XVAL,CHKSTART,XVAL,(YO+YU)/2.0D0
         WRITE (32,2013) J,XVAL,CHKSTART,XVAL,(YO+YU)/2.0D0
         WRITE (34,2013) JJ,XVAL,CHKSTART,XVAL,(YO+YU)/2.0D0
  2013 FORMAT ( &
       'LINIEN    ',I2,2X,'0  0   0',4(2X,1PE15.8),'   999  999  0')
! draw label
        I2=INDEX(EXPER,'  ')
        WRITE (30,2014) EXPER(1:I2),XVAL+XERR,(YU+YO)/2.0D0,GRSBIV
        WRITE (32,2014) EXPER(1:I2),XVAL+XERR,(YU+YO)/2.0D0,GRSBIV
        WRITE (34,2014) EXPER(1:I2),XVAL+XERR,(YU+YO)/2.0D0,GRSBIV
  2014  FORMAT ( &
        'TEXT    ',A,2(2X,1PE15.8),2X,0P,F6.2,'  0.5  0  -0.5  0')

      END IF
!=================================================================
!======
      IF (AUTOTYP(PLNR).EQ.'EXC') THEN
        WRITE (UNIT=6,FMT='(''-> plot typ = EXC'')')
      J=0
      JJ=0
! re-define CCODE (>1 if not consistent)
!
        WRITE (UNIT=35,FMT='('' in plotexp(EXCH): CCODE='',I4)') CCODE
        
       IF (CCODE.GT.0) JJ=2
       IF (MCODE.GT.0) JJ=2

       WRITE (UNIT=6,FMT='(''MCODE = '',I3)') MCODE

!      WRITE (UNIT=6,FMT='(''HERE EXC'',A)') REAID

! draw box around observed point
       WRITE (30,1020) J,XCMIN,YCMIN,XCMAX,YCMIN, &
       XCMAX,YCMAX,XCMIN,YCMAX
       WRITE (32,1020) J,XCMIN,YCMIN,XCMAX,YCMIN, &
       XCMAX,YCMAX,XCMIN,YCMAX
       WRITE (34,1020) JJ,XCMIN,YCMIN,XCMAX,YCMIN, &
       XCMAX,YCMAX,XCMIN,YCMAX
  1020 FORMAT ( &
       'LINIEN    ',I2,2X,'1  0   0',8(2X,1PE15.8),'   999  999  0')
! draw line from starting point to observed point
       F1=(XCMAX+XCMIN)/2.0D0
       F2=(YCMAX+YCMIN)/2.0D0
       WRITE (30,1022) J,XCSTART,YCSTART,F1,F2
       WRITE (32,1022) J,XCSTART,YCSTART,F1,F2
       WRITE (34,1022) JJ,XCSTART,YCSTART,F1,F2
  1022 FORMAT ( &
       'LINIEN    ',I2,2X,'0  0   0',4(2X,1PE15.8),'   999  999  0')
! draw dotted line from calculated to observed point
!         WRITE (30,1024) J,XCWERT,YCWERT,F1,F2
       WRITE (32,1024) J,XCWERT,YCWERT,F1,F2
       WRITE (34,1024) J,XCWERT,YCWERT,F1,F2
  1024 FORMAT ( &
       'STYLE   0.05   0.05   0.05   0.05',/, &
       'LINIEN   ',I2,2X,'0  0   0',4(2X,1PE15.8),'  999  999  0',/, &
       'STYLE   0.0   0.0   0.0   0.0')
! drow dot at calculated point
!!       WRITE (UNIT=32,FMT='(/,''FGRAY    0'')') 
!!       WRITE (UNIT=34,FMT='(/,''FGRAY    0'')') 
       WRITE (UNIT=32,FMT='(/,A)') FCOLOR(NPLAUT)
       WRITE (UNIT=34,FMT='(/,A)') FCOLOR(NPLAUT)
!       WRITE (30,1026) PTSYMB,XCWERT,YCWERT
       WRITE (32,1026) PTSYMB,XCWERT,YCWERT
       WRITE (34,1026) PTSYMB,XCWERT,YCWERT
  1026 FORMAT ( &
       'PUNKTE    ',2X,I3,2X,'0.1',2(2X,1PE15.8),'   999  999  0')
!!       WRITE (UNIT=32,FMT='(/,''FGRAY    0.5'')') 
!!       WRITE (UNIT=34,FMT='(/,''FGRAY    0.0'')') 
       WRITE (UNIT=32,FMT='(/,A)') FGRAU
       WRITE (UNIT=34,FMT='(/,A)') FCOLOR(NPLAUT)
! write label
       I2=INDEX(EXPER,'  ')
       WRITE (30,1028) EXPER(1:I2),XCMAX,(YCMIN+YCMAX)/2.0D0,GRSEXC
       WRITE (32,1028) EXPER(1:I2),XCMAX,(YCMIN+YCMAX)/2.0D0,GRSEXC
       WRITE (34,1028) EXPER(1:I2),XCMAX,(YCMIN+YCMAX)/2.0D0,GRSEXC
  1028 FORMAT ( &
        'TEXT    ',A,2(2X,1PE15.8),2X,0P,F6.2,'  0.5  0  -0.5  0')
      END IF
!=================================================================
!======
      IF (AUTOTYP(PLNR).EQ.'Pq'.OR.AUTOTYP(PLNR).EQ.'Tq') THEN
        WRITE (UNIT=6,FMT='(''-> plot typ = PX or TX'')')
      IF (AUTOTYP(PLNR).EQ.'PX') THEN
        YRE=PRE
        YERR=PERR0
      END IF
      IF (AUTOTYP(PLNR).EQ.'TX') THEN
        YRE=TEM
        YERR=TERR0
      END IF
      J=0
      JJ=0
! re-define CCODE (>1 if not consistent)
!
! draw box around observed point
       IF (CHKLIM.GT.0) J=2
       
        WRITE (UNIT=35,FMT='('' in plotexp(TX/PX): CCODE='',I4)') CCODE
        
       IF (CCODE.GT.0) JJ=2
       WRITE (30,1030) J,XCMIN,YRE-YERR,XCMAX,YRE-YERR, &
       XCMAX,YRE+YERR,XCMIN,YRE+YERR
       WRITE (32,1030) J,XCMIN,YRE-YERR,XCMAX,YRE-YERR, &
       XCMAX,YRE+YERR,XCMIN,YRE+YERR
       WRITE (34,1030) JJ,XCMIN,YRE-YERR,XCMAX,YRE-YERR, &
       XCMAX,YRE+YERR,XCMIN,YRE+YERR
!!       JJ=0
       IF (CCODE.GT.0) JJ=2
       WRITE (30,1030) J,YCMIN,YRE-YERR,YCMAX,YRE-YERR, &
       YCMAX,YRE+YERR,YCMIN,YRE+YERR
       WRITE (32,1030) J,YCMIN,YRE-YERR,YCMAX,YRE-YERR, &
       YCMAX,YRE+YERR,YCMIN,YRE+YERR
       WRITE (34,1030) JJ,YCMIN,YRE-YERR,YCMAX,YRE-YERR, &
       YCMAX,YRE+YERR,YCMIN,YRE+YERR
  8030 FORMAT ( &
       'LINIEN    ',I2,2X,'1  0   0',8(2X,1PE15.8),'   999  999  0')
! draw cross at starting point (XCSTART,YRE)
       WRITE (UNIT=30,FMT='(''FAT     0.02'')')
       WRITE (UNIT=32,FMT='(''FAT     0.02'')')
       WRITE (UNIT=34,FMT='(''FAT     0.02'')')
       WRITE (30,1031) XCSTART,YRE
       WRITE (32,1031) XCSTART,YRE
       WRITE (34,1031) XCSTART,YRE
       WRITE (30,1031) YCSTART,YRE
       WRITE (32,1031) YCSTART,YRE
       WRITE (34,1031) YCSTART,YRE
  8031 FORMAT ( &
       'PUNKTE    ',2X,'-2   0.1',2(2X,1PE15.8),'   999  999  0')
       WRITE (UNIT=30,FMT='(''FAT     0.01'')')
       WRITE (UNIT=32,FMT='(''FAT     0.01'')')
       WRITE (UNIT=34,FMT='(''FAT     0.01'')')
! draw line from starting point to observed point
       F1=(XCMAX+XCMIN)/2.0D0
       F2=(YCMAX+YCMIN)/2.0D0
         WRITE (30,1032) J,XCSTART,YRE,F1,YRE
         WRITE (32,1032) J,XCSTART,YRE,F1,YRE
         WRITE (34,1032) JJ,XCSTART,YRE,F1,YRE
         WRITE (30,1032) J,YCSTART,YRE,F2,YRE
         WRITE (32,1032) J,YCSTART,YRE,F2,YRE
         WRITE (34,1032) JJ,YCSTART,YRE,F2,YRE
  8032 FORMAT ( &
       'LINIEN    ',I2,2X,'0  0   0',4(2X,1PE15.8),'   999  999  0')
! draw dotted line from calculated to observed point
!         WRITE (30,1034) J,XCWERT,YCWERT,F1,F2
         WRITE (32,1034) J,XCWERT,YRE,F1,YRE
         WRITE (34,1034) J,XCWERT,YRE,F1,YRE
         WRITE (32,1034) J,YCWERT,YRE,F2,YRE
         WRITE (34,1034) J,YCWERT,YRE,F2,YRE
  8034 FORMAT ( &
       'STYLE   0.05   0.05   0.05   0.05',/, &
       'LINIEN   ',I2,2X,'0  0   0',4(2X,1PE15.8),'  999  999  0',/, &
       'STYLE   0.0   0.0   0.0   0.0')
! drow dot at calculated point
!       WRITE (30,1036) XCWERT,YCWERT
!!       WRITE (UNIT=32,FMT='(/,''FGRAY    0'')') 
!!       WRITE (UNIT=34,FMT='(/,''FGRAY    0'')') 
       WRITE (UNIT=32,FMT='(/,A)') FCOLOR(NPLAUT)
       WRITE (UNIT=32,FMT='(/,A)') FCOLOR(NPLAUT)
       WRITE (32,1036) PTSYMB,XCWERT,YRE
       WRITE (34,1036) PTSYMB,XCWERT,YRE
       WRITE (32,1036) PTSYMB,YCWERT,YRE
       WRITE (34,1036) PTSYMB,YCWERT,YRE
  8036 FORMAT ( &
       'PUNKTE    ',2X,I3,2X,'0.1',2(2X,1PE15.8),'   999  999  0')
!!       WRITE (UNIT=32,FMT='(/,''FGRAY    0.5'')') 
!!       WRITE (UNIT=34,FMT='(/,''FGRAY    0.0'')') 
       WRITE (UNIT=32,FMT='(/,A)') FGRAU
       WRITE (UNIT=32,FMT='(/,A)') FCOLOR(NPLAUT)
! write label
       I2=INDEX(EXPER,'  ')
       WRITE (30,1038) EXPER(1:I2),XCMAX,YRE,GRSPTX
       WRITE (32,1038) EXPER(1:I2),XCMAX,YRE,GRSPTX
       WRITE (34,1038) EXPER(1:I2),XCMAX,YRE,GRSPTX
       WRITE (30,1038) EXPER(1:I2),YCMAX,YRE,GRSPTX
       WRITE (32,1038) EXPER(1:I2),YCMAX,YRE,GRSPTX
       WRITE (34,1038) EXPER(1:I2),YCMAX,YRE,GRSPTX
  8038 FORMAT ( &
        'TEXT    ',A,2(2X,1PE15.8),2X,0P,F6.2,'  0.5  0  -0.5  0')
      END IF
!=================================================================
!======
      IF (AUTOTYP(PLNR).EQ.'PX'.OR.AUTOTYP(PLNR).EQ.'TX') THEN
        WRITE (UNIT=6,FMT='(''-> plot typ = PX or TX'')')
      IF (AUTOTYP(PLNR).EQ.'PX') THEN
        YRE=PRE
        YERR=PERR0
      END IF
      IF (AUTOTYP(PLNR).EQ.'TX') THEN
        YRE=TEM
        YERR=TERR0
      END IF
      J=0
      JJ=0
! re-define CCODE (>1 if not consistent)
!
! draw box around observed point
!       IF (CHKLIM.GT.0) J=2
       
        WRITE (UNIT=35,FMT='('' in plotexp(TX/PX): CCODE='',I4)') CCODE
        
       IF (CCODE.GT.0) JJ=2
! draw box around observed point
       WRITE (30,1030) J,CHKMIN,YRE-YERR,CHKMAX,YRE-YERR, &
       CHKMAX,YRE+YERR,CHKMIN,YRE+YERR
       WRITE (32,1030) J,CHKMIN,YRE-YERR,CHKMAX,YRE-YERR, &
       CHKMAX,YRE+YERR,CHKMIN,YRE+YERR
       WRITE (34,1030) JJ,CHKMIN,YRE-YERR,CHKMAX,YRE-YERR, &
       CHKMAX,YRE+YERR,CHKMIN,YRE+YERR
  1030 FORMAT ( &
       'LINIEN    ',I2,2X,'1  0   0',8(2X,1PE15.8),'   999  999  0')
! draw cross at starting point (XCSTART,YRE)
       IF (CHKLIM.NE.0) THEN
         WRITE (UNIT=30,FMT='(''FAT     0.02'')')
         WRITE (UNIT=32,FMT='(''FAT     0.02'')')
         WRITE (UNIT=34,FMT='(''FAT     0.02'')')
         WRITE (30,1031) CHKSTART,YRE
         WRITE (32,1031) CHKSTART,YRE
         WRITE (34,1031) CHKSTART,YRE
  1031 FORMAT ( &
       'PUNKTE    ',2X,'-2   0.1',2(2X,1PE15.8),'   999  999  0')
         WRITE (UNIT=30,FMT='(''FAT     0.01'')')
         WRITE (UNIT=32,FMT='(''FAT     0.01'')')
         WRITE (UNIT=34,FMT='(''FAT     0.01'')')
! draw line from starting point to observed point
         F1=(CHKMIN+CHKMAX)/2.0D0
         WRITE (30,1032) J,CHKSTART,YRE,F1,YRE
         WRITE (32,1032) J,CHKSTART,YRE,F1,YRE
         WRITE (34,1032) JJ,CHKSTART,YRE,F1,YRE
  1032 FORMAT ( &
       'LINIEN    ',I2,2X,'0  0   0',4(2X,1PE15.8),'   999  999  0')
       END IF
! draw dotted line from calculated to observed point
!         WRITE (30,1034) J,XCWERT,YCWERT,F1,F2
         F1=(CHKMIN+CHKMAX)/2.0D0
         WRITE (32,1034) J,XCWERT,YRE,F1,YRE
         WRITE (34,1034) J,XCWERT,YRE,F1,YRE
  1034 FORMAT ( &
       'STYLE   0.05   0.05   0.05   0.05',/, &
       'LINIEN   ',I2,2X,'0  0   0',4(2X,1PE15.8),'  999  999  0',/, &
       'STYLE   0.0   0.0   0.0   0.0')
! drow dot at calculated point
!       WRITE (30,1036) XCWERT,YCWERT
!!       WRITE (UNIT=32,FMT='(/,''FGRAY    0'')') 
!!       WRITE (UNIT=34,FMT='(/,''FGRAY    0'')') 
       WRITE (UNIT=32,FMT='(/,A)') FGRAU
       WRITE (UNIT=34,FMT='(/,A)') FGRAU
       WRITE (32,1036) PTSYMB,XCWERT,YRE
       WRITE (34,1036) PTSYMB,XCWERT,YRE
  1036 FORMAT ( &
       'PUNKTE    ',2X,I3,2X,'0.1',2(2X,1PE15.8),'   999  999  0')
!!       WRITE (UNIT=32,FMT='(/,''FGRAY    0.5'')') 
!!       WRITE (UNIT=34,FMT='(/,''FGRAY    0.0'')') 
       WRITE (UNIT=32,FMT='(/,A)') FGRAU
       WRITE (UNIT=34,FMT='(/,A)') FCOLOR(NPLAUT)
! write label
       I2=INDEX(EXPER,'  ')
       WRITE (30,1038) EXPER(1:I2),CHKMAX,YRE,GRSPTX
       WRITE (32,1038) EXPER(1:I2),CHKMAX,YRE,GRSPTX
       WRITE (34,1038) EXPER(1:I2),CHKMAX,YRE,GRSPTX
  1038 FORMAT ( &
        'TEXT    ',A,2(2X,1PE15.8),2X,0P,F6.2,'  0.5  0  -0.5  0')
      END IF
!=================================================================
!======
      IF (AUTOTYP(PLNR).EQ.'EX2') THEN
        WRITE (UNIT=6,FMT='(''-> plot typ = EX2'')')

       WRITE (6,1041) XRPH,XREL1,XREL2,XREL1CH,XREL2CH
 1041 FORMAT (5A)

      CALL GETVAL(XRPH,XREL1,XREL1DIV,IP,IS,IE,F1)
      CALL GETVAL(XRPH,XREL2,XREL2DIV,IP,IS,IE,F2)
      XCWERT=F1/(F1+F2)
      XCMIN=XCWERT-0.01D0
      XCMAX=XCWERT+0.01D0
      XCSTART=XCWERT
      IF (KDMIN.LT.1D-10) KDMIN=1D-10
      IF (KDMIN.GT.1D10) KDMIN=1D10
      IF (KDMAX.LT.1D-10) KDMAX=1D-10
      IF (KDMAX.GT.1D10) KDMAX=1D10
      IF (KD0.LT.1D-10) KD0=1D-10
      IF (KD0.GT.1D10) KD0=1D10
      IF (KDC.LT.1D-10) KDC=1D-10
      IF (KDC.GT.1D10) KDC=1D10
      YCMIN=DLOG(KDMIN)
      YCMAX=DLOG(KDMAX)
      YCSTART=DLOG(KD0)
      YCWERT=DLOG(KDC)


      WRITE (UNIT=6,FMT='(''x and ln KD?'',2f20.10)') XCWERT,YCWERT



!!       WRITE (6,5010) KD0,YCSTART
!! 5010 FORMAT ('KD0,YCSTART',2f20.10)
!!       WRITE (6,5020) KDC,YCWERT
!! 5020 FORMAT ('KDC,YCWERT',2f20.10)
!!       WRITE (6,5030) KDMIN,YCMIN
!! 5030 FORMAT ('KDMIN,YCMIN',2f20.10)
!!       WRITE (6,5040) KDMAX,YCMAX
!! 5040 FORMAT ('KDMAX,YCMAX',2f20.10)
!
      IF (YCSTART.LT.YUNTEN(PLNR)) YCSTART=YUNTEN(PLNR)
      IF (YCSTART.GT.YOBEN(PLNR)) YCSTART=YOBEN(PLNR)
      J=0
      JJ=0
       
        WRITE (UNIT=35,FMT='('' in plotexp(EX2): CCODE='',I4)') CCODE
        
!      CALL FIBLA(EXPEC,I1)
!      IF (EXPEC(I1:I1).EQ.'+') J=2
      IF (ISINC.EQ.1) JJ=2

!      WRITE (UNIT=6,FMT='(''HERE EX2'')')

       WRITE (30,1040) J,XCMIN,YCMIN,XCMAX,YCMIN, &
       XCMAX,YCMAX,XCMIN,YCMAX
       WRITE (32,1040) J,XCMIN,YCMIN,XCMAX,YCMIN, &
       XCMAX,YCMAX,XCMIN,YCMAX
       WRITE (34,1040) JJ,XCMIN,YCMIN,XCMAX,YCMIN, &
       XCMAX,YCMAX,XCMIN,YCMAX
  1040 FORMAT ( &
       'LINIEN    ',I2,2X,'1  0   0',8(2X,1PE15.8),'   999  999  0')
!
       F1=(XCMAX+XCMIN)/2.0D0
       F2=(YCMAX+YCMIN)/2.0D0
         WRITE (30,1042) J,XCSTART,YCSTART,F1,F2
         WRITE (32,1042) J,XCSTART,YCSTART,F1,F2
         WRITE (34,1042) JJ,XCSTART,YCSTART,F1,F2
  1042 FORMAT ( &
       'LINIEN    ',I2,2X,'0  0   0',4(2X,1PE15.8),'   999  999  0')

!         WRITE (30,1044) J,XCWERT,YCWERT,F1,F2
         WRITE (32,1044) J,XCWERT,YCWERT,F1,F2
         WRITE (34,1044) J,XCWERT,YCWERT,F1,F2
  1044 FORMAT ( &
       'STYLE   0.05   0.05   0.05   0.05',/, &
       'LINIEN   ',I2,2X,'0  0   0',4(2X,1PE15.8),'  999  999  0',/, &
       'STYLE   0.0   0.0   0.0   0.0')

!       WRITE (30,1046) PTSYMB,XCWERT,YCWERT
       WRITE (32,1046) PTSYMB,XCWERT,YCWERT
       WRITE (34,1046) PTSYMB,XCWERT,YCWERT
  1046 FORMAT ( &
       'PUNKTE    ',2X,I3,2X,'0.1',2(2X,1PE15.8),'   999  999  0')

       I2=INDEX(EXPER,'  ')
       WRITE (30,1048) EXPER(1:I2),XCMAX,(YCMIN+YCMAX)/2.0D0,GRSEX2
       WRITE (32,1048) EXPER(1:I2),XCMAX,(YCMIN+YCMAX)/2.0D0,GRSEX2
       WRITE (34,1048) EXPER(1:I2),XCMAX,(YCMIN+YCMAX)/2.0D0,GRSEX2
  1048 FORMAT ( &
        'TEXT    ',A,2(2X,1PE15.8),2X,0P,F6.2,'  0.5  0  -0.5  0')
      END IF
!=================================================================
!======
      IF (AUTOTYP(PLNR).EQ.'LKD') THEN
        WRITE (UNIT=6,FMT='(''-> plot typ = LKD'')')
      J=0
      JJ=0
      IF (KEY1.EQ.'1000/T') THEN
       XCMIN=1000.0D0/(TEM+273.15D0+TERR0)
       XCMAX=1000.0D0/(TEM+273.15D0-TERR0)
       XCSTART=1000.0D0/(TEM+273.15D0)
      END IF
      IF (KEY1.EQ.'TC') THEN
       XCMIN=TEM-TERR0
       XCMAX=TEM+TERR0
       XCSTART=TEM
      END IF
      IF (KEY1.EQ.'TK') THEN
       XCMIN=TEM-TERR0+273.15D0
       XCMAX=TEM+TERR0+273.15D0
       XCSTART=TEM+273.15D0
      END IF
      IF (KDMIN.LT.1D-10) KDMIN=1D-10
      IF (KDMIN.GT.1D10) KDMIN=1D10
      IF (KDMAX.LT.1D-10) KDMAX=1D-10
      IF (KDMAX.GT.1D10) KDMAX=1D10
      IF (KD0.LT.1D-10) KD0=1D-10
      IF (KD0.GT.1D10) KD0=1D10
      IF (KDC.LT.1D-10) KDC=1D-10
      IF (KDC.GT.1D10) KDC=1D10
      YCMIN=DLOG(KDMIN)
      YCMAX=DLOG(KDMAX)
      YCSTART=DLOG(KD0)
      YCWERT=DLOG(KDC)
      IF (YCSTART.LT.PLYMIN) YCSTART=PLYMIN
      IF (YCSTART.GT.PLYMAX) YCSTART=PLYMAX
!      CALL FIBLA(EXPEC,I1)
!      IF (EXPEC(I1:I1).EQ.'+') J=2
! draw box around observed point
       
        WRITE (UNIT=35,FMT='('' in plotexp(LKD): CCODE='',I4)') CCODE
        
      IF (CCODE.GT.0) JJ=2
       WRITE (30,1050) J,XCMIN,YCMIN,XCMAX,YCMIN, &
       XCMAX,YCMAX,XCMIN,YCMAX
       WRITE (32,1050) J,XCMIN,YCMIN,XCMAX,YCMIN, &
       XCMAX,YCMAX,XCMIN,YCMAX
       WRITE (34,1050) JJ,XCMIN,YCMIN,XCMAX,YCMIN, &
       XCMAX,YCMAX,XCMIN,YCMAX
  1050 FORMAT ( &
       'LINIEN    ',I2,2X,'1  0   0',8(2X,1PE15.8),'   999  999  0')
! droxaw line from start to middle of box
       F1=(XCMAX+XCMIN)/2.0D0
       F2=(YCMAX+YCMIN)/2.0D0
       WRITE (30,1052) J,XCSTART,YCSTART,F1,F2
       WRITE (32,1052) J,XCSTART,YCSTART,F1,F2
       WRITE (34,1052) JJ,XCSTART,YCSTART,F1,F2
  1052 FORMAT ( &
       'LINIEN    ',I2,2X,'0  0   0',4(2X,1PE15.8),'   999  999  0')
! draw dotted line from calculated point to middle of box
!       WRITE (30,1054) J,XCSTART,YCWERT,F1,F2
       WRITE (32,1054) J,XCSTART,YCWERT,F1,F2
       WRITE (34,1054) J,XCSTART,YCWERT,F1,F2
  1054 FORMAT ( &
       'STYLE   0.05   0.05   0.05   0.05',/, &
       'LINIEN   ',I2,2X,'0  0   0',4(2X,1PE15.8),'  999  999  0',/, &
       'STYLE   0.0   0.0   0.0   0.0')
! draw point at calculated point
!       WRITE (30,1056) PTSYMB,XCSTART,YCWERT
       WRITE (32,1056) PTSYMB,XCSTART,YCWERT
       WRITE (34,1056) PTSYMB,XCSTART,YCWERT
  1056 FORMAT ( &
       'PUNKTE    ',2X,I3,2X,'0.1',2(2X,1PE15.8),'   999  999  0')
! write label
       I2=INDEX(EXPER,'  ')
       WRITE (30,1058) EXPER(1:I2),XCMAX,(YCMIN+YCMAX)/2.0D0,GRSLKD
       WRITE (32,1058) EXPER(1:I2),XCMAX,(YCMIN+YCMAX)/2.0D0,GRSLKD
       WRITE (34,1058) EXPER(1:I2),XCMAX,(YCMIN+YCMAX)/2.0D0,GRSLKD
  1058 FORMAT ( &
        'TEXT    ',A,2(2X,1PE15.8),2X,0P,F6.2,'  0.5  0  -0.5  0')
      END IF
!=================================================================
!======
      IF (AUTOTYP(PLNR).EQ.'LKR') THEN
        WRITE (UNIT=6,FMT='(''-> plot typ = LKR'')')
      J=0
      JJ=0
      CALL FIBLA(EXPEC,I1)
      IF (EXPEC(I1:I1).EQ.'+') J=2
       
        WRITE (UNIT=35,FMT='('' in plotexp(LKR): CCODE='',I4)') CCODE
        
      IF (CCODE.GT.0) JJ=2
       WRITE (30,1060) J,TEM-TERR0,CHKMIN,TEM+TERR0,CHKMIN, &
       TEM+TERR0,CHKMAX,TEM-TERR0,CHKMAX
       WRITE (32,1060) J,TEM-TERR0,CHKMIN,TEM+TERR0,CHKMIN, &
       TEM+TERR0,CHKMAX,TEM-TERR0,CHKMAX
       WRITE (34,1060) JJ,TEM-TERR0,CHKMIN,TEM+TERR0,CHKMIN, &
       TEM+TERR0,CHKMAX,TEM-TERR0,CHKMAX
  1060 FORMAT ( &
       'LINIEN    ',I2,2X,'1  0   0',8(2X,1PE15.8),'   999  999  0')
       IF (.NOT.USEDFOR) THEN
       WRITE (30,1062) TEM-TERR0,CHKMIN,TEM+TERR0,CHKMAX
       WRITE (30,1062) TEM-TERR0,CHKMAX,TEM+TERR0,CHKMIN
       WRITE (32,1062) TEM-TERR0,CHKMIN,TEM+TERR0,CHKMAX
       WRITE (32,1062) TEM-TERR0,CHKMAX,TEM+TERR0,CHKMIN
       WRITE (34,1062) TEM-TERR0,CHKMIN,TEM+TERR0,CHKMAX
       WRITE (34,1062) TEM-TERR0,CHKMAX,TEM+TERR0,CHKMIN
  1062 FORMAT ( &
       'LINIEN     0  0  0   0',4(2X,1PE15.8),'   999  999  0')
       END IF
       I2=INDEX(EXPER,'  ')
       WRITE (30,1064) EXPER(1:I2),TEM+TERR0,(CHKMIN+CHKMAX)/2.0D0,GRSLKR
       WRITE (32,1064) EXPER(1:I2),TEM+TERR0,(CHKMIN+CHKMAX)/2.0D0,GRSLKR
!       WRITE (34,1064) EXPER(1:I2),TEM+TERR0,(CHKMIN+CHKMAX)/2.0D0,GRSLKR
  1064 FORMAT ( &
        'TEXT    ',A,2(2X,1PE15.8),2X,0P,F6.2,'  0.5  0  -0.5  0')
      END IF
!=================================================================
!======
      IF (AUTOTYP(PLNR).EQ.'PT') THEN
        WRITE (UNIT=6,FMT='(''-> plot typ = PT'')')
      J=0
      JJ=0
      CALL FIBLA(EXPEC,I1)
      IF (EXPEC(I1:I1).EQ.'+') J=2
       
        WRITE (UNIT=35,FMT='('' in plotexp(PT): CCODE='',I4)') CCODE
        
      IF (CCODE.GT.0) JJ=2
        WRITE (UNIT=6,FMT='(''WRITE 30'')')
       WRITE (30,1070) J,TEM-TERR0,PRE-PERR0,TEM+TERR0,PRE-PERR0, &
       TEM+TERR0,PRE+PERR0,TEM-TERR0,PRE+PERR0
       WRITE (32,1070) J,TEM-TERR0,PRE-PERR0,TEM+TERR0,PRE-PERR0, &
       TEM+TERR0,PRE+PERR0,TEM-TERR0,PRE+PERR0
       WRITE (34,1070) JJ,TEM-TERR0,PRE-PERR0,TEM+TERR0,PRE-PERR0, &
       TEM+TERR0,PRE+PERR0,TEM-TERR0,PRE+PERR0
  1070 FORMAT ( &
       'LINIEN    ',I2,2X,'1  0   0',8(2X,1PE15.8),'   999  999  0')
       IF (.NOT.USEDFOR) THEN
       WRITE (30,1072) TEM-TERR0,PRE-PERR0,TEM+TERR0,PRE+PERR0
       WRITE (30,1072) TEM-TERR0,PRE+PERR0,TEM+TERR0,PRE-PERR0
       WRITE (32,1072) TEM-TERR0,PRE-PERR0,TEM+TERR0,PRE+PERR0
       WRITE (32,1072) TEM-TERR0,PRE+PERR0,TEM+TERR0,PRE-PERR0
       WRITE (34,1072) TEM-TERR0,PRE-PERR0,TEM+TERR0,PRE+PERR0
       WRITE (34,1072) TEM-TERR0,PRE+PERR0,TEM+TERR0,PRE-PERR0
  1072 FORMAT ( &
       'LINIEN     0  0  0   0',4(2X,1PE15.8),'   999  999  0')
       END IF
! write label
       I2=INDEX(EXPER,'  ')
       WRITE (30,1074) EXPER(1:I2),TEM+TERR0,PRE,GRSPT
       WRITE (32,1074) EXPER(1:I2),TEM+TERR0,PRE,GRSPT
       WRITE (34,1074) EXPER(1:I2),TEM+TERR0,PRE,GRSPT
  1074 FORMAT ( &
        'TEXT    ',A,2(2X,1PE15.8),2X,0P,F6.2,'  0.5  0  -0.5  0')
!       TANG=TANG+5.0D0
      END IF
!=================================================================
!======
      IF (AUTOTYP(PLNR).EQ.'TXC') THEN
        WRITE (UNIT=6,FMT='(''-> plot typ = TXC'')')
      J=0
      JJ=0
      CALL FIBLA(EXPEC,I1)
      IF (EXPEC(I1:I1).EQ.'+') J=2
       
        WRITE (UNIT=35,FMT='('' in plotexp(TXC): CCODE='',I4)') CCODE
        
      IF (CCODE.GT.0) JJ=2
       WRITE (30,1080) J,XCO2-XCO2ERR,TEM-TERR0,XCO2+XCO2ERR, &
       TEM-TERR0,XCO2+XCO2ERR,TEM+TERR0,XCO2-XCO2ERR,TEM+TERR0
       WRITE (32,1080) J,XCO2-XCO2ERR,TEM-TERR0,XCO2+XCO2ERR, &
       TEM-TERR0,XCO2+XCO2ERR,TEM+TERR0,XCO2-XCO2ERR,TEM+TERR0
       WRITE (34,1080) JJ,XCO2-XCO2ERR,TEM-TERR0,XCO2+XCO2ERR, &
       TEM-TERR0,XCO2+XCO2ERR,TEM+TERR0,XCO2-XCO2ERR,TEM+TERR0
  1080 FORMAT ( &
       'LINIEN    ',I2,2X,'1  0   0',8(2X,1PE15.8),'   999  999  0')
       IF (.NOT.USEDFOR) THEN
       WRITE (30,1082) XCO2-XCO2ERR,TEM-TERR0,XCO2+XCO2ERR,TEM+TERR0
       WRITE (30,1082) XCO2-XCO2ERR,TEM+TERR0,XCO2+XCO2ERR,TEM-TERR0
       WRITE (32,1082) XCO2-XCO2ERR,TEM-TERR0,XCO2+XCO2ERR,TEM+TERR0
       WRITE (32,1082) XCO2-XCO2ERR,TEM+TERR0,XCO2+XCO2ERR,TEM-TERR0
       WRITE (34,1082) XCO2-XCO2ERR,TEM-TERR0,XCO2+XCO2ERR,TEM+TERR0
       WRITE (34,1082) XCO2-XCO2ERR,TEM+TERR0,XCO2+XCO2ERR,TEM-TERR0
  1082 FORMAT ( &
       'LINIEN     0  0  0   0',4(2X,1PE15.8),'   999  999  0')
       END IF
       I2=INDEX(EXPER,'  ')
       WRITE (30,1084) EXPER(1:I2),XCO2+XCO2ERR,TEM,GRSTXC
       WRITE (32,1084) EXPER(1:I2),XCO2+XCO2ERR,TEM,GRSTXC
       WRITE (34,1084) EXPER(1:I2),XCO2+XCO2ERR,TEM,GRSTXC
  1084 FORMAT ( &
        'TEXT    ',A,2(2X,1PE15.8),2X,0P,F6.2,'  0.5  0  -0.5  0')
      END IF
!=================================================================
!======
      IF (AUTOTYP(PLNR).EQ.'ISO') THEN
        WRITE (UNIT=6,FMT='(''-> plot typ = ISO'')')
      J=0
      JJ=0
      CALL FIBLA(EXPEC,I1)
      IF (EXPEC(I1:I1).EQ.'+') J=2
       
        WRITE (UNIT=35,FMT='('' in plotexp(ISO): CCODE='',I4)') CCODE
        
      IF (CCODE.GT.0) JJ=2
       WRITE (30,1090) J,TC-TERR0,PRE-PERR0,TC+TERR0,PRE-PERR0, &
       TC+TERR0,PRE+PERR0,TC-TERR0,PRE+PERR0
       WRITE (32,1090) J,TC-TERR0,PRE-PERR0,TC+TERR0,PRE-PERR0, &
       TC+TERR0,PRE+PERR0,TC-TERR0,PRE+PERR0
       WRITE (34,1090) JJ,TC-TERR0,PRE-PERR0,TC+TERR0,PRE-PERR0, &
       TC+TERR0,PRE+PERR0,TC-TERR0,PRE+PERR0
  1090 FORMAT ( &
       'LINIEN    ',I2,2X,'1  0   0',8(2X,1PE15.8),'   999  999  0')
       IF (.NOT.USEDFOR) THEN
       WRITE (30,1092) TC-TERR0,PRE-PERR0,TC+TERR0,PRE+PERR0
       WRITE (30,1092) TC-TERR0,PRE+PERR0,TC+TERR0,PRE-PERR0
       WRITE (32,1092) TC-TERR0,PRE-PERR0,TC+TERR0,PRE+PERR0
       WRITE (32,1092) TC-TERR0,PRE+PERR0,TC+TERR0,PRE-PERR0
       WRITE (34,1092) TC-TERR0,PRE-PERR0,TC+TERR0,PRE+PERR0
       WRITE (34,1092) TC-TERR0,PRE+PERR0,TC+TERR0,PRE-PERR0
  1092 FORMAT ( &
       'LINIEN     0  0  0   0',4(2X,1PE15.8),'   999  999  0')
       END IF
       I2=INDEX(EXPER,'  ')
       WRITE (30,1094) EXPER(1:I2),TC+TERR0,PRE,GRSISO
       WRITE (32,1094) EXPER(1:I2),TC+TERR0,PRE,GRSISO
       WRITE (34,1094) EXPER(1:I2),TC+TERR0,PRE,GRSISO
  1094 FORMAT ( &
        'TEXT    ',A,2(2X,1PE15.8),2X,0P,F6.2,'  0.5  0  -0.5  0')
!       WRITE (UNIT=CH20,FMT='(F20.10)') CHKMIN
!       CALL ZAHL(CH20,I1,I2)
       I1=5
       CALL MAKEZAHL(CHKMAX,I1,CH20,I2)
       WRITE (30,1096) CH20(1:I2),TC-TERR0,PRE+PERR0,GRSISO
       WRITE (32,1096) CH20(1:I2),TC-TERR0,PRE+PERR0,GRSISO
       WRITE (34,1096) CH20(1:I2),TC-TERR0,PRE+PERR0,GRSISO
  1096 FORMAT ( &
        'TEXT    ',A,2(2X,1PE15.8),2X,0P,F6.2,'  0.5  0  -0.5  0')
!       WRITE (UNIT=CH20,FMT='(F20.10)') CHKMAX
!       CALL ZAHL(CH20,I1,I2)
       I1=5
       CALL MAKEZAHL(CHKMIN,I1,CH20,I2)
       WRITE (30,1098) CH20(1:I2),TC-TERR0,PRE-PERR0,GRSISO
       WRITE (32,1098) CH20(1:I2),TC-TERR0,PRE-PERR0,GRSISO
       WRITE (34,1098) CH20(1:I2),TC-TERR0,PRE-PERR0,GRSISO
  1098 FORMAT ( &
        'TEXT    ',A,2(2X,1PE15.8),2X,0P,F6.2,'  0.5  0  -0.5  0')
      END IF
!=================================================================
!=================================================================
      TEM=TEMNOW
!      
!      
      CLOSE (UNIT=30)
      CLOSE (UNIT=32)
      CLOSE (UNIT=34)

!
      RETURN
      END
!-----
!*************************************************************
!*************************************************************
      SUBROUTINE DOMSCRIPT
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
      include 'checkdb.cmn'
!
!-----END OF COMMON VARIABLES
!
      INTEGER(4) I1,I2,I,IJ,IO
      CHARACTER(250) CH001
!----------------------------------------------------------------
!--    make domino script file 31   .txt
!----------------------------------------------------------------
      IF (PRTEST) WRITE (UNIT=6,FMT='(''-> DOMSCRIPT'')')
      IF (PRTEST) WRITE (UNIT=35,FMT='(''-> DOMSCRIPT'')')
!
!       CALL LABLA(PLFILE,I1)
!       CALL LABLA(DBUSED,I2)
!       PLFPLUS=PLFILE(1:I1)//'_'//DBUSED(1:I2)
       CALL LABLA(AUTOPLOT(NPLOTS),I1)
      OPFILE=NEWDIR(1:LDIR)//AUTOPLOT(NPLOTS)(1:I1)//'.txt'
!      CALL PUST(6,'Open file 31: '//OPFILE)
      CALL LABLA(OPFILE,IO)
!-- open .txt script for domino
       OPEN (UNIT=31,FILE=OPFILE(1:IO),STATUS='UNKNOWN')
       CH001='script.'//AUTOPLOT(NPLOTS)(1:I1)//'.txt'
       CALL PUST(31,CH001)
       CH001=NEWDIR(1:LDIR)//AUTOPLOT(NPLOTS)(1:I1)//'.plt'
       CALL PUST(31,CH001)
       CALL LABLA(filename(dbs),I1)
       CALL LABLA(PICKSTRING,I2)
       CH001=filename(dbs)(1:I1)//'   '//PICKSTRING(1:I2)
       CALL PUST(31,CH001)
      WRITE (31,1052) PRAT,LO1MAX,EQUALX,TEST,DXMIN,DXSCAN, &
      DXSTAR,STPSTA,STPMAX,GCMAX
 1052 FORMAT (F10.4,2X,I5,2X,F10.6,2X,E14.7,2X,E14.7,2X,E14.7, &
      E14.7,2X,I5,2X,I6,2X,I6)
!=====
!=====
       DO I=1,6
       CALL LABLA(CHLINE(I),I2)
       IF (I2.NE.0) THEN
       CH001='1   '//CHLINE(I)(1:I2)//'   *'
       ELSE
       CH001=' '
       END IF
       CALL PUST(31,CH001)
       END DO
       CALL PUST(31,XLINE)
       CALL PUST(31,YLINE)
       CALL PUST(31,WASLINE)
       CALL PUST(31,LABLINE)
       WRITE (UNIT=31,FMT='(1PE14.7,2X,1PE14.7)') TSIM,PSIM
!=====
       CALL LABLA(AUTOPLOT(NPLOTS),I1)
       CH001=NEWDIR(1:LDIR)//'_'//AUTOPLOT(NPLOTS)(1:I1)//'_pix'
       CALL PUST(31,CH001)
       CLOSE (UNIT=31)
!----------------------------------------------------------------
!--    make domjob file 31   _job
!----------------------------------------------------------------
       CALL LABLA(JONAME,IJ)
       CH001=AUTOPLOT(NPLOTS)(1:I1)//JONAME(1:IJ)
       CALL LABLA(CH001,IJ)
      OPFILE=NEWDIR(1:LDIR)//CH001(1:IJ)
!      CALL PUST(6,'Open file 31: '//OPFILE)
      CALL LABLA(OPFILE,IO)
!-- open _job for domino (runs script .txt)
       OPEN (UNIT=31,FILE=OPFILE(1:IO),STATUS='UNKNOWN')

       CH001='domino  '// &
       NEWDIR(1:LDIR)//AUTOPLOT(NPLOTS)(1:I1)//'.txt'
       CALL PUST(31,CH001)
!
       CH001='guzzler  '// &
       NEWDIR(1:LDIR)//AUTOPLOT(NPLOTS)(1:I1)//'.plt  '// &
       NEWDIR(1:LDIR)//AUTOPLOT(NPLOTS)(1:I1)//'.cln  '// &
       NEWDIR(1:LDIR)//AUTOPLOT(NPLOTS)(1:I1)// &
       '.rxn'
       CALL PUST(31,CH001)
!
       CH001='explot  '// &
       NEWDIR(1:LDIR)//AUTOPLOT(NPLOTS)(1:I1)//'_0exp  '// &
       NEWDIR(1:LDIR)//AUTOPLOT(NPLOTS)(1:I1)//'_0exp.ps'
       CALL PUST(31,CH001)
!
       CH001='cat2  '// &
       NEWDIR(1:LDIR)//AUTOPLOT(NPLOTS)(1:I1)//'.cln  ' &
       //NEWDIR(1:LDIR)//AUTOPLOT(NPLOTS)(1:I1)//'_exn   '// &
       NEWDIR(1:LDIR)//AUTOPLOT(NPLOTS)(1:I1)//'_com'
       CALL PUST(31,CH001)

       CH001='explot  '// &
       NEWDIR(1:LDIR)//AUTOPLOT(NPLOTS)(1:I1)//'_com  '// &
       NEWDIR(1:LDIR)//AUTOPLOT(NPLOTS)(1:I1)//'_com.ps'
       CALL PUST(31,CH001)
!!       CH001='cat  '//AUTOPLOT(NPLOTS)(1:I1)//'.cln  ' &
!!       //AUTOPLOT(NPLOTS)(1:I1)//'_exn2  >  '// &
!
       CH001='cat2  '// &
       NEWDIR(1:LDIR)//AUTOPLOT(NPLOTS)(1:I1)//'.cln  ' &
       //NEWDIR(1:LDIR)//AUTOPLOT(NPLOTS)(1:I1)//'_exn2   '// &
       NEWDIR(1:LDIR)//AUTOPLOT(NPLOTS)(1:I1)//'_com2'
       CALL PUST(31,CH001)

       CH001='explot  '// &
       NEWDIR(1:LDIR)//AUTOPLOT(NPLOTS)(1:I1)//'_com2  '// &
       NEWDIR(1:LDIR)//AUTOPLOT(NPLOTS)(1:I1)//'_com2.ps'
       CALL PUST(31,CH001)

       CLOSE (UNIT=31)
       CALL LABLA(AUTOPLOT(NPLOTS),I1)
       CALL MAKEEXEC(NEWDIR(1:LDIR)//AUTOPLOT(NPLOTS)(1:I1)//'_job')
!
      RETURN
      END
!-----
!*************************************************************
!*************************************************************
      SUBROUTINE PRTVAL
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
      include 'checkdb.cmn'
!
!-----END OF COMMON VARIABLES
      CHARACTER(250) CH001
      INTEGER(4) IP,IE,IS,I1,I2,J,IST,I3
      REAL(8) CHECKWERT
      CHARACTER(50) CHXX
!--
!+++
      IF (PRTEST) THEN
        WRITE (UNIT=scr,FMT='(''-> printing values (PRTVAL)'')')
        WRITE (UNIT=35,FMT='(''-> printing values (PRTVAL)'')')
      END IF
!+++
!---- assign value
      CALL GETVAL(CHKPH,CHKEL,CHKELDIV,IP,IS,IE,CHECKWERT)
!!      IF (IP.NE.0.AND.IE.NE.0) THEN
       CALL LABLA(CHKPH,I1)
       CALL LABLA(CHKELCH,I2)
!!       WRITE (CH001,1000) CHKPH(1:I1),CHKELCH(1:I2),CHECKWERT
!! 1000 FORMAT (5X,A,2X,A,3X,F14.6)
!!       CALL PUST(scr,CH001)
!!       CALL PUST(35,CH001)
!!      END IF
!--
      IST=8
      CALL MAKEZAHL(CHECKWERT,IST,CHXX,I3)
      WRITE (UNIT=scr,FMT='(5X,A,2X,A,3X,A)') &
             CHKPH(1:I1),CHKELCH(1:I2),CHXX(1:I3)
      WRITE (UNIT=35,FMT='(5X,A,2X,A,3X,A)') &
             CHKPH(1:I1),CHKELCH(1:I2),CHXX(1:I3)
!--
      IF (IP.EQ.0) THEN
       CALL LABLA(CHKPH,I1)
       CH001='     '//CHKPH(1:I1)
       CALL LABLA(CH001,I1)
       DO J=I1+2,112
        CH001(J:J)='.'
       END DO
       CH001(114:)='phase not stable 2'
       CALL PUST(scr,CH001)
       CALL PUST(35,CH001)
      END IF
!--
!--
      IF (IE.EQ.0.AND.IP.NE.0) THEN
       CALL LABLA(CHKPH,I1)
       CALL LABLA(CHKEL,I2)
       CH001='     '//CHKPH(1:I1)//' : '//CHKEL(1:I2)
       CALL LABLA(CH001,I1)
       DO J=I1+2,113
        CH001(J:J)='-'
       END DO
       CH001(115:)='unknown element'
       CALL PUST(scr,CH001)
       CALL PUST(35,CH001)
      END IF
!--
      RETURN
      END
!
!-----
!*************************************************************
!*************************************************************
      SUBROUTINE CHECKBIN
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
      include 'checkdb.cmn'
!
!-----END OF COMMON VARIABLES
!      CHARACTER(500) CH001
      INTEGER(4) I1,I2,IP,IS,IE
      REAL(8) F1,F2,TORIG,PORIG

      CALL NURVONPT
      CALL CALSTR
      CALL PRININ
      CALL THERIA
        WRITE (UNIT=35,FMT='(/,''TC,P '',2F20.10)') TC,P
        WRITE (UNIT=35,FMT='(/,''TEM,PRE '',2F20.10)') TEM,PRE
      WRITE (UNIT=35,FMT='('' BIXPH,BIXELCH '',2A20)') BIXPH,BIXELCH
      WRITE (UNIT=35,FMT='('' BIYPH,BIYELCH '',2A20)') BIYPH,BIYELCH
      CALL GETVAL(BIXPH,BIXEL,BIXELDIV,IP,IS,IE,F1)
      WRITE (UNIT=6,FMT='(''F1 = '',F20.10)') F1
      WRITE (UNIT=35,FMT='(''F1 = '',F20.10)') F1
      CALL GETVAL(BIYPH,BIYEL,BIYELDIV,IP,IS,IE,F2)
      WRITE (UNIT=6,FMT='(''F2 = '',F20.10)') F2
      WRITE (UNIT=35,FMT='(''F2 = '',F20.10)') F2

      TORIG=TEM
      PORIG=PRE
      DO I1=-1,1,2
        DO I2=-1,1,2

        TC=TORIG+REAL(I1)*TERR0
        P=PORIG+REAL(I2)*PERR0
        WRITE (UNIT=35,FMT='(/,''TC,P '',2F20.10)') TC,P
      CALL NURVONPT
      CALL CALSTR
      CALL PRININ
      CALL THERIA
      CALL GETVAL(BIXPH,BIXEL,BIXELDIV,IP,IS,IE,F1)
      WRITE (UNIT=6,FMT='(''F1 = '',F20.10)') F1
      WRITE (UNIT=35,FMT='(''F1 = '',F20.10)') F1
      CALL GETVAL(BIYPH,BIYEL,BIYELDIV,IP,IS,IE,F2)
      WRITE (UNIT=6,FMT='(''F2 = '',F20.10)') F2
      WRITE (UNIT=35,FMT='(''F2 = '',F20.10)') F2

        END DO
      END DO

      TEM=TORIG
      PRE=PORIG
!--
      RETURN
      END
!
!
!-----
!*************************************************************
!*************************************************************
      SUBROUTINE CHECKBIVA(FBMIN,FBMAX)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
      include 'checkdb.cmn'
!
!-----END OF COMMON VARIABLES
      CHARACTER(500) CH001
      CHARACTER(50) TWI(3),TEXT1,TEXTQ
      INTEGER(4) I,II,IP,IS,IE,IFF,I1,I2,i4,I5,SIGI(2),PLNR
      REAL(8) FBMIN,FBMAX,FA,FB,DDX,WERT(3),FF(3),XAV,XERR, &
      YAV,PROZ

      WRITE (UNIT=6,FMT='(''->CHECKBIVA'')')
      WRITE (UNIT=35,FMT='(''->CHECKBIVA'')')
       CALL LABLA(DBUSED,I2)
      WRITE (UNIT=6,FMT='(''->DBUSED'',A)') DBUSED(1:I2)
      WRITE (UNIT=35,FMT='(''->DBUSED'',A)') DBUSED(1:I2)

      CALL PRTFIRST

      ISINC=0
!      NINC=0

      DDX=1D-7
      TWI(1)='too small            X  |-----|  '
      TWI(2)='too large               |-----|  X'
      TWI(3)='OK                      |--X--|  '

      FF(1)=FBMIN
      FF(2)=FBMAX
      FF(3)=(FBMIN+FBMAX)/2.0D0

      DO IFF=1,3
      FB=FF(IFF)
      IF (FB.LT.0.0D0) FB=0.0D0
      IF (FB.GT.1.0D0) FB=1.0D0
      FA=1.0D0-FB
      DO,I=1,NUN
       BULK(I)=0.0D0
      END DO
      DO I=1,NUN
        II=CHMCOD(I)
        BULK(I)=FA*CHEBI(1,II)+FB*CHEBI(2,II)
      END DO
      IF (FB.LT.1D-7) THEN
        DO I=1,NUN
          II=CHMCOD(I)
          BULK(I)=(1.0D0-DDX)*CHEBI(1,II)+DDX*CHEBI(2,II)
        END DO
      END IF
      IF (FB.GT.(1.0D0-DDX)) THEN
        DO I=1,NUN
          II=CHMCOD(I)
          BULK(I)=DDX*CHEBI(1,II)+(1.0D0-DDX)*CHEBI(2,II)
        END DO
      END IF
!----
      CALL NURVONPT
      CALL CALSTR
      CALL PRININ
      CALL THERIA
      CALL GETVAL(BIXPH,BIXEL,BIXELDIV,IP,IS,IE,WERT(IFF))
      WRITE (UNIT=6,FMT='(''WERT(IFF)='',F20.10)') WERT(IFF)
      CALL GETVAL(BIYPH,BIYEL,BIYELDIV,IP,IS,IE,WERT(IFF))
      WRITE (UNIT=6,FMT='(''WERT(IFF)='',F20.10)') WERT(IFF)

      END DO


      XAV=FF(3)
      XERR=DABS(FBMIN-FBMAX)/2.0D0
      YAV=(CHKMIN+CHKMAX)/2.0D0

      CALL BUBU(CH001)
      CALL LABLA(CH001,I1)
      WRITE (35,1000) FF(3),CH001(1:I1)
 1000 FORMAT (5X,'bulk for x = ',F8.3,' : ',A)
      CALL LABLA(filename(dbs),I2)
      WRITE (35,1002) TC,P,filename(dbs)(1:I2)
 1002 FORMAT (5X,'TC =',F8.2,'  P =',F9.2,5X, &
      'database  :  ',A)
      WRITE (35,1004) TERR0,PERR0
 1004 FORMAT (5X,'+-',2X,F8.2,5X,F9.2)
      WRITE (35,FMT='(5X,55A1)') ('-',I=1,55)

      WRITE (35,1050) XAV,XERR
 1050 FORMAT (5X,'binary  X = ',2X,F8.3,'    +/- ',F8.3)
      CALL LABLA(BIYPH,I1)
      CALL LABLA(BIYELCH,I2)
      WRITE (35,1052) BIYPH(1:I1),BIYELCH(1:I2),CHKMIN,CHKMAX
 1052 FORMAT(5X,'value: ',A,2X,A,' =    minimum:',F10.4,'    maximum:',F10.4)

      
      IF (WERT(1).LT.CHKMIN) SIGI(1)=1
      IF (WERT(1).GT.CHKMAX) SIGI(1)=2
      IF (WERT(1).GT.CHKMIN.AND.WERT(1).LT.CHKMAX) SIGI(1)=3
      IF (WERT(2).LT.CHKMIN) SIGI(2)=1
      IF (WERT(2).GT.CHKMAX) SIGI(2)=2
      IF (WERT(2).GT.CHKMIN.AND.WERT(2).LT.CHKMAX) SIGI(2)=3

!!      TEXT1='| outside uncertainties'
      WRITE (TEXT1,1060) ('+',I=1,23)
 1060 FORMAT ('|',23A1,' outside uncertainties')
      TEXTQ='outside uncertainties'

!      CCODE=1
       IF (ISINC.EQ.0) ISINC=1
!!!      NINC=NINC+1
      IF (SIGI(1).EQ.1.AND.SIGI(2).EQ.2) THEN
        TEXT1='|OK'
        TEXTQ='OK'
        CCODE=0
        ISINC=0
      END IF
      IF (SIGI(1).EQ.2.AND.SIGI(2).EQ.1) THEN
        TEXT1='|OK'
        TEXTQ='OK'
        CCODE=0
        ISINC=0
      END IF
      IF (SIGI(1).EQ.3.OR.SIGI(2).EQ.3)  THEN
        TEXT1='|OK'
        TEXTQ='OK'
        CCODE=0
        ISINC=0
      END IF

      IF (ISINC.EQ.1) THEN
        CCODE=1
!!        NINC=NINC+1
      END IF

      
      DO I=1,2
      PROZ=100.0D0*(WERT(I)-YAV)/YAV
      CALL LABLA(TWI(SIGI(I)),I1)
      WRITE (35,1054) FF(I),WERT(I),PROZ,TWI(SIGI(I))(1:I1)
 1054 FORMAT (5X,'X = ',F8.3,5X,'value =',F10.3,18X,F7.2,' % |',A)
      END DO
      CALL LABLA(TEXT1,I1)
      WRITE (35,1056) TEXT1(1:I1)
 1056 FORMAT (67X,A)

!summary
!      CALL LABLA(BIYELCH,I1)
!      I2=INDEX(EXPER,'  ')
!      CALL LABLA(BIYPH,I4)
!      CALL LABLA(TEXTQ,I5)
!      CH001=' '
!      WRITE (UNIT=CH001,FMT='(A,2X,A)') CHKPH(1:16),CHKELCH(1:8)
!      CALL LABLA(CH001,I2)
!      WRITE (39,2015) TEM,PRE,F1,CH001(1:39),TEXTQ(1:I5)
! 2015 FORMAT (3X,'T = ',F8.2,'  P = ',F10.1,2X,'value: ',F15.6,2X,A,2X,A)

      CALL LABLA(TEXTQ,I5)
      CALL LABLA(EXPER,I1)
      IF (I1.LT.10) I1=10
        IF (PEXP.EQ.0) THEN
          WRITE (39,4010) EXPER(1:I1),TEM,PRE,XAV,YAV,TEXTQ(1:I5)
 4010     FORMAT (/,3X,A,2X,'binary:  T = ',F8.1,'  P =',F10.1, &
          '   X =',F8.3,'   val = ',f10.3,20X,A)
        ELSE
          CALL LABLA(BLAN,I4)
          WRITE (39,4011) BLAN(1:I4),TEM,PRE,XAV,YAV,TEXTQ(1:I5)
 4011     FORMAT (3X,A,2X,'binary:  T = ',F8.1,'  P =',F10.1, &
          '   X =',F8.3,'   val = ',f10.3,20X,A)
        END IF
        PEXP=PEXP+1
!!-----end summary
!-------------------------------------------------------------
!---- make plot
!-------------------------------------------------------------
      DO I=1,NPLOTS
        IF (AUTOTYP(I).EQ.'BIV') THEN
          PLNR=I
          XCHKMIN=FBMIN
          XCHKMAX=FBMAX
          CALL PLOTEXP(PLNR)
        END IF
      END DO

!--
      RETURN
      END
!-----
!*************************************************************
!*************************************************************
      SUBROUTINE CHECKVAL
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
      include 'checkdb.cmn'
!
!-----END OF COMMON VARIABLES
      INTEGER(4) I,IP,J,IK,IG,I1,I2,IC1,IC2
      CHARACTER(250) CH001
      CHARACTER(80) CH80
      CHARACTER(32) CH16
      REAL(8) CHECKWERT,F1,F2,FF,PROZ
!-----
      IF (PRTEST) THEN
      WRITE (UNIT=6,FMT='(//,''-> check value (CHECKVAL)'')')
      WRITE (UNIT=35,FMT='(//,''-> check value (CHECKVAL)'')')
      END IF
!--
      CHECKWERT=0.0D0
      IF (.NOT.ALLP) THEN
      DO I=1,NC
       CHEM(I)=1.0D0
      END DO
      USE='*'
      LUSE=1
      CALL DBREAD
      ALLP=.TRUE.
      CALL PRTFIRST
!
      END IF
!=====
!+++
      IF (PRTEST) THEN
      CALL LABLA(CHKPH,I1)
      CALL LABLA(CHKEL,I2)
      WRITE (6,3000) CHKPH(1:I1),CHKEL(1:I2),CHKELDIV,CHKMIN,CHKMAX
 3000 FORMAT (' to check: ',2(2X,A),'/',F8.4,2(2X,F8.4))
      END IF
!+++

      DO I=1,NPHA
      IF (VERGL(NAME(I),CHKPH)) THEN
       TC=TEM
       P=PRE
       CALL NURVONPT
       IP=I
       CALL DAREST(IP)
       CALL GCALC(IP)
       CH001=' '
      WRITE (CH001,1002) TC,P
 1002 FORMAT (5X,'TC =',F8.2,'  P =',F9.2)
!-
      IF (VERGL(CHKEL,'S')) CHECKWERT=SR/CHKELDIV
      IF (VERGL(CHKEL,'V')) CHECKWERT=VOLUM/CHKELDIV
      IF (VERGL(CHKEL,'CP')) CHECKWERT=CPR/CHKELDIV
      IF (VERGL(CHKEL,'V/V0')) CHECKWERT=VOLUM/CHKELDIV/REDATA(4,IP)/10.0D0
!
!
       WRITE (CH80,2002) CHKMIN,CHECKWERT,CHKMAX
 2002 FORMAT (F8.4,' < ',F8.4,' > ',F8.4)
       CALL COLLAPS(CH80,J)
       IK=INDEX(CH80,'<')
       IG=INDEX(CH80,'>')
       CALL LABLA(CHKPH,I1)
       CALL LABLA(CHKELCH,I2)
       WRITE (CH001(33:),2000) CHKPH(1:I1),CHKELCH(1:I2), &
       CH80(1:IK-1),CH80(IK+1:IG-1),CH80(IG+1:)
 2000 FORMAT (5X,A,2X,A,':  ',A,' < ',A,' < ',A)
       CALL LABLA(CH001,I1)
       FF=(CHKMIN+CHKMAX)/2.0D0
       PROZ=(CHECKWERT-FF)/FF*100.0D0
       CH16=' '
       WRITE (UNIT=CH16,FMT='(1PE9.2)') PROZ
       CALL FIBLA(CH16,IC1)
       CALL LABLA(CH16,IC2)
       IF (CHECKWERT.GE.CHKMIN.AND.CHECKWERT.LE.CHKMAX) THEN
        CH001(I1+2:)='( '//CH16(IC1:IC2)//' % )'
       ELSE 
        CH001(I1+2:)='( '//CH16(IC1:IC2)//' % )'
        CALL LABLA(CH001,I1)
        F1=CHECKWERT-CHKMIN
        F2=CHECKWERT-CHKMAX
        IF (DABS(F1).LT.DABS(F2)) THEN
        FF=F1
        ELSE
        FF=F2
        END IF
!!        CH001=' '
        CH001(I1+2:)='+'
        I1=I1+2
        DO J=I1+1,108
         CH001(J:J)='+'
        END DO
        IF (FF.GT.0.0D0) THEN
        CH001(110:)='(VAL)value too large'
        ELSE
        CH001(110:)='(VAL)value too small'
        END IF
       END IF
       CALL PUST(scr,CH001)
       CALL PUST(35,CH001)
!
      END IF
      END DO
!----
      RETURN
      END
!-----
!*************************************************************
!*************************************************************
      SUBROUTINE CHECKVALPT(TEXTQ,COUNT)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
      include 'checkdb.cmn'
!
!-----END OF COMMON VARIABLES
      INTEGER(4) I,IP,J,I1,I2,I3,I4,ITT,IPP,IC,IE,IS, &
      BARSHIFT,FEA,FEA1,BPOS,PLNR,COUNT,INVAL,INQ
      CHARACTER(250) TEXT1,TEXT2,TEXTQ
      CHARACTER(80) CHE2,CHE1,SUMSTR
      CHARACTER(32) CH16
      CHARACTER(7) EBAR1,EBAR2
      CHARACTER(4) V1
      REAL(8) WERT,WERTMIN,WERTMAX,F1,FF,PROZ,TOFMIN,POFMIN, &
      TOFMAX,POFMAX,MAXPROZ,PTPROZ,F2,F3,F4,ZERO
!-----
      IF (PRTEST) THEN
      WRITE (UNIT=6,FMT='(''-> check value (CHECKVALPT)'')')
      WRITE (UNIT=35,FMT='(''-> check value (CHECKVALPT)'')')
      END IF

      ZERO=0.0D0
      COUNT=0
      INVAL=0
      TEXTQ='not initialized'
!+++
!      IF (PRTEST) THEN
!      CALL LABLA(CHKPH,I1)
!      CALL LABLA(CHKEL,I2)
!      WRITE (6,3000) CHKPH(1:I1),CHKEL(1:I2),CHKELDIV,CHKMIN,CHKMAX
!      WRITE (35,3000) CHKPH(1:I1),CHKEL(1:I2),CHKELDIV,CHKMIN,CHKMAX
! 3000 FORMAT (' to check: ',2(2X,A),'/',F8.4,2(2X,F8.4))
!      END IF
!==
!====
!====
!====
      IF (KEY1.EQ.'TC') CTUNIT='TC'
      IF (KEY1.EQ.'TK') CTUNIT='TK'
      SUMSTR=' '
      WERTMIN=1D20
      WERTMAX=-1D20
      DO ITT=-1,1,2
      DO IPP=-1,1,2
      IF (KEY1.EQ.'TC') TC=TEM+DFLOAT(ITT)*TERR0
      IF (KEY1.EQ.'TK') TC=TEM+DFLOAT(ITT)*TERR0-273.15D0
      P=PRE+DFLOAT(IPP)*PERR0
      CALL NURVONPT
      CALL CALSTR
      CALL THERIA
      CALL GETVAL(CHKPH,CHKEL,CHKELDIV,IP,IS,IE,F1)
      IF (IP.EQ.0) GOTO 22
      IF (F1.LT.WERTMIN) THEN
        WERTMIN=F1
        IF (KEY1.EQ.'TC') TOFMIN=TC
        IF (KEY1.EQ.'TK') TOFMIN=TC+273.15D0
        POFMIN=P
      END IF
      IF (F1.GT.WERTMAX) THEN
        WERTMAX=F1
        IF (KEY1.EQ.'TC') TOFMAX=TC
        IF (KEY1.EQ.'TK') TOFMAX=TC+273.15D0
        POFMAX=P
      END IF
   22 CONTINUE
      END DO
      END DO
!====
      INQ=0
      IF (CHKLIM.NE.0) THEN
        F1=DABS(CHKSTART-CHKMIN)
        F2=DABS(CHKSTART-CHKMAX)
        F3=DABS(CHKSTART-WERTMIN)
        F4=DABS(CHKSTART-WERTMAX)
        
        IF (F1.GT.F3.AND.F1.GT.F4.AND.F2.GT.F3.AND.F2.GT.F4) THEN
          INQ=1
        END IF
        
!!        INQ=0
        F1=CHKSTART-(WERTMIN+WERTMAX)/2.0D0
        F2=CHKSTART-CHKMIN
        F3=CHKSTART-CHKMAX
        IF (F1.GT.ZERO.AND.F2.LT.ZERO.AND.F3.LT.ZERO) THEN
          INQ=1
        END IF
        IF (F1.LT.ZERO.AND.F2.GT.ZERO.AND.F3.GT.ZERO) THEN
          INQ=1
        END IF
        
!!        WRITE (UNIT=35,FMT='(''F1,F2,F3,F4='',4F12.5)') F1,F2,F3,F4
        
      END IF


!==
      IF (KEY1.EQ.'TC') TC=TEM
      IF (KEY1.EQ.'TK') TC=TEM-273.15D0
      P=PRE
      CALL NURVONPT
      CALL CALSTR
      CALL THERIA
      CALL GETVAL(CHKPH,CHKEL,CHKELDIV,IP,IS,IE,WERT)
!--
      IF (IP.GT.NUN2) THEN
!       CCODE=10
       CALL LABLA(CHKPH,I1)
       TEXT1='     '//CHKPH(1:I1)
       CALL LABLA(TEXT1,IC)
       DO J=IC+2,81
        TEXT1(J:J)='+'
       END DO
       TEXT1(83:)= &
       'phase not stable, check value of unstable phase'
       CALL PUST(scr,TEXT1)
       CALL PUST(35,TEXT1)
!       CALL SHORTSUM
!       RETURN
      END IF
!--
      IF (IP.EQ.0.AND.IE.EQ.0) THEN
       CCODE=10
       CALL LABLA(CHKPH,I1)
       TEXT1='     '//CHKPH(1:I1)
       CALL LABLA(TEXT1,IC)
       DO J=IC+2,112
        TEXT1(J:J)='+'
       END DO
       TEXT1(114:)='phase not stable 3'
       CALL PUST(scr,TEXT1)
       CALL PUST(35,TEXT1)
       CALL SHORTSUM
       RETURN
      END IF
!--
      IF (IE.EQ.0.AND.IP.NE.0) THEN
       CCODE=5
       CALL LABLA(CHKPH,I1)
       CALL LABLA(CHKEL,I2)
       TEXT1='     '//CHKPH(1:I1)//' : '//CHKEL(1:I2)
       CALL LABLA(TEXT1,IC)
       DO J=IC+2,113
        TEXT1(J:J)='-'
       END DO
       TEXT1(115:)='unknown variable 3'
       CALL PUST(scr,TEXT1)
       CALL PUST(35,TEXT1)
       RETURN
      END IF
!====
!====
      IF (WERT.GT.WERTMAX.OR.WERT.LT.WERTMIN) THEN
       WERTMIN=WERT
       WERTMAX=WERT
       POFMIN=P
       POFMAX=P
        IF (KEY1.EQ.'TC') TOFMAX=TC
        IF (KEY1.EQ.'TK') TOFMAX=TC+273.15D0
      END IF
!====

       TEXT1=' '
       IC=0
       FF=(CHKMIN+CHKMAX)/2.0D0
       PROZ=(WERT-FF)/FF*100.0D0
       MAXPROZ=(CHKMAX-FF)/FF*100.0D0
       FF=(WERTMIN+WERTMAX)/2.0D0
       PTPROZ=(WERTMAX-FF)/FF*100.0D0

!====
      BPOS=19
      EBAR1='x--|--x'
      EBAR2='x--0--x'
      CHE1='|'
      CHE1(BPOS:)=EBAR1
      BARSHIFT=BPOS
      FEA=0
      FEA1=0
      IF (WERT.LE.CHKMAX.AND.WERT.GE.CHKMIN) FEA1=1
      IF (WERTMIN.LE.CHKMAX.AND.WERTMAX.GE.CHKMIN) FEA=1
      IF (WERT.LT.CHKMIN) BARSHIFT=BPOS-9
      IF (WERT.GT.CHKMAX) BARSHIFT=BPOS+9
      IF (FEA.EQ.1.AND.WERT.LE.CHKMIN) BARSHIFT=BPOS-5
      IF (FEA.EQ.1.AND.WERT.GT.CHKMAX) BARSHIFT=BPOS+5
      IF (FEA1.EQ.1.AND.WERT.LE.(CHKMIN+CHKMAX)/2.0D0) BARSHIFT=BPOS-2
      IF (FEA1.EQ.1.AND.WERT.GT.(CHKMIN+CHKMAX)/2.0D0) BARSHIFT=BPOS+2
      IF (DABS(PROZ).LT.0.001) BARSHIFT=BPOS
      INVAL=10
      IF (FEA.EQ.1) INVAL=0
      CHE2='|'
      CHE2(BARSHIFT:)=EBAR2

       CH16=' '
       IF (DABS(PROZ).GT.0.01D0.AND.DABS(PROZ).LT.1000.0D0) THEN
         WRITE (UNIT=CH16,FMT='(F7.2)') PROZ
       ELSE
         WRITE (UNIT=CH16,FMT='(1PE10.2)') PROZ
       END IF
       CALL FIBLA(CH16,I1)
       CALL LABLA(CH16,I2)
       TEXT1(IC+2:)='( '//CH16(I1:I2)//' % )'
       CALL LABLA(TEXT1,IC)

!!      WRITE (UNIT=35,FMT='(''TEXT1='',A)') TEXT1(1:IC)

       IF (FEA.EQ.0.AND.WERT.LE.CHKMIN) THEN
        IF (CHKLIM.GT.0) THEN
          TEXT1(IC+2:)='upper limit OK'
          TEXTQ='upper limit OK'
          INVAL=1
        ELSE
        DO J=IC+1,43
         TEXT1(J:J)='+'
        END DO
        TEXT1(45:)='(VALPT)value too small'
        TEXTQ='value too small'
        COUNT=COUNT+1
        INVAL=2
        IF (ISINC.EQ.0) THEN
!!          NINC=NINC+1
          ISINC=1
!          CCODE=1
        END IF
       END IF
       END IF

       IF (FEA.EQ.0.AND.WERT.GT.CHKMAX) THEN
        IF (CHKLIM.LT.0) THEN
          TEXT1(IC+2:)='lower limit OK'
          TEXTQ='lower limit OK'
          INVAL=1
        ELSE
        DO J=IC+1,43
         TEXT1(J:J)='+'
        END DO
        TEXT1(45:)='(VALPT)value too large'
        TEXTQ='value too large'
        COUNT=COUNT+1
        INVAL=2
        IF (ISINC.EQ.0) THEN
!!          NINC=NINC+1
          ISINC=1
!          CCODE=1
        END IF
       END IF
       END IF

       IF (FEA.GT.0) THEN
          TEXT1(IC+2:)='OK'
          TEXTQ='OK'
       END IF

       CALL LABLA(TEXT1,IC)

!       CALL PUST(scr,TEXT1)
!       CALL PUST(35,TEXT1)

!==
      IF (CHKLIM.LT.0) TEXT2='lower limit'
      IF (CHKLIM.GT.0) TEXT2='upper limit'
      IF (CHKLIM.EQ.0) TEXT2='bracket'
      
      IF (CHKLIM.NE.0) THEN
        WRITE (UNIT=TEXT2(15:),FMT='(''starting value = '',F12.5)') CHKSTART
      END IF
      
      IF (KEY1.EQ.'TC') V1='T[C]'
      IF (KEY1.EQ.'TK') V1='T[K]'
      WRITE (scr,FMT='(5X,55A1)') ('-',I=1,55)
      WRITE (35,FMT='(5X,55A1)') ('-',I=1,55)
      CALL LABLA(CHKELCH,I1)
      I2=INDEX(EXPER,'  ')
      CALL LABLA(TEXT2,I3)
      CALL LABLA(CHKPH,I4)
      SUMSTR=CHKPH(1:I4)//'  '//CHKELCH(1:I1)
      WRITE (6,1102) EXPER(1:I2),CHKPH(1:I4),CHKELCH(1:I1), &
      TEXT2(1:I3)
      WRITE (35,1102) EXPER(1:I2),CHKPH(1:I4),CHKELCH(1:I1), &
      TEXT2(1:I3)
 1102 FORMAT (5X,A,'  check value:  ',A,2X,A,3X,A)
      IF (CHKELCH.EQ.'V/V0') THEN
      CALL LABLA(CHKEL,I1)
      WRITE (6,1100) V1,CHKEL(1:8)
      WRITE (35,1100) V1,CHKEL(1:8)
      ELSE
      CALL LABLA(CHKELCH,I1)
      WRITE (6,1100) V1,CHKELCH(1:8)
      WRITE (35,1100) V1,CHKELCH(1:8)
      END IF
 1100 FORMAT (9X,A4,4X,'P[Bar]',7X,A8,4X,'min',9X,'max',10X,'+/-')

      CALL LABLA(CHE1,I1)
      IF (CHKLIM.LT.0) CHE1(I1+1:I1+9)='---------'
      IF (CHKLIM.GT.0) CHE1(I1-15:I1-7)='---------'

      CALL LABLA(CHE1,I2)
      CALL LABLA(CHE2,I1)

      IF ((CHKMIN+CHKMAX)/2.0D0.GT.1D-2.AND.(CHKMIN+CHKMAX)/2.0D0.LT.1D4) THEN

      WRITE (6,1000) TEM,PRE,(CHKMIN+CHKMAX)/2.0D0,CHKMIN, &
      CHKMAX,MAXPROZ,CHE1(1:I2)
      WRITE (35,1000) TEM,PRE,(CHKMIN+CHKMAX)/2.0D0,CHKMIN, &
      CHKMAX,MAXPROZ,CHE1(1:I2)
 1000 FORMAT (5X,F8.2,F10.1,1X,F12.5,F12.5,F12.5,4X,F7.2,' %',A)
      WRITE (6,1002) WERT,PTPROZ,CHE2(1:I1)
      WRITE (35,1002) WERT,PTPROZ,CHE2(1:I1)
 1002 FORMAT (24X,F12.5,28X,F7.2,' %',A)
      WRITE (6,1004) TOFMIN,POFMIN,WERTMIN
      WRITE (35,1004) TOFMIN,POFMIN,WERTMIN
 1004 FORMAT (5X,F8.2,F10.1,13X,F12.5)
      WRITE (6,1008) TOFMAX,POFMAX,WERTMAX,TEXT1(1:IC)
      WRITE (35,1008) TOFMAX,POFMAX,WERTMAX,TEXT1(1:IC)
 1008 FORMAT (5X,F8.2,F10.1,25X,F12.5,3X,A)

      ELSE

      WRITE (6,1001) TEM,PRE,(CHKMIN+CHKMAX)/2.0D0,CHKMIN, &
      CHKMAX,MAXPROZ,CHE1(1:I2)
      WRITE (35,1001) TEM,PRE,(CHKMIN+CHKMAX)/2.0D0,CHKMIN, &
      CHKMAX,MAXPROZ,CHE1(1:I2)
 1001 FORMAT (5X,F8.2,F10.1,1X,1PE12.4,1PE12.4,1PE12.4,4X, &
      1PE10.2,' %',A)
      WRITE (6,1003) WERT,PTPROZ,CHE2(1:I1)
      WRITE (35,1003) WERT,PTPROZ,CHE2(1:I1)
 1003 FORMAT (24X,1PE12.4,28X,1PE10.2,' %',A)
      WRITE (6,1005) TOFMIN,POFMIN,WERTMIN
      WRITE (35,1005) TOFMIN,POFMIN,WERTMIN
 1005 FORMAT (5X,F8.2,F10.1,13X,1PE12.4)
      WRITE (6,1009) TOFMAX,POFMAX,WERTMAX,TEXT1(1:IC)
      WRITE (35,1009) TOFMAX,POFMAX,WERTMAX,TEXT1(1:IC)
 1009 FORMAT (5X,F8.2,F10.1,25X,1PE12.4,3X,A)

      END IF

!      WRITE (scr,FMT='(5X,55A1)') ('-',I=1,55)
!      WRITE (35,FMT='(5X,55A1)') ('-',I=1,55)
!==


!!-----summary
!      IF (PRISUM.AND.CCODE.NE.0) THEN
!      WRITE (UNIT=36,FMT='('' '')')
!      CALL FIBLA(REFER,I1)
!      CALL PUST(36,REFER(I1:))
!      CALL PUST(36,'reaction:   '//NICREAC)
!!!      CALL PUST(36,'plotfile:   '//PLFILE)
!      IF (NPLOTS.GT.0) THEN
!        DO I=1,NPLOTS
!          CALL PUST(36,'plotfile:   '//AUTOPLOT(I))
!        END DO
!      END IF
!      I1=INDEX(EXPER,'  ')
!      CALL PUST(36,'experiment: '//EXPER(1:I1-1))
!      CALL LABLA(SUMSTR,I1)
!      IF (WERT.GT.1D-2.AND.WERT.LT.1D4) THEN
!        WRITE (36,1022) TEM,PRE,SUMSTR(1:I1),WERT,TEXT1(1:IC)
! 1022   FORMAT (5X,'T = ',F8.2,'  P = ',F12.1,2X,A,' : ',F12.5,2X,A)
!      ELSE
!        WRITE (36,1023) TEM,PRE,SUMSTR(1:I1),WERT,TEXT1(1:IC)
! 1023   FORMAT (5X,'T = ',F8.2,'  P = ',F12.1,2X,A,' : ',1PE12.4,2X,A)
!      END IF
!      END IF
!!-----end summary
!-------------------------------------------------------------
!---- make plot
!-------------------------------------------------------------
      XCWERT=WERT
      IF (INVAL.EQ.0) THEN
        CCODE=0
      ELSE
         CCODE=1
      END IF
      IF (CHKLIM.NE.0) THEN
        CCODE=0
        IF (INVAL.EQ.1) CCODE=0
        IF (INQ.EQ.1) CCODE=1
      END IF
      
!!      WRITE (UNIT=6,FMT='(''INVAL,INQ,CCODE='',3I4)') INVAL,INQ,CCODE
!!      WRITE (UNIT=35,FMT='(''INVAL,INQ,CCODE='',3I4)') INVAL,INQ,CCODE
      
      DO I=1,NPLOTS
        IF (AUTOTYP(I).EQ.'VAL') THEN
          YCWERT=WERT
          PLNR=I
          CALL PLOTEXP(PLNR)
        END IF
        IF (AUTOTYP(I).EQ.'ISO') THEN
          PLNR=I
          CALL PLOTEXP(PLNR)
        END IF
        
        IF (AUTOTYP(I).EQ.'PX') THEN
          PLNR=I
          CALL PLOTEXP(PLNR)
        END IF
        IF (AUTOTYP(I).EQ.'TX') THEN
          PLNR=I
          CALL PLOTEXP(PLNR)
        END IF
      END DO
!----
      RETURN
      END
!-----
!*************************************************************
!*************************************************************
      SUBROUTINE CHECKKREA
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
      include 'checkdb.cmn'
!
!-----END OF COMMON VARIABLES
      INTEGER(4) I,IP,J,I1,I2,I3,I4,ITT,IPP,IC,IE,IS, &
      BARSHIFT,FEA,FEA1,BPOS,PLNR
      CHARACTER(250) CH001
      CHARACTER(150) TEXT1,TEXT2
      CHARACTER(80) CHE2,CHE1
      CHARACTER(32) CH16
      CHARACTER(7) EBAR1,EBAR2
      CHARACTER(4) V1
      REAL(8) WERT,WERTMIN,WERTMAX,F1,FF,PROZ,TOFMIN,POFMIN, &
      TOFMAX,POFMAX,MAXPROZ,PTPROZ,GOFR
!-----
      IF (PRTEST) THEN
      WRITE (UNIT=6,FMT='(//,'' check reaction (CHECKKREA)'')')
      WRITE (UNIT=35,FMT='(//,'' check reaction (CHECKKREA)'')')
      END IF
!+++
      IF (PRTEST) THEN
      CALL LABLA(CHKPH,I1)
      CALL LABLA(CHKEL,I2)
      WRITE (6,3000) CHKPH(1:I1),CHKEL(1:I2),CHKELDIV,CHKMIN,CHKMAX
 3000 FORMAT (' to check: ',2(2X,A),'/',F8.4,2(2X,F8.4))
      END IF
!==
      IF (NCOEFF.EQ.0) THEN
       WRITE (UNIT=6,FMT='(//'' reaction coefficients not defined'')')
       WRITE (UNIT=35,FMT='(//'' reaction coefficients not defined'')')
       RETURN
      END IF
!====
      KEY1='TC'
      IF (KEY1.EQ.'TC') CTUNIT='TC'
      IF (KEY1.EQ.'TK') CTUNIT='TK'
!====
      CHKEL='G'
      CHKELDIV=1.0D0
      WERTMIN=1D20
      WERTMAX=-1D20
      DO ITT=-1,1,2
      DO IPP=-1,1,2
      IF (KEY1.EQ.'TC') TC=TEM+DFLOAT(ITT)*TERR0
      IF (KEY1.EQ.'TK') TC=TEM+DFLOAT(ITT)*TERR0-273.15D0
      P=PRE+DFLOAT(IPP)*PERR0
      CALL NURVONPT
      CALL CALSTR
!      CALL THERIA
!
      GOFR=0.0D0
      DO I=1,NCOEFF
        CALL GETVAL(RPHA(I),CHKEL,CHKELDIV,IP,IS,IE,F1)
        GOFR=GOFR+COEFF(I)*F1
      END DO
      F1=-GOFR/R/T
!
      IF (F1.LT.WERTMIN) THEN
        WERTMIN=F1
        IF (KEY1.EQ.'TC') TOFMIN=TC
        IF (KEY1.EQ.'TK') TOFMIN=TC+273.15D0
        POFMIN=P
      END IF
      IF (F1.GT.WERTMAX) THEN
        WERTMAX=F1
        IF (KEY1.EQ.'TC') TOFMAX=TC
        IF (KEY1.EQ.'TK') TOFMAX=TC+273.15D0
        POFMAX=P
      END IF
      END DO
      END DO
!====
      IF (KEY1.EQ.'TC') TC=TEM
      IF (KEY1.EQ.'TK') TC=TEM-273.15D0
      P=PRE
      CALL NURVONPT
      CALL CALSTR
!
      GOFR=0.0D0
      DO I=1,NCOEFF
        CALL GETVAL(RPHA(I),CHKEL,CHKELDIV,IP,IS,IE,F1)
        GOFR=GOFR+COEFF(I)*F1
      END DO
      WERT=-GOFR/R/T
!====
      CHKLIM=0
      CHKPH='reaction'
      CHKELCH='LN(K)'
!====
       TEXT1=' '
       IC=0
       FF=(CHKMIN+CHKMAX)/2.0D0
       PROZ=(WERT-FF)/FF*100.0D0
       MAXPROZ=(CHKMAX-FF)/FF*100.0D0
       FF=(WERTMIN+WERTMAX)/2.0D0
       PTPROZ=(WERTMAX-FF)/FF*100.0D0
!====
      BPOS=19
      EBAR1='x--|--x'
      EBAR2='x--0--x'
      CHE1='|'
      CHE1(BPOS:)=EBAR1
      BARSHIFT=BPOS
      FEA=0
      FEA1=0
      IF (WERT.LE.CHKMAX.AND.WERT.GE.CHKMIN) FEA1=1
      IF (WERTMIN.LE.CHKMAX.AND.WERTMAX.GE.CHKMIN) FEA=1
      IF (WERT.LT.CHKMIN) BARSHIFT=BPOS-9
      IF (WERT.GT.CHKMAX) BARSHIFT=BPOS+9
      IF (FEA.EQ.1.AND.WERT.LE.CHKMIN) BARSHIFT=BPOS-5
      IF (FEA.EQ.1.AND.WERT.GT.CHKMAX) BARSHIFT=BPOS+5
      IF (FEA1.EQ.1.AND.WERT.LE.(CHKMIN+CHKMAX)/2.0D0) BARSHIFT=BPOS-2
      IF (FEA1.EQ.1.AND.WERT.GT.(CHKMIN+CHKMAX)/2.0D0) BARSHIFT=BPOS+2
      IF (DABS(PROZ).LT.0.001) BARSHIFT=BPOS
      CCODE=1
      IF (FEA.EQ.1) CCODE=0
      CHE2='|'
      CHE2(BARSHIFT:)=EBAR2

       CH16=' '
       WRITE (UNIT=CH16,FMT='(F7.2)') PROZ
       CALL FIBLA(CH16,I1)
       CALL LABLA(CH16,I2)
       TEXT1(IC+2:)='( '//CH16(I1:I2)//' % )'
       CALL LABLA(TEXT1,IC)

       IF (FEA.EQ.0.AND.WERT.LE.CHKMIN) THEN
        IF (CHKLIM.GT.0) THEN
        TEXT1(IC+2:)='upper limit OK'
        CCODE=0
        ELSE
        DO J=IC+1,45
         TEXT1(J:J)='+'
        END DO
        TEXT1(47:)='(REA)value too small'
       END IF
       END IF

       IF (FEA.EQ.0.AND.WERT.GT.CHKMAX) THEN
        IF (CHKLIM.LT.0) THEN
        TEXT1(IC+2:)='lower limit OK'
        CCODE=0
        ELSE
        DO J=IC+1,45
         TEXT1(J:J)='+'
        END DO
        TEXT1(47:)='(REA)value too large'
       END IF
       END IF
       CALL LABLA(TEXT1,IC)
!
!       CALL PUST(scr,TEXT1)
!       CALL PUST(35,TEXT1)
!==
      IF (CHKLIM.LT.0) TEXT2='lower limit'
      IF (CHKLIM.GT.0) TEXT2='upper limit'
      IF (CHKLIM.EQ.0) TEXT2='bracket'
      IF (KEY1.EQ.'TC') V1='T[C]'
      IF (KEY1.EQ.'TK') V1='T[K]'
      WRITE (scr,FMT='(5X,55A1)') ('-',I=1,55)
      WRITE (35,FMT='(5X,55A1)') ('-',I=1,55)
!
       CH001='     reaction: '
       DO I=1,NCOEFF
         CALL LABLA(CH001,I1)
         WRITE (UNIT=CH16,FMT='(F7.2)') COEFF(I)
         CALL FIBLA(CH16,I2)
         CALL LABLA(CH16,I3)
         CALL LABLA(RPHA(I),I4)
         CH001(I1+3:)=CH16(I2:I3)//'  '//RPHA(I)(1:I4)
       END DO
       CALL PUST(scr,CH001)
       CALL PUST(35,CH001)
!
      CALL LABLA(CHKELCH,I1)
      I2=INDEX(EXPER,'  ')
      CALL LABLA(TEXT2,I3)
      CALL LABLA(CHKPH,I4)
      WRITE (6,1102) EXPER(1:I2),CHKPH(1:I4),CHKELCH(1:I1), &
      TEXT2(1:I3)
      WRITE (35,1102) EXPER(1:I2),CHKPH(1:I4),CHKELCH(1:I1), &
      TEXT2(1:I3)
 1102 FORMAT (5X,A,'  check value:  ',A,2X,A,3X,A)
      IF (CHKELCH.EQ.'V/V0') THEN
      CALL LABLA(CHKEL,I1)
      WRITE (6,1100) V1,CHKEL(1:8)
      WRITE (35,1100) V1,CHKEL(1:8)
      ELSE
      CALL LABLA(CHKELCH,I1)
      WRITE (6,1100) V1,CHKELCH(1:8)
      WRITE (35,1100) V1,CHKELCH(1:8)
      END IF
 1100 FORMAT (9X,A4,4X,'P[Bar]',7X,A8,4X,'min',9X,'max',10X,'+/-')

      CALL LABLA(CHE1,I1)
      IF (CHKLIM.LT.0) CHE1(I1+1:I1+9)='---------'
      IF (CHKLIM.GT.0) CHE1(I1-15:I1-7)='---------'

      CALL LABLA(CHE1,I1)
      WRITE (6,1000) TEM,PRE,(CHKMIN+CHKMAX)/2.0D0,CHKMIN, &
      CHKMAX,MAXPROZ,CHE1(1:I1)
      WRITE (35,1000) TEM,PRE,(CHKMIN+CHKMAX)/2.0D0,CHKMIN, &
      CHKMAX,MAXPROZ,CHE1(1:I1)
 1000 FORMAT (5X,F8.2,F10.1,1X,F12.5,F12.5,F12.5,4X,F7.2,' %',A)

      CALL LABLA(CHE2,I1)
      WRITE (6,1002) WERT,PTPROZ,CHE2(1:I1)
      WRITE (35,1002) WERT,PTPROZ,CHE2(1:I1)
 1002 FORMAT (24X,F12.5,28X,F7.2,' %',A)

      WRITE (6,1004) TOFMIN,POFMIN,WERTMIN
      WRITE (35,1004) TOFMIN,POFMIN,WERTMIN
 1004 FORMAT (5X,F8.2,F10.1,13X,F12.5)

      WRITE (6,1008) TOFMAX,POFMAX,WERTMAX,TEXT1(1:IC)
      WRITE (35,1008) TOFMAX,POFMAX,WERTMAX,TEXT1(1:IC)
 1008 FORMAT (5X,F8.2,F10.1,25X,F12.5,3X,A)
!      WRITE (scr,FMT='(5X,55A1)') ('-',I=1,55)
!      WRITE (35,FMT='(5X,55A1)') ('-',I=1,55)
!==
!-------------------------------------------------------------
!---- make plot
!-------------------------------------------------------------
      DO I=1,NPLOTS
        IF (AUTOTYP(I).EQ.'LKR') THEN
          PLNR=I
          CALL PLOTEXP(PLNR)
        END IF
      END DO
!----
      RETURN
      END
!-----
!*************************************************************
!*************************************************************
      SUBROUTINE CHECKTAB(CH250)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
      include 'checkdb.cmn'
!
!-----END OF COMMON VARIABLES
      INTEGER(4) I,IP,IS,IE,J,I1,I2,I3,I4,I5,EXNR,IC,ICF,COUNTIN
      CHARACTER*(*) CH250
      CHARACTER(250) CH001,CH002,TEXTQ
      CHARACTER(32) CH16,CHNR,V1,V2,EINH
      REAL(8) F1,VNULL,FAK,ZPFU,FF,WW
!-----
      IF (PRTEST) THEN
      WRITE (UNIT=6,FMT='(//,'' check table (CHECKTAB)'')')
      WRITE (UNIT=35,FMT='(//,'' check table (CHECKTAB)'')')
      WRITE (UNIT=6,FMT='(''NEWDIR='',A)') NEWDIR
      WRITE (UNIT=35,FMT='(''NEWDIR='',A)') NEWDIR
      END IF
!--


        CALL LABLA(REAID,I1)
        CALL LABLA(NICREAC,I2)
        CALL LABLA(REFER,I3)
!!        CALL LABLA(TABBUL,I4)
!!        CALL LABLA(TABPHA,I5)
!        CALL PUST(37,REAID(1:I1)//NICREAC(1:I2)//REFER(1:I3))
        WRITE (UNIT=39,FMT='('' '')')
        CALL PUSTU(39,REAID(1:I1)//'     '//NICREAC(1:I2)//'     '//REFER(1:I3))


      ICF=0
      VNULL=1.0D0
      FAK=1.0D0
       CALL TAXI(CH250,TABID)
       CALL TAXI(CH250,KEY1)
       CALL TAXI(CH250,KEY2)
       CALL TAXI(CH250,CHKPH)
       CALL TRANSL(CHKPH)
       CALL FUNTAXI(CH250,CHKEL,CHKELDIV,CHKELCH)
       CHKLIM=0
       CALL TAXI(CH250,EINH)
       CALL GELI(CH250,ZPFU)
       CH001=' '
!==
      CALL LABLA(CHKPH,I1)
      CALL LABLA(CHKELCH,I2)
      EXPER='Table:  '//CHKPH(1:I1)//'  '//CHKELCH(1:I2)
!
      CALL LABLA(EXPER,IC)
      CALL LABLA(EINH,I1)
      WRITE (UNIT=CH16,FMT='(F5.1)') ZPFU
      CALL COLLAPS(CH16,I2)
      EXPER(IC+2:)='unit = '//EINH(1:I1)//'  Z = '//CH16(1:I2)
!      CALL PUST(scr,CH001)
!      CALL PUST(35,CH001)
!
      CALL PRTFIRST
!      IF (NPLOTS.GT.0) THEN
!        CALL LABLA(AUTOPLOT(NPLOTS),I1)
!        CH001='plotfile: '//AUTOPLOT(NPLOTS)(1:I1)//'  typ: '//PLOTTYP
!        CALL PUST(scr,CH001)
!        CALL PUST(35,CH001)
!      END IF
      CH001=' bulk: '//BUFORMUL(1:143)
      CALL PUSTCOL(scr,CH001,1,129)
      CALL PUSTCOL(35,CH001,1,129)
!      WRITE (6,1000)
!+++
      CH001=' phases:'
      DO J=1,NCHOOSE0
      CALL LABLA(CH001,I2)
      IF (J.EQ.1) THEN
      CH001(I2+3:)=CHOOSE0(J)
      ELSE
      CH001(I2+2:)=CHOOSE0(J)
      END IF
      END DO
      CALL PUST(scr,CH001)
      CALL PUST(35,CH001)
!+++
      IF (KEY1.EQ.'TC') CTUNIT='TC'
      IF (KEY1.EQ.'TK') CTUNIT='TK'
      EXNR=0
      V1=KEY1
      IF (KEY1.EQ.'TC') V1='T [C]'
      IF (KEY1.EQ.'TK') V1='T [K]'
      V2=KEY2
      IF (KEY2.EQ.'P') V2='P [Bar]'
      CH16=CHKELCH
      IF (CHKELCH.EQ.'V/V0') CH16='V'
      CALL LABLA(CH16,J)

       NPICK=NCHOOSE
       DO I=1,NCHOOSE
        PICK(I)=CHOOSE(I)
       END DO
       CALL DBREAD
       IF (CHKEL.EQ.'V/V0') THEN
        TC=T0-273.15D0
        P=P0
        CALL NURVONPT
        CALL CALSTR
        CALL THERIA
        CHKEL='V'
        EINH='rat'
        CALL GETVAL(CHKPH,CHKEL,CHKELDIV,IP,IS,IE,VNULL)
       END IF
       IF (VERGL(EINH,'ANG')) THEN
         FAK=6.02214129D0/10.0D0/ZPFU
!         CHKEL='V'
       END IF
!==

       NEXP=0
       NINC=0
       COUNTIN=0

   37  READ (UNIT=drv,FMT='(A)',END=999) CH002
       IF (CH002.EQ.' ') GOTO 999
       IF (CH002(1:1).EQ.'!') GOTO 37
       ICF=0
       NEXP=NEXP+1
       CALL GELI(CH002,TEM)
       CALL GELI(CH002,TERR0)
       CALL GELI(CH002,PRE)
       CALL GELI(CH002,PERR0)
       CALL GELI(CH002,F1)
       CALL TAXI(CH002,CH16)
       CALL MINMAX(F1,CH16,CHKMIN,CHKMAX)

       IF (TEM.LT.TSHMIN) TSHMIN=TEM
       IF (TEM.GT.TSHMAX) TSHMAX=TEM
       IF (PRE.LT.PSHMIN) PSHMIN=PRE
       IF (PRE.GT.PSHMAX) PSHMAX=PRE

!       CHKMIN=F1-F2
!       CHKMAX=F1+F2
       IF (VERGL(EINH,'RAT')) THEN
         CHKMIN=CHKMIN*VNULL
         CHKMAX=CHKMAX*VNULL
       END IF
       IF (VERGL(EINH,'ANG')) THEN
         CHKMIN=CHKMIN*FAK
         CHKMAX=CHKMAX*FAK
       END IF

       EXNR=EXNR+1
       WRITE (UNIT=CHNR,FMT='(I6)') EXNR
       CALL COLLAPS(CHNR,J)
      CALL LABLA(TABID,I1)
      EXPER=TABID(1:I1)//'-'//CHNR(1:J)
      CALL LABLA(EXPER,J)
      WRITE (UNIT=CH001,FMT=1005) EXPER(1:J),TEM,PRE
 1005 FORMAT (1X,A,2X,F9.3,2X,F9.1)
       CALL LABLA(CH001,IC)
!==
      IF (EXNR.EQ.1.AND.EINH.EQ.'ANG'.AND.CHKEL.EQ.'V') THEN
       WW=(CHKMIN+CHKMAX)/2.0D0
       CALL GUESSZ(WW,ZPFU,FF)
       IF (DABS(FF-ZPFU).GT.0.25D0) THEN
        CALL LABLA(CHKPH,I1)
        WRITE (6,2002) CHKPH(1:I1),ZPFU
 2002 FORMAT(A,2X,'database ZPFU = ',F4.1)
        WRITE (6,2000) CHKPH(1:I1),FF
 2000 FORMAT(A,2X,'guessed  ZPFU = ',F4.1)
        CALL EXIT
        CHKMIN=CHKMIN*ZPFU/FF
        CHKMAX=CHKMAX*ZPFU/FF
        ZPFU=FF
       END IF
      END IF
!==
      I1=0
      CALL CHECKVALPT(TEXTQ,I1)
      IF (I1.NE.0) COUNTIN=COUNTIN+1

!-----q-summary

      CALL LABLA(CHKELCH,I1)
      I2=INDEX(EXPER,'  ')
      CALL LABLA(CHKPH,I4)
      CALL LABLA(TEXTQ,I5)
      CH002=' '
      WRITE (UNIT=CH002,FMT='(A,2X,A)') CHKPH(1:16),CHKELCH(1:8)
      CALL LABLA(CH002,I2)
      WRITE (39,2015) TEM,PRE,F1,CH002(1:39),TEXTQ(1:I5)
 2015 FORMAT (3X,'T = ',F8.2,'  P = ',F10.1,2X,'value: ',F15.6,2X,A,2X,A)

      GOTO 37

  999 CONTINUE

      ISINC=0
      NINC=COUNTIN

!----
      RETURN
      END
!-----
!*************************************************************
!*************************************************************
      SUBROUTINE SEARCHASS(NFEHL1)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
      include 'checkdb.cmn'
!
!-----END OF COMMON VARIABLES
      CHARACTER(200) CH001
      CHARACTER(250) ASSTEXT,OKTEXT
      CHARACTER(40) CH40
      CHARACTER(32) TEXT,TEXT2
      CHARACTER(4) GITT(-1:1,-1:1)
      INTEGER(4) I,IS,NFEHL,IP,NPLUS,NMINUS,I1,IM,ITEM,IPRE,SPIR, &
      ESPIR,IKONT,IKONP,ITBACK,IPBACK,NFEHL1,I2,I3
      REAL(8) TNOW,PNOW,TERR,PERR,TCOR,PCOR,FF,SUMT,SUMP
!----
!==
      IF (PRTEST) WRITE (UNIT=6,FMT='(''-> SEARCHASS'')')
      IF (PRTEST) WRITE (UNIT=35,FMT='(''-> SEARCHASS'')')
!
      TNOW=TC
      PNOW=P
      TERR=TERR0
      PERR=PERR0
      OKTEXT=' '
      ITBACK=0
      IPBACK=0
!+++++
      ESPIR=0
      SUMT=0.0D0
      SUMP=0.0D0
      DO SPIR=1,10
      IF (ESPIR.GT.0) GOTO 888
      TCOR=DBLE(SPIR)*TERR
      PCOR=DBLE(SPIR)*PERR
      DO ITEM=1,-1,-1
      DO IPRE=1,-1,-1
      IF (ITEM.EQ.0.AND.IPRE.EQ.0) THEN
      GITT(ITEM,IPRE)=' -- '
      GOTO 10
      END IF
!+++++
      GITT(ITEM,IPRE)=' -- '
      TC=TNOW+DBLE(ITEM)*TCOR
      IF (TC.LT.0.D0) TC=0.0D0
      P=PNOW+DBLE(IPRE)*PCOR
      IF (P.LT.1.0D0) P=1.0D0
      CALL NURVONPT
      CALL MACHACT
      CALL CALSTR
      CALL THERIA
!====
      ASSTEXT=' '
      DO I=1,NUN2
       I1=I
       CALL GETNAME(I1,IS,TEXT)
       FF=NN(I)
       CH40=' '
       WRITE (UNIT=CH40,FMT='(F10.3)') NN(I)
       CALL FIBLA(CH40,I2)
       IF (I2.EQ.0) I2=1
       CALL LABLA(CH40,I3)
       IF (I3.EQ.0) I3=1
       CALL LABLA(ASSTEXT,I1)
       IF (I.EQ.1) THEN
       ASSTEXT(I1+2:)=CH40(I2:I3)//' '//TEXT
       ELSE
       ASSTEXT(I1+2:)='+ '//CH40(I2:I3)//' '//TEXT
       END IF
      END DO
!==
      NFEHL=0
!== check if an EXPEC phase is missing
      DO IP=1,NPASS
      NPLUS=0
      IPASS(IP)=0
      DO I=1,NUN2
       I1=I
       CALL GETNAME(I1,IS,TEXT)
       IF (VERGL(TEXT,PASS(IP))) THEN
        NPLUS=NPLUS+1
        IPASS(IP)=I
       END IF
      END DO
!!!!!!!!!!!!       IF (NPLUS.NE.PMULT(IP)) THEN
       IF (NPLUS.EQ.0.AND.PMULT(IP).GT.0) THEN
       NFEHL=NFEHL+1
       END IF
      END DO
!== check if a not EXPEC phase is present
      DO IM=1,NMASS
      NMINUS=0
      DO I=1,NUN2
       I1=I
       CALL GETNAME(I1,IS,TEXT)
       IF (VERGL(TEXT,MASS(IM))) NMINUS=NMINUS+1
      END DO
       IF (NMINUS.NE.0) THEN
       NFEHL=NFEHL+1
       END IF
      END DO
!--
      IF (NFEHL.EQ.0) THEN
      GITT(ITEM,IPRE)='(OK)'
      OKTEXT=ASSTEXT
      CH001=' '
      ESPIR=ESPIR+1
      SUMT=SUMT+DBLE(ITEM)
      SUMP=SUMP+DBLE(IPRE)
      ITBACK=ITEM
      IPBACK=IPRE
!=====
!      WRITE (CH001,1000) DBLE(ITEM)*TCOR,DBLE(IPRE)*PCOR,ASSTEXT
! 1000 FORMAT (5X,'T(+) =',F8.2,'   P(+) =',F9.2,2X,A)
!      CALL LABLA(CH001,I1)
!      CH001(I1+2:)='(OK)'
!      CALL PUST(scr,CH001)
!      CALL PUST(35,CH001)
!=====
      END IF
!+++++
   10 CONTINUE
      END DO
      END DO
      END DO
!+++++
  888 CONTINUE
      IF (ESPIR.EQ.0) THEN
      CH001=' '
      WRITE (CH001,1010) TCOR,PCOR
 1010 FORMAT (5X,'T+/- =',F8.2,'   P+/- =',F11.2)
      CALL LABLA(CH001,I1)
      CALL LABLA(CH001,I1)
        DO I=I1+2,108
         CH001(I:I)='+'
        END DO
      CH001(110:)='assemblage not found'
      CALL PUST(scr,CH001)
      CALL PUST(35,CH001)
      CCODE=10
      MCODE=1
!!--- summary
!      WRITE (UNIT=36,FMT='('' '')')
!      CALL FIBLA(REFER,I1)
!      CALL PUST(36,REFER(I1:))
!      CALL PUST(36,'reaction:   '//NICREAC)
!!!      CALL PUST(36,'plotfile:   '//PLFILE)
!      IF (NPLOTS.GT.0) THEN
!        DO I=1,NPLOTS
!          CALL PUST(36,'plotfile:   '//AUTOPLOT(I))
!        END DO
!      END IF
!      I1=INDEX(EXPER,'  ')
!      CALL PUST(36,'experiment: '//EXPER(1:I1-1))
!!      IF (NPLOTS.GT.0) CALL PUST(36,'plotfile: '//AUTOPLOT(NPLOTS))
!      WRITE (36,2006) TCOR,PCOR,SPIR-1,('+',I=1,27)
! 2006 FORMAT ('+/- ',F8.2,' TC  ',F11.2,' Bar  = ',I2, &
!      ' * uncertainty  ',27A,' assemblage not found')
!!---
      END IF
!+++++
      CALL LABLA(EXPER,I1)
      IF (I1.LT.10) I1=10
      IF (ESPIR.EQ.0) THEN
        IF (PEXP.EQ.0) THEN
          WRITE (39,4010) EXPER(1:I1),TEM,PRE,INCSTR
 4010     FORMAT (/,3X,A10,2X,'assemblage:  T = ',F8.1,'  P =',F10.1,10X,A,'in a Galaxy far far away')
        IF (ISINC.EQ.0) THEN
!!          NINC=NINC+1
          ISINC=1
          CCODE=1
        END IF
        ELSE
          WRITE (39,4011) BLAN(1:I1),TEM,PRE,INCSTR
 4011     FORMAT (3X,A10,2X,'assemblage:  T = ',F8.1,'  P =',F10.1,10X,A,'in a Galaxy far far away')
        IF (ISINC.EQ.0) THEN
!!          NINC=NINC+1
          ISINC=1
          CCODE=1
        END IF
        END IF
        PEXP=PEXP+1
      END IF
      IF (ESPIR.GT.0) THEN
        IF (SPIR.LE.2) THEN

!      WRITE (39,1007) EXPER(1:I1),TEM,PRE
! 1007 FORMAT (/,3X,A10,2X,'assemblage:  T = ',F8.1,'  P =',F10.1,10X,'OK(s)')

          IF (PEXP.EQ.0) THEN
            WRITE (39,4000) EXPER(1:I1),TEM,PRE
 4000       FORMAT (/,3X,A10,2X,'assemblage:  T = ',F8.1,'  P =',F10.1,10X,'OK(stable)')
          ELSE
            WRITE (39,4001) BLAN(1:I1),TEM,PRE
 4001       FORMAT (3X,A10,2X,'assemblage:  T = ',F8.1,'  P =',F10.1,10X,'OK(stable)')
          END IF
          PEXP=PEXP+1
        ELSE
          WRITE (UNIT=TEXT2,FMT='(''('',I3,''x)'')') SPIR-1
          CALL COLLAPS(TEXT2,I2)
          IF (PEXP.EQ.0) THEN
            WRITE (39,4005) EXPER(1:I1),TEM,PRE,INCSTR,TEXT2(1:I2)
 4005       FORMAT (/,3X,A10,2X,'assemblage:  T = ',F8.1,'  P =',F10.1,10X,A,'outside uncertainties :',A)
        IF (ISINC.EQ.0) THEN
!!          NINC=NINC+1
          ISINC=1
          CCODE=1
        END IF
          ELSE
            WRITE (39,4006) BLAN(1:I1),TEM,PRE,INCSTR,TEXT2(1:I2)
 4006       FORMAT (3X,A10,2X,'assemblage:  T = ',F8.1,'  P =',F10.1,10X,A,'outside uncertainties :',A)
        IF (ISINC.EQ.0) THEN
!!          NINC=NINC+1
          ISINC=1
          CCODE=1
        END IF
          END IF
          PEXP=PEXP+1
        END IF
      END IF
!+++++
      IF (ESPIR.GT.0) THEN
      WRITE (scr,FMT='(5X,49A1)') ('-',I=1,49)
      WRITE (35,FMT='(5X,49A1)') ('-',I=1,49)
      IF (SPIR.LE.2) THEN
      WRITE (scr,2002) -TCOR,0.0D0,TCOR,SPIR-1
      WRITE (35,2002) -TCOR,0.0D0,TCOR,SPIR-1
 2002 FORMAT (5X,'T(+) =',12X,'|',3(2X,F8.2),9X,'= ',I2, &
      ' * uncertainty')
      ELSE
      WRITE (scr,2001) -TCOR,0.0D0,TCOR,SPIR-1,('+',I=1,23)
      WRITE (35,2001) -TCOR,0.0D0,TCOR,SPIR-1,('+',I=1,23)
 2001 FORMAT (5X,'T(+) =',12X,'|',3(2X,F8.2),9X, &
      '= ',I2,' * uncertainty  +',23A,' reaction too far away')
      CCODE=5
!!--- summary
!      WRITE (UNIT=36,FMT='('' '')')
!      CALL FIBLA(REFER,I1)
!      CALL PUST(36,REFER(I1:))
!      CALL PUST(36,'reaction:   '//NICREAC)
!!!      CALL PUST(36,'plotfile:   '//PLFILE)
!      IF (NPLOTS.GT.0) THEN
!        DO I=1,NPLOTS
!          CALL PUST(36,'plotfile:   '//AUTOPLOT(I))
!        END DO
!      END IF
!      I1=INDEX(EXPER,'  ')
!      CALL PUST(36,'experiment: '//EXPER(1:I1-1))
!!      IF (NPLOTS.GT.0) CALL PUST(36,'plotfile: '//AUTOPLOT(NPLOTS))
!      WRITE (36,2005) TCOR,PCOR,SPIR-1,('+',I=1,17)
! 2005 FORMAT ('+/- ',F7.2,' TC  ',F8.3,' Bar  = ',I2, &
!      ' * uncertainty  ',17A,' reaction too far away')
!!---
      END IF
      WRITE (scr,FMT='(5X,49A1)') ('-',I=1,49)
      WRITE (35,FMT='(5X,49A1)') ('-',I=1,49)
      DO IPRE=1,-1,-1
      CH001=' '
      FF=DBLE(IPRE)*PCOR
      WRITE (CH001,2000) FF,(GITT(ITEM,IPRE),ITEM=-1,1)
 2000 FORMAT (5X,'P(+) = ',F9.2,2X,'|',6X,A4,6X,A4,6X,A4)
!      CALL LABLA(CH001,I1)
!      IF (GITT(1,IPRE).NE.'(OK)') I1=I1+1
!      IF (GITT(-1,IPRE).EQ.'(OK)'.OR.GITT(0,IPRE).EQ.'(OK)' &
!      .OR.GITT(1,IPRE).EQ.'(OK)') CH001(I1+9:)=OKTEXT
      CALL PUST(scr,CH001)
      CALL PUST(35,CH001)
      END DO
!----- define consistent T and P ?
      FF=SUMT/DBLE(ESPIR)
      IKONT=0
      IF (FF.GT.1D-8) IKONT=1
      IF (FF.LT.-1D-8) IKONT=-1
      FF=SUMP/DBLE(ESPIR)
      IKONP=0
      IF (FF.GT.1D-8) IKONP=1
      IF (FF.LT.-1D-8) IKONP=-1
      IF (GITT(IKONT,IKONP).NE.'(OK)') THEN
       IKONT=ITBACK
       IKONP=IPBACK
      END IF
!====
      TC=TNOW+DBLE(IKONT)*TCOR
      IF (TC.LT.0.D0) TC=0.0D0
      P=PNOW+DBLE(IKONP)*PCOR
      IF (P.LT.1.0D0) P=1.0D0
      CALL NURVONPT
      CALL MACHACT
      CALL CALSTR
      CALL THERIA
!====
      ASSTEXT=' '
      DO I=1,NUN2
       I1=I
       CALL GETNAME(I1,IS,TEXT)
       FF=NN(I)
       CH40=' '
       WRITE (UNIT=CH40,FMT='(F10.3)') NN(I)
       CALL FIBLA(CH40,I2)
       IF (I2.EQ.0) I2=1
       CALL LABLA(CH40,I3)
       IF (I3.EQ.0) I3=1
       CALL LABLA(ASSTEXT,I1)
       IF (I.EQ.1) THEN
       ASSTEXT(I1+2:)=CH40(I2:I3)//' '//TEXT
       ELSE
       ASSTEXT(I1+2:)='+ '//CH40(I2:I3)//' '//TEXT
       END IF
      END DO
!====
      CH001=' '
      WRITE (CH001,1002) TC,P
 1002 FORMAT (5X,'TC =',F8.2,'  P =',F9.2)
      CALL PUST(scr,' ')
      CALL PUST(35,' ')
      CALL PUST(scr,CH001)
      CALL PUST(35,CH001)
      CALL PUST(scr,'     assemblage: '//ASSTEXT)
      CALL PUST(35,'     assemblage: '//ASSTEXT)
      NFEHL1=0
      TC=TNOW
      P=PNOW
!==
!-----
      END IF
!====
      RETURN
      END
!-----
!*************************************************************
!*************************************************************
      SUBROUTINE SEARCHPTX(NFEHL1)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
      include 'checkdb.cmn'
!
!-----END OF COMMON VARIABLES
      CHARACTER(200) CH001
      CHARACTER(250) ASSTEXT,OKTEXT
      CHARACTER(40) CH40
      CHARACTER(32) TEXT
!      CHARACTER(4) GITT(-1:1,-1:1)
      INTEGER(4) I,IS,NFEHL,IP,NPLUS,NMINUS,I1,IM,ITEM,IPRE, &
      ICO2,NFEHL1,I2,I3,COMAY,IFOUND
      REAL(8) TNOW,PNOW,CNOW,TERR,PERR,CERR,FF, &
      F1,F2,F3,F4,ITOK,IPOK,ICOK
!----
      IF (PRTEST) WRITE (UNIT=6,FMT='(''-> SEARCHPTX'')')
      IF (PRTEST) WRITE (UNIT=35,FMT='(''-> SEARCHPTX'')')

!!      CALL PUST(35,BUFORMUL)
!      CALL PUST(35,BUFORMUL0)
!==
      COMAY=COMAX
      IFOUND=0
      TNOW=TC
      PNOW=P
      CNOW=XCO2
      TERR=TERR0
      PERR=PERR0
      CERR=XCO2ERR
      WRITE (scr,FMT='(5X,49A1)') ('-',I=1,49)
      WRITE (35,FMT='(5X,49A1)') ('-',I=1,49)
      WRITE (scr,FMT='(''     check P-T-XCO2'')')
      WRITE (35,FMT='(''     check P-T-XCO2'')')
!      OKTEXT=' '
!+++++
!+++++
      DO ICO2=1,-1,-1

       XCO2=CNOW+DBLE(ICO2)*CERR
       FF=1000.0D0
       F1=FF*2.0D0*(1.0D0-XCO2)
       F2=FF*(1.0D0-XCO2)
       F3=FF*XCO2
       F4=FF*2.0D0*XCO2
       WRITE (UNIT=FLUFORMUL,FMT=1057) F1,F2,F3,F4
 1057  FORMAT ('H(',F8.1,')O(',F8.1,')C(',F8.1,')O(',F8.1,')')
       CALL COLLAPS(FLUFORMUL,I1)
!       WRITE (UNIT=35,FMT='(''FLU='',A,''='')') FLUFORMUL(1:I1)
       CALL LABLA(BUFORMUL0,I2)
       BUFORMUL=BUFORMUL0(1:I2)//FLUFORMUL(1:I1)
       FORMUL=BUFORMUL
       USE=BULKUSE
       CALL CHEMIE(COMAY,NC,OXYDE,OXANZ,FORMUL,CHEM)
       CALL LABLA(USE,LUSE)
!      CALL DBREAD
       DO I=1,NUN
        BULK(I)=CHEM(CHMCOD(I))
       END DO

      DO ITEM=1,-1,-1
      DO IPRE=1,-1,-1
      IF (ITEM.EQ.0.AND.IPRE.EQ.0.AND.ICO2.EQ.0) THEN
      GOTO 10
      END IF
!+++++
      TC=TNOW+DBLE(ITEM)*TERR
      IF (TC.LT.0.D0) TC=0.0D0
      P=PNOW+DBLE(IPRE)*PERR
      IF (P.LT.1.0D0) P=1.0D0
      CALL NURVONPT
!      CALL MACHACT
      CALL CALSTR
      CALL THERIA
!====
      ASSTEXT=' '
      DO I=1,NUN2
       I1=I
       CALL GETNAME(I1,IS,TEXT)
       FF=NN(I)
       CH40=' '
       WRITE (UNIT=CH40,FMT='(F10.3)') NN(I)
       CALL FIBLA(CH40,I2)
       IF (I2.EQ.0) I2=1
       CALL LABLA(CH40,I3)
       IF (I3.EQ.0) I3=1
       CALL LABLA(ASSTEXT,I1)
       IF (I.EQ.1) THEN
       ASSTEXT(I1+2:)=CH40(I2:I3)//' '//TEXT
       ELSE
       ASSTEXT(I1+2:)='+ '//CH40(I2:I3)//' '//TEXT
       END IF
      END DO
!==
      NFEHL=0
!== check if an EXPEC phase is missing
      DO IP=1,NPASS
      NPLUS=0
      IPASS(IP)=0
      DO I=1,NUN2
       I1=I
       CALL GETNAME(I1,IS,TEXT)
       IF (VERGL(TEXT,PASS(IP))) THEN
        NPLUS=NPLUS+1
        IPASS(IP)=I
       END IF
      END DO
!!!!!!!!!!!!       IF (NPLUS.NE.PMULT(IP)) THEN
       IF (NPLUS.EQ.0.AND.PMULT(IP).GT.0) THEN
       NFEHL=NFEHL+1
       END IF
      END DO
!== check if a not EXPEC phase is present
      DO IM=1,NMASS
      NMINUS=0
      DO I=1,NUN2
       I1=I
       CALL GETNAME(I1,IS,TEXT)
       IF (VERGL(TEXT,MASS(IM))) THEN
        NMINUS=NMINUS+1
       END IF
      END DO
       IF (NMINUS.NE.0) THEN
       NFEHL=NFEHL+1
        END IF
      END DO
!--
      IF (NFEHL.EQ.0) THEN
      IFOUND=IFOUND+1
      IF (IFOUND.EQ.1) THEN
      WRITE (35,1002)
 1002 FORMAT (8X,'T +/-',6X,'P +/-',5X,'XCO2 +/-')
      END IF
      OKTEXT=ASSTEXT
      ICOK=ICO2
      ITOK=ITEM
      IPOK=IPRE
      WRITE (scr,1004) DBLE(ITEM)*TERR,DBLE(IPRE)*PERR,DBLE(ICO2)*CERR
      WRITE (35,1004) DBLE(ITEM)*TERR,DBLE(IPRE)*PERR,DBLE(ICO2)*CERR
 1004 FORMAT (5X,F7.2,4X,F8.3,4X,F7.4,'      assemblage stable (OK)')
!=====
      END IF
!+++++
   10 CONTINUE
      END DO
      END DO
      END DO
!+++++
!+++++
      IF (IFOUND.EQ.0) THEN
      CH001=' '
      WRITE (CH001,1010) TERR,PERR,CERR
 1010 FORMAT (5X,'T+/- =',F8.2,'   P+/- =',F11.2,'   XCO2+/- =',F7.4)
      CALL LABLA(CH001,I1)
      CALL LABLA(CH001,I1)
        DO I=I1+2,108
         CH001(I:I)='+'
        END DO
      CH001(97:)=' assemblage not within PTX-bracket'
      CALL PUST(scr,CH001)
      CALL PUST(35,CH001)
      CCODE=10
      MCODE=1
!!--- summary
!      WRITE (UNIT=36,FMT='('' '')')
!      CALL FIBLA(REFER,I1)
!      CALL PUST(36,REFER(I1:))
!      CALL PUST(36,'reaction:   '//NICREAC)
!!!      CALL PUST(36,'plotfile:   '//PLFILE)
!      IF (NPLOTS.GT.0) THEN
!        DO I=1,NPLOTS
!          CALL PUST(36,'plotfile:   '//AUTOPLOT(I))
!        END DO
!      END IF
!      I1=INDEX(EXPER,'  ')
!      CALL PUST(36,'experiment: '//EXPER(1:I1-1))
!      WRITE (36,2006) TNOW,PNOW,CNOW,('+',I=1,27)
! 2006 FORMAT ('TC = ',F7.2,'  P= ',F12.3,'  XCO2 = ',F7.4, &
!      '   ',27A,' assemblage not within PTX-bracket')
!!---
      CALL LABLA(EXPER,I1)
      IF (I1.LT.10) I1=10
      IF (PEXP.EQ.0) THEN
        WRITE (39,4005) EXPER(1:I1),TEM,PRE
 4005   FORMAT (/,3X,A10,2X,'assemblage:  T = ',F8.1,'  P =',F10.1,20X,'assemblage not within PTX uncertainties')
        IF (ISINC.EQ.0) THEN
!!          NINC=NINC+1
          ISINC=1
          CCODE=1
        END IF
      ELSE
        WRITE (39,4006) BLAN(1:I1),TEM,PRE
 4006   FORMAT (3X,A10,2X,'assemblage:  T = ',F8.1,'  P =',F10.1,20X,'assemblage not within PTX uncertainties')
        IF (ISINC.EQ.0) THEN
!!          NINC=NINC+1
          ISINC=1
          CCODE=1
        END IF
      END IF
      PEXP=PEXP+1
      END IF
!+++++
!+++++
      IF (IFOUND.GT.0) THEN
      CCODE=0
      CALL LABLA(EXPER,I1)
      IF (I1.LT.10) I1=10
      IF (PEXP.EQ.0) THEN
        WRITE (39,4010) EXPER(1:I1),TEM,PRE
 4010   FORMAT (/,3X,A10,2X,'assemblage:  T = ',F8.1,'  P =',F10.1,10X,'OK(stable)')
      ELSE
        WRITE (39,4011) BLAN(1:I1),TEM,PRE
 4011   FORMAT (3X,A10,2X,'assemblage:  T = ',F8.1,'  P =',F10.1,10X,'OK(stable)')
      END IF
      PEXP=PEXP+1
      END IF
      NFEHL1=0
!-----
      TC=TNOW
      P=PNOW
      XCO2=CNOW
!====
      RETURN
      END
!-----
!*************************************************************
!*************************************************************
      SUBROUTINE PRTFIRST
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
      include 'checkdb.cmn'
!
!-----END OF COMMON VARIABLES
      INTEGER(4) LTI,LEX,J,I1,I2
      CHARACTER(500) CH001
!---
      IF (PRTEST) WRITE (UNIT=6,FMT='(''-> PRTFIRST'')')
      IF (PRTEST) WRITE (CH001,FMT='(''-> PRTFIRST'')')
!
      CALL LABLA(TITLE,LTI)
      CALL FIBLA(TITLE,I1)
      IF (I1.EQ.0) I1=1
      IF (LTI.EQ.0) LTI=1
      CALL LABLA(EXPER,LEX)
      CALL FIBLA(EXPER,I2)
      WRITE (scr,1000) ('=',J=1,130)
      WRITE (35,1000) ('=',J=1,130)
 1000 FORMAT (/130A1)
!
      IF (.NOT.USEDFOR) THEN
      CH001=' '
      DO J=1,109
       CH001(J:J)='='
      END DO
      CH001(111:)='not used for fitting'
      CALL PUST(scr,CH001)
      CALL PUST(35,CH001)
      END IF
!
      CH001=TITLE(I1:)
      CALL PUSTCOL(scr,CH001,2,129)
      CALL PUSTCOL(35,CH001,2,129)
      CH001=EXPER(1:LEX)
      CALL PUSTCOL(scr,CH001,2,129)
      CALL PUSTCOL(35,CH001,2,129)
      IF (NICREAC.NE.' ') THEN
       CALL PUST(scr,' REAC: '//NICREAC)
       CALL PUST(35,' REAC: '//NICREAC)
      END IF
      IF (INFO1.NE.' ') THEN
       CALL PUST(scr,' INFO1: '//INFO1)
       CALL PUST(35,' INFO1: '//INFO1)
      END IF
      IF (COMM1.NE.' ') THEN
       CALL PUST(scr,' COMM1: '//COMM1)
       CALL PUST(35,' COMM1: '//COMM1)
      END IF
      IF (COMM2.NE.' ') THEN
       CALL PUST(scr,' COMM2: '//COMM2)
       CALL PUST(35,' COMM2: '//COMM2)
      END IF
      IF (NLOGA.GT.0) THEN
      DO J=1,NLOGA
       CH001=' '
       WRITE (UNIT=CH001,FMT='(''log a('',A)') ALOGA(J)
       CALL LABLA(CH001,I1)
       WRITE (UNIT=CH001(I1+1:),FMT='('') = '',F10.4)') XLOGA(J)
       CALL PUST(scr,CH001)
       CALL PUST(35,CH001)
      END DO
      END IF
      IF (XCO2.GT.0.0D0) THEN
       CH001=' '
       WRITE (UNIT=CH001,FMT='(''X(CO2) = '',2(2X,F7.4))') XCO2,XCO2ERR
       CALL PUST(scr,CH001)
       CALL PUST(35,CH001)
      END IF
      IF (NPLOTS.GT.0) THEN
       DO J=1,NPLOTS
        CALL LABLA(AUTOPLOT(J),I1)
        CH001=' plotfile: '//NEWDIR(1:LDIR)//AUTOPLOT(J)(1:I1)//'  typ: '//AUTOTYP(J)
        CALL PUST(scr,CH001)
        CALL PUST(35,CH001)
       END DO
      END IF

!-----
      RETURN
      END
!-----
!*************************************************************
!*************************************************************
      SUBROUTINE MACHACT
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
      include 'checkdb.cmn'
!
!-----END OF COMMON VARIABLES
      INTEGER(4) I,IT,IFF
      REAL(8) FF
!--
      DO I=1,NLOGA
       IFF=0
       DO IT=1,NPHA
        IF (NAME(IT).EQ.ALOGA(I)) IFF=IT
       END DO
       IF (IFF.NE.0) THEN
        FF=XLOGA(I)*2.302585093D0
        FF=FF*8.3143D0
        FF=FF*(TC+273.15D0)
        GGK(IFF)=GGK(IFF)+FF
       END IF
      END DO
!----
      RETURN
      END
!-----
!*************************************************************
!*************************************************************
!      SUBROUTINE BINVAL(CH001)
!      IMPLICIT NONE
!      INCLUDE 'theriak.cmn'
!      include 'files.cmn'
!      include 'checkdb.cmn'
!!
!      CHARACTER*(*) CH001
!      CHARACTER(250) CH002
!      CHARACTER(32) CH1,KEY
!      INTEGER(4) I1,I2,I3,I0
!!
!      CH002=CH001
!      CALL TAXI(CH002,KEY)
!      CALL TAXI(CH002,CH1)
!      CALL TRANSL(CH1)
!      CALL LABLA(KEY,I0)
!      CALL LABLA(CH1,I1)
!      CALL FIBLA(CH002,I2)
!      CALL LABLA(CH002,I3)
!      CH001=KEY(1:I0)//'   '//CH1(1:I1)//'   '//CH002(I2:I3)
!!
!      RETURN
!      END
!!-----
!*************************************************************
!*************************************************************
      SUBROUTINE GETRAT(CH001,PH,EL1,EL1DIV,EL1CH, &
      EL2,EL2DIV,EL2CH,PLM,PLP,PLW)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
      include 'checkdb.cmn'
!
      CHARACTER*(*) CH001,PH,EL1,EL2,EL1CH,EL2CH
      CHARACTER(250) CH002
      CHARACTER(32) CH1,KEY
      REAL(8) PLM,PLP,PLW,EL1DIV,EL2DIV
      INTEGER(4) I1,I2,I3,I0
!
      CH002=CH001
      CALL TAXI(CH002,KEY)
      CALL TAXI(CH002,CH1)
      CALL TRANSL(CH1)
      PH=CH1
      CALL LABLA(KEY,I0)
      CALL LABLA(CH1,I1)
      CALL FIBLA(CH002,I2)
      CALL LABLA(CH002,I3)
      CH001=KEY(1:I0)//'   '//CH1(1:I1)//'   '//CH002(I2:I3)
!
      CALL FUNTAXI(CH002,EL1,EL1DIV,EL1CH)
      CALL FUNTAXI(CH002,EL2,EL2DIV,EL2CH)
      CALL GELI(CH002,PLM)
      CALL GELI(CH002,PLP)
      CALL GELI(CH002,PLW)
!
      RETURN
      END
!
!!-----
!*************************************************************
!*************************************************************
      SUBROUTINE GETRATX(CH001,PH,EL1,EL1DIV,EL1CH, &
      EL2,EL2DIV,EL2CH)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
      include 'checkdb.cmn'
!
      CHARACTER*(*) CH001,PH,EL1,EL2,EL1CH,EL2CH
      CHARACTER(250) CH002
      CHARACTER(32) CH1,KEY
      REAL(8) EL1DIV,EL2DIV
      INTEGER(4) I1,I2,I3,I0
!
      CH002=CH001
      CALL TAXI(CH002,KEY)
      CALL TAXI(CH002,CH1)
      CALL TRANSL(CH1)
      PH=CH1
      CALL LABLA(KEY,I0)
      CALL LABLA(CH1,I1)
      CALL FIBLA(CH002,I2)
      CALL LABLA(CH002,I3)
      CH001=KEY(1:I0)//'   '//CH1(1:I1)//'   '//CH002(I2:I3)
!
      CALL FUNTAXI(CH002,EL1,EL1DIV,EL1CH)
      CALL FUNTAXI(CH002,EL2,EL2DIV,EL2CH)
!
      RETURN
      END
!
!-----
!*************************************************************
!*************************************************************
      SUBROUTINE SHORTSUM
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
      include 'checkdb.cmn'
!
      CHARACTER(20) TEXT
      INTEGER(4) I1,IS,I,J
      DO I=1,NUN2
       I1=I
       CALL GETNAME(I1,IS,TEXT)
       CALL LABLA(TEXT,J)
      WRITE (6,1000) NN(I),TEXT(1:J)
      WRITE (35,1000) NN(I),TEXT(1:J)
 1000 FORMAT ('     n, phase :',F10.4,2X,A)
      END DO
!---
      RETURN
      END
!
!-----
!*************************************************************
!*************************************************************
      SUBROUTINE GUESSZ(WW,Z1,ZZ)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
      include 'checkdb.cmn'
      INTEGER(4) IP,IS,IE,II
      REAL(8) WW,Z1,ZZ,WERT,FAK1,TOLER
!
      TOLER=0.25D0
      FAK1=6.02214129D0/10.0D0/Z1
      IF (KEY1.EQ.'TK') TC=TEM-273.15D0
      P=PRE
      CALL NURVONPT
      CALL CALSTR
      CALL THERIA
      CALL GETVAL(CHKPH,CHKEL,CHKELDIV,IP,IS,IE,WERT)
      WRITE (UNIT=6,FMT='(''V(mea) = '',F20.10)') WW
      WRITE (UNIT=6,FMT='(''V(cal) = '',F20.10)') WERT
      IF (DABS(WERT).LT.1D-12) THEN
        ZZ=Z1
        RETURN
      END IF
      ZZ=WW*Z1/WERT
      II=NINT(ZZ)
      ZZ=REAL(II)
!!      IF (DABS(ZZ-Z1).GT.TOLER) THEN
!!        WRITE (UNIT=6,FMT='(''ZZ,Z1 = '',2F20.10)') ZZ,Z1
!!        CALL EXIT
!!      END IF
!
      RETURN
      END
!
!******************************
      SUBROUTINE FUNTAXI(CH,CH1,F1,F1CH)
      implicit none         
      CHARACTER*(*) CH,CH1,F1CH
      CHARACTER(32) CH001,CH002
      REAL(8) F1,FF
      INTEGER(4) I1
      LOGICAL(4) VERGL
!----
      CALL TAXI(CH,CH001)
      I1=INDEX(CH001,'/')
!      IF (I1.EQ.0.OR.CH001.EQ.'V/V0') THEN
      IF (I1.EQ.0.OR.VERGL(CH001,'V/V0')) THEN
       CH1=CH001
       F1=1.0D0
       F1CH=CH001
      ELSE
       CH1=CH001(1:I1-1)
       CH002=CH001(I1+1:)
       F1CH=CH001
       CALL GELI(CH002,FF)
       F1=FF
      END IF
!----
      RETURN
      END
!-----
!******************************
      SUBROUTINE CHECKNAME1(NAME1,NAME2)
      IMPLICIT NONE
!-----
      CHARACTER*(*) NAME1,NAME2
      CHARACTER*(100) CH001
      CHARACTER(19) VERBOTEN
      INTEGER(4) I,I1,J,K,I2
      DATA VERBOTEN /'*,!@%$^&|~<>/.#()+='/
!-----
      CH001=NAME1
      CALL LABLA(CH001,I1)
      DO 500,I=1,I1
      J=INDEX(VERBOTEN,CH001(I:I))
      K=ICHAR(CH001(I:I))
      IF (J.NE.0.OR.K.LE.32.OR.K.EQ.92) CH001(I:I)=' '
  500 CONTINUE
!---- start collapsing
      I2=0
      NAME2=' '
      DO I=1,I1
       IF (CH001(I:I).NE.' ') THEN
        I2=I2+1
        NAME2(I2:I2)=CH001(I:I)
       END IF
      END DO
!=====
      RETURN
      END
!-----
!******************************
      SUBROUTINE MINMAX(W1,CH16,FMIN,FMAX)
      IMPLICIT NONE
!-----
      CHARACTER*(*) CH16
      REAL(8) W1,FMIN,FMAX,F1,FF
      INTEGER(4) III,I1
!---- III = 0 : values are minimum and maximum
!           xmin   xmax
!---- III = 1 : values are value and error
!           value    +err
!---- III = 2 : values are value and error in %
!           value    per%
!-----
      III=0
      IF (CH16(1:1).EQ.'+') THEN
        III=1
        CH16(1:1)=' '
      END IF
      CALL LABLA(CH16,I1)
      IF (I1.EQ.0) I1=1
!
!      WRITE(6,*) '+'//CH16//'+'
!
      IF (CH16(I1:I1).EQ.'%') THEN
        III=2
        CH16(I1:I1)=' '
      END IF
      FMIN=W1
      CALL GELI(CH16,FMAX)
!--
      IF (III.EQ.1) THEN
      FF=FMIN
      F1=FMAX
      FMIN=FF-F1
      FMAX=FF+F1
      END IF
      IF (III.EQ.2) THEN
      FF=FMIN
      F1=FMAX
      FMIN=FF-F1*FF/100.0D0
      FMAX=FF+F1*FF/100.0D0
      END IF
!=====
      RETURN
      END
!-----
!******************************
      SUBROUTINE TRANSL(CH16)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
      include 'checkdb.cmn'
!-----
      CHARACTER*(*) CH16
      INTEGER(4) I
!-----
      DO I=1,NTRANS
        IF (VERGL(TRANS(I,1),CH16)) CH16=TRANS(I,2)
      END DO
!=====
      RETURN
      END
!-----
!******************************
      SUBROUTINE ELEINDB(CH500,ISU)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
      include 'checkdb.cmn'
!-----
      CHARACTER*(*) CH500
      CHARACTER(16) EL1
      INTEGER(4) ISU,I,I1,I2,IFOUND
!-----
      ISU=0
      CALL LABLA(CH500,I1)
      I2=0
      DO I=1,I1
        IF (CH500(I:I).EQ.'(') I2=1
        IF (CH500(I:I).EQ.')') THEN
          CH500(I:I)=' '
          I2=0
        END IF
        IF (I2.EQ.1) CH500(I:I)=' '
      END DO
      IFOUND=0
   10 CALL TAXI(CH500,EL1)
      IF (EL1.EQ.' ') GOTO 12
      DO I=1,NELEDB
        IF (EL1.EQ.ELEDB(I)) THEN
         IFOUND=1
         GOTO 11
        END IF
      END DO
   11 CONTINUE
      IF (IFOUND.EQ.0) THEN
        WRITE (UNIT=6,FMT='(''element not found: '',a)') EL1
        WRITE (UNIT=35,FMT='(''element not found: '',a)') EL1
!        WRITE (UNIT=36,FMT='(''element not found: '',a)') EL1
        ISU=1
        RETURN
      END IF
      GOTO 10
   12 CONTINUE
!=====
      RETURN
      END
!-----
!******************************
      SUBROUTINE PHAINDB(ISU)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
      include 'checkdb.cmn'
!-----
      INTEGER(4) ISU,I,II,IFOUND,I1,I2,I3,IO,I001
      CHARACTER(200) CH001
!-----
      ISU=0
      DO I=1,NCHOOSE
        IFOUND=0
        DO II=1,NPHADB
          IF (VERGL(CHOOSE(I),PHADB(II))) THEN
            IFOUND=1
            GOTO 11
          END IF
        END DO
   11   CONTINUE

      IF (IFOUND.EQ.0) THEN
        CALL LABLA(filename(drv),I1)
        CALL LABLA(REAID,I3)
        CALL LABLA(DBUSED,I2)
        CALL LABLA(CHOOSE(I),I001)
        WRITE (55,1010) filename(drv)(1:I1),REAID(1:I3),DBUSED(1:I2),CHOOSE(I)(1:I001)
        ISU=1
      END IF
 1010 FORMAT (' file ',A,3X,A,3X,A,'   missing:   ',A)

      IF (IFOUND.EQ.-2) THEN
        CALL LABLA(CHOOSE(I),I001)
!
!-- open check_missing_database_reafile_reaid
        CALL LABLA(filename(drv),I1)
        CALL LABLA(DBUSED,I2)
        CALL LABLA(REAID,I3)
        CH001='check_missing_'//DBUSED(1:I2)//'_'//filename(drv)(1:I1)//'_'//REAID(1:I3)
        CALL LABLA(CH001,I1)
        OPFILE=NEWDIR(1:LDIR)//CH001(1:I1)
        CALL PUST(6,'Open file 36: '//OPFILE)
        CALL LABLA(OPFILE,IO)
!        WRITE (UNIT=6,FMT='(''OPEN 6983 '',A)')OPFILE(1:IO)
        OPEN (UNIT=36,FILE=OPFILE(1:IO),STATUS='UNKNOWN')
        CALL PUST(36,CH001)
        CALL PUST(36,' ')
!
        WRITE (UNIT=6,FMT='('' phase not found: '',a)') CHOOSE(I)(1:I001)
        WRITE (UNIT=36,FMT='('' =============================='')')
        WRITE (UNIT=36,FMT='('' phase not found: '',a)') CHOOSE(I)(1:I001)
        WRITE (UNIT=36,FMT='('' =============================='')')
        ISU=1
        CLOSE (UNIT=36)
!TEST        RETURN
      END IF
      END DO
!=====
      RETURN
      END
!-----
!-----
!******************************
      SUBROUTINE CHECKNAME(DIRNAME,ILMAX)
      IMPLICIT NONE
!-----
!---  this is the very restricive version
!-----
      CHARACTER*(*) DIRNAME
      CHARACTER(64) ERLAUBT
      INTEGER(4) I,I1,J,ILMAX
      ERLAUBT='0123456789'// &
      'ABCDEFGHIJKLMNOPQRSTUVWXYZ'// &
      'abcdefghijklmnopqrstuvwxyz'// &
      '_#'
!-----
      ILMAX=30

      CALL LABLA(DIRNAME,I1)
      IF (I1.GT.ILMAX) THEN
      DO I=ILMAX+1,I1
        DIRNAME(I:I)=' '
      END DO
      I1=ILMAX
      END IF
      DO I=1,I1
        J=INDEX(ERLAUBT,DIRNAME(I:I))
        IF (J.EQ.0) DIRNAME(I:I)='a'
      END DO
!=====
      RETURN
      END
!-----
!******************************
      SUBROUTINE PREMAKEF(DIRNAME,ILMAX,IL)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      INCLUDE 'files.cmn'
!-----
      CHARACTER*(*) DIRNAME
      CHARACTER(200) CH200
      INTEGER(4) ILMAX,I1,I2,IL
!----- 1: stringlength maximal ILMAX including "_" and dir
!----- (length is now set to 30 in CHECKNAME)
      CALL FIBLA(DIRNAME,I1)
      CALL LABLA(DIRNAME,I2)
      IF (DIRNAME(I1:I1).EQ.'_') I1=I1+1
      IF (DIRNAME(I2:I2).EQ.dir) I2=I2-1
      CH200='_'//DIRNAME(I1:I2)
!----- 2: remove any forbidden characters
      CALL CHECKNAME(CH200,ILMAX)
      CALL LABLA(CH200,IL)
      IL=IL+1
      CH200(IL:IL)=dir
      WRITE (scr,1000) CH200(1:IL)
 1000 FORMAT (/,' proposed folder name: ',A)

      DIRNAME=CH200(1:IL)
      RETURN
      END
!-----
!******************************
!-----
!******************************
      SUBROUTINE NEWREADIR
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      INCLUDE 'files.cmn'
      include 'checkdb.cmn'
!-----
      INTEGER(4) ILMAX,IL,I1,I2,IO,ISD
      CHARACTER(120) DIRNAME
      CHARACTER(200) CH001
!-    DIRNAME is local (modified in Subroutines)
!-    The global variable for the directory is NEWDIR
      DIRNAME=REAID

      WRITE (UNIT=6,FMT='('' in newreadir: dirname='',a)') DIRNAME

      CALL COLLAPS(DIRNAME,IL)
      CALL PREMAKEF(DIRNAME,ILMAX,IL)
      CALL MAKEFOLDER(DIRNAME)
      NEWDIR=DIRNAME(1:IL)
      LDIR=IL
      CLOSE(UNIT=35)
!!      CLOSE(UNIT=36)
!!      CLOSE(UNIT=37)
!!      CLOSE(UNIT=38)
      CLOSE(UNIT=39)
!!      CLOSE(UNIT=51)

!--
      CALL LABLA(sdate,ISD)
!!-open reatable_database
!      CALL LABLA(DBNAME,I1)
!      OPFILE=NEWDIR(1:LDIR)//'reatable_'//DBNAME(1:I1)
!      CALL PUST(6,'Open file 37: '//OPFILE)
!      CALL LABLA(OPFILE,IO)
!      OPEN (UNIT=37,FILE=OPFILE(1:IO),STATUS='UNKNOWN')
!      CALL PUST(37,'reatable')
!      CALL PUST(37,' ')
!!--
!!-open checkshort_database
!      CALL LABLA(DBNAME,I1)
!      OPFILE=NEWDIR(1:LDIR)//'checkshort_'//DBNAME(1:I1)
!      CALL PUST(6,'Open file 38: '//OPFILE)
!      CALL LABLA(OPFILE,IO)
!      OPEN (UNIT=38,FILE=OPFILE(1:IO),STATUS='UNKNOWN')
!      CALL PUST(38,'short '//DBNAME(1:I1)//'  '//sdate(1:ISD))
!      CALL PUST(38,' ')
!!--
!-open checkquiteshort_database
      CALL LABLA(DBNAME,I1)
      OPFILE=NEWDIR(1:LDIR)//'checkquiteshort_'//DBNAME(1:I1)
      CALL PUST(6,'Open file 39: '//OPFILE)
      CALL LABLA(OPFILE,IO)
!      WRITE (UNIT=6,FMT='(''OPEN 7119 '',A)') OPFILE(1:IO)
      OPEN (UNIT=39,FILE=OPFILE(1:IO),STATUS='UNKNOWN')
      CALL PUST(39,'quiteshort '//DBNAME(1:I1)//'  '//sdate(1:ISD))
      CALL PUST(39,' ')
!
!-open check_database_reafile

      WRITE (UNIT=6,FMT='('' now opening 35 '')') 

      CALL LABLA(filename(drv),I1)
      CALL LABLA(DBUSED,I2)
      CH001='check_'//DBUSED(1:I2)//'_'//filename(drv)(1:I1)
      CALL LABLA(CH001,I1)
      OPFILE=NEWDIR(1:LDIR)//CH001(1:I1)
      CALL PUST(6,'Open file 35: '//OPFILE)
      CALL LABLA(OPFILE,IO)
!      WRITE (UNIT=6,FMT='(''OPEN 7135 '',A)') OPFILE(1:IO)
      OPEN (UNIT=35,FILE=OPFILE(1:IO),STATUS='UNKNOWN')
      CALL PUST(35,CH001(1:I1)//'  '//sdate(1:ISD))
      CALL PUST(35,' ')
!
!!-- open checksummary_database_reafile
!      CH001='checksummary_'//DBUSED(1:I2)//'_'//filename(drv)(1:I1)
!      CALL LABLA(CH001,I1)
!      OPFILE=NEWDIR(1:LDIR)//CH001(1:I1)
!      CALL PUST(6,'Open file 36: '//OPFILE)
!      CALL LABLA(OPFILE,IO)
!      OPEN (UNIT=36,FILE=OPFILE(1:IO),STATUS='UNKNOWN')
!      CALL PUST(36,CH001(1:I1)//'  '//sdate(1:ISD))
!      CALL PUST(36,' ')
!
!!-- open checkphases_database_reafile
!      CH001='checkphases_'//DBUSED(1:I2)//'_'//filename(drv)(1:I1)
!      CALL LABLA(CH001,I1)
!      OPFILE=NEWDIR(1:LDIR)//CH001(1:I1)
!      CALL PUST(6,'Open file 51: '//OPFILE)
!      CALL LABLA(OPFILE,IO)
!      OPEN (UNIT=51,FILE=OPFILE(1:IO),STATUS='UNKNOWN')
!      CALL PUST(51,CH001(1:I1)//'  '//sdate(1:ISD))
!      CALL PUST(51,' ')



      RETURN
      END
!-----
!******************************
