!-----Version: 28.05.2022
!               ***********
!               * THERIAK *
!               ***********
!
!     Program written by Christian de Capitani
!     at the Department of Geological Sciences,
!     University of British Columbia, Vancouver, B.C. Canada
!     (May 1984 - Sept 1987)
!
!     revision: April 1987
!     minor changes: December 1987
!     major revision: July 1993
!     revisions: October 2002, July 2004, February 2005, May 2006, March 2007,
!                December 2007, May 2008, August 2009, June 2014
!
!     for details of algorithm see:
!     de Capitani C. and Brown T.H. : The computation of chemical
!     equilibrium in complex systems containing non-ideal solutions.
!     Geochim. Cosmochim. Acta 51(1987):2639-2652
!
!     Any suggestions, complaints or comments are greatly appreciated
!     by the author and should be sent to:
!          Christian de Capitani
!          Mineralogisch-Petrographisches Institut
!          Universitaet Basel
!          Bernoullistrasse 30
!          CH-4056 BASEL
!
!
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!*****
!
!-----END OF COMMON VARIABLES
!     LPFILE: drv-File used
!     SIM: simulated terminal input
!     LOLO: manual loop
!     CONCH: consistency check (not in theriak)
      LOGICAL(4) LPFILE,SIM,LOLO,CONCH,VARTEST
      INTEGER(4) I001,I002,I,I1,COMAY,NPTS,EINS,ALLES,ierr,j,BIN1, &
      LARG,I2,IP,IS,IE
      REAL(8) FF,FMF
      CHARACTER(16) CH16,CH8,CH32
      CHARACTER(80) KOMMENTAR
      CHARACTER(500) CH001,CH002,SYREC,CHIN(3),ZEITSTRING
!*****
      progname='THERIAK'
      vers='28.05.2022'
      task='"Computation of equilibrium assemblages at given PT"'
      EINS=1
      ierr=0
!---- for no printing in 'initialize', set ierr=9
      call initialize('$THERIAK-FILES',ierr)
      if(ierr.ne.0) STOP
      REFAS=.FALSE.
!
!-----
      VARTEST=.FALSE.
      SIMFILE=.FALSE.
      LARG=0
      IERR=0
      DO I=1,5
       CALL GetLineArgs (I,LARGUM(I),IERR)
       IF(IERR.NE.0.OR.LARGUM(I).EQ.' ')THEN
         GOTO 10
       ELSE
         LARG=LARG+1
       END IF
      END DO
   10 CONTINUE
      IFNR=5
      IF (LARG.GT.0) THEN
       INFILE=LARGUM(1)
       SIMFILE=.TRUE.
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
!
!*****
      CHIN(1)=' '
      CHIN(2)='no'
      CHIN(3)=' '
!------------------
!     Open UNIT=log
!------------------
      j=log
      line=filename(j)(1:fnl(j))//ext(j)
      path=wpath
      akzess=' '
      state=' '
      call openfile(j,ierr)
      if(ierr.ne.0) STOP
!-----
      DO I=1,3
        READ (UNIT=log,FMT='(A500)',END=15) CHIN(I)
      END DO
   15 CONTINUE
      WRITE (6,1000)
 1000 FORMAT (/ &
      ' -------------------'/ &
      ' database definition'/ &
      ' -------------------')
      CALL LABLA(CHIN(1),I002)
      IF (I002.EQ.0) I002=1
      CH002=' Enter [ "?" | CR | "files" | database filename ] <'// &
      CHIN(1)(1:I002)//'>?'
!-----
    2 CONTINUE
      CALL PUST (6,CH002)
      READ (IFNR,FMT='(A500)') CH001
      IF (CH001.EQ.'?') THEN
        CALL helpme('$THK-START')
        GOTO 2
      END IF
      IF (VERGL(CH001,'files')) THEN
        CALL listfiles
        GOTO 2
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
!===== for theriak3 and others
      filename(dbs)=DBNAME
!------------------
!     Open UNIT=out
!------------------
      j=out
      line=filename(j)(1:fnl(j))//ext(j)
      path=wpath
      akzess=' '
      state=' '
      call openfile(j,ierr)
      if(ierr.ne.0) STOP
!------------------
!     open UNIT=dbs
!------------------
      j=dbs
      line=DBNAME
      path=wpath
      akzess=' '
      state='old'
      call openfile(j,ierr)
      if(ierr.ne.0) STOP
!------------------
!     open UNIT=dat
!------------------
      j=dat
      line=filename(j)(1:fnl(j))//ext(j)
      path=wpath
      akzess=' '
      state='old'
      call openfile(j,ierr)
      if(ierr.ne.0) STOP
!-----
      WRITE (scr,1010) DBNAME(1:I001)
      WRITE (out,1010) DBNAME(1:I001)
 1010 FORMAT (/' database for this run: ',A/)
!*****
      COMAY=COMAX
      CALL PROREAD(SYREC)
!-- pick nach proread! da dort NPICK=0
      NPICK=0
      DO I=1,COMAX
       CALL TAXI(CH002,CH16)
       IF (CH16.NE.' ') THEN
       NPICK=NPICK+1
       PICK(NPICK)=CH16
       ELSE
       GOTO 20
       END IF
      END DO
   20 CONTINUE
!--
      Call LABLA(line,I1)
      WRITE (scr,1012) line(1:I1)
      WRITE (out,1012) line(1:I1) 
 1012 FORMAT (/,' Input from file',1x,a)
      write(scr,1014) ('-',I=1,I1+16)
      write(out,1014) ('-',I=1,I1+16)
 1014 format(1x,130a1,:)
      write(scr,1016) TC,P
      write(out,1016) TC,P
 1016 format(' T =',F8.2,' C     P =',F9.2,' Bar')
      CALL PUST (scr,' '//SYREC)
      CALL PUST (out,' '//SYREC)
!*****
    3 WRITE (scr,1020)
      WRITE (out,1020)
 1020 FORMAT (/ &
      ' ---------------------------'/ &
      ' define type of calculations'/ &
      ' ---------------------------')
      CALL LABLA(CHIN(2),I002)
      IF (I002.EQ.0) I002=1
      CH002=' Enter [ "?" | CR | "no" | "bin" | "loop" | "check" |' &
      //' filename ] <'//CHIN(2)(1:I002)//'>?'
      CALL PUST (scr,CH002)
      CH001=' '
      READ (UNIT=IFNR,FMT='(A500)') CH001
!---
      IF (CH001.EQ.'?') THEN
        CALL helpme('$THK-SPECIAL')
        GOTO 3
      END IF
!---
      CALL LABLA(CH001,I001)
      IF (I001.EQ.0) THEN
        CH001=CHIN(2)
        I001=I002
      ELSE
        CHIN(2)=CH001
      END IF
      SIM=.FALSE.
      LOLO=.FALSE.
      CONCH=.FALSE.
      CALL TAXI(CH001,CH16)
!-----
      IF (CH16.EQ.'check') THEN
        VARTEST=.TRUE.
        CH16='no'
      END IF
!-----
      IF (CH16.EQ.' ') CH16='no'
      LPFILE=.FALSE.
!-
!-
      IF (VERGL(CH16,'bin')) THEN
      SIM=.TRUE.
      CALL GELI(CH001,FF)
      I001=IDINT(FF)
      IF (I001.LE.0) THEN
      NPTS=100
      ELSE
      NPTS=I001
      IF (NPTS.GT.400) NPTS=400
      END IF
      CALL GELI(CH001,FF)
      BIN1=IDINT(FF)
      END IF
!-
!-
      IF (VERGL(CH16,'bin++')) THEN
      SIM=.TRUE.
      CALL GELI(CH001,FF)
      I001=IDINT(FF)
      IF (I001.LE.0) THEN
      NPTS=100
      ELSE
      NPTS=I001
      IF (NPTS.GT.400) NPTS=400
      END IF
      BIN1=-1
      END IF
!-
!-
      IF (VERGL(CH16,'fun++')) THEN
      SIM=.TRUE.
      CALL GELI(CH001,FF)
      I001=IDINT(FF)
      IF (I001.LE.0) THEN
      NPTS=100
      ELSE
      NPTS=I001
      IF (NPTS.GT.400) NPTS=400
      END IF
      BIN1=-2
      END IF
!-
!-
      IF (VERGL(CH16,'rea++')) THEN
      SIM=.TRUE.
      CALL GELI(CH001,FF)
      I001=IDINT(FF)
      IF (I001.LE.0) THEN
      NPTS=100
      ELSE
      NPTS=I001
      IF (NPTS.GT.400) NPTS=400
      END IF
      BIN1=-3
      END IF
!-
!-
      IF (VERGL(CH16,'loop')) LOLO=.TRUE.
      IF (.NOT.SIM.AND..NOT.LOLO) LPFILE=.TRUE.
!-
      IF (VERGL(CH16,'no')) LPFILE=.FALSE.
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
      if(ierr.ne.0) STOP
!-----
      DO I=1,3
       CALL PUST(log,CHIN(I))
      END DO
      CLOSE (UNIT=log)
!-----
!-----READ  PRTCOD, FORMUL AND USE FROM SYREC
!-----SET UP FIRST NUN COLUMNS OF MATRIX
      CALL GELI(SYREC,FF)
      PRTCOD=IDINT(FF)
      IF (PRTCOD.GE.10) THEN
       SECO=.TRUE.
       PRTCOD=PRTCOD-10
      END IF
      IF (PRTCOD.EQ.0) TEST=DABS(TEST)
      DO I=1,11
       PRTLOG(I)=.FALSE.
      END DO
      IF (PRTCOD.LE.-2) PRTLOG(1)=.TRUE.
      IF (PRTCOD.EQ.-1) THEN
       DO I=2,5
        PRTLOG(I)=.TRUE.
       END DO
      END IF
      IF (PRTCOD.EQ.0) THEN
       DO I=5,6
        PRTLOG(I)=.TRUE.
       END DO
      END IF
      IF (PRTCOD.GE.1) THEN
       DO I=2,8
        PRTLOG(I)=.TRUE.
       END DO
      END IF
      CALL FIBLA(SYREC,I1)
      IF (I1.EQ.0) I1=1
!      CH002=' bulk: '//SYREC(I1:)
!      CALL PUST(scr,CH002)
!      CALL PUST(out,CH002)
!-----
      IF (PRTLOG(5)) THEN
      WRITE (UNIT=scr,FMT=1030) TEST,LO1MAX,EQUALX,DXMIN
      WRITE (UNIT=out,FMT=1030) TEST,LO1MAX,EQUALX,DXMIN
 1030 FORMAT (/,' TEST =',1PE11.4,8X,'LO1MAX =',I4,13X,'EQUALX =', &
      1PE11.4,6X,'DELXMIN =',1PE11.4)
      WRITE (UNIT=scr,FMT=1032) DXSCAN,DXSTAR,STPSTA,STPMAX,GCMAX
      WRITE (UNIT=out,FMT=1032) DXSCAN,DXSTAR,STPSTA,STPMAX,GCMAX
 1032 FORMAT (' DELXSCAN =',1PE11.4,4X,'DELXSTAR =',1PE11.4,4X, &
      'STEPSTAR =',I4,11X,'STEPMAX =',I4,12X,'GCMAX =',I5)
      END IF
!-----
      CALL TAXI(SYREC,FORMUL)
      NBUL=1
      BULINE(1)=FORMUL
      CALL TAXI(SYREC,USE)
      CALL TAXI(SYREC,KOMMENTAR)
!---- define first three COMINS
      CALL LABLA(CHIN(1),I002)
      NCOMIN=3
      COMINS(1)=KOMMENTAR
      COMINS(2)='theriak version: '//vers(1:10)
!      COMINS(3)='database: '//CHIN(1)(1:I002)
      COMINS(3)='database: '//DBNAME
!+++++
      CALL CHEMIE(COMAY,NC,OXYDE,OXANZ,FORMUL,CHEM)
      CALL LABLA(USE,LUSE)
!-----------------------------------------------------------------
      CALL DBREAD
!---
      IF (PEXCL.GT.0) THEN
      DO I=1,NPHA
       IF (NAME(I)(1:1).EQ.'$') THEN
       NCOMIN=NCOMIN+1
       COMINS(NCOMIN)='excl.: '//NAME(I)(2:)
       IF (NCOMIN.GE.50) GOTO 30
       END IF
      END DO
      END IF
      IF (SEXCL.GT.0) THEN
      DO I=1,NSOL
       IF (EXSOL(I)) THEN
       NCOMIN=NCOMIN+1
       COMINS(NCOMIN)='excl.: '//SOLNAM(I)
       IF (NCOMIN.GE.50) GOTO 30
       END IF
      END DO
      END IF
      DO I=1,NPHA
      IF (LODATA(3,I)) THEN
       NCOMIN=NCOMIN+1
       COMINS(NCOMIN)=ABK(I)
       I1=INDEX(COMINS(NCOMIN),'  ')
       COMINS(NCOMIN)(I1:)=': '//CHDATA(I)
       IF (NCOMIN.GE.60) GOTO 30
      END IF
      END DO
      DO I=1,NSOL
        NCOMIN=NCOMIN+1
        IF (NCOMIN.GE.60) GOTO 30
        CALL LABLA(SOLNAM(I),I001)
        COMINS(NCOMIN)=SOLNAM(I)(1:I001)//': '//SOLINFO(I)
      END DO
  30 CONTINUE
!---
      CALL NURVONPT
      CALL CALSTR
      ALLES=NSOL
      IF (PRTLOG(1)) THEN
      CALL GIBBSTEST(EINS,ALLES)
!!  STOP will generate 
!!  The following floating-point exceptions are signalling: IEEE_INVALID_FLAG IEEE_DIVIDE_BY_ZERO
!!  CALL EXIT seems OK
      CALL EXIT
      STOP
      END IF
      IF (LPFILE.OR.PRTCOD.EQ.0) CALL GIBBSTEST(EINS,ALLES)
      IF (.NOT.LPFILE) THEN
      IF (PRTLOG(2).OR.PRTLOG(3).OR.PRTLOG(4)) CALL PRININ
      IF (PRTCOD.EQ.-1) CALL EXIT
!!      IF (PRTCOD.EQ.-1) STOP
!---
!
      IF (SIM) THEN
!-- this is just in case a "-2" is in THERIN
      PRTLOG(1)=.FALSE.
      IF (BIN1.GE.0) CALL SIMPL(NPTS,BIN1)
      IF (BIN1.EQ.-1) CALL AUTOBIN(NPTS)
      IF (BIN1.EQ.-2) CALL AUTOFUN(NPTS)
      IF (BIN1.EQ.-3) CALL AUTOREA(NPTS)
      END IF
      IF (LOLO) CALL MANLOOP
      IF (.NOT.SIM.AND..NOT.LOLO) CALL THERIA
      CLOSE (dat)
!---
      ELSE
      CLOSE (dat)
      CH001=CHIN(2)
      CALL MINILOOP(CH001)
      END IF
!---
!---  here if input 'check'
      IF (VARTEST) THEN
      WRITE (UNIT=6,FMT='(/''-------------------------'')')
      WRITE (UNIT=6,FMT='(''testing GETVAL subroutine'')')
       DO I=1,1000
     7  WRITE (UNIT=6,FMT='(/''enter: phase  var  (or "?")'')')
        READ (UNIT=IFNR,FMT='(A500)') CH001
        IF (CH001.EQ.' ') GOTO 40
!---
      IF (CH001.EQ.'?') THEN
        CALL helpme('$THK-GETVAL')
        GOTO 7
      END IF
!---

        CALL TAXI(CH001,CH16)


!        I1=INDEX(CH001,'/')
!        IF (I1.EQ.0.) THEN
!         CALL TAXI(CH001,CH8)
!         FMF=1.0D0
!        ELSE
!         CALL FIBLA(CH001,I2)
!         CH8=CH001(I2:I1-1)
!         CH002=CH001(I1+1:)
!         CALL GELI(CH002,FMF)
!        END IF
!      write (unit=6,fmt='(''vor taxi'')')
      CALL FUNTAXI(CH001,CH8,FMF,CH32)
!      write (unit=6,fmt='(''nach taxi'')')

        IF (DABS(FMF).LT.1D-20) FMF=1.0D0
        CALL GETVAL(CH16,CH8,FMF,IP,IS,IE,FF)
        CALL LABLA(CH16,I1)
        CALL LABLA(CH8,I2)
        IF (IP.GT.NUN2) WRITE (6,2000) CH16(1:I1)
 2000 FORMAT (A,' is not stable')
      WRITE (6,2010) CH16(1:I1),CH8(1:I2),FMF,IP,IS,IE
 2010 FORMAT ('"',A,'"',2X,'"',A,'"   FMF = ',F7.2,'    IP = ',I4, &
      '     IS = ',I4,'     IE =',I4)
        WRITE (UNIT=6,FMT='(1PE20.12)') FF
       END DO
   40 CONTINUE
      END IF
!---
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
!       DO I=1,NUN
!         BULK(I)=CHEM(CHMCOD(I))
!       END DO
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
      WRITE (out,150) ZEITSTRING(1:I001)
  150 FORMAT (/,' exit THERIAK',/,1X,A)
      END
!-----
!********************************
      SUBROUTINE MANLOOP
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!-----END OF COMMON VARIABLES
      CHARACTER(500) SYREC
!----
   10 CONTINUE
      SYREC=' '
      WRITE (scr,1000)
      WRITE (out,1000)
 1000 FORMAT (/ &
      ' -------------------------------'/ &
      ' define Temperature and Pressure'/ &
      ' -------------------------------')
      WRITE (scr,1010)
 1010 FORMAT(' Enter [ "?" | CR | "end" | T(C)  P(bar) ]: ')
      READ (UNIT=kbd,FMT='(A500)',END=99) SYREC
      IF (VERGL(SYREC,'end').OR.SYREC.eq.' ') RETURN
      IF (SYREC.EQ.'?') THEN
         CALL helpme('$THK-LOOP')
         GOTO 10
      END IF
      CALL GELI(SYREC,TC)
      CALL GELI(SYREC,P)
      CALL NURVONPT
      CALL CALSTR
      CALL PRININ
      WRITE (UNIT=6,FMT='(''theriak with T='',F10.2,'' P='',F11.1)') TC,P
      CALL THERIA
      GOTO 10
!----
   99 CONTINUE
      RETURN
      END
!-----
!********************************
      SUBROUTINE SIMPL(NPTS,BIN1)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!
!-----END OF COMMON VARIABLES
      INTEGER(4) I,I1,I2,I3,II,COMAY,EINS,ALLES,JX,NPTS,BIN1
      REAL(8) FF,CHEXX(2,COMAX),YWERT(0:1000),XXMIN,XXMAX,YYMIN,YYMAX, &
      FA,FB,BREIT,HOCH,Y0,Y1,NEUWERT(0:1000),XVAL(0:1000), &
      F1,F2,F3,DDX
!      REAL(8) SIZLAB,ANGE
      CHARACTER(500) SYREC,BULKLINE(2)
      CHARACTER(100) XVARI,YVARI
      CHARACTER(80) XTXT,YTXT,DATI,ZEITSTRING
!
      integer ierr,j
!----
      WRITE (UNIT=6,FMT='(''entering BIN'')')
      PRTLOG(9)=.TRUE.
      PRTLOG(2)=.TRUE.
      NVARTBL=0
      NROWTBL=0
      COMAY=COMAX
      EINS=1
!      DO I=1,8
!       PRTLOG(I)=.FALSE.
!      END DO
!---
      DO I=1,2
       READ (UNIT=dat,FMT='(A500)') SYREC
       CALL GELI(SYREC,FF)
       CALL TAXI(SYREC,FORMUL)
       BULKLINE(I)=FORMUL
       CALL LABLA(FORMUL,I2)
       WRITE (UNIT=out,FMT='(170A1)') (FORMUL(I3:I3),I3=1,I2)
       CALL CHEMIE(COMAY,NC,OXYDE,OXANZ,FORMUL,CHE)
       DO II=1,NC
         CHEXX(I,II)=CHE(II)
       END DO
      END DO
      NBUL=3
      BULINE(2)=BULKLINE(1)
      BULINE(3)=BULKLINE(2)
      XXMIN=0.0D0
      XXMAX=1.0D0
      YYMIN=0.0D0
      YYMAX=1.0D0
      BREIT=15.0D0
      HOCH=15.0D0
      XVARI='binary'
      YVARI='G'
!-----
      DO II=1,NC
        CHEM(II)=0.0D0
        DO I=1,2
          CHEM(II)=CHEM(II)+CHEXX(I,II)
        END DO
      END DO
      CALL DBREAD
      WRITE (scr,1000) CHNAME(1),CHEM(CHMCOD(1))
      WRITE (out,1000) CHNAME(1),CHEM(CHMCOD(1))
 1000 FORMAT (' Bulk composition: ',A8,1X,F11.6)
      DO I=2,NUN
!      WRITE (scr,1010) CHNAME(I),CHEM(CHMCOD(I))
       WRITE (out,1010) CHNAME(I),CHEM(CHMCOD(I))
 1010 FORMAT (19X,A8,1X,F11.6)
      END DO
      CALL NURVONPT
      CALL CALSTR
      ALLES=NSOL
      CALL GIBBSTEST(EINS,ALLES)
!----
      DDX=1D-7
      FF=DBLE(NPTS)
      DO 500,JX=0,NPTS
      YWERT(JX)=0.0D0
      IF (BIN1.NE.0.AND.BIN1.NE.JX) GOTO 500
      FB=DBLE(JX)/FF
      FA=1.0D0-FB
      XVAL(JX)=FB
      DO,I=1,NUN
       BULK(I)=0.0D0
      END DO
      DO I=1,NUN
        II=CHMCOD(I)
        BULK(I)=FA*CHEXX(1,II)+FB*CHEXX(2,II)
      END DO
      IF (JX.EQ.0) THEN
        DO I=1,NUN
          II=CHMCOD(I)
          BULK(I)=(1.0D0-DDX)*CHEXX(1,II)+DDX*CHEXX(2,II)
        END DO
      END IF
      IF (JX.EQ.NPTS) THEN
        DO I=1,NUN
          II=CHMCOD(I)
          BULK(I)=DDX*CHEXX(1,II)+(1.0D0-DDX)*CHEXX(2,II)
        END DO
      END IF
!----
      CALL NURVONPT
      CALL CALSTR
      CALL PRININ
      CALL TRENNE(130)
      WRITE (UNIT=scr,FMT=1050) JX
      WRITE (UNIT=out,FMT=1050) JX
 1050 FORMAT (/,' THERIAK-loop = ',I6)
      CALL THERIA
      YWERT(JX)=GGTOT
  500 CONTINUE
!-----
!!!      CALL PRTTBL
!
      NVARTBL=NVARTBL+1
      IF (NVARTBL.GT.MAXVARTBL) THEN
      NVARTBL=NVARTBL-1
      END IF
      VARTBL(NVARTBL)='G(rel)'
!
      Y0=YWERT(0)
      Y1=YWERT(NPTS)
      YYMAX=-1D34
      YYMIN=1D34
      FF=DBLE(NPTS)
      WRITE (UNIT=scr,FMT='('' '')')
      DO 600,I=0,NPTS
        NEUWERT(I)=YWERT(I)-Y0-(Y1-Y0)*DBLE(I)/FF
        IF (NEUWERT(I).GT.YYMAX) YYMAX=NEUWERT(I)
        IF (NEUWERT(I).LT.YYMIN) YYMIN=NEUWERT(I)
!
        OUTTBL(I+1,NVARTBL)=NEUWERT(I)
!
        WRITE (*,1060) I,YWERT(I),NEUWERT(I)
        WRITE (out,1060) I,YWERT(I),NEUWERT(I)
 1060 FORMAT (1X,I4,2X,1PE20.12,2X,1PE20.12)
  600 CONTINUE
!
      CALL PRTTBL
!----
      CALL CPUTIME(ZEITSTRING)
      DATI=sdate
      CALL LABLA(DATI,J)
      DATI=DATI(1:J)//'  '//ZEITSTRING
!------------------
!     open UNIT=inf
!------------------
      j=inf
      line=filename(j)(1:fnl(j))//ext(j)
      path=wpath
      akzess=' '
      state='unknown'
      call openfile(j,ierr)
      if(ierr.ne.0) STOP
!-----
!      WRITE (inf,FMT='(A)') DATI
      CALL PUST(inf,DATI)
      WRITE (inf,FMT='(I4)') NBUL
      DO II=1,NBUL
!       WRITE (inf,FMT='(A)') BULINE(II)
       CALL PUST(inf,BULINE(II))
      END DO
      WRITE (inf,FMT='(I4)') NCOMIN
      DO II=1,NCOMIN
!       WRITE (inf,FMT='(A)') COMINS(II)
       CALL PUST(inf,COMINS(II))
      END DO
      CLOSE (UNIT=inf)
!----
!------------------
!     open UNIT=bin
!------------------
      j=bin
      line=filename(j)(1:fnl(j))//ext(j)
      path=wpath
      akzess=' '
      state=' '
      call openfile(j,ierr)
      if(ierr.ne.0) STOP
!-----
      IF (YYMAX-YYMIN.LT.1D-8) THEN
      WRITE (UNIT=*,FMT=2000) YYMIN
 2000 FORMAT (/,' Minimum and maximum are both = ',1PE10.3)
      YYMAX=YYMAX+DABS(YYMAX*0.05D0+1.0D0)
      YYMIN=YYMIN-DABS(YYMIN*0.05D0+1.0D0)
      END IF
!----
      F1=BREIT+1.0D0
      WRITE (bin,2010) F1,F1
 2010 FORMAT ('NULLPT     5   3'/ &
      'FONT      Helvetica'/ &
      'FAT   0.01'/ &
      'PUNKTE   97  0.5   ',F7.4,'  0.25  999  999 '/ &
      'PUNKTE   98  0.5   ',F7.4,'  0.25  999  999 '/ &
      'FAT   0.02')
      F1=BREIT
      F2=-2.0D0
      F3=0.2D0
      WRITE (bin,2012) DATI,F1,F2,F3
 2012 FORMAT ('TEXTB  ',A,/,3(2X,F9.4),'  0  0  0  0')
      F1=0.0D0
      F3=0.2D0
      DO II=1,NBUL
        F2=HOCH+0.8D0+DBLE(II-1)*0.35D0
        CALL LABLA(BULINE(II),I1)
        WRITE (bin,2014) II,BULINE(II)(1:I1),F1,F2,F3
 2014 FORMAT ('TEXT  ','Bulk(',I1,')= ',A,3(2X,F9.4),'  0  0  0  0')
      END DO
      F1=BREIT+1.5D0
      F3=0.2D0
      DO II=1,NCOMIN
        F2=HOCH-DBLE(II-1)*0.35D0
        CALL LABLA(COMINS(II),I1)
        IF (I1.NE.0) THEN
        WRITE (bin,2016) COMINS(II)(1:I1),F1,F2,F3
 2016 FORMAT ('TEXT  ',A,3(2X,F9.4),'  0  0  0  0')
      END IF
 !
      END DO
      WRITE (UNIT=XTXT,FMT=2018) TC,P
 2018 FORMAT ('x,T=',F8.2,'[C],P=',F8.1,'[Bar]')
      CALL COLLAPS(XTXT,I1)
!      XTXT='x'
      YTXT='G(rel) [J]'
      WRITE (bin,FMT='(A/A/A)') 'NPLOG2',XTXT(1:I1),YTXT
      WRITE (bin,2020) XXMIN,XXMAX,YYMIN,YYMAX,BREIT,HOCH
 2020 FORMAT(4(1PE20.12),0P,F10.3,F10.3)
!-----
      WRITE (bin,FMT='(''    0'',I5,''    0    0    0    0'')') &
      NPTS+1
      WRITE (bin,2022) (XVAL(I),NEUWERT(I),3-MIN0(1,I),I=0,NPTS)
 2022 FORMAT (7(2(1PE20.12),I2))
!      SIZLAB=0.35D0
!      ANGE=0.0D0
!      CALL LABLA(YVARI,I2)
!--
!      FORMUL=BULKLINE(1)
!      CALL LABLA(FORMUL,I1)
!      WRITE (bin,2025) XVAL(0),YYMAX,SIZLAB,ANGE,I1,FORMUL
!      FORMUL=BULKLINE(2)
!      CALL LABLA(FORMUL,I1)
!!--
!      WRITE (bin,2025) XVAL(NPTS),YYMAX,SIZLAB,ANGE,I1,FORMUL
! 2025 FORMAT (2(1PE20.12),0PF10.7,F10.4,I5,A170)
      CLOSE (UNIT=bin)
!
!----
!
      RETURN
      END
!-----
!********************************
      SUBROUTINE AUTOBIN(NPTS)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!
!     Input for AUTOBIN is e.g.
!
!     JUN92.bs     OLIVINEi  OPX
!     bin++    50
!     0  SI(10)AL(00)FE( 0)MG( 10)O(030)SI(5)FE( 0.000)MG(10.000)O(20)   *
!     0  SI(10)AL(00)FE( 10)MG(0)O(030)SI(5)FE( 10)MG(0.000)O(20)   *
!     VAL   OLIVINEi   FE/2   0   1   15
!     VAL   OPX   FE/2   0   1   15
!     1000   5000
!     800   2000
!     200    4000
!
!
!     JUN92.bs     OLIVINEi  OPX
!     bin++    50
!     0  SI(10)AL(00)FE( 0)MG( 10)O(030)SI(5)FE( 0.000)MG(10.000)O(20)   *
!     0  SI(10)AL(00)FE( 10)MG(0)O(030)SI(5)FE( 10)MG(0.000)O(20)   *
!     RAT   OLIVINEi   FE   MG   0   1   15
!     RAT   OPX   FE   MG   0   1   15
!       900.0000  16000.0000
!
!     VAL  ph  var  (see "check" GETVAL)
!     VAL0 makes Y(0) and Y(max) = 0
!     RAT  ph  el1  el2  (plots el1/(el1+el2) of ph)
!     LNKD  ph1  el1  el2  ph2  el1  el2  (plots el1/el2 of ph1/ph2)
!
!-----END OF COMMON VARIABLES
      INTEGER(4) I,I1,I2,I3,II,COMAY,EINS,ALLES,JX,NPTS,NOWL,DURCH, &
      IVON,IBIS,I4,AT0,IP,IS,IE,IPI(0:1000)
      REAL(8) FF,CHEXX(2,COMAX),YWERT(0:1000),XXMIN,XXMAX,YYMIN,YYMAX, &
      FA,FB,BREIT,HOCH,Y0,Y1,XVAL(0:1000),XFM1,XFM2,YFM1,YFM2, &
      YFM3,YFM4,F1,F2,F3,F4,DDX,FMF1,FMF2,XWERT(0:1000),YPOS, &
      RAT1,RAT2
      CHARACTER(500) SYREC,BULKLINE(2)
      CHARACTER(100) XVARI,YVARI
      CHARACTER(80) DATI,ZEITSTRING,TEXT,TEXT1,TEXT2,OUTFILE
      REAL(8) WERT1,WERT2
      CHARACTER(8) KEY1,KEY2
      CHARACTER(16) VDEF1,VDEF2,XEL1,XEL2,YEL1,YEL2,YEL3,YEL4
      CHARACTER(32) PH4,PH1,PH2,CH32,XCH1,XCH2,YCH1,YCH2,YCH3,YCH4
!
      integer j
!----
      WRITE (UNIT=6,FMT='(''entering AUTOBIN'')')
      PRTLOG(2)=.TRUE.
      COMAY=COMAX
      EINS=1
      NOWL=100
      DURCH=0
      IVON=0
      IBIS=NPTS
      AT0=0
      DO I=1,8
        PRTLOG(I)=.FALSE.
      END DO
      PRTLOG(6)=.TRUE.
!---
!==== read the bulk compositions
      DO I=1,2
       READ (UNIT=IFNR,FMT='(A500)') SYREC
       CALL GELI(SYREC,FF)
       CALL TAXI(SYREC,FORMUL)
       BULKLINE(I)=FORMUL
       CALL LABLA(FORMUL,I2)
       WRITE (UNIT=6,FMT='(170A1)') (FORMUL(I3:I3),I3=1,I2)
       WRITE (UNIT=out,FMT='(170A1)') (FORMUL(I3:I3),I3=1,I2)
       CALL CHEMIE(COMAY,NC,OXYDE,OXANZ,FORMUL,CHE)
       DO II=1,NC
         CHEXX(I,II)=CHE(II)
       END DO
      END DO
      NBUL=3
      BULINE(2)=BULKLINE(1)
      BULINE(3)=BULKLINE(2)
      XXMIN=0.0D0
      XXMAX=1.0D0
      YYMIN=0.0D0
      YYMAX=1.0D0
      BREIT=15.0D0
      HOCH=15.0D0
!-----
      DO II=1,NC
        CHEM(II)=0.0D0
        DO I=1,2
          CHEM(II)=CHEM(II)+CHEXX(I,II)
        END DO
      END DO
      CALL DBREAD
!     WRITE (scr,1000) CHNAME(1),CHEM(CHMCOD(1))
      WRITE (out,1000) CHNAME(1),CHEM(CHMCOD(1))
 1000 FORMAT (' Bulk composition: ',A8,1X,F11.6)
      DO I=2,NUN
!      WRITE (scr,1010) CHNAME(I),CHEM(CHMCOD(I))
       WRITE (out,1010) CHNAME(I),CHEM(CHMCOD(I))
 1010 FORMAT (19X,A8,1X,F11.6)
      END DO
      CALL NURVONPT
      CALL CALSTR
      ALLES=NSOL
      CALL GIBBSTEST(EINS,ALLES)
!
!==== read the variables
      READ (UNIT=IFNR,FMT='(A500)') SYREC
      CALL TAXI(SYREC,KEY1)
      CALL TAXI(SYREC,PH1)
      IF (KEY1.EQ.'VAL') THEN
        CALL TAXI(SYREC,TEXT)
        CALL FUNTAXI(TEXT,VDEF1,FMF1,CH32)
        CALL LABLA(PH1,I1)
        CALL LABLA(CH32,I2)
        XVARI=CH32(1:I2)//' ('//PH1(1:I1)//')'
      END IF
      IF (KEY1.EQ.'RAT') THEN
        CALL TAXI(SYREC,TEXT)
        CALL FUNTAXI(TEXT,XEL1,XFM1,XCH1)
        CALL TAXI(SYREC,TEXT)
        CALL FUNTAXI(TEXT,XEL2,XFM2,XCH2)
        CALL LABLA(PH1,I1)
        CALL LABLA(XCH1,I2)
        CALL LABLA(XCH2,I3)
        XVARI=XCH1(1:I2)//'/('//XCH1(1:I2)// &
        '+'//XCH2(1:I3)//') ('//PH1(1:I1)//')'
      END IF
      CALL GELI(SYREC,XXMIN)
      CALL GELI(SYREC,XXMAX)
      CALL GELI(SYREC,BREIT)
!-
      READ (UNIT=IFNR,FMT='(A500)') SYREC
      CALL TAXI(SYREC,KEY2)
      CALL TAXI(SYREC,PH2)
      IF (KEY2.EQ.'VAL0') THEN
        KEY2='VAL'
        AT0=1
      END IF
      IF (KEY2.EQ.'VAL') THEN
        CALL TAXI(SYREC,TEXT)
        CALL FUNTAXI(TEXT,VDEF2,FMF2,CH32)
        CALL LABLA(PH2,I1)
        CALL LABLA(CH32,I2)
        YVARI=CH32(1:I2)//' ('//PH2(1:I1)//')'
      END IF
      IF (KEY2.EQ.'RAT') THEN
        CALL TAXI(SYREC,TEXT)
        CALL FUNTAXI(TEXT,YEL1,YFM1,YCH1)
        CALL TAXI(SYREC,TEXT)
        CALL FUNTAXI(TEXT,YEL2,YFM2,YCH2)
        CALL LABLA(PH2,I1)
        CALL LABLA(YCH1,I2)
        CALL LABLA(YCH2,I3)
        YVARI=YCH1(1:I2)//'/('//YCH1(1:I2)// &
        '+'//YCH2(1:I3)//') ('//PH2(1:I1)//')'
      END IF

      IF (KEY2.EQ.'LNKD') THEN
        CALL TAXI(SYREC,TEXT)
        CALL FUNTAXI(TEXT,YEL1,YFM1,YCH1)
        CALL TAXI(SYREC,TEXT)
        CALL FUNTAXI(TEXT,YEL2,YFM2,YCH2)
        CALL TAXI(SYREC,PH4)
        CALL TAXI(SYREC,TEXT)
        CALL FUNTAXI(TEXT,YEL3,YFM3,YCH3)
        CALL TAXI(SYREC,TEXT)
        CALL FUNTAXI(TEXT,YEL4,YFM4,YCH4)
        CALL LABLA(PH2,I1)
        CALL LABLA(PH4,I4)
        CALL LABLA(YCH1,I2)
        CALL LABLA(YCH4,I3)
        YVARI='LNKD '//YCH1(1:I2)//'/'//YCH4(1:I3)//' '// &
        PH2(1:I1)//'/'//PH4(1:I4)
      END IF

      CALL GELI(SYREC,YYMIN)
      CALL GELI(SYREC,YYMAX)
      CALL GELI(SYREC,HOCH)
!
!==== read T and P
   10 DURCH=DURCH+1
      READ (UNIT=IFNR,FMT='(A500)',END=999) SYREC
      IF (SYREC.EQ.' ') GOTO 999
      CALL GELI(SYREC,TC)
      WRITE (UNIT=6,FMT='(''TC  = '',F12.5)') TC
      CALL GELI(SYREC,P)
      WRITE (UNIT=6,FMT='(''P   = '',F12.5)') P
!----
      DDX=1D-5
      FF=DBLE(NPTS)
!**** main loop
      DO 500,JX=0,NPTS
      XWERT(JX)=0.0D0
      YWERT(JX)=0.0D0
      FB=DBLE(JX)/FF
      IF (JX.EQ.0) FB=DDX
      IF (JX.EQ.NPTS) FB=1.0D0-1D-5
      FA=1.0D0-FB
      XVAL(JX)=FB
      DO I=1,NUN
        BULK(I)=0.0D0
      END DO
      DO I=1,NUN
        II=CHMCOD(I)
        BULK(I)=FA*CHEXX(1,II)+FB*CHEXX(2,II)
      END DO
      IF (JX.EQ.0) THEN
        DO I=1,NUN
          II=CHMCOD(I)
          BULK(I)=(1.0D0-DDX)*CHEXX(1,II)+DDX*CHEXX(2,II)
        END DO
      END IF
      IF (JX.EQ.NPTS) THEN
        DO I=1,NUN
          II=CHMCOD(I)
          BULK(I)=DDX*CHEXX(1,II)+(1.0D0-DDX)*CHEXX(2,II)
        END DO
      END IF
!----
      CALL NURVONPT
      CALL CALSTR
      CALL PRININ
      CALL TRENNE(130)
      WRITE (UNIT=scr,FMT=1050) JX
      WRITE (UNIT=out,FMT=1050) JX
 1050 FORMAT (/,' THERIAK-loop = ',I6)
      CALL THERIA
!----      
      IF (KEY1.EQ.'VAL') THEN
        CALL GETVAL(PH1,VDEF1,FMF1,IP,IS,IE,WERT1)
        WERT1=WERT1
      END IF

      IF (KEY2.EQ.'VAL') THEN
        CALL GETVAL(PH2,VDEF2,FMF2,IP,IS,IE,WERT2)
        WERT2=WERT2
      END IF

      IF (KEY1.EQ.'RAT') THEN
        CALL GETVAL(PH1,XEL1,XFM1,IP,IS,IE,F1)
        CALL GETVAL(PH1,XEL2,XFM2,IP,IS,IE,F2)
        WERT1=F1/(F1+F2)
      END IF
      IF (KEY2.EQ.'RAT') THEN
        CALL GETVAL(PH2,YEL1,YFM1,IP,IS,IE,F1)
        CALL GETVAL(PH2,YEL2,YFM2,IP,IS,IE,F2)
        WERT2=F1/(F1+F2)
      END IF



      IF (KEY2.EQ.'LNKD') THEN
        CALL GETVAL(PH2,YEL1,YFM1,IP,IS,IE,F1)
        CALL GETVAL(PH2,YEL2,YFM2,IP,IS,IE,F2)
        RAT1=F1/(F2)
        CALL GETVAL(PH4,YEL3,YFM3,IP,IS,IE,F3)
        CALL GETVAL(PH4,YEL4,YFM4,IP,IS,IE,F4)
        RAT2=F3/(F4)
        IF (RAT1/RAT2.LE.0.0D0) THEN
          WERT2=0.0D0
        ELSE
          WERT2=DLOG(RAT1/RAT2)
        END IF
      END IF


      WRITE (UNIT=6,FMT='(/'' x,y = '',2(1PE20.12))') WERT1,WERT2
!----      
      XWERT(JX)=WERT1
      YWERT(JX)=WERT2
  500 CONTINUE
!**** end main loop
!-----
      IF (AT0.EQ.1) THEN
        Y0=YWERT(0)
        Y1=YWERT(NPTS)
        FF=DBLE(NPTS)
        DO I=0,NPTS
          YWERT(I)=YWERT(I)-Y0-(Y1-Y0)*DBLE(I)/FF
        END DO
      END IF
!-----
      DO I=0,NPTS
       IF (XWERT(I).LE.XXMAX.AND.XWERT(I).GE.XXMIN.AND. &
       YWERT(I).LE.YYMAX.AND.YWERT(I).GE.YYMIN) THEN
       IVON=I
       GOTO 50
       END IF
      END DO
   50 CONTINUE
      DO I=NPTS,0,-1
       IF (XWERT(I).LE.XXMAX.AND.XWERT(I).GE.XXMIN.AND. &
       YWERT(I).LE.YYMAX.AND.YWERT(I).GE.YYMIN) THEN
       IBIS=I
       GOTO 52
       END IF
      END DO
   52 CONTINUE
!
!      IVON=0
!      IBIS=NPTS
!
      WRITE (UNIT=scr,FMT='('' '')')
      DO I=0,NPTS
      WRITE (scr,2000) I,XWERT(I),YWERT(I)
      WRITE (out,2000) I,XWERT(I),YWERT(I)
 2000 FORMAT (1X,I4,2X,1PE20.12,2X,1PE20.12)
      END DO
      WRITE (UNIT=scr,FMT='(''limits: '',2I4)') IVON,IBIS
!
!----
      CALL CPUTIME(ZEITSTRING)
      DATI=sdate
      CALL LABLA(DATI,J)
      DATI=DATI(1:J)//'  '//ZEITSTRING
!============================================================
      IF (DURCH.EQ.1) THEN
!------------------
!     open UNIT=bin
!------------------
      IF (SIMFILE) THEN
      I1=INDEX(INFILE,'.txt')
      IF (I1.NE.0) THEN
        OUTFILE=INFILE(1:I1-1)
      ELSE
        OUTFILE=INFILE
      END IF
      CALL LABLA(OUTFILE,I1)
      OPEN (UNIT=bin,FILE=OUTFILE(1:I1)//'_bin_loop',STATUS='UNKNOWN')
      ELSE
      OPEN (UNIT=bin,FILE='bin_loop',STATUS='UNKNOWN')
      END IF
!-----
      IF (XXMAX-XXMIN.LT.1D-8) THEN
      WRITE (UNIT=*,FMT=2010) YYMIN
 2010 FORMAT (/,' X Minimum and maximum are both = ',1PE10.3)
      XXMAX=XXMAX+DABS(XXMAX*0.05D0+1.0D0)
      XXMIN=XXMIN-DABS(XXMIN*0.05D0+1.0D0)
      END IF
      IF (YYMAX-YYMIN.LT.1D-8) THEN
      WRITE (UNIT=*,FMT=2012) YYMIN
 2012 FORMAT (/,' Minimum and maximum are both = ',1PE10.3)
      YYMAX=YYMAX+DABS(YYMAX*0.05D0+1.0D0)
      YYMIN=YYMIN-DABS(YYMIN*0.05D0+1.0D0)
      END IF
!----
      F1=BREIT+1.0D0
      WRITE (bin,2014) F1,F1
 2014 FORMAT ('NULLPT     5   3'/ &
      'FONT      Helvetica'/ &
      'FAT   0.01'/ &
      'PUNKTE   97  0.5   ',F7.4,'  0.25  999  999 '/ &
      'PUNKTE   98  0.5   ',F7.4,'  0.25  999  999 '/ &
      'FAT   0.02')
      F1=BREIT
      F2=-2.0D0
      F3=0.2D0
      WRITE (bin,2016) DATI,F1,F2,F3
 2016 FORMAT ('TEXTB  ',A,/,3(2X,F9.4),'  0  0  0  0')
      F1=0.0D0
      F3=0.2D0
      DO II=2,NBUL
      F2=HOCH+0.8D0+DBLE(II-1)*0.35D0
      CALL LABLA(BULINE(II),I1)
      WRITE (bin,2018) II,BULINE(II)(1:I1),F1,F2,F3
 2018 FORMAT ('TEXT  ','Bulk(',I1,')= ',A,3(2X,F9.4),'  0  0  0  0')
      END DO
      F1=BREIT+1.5D0
      F3=0.2D0
      YPOS=HOCH+F3*1.5D0
      DO II=1,NCOMIN
      IF (II.EQ.NOWL) F3=0.4D0
      F2=YPOS-F3*1.5D0
      YPOS=F2
      CALL LABLA(COMINS(II),I1)
      IF (I1.NE.0) THEN
      WRITE (bin,2020) COMINS(II)(1:I1),F1,F2,F3
 2020 FORMAT ('TEXT  ',A,3(2X,F9.4),'  0  0  0  0')
      END IF
      END DO
      END IF
!
      DO I=0,NPTS
        IPI(I)=2
      END DO
      DO I=0,NPTS
        IF (YWERT(I).GT.YYMAX.OR.YWERT(I).LT.YYMIN) THEN
          IPI(I)=3
          IPI(I+1)=3
        END IF
        IF (XWERT(I).GT.XXMAX.OR.XWERT(I).LT.XXMIN) THEN
          IPI(I)=3
          IPI(I+1)=3
        END IF
      END DO
!
!============================================================
      CALL LABLA(XVARI,I1)
      CALL LABLA(YVARI,I2)
      WRITE (bin,FMT='(A/A/A)') 'NPLOG2',XVARI(1:I1),YVARI(1:I2)
      WRITE (bin,2030) XXMIN,XXMAX,YYMIN,YYMAX,BREIT,HOCH
 2030 FORMAT(4(1PE20.12),0P,F10.3,F10.3)
!-----
      WRITE (bin,FMT='(''    0'',I5,''    0    0    0    0'')') &
      IBIS-IVON+1
      WRITE (bin,2032) (XWERT(I),YWERT(I),IPI(I),I=IVON,IBIS)
 2032 FORMAT (7(2(1PE20.12),I2))
!
      I1=5
      CALL MAKEZAHL(TC,I1,TEXT1,I2)
      CALL MAKEZAHL(P,I1,TEXT2,I3)
      TEXT='T='//TEXT1(1:I2)//' P='//TEXT2(1:I3)
!      WRITE (UNIT=TEXT,FMT=2015) TC,P
! 2015 FORMAT ('T=',F8.2,',P=',F8.1)
      CALL LABLA(TEXT,I1)
      I2=(IBIS+IVON)/2
      WRITE (UNIT=scr,FMT='(''i2: '',2I4)') I2
      WRITE (bin,2034) TEXT(1:I1),XWERT(I2),YWERT(I2)
 2034 FORMAT (//'TEXT     ',A,2X,1PE10.3,2X,1PE10.3, &
       '   0.2   0.5   0   -0.5   0')
!
!----
      GOTO 10
  999 CLOSE (UNIT=bin)
!----
      RETURN
      END
!-----
!********************************
      SUBROUTINE AUTOFUN(NPTS)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!
!     Input for AUTOFUN is e.g.
!
!     JUN92.bs     OLIVINEi  OPX
!     fun++    50
!     0  SI(10)FE(5)MG(5)O(30)   *
!     key    min   max   br
!           key = TC, TK, 1000/T, P
!     VAL   phase   variable   min   max   ho
!or   RAT   phase   el1   el2   min   max   ho
!or   LNKD  phase1   el11  el12   phase2  el21  el22   min  max  ho 
!     const1
!     const2
!     etc.
!
!     const = constant Pressure or Temperature
!     phase is phase or "bulk"
!     variable is element, site occupancy, V, S, Cp etc.
!
!     RAT uses e.g. FE/(FE+MG)
!
!-----END OF COMMON VARIABLES
      INTEGER(4) I,I1,I2,I3,II,COMAY,EINS,ALLES,JX,NPTS,NOWL,DURCH, &
      IVON,IBIS,I4,IP,IS,IE,IPI(0:1000)
      REAL(8) FF,YWERT(0:1000),XXMIN,XXMAX,YYMIN,YYMAX,XCONST, &
      BREIT,HOCH,Y0,Y1,YFM1,YFM2,YFM3,YFM4, &
      F1,F2,F3,F4,DELTX,FMF2,XWERT(0:1000),YPOS,RAT1,RAT2
      CHARACTER(500) SYREC,BULKLINE
      CHARACTER(100) XVARI,YVARI
      CHARACTER(80) DATI,ZEITSTRING,TEXT,TEXT2,OUTFILE
      REAL(8) WERT2
      CHARACTER(8) KEY1,KEY2
      CHARACTER(16) VDEF2,YEL1,YEL2,YEL3,YEL4
      CHARACTER(32) PH4,PH2,CH32,YCH1,YCH2,YCH3,YCH4
!
      integer j
!----
      WRITE (UNIT=6,FMT='(''entering AUTOFUN'')')
      PRTLOG(2)=.TRUE.
      COMAY=COMAX
      EINS=1
      NOWL=100
      DURCH=0
      IVON=0
      IBIS=NPTS
!      DO I=1,8
!        PRTLOG(I)=.FALSE.
!      END DO
!      PRTLOG(6)=.TRUE.
!
!==== read the bulk composition
      READ (UNIT=IFNR,FMT='(A500)') SYREC
      CALL GELI(SYREC,FF)
      CALL TAXI(SYREC,FORMUL)
      BULKLINE=FORMUL
      CALL LABLA(FORMUL,I2)
      WRITE (UNIT=6,FMT='(170A1)') (FORMUL(I3:I3),I3=1,I2)
      WRITE (UNIT=out,FMT='(170A1)') (FORMUL(I3:I3),I3=1,I2)
      CALL CHEMIE(COMAY,NC,OXYDE,OXANZ,FORMUL,CHEM)
      CALL LABLA(USE,LUSE)
      CALL DBREAD
      WRITE (UNIT=6,FMT='(''now dbread'')')
      NBUL=2
      BULINE(2)=BULKLINE
      XXMIN=0.0D0
      XXMAX=1.0D0
      YYMIN=0.0D0
      YYMAX=1.0D0
      BREIT=15.0D0
      HOCH=15.0D0
!-----
      CALL NURVONPT
      CALL CALSTR
      ALLES=NSOL
      CALL GIBBSTEST(EINS,ALLES)
!
!==== read the variables
      READ (UNIT=IFNR,FMT='(A500)') SYREC
      CALL TAXI(SYREC,KEY1)
      CALL GELI(SYREC,XXMIN)
      CALL GELI(SYREC,XXMAX)
      CALL GELI(SYREC,BREIT)
      XVARI=KEY1
      IF (KEY1.EQ.'TC') XVARI='T [C]'
      IF (KEY1.EQ.'TK') XVARI='T [K]'
      IF (KEY1.EQ.'P') XVARI='P [Bar]'
!-
      READ (UNIT=IFNR,FMT='(A500)') SYREC
      CALL TAXI(SYREC,KEY2)
      CALL TAXI(SYREC,PH2)
      IF (KEY2.EQ.'VAL') THEN
        CALL TAXI(SYREC,TEXT)
        CALL FUNTAXI(TEXT,VDEF2,FMF2,CH32)
! here catch V/V0
        CALL LABLA(PH2,I1)
        CALL LABLA(CH32,I2)
        YVARI=CH32(1:I2)//' ('//PH2(1:I1)//')'
      END IF
      IF (KEY2.EQ.'RAT') THEN
        CALL TAXI(SYREC,TEXT)
        CALL FUNTAXI(TEXT,YEL1,YFM1,YCH1)
        CALL TAXI(SYREC,TEXT)
        CALL FUNTAXI(TEXT,YEL2,YFM2,YCH2)
        CALL LABLA(PH2,I1)
        CALL LABLA(YCH1,I2)
        CALL LABLA(YCH2,I3)
        YVARI=YCH1(1:I2)//'/('//YCH1(1:I2)// &
        '+'//YCH2(1:I3)//') ('//PH2(1:I1)//')'
      END IF
      IF (KEY2.EQ.'LNKD') THEN
        CALL TAXI(SYREC,TEXT)
        CALL FUNTAXI(TEXT,YEL1,YFM1,YCH1)
        CALL TAXI(SYREC,TEXT)
        CALL FUNTAXI(TEXT,YEL2,YFM2,YCH2)
        CALL TAXI(SYREC,PH4)
        CALL TAXI(SYREC,TEXT)
        CALL FUNTAXI(TEXT,YEL3,YFM3,YCH3)
        CALL TAXI(SYREC,TEXT)
        CALL FUNTAXI(TEXT,YEL4,YFM4,YCH4)
        CALL LABLA(PH2,I1)
        CALL LABLA(PH4,I4)
        CALL LABLA(YCH1,I2)
        CALL LABLA(YCH4,I3)
        YVARI='LNKD '//YCH1(1:I2)//'/'//YCH4(1:I3)//' '// &
        PH2(1:I1)//'/'//PH4(1:I4)
      END IF
      CALL GELI(SYREC,YYMIN)
      CALL GELI(SYREC,YYMAX)
      CALL GELI(SYREC,HOCH)
!
!==== read XCONST
   10 DURCH=DURCH+1
      READ (UNIT=IFNR,FMT='(A500)',END=999) SYREC
      IF (SYREC.EQ.' ') GOTO 999
      CALL GELI(SYREC,XCONST)
      WRITE (UNIT=6,FMT='(''XCONST  = '',F12.5)') XCONST
!----
      FF=DBLE(NPTS)
      DELTX=(XXMAX-XXMIN)/FF
!**** main loop
      DO 500,JX=0,NPTS
      YWERT(JX)=0.0D0
      XWERT(JX)=XXMIN+DBLE(JX)*DELTX
      IF (KEY1.EQ.'TC') THEN
       TC=XWERT(JX)
       P=XCONST
      END IF
      IF (KEY1.EQ.'TK') THEN
       TC=XWERT(JX)-273.15D0
       P=XCONST
      END IF
      IF (KEY1.EQ.'1000/T') THEN
       T=1000.0D0/XWERT(JX)
       TC=T-273.15D0
       P=XCONST
      END IF
      IF (KEY1.EQ.'P') THEN
       P=XWERT(JX)
       TC=XCONST
      END IF
!----
      CALL NURVONPT
      CALL CALSTR
      CALL PRININ
      CALL TRENNE(130)
      WRITE (UNIT=scr,FMT=1000) JX
      WRITE (UNIT=out,FMT=1000) JX
 1000 FORMAT (/,' THERIAK-loop = ',I6)
      CALL THERIA
!----      
      IF (KEY2.EQ.'VAL') CALL GETVAL(PH2,VDEF2,FMF2,IP,IS,IE,WERT2)
      IF (KEY2.EQ.'RAT') THEN
        CALL GETVAL(PH2,YEL1,YFM1,IP,IS,IE,F1)
        CALL GETVAL(PH2,YEL2,YFM2,IP,IS,IE,F2)
        WERT2=F1/(F1+F2)
      END IF
      IF (KEY2.EQ.'LNKD') THEN
        CALL GETVAL(PH2,YEL1,YFM1,IP,IS,IE,F1)
        CALL GETVAL(PH2,YEL2,YFM2,IP,IS,IE,F2)
        RAT1=F1/(F2)
        CALL GETVAL(PH4,YEL3,YFM3,IP,IS,IE,F3)
        CALL GETVAL(PH4,YEL4,YFM4,IP,IS,IE,F4)
        RAT2=F3/(F4)
        IF (RAT1/RAT2.LE.0.0D0) THEN
          WERT2=0.0D0
        ELSE
          WERT2=DLOG(RAT1/RAT2)
        END IF
      END IF
      WRITE (UNIT=6,FMT='(/'' x,y = '',2(1PE20.12))') XWERT(JX),WERT2
!----      
      YWERT(JX)=WERT2
  500 CONTINUE
!-----
      Y0=YWERT(0)
      Y1=YWERT(NPTS)
!-----
      DO I=0,NPTS
       IF (XWERT(I).LE.XXMAX.AND.XWERT(I).GE.XXMIN.AND. &
       YWERT(I).LE.YYMAX.AND.YWERT(I).GE.YYMIN) THEN
       IVON=I
       GOTO 50
       END IF
      END DO
   50 CONTINUE
      DO I=NPTS,0,-1
       IF (XWERT(I).LE.XXMAX.AND.XWERT(I).GE.XXMIN.AND. &
       YWERT(I).LE.YYMAX.AND.YWERT(I).GE.YYMIN) THEN
       IBIS=I
       GOTO 52
       END IF
      END DO
   52 CONTINUE
      WRITE (UNIT=scr,FMT='('' '')')
      DO I=0,NPTS
      WRITE (scr,1005) I,XWERT(I),YWERT(I)
      WRITE (out,1005) I,XWERT(I),YWERT(I)
 1005 FORMAT (1X,I4,2X,1PE20.12,2X,1PE20.12)
      END DO
      WRITE (UNIT=scr,FMT='(''limits: '',2I4)') IVON,IBIS
!
!----
      CALL CPUTIME(ZEITSTRING)
      DATI=sdate
      CALL LABLA(DATI,J)
      DATI=DATI(1:J)//'  '//ZEITSTRING
!============================================================
      IF (DURCH.EQ.1) THEN
!------------------
!     open UNIT=bin
!------------------
      IF (SIMFILE) THEN
      I1=INDEX(INFILE,'.txt')
      IF (I1.NE.0) THEN
        OUTFILE=INFILE(1:I1-1)
      ELSE
        OUTFILE=INFILE
      END IF
      CALL LABLA(OUTFILE,I1)
      OPEN (UNIT=bin,FILE=OUTFILE(1:I1)//'_fun_loop',STATUS='UNKNOWN')
      ELSE
      OPEN (UNIT=bin,FILE='fun_loop',STATUS='UNKNOWN')
      END IF
!-----
      IF (XXMAX-XXMIN.LT.1D-8) THEN
      WRITE (UNIT=*,FMT=2000) YYMIN
 2000 FORMAT (/,' X Minimum and maximum are both = ',1PE10.3)
      XXMAX=XXMAX+DABS(XXMAX*0.05D0+1.0D0)
      XXMIN=XXMIN-DABS(XXMIN*0.05D0+1.0D0)
      END IF
      IF (YYMAX-YYMIN.LT.1D-8) THEN
      WRITE (UNIT=*,FMT=2002) YYMIN
 2002 FORMAT (/,' Minimum and maximum are both = ',1PE10.3)
      YYMAX=YYMAX+DABS(YYMAX*0.05D0+1.0D0)
      YYMIN=YYMIN-DABS(YYMIN*0.05D0+1.0D0)
      END IF
!----
      F1=BREIT+1.0D0
      WRITE (bin,2010) F1,F1
 2010 FORMAT ('NULLPT     5   3'/ &
      'FONT      Helvetica'/ &
      'FAT   0.01'/ &
      'PUNKTE   97  0.5   ',F7.4,'  0.25  999  999 '/ &
      'PUNKTE   98  0.5   ',F7.4,'  0.25  999  999 '/ &
      'FAT   0.02')
      F1=BREIT
      F2=-2.0D0
      F3=0.2D0
      WRITE (bin,2012) DATI,F1,F2,F3
 2012 FORMAT ('TEXTB  ',A,/,3(2X,F9.4),'  0  0  0  0')
      F1=0.0D0
      F3=0.2D0
      DO II=2,NBUL
      F2=HOCH+0.8D0+DBLE(II-1)*0.35D0
      CALL LABLA(BULINE(II),I1)
      WRITE (bin,2014) II,BULINE(II)(1:I1),F1,F2,F3
 2014 FORMAT ('TEXT  ','Bulk(',I1,')= ',A,3(2X,F9.4),'  0  0  0  0')
      END DO
      F1=BREIT+1.5D0
      F3=0.2D0
      YPOS=HOCH+F3*1.5D0
      DO II=1,NCOMIN
      IF (II.EQ.NOWL) F3=0.4D0
      F2=YPOS-F3*1.5D0
      YPOS=F2
      CALL LABLA(COMINS(II),I1)
      IF (I1.NE.0) THEN
      WRITE (bin,2016) COMINS(II)(1:I1),F1,F2,F3
 2016 FORMAT ('TEXT  ',A,3(2X,F12.4),'  0  0  0  0')
      END IF
      END DO
      END IF
!
!!      WRITE (bin,2015) BREIT,HOCH,XXMIN,XXMAX,YYMIN,YYMAX
!! 2015 FORMAT ('ACHSEN   0   ',6(2X,1PE20.12)/'CLIP  ')
!
      DO I=0,NPTS
        IPI(I)=2
      END DO
      DO I=0,NPTS
        IF (YWERT(I).GT.YYMAX.OR.YWERT(I).LT.YYMIN) THEN
          IPI(I)=3
          IPI(I+1)=3
        END IF
      END DO
!
!============================================================
      CALL LABLA(XVARI,I1)
      CALL LABLA(YVARI,I2)
      WRITE (bin,FMT='(A/A/A)') 'NPLOG2',XVARI(1:I1),YVARI(1:I2)
      WRITE (bin,2020) XXMIN,XXMAX,YYMIN,YYMAX,BREIT,HOCH
 2020 FORMAT(4(1PE20.12),0P,F10.3,F10.3)
!-----
      WRITE (bin,FMT='(''    0'',I5,''    0    0    0    0'')') &
      IBIS-IVON+1
      WRITE (bin,2022) (XWERT(I),YWERT(I),IPI(I),I=IVON,IBIS)
 2022 FORMAT (7(2(1PE20.12),I2))
!
      IF (KEY1.EQ.'P') THEN
      I1=6
      CALL MAKEZAHL(TC,I1,TEXT2,I2)
      TEXT='T='//TEXT2(1:I2)
!      WRITE (UNIT=TEXT,FMT=2024) TC
! 2024 FORMAT ('T=',F8.2)
      ELSE
      I1=6
      CALL MAKEZAHL(P,I1,TEXT2,I2)
      TEXT='P='//TEXT2(1:I2)
!      WRITE (UNIT=TEXT,FMT=2026) P
! 2026 FORMAT ('P=',F8.2)
      END IF
      CALL COLLAPS(TEXT,I1)
      I2=(IBIS+IVON)/2
!      WRITE (UNIT=scr,FMT='(''i2: '',2I4)') I2
      WRITE (bin,2030) TEXT(1:I1),XWERT(I2),YWERT(I2)
 2030 FORMAT (//'TEXT     ',A,2X,F12.3,2X,F12.3, &
       '   0.2   0.5   0   -0.5   0')
!
!----
      GOTO 10
  999 CLOSE (UNIT=bin)
!----
      RETURN
      END
!-----
!********************************
      SUBROUTINE AUTOREA(NPTS)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!
!     Input for AUTOREA is e.g.
!
!     JUN92.bs     OLIVINEi  OPX
!     rea++    50
!     0  SI(10)FE(5)MG(5)O(30)   *
!     key    min   max   br
!           key = TC, TK, 1000/T, P
!     LNKR  coeff1  pha1  coeff2  pha2  etc.   min  max  ho 
!     const1
!     const2
!     etc.
!
!     const = constant Pressure or Temperature
!     variable is  V, S, Cp etc.
!
!
!-----END OF COMMON VARIABLES
      INTEGER(4) I,I1,I2,I3,II,COMAY,EINS,ALLES,JX,NPTS,NOWL,DURCH, &
      IVON,IBIS,NCOEFF,IP,IS,IE,IPI(0:1000)
      REAL(8) FF,YWERT(0:1000),XXMIN,XXMAX,YYMIN,YYMAX,XCONST, &
      BREIT,HOCH,Y0,Y1,YFM1, &
      F1,F2,F3,DELTX,XWERT(0:1000),YPOS
      CHARACTER(500) SYREC,BULKLINE
      CHARACTER(100) XVARI,YVARI
      CHARACTER(80) DATI,ZEITSTRING,TEXT,TEXT2,OUTFILE
      REAL(8) WERT2,COEFF(10),GOFR
      CHARACTER(8) KEY1,KEY2
      CHARACTER(16) YEL1
      CHARACTER(32) RPHA(10)
!
      integer j
!----
      WRITE (UNIT=6,FMT='(''entering AUTOREA'')')
      PRTLOG(2)=.TRUE.
      COMAY=COMAX
      EINS=1
      NOWL=100
      DURCH=0
      IVON=0
      IBIS=NPTS
!      DO I=1,8
!        PRTLOG(I)=.FALSE.
!      END DO
!      PRTLOG(6)=.TRUE.
!
!==== read the bulk composition
      READ (UNIT=IFNR,FMT='(A500)') SYREC
      CALL GELI(SYREC,FF)
      CALL TAXI(SYREC,FORMUL)
      BULKLINE=FORMUL
      CALL LABLA(FORMUL,I2)
      WRITE (UNIT=6,FMT='(170A1)') (FORMUL(I3:I3),I3=1,I2)
      WRITE (UNIT=out,FMT='(170A1)') (FORMUL(I3:I3),I3=1,I2)
      CALL CHEMIE(COMAY,NC,OXYDE,OXANZ,FORMUL,CHEM)
      CALL LABLA(USE,LUSE)
      CALL DBREAD
      WRITE (UNIT=6,FMT='(''now dbread'')')
      NBUL=2
      BULINE(2)=BULKLINE
      XXMIN=0.0D0
      XXMAX=1.0D0
      YYMIN=0.0D0
      YYMAX=1.0D0
      BREIT=15.0D0
      HOCH=15.0D0
!-----
      CALL NURVONPT
      CALL CALSTR
      ALLES=NSOL
      CALL GIBBSTEST(EINS,ALLES)
!
!==== read the variables
      READ (UNIT=IFNR,FMT='(A500)') SYREC
      CALL TAXI(SYREC,KEY1)
      CALL GELI(SYREC,XXMIN)
      CALL GELI(SYREC,XXMAX)
      CALL GELI(SYREC,BREIT)
      XVARI=KEY1
      IF (KEY1.EQ.'TC') XVARI='T [C]'
      IF (KEY1.EQ.'TK') XVARI='T [K]'
      IF (KEY1.EQ.'P') XVARI='P [Bar]'
!-
      READ (UNIT=IFNR,FMT='(A500)') SYREC
      CALL TAXI(SYREC,KEY2)
      CALL GELI(SYREC,YYMIN)
      CALL GELI(SYREC,YYMAX)
      CALL GELI(SYREC,HOCH)
!-
      READ (UNIT=IFNR,FMT='(A500)') SYREC
      CALL PUST(6,SYREC)
      NCOEFF=0
    4 CALL GELI(SYREC,FF)
      IF (DABS(FF).LT.1D-20) GOTO 5
      NCOEFF=NCOEFF+1
      COEFF(NCOEFF)=FF
      CALL TAXI(SYREC,RPHA(NCOEFF))
      GOTO 4
    5 CONTINUE

      YVARI='lnK (reaction)'
      NCOMIN=NCOMIN+1
      COMINS(NCOMIN)='reaction:'
      DO I=1,NCOEFF
       NCOMIN=NCOMIN+1
       WRITE (UNIT=COMINS(NCOMIN),FMT='(F7.2,1X,A)') COEFF(I),RPHA(I)
      END DO


      YEL1='G'
      YFM1=1.0D0
!
!==== read XCONST
   10 DURCH=DURCH+1
      READ (UNIT=IFNR,FMT='(A500)',END=999) SYREC
      IF (SYREC.EQ.' ') GOTO 999
      CALL GELI(SYREC,XCONST)
      WRITE (UNIT=6,FMT='(''XCONST  = '',F12.5)') XCONST
!----
      FF=DBLE(NPTS)
      DELTX=(XXMAX-XXMIN)/FF
!**** main loop
      DO 500,JX=0,NPTS
      YWERT(JX)=0.0D0
      XWERT(JX)=XXMIN+DBLE(JX)*DELTX
      IF (KEY1.EQ.'TC') THEN
       TC=XWERT(JX)
       P=XCONST
      END IF
      IF (KEY1.EQ.'TK') THEN
       TC=XWERT(JX)-273.15D0
       P=XCONST
      END IF
      IF (KEY1.EQ.'1000/T') THEN
       T=1000.0D0/XWERT(JX)
       TC=T-273.15D0
       P=XCONST
      END IF
      IF (KEY1.EQ.'P') THEN
       P=XWERT(JX)
       TC=XCONST
      END IF
!----
      CALL NURVONPT
      CALL CALSTR
      CALL PRININ
      CALL TRENNE(130)
      WRITE (UNIT=scr,FMT=1000) JX
      WRITE (UNIT=out,FMT=1000) JX
 1000 FORMAT (/,' THERIAK-loop = ',I6)
      CALL THERIA
!----      
      IF (KEY2.EQ.'LNKR') THEN
      GOFR=0.0D0
      DO I=1,NCOEFF
        CALL GETVAL(RPHA(I),YEL1,YFM1,IP,IS,IE,F1)
        GOFR=GOFR+COEFF(I)*F1


      END DO
      WERT2=-GOFR/R/T
!      WERT2=GOFR
      END IF
      WRITE (UNIT=6,FMT='(/'' x,y = '',2(1PE20.12))') XWERT(JX),WERT2
!----      
      YWERT(JX)=WERT2
  500 CONTINUE
!-----
      Y0=YWERT(0)
      Y1=YWERT(NPTS)
      IVON=0
      IBIS=NPTS
!-----
      WRITE (UNIT=scr,FMT='('' '')')
      DO I=0,NPTS
      WRITE (scr,1010) I,XWERT(I),YWERT(I)
      WRITE (out,1010) I,XWERT(I),YWERT(I)
 1010 FORMAT (1X,I4,2X,1PE20.12,2X,1PE20.12)
      END DO
      WRITE (UNIT=scr,FMT='(''limits: '',2I4)') IVON,IBIS
!
!----
      CALL CPUTIME(ZEITSTRING)
      DATI=sdate
      CALL LABLA(DATI,J)
      DATI=DATI(1:J)//'  '//ZEITSTRING
!============================================================
      IF (DURCH.EQ.1) THEN
!------------------
!     open UNIT=bin
!------------------
      IF (SIMFILE) THEN
      I1=INDEX(INFILE,'.txt')
      IF (I1.NE.0) THEN
        OUTFILE=INFILE(1:I1-1)
      ELSE
        OUTFILE=INFILE
      END IF
      CALL LABLA(OUTFILE,I1)
      OPEN (UNIT=bin,FILE=OUTFILE(1:I1)//'_rea_loop',STATUS='UNKNOWN')
      ELSE
      OPEN (UNIT=bin,FILE='rea_loop',STATUS='UNKNOWN')
      END IF
!-----
      IF (XXMAX-XXMIN.LT.1D-8) THEN
      WRITE (UNIT=*,FMT=2000) YYMIN
 2000 FORMAT (/,' X Minimum and maximum are both = ',1PE10.3)
      XXMAX=XXMAX+DABS(XXMAX*0.05D0+1.0D0)
      XXMIN=XXMIN-DABS(XXMIN*0.05D0+1.0D0)
      END IF
      IF (YYMAX-YYMIN.LT.1D-8) THEN
      WRITE (UNIT=*,FMT=2002) YYMIN
 2002 FORMAT (/,' Minimum and maximum are both = ',1PE10.3)
      YYMAX=YYMAX+DABS(YYMAX*0.05D0+1.0D0)
      YYMIN=YYMIN-DABS(YYMIN*0.05D0+1.0D0)
      END IF
!----
      F1=BREIT+1.0D0
      WRITE (bin,2004) F1,F1
 2004 FORMAT ('NULLPT     5   3'/ &
      'FONT      Helvetica'/ &
      'FAT   0.01'/ &
      'PUNKTE   97  0.5   ',F7.4,'  0.25  999  999 '/ &
      'PUNKTE   98  0.5   ',F7.4,'  0.25  999  999 '/ &
      'FAT   0.02')
      F1=BREIT
      F2=-2.0D0
      F3=0.2D0
      WRITE (bin,2006) DATI,F1,F2,F3
 2006 FORMAT ('TEXTB  ',A,/,3(2X,F9.4),'  0  0  0  0')
      F1=0.0D0
      F3=0.2D0
      DO II=2,NBUL
      F2=HOCH+0.8D0+DBLE(II-1)*0.35D0
      CALL LABLA(BULINE(II),I1)
      WRITE (bin,2008) II,BULINE(II)(1:I1),F1,F2,F3
 2008 FORMAT ('TEXT  ','Bulk(',I1,')= ',A,3(2X,F9.4),'  0  0  0  0')
      END DO
      F1=BREIT+1.5D0
      F3=0.2D0
      YPOS=HOCH+F3*1.5D0
      DO II=1,NCOMIN
       IF (II.EQ.NOWL) F3=0.4D0
       F2=YPOS-F3*1.5D0
       YPOS=F2
       CALL LABLA(COMINS(II),I1)
       IF (I1.NE.0) THEN
        WRITE (bin,2010) COMINS(II)(1:I1),F1,F2,F3
 2010   FORMAT ('TEXT  ',A,3(2X,F9.4),'  0  0  0  0')
       END IF
      END DO
! end of durch=1
      END IF
!
      DO I=0,NPTS
        IPI(I)=2
      END DO
      DO I=0,NPTS
        IF (YWERT(I).GT.YYMAX.OR.YWERT(I).LT.YYMIN) THEN
          IPI(I)=3
          IPI(I+1)=3
        END IF
      END DO
!
!============================================================
      CALL LABLA(XVARI,I1)
      CALL LABLA(YVARI,I2)
      WRITE (bin,FMT='(A/A/A)') 'NPLOG2',XVARI(1:I1),YVARI(1:I2)
      WRITE (bin,2020) XXMIN,XXMAX,YYMIN,YYMAX,BREIT,HOCH
 2020 FORMAT(4(1PE20.12),0P,F10.3,F10.3)
!-----
      WRITE (bin,FMT='(''    0'',I5,''    0    0    0    0'')') &
      IBIS-IVON+1
      WRITE (bin,2022) (XWERT(I),YWERT(I),IPI(I),I=IVON,IBIS)
 2022 FORMAT (7(2(1PE20.12),I2))
!
      IF (KEY1.EQ.'P') THEN
      I1=5
      CALL MAKEZAHL(TC,I1,TEXT2,I2)
      TEXT='T='//TEXT2(1:I2)
!      WRITE (UNIT=TEXT,FMT=2024) TC
! 2024 FORMAT ('T=',F8.2)
      ELSE
      I1=5
      CALL MAKEZAHL(P,I1,TEXT2,I2)
      TEXT='P='//TEXT2(1:I2)
!      WRITE (UNIT=TEXT,FMT=2026) P
! 2026 FORMAT ('P=',F8.2)
      END IF
      CALL COLLAPS(TEXT,I1)
      I2=(IBIS+IVON)/2
!      WRITE (UNIT=scr,FMT='(''i2: '',2I4)') I2
      WRITE (bin,2030) TEXT(1:I1),XWERT(I2),YWERT(I2)
 2030 FORMAT (//'TEXT     ',A,2X,F12.3,2X,F12.3, &
       '   0.2   0.5   0   -0.5   0')
!
!----
      GOTO 10
  999 CLOSE (UNIT=bin)
!----
      RETURN
      END
!-----
!********************************
      SUBROUTINE ADDTOBUL(TOADD)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
!-----END OF COMMON VARIABLES
      REAL(8) FF,TOADD(COMAX),FAK
      INTEGER(4) I,IC,I0
      LOGICAL(4) MORE
!*****
      FAK=1.0D0
      I0=0
      DO IC=1,NC
        IF (TOADD(IC).LT.0.0D0) THEN
          FF=-CHEM(IC)/TOADD(IC)
          IF (FF.LT.FAK) THEN
            FAK=FF
            I0=IC
          END IF
        END IF
      END DO
      IF (FAK.LE.0.0D0) RETURN
      DO IC=1,NC
        CHE(IC)=CHEM(IC)+TOADD(IC)*FAK
      END DO
      IF (I0.NE.0) CHE(I0)=0.0D0
      MORE=.FALSE.
      DO I=1,NC
        IF (DABS(CHE(I)).LT.1D-20.NEQV.DABS(CHEM(I)).LT.1D-20) MORE=.TRUE.
        CHEM(I)=CHE(I)
      END DO
      IF (MORE) THEN
      CALL DBREAD
      ELSE
      DO I=1,NUN
        BULK(I)=CHE(CHMCOD(I))
      END DO
      END IF
!-----
      RETURN
      END
!-----
!********************************
      SUBROUTINE REMOPH(TOADD)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
!-----END OF COMMON VARIABLES
      REAL(8) TOADD(COMAX)
      INTEGER(4) I,IC
      LOGICAL(4) MORE
!*****
      DO IC=1,NC
        CHE(IC)=CHEM(IC)+TOADD(IC)
!Nov2013 1d-14 because when almost zero minimization may fail
        IF (CHE(IC).LT.1.0D-14) CHE(IC)=0.0D0
      END DO
      MORE=.FALSE.
      DO I=1,NC
        IF (DABS(CHE(I)).LT.1D-20.NEQV.DABS(CHEM(I)).LT.1D-20) MORE=.TRUE.
        CHEM(I)=CHE(I)
      END DO
      IF (MORE) THEN
      CALL DBREAD
      ELSE
      DO I=1,NUN
        BULK(I)=CHE(CHMCOD(I))
      END DO
      END IF
!-----
      RETURN
      END
!-----
!********************************
      SUBROUTINE MINILOOP(CH001)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!
!-----END OF COMMON VARIABLES
      INTEGER(4) NPTS,ierr,j,II
      REAL(8) FF,TPSTEP,T1,T2,P1,P2,DET,DEP,TOADD(COMAX),REFRA(50), &
      INFLOW(COMAX),OUTFLOW(COMAX),F1,NNREM(50),X16
      INTEGER(4) I,IC,IR,IP,I001,COMAY,NREP,KOUNT,SUBPH(50),TVON,TBIS
      CHARACTER(500) CH001,CH002,SYREC
      CHARACTER(32) REPHASE(50),CH16,TEXT
      CHARACTER(8) LOOPCODE
      CHARACTER(80) DATI,ZEITSTRING
      LOGICAL(4) SUBD
!----
!     DO I=1,10
!       PRTLOG(I)=.FALSE.
!     END DO
      PRTLOG(9)=.TRUE.
      PRTLOG(2)=.TRUE.
      PRTLOG(6)=.TRUE.
      DO I=1,50
       NNREM(I)=0.0D0
      END DO
      CH16=' '
      NVARTBL=0
      NROWTBL=0
      NREP=0
      KOUNT=0
      TVON=0
      TBIS=0
      COMAY=COMAX
      SUBD=.FALSE.
      CALL LABLA(CH001,I001)
      IF (NCOMIN.LT.50) THEN
      NCOMIN=NCOMIN+1
      COMINS(NCOMIN)='drv-file: '//CH001(1:I001)
      END IF
!------------------
!     open UNIT=drv
!------------------
      j=drv
      line=CH001
      path=wpath
      akzess=' '
      state='old'
      call openfile(j,ierr)
      if(ierr.ne.0) STOP
!-----
   10 CONTINUE
      READ (UNIT=drv,FMT='(A500)',END=888) SYREC
      IF (NCOMIN.LT.50) THEN
      NCOMIN=NCOMIN+1
      COMINS(NCOMIN)=SYREC(1:80)
      END IF
      CH002='special input: '//SYREC(1:485)
      CALL PUST(scr,CH002)
!      CALL PUST(out,CH002)
      LOOPCODE='BLAH'
      IF (.NOT.SUBD) CALL TAXI(SYREC,LOOPCODE)
!*****
      IF (LOOPCODE(1:1).EQ.'!') GOTO 10
      IF (VERGL(LOOPCODE,'END')) GOTO 888
!*****
!*****
      IF (VERGL(LOOPCODE,'SUB')) THEN
      CALL TAXI(SYREC,CH001)
      CALL TAXI(SYREC,CH002)
!------------------------
!     open UNIT=41 and 42
!------------------------
      CALL LABLA(CH001,I001)
      OPEN (UNIT=41,FILE=CH001(1:I001))
      CALL LABLA(CH002,I001)
      OPEN (UNIT=42,FILE=CH002(1:I001))
!-----
      SUBD=.TRUE.
      GOTO 10
      END IF
!*****
!***** (subduction, not updated)
      IF (SUBD) THEN
      DO I=1,NC
        INFLOW(I)=0.0D0
        OUTFLOW(I)=0.0D0
      END DO
      CALL GELI(SYREC,TC)
      CALL GELI(SYREC,P)
      READ (UNIT=41,FMT='(100(2X,F12.6))',END=111) (INFLOW(I),I=1,NC)
  111 CONTINUE
      CALL ADDTOBUL(INFLOW)
      CALL NURVONPT
      CALL CALSTR
      IF (PRTLOG(2).OR.PRTLOG(3).OR.PRTLOG(4)) CALL PRININ
      KOUNT=KOUNT+1
      CALL TRENNE(130)
      WRITE (UNIT=scr,FMT=1000) KOUNT
      WRITE (UNIT=out,FMT=1000) KOUNT
 1000 FORMAT (/,' THERIAK-loop = ',I6)
      CALL THERIA
      CALL PRTTBL
!+++++
      IF (NREP.GT.0) THEN
      DO 500,IP=1,NUN2
      IF (NUMMER(IP).LE.0) THEN
      TEXT=SOLNAM(EMCODE(IP))
      ELSE
      TEXT=NAME(NUMMER(IP))
      END IF
      DO 550,IR=1,NREP
      IF (REPHASE(IR).EQ.TEXT) THEN
!---- phase to remove
      DO IC=1,NC
        TOADD(IC)=0.0D0
      END DO
      DO IC=1,NUN
        TOADD(CHMCOD(IC))= &
        TOADD(CHMCOD(IC))-X(IP,IC)*NN(IP)*REFRA(IR)
      END DO
      CALL REMOPH(TOADD)
      IF (SUBPH(IR).EQ.1) THEN
        DO I=1,NC
          OUTFLOW(I)=OUTFLOW(I)-TOADD(I)
        END DO
      END IF
      END IF
!---- end phase to remove
  550 CONTINUE
  500 CONTINUE
      END IF
!---
      WRITE (UNIT=42,FMT='(100(2X,F12.6))') (OUTFLOW(I),I=1,NC)
      GOTO 10
      END IF
!*****
!*****
      IF (VERGL(LOOPCODE,'TEST')) THEN
      CALL GELI(SYREC,FF)
      TVON=IDINT(FF)
      CALL GELI(SYREC,FF)
      TBIS=IDINT(FF)
      GOTO 10
      END IF
!*****
      IF (VERGL(LOOPCODE,'COMP')) THEN
      CALL TAXI(SYREC,FORMUL)
      CALL TAXI(SYREC,USE)
      CALL CHEMIE(COMAY,NC,OXYDE,OXANZ,FORMUL,CHEM)
      CALL LABLA(USE,LUSE)
      CALL DBREAD
!     IF (PRTLOG(1)) STOP
      GOTO 10
      END IF
!*****
      IF (VERGL(LOOPCODE,'ADD')) THEN
      CALL TAXI(SYREC,FORMUL)
      CALL CHEMIE(COMAY,NC,OXYDE,OXANZ,FORMUL,CHE)
      DO IC=1,NC
        TOADD(IC)=CHE(IC)
      END DO
      CALL ADDTOBUL(TOADD)
      GOTO 10
      END IF
!*****
      IF (VERGL(LOOPCODE,'REMOVE')) THEN
      CALL TAXI(SYREC,CH16)
      CALL GELI(SYREC,FF)
      CALL GELI(SYREC,F1)
      IR=0
      DO I=1,NREP
        IF (CH16.EQ.REPHASE(I)) THEN
          REFRA(I)=FF/100.0D0
          IR=I
          SUBPH(IR)=IDINT(F1)
          GOTO 30
        END IF
      END DO
   30 CONTINUE
      IF (IR.EQ.0) THEN
      NREP=NREP+1
      IF (NREP.GT.50) THEN
      WRITE (UNIT=*,FMT='('' NREP too big'')')
      STOP
      END IF
      REPHASE(NREP)=CH16
      REFRA(NREP)=FF/100.0D0
      SUBPH(NREP)=IDINT(F1)
      END IF
      GOTO 10
      END IF
!*****
      IF (VERGL(LOOPCODE,'TP').OR.VERGL(LOOPCODE,'REF')) THEN
      T1=TC
      P1=P
      CALL GELI(SYREC,T2)
      CALL GELI(SYREC,P2)
      CALL GELI(SYREC,TPSTEP)
      IF (TPSTEP.LT.1.0D0) TPSTEP=1.0D0
      NPTS=IDINT(TPSTEP)
      DET=(T2-T1)/TPSTEP
      DEP=(P2-P1)/TPSTEP
      TC=T1
      P=P1
!-----
      DO 600,I=1,NPTS
      TC=TC+DET
      P=P+DEP
      CALL NURVONPT
      CALL CALSTR
      IF (PRTLOG(2).OR.PRTLOG(3).OR.PRTLOG(4)) CALL PRININ
      KOUNT=KOUNT+1
      IF (KOUNT.EQ.TVON) TEST=-DABS(TEST)
      IF (KOUNT.EQ.TBIS) TEST=DABS(TEST)
      CALL TRENNE(130)
      WRITE (UNIT=scr,FMT=2000) KOUNT
      WRITE (UNIT=out,FMT=2000) KOUNT
 2000 FORMAT (/,' THERIAK-loop = ',I6)
      CALL THERIA
      IF (REFAS) CALL SHOWREF
!+++++
      IF (NREP.GT.0) THEN
       DO IP=1,NUN2
        IF (NUMMER(IP).LE.0) THEN
         TEXT=SOLNAM(EMCODE(IP))
        ELSE
         TEXT=NAME(NUMMER(IP))
        END IF
        DO IR=1,NREP
         IF (REPHASE(IR).EQ.TEXT) THEN
          NNREM(IR)=NNREM(IR)+NN(IP)*(REFRA(IR))
         END IF
          CH16='nsum_'//REPHASE(IR)(1:27)
          X16=NNREM(IR)
          CALL SETTBL(CH16,X16)
        END DO
       END DO
      END IF
!+++++
      CALL PRTTBL
!+++++
      IF (VERGL(LOOPCODE,'REF')) CALL MACHREF
!+++++
      IF (NREP.GT.0) THEN
      DO 650,IP=1,NUN2
      IF (NUMMER(IP).LE.0) THEN
      TEXT=SOLNAM(EMCODE(IP))
      ELSE
      TEXT=NAME(NUMMER(IP))
      END IF
      DO 660,IR=1,NREP
      IF (REPHASE(IR).EQ.TEXT) THEN
!---- phase to remove
      DO IC=1,NC
        TOADD(IC)=0.0D0
      END DO
      DO IC=1,NUN
        TOADD(CHMCOD(IC))= &
        TOADD(CHMCOD(IC))-X(IP,IC)*NN(IP)*REFRA(IR)
      END DO
      CALL REMOPH(TOADD)
      END IF
!---- end phase to remove
  660 CONTINUE
  650 CONTINUE
      END IF
!+++++
  600 CONTINUE
      END IF
!*****
      GOTO 10
!---
  888 CONTINUE
      CLOSE (UNIT=drv)
      CALL PRTTBL
!----
      CALL CPUTIME(ZEITSTRING)
      DATI=sdate
      CALL LABLA(DATI,J)
      DATI=DATI(1:J)//'  '//ZEITSTRING
!------------------
!     open UNIT=inf
!------------------
      j=inf
      line=filename(j)(1:fnl(j))//ext(j)
      path=wpath
      akzess=' '
      state='unknown'
      call openfile(j,ierr)
      if(ierr.ne.0) STOP
!-----
!      WRITE (inf,FMT='(A)') DATI
      CALL PUST(inf,DATI)
      WRITE (inf,FMT='(I4)') NBUL
      DO II=1,NBUL
!       WRITE (inf,FMT='(A)') BULINE(II)
       CALL PUST(inf,BULINE(II))
      END DO
      WRITE (inf,FMT='(I4)') NCOMIN
      DO II=1,NCOMIN
!       WRITE (inf,FMT='(A)') COMINS(II)
       CALL PUST(inf,COMINS(II))
      END DO
      CLOSE (UNIT=inf)
!----
      RETURN
      END
!-----
!********************************
      SUBROUTINE MACHREF
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!
!-----END OF COMMON VARIABLES
      INTEGER(4) I,II,IS
!-----
      REFAS=.TRUE.
      RENUN2=NUN2
      DO I=1,NUN2
       RENUM(I)=NUMMER(I)
       REEMC(I)=EMCODE(I)
       RENN(I)=NN(I)
       IF (NUMMER(I).EQ.0) THEN
        IS=EMCODE(I)
        DO II=1,NEND(IS)
         REXEM(I,II)=XEM(I,II)
        END DO
       END IF
      END DO
!-----
      RETURN
      END
!-----
!********************************
      SUBROUTINE SHOWREF
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!
!-----END OF COMMON VARIABLES
      INTEGER(4) I,II,IS
!      INTEGER(4) IGAR,ISTA,IAND
      REAL(8) SUMME,XXSC(EMAX),GSC
      CHARACTER(32) CH16
      REAL(8) X16
!-----
      SUMME=0.0D0
      DO I=1,RENUN2
       IF (RENUM(I).EQ.0) THEN
        IS=REEMC(I)
        DO II=1,NEND(IS)
         XXSC(II)=REXEM(I,II)
        END DO
        CALL GNONID(IS,XXSC,GSC)
!      WRITE (6,1000) I,RENN(I),GSC
!      WRITE (out,1000) I,RENN(I),GSC
! 1000 FORMAT ('sol: I,NN(I),G(I)=',I2,2(2X,1PE15.8))
       ELSE
        GSC=GG(RENUM(I))
!      WRITE (6,1002) I,RENN(I),GSC
!      WRITE (out,1002) I,RENN(I),GSC
! 1002 FORMAT ('non: I,NN(I),G(I)=',I2,2(2X,1PE15.8))
       END IF
       SUMME=SUMME+RENN(I)*GSC
      END DO
!-----
!      IGAR=0
!      ISTA=0
!      IAND=0
!      DO I=1,NUN2
!       IF (NUMMER(I).EQ.0) THEN
!        IF (SOLNAM(EMCODE(I)).EQ.'Grt') IGAR=I
!        IF (SOLNAM(EMCODE(I)).EQ.'St') ISTA=I
!       ELSE
!        IF (NAME(NUMMER(I)).EQ.'andalusite') IAND=I
!       END IF
!      END DO
!-----
      WRITE (6,2000) SUMME
      WRITE (out,2000) SUMME
 2000 FORMAT (' Difference to reference: ',1PE15.8)
      CH16='G_overstep'
      X16=SUMME
      CALL SETTBL(CH16,X16)
!-----
!      IF (IGAR.NE.0.AND.TC.GT.544.6) THEN
!      WRITE (6,2010) NN(IGAR)
!      WRITE (out,2010) NN(IGAR)
! 2010 FORMAT (' amount of garnet: ',1PE15.8)
!      X16=SUMME/NN(IGAR)/12.0D0
!      CH16='Gref_Grt'
!      CALL SETTBL(CH16,X16)
!      END IF
!-----
!      IF (ISTA.NE.0.AND.TC.GT.555.3) THEN
!      WRITE (6,2012) NN(ISTA)
!      WRITE (out,2012) NN(ISTA)
! 2012 FORMAT (' amount of staurolite: ',1PE15.8)
!      X16=SUMME/NN(ISTA)/48.0D0/(TC-555.3)
!      CH16='Gref_St'
!      CALL SETTBL(CH16,X16)
!      END IF
!-----
!      IF (IAND.NE.0.AND.TC.GT.558.9) THEN
!      WRITE (6,2014) NN(IAND)
!      WRITE (out,2014) NN(IAND)
! 2014 FORMAT (' amount of andalusite: ',1PE15.8)
!      X16=SUMME/NN(IAND)/5.0D0/(TC-558.9)
!      CH16='Gref_And'
!      CALL SETTBL(CH16,X16)
!      END IF
!-----
      RETURN
      END
!-----
!-----
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
