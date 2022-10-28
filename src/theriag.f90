!-----Version: 11.03.2020
!               ***********
!               * THERIA_G *
!               ***********
!
!     Program written by:
!     Fred Gaidies and Christian de Capitani
!     at the Institute of Mineralogy and Petrography,
!     Basel University, Switzerland
!
!     for details and applications of algorithm see:
!     Gaidies, F., de Capitani C. and Abart, R.: THERIA_G: A
!     software program to nummerically model prograde garnet
!     growth.
!     Contrib. Mineral. Petrol. (under review)
!
!
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!*****
!
!-----END OF COMMON VARIABLES
      LOGICAL*4 LPFILE,SIM,LOLO,GRT
      INTEGER*4 I001,I002,I,I1,COMAY,NPTS,EINS,ALLES,ierr,j,k
      REAL*8 FF
      CHARACTER*16 CH16
      CHARACTER*500 CH001,CH002,SYREC,CHIN(2),ZEITSTRING
!*****
      progname='THERIAK'
      vers='11.03.2020'
      task='"Computation of equilibrium assemblages at given PT"'
      EINS=1
      call initialize('$THERIAK-FILES',ierr)
      if(ierr.ne.0) STOP
!*****
      CHIN(1)=' '
      CHIN(2)='no'
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
      DO I=1,2
        READ (UNIT=log,FMT='(A500)',END=411) CHIN(I)
      END DO
!---- new header for Theria_g
      call LABLA(vers,j)
      call LABLA(os,k)
      WRITE (6,90) vers(1:j),os(1:k)
   90 FORMAT (//' Program THERIA_G , Version (dd.mm.yy)',1x,a, &
      1x,'(',a,')')
      j=9+j+k
      WRITE(6,91) ('=',i=1,32+j)
   91 FORMAT(1X,130A1)
      WRITE(6,92)
   92 FORMAT(/,1x,'"Simulation of prograde garnet growth"',///, &
      ' Code based on THERIAK written by:', &
      /,11x,'Fred Gaidies and Christian de Capitani', &
      /,11x,'(Basel University, Switzerland)', &
      /,11x,'E-mail: fred.gaidies@unibas.ch' &
      /,11x,'christian.decapitani@unibas.ch'/)
      WRITE(6,91) ('=',j=1,80)
!---
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
      READ (5,FMT='(A500)') CH001
      IF (CH001.EQ.'?') THEN
        CALL helpme('$THK-START')
        GOTO 412
      END IF
!       IF (CH001.EQ.'files') THEN
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
      WRITE (scr,101) DBNAME(1:I001)
      WRITE (out,101) DBNAME(1:I001)
  101 FORMAT (/' database for this run: ',A/)
!*****
      COMAY=COMAX
      CALL PROREAD(SYREC)
      Call LABLA(filename(dat),I1)
      WRITE (scr,102) filename(dat)(1:I1)
      WRITE (out,102) filename(dat)(1:I1)
  102 FORMAT (/,' Input from file',1x,a)
      write(scr,104) ('-',I=1,I1+16)
      write(out,104) ('-',I=1,I1+16)
  104 format(1x,130a1,:)
      write(scr,106) TC,P
      write(out,106) TC,P
  106 format(' T =',F8.2,' C     P =',F9.2,' Bar')
      CALL PUST (scr,' '//SYREC)
      CALL PUST (out,' '//SYREC)
!*****
!---- jump this dialog for THERAI_G
!    3 WRITE (scr,110)
!      WRITE (out,110)
!  110 FORMAT (/
!     >' ---------------------------'/
!     >' define type of calculations'/
!     >' ---------------------------')
!      CALL LABLA(CHIN(2),I002)
!      IF (I002.EQ.0) I002=1
!      CH002=' Enter [ "?" | CR | "no" | "bin" | "loop" | filename ] <'
!     >//CHIN(2)(1:I002)//'>?'
!      CALL PUST (scr,CH002)
!      CH001=' '
!      READ (*,FMT='(A500)') CH001
       CH001='THERIA_G'
!---
!      IF (CH001.EQ.'?') THEN
!      CALL helpme('$THK-SPECIAL')
!      GOTO 3
!      END IF
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
      GRT=.FALSE.
      CALL TAXI(CH001,CH16)
!-----
      IF (CH16.EQ.' ') CH16='no'
      LPFILE=.FALSE.
!      IF (CH16.EQ.'bin') THEN
      IF (VERGL(CH16,'bin')) THEN
      SIM=.TRUE.
      CALL GELI(CH001,FF)
      IF (FF.EQ.0.0D0) THEN
      NPTS=100
      ELSE
      NPTS=IDINT(FF)
      END IF
      END IF
      IF (CH16.EQ.'loop') LOLO=.TRUE.
      IF (CH16.EQ.'THERIA_G') GRT=.TRUE.
      IF (.NOT.SIM.AND..NOT.LOLO.AND..NOT.GRT) LPFILE=.TRUE.
      IF (CH16.EQ.'no') LPFILE=.FALSE.
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
      DO I=1,2
        CALL PUST(log,CHIN(I))
      END DO
      CLOSE (UNIT=log)
!-----
!-----READ  PRTCOD, FORMUL AND USE FROM SYREC
!-----SET UP FIRST NUN COLUMNS OF MATRIX
      CALL GELI(SYREC,FF)
      PRTCOD=IDINT(FF)
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
      CH002=' '//SYREC(I1:)
      CALL PUST(scr,CH002)
      CALL PUST(out,CH002)
!-----
      IF (PRTLOG(5)) THEN
      WRITE (UNIT=scr,FMT=140) TEST,LO1MAX,EQUALX,DXMIN
      WRITE (UNIT=out,FMT=140) TEST,LO1MAX,EQUALX,DXMIN
  140 FORMAT (/,' TEST =',1PE11.4,8X,'LO1MAX =',I4,13X,'EQUALX =', &
      1PE11.4,6X,'DELXMIN =',1PE11.4)
      WRITE (UNIT=scr,FMT=141) DXSCAN,DXSTAR,STPSTA,STPMAX,GCMAX
      WRITE (UNIT=out,FMT=141) DXSCAN,DXSTAR,STPSTA,STPMAX,GCMAX
  141 FORMAT (' DELXSCAN =',1PE11.4,4X,'DELXSTAR =',1PE11.4,4X, &
      'STEPSTAR =',I4,11X,'STEPMAX =',I4,12X,'GCMAX =',I5)
      END IF
!-----
      CALL TAXI(SYREC,FORMUL)
! following for theria_g only
      MAKEO=0
      IF (INDEX(FORMUL,'O(?)').NE.0) MAKEO=1
      CALL TAXI(SYREC,USE)
!+++++
      CALL CHEMIE(COMAY,NC,OXYDE,OXANZ,FORMUL,CHEM)
      CALL LABLA(USE,LUSE)
!-----------------------------------------------------------------
      CALL DBREAD
      CALL NURVONPT
      CALL CALSTR
      ALLES=NSOL
      IF (PRTLOG(1)) THEN
      CALL GIBBSTEST(EINS,ALLES)
      STOP
      END IF
      IF (LPFILE.OR.PRTCOD.EQ.0) CALL GIBBSTEST(EINS,ALLES)
      IF (.NOT.LPFILE) THEN
      IF (PRTLOG(2).OR.PRTLOG(3).OR.PRTLOG(4)) CALL PRININ
      IF (PRTCOD.EQ.-1) STOP
!---
      IF (SIM) CALL SIMPL(NPTS)
      IF (LOLO) CALL MANLOOP
      IF (GRT) CALL GROWG
      IF (.NOT.SIM.AND..NOT.LOLO.AND..NOT.GRT) CALL THERIA
      CLOSE (dat)
!---
      ELSE
      CLOSE (dat)
      CH001=CHIN(2)
      CALL MINILOOP(CH001)
      GOTO 999
!-----------------------------------------------------------------
  999 CONTINUE
!-----------------------------------------------------------------
      END IF
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
!     PRTLOG(10)=.TRUE.    (used to print image for thermap)
!*****
!-----------------------------------------------------------------
!     CALL NURVONPT
!     CALL CALSTR
!     IF (PRTLOG(2).OR.PRTLOG(3).OR.PRTLOG(4)) CALL PRININ
!     CALL THERIA
      CALL CPUTIME(ZEITSTRING)
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
      CHARACTER*500 SYREC
!----
   10 CONTINUE
      SYREC=' '
      WRITE (scr,110)
      WRITE (out,110)
  110 FORMAT (/ &
      ' -------------------------------'/ &
      ' define Temperature and Pressure'/ &
      ' -------------------------------')
      WRITE (scr,1000)
 1000 FORMAT(' Enter [ "?" | CR | "end" | T(C)  P(bar) ]: ')
      READ (UNIT=kbd,FMT='(A500)',END=99) SYREC
!      IF (SYREC.EQ.'end'.OR.SYREC.eq.' ') RETURN
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
      CALL THERIA
      GOTO 10
!----
   99 CONTINUE
      RETURN
      END
!-----
!********************************
      SUBROUTINE SIMPL(NPTS)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!
!-----END OF COMMON VARIABLES
      INTEGER*4 I,I1,I2,I3,II,COMAY,EINS,ALLES,JX,NPTS
      REAL*8 FF,CHEXX(2,COMAX),YWERT(0:100),XXMIN,XXMAX,YYMIN,YYMAX, &
      FA,FB,BREIT,HOCH,Y0,Y1,NEUWERT(0:100),SIZLAB,ANGE,XVAL(0:100)
      CHARACTER*500 SYREC,BULKLINE(2)
      CHARACTER*16 XVARI,YVARI,BLANK
      CHARACTER*25 XTXT,YTXT
!
      integer ierr,j
!----
      PRTLOG(9)=.TRUE.
      PRTLOG(2)=.TRUE.
      NVARTBL=0
      NROWTBL=0
      COMAY=COMAX
      EINS=1
      BLANK=' '
!     DO 500,I=1,8
! 500 PRTLOG(I)=.FALSE.
!---
      DO 510,I=1,2
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
  510 CONTINUE
      XXMIN=0.0D0
      XXMAX=1.0D0
      YYMIN=0.0D0
      YYMAX=1.0D0
      BREIT=20.0D0
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
!     WRITE (scr,2100) CHNAME(1),CHEM(CHMCOD(1))
      WRITE (out,2100) CHNAME(1),CHEM(CHMCOD(1))
 2100 FORMAT (' Bulk composition: ',A8,1X,F11.6)
      DO 522,I=2,NUN
!     WRITE (scr,1000) CHNAME(I),CHEM(CHMCOD(I))
      WRITE (out,1000) CHNAME(I),CHEM(CHMCOD(I))
 1000 FORMAT (19X,A8,1X,F11.6)
  522 CONTINUE
      CALL NURVONPT
      CALL CALSTR
      ALLES=NSOL
      CALL GIBBSTEST(EINS,NSOL)
!----
      FF=DBLE(NPTS)
      DO 550,JX=0,NPTS
      FB=DBLE(JX)/FF
      FA=1.0D0-FB
      XVAL(JX)=FB
      DO I=1,NUN
        BULK(I)=0.0D0
      END DO
      DO I=1,NUN
        II=CHMCOD(I)
        BULK(I)=FA*CHEXX(1,II)+FB*CHEXX(2,II)
      END DO
!----
      CALL TRENNE(130)
      CALL NURVONPT
      CALL CALSTR
      CALL PRININ
      CALL THERIA
      YWERT(JX)=GGTOT
  550 CONTINUE
!-----
      CALL PRTTBL
      Y0=YWERT(0)
      Y1=YWERT(NPTS)
      YYMAX=-1D34
      YYMIN=1D34
      FF=DBLE(NPTS)
      DO 600,I=0,NPTS
      NEUWERT(I)=YWERT(I)-Y0-(Y1-Y0)*DBLE(I)/FF
      IF (NEUWERT(I).GT.YYMAX) YYMAX=NEUWERT(I)
      IF (NEUWERT(I).LT.YYMIN) YYMIN=NEUWERT(I)
      WRITE (*,1005) I,YWERT(I),NEUWERT(I)
      WRITE (out,1005) I,YWERT(I),NEUWERT(I)
 1005 FORMAT (1X,I4,2X,1PE20.12,2X,1PE20.12)
  600 CONTINUE
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
      WRITE (UNIT=*,FMT=2005) YYMIN
 2005 FORMAT (/,' Minimum and maximum are both = ',1PE10.3)
      YYMAX=YYMAX+DABS(YYMAX*0.05D0+1.0D0)
      YYMIN=YYMIN-DABS(YYMIN*0.05D0+1.0D0)
      END IF
!----
      WRITE (bin,2010) BREIT+1.5D0
 2010 FORMAT ('NULLPT     5   3'/ &
      'FONT      Helvetica'/ &
      'FAT   0.02'/'PUNKTE   98  0.5   ',F7.4,'  0.25  999  999 ', &
      'FAT   0.03'/ &
      'NPLOG2')
      XTXT='x'
      YTXT='G(rel) [J]'
      WRITE (bin,FMT='(A25/A25)') XTXT,YTXT
      WRITE (bin,2012) XXMIN,XXMAX,YYMIN,YYMAX,BREIT,HOCH
 2012 FORMAT(4(1PE20.12),0P,F10.3,F10.3)
!-----
      WRITE (bin,FMT='(''    2'',I5,''    0    0    0    0'')') &
      NPTS+1
      WRITE (bin,2016) (XVAL(I),NEUWERT(I),3-MIN0(1,I),I=0,NPTS)
 2016 FORMAT (7(2(1PE20.12),I2))
      SIZLAB=0.35D0
      ANGE=0.0D0
      CALL LABLA(YVARI,I2)
!--
      FORMUL=BULKLINE(1)
      CALL LABLA(FORMUL,I1)
      WRITE (bin,2025) XVAL(0),YYMAX,SIZLAB,ANGE,I1,FORMUL
      FORMUL=BULKLINE(2)
      CALL LABLA(FORMUL,I1)
!--
      WRITE (bin,2025) XVAL(NPTS),YYMAX,SIZLAB,ANGE,I1,FORMUL
 2025 FORMAT (2(1PE20.12),0PF10.7,F10.4,I5,A170)
      CLOSE (UNIT=bin)
      RETURN
      END
!-----
!********************************
      SUBROUTINE ADDTOBUL(TOADD)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
!-----END OF COMMON VARIABLES
      REAL*8 FF,TOADD(COMAX),FAK
      INTEGER*4 I,IC,I0
      LOGICAL*4 MORE
!*****
      FAK=1.0D0
      I0=0
      DO 500,IC=1,NC
      IF (TOADD(IC).LT.0.0D0) THEN
      FF=-CHEM(IC)/TOADD(IC)
      IF (FF.LT.FAK) THEN
      FAK=FF
      I0=IC
      END IF
      END IF
  500 CONTINUE
      IF (FAK.LE.0.0D0) RETURN
      DO IC=1,NC
        CHE(IC)=CHEM(IC)+TOADD(IC)*FAK
        IF (CHE(IC).LT.1D-6) CHE(IC)=0.0D0
      END DO
      IF (I0.NE.0) CHE(I0)=0.0D0
      MORE=.FALSE.
      DO I=1,NC
        IF (CHE(I).EQ.0.0D0.NEQV.CHEM(I).EQ.0.0D0) MORE=.TRUE.
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
      REAL*8 TOADD(COMAX)
      INTEGER*4 I,IC
      LOGICAL*4 MORE
!*****
      DO IC=1,NC
        CHE(IC)=CHEM(IC)+TOADD(IC)
        IF (CHE(IC).LT.0.0D0) CHE(IC)=0.0D0
      END DO
      MORE=.FALSE.
      DO I=1,NC
        IF (CHE(I).EQ.0.0D0.NEQV.CHEM(I).EQ.0.0D0) MORE=.TRUE.
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
      INTEGER*4 NPTS,ierr,j
      REAL*8 FF,TPSTEP,T1,T2,P1,P2,DET,DEP,TOADD(COMAX),REFRA(50)
      INTEGER*4 I,IC,IR,IP,I001,COMAY,NREP,KOUNT
      CHARACTER*500 CH001,SYREC
!      CHARACTER*500 CH002
      CHARACTER*16 REPHASE(50),CH16,TEXT
      CHARACTER*8 LOOPCODE
!----
!      DO I=1,10
!        PRTLOG(I)=.FALSE.
!      END DO
      PRTLOG(9)=.TRUE.
      PRTLOG(2)=.TRUE.
      PRTLOG(6)=.TRUE.
      NVARTBL=0
      NROWTBL=0
      NREP=0
      KOUNT=0
      COMAY=COMAX
      CALL LABLA(CH001,I001)
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
!      CH002='special input: '//SYREC
!      CALL PUST(scr,CH002)
!      CALL PUST(out,CH002)
      CALL TAXI(SYREC,LOOPCODE)
!*****
      IF (LOOPCODE(1:1).EQ.'!') GOTO 10
!      IF (LOOPCODE.EQ.'END') GOTO 888
      IF (VERGL(LOOPCODE,'END')) GOTO 888
!*****
!      IF (LOOPCODE.EQ.'COMP') THEN
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
!      IF (LOOPCODE.EQ.'ADD') THEN
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
!      IF (LOOPCODE.EQ.'REMOVE') THEN
      IF (VERGL(LOOPCODE,'REMOVE')) THEN
      CALL TAXI(SYREC,CH16)
      CALL GELI(SYREC,FF)
      IR=0
      DO 600,I=1,NREP
        IF (CH16.EQ.REPHASE(I)) THEN
        REFRA(I)=FF/100.0D0
        IR=I
        GOTO 601
        END IF
  600 CONTINUE
  601 CONTINUE
      IF (IR.EQ.0) THEN
      NREP=NREP+1
      IF (NREP.GT.50) THEN
      WRITE (UNIT=*,FMT='('' NREP too big'')')
      STOP
      END IF
      REPHASE(NREP)=CH16
      REFRA(NREP)=FF/100.0D0
      END IF
      GOTO 10
      END IF
!*****
!      IF (LOOPCODE.EQ.'TP') THEN
      IF (VERGL(LOOPCODE,'TP')) THEN
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
      DO 800,I=1,NPTS
      CALL TRENNE(130)
      TC=TC+DET
      P=P+DEP
      CALL NURVONPT
      CALL CALSTR
      IF (PRTLOG(2).OR.PRTLOG(3).OR.PRTLOG(4)) CALL PRININ
      KOUNT=KOUNT+1
      WRITE (UNIT=scr,FMT=1000) KOUNT
      WRITE (UNIT=out,FMT=1000) KOUNT
 1000 FORMAT (/,' THERIAK-loop = ',I6)
!      IF (KOUNT.EQ.30) THEN
!      TEST=-TEST
!      END IF
      CALL THERIA
!      IF (NUMMER(1).GT.0) STOP
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
  800 CONTINUE
      END IF
!*****
      GOTO 10
!---
  888 CONTINUE
      CLOSE (UNIT=drv)
      CALL PRTTBL
      RETURN
      END
!-----
!********************************
      SUBROUTINE GROWG
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      INCLUDE 'files.cmn'
!-----
      INTEGER*4 MAXLAY,MAXGEN
      PARAMETER (MAXLAY=400,MAXGEN=50)
!-----END OF COMMON VARIABLES
      REAL*8 VOL1M(MAXGEN),GARNET(MAXGEN,4,MAXLAY),VLAY(MAXGEN,MAXLAY), &
      DLAY(MAXGEN,MAXLAY),XLAY(MAXGEN,MAXLAY),MOLAY(MAXGEN,MAXLAY), &
      TLAY(MAXGEN,MAXLAY),PLAY(MAXGEN,MAXLAY),VLAYTOT(MAXGEN), &
      MOLAYTOT(MAXGEN),VOLGAR,RAD0,NGARCCM(MAXGEN)
      INTEGER*4 NLAY(MAXGEN),NGARTOT(MAXGEN),NGENTOT, &
      NGEN,NCURAS,CURASNR,LAYASNR(MAXGEN,MAXLAY)
      CHARACTER*125 CURAS,LAYAS(MAXGEN,MAXLAY),CURASSES(MAXLAY)
      COMMON /LAYER/ VOL1M,GARNET,VLAY,DLAY,XLAY,MOLAY,TLAY,PLAY, &
      VLAYTOT,MOLAYTOT,VOLGAR,RAD0
      COMMON /LAYEI/ NLAY,NGARTOT,NGARCCM,NGENTOT,NGEN,NCURAS, &
      CURASNR,LAYASNR
      COMMON /LAYEC/ CURAS,LAYAS,CURASSES
!-----END OF LAYER COMMON VARIABLES
      REAL*8 ORIGBUL(COMAX),GROWBUL(COMAX),BACKBUL(COMAX)
      COMMON /BULLR/ ORIGBUL,GROWBUL,BACKBUL
!-----END OF BULK COMMON VARIABLES
      REAL*8 VMN(MAXGEN,MAXLAY),VFE(MAXGEN,MAXLAY),VMG(MAXGEN,MAXLAY), &
      VCA(MAXGEN,MAXLAY),XXC(MAXGEN,MAXLAY),NODIST, &
      DRVHR(MAXGEN,MAXLAY),XHR(MAXGEN,MAXLAY),CVHR(MAXGEN,3*MAXLAY), &
      VNOD(MAXGEN,MAXLAY),MONOD(MAXGEN,MAXLAY),VNODTOT(MAXGEN), &
      MONODTOT(MAXGEN),MOSPELAST,MOALMLAST,MOPYRLAST,MOGROLAST, &
      BACKSPE,BACKALM,BACKPYR,BACKGRO,MNGENLA(MAXGEN),FEGENLA(MAXGEN), &
      MGGENLA(MAXGEN),CAGENLA(MAXGEN)
      INTEGER*4 NNOD(MAXGEN),NRHR(MAXGEN),DIFFCODE,DFCODE
      COMMON /PROFIR/ VMN,VFE,VMG,VCA,XXC,NODIST,DRVHR,CVHR,XHR, &
      VNOD,MONOD,VNODTOT,MONODTOT,MOSPELAST,MOALMLAST,MOPYRLAST, &
      MOGROLAST,BACKSPE,BACKALM,BACKPYR,BACKGRO,MNGENLA,FEGENLA, &
      MGGENLA,CAGENLA
      COMMON /PROFII/ NNOD,NRHR,DIFFCODE,DFCODE
!----- end of Profile Common variables
      INTEGER*4 NSEG,SEGM
      REAL*8 TSEG(50),PSEG(50),TIMSEG(50)
      COMMON /SEGMI/ NSEG,SEGM
      COMMON /SEGMR/ TSEG,PSEG,TIMSEG
!----- end of stepping Common variables
      INTEGER*4 N00X(MAXGEN)
      COMMON /GENIN/ N00X
!----- end of genprint Common variables
      REAL*8 RJ,RCAL,PI,DMNZERO,AEMN,AVMN,DFEZERO,AEFE,AVFE, &
      DMGZERO,AEMG,AVMG
      COMMON /GLODA/ RJ,RCAL,PI,DMNZERO,AEMN,AVMN,DFEZERO,AEFE,AVFE, &
      DMGZERO,AEMG,AVMG
!----- end of GLOBAL DATA Common variables
!-----
      REAL*8 FF,T1,T2,P1,P2, &
      VAIM,TINI,PINI, &
      DIFTIM,TIMTOT, &
      F1,RAIM,GART,TPMY,TLO,PLO,TUP,PUP, &
      NG1,NG2,NGLO, &
      TNEW,PNEW,NGNEW,TIMLADI
      INTEGER*4 I,IC,IR,NREP,KOUNT,I1,I2, &
      INICODE,GROWCODE,FER,IG,IS,OONR
      CHARACTER*500 SYREC
      CHARACTER*125 AS1,AS2,ASUP,ASNEW
      CHARACTER*4 MODUS

      GART=1.0D0
!----
!      OPEN (UNIT=40,FILE='grt_profiles',STATUS='UNKNOWN')
!      OPEN (UNIT=41,FILE='grt_layers',STATUS='UNKNOWN')
!      OPEN (UNIT=42,FILE='grt_nodes',STATUS='UNKNOWN')
      DO I=1,11
        PRTLOG(I)=.FALSE.
      END DO
      DRU=.TRUE.
!      PRTLOG(9)=.TRUE.
!      PRTLOG(2)=.TRUE.
!      PRTLOG(6)=.TRUE.
!----

!mar2015
      WRITE(UNIT=6,FMT='(''enter GROWG'')')
      WRITE(UNIT=10,FMT='(''enter GROWG'')')

      CALL TRENNE(130)
      WRITE (scr,1000)
      WRITE (out,1000)
 1000 FORMAT (/ &
      ' -----------------------------------------------'/ &
      ' THERIA_G - Simulation of prograde garnet growth'/ &
      ' Fred Gaidies and Cristian de Capitani '/ &
      ' -----------------------------------------------')
!-----
      OONR=out
      NVARTBL=0
      NROWTBL=0
!C      DO 501,I=1,NUN
      DO 300,I=1,NC
      GROWBUL(I)=0.0D0
      BACKBUL(I)=0.0D0
      ORIGBUL(I)=CHEM(I)
  300 CONTINUE
!---- check that database has GARNET, with spessartine, almanine, pyrope, grossular
      IS=0
      DO I=1,NSOL
        IF (SOLNAM(I).EQ.'GARNET') IS=I
      END DO
      IF (IS.EQ.0) THEN
      WRITE (UNIT=scr,FMT='('' no GARNET in database.'')')
      WRITE (UNIT=out,FMT='('' no GARNET in database.'')')
      STOP
      END IF
      IC=0
      DO 304,I=1,NEND(IS)
      IF (NAME(EM(IS,I)).EQ.'spessartine') IC=IC+1
      IF (NAME(EM(IS,I)).EQ.'almandine') IC=IC+1
      IF (NAME(EM(IS,I)).EQ.'pyrope') IC=IC+1
      IF (NAME(EM(IS,I)).EQ.'grossular') IC=IC+1
  304 CONTINUE
      IF (IC.NE.4) THEN
      WRITE (UNIT=scr, &
      FMT='('' wrong endmembers of GARNET in database.'')')
      WRITE (UNIT=out, &
      FMT='('' wrong endmembers of GARNET in database.'')')
      STOP
      END IF
!---- define GLOBAL DATA
      PI=3.14159265358979D0
      RJ=8.31451070D0
      RCAL=RJ/4.1868D0
!    DMnzero,DFezero,DMgzero      pre-exponential constant in cm^2/s
!    AEMn,AEFe,AEMg               activation energy for diffusion at 1 bar in cal/mol
!    AVMn,AVFe,AVMg               activation volume for diffusion in cm^3/mol
      OPEN (UNIT=44,FILE='theriag_DIF.txt',STATUS='OLD')
      READ (UNIT=44,FMT='(A250)') SYREC
      READ (44,*) DMNZERO,AEMN,AVMN
      READ (44,*) DFEZERO,AEFE,AVFE
      READ (44,*) DMGZERO,AEMG,AVMG
      WRITE (scr,1002)
      WRITE (out,1002)
 1002 FORMAT ( &
      ' from file: theriag_DIF.txt',/ &
      ' --------------------------')
      CALL PUST(6,SYREC)
      WRITE (6,1004) DMNZERO,AEMN,AVMN,DFEZERO,AEFE,AVFE, &
      DMGZERO,AEMG,AVMG
      WRITE (out,1004) DMNZERO,AEMN,AVMN,DFEZERO,AEFE,AVFE, &
      DMGZERO,AEMG,AVMG
 1004 FORMAT (' D = D0 * EXP [(-Ae-(p-p0)*Av) / RT]', &
      //'       D0[cm^2/s]   Ae[cal/mol]   Av[cm^3/mol]', &
      /'       ----------   -----------   ------------', &
      /' Mn:  ',1PE12.5,2X,1PE12.5,2X,1PE12.5, &
      /' Fe:  ',1PE12.5,2X,1PE12.5,2X,1PE12.5, &
      /' Mg:  ',1PE12.5,2X,1PE12.5,2X,1PE12.5, &
      /' Ca:   D(Ca) = D(Fe)/2')
      CLOSE (UNIT=44)
!----
      OPEN (UNIT=44,FILE='theriag_CSD.txt',STATUS='OLD')
      READ (44,*) NODIST,RAD0
      IF (NODIST.EQ.0.0D0) NODIST=10.0D0
      IF (RAD0.EQ.0.0D0) RAD0=100.0D0
      NGENTOT=0
      DO 310,IG=1,MAXGEN
        READ (44,*,END=311) NGARCCM(IG)
        IF (NGARCCM(IG).EQ.0) GOTO 311
        NGENTOT=IG
  310 CONTINUE
  311 CONTINUE
      CLOSE (UNIT=44)
      WRITE (scr,1006)
      WRITE (out,1006)
 1006 FORMAT (/ &
      ' from file: theriag_CSD.txt',/ &
      ' --------------------------')
      WRITE (scr,1008) NODIST,NODIST+RAD0
      WRITE (out,1008) NODIST,NODIST+RAD0
 1008 FORMAT ( &
      ' Shell thickness:',F10.5,' mue', &
      /,' Size of radius class:     ',F10.5,' mue')
      DO 312,IG=1,NGENTOT
      WRITE (scr,1010) IG,NGARCCM(IG)
      WRITE (out,1010) IG,NGARCCM(IG)
 1010 FORMAT (' Radius class :',I5,3X,'no. of garnets:  ',F7.4, &
      ' [/cm^3]')
  312 CONTINUE
      WRITE (6,1012)
      WRITE (out,1012)
 1012 FORMAT (/ &
      ' from file: theriag_PTt.txt',/ &
      ' --------------------------')
      WRITE (scr,1014)
      WRITE (out,1014)
 1014 FORMAT ( &
      ' P-T-t-path:  temperature  pressure    time [m.y.]',/ &
      '              -----------  --------    -----------')
      NSEG=0
      OPEN (UNIT=44,FILE='theriag_PTt.txt',STATUS='OLD')
      DO 314,I=1,100
      READ (UNIT=44,FMT='(A500)',END=315) SYREC
      CALL GELI(SYREC,FF)
      IF (FF.EQ.0.0D0) GOTO 315
      NSEG=NSEG+1
      TSEG(NSEG)=FF
      CALL GELI(SYREC,FF)
      PSEG(NSEG)=FF
      CALL GELI(SYREC,FF)
      TIMSEG(NSEG)=FF
      WRITE (scr,1016) TSEG(NSEG),PSEG(NSEG),TIMSEG(NSEG)
      WRITE (out,1016) TSEG(NSEG),PSEG(NSEG),TIMSEG(NSEG)
 1016 FORMAT (13X,F10.4,2X,F10.2,2X,F10.5)
  314 CONTINUE
  315 CONTINUE
      CLOSE (UNIT=44)
!---
      WRITE (scr,1018)
      WRITE (out,1018)
 1018 FORMAT(/ &
      ' bulk composition [mol]:',/ &
      ' -----------------------')
      DO 320,I=1,NC
      IF (ORIGBUL(I).NE.0.0D0) THEN
      WRITE (scr,1020) OXYDE(I),ORIGBUL(I)
      WRITE (out,1020) OXYDE(I),ORIGBUL(I)
 1020 FORMAT (1X,A8,F13.8)
      END IF
  320 CONTINUE
!----
      NCURAS=0
      NGEN=1
      INICODE=0
      DIFFCODE=0
      DFCODE=1
      GROWCODE=0
      NVARTBL=0
      NROWTBL=0
      NREP=0
      KOUNT=0
      TIMTOT=0.0D0
      TPMY=0.0D0
      NGARTOT(1)=1
      VOL1M(1)=1D-12*4.0D0*PI/3.0D0
!-----
      DO 400,IG=1,NGENTOT
      N00X(IG)=0
      NNOD(IG)=0
      NLAY(IG)=0
      VLAYTOT(IG)=0.0D0
      MOLAYTOT(IG)=0.0D0
      DO IR=1,4
        DO IC=1,200
          GARNET(IG,IR,IC)=0.0D0
        END DO
      END DO
  400 CONTINUE
      BACKSPE=0.0D0
      BACKALM=0.0D0
      BACKPYR=0.0D0
      BACKGRO=0.0D0
!-----
      TIMLADI=TIMSEG(1)
      FER=0
      SEGM=1
      TINI=TSEG(1)
      PINI=PSEG(1)
!---
      T1=TINI
      P1=PINI
      TC=T1
      P=P1
      WRITE (scr,2000) TC,P
      WRITE (out,2000) TC,P
 2000 FORMAT (/,' --> 1st step',4X,'T = ',F10.4,3X,'P = ',F11.3)
      CALL PRTBUL
      CALL NURVONPT
      CALL CALSTR
      CALL MAKEOX
      CALL THERIA
      CALL PRTSHORT
      AS1=CURAS
      NG1=VOLGAR
      VOL1M(1)=1D-12*GART*4.0D0*PI/3.0D0
      DO IG=1,NGENTOT
        NGARTOT(IG)=INT(NGARCCM(IG)*VOLSOL)
      END DO
      DO IG=1,NGENTOT
        VOL1M(IG)=1D-12*DBLE(NGARTOT(IG))*4.0D0*PI/3.0D0
      END DO
!-----
!      DO 408,IG=1,NGENTOT
!      WRITE (scr,2002) IG,NGARTOT(IG),VOL1M(IG)
!      WRITE (out,2002) IG,NGARTOT(IG),VOL1M(IG)
! 2002 FORMAT ('size class :',I5,3X,'no. of garnets:',I7,
!     >4X,'VOL1M: ',1PE15.8)
!  408 CONTINUE
!---
!C      T2=T1+TINCR
!C      P2=P1+PINCR
      CALL NEXTSTEP(T1,P1,TNEW,PNEW,FER)
      T2=TNEW
      P2=PNEW
!---
      TC=T2
      P=P2
      WRITE (scr,2004) TC,P
      WRITE (out,2004) TC,P
 2004 FORMAT (/,' --> 2nd step',4X,'T = ',F10.4,3X,'P = ',F11.3)
      CALL PRTBUL
      CALL NURVONPT
      CALL CALSTR
      CALL MAKEOX
      CALL THERIA
      CALL PRTSHORT
      AS2=CURAS
      NG2=VOLGAR
!-----
      MODUS='STEP'
!-----
    1 CONTINUE
      RAIM=NODIST
      VAIM=0.0D0
      DO 410,IG=1,NGEN
      IF (NLAY(IG).EQ.0) THEN
      VAIM=VAIM+(RAIM**3)*VOL1M(IG)
      ELSE
      FF=XLAY(IG,NLAY(IG))+RAIM
      F1=(FF**3)*VOL1M(IG)
      VAIM=VAIM+F1-VLAYTOT(IG)
      END IF
  410 CONTINUE
!=====
!----- action if assemblage 1 NE assemblage 2
      IF (AS1.NE.AS2) THEN
      CALL LABLA(AS1,I1)
      CALL LABLA(AS2,I2)
      WRITE (scr,2006) AS2(1:I2)
      WRITE (out,2006) AS2(1:I2)
 2006 FORMAT (/' --> different assemblage',28X,'assemblage: ',A)
!      WRITE (UNIT=40,FMT='(/'' --> different assemblage'')')
!      WRITE (scr,2010) T1,P1,NG1,AS1(1:i1),T2,P2,NG2,AS2(1:I2)
!      WRITE (40,2010) T1,P1,NG1,AS1(1:i1),T2,P2,NG2,AS2(1:I2)
! 2010 FORMAT (' T1,P1,NG1 = ',3(2X,1PE15.8),2X,A/
!     >' T2,P2,NG2 = ',3(2X,1PE15.8),2X,A)
      CALL CHECKASS(T1,P1,AS1,T2,P2,AS2,TLO,PLO,NGLO,TUP,PUP,ASUP)
!
!      IF (NGLO.EQ.0.0D0) THEN
!      WRITE (scr,2007)
! 2007 FORMAT (' NGLO=0')
!      STOP
!      END IF
!
      T2=TLO
      P2=PLO
      AS2=AS1
      NG2=NGLO
      MODUS='ASS'
      GOTO 1
      END IF
!=====
!----- action if assemblage 1 EQ assemblage 2
      IF (AS1.EQ.AS2) THEN
!===== modus='step'
      IF (MODUS.EQ.'STEP') THEN
!----- NG2=0
      IF (NG2.EQ.0) THEN
      CALL LABLA(AS1,I1)
      CALL LABLA(AS2,I2)
      WRITE (scr,2011) AS2(1:I2)
      WRITE (out,2011) AS2(1:I2)
 2011 FORMAT (/' --> step: ass2 no garnet',28X,'assemblage: ',A)
!      WRITE (UNIT=40,FMT='(/'' --> step: ass2 no garnet'')')
!      WRITE (scr,2012) T1,P1,NG1,AS1(1:I1),T2,P2,NG2,AS2(1:I2)
!      WRITE (40,2012) T1,P1,NG1,AS1(1:I1),T2,P2,NG2,AS2(1:I2)
! 2012 FORMAT (' T1,P1,NG1 = ',3(2X,1PE15.8),2X,A/
!     >' T2,P2,NG2 = ',3(2X,1PE15.8),2X,A)
!      CALL JUSTFORA(T2,P2)
      IF (NLAY(1).GT.1) THEN
      CALL NEXTDIFTIM(T2,P2,TIMLADI,DIFTIM)
      CALL DIFFUS(T2,P2,DIFTIM,OONR)
      TIMLADI=TIMLADI+DIFTIM
      TIMTOT=TIMTOT+DIFTIM
      DIFFCODE=1
      CALL PRTGAR(TIMTOT)
      CALL PRTNOD(TIMTOT)
      ELSE
      IF (NLAY(1).EQ.1) CALL PRTNOD(TIMTOT)
      CALL NEXTDIFTIM(T2,P2,TIMLADI,DIFTIM)
      TIMLADI=TIMLADI+DIFTIM
      END IF
      GOTO 888
      END IF
!----- NG2>=VAIM
      IF (NG2.GE.VAIM) THEN
      CALL LABLA(AS1,I1)
      CALL LABLA(AS2,I2)
      WRITE (scr,2013) AS2(1:I2)
      WRITE (out,2013) AS2(1:I2)
 2013 FORMAT (/' --> step: interval',34X,'assemblage: ',A)
!      WRITE (UNIT=40,FMT='(/'' --> step: interval'')')
!      WRITE (scr,2014) T1,P1,NG1,AS1(1:I1),T2,P2,NG2,AS2(1:I2)
!      WRITE (40,2014) T1,P1,NG1,AS1(1:I1),T2,P2,NG2,AS2(1:I2)
! 2014 FORMAT (' T1,P1,NG1 = ',3(2X,1PE15.8),2X,A/
!     >' T2,P2,NG2 = ',3(2X,1PE15.8),2X,A)
      CALL GROW1L(T1,P1,AS1,T2,P2,VAIM,TNEW,PNEW,ASNEW,NGNEW)
      T2=TNEW
      P2=PNEW
      AS2=ASNEW
      NG2=NGNEW
      MODUS='GR1L'
      GOTO 1
!----- NG2<VAIM
      ELSE
      WRITE (scr,2015) NG2
      WRITE (out,2015) NG2
 2015 FORMAT (/' --> step: ass2 not enough',27X,'vol = ',1PE15.8)
!      WRITE (UNIT=40,FMT='(/'' --> step: ass2 not enough'')')
!      CALL LABLA(AS1,I1)
!      CALL LABLA(AS2,I2)
!      WRITE (scr,2016) T1,P1,NG1,AS1(1:I1),T2,P2,NG2,AS2(1:I2)
!      WRITE (40,2016) T1,P1,NG1,AS1(1:I1),T2,P2,NG2,AS2(1:I2)
! 2016 FORMAT (' T1,P1,NG1 = ',3(2X,1PE15.8),2X,A/
!     >' T2,P2,NG2 = ',3(2X,1PE15.8),2X,A)
      GOTO 888
      END IF
      END IF
!=====  end modus='step'
!===== modus='ass'
      IF (MODUS.EQ.'ASS') THEN
!----- NG2=0
      IF (NG2.EQ.0) THEN
      CALL LABLA(AS1,I1)
      CALL LABLA(AS2,I2)
      WRITE (scr,2017) AS2(1:I2)
      WRITE (out,2017) AS2(1:I2)
 2017 FORMAT (/' --> ass: ass2 no garnet',29X,'assemblage: ',A)
!      WRITE (UNIT=40,FMT='(/'' --> ass: ass2 no garnet'')')
!      WRITE (scr,2018) T1,P1,NG1,AS1(1:I1),T2,P2,NG2,AS2(1:I2)
!      WRITE (40,2018) T1,P1,NG1,AS1(1:I1),T2,P2,NG2,AS2(1:I2)
! 2018 FORMAT (' T1,P1,NG1 = ',3(2X,1PE15.8),2X,A/
!     >' T2,P2,NG2 = ',3(2X,1PE15.8),2X,A)
!      CALL JUSTFORA(T2,P2)
      IF (NLAY(1).GT.1) THEN
      CALL NEXTDIFTIM(T2,P2,TIMLADI,DIFTIM)
      CALL DIFFUS(T2,P2,DIFTIM,OONR)
      TIMLADI=TIMLADI+DIFTIM
      TIMTOT=TIMTOT+DIFTIM
      DIFFCODE=1
      CALL PRTGAR(TIMTOT)
      CALL PRTNOD(TIMTOT)
      ELSE
      IF (NLAY(1).EQ.1) CALL PRTNOD(TIMTOT)
      CALL NEXTDIFTIM(T2,P2,TIMLADI,DIFTIM)
      TIMLADI=TIMLADI+DIFTIM
      END IF
      T2=TUP
      P2=PUP
      AS2=ASUP
      NG2=0.0D0
      MODUS='STEP'
      GOTO 888
      END IF
!----- NG2>=VAIM
      IF (NG2.GE.VAIM) THEN
      WRITE (UNIT=scr,FMT='(/'' --> ass: interval'')')
      WRITE (UNIT=out,FMT='(/'' --> ass: interval'')')
!      WRITE (UNIT=40,FMT='(/'' --> ass: interval'')')
!      CALL LABLA(AS1,I1)
!      CALL LABLA(AS2,I2)
!      WRITE (scr,2020) T1,P1,NG1,AS1(1:I1),T2,P2,NG2,AS2(1:I2)
!      WRITE (40,2020) T1,P1,NG1,AS1(1:I1),T2,P2,NG2,AS2(1:I2)
! 2020 FORMAT (' T1,P1,NG1 = ',3(2X,1PE15.8),2X,A/ &
!      ' T2,P2,NG2 = ',3(2X,1PE15.8),2X,A)
      T2=TLO
      P2=PLO
      AS2=AS1
      NG2=NGLO
      MODUS='STEP'
      GOTO 1
!----- NG2<VAIM
      ELSE
      WRITE (UNIT=scr,FMT='(/'' --> ass: make layer'')')
      WRITE (UNIT=out,FMT='(/'' --> ass: make layer'')')
!      WRITE (UNIT=40,FMT='(/'' --> ass: make layer'')')
!      CALL LABLA(AS1,I1)
!      CALL LABLA(AS2,I2)
!      WRITE (scr,2022) T1,P1,NG1,AS1(1:I1),T2,P2,NG2,AS2(1:I2)
!      WRITE (40,2022) T1,P1,NG1,AS1(1:I1),T2,P2,NG2,AS2(1:I2)
! 2022 FORMAT (' T1,P1,NG1 = ',3(2X,1PE15.8),2X,A/
!     >' T2,P2,NG2 = ',3(2X,1PE15.8),2X,A)
      CALL MAKELAYER(T2,P2)
!      CALL PRTGAR(TIMTOT)
      CALL PRTLAY(TIMTOT)
      IF (NLAY(1).GT.1) THEN
      CALL NEXTDIFTIM(T2,P2,TIMLADI,DIFTIM)
      CALL DIFFUS(T2,P2,DIFTIM,OONR)
      TIMLADI=TIMLADI+DIFTIM
      TIMTOT=TIMTOT+DIFTIM
      DIFFCODE=1
      CALL PRTGAR(TIMTOT)
      CALL PRTNOD(TIMTOT)
      ELSE
      IF (NLAY(1).EQ.1) CALL PRTNOD(TIMTOT)
      CALL NEXTDIFTIM(T2,P2,TIMLADI,DIFTIM)
      TIMLADI=TIMLADI+DIFTIM
      END IF
      T2=TUP
      P2=PUP
      AS2=ASUP
      NG2=0.0D0
      MODUS='STEP'
      GOTO 888
      END IF
      END IF
!===== end modus='ass'
!===== modus='gr1l'
      IF (MODUS.EQ.'GR1L') THEN
      WRITE (scr,FMT='(/'' --> grow: make layer'')')
      WRITE (out,FMT='(/'' --> grow: make layer'')')
!      WRITE (UNIT=40,FMT='(/'' --> gr1l: make layer'')')
!      CALL LABLA(AS1,I1)
!      CALL LABLA(AS2,I2)
!      WRITE (scr,2024) T1,P1,NG1,AS1(1:I1),T2,P2,NG2,AS2(1:I2)
!      WRITE (40,2024) T1,P1,NG1,AS1(1:I1),T2,P2,NG2,AS2(1:I2)
! 2024 FORMAT (' T1,P1,NG1 = ',3(2X,1PE15.8),2X,A/
!     >' T2,P2,NG2 = ',3(2X,1PE15.8),2X,A)
      CALL MAKELAYER(T2,P2)
!      CALL PRTGAR(TIMTOT)
      CALL PRTLAY(TIMTOT)
      IF (NLAY(1).GT.1) THEN
      CALL NEXTDIFTIM(T2,P2,TIMLADI,DIFTIM)
      CALL DIFFUS(T2,P2,DIFTIM,OONR)
      TIMLADI=TIMLADI+DIFTIM
      TIMTOT=TIMTOT+DIFTIM
      DIFFCODE=1
      CALL PRTGAR(TIMTOT)
      CALL PRTNOD(TIMTOT)
      ELSE
      IF (NLAY(1).EQ.1) CALL PRTNOD(TIMTOT)
      CALL PRTGAR(TIMTOT)
      CALL NEXTDIFTIM(T2,P2,TIMLADI,DIFTIM)
      TIMLADI=TIMLADI+DIFTIM
      END IF
      MODUS='STEP'
      GOTO 888
      END IF
!===== end modus='gr1l'
      END IF
!----- end if assemblage 1 EQ assemblage 2
!=====
!===== make new step
  888 CONTINUE
      T1=T2
      P1=P2
      AS1=AS2
      NG1=NG2
!C      T2=T1+TINCR
!C      P2=P1+PINCR
!C      IF (T2.GT.TEND) GOTO 999
      CALL NEXTSTEP(T1,P1,TNEW,PNEW,FER)
      IF (FER.EQ.1) GOTO 999
      T2=TNEW
      P2=PNEW
!---
      TC=T2
      P=P2
      WRITE (scr,2030) TC,P
      WRITE (out,2030) TC,P
 2030 FORMAT (/,' --> next step',3X,'T = ',F10.4,3X,'P = ',F11.3)
!      WRITE (UNIT=40,FMT='(/'' --> next step'')')
      CALL PRTBUL
      CALL NURVONPT
      CALL CALSTR
      CALL MAKEOX
      CALL THERIA
      CALL PRTSHORT
      AS2=CURAS
      NG2=VOLGAR
      MODUS='STEP'
      GOTO 1
!=====
!===== end
  999 CONTINUE

!=====

!mar2015
      WRITE(UNIT=6,FMT='(''exit GROWG'')')
      WRITE(UNIT=10,FMT='(''exit GROWG'')')

      RETURN
      END
!-----
!********************************
      SUBROUTINE NEXTSTEP(TX,PX,TNEW,PNEW,FER)
      IMPLICIT NONE
      INTEGER*4 NSEG,SEGM
      REAL*8 TSEG(50),PSEG(50),TIMSEG(50)
      COMMON /SEGMI/ NSEG,SEGM
      COMMON /SEGMR/ TSEG,PSEG,TIMSEG
!----- end of stepping Common variables
      REAL*8 RJ,RCAL,PI,DMNZERO,AEMN,AVMN,DFEZERO,AEFE,AVFE, &
      DMGZERO,AEMG,AVMG
      COMMON /GLODA/ RJ,RCAL,PI,DMNZERO,AEMN,AVMN,DFEZERO,AEFE,AVFE, &
      DMGZERO,AEMG,AVMG
!----- end of GLOBAL DATA Common variables
      INTEGER*4 FER
      REAL*8 F1,F2,DF1,DF2,XSTEP,DXT,DXP,TX,PX,TNEW,PNEW
!---

!mar2015
      WRITE(UNIT=6,FMT='(''enter NEXTSTEP'')')
      WRITE(UNIT=10,FMT='(''enter NEXTSTEP'')')

      IF (TX.EQ.TSEG(SEGM+1).AND.PX.EQ.PSEG(SEGM+1)) THEN
      SEGM=SEGM+1
      IF (SEGM.GT.NSEG-1) FER=1
      END IF
!----
      F1=TSEG(SEGM+1)-TSEG(SEGM)
      F2=(PSEG(SEGM+1)-PSEG(SEGM))
      DF1=DABS(F1)
      DF2=DABS(F2/20.0D0)
      XSTEP=DF1
      IF (DF2.GT.DF1) XSTEP=DF2
      DXT=F1/XSTEP
      DXP=F2/XSTEP
      TNEW=TX+DXT
      PNEW=PX+DXP
!      WRITE (6,1000) SEGM,TX,PX,TNEW,PNEW
!      WRITE (40,1000) SEGM,TX,PX,TNEW,PNEW
! 1000 FORMAT (/' find next step: segm,t,p =',I4,4(2X,1PE15.8))
!-----
      IF ((TNEW-TSEG(SEGM))*(TSEG(SEGM+1)-TNEW).LT.0.0D0 &
      .OR.(PNEW-PSEG(SEGM))*(PSEG(SEGM+1)-PNEW).LT.0.0D0) THEN
      TNEW=TSEG(SEGM+1)
      PNEW=PSEG(SEGM+1)
!C      SEGM=SEGM+1
!      WRITE (6,1005) SEGM,TX,PX,TNEW,PNEW
!      WRITE (40,1005) SEGM,TX,PX,TNEW,PNEW
! 1005 FORMAT (' new segment     : segm,t,p =',I4,4(2X,1PE15.8))
      END IF
!=====

!mar2015
      WRITE(UNIT=6,FMT='(''exit NEXTSTEP'')')
      WRITE(UNIT=10,FMT='(''exit NEXTSTEP'')')

      RETURN
      END
!-----
!********************************
      SUBROUTINE NEXTDIFTIM(TX,PX,TIMLADI,DIFTIM)
      IMPLICIT NONE
      INTEGER*4 NSEG,SEGM
      REAL*8 TSEG(50),PSEG(50),TIMSEG(50)
      COMMON /SEGMI/ NSEG,SEGM
      COMMON /SEGMR/ TSEG,PSEG,TIMSEG
!----- end of stepping Common variables
      REAL*8 RJ,RCAL,PI,DMNZERO,AEMN,AVMN,DFEZERO,AEFE,AVFE, &
      DMGZERO,AEMG,AVMG
      COMMON /GLODA/ RJ,RCAL,PI,DMNZERO,AEMN,AVMN,DFEZERO,AEFE,AVFE, &
      DMGZERO,AEMG,AVMG
!----- end of GLOBAL DATA Common variables
      REAL*8 TX,PX,TIMLADI,DIFTIM,DX,DX1,TIMX

!mar2015
      WRITE(UNIT=6,FMT='(''enter NEXTDIFTIM'')')
      WRITE(UNIT=10,FMT='(''enter NEXTDIFTIM'')')

      IF (SEGM.GT.NSEG-1) THEN
      DIFTIM=TIMSEG(SEGM)-TIMLADI

!mar2015
      WRITE(UNIT=6,FMT='(''exit1 NEXTDIFTIM'')')
      WRITE(UNIT=10,FMT='(''exit1 NEXTDIFTIM'')')

      RETURN
      END IF
!---
      DX=(TSEG(SEGM+1)-TSEG(SEGM))**2+(PSEG(SEGM+1)-PSEG(SEGM))**2
      DX1=(TX-TSEG(SEGM))**2+(PX-PSEG(SEGM))**2
      DX=DSQRT(DX)
      DX1=DSQRT(DX1)
      TIMX=(DX1/DX)*(TIMSEG(SEGM+1)-TIMSEG(SEGM))+TIMSEG(SEGM)
      DIFTIM=TIMX-TIMLADI
!-----
!      WRITE (6,2000) TSEG(SEGM+1),TSEG(SEGM),PSEG(SEGM+1),PSEG(SEGM)
!      WRITE (40,2000) TSEG(SEGM+1),TSEG(SEGM),PSEG(SEGM+1),PSEG(SEGM)
! 2000 FORMAT (/' t1,t2,p1,p2:',4(2X,1PE15.8))
!      WRITE (6,2002) DX,DX1
!      WRITE (40,2002) DX,DX1
! 2002 FORMAT (' dx,dx1     :',2(2X,1PE15.8))
      IF (DIFTIM.LT.0.0D0) DIFTIM=0.0D0
!      WRITE (6,1000) TX,PX,TIMX,DIFTIM
!      WRITE (40,1000) TX,PX,TIMX,DIFTIM
! 1000 FORMAT (/' T,P =',2(2X,1PE15.8)/
!     >' total time since zero:      ',1PE15.8/
!     >' calculated diffusion time:  ',1PE15.8)
!=====

!mar2015
      WRITE(UNIT=6,FMT='(''exit2 NEXTDIFTIM'')')
      WRITE(UNIT=10,FMT='(''exit2 NEXTDIFTIM'')')

      RETURN
      END
!-----
!********************************
      SUBROUTINE CHECKASS(T1,P1,AS1,T2,P2,AS2,TLO,PLO,NGLO, &
      TUP,PUP,ASUP)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!-----
      INTEGER*4 MAXLAY,MAXGEN
      PARAMETER (MAXLAY=400,maxgen=50)
!-----END OF COMMON VARIABLES
      REAL*8 VOL1M(MAXGEN),GARNET(MAXGEN,4,MAXLAY),VLAY(MAXGEN,MAXLAY), &
      DLAY(MAXGEN,MAXLAY),XLAY(MAXGEN,MAXLAY),MOLAY(MAXGEN,MAXLAY), &
      TLAY(MAXGEN,MAXLAY),PLAY(MAXGEN,MAXLAY),VLAYTOT(MAXGEN), &
      MOLAYTOT(MAXGEN),VOLGAR,RAD0,NGARCCM(MAXGEN)
      INTEGER*4 NLAY(MAXGEN),NGARTOT(MAXGEN),NGENTOT, &
      NGEN,NCURAS,CURASNR,LAYASNR(MAXGEN,MAXLAY)
      CHARACTER*125 CURAS,LAYAS(MAXGEN,MAXLAY),CURASSES(MAXLAY)
      COMMON /LAYER/ VOL1M,GARNET,VLAY,DLAY,XLAY,MOLAY,TLAY,PLAY, &
      VLAYTOT,MOLAYTOT,VOLGAR,RAD0
      COMMON /LAYEI/ NLAY,NGARTOT,NGARCCM,NGENTOT,NGEN,NCURAS, &
      CURASNR,LAYASNR
      COMMON /LAYEC/ CURAS,LAYAS,CURASSES
!-----END OF LAYER COMMON VARIABLES
      REAL*8 RJ,RCAL,PI,DMNZERO,AEMN,AVMN,DFEZERO,AEFE,AVFE, &
      DMGZERO,AEMG,AVMG
      COMMON /GLODA/ RJ,RCAL,PI,DMNZERO,AEMN,AVMN,DFEZERO,AEFE,AVFE, &
      DMGZERO,AEMG,AVMG
!----- end of GLOBAL DATA Common variables
      REAL*8 T1,T2,P1,P2,TOB,POB,TUN,PUN,TLO,PLO,TUP,PUP,NGLO
      CHARACTER*125 AS1,AS2,ASUP
      INTEGER*4 I1
!-----
!-----search for same assemblage
      TUN=T1
      PUN=P1
      TOB=T2
      POB=P2
      ASUP=AS2
      NGLO=0.0D0
   10 IF (DABS(TOB-TUN).LT.1D-4.AND.DABS(POB-PUN).LT.1D-2) GOTO 11
!-----Druck ebenso anpassen!
      TC=(TUN+TOB)/2.0D0
      P=(PUN+POB)/2.0D0
      CALL NURVONPT
      CALL CALSTR
      CALL MAKEOX
      CALL THERIA
      CALL PRTSHORT
      CALL LABLA(CURAS,I1)
      IF (CURAS.NE.AS1) THEN
      WRITE (scr,1006) TC,P,CURAS(1:I1)
      WRITE (out,1006) TC,P,CURAS(1:I1)
 1006 FORMAT (' upper limit',5X,'T = ',F10.4,3X,'P = ',F11.3, &
      4X,'assemblage = ',A)
      ASUP=CURAS
      TOB=TC
      POB=P
      ELSE
      WRITE (scr,1008) TC,P,CURAS(1:I1)
      WRITE (out,1008) TC,P,CURAS(1:I1)
 1008 FORMAT (' lower limit',5X,'T = ',F10.4,3X,'P = ',F11.3, &
      4X,'assemblage = ',A)
      NGLO=VOLGAR
      TUN=TC
      PUN=P
      END IF
      GOTO 10
   11 CONTINUE
      TLO=TUN
      PLO=PUN
      TUP=TOB
      PUP=POB
!-----
      RETURN
      END
!-----
!********************************
      SUBROUTINE GROW1L(T1,P1,AS1,T2,P2,VAIM,TNEW,PNEW,ASNEW,NGNEW)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      INCLUDE 'files.cmn'
!-----
      INTEGER*4 MAXLAY,MAXGEN
      PARAMETER (MAXLAY=400,maxgen=50)
!-----END OF COMMON VARIABLES
      REAL*8 VOL1M(MAXGEN),GARNET(MAXGEN,4,MAXLAY),VLAY(MAXGEN,MAXLAY), &
      DLAY(MAXGEN,MAXLAY),XLAY(MAXGEN,MAXLAY),MOLAY(MAXGEN,MAXLAY), &
      TLAY(MAXGEN,MAXLAY),PLAY(MAXGEN,MAXLAY),VLAYTOT(MAXGEN), &
      MOLAYTOT(MAXGEN),VOLGAR,RAD0,NGARCCM(MAXGEN)
      INTEGER*4 NLAY(MAXGEN),NGARTOT(MAXGEN),NGENTOT, &
      NGEN,NCURAS,CURASNR,LAYASNR(MAXGEN,MAXLAY)
      CHARACTER*125 CURAS,LAYAS(MAXGEN,MAXLAY),CURASSES(MAXLAY)
      COMMON /LAYER/ VOL1M,GARNET,VLAY,DLAY,XLAY,MOLAY,TLAY,PLAY, &
      VLAYTOT,MOLAYTOT,VOLGAR,RAD0
      COMMON /LAYEI/ NLAY,NGARTOT,NGARCCM,NGENTOT,NGEN,NCURAS, &
      CURASNR,LAYASNR
      COMMON /LAYEC/ CURAS,LAYAS,CURASSES
!-----END OF LAYER COMMON VARIABLES
      REAL*8 RJ,RCAL,PI,DMNZERO,AEMN,AVMN,DFEZERO,AEFE,AVFE, &
      DMGZERO,AEMG,AVMG
      COMMON /GLODA/ RJ,RCAL,PI,DMNZERO,AEMN,AVMN,DFEZERO,AEFE,AVFE, &
      DMGZERO,AEMG,AVMG
!----- end of GLOBAL DATA Common variables
      REAL*8 T1,T2,P1,P2, &
      VAIM,TOB,TUN,POB,PUN,TNEW,PNEW, &
      VOB,VUN,NGNEW
      CHARACTER*125 ASNEW,AS1
!----

!mar2015
      WRITE(UNIT=6,FMT='(''enter GROW1L'')')
      WRITE(UNIT=10,FMT='(''enter GROW1L'')')

!-----search for layer growth
      TUN=T1
      PUN=P1
      TOB=T2
      POB=P2
      VOB=VAIM
      VUN=0.0D0
   10 IF (DABS((VOB-VUN)/VAIM).LT.1D-4.OR.DABS(TOB-TUN).LT.1D-4) &
       GOTO 11
      TC=(TUN+TOB)/2.0D0
      P=(PUN+POB)/2.0D0
      CALL NURVONPT
      CALL CALSTR
      CALL MAKEOX
      CALL THERIA
      CALL PRTSHORT
      IF (CURAS.NE.AS1) THEN
      TNEW=TC
      PNEW=P
      ASNEW=CURAS
      NGNEW=VOLGAR
      RETURN
      END IF
!---
      IF (VOLGAR.GT.VAIM) THEN
      VOB=VOLGAR
      WRITE (scr,1006) TC,P,VAIM,VOLGAR
      WRITE (out,1006) TC,P,VAIM,VOLGAR
 1006 FORMAT (' upper limit',5X,'T = ',F10.4,3X,'P = ',F11.3, &
      4X,'aim= ',1PE12.5,2X,'vol= ',1PE12.5)
      TOB=TC
      POB=P
      ELSE
      VUN=VOLGAR
      WRITE (scr,1008) TC,P,VAIM,VOLGAR
      WRITE (out,1008) TC,P,VAIM,VOLGAR
 1008 FORMAT (' lower limit',5X,'T = ',F10.4,3X,'P = ',F11.3, &
      4X,'aim= ',1PE12.5,2X,'vol= ',1PE12.5)
      TUN=TC
      PUN=P
      END IF
      GOTO 10
   11 CONTINUE
      TC=(TUN+TOB)/2.0D0
      P=(PUN+POB)/2.0D0
      CALL NURVONPT
      CALL CALSTR
      CALL MAKEOX
      CALL THERIA
      CALL PRTSHORT
      IF (CURAS.NE.AS1) THEN
      TNEW=TC
      PNEW=P
      ASNEW=CURAS
      NGNEW=VOLGAR
      RETURN
      END IF
      TNEW=TC
      PNEW=P
      ASNEW=CURAS
      NGNEW=VOLGAR
      WRITE (UNIT=scr,FMT='(/'' new layer'')')
      WRITE (UNIT=out,FMT='(/'' new layer'')')
!++++

!mar2015
      WRITE(UNIT=6,FMT='(''exit GROW1L'')')
      WRITE(UNIT=10,FMT='(''exit GROW1L'')')

      RETURN
      END
!-----
!********************************
      SUBROUTINE JUSTFORA(TX,PX)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!-----
      REAL*8 TX,PX
      TC=TX
      P=PX
      PRTLOG(9)=.TRUE.
      PRTLOG(6)=.TRUE.
      CALL NURVONPT
      CALL CALSTR
      CALL MAKEOX
      CALL THERIA
      CALL PRTSHORT
      PRTLOG(9)=.FALSE.
      PRTLOG(6)=.FALSE.
      CALL PRTTBL
!++++
      RETURN
      END
!-----
!********************************
      SUBROUTINE MAKELAYER(TX,PX)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!-----
      INTEGER*4 MAXLAY,MAXGEN
      PARAMETER (MAXLAY=400,maxgen=50)
!-----END OF COMMON VARIABLES
      REAL*8 VOL1M(MAXGEN),GARNET(MAXGEN,4,MAXLAY),VLAY(MAXGEN,MAXLAY), &
      DLAY(MAXGEN,MAXLAY),XLAY(MAXGEN,MAXLAY),MOLAY(MAXGEN,MAXLAY), &
      TLAY(MAXGEN,MAXLAY),PLAY(MAXGEN,MAXLAY),VLAYTOT(MAXGEN), &
      MOLAYTOT(MAXGEN),VOLGAR,RAD0,NGARCCM(MAXGEN)
      INTEGER*4 NLAY(MAXGEN),NGARTOT(MAXGEN),NGENTOT, &
      NGEN,NCURAS,CURASNR,LAYASNR(MAXGEN,MAXLAY)
      CHARACTER*125 CURAS,LAYAS(MAXGEN,MAXLAY),CURASSES(MAXLAY)
      COMMON /LAYER/ VOL1M,GARNET,VLAY,DLAY,XLAY,MOLAY,TLAY,PLAY, &
      VLAYTOT,MOLAYTOT,VOLGAR,RAD0
      COMMON /LAYEI/ NLAY,NGARTOT,NGARCCM,NGENTOT,NGEN,NCURAS, &
      CURASNR,LAYASNR
      COMMON /LAYEC/ CURAS,LAYAS,CURASSES
!-----END OF LAYER COMMON VARIABLES
      REAL*8 ORIGBUL(COMAX),GROWBUL(COMAX),BACKBUL(COMAX)
      COMMON /BULLR/ ORIGBUL,GROWBUL,BACKBUL
!-----END OF BULK COMMON VARIABLES
      REAL*8 VMN(MAXGEN,MAXLAY),VFE(MAXGEN,MAXLAY),VMG(MAXGEN,MAXLAY), &
      VCA(MAXGEN,MAXLAY),XXC(MAXGEN,MAXLAY),NODIST, &
      DRVHR(MAXGEN,MAXLAY),XHR(MAXGEN,MAXLAY),CVHR(MAXGEN,3*MAXLAY), &
      VNOD(MAXGEN,MAXLAY),MONOD(MAXGEN,MAXLAY),VNODTOT(MAXGEN), &
      MONODTOT(MAXGEN),MOSPELAST,MOALMLAST,MOPYRLAST,MOGROLAST, &
      BACKSPE,BACKALM,BACKPYR,BACKGRO,MNGENLA(MAXGEN),FEGENLA(MAXGEN), &
      MGGENLA(MAXGEN),CAGENLA(MAXGEN)
      INTEGER*4 NNOD(MAXGEN),NRHR(MAXGEN),DIFFCODE,DFCODE
      COMMON /PROFIR/ VMN,VFE,VMG,VCA,XXC,NODIST,DRVHR,CVHR,XHR, &
      VNOD,MONOD,VNODTOT,MONODTOT,MOSPELAST,MOALMLAST,MOPYRLAST, &
      MOGROLAST,BACKSPE,BACKALM,BACKPYR,BACKGRO,MNGENLA,FEGENLA, &
      MGGENLA,CAGENLA
      COMMON /PROFII/ NNOD,NRHR,DIFFCODE,DFCODE
!----- end of Profile Common variables
      REAL*8 RJ,RCAL,PI,DMNZERO,AEMN,AVMN,DFEZERO,AEFE,AVFE, &
      DMGZERO,AEMG,AVMG
      COMMON /GLODA/ RJ,RCAL,PI,DMNZERO,AEMN,AVMN,DFEZERO,AEFE,AVFE, &
      DMGZERO,AEMG,AVMG
!----- end of GLOBAL DATA Common variables
!-----
      REAL*8 TOADD(COMAX),FF,F1, &
      TX,PX, &
      FF1,FF2,FF3,FF4,VALLG,VGEN(MAXGEN)
      INTEGER*4 I,IC,IP,IS,IG
      CHARACTER*16 TEXT
!----- eventuell testen ob entmischung???

!mar2015
      WRITE(UNIT=6,FMT='(''enter MAKELAYER'')')
      WRITE(UNIT=10,FMT='(''enter MAKELAYER'')')

!-----
      TC=TX
      P=PX
      PRTLOG(9)=.TRUE.
      PRTLOG(6)=.TRUE.
      CALL PRTBUL
      CALL NURVONPT
      CALL CALSTR
      CALL MAKEOX
      CALL THERIA
      CALL PRTSHORT
      PRTLOG(9)=.FALSE.
      PRTLOG(6)=.FALSE.
      CALL PRTTBL
!      AS2=CURAS
!      NG2=VOLGAR
!-----
      WRITE (scr,2000) TC,P
      WRITE (out,2000) TC,P
!      WRITE (40,2000) TC,P
 2000 FORMAT ( &
      /' grow layer:   TC = ',F10.3,'  P = ',F10.3, &
      /' ===========')
      IP=0
      DO 700,I=1,NUN2
      IF (NUMMER(I).LE.0) THEN
      TEXT=SOLNAM(EMCODE(I))
      ELSE
      TEXT=NAME(NUMMER(I))
      END IF
      IF (TEXT.EQ.'GARNET') IP=I
  700 CONTINUE
      IF (IP.EQ.0) THEN
      CALL TRENNE(70)
      WRITE (scr,1000)
      WRITE (out,1000)
!      WRITE (40,1000)
 1000 FORMAT (' no new garnet available from last equilibrium')
      RETURN
      END IF
!----
!----
      FF1=0.0D0
      FF2=0.0D0
      FF3=0.0D0
      FF4=0.0D0
      IS=EMCODE(IP)
      DO 710,IC=1,NEND(IS)
      IF (NAME(EM(IS,IC)).EQ.'spessartine') &
      FF1=100.0D0*XEM(IP,IC)
      IF (NAME(EM(IS,IC)).EQ.'almandine') &
      FF2=100.0D0*XEM(IP,IC)
      IF (NAME(EM(IS,IC)).EQ.'pyrope') &
      FF3=100.0D0*XEM(IP,IC)
  710 CONTINUE
!C      GARNET(4,NLAY)=
!C     >100.0D0-GARNET(1,NLAY)-GARNET(2,NLAY)-GARNET(3,NLAY)
      FF4=100.0D0-FF1-FF2-FF3
!-----
!C      VLAY(NLAY)=VOLPH(IP)
!=====

!mar2015
      IF (NLAY(NGEN).GT.0) THEN
      IF (XLAY(NGEN,NLAY(NGEN)).GT.RAD0) NGEN=NGEN+1
      END IF


      IF (NGEN.GT.NGENTOT) NGEN=NGENTOT
!=====
      VALLG=VOLPH(IP)
      CALL CALCGENER(VALLG,VGEN)
!=====
      DO 400,IG=1,NGEN
      NLAY(IG)=NLAY(IG)+1
      GARNET(IG,1,NLAY(IG))=FF1
      GARNET(IG,2,NLAY(IG))=FF2
      GARNET(IG,3,NLAY(IG))=FF3
      GARNET(IG,4,NLAY(IG))=FF4
      VLAY(IG,NLAY(IG))=VGEN(IG)
!-----
      MOLAY(IG,NLAY(IG))=NN(IP)*VGEN(IG)/VALLG
      VLAYTOT(IG)=VLAYTOT(IG)+VLAY(IG,NLAY(IG))
      MOLAYTOT(IG)=MOLAYTOT(IG)+MOLAY(IG,NLAY(IG))
      TLAY(IG,NLAY(IG))=TC
      PLAY(IG,NLAY(IG))=P
      FF=(VLAYTOT(IG)/VOL1M(IG))**(1.0D0/3.0D0)
      XLAY(IG,NLAY(IG))=FF
      IF (NLAY(IG).EQ.1) THEN
      DLAY(IG,1)=FF
      ELSE
      DLAY(IG,NLAY(IG))=FF-XLAY(IG,NLAY(IG)-1)
      END IF
      LAYAS(IG,NLAY(IG))=CURAS
      LAYASNR(IG,NLAY(IG))=CURASNR
!=====
  400 CONTINUE
!=====
      IF (NLAY(1).GT.0) THEN
      DO IC=1,NC
        TOADD(IC)=0.0D0
      END DO
      DO IC=1,NUN
        TOADD(CHMCOD(IC))= &
        TOADD(CHMCOD(IC))-X(IP,IC)*NN(IP)
      END DO
!
!C      DO 725,I=1,NUN
!C  725 GROWBUL(I)=GROWBUL(I)+TOADD(CHMCOD(I))
      DO I=1,NC
        GROWBUL(I)=GROWBUL(I)+TOADD(I)
      END DO
!
      CALL ADDTOBUL(TOADD)
      CALL PRTBUL
      END IF
!=====================================================
!=====================================================
!C      I001=DNINT(DLAY(NLAY)/NODIST)
!C      IF (I001.EQ.0) I001=1
!C      F001=DBLE(I001)
!C      F1=DLAY(NLAY)/F001
!C      DO 730,I=1,I001
!C      NNOD=NNOD+1
!C      VMN(NNOD)=GARNET(1,NLAY)
!C      VFE(NNOD)=GARNET(2,NLAY)
!C      VMG(NNOD)=GARNET(3,NLAY)
!C      VCA(NNOD)=GARNET(4,NLAY)
!C      IF (NNOD.EQ.1) THEN
!C      XXC(1)=F1/2.0D0
!C      ELSE
!C      XXC(NNOD)=XXC(NNOD-1)+F1
!C      END IF
!C  730 CONTINUE
!CC=====
!CC---- redefine garnet with constant node-distances
!CC=====
!C      FF=XLAY(NLAY)
!C      NRHR=NNOD
!C      IF (NNOD.EQ.1) THEN
!C      XHR(1)=XXC(1)
!C      DRVHR(1)=2.0D0*XHR(1)
!C      ELSE
!C      F1=FF/(DBLE(NNOD))
!C      XHR(1)=F1/2.0D0
!C      DRVHR(1)=F1
!C      DO 735,I=2,NRHR
!C      XHR(I)=XHR(I-1)+F1
!C  735 DRVHR(I)=F1
!C      END IF
!CC
!CC?????????????????????? ev vol-gewichtete Interpolation ???????????????
!CC
!CC-----
!C      FMN(1)=VMN(1)
!C      FFE(1)=VFE(1)
!C      FMG(1)=VMG(1)
!C      FCA(1)=VCA(1)
!C      DO 740,IH=2,NRHR
!C      DO 750,I=1,NNOD
!C      IF (XHR(IH).GE.XXC(I-1).AND.XHR(IH).LT.XXC(I)) THEN
!CC-----    formel f(x): F=F2+(X2-X)*(F1-F2)/(X2-X1)
!C      WT1=(XXC(I)-XHR(IH))
!C      WT2=(XXC(I)-XXC(I-1))
!CC-volwt      WT1=(XXC(I)**3-XHR(IH)**3)
!CC-volwt      WT2=(XXC(I)**3-XXC(I-1)**3)
!C      FMN(IH)=VMN(I)+(WT1)*(VMN(I-1)-VMN(I))/WT2
!C      FFE(IH)=VFE(I)+(WT1)*(VFE(I-1)-VFE(I))/WT2
!C      FMG(IH)=VMG(I)+(WT1)*(VMG(I-1)-VMG(I))/WT2
!C      FCA(IH)=100.0D0-FMN(IH)-FFE(IH)-FMG(IH)
!C      GOTO 751
!C      END IF
!C  750 CONTINUE
!C  751 CONTINUE
!C  740 CONTINUE
!C      FMN(NRHR)=VMN(NNOD)
!C      FFE(NRHR)=VFE(NNOD)
!C      FMG(NRHR)=VMG(NNOD)
!C      FCA(NRHR)=VCA(NNOD)
!C      DO 760,I=1,NRHR
!C      XHR(I)=XHR(I)/1D4
!C      DRVHR(I)=DRVHR(I)/1D4
!C      CVHR(I)=FMN(I)
!C      CVHR(I+NRHR)=FFE(I)
!C      CVHR(I+2*NRHR)=FMG(I)
!C  760 CONTINUE
!C
!C
!=====
      DO 420,IG=1,NGEN
      NNOD(IG)=NNOD(IG)+1
      MONOD(IG,NNOD(IG))=MOLAY(IG,NLAY(IG))
      NRHR(IG)=NNOD(IG)
      VMN(IG,NNOD(IG))=GARNET(IG,1,NLAY(IG))
      VFE(IG,NNOD(IG))=GARNET(IG,2,NLAY(IG))
      VMG(IG,NNOD(IG))=GARNET(IG,3,NLAY(IG))
      VCA(IG,NNOD(IG))=GARNET(IG,4,NLAY(IG))
      IF (NNOD(IG).EQ.1) THEN
      DRVHR(IG,1)=DLAY(IG,1)/1D4
      XHR(IG,1)=DRVHR(IG,1)/2.0D0
      XXC(IG,1)=XHR(IG,1)*1D4
      ELSE
      DRVHR(IG,NNOD(IG))=DLAY(IG,NLAY(IG))/1D4
      F1=(DRVHR(IG,NNOD(IG)-1)+DRVHR(IG,NNOD(IG)))/2.0D0
      XHR(IG,NNOD(IG))=XHR(IG,NNOD(IG)-1)+F1
      XXC(IG,NNOD(IG))=XHR(IG,NNOD(IG))*1D4
      END IF
!---
      DO 760,I=1,NRHR(IG)
        CVHR(IG,I)=VMN(IG,I)
        CVHR(IG,I+NRHR(IG))=VFE(IG,I)
        CVHR(IG,I+2*NRHR(IG))=VMG(IG,I)
  760 CONTINUE
      IF (NNOD(IG).EQ.1) THEN
      FF=XHR(IG,1)+(DRVHR(IG,1)/2.0D0)
      VNOD(IG,1)=((4.0D0/3.0D0)*PI*FF**3)*DBLE(NGARTOT(IG))
      VNODTOT(IG)=VNOD(IG,1)
      ELSE
      FF=XHR(IG,NNOD(IG))+(DRVHR(IG,NNOD(IG))/2.0D0)
      F1=((4.0D0/3.0D0)*PI*FF**3)*DBLE(NGARTOT(IG))
      VNOD(IG,NNOD(IG))=F1-VNODTOT(IG)
      VNODTOT(IG)=F1
      END IF
!=====
  420 CONTINUE
!=====
!---- for bookkeeping and printing:
!C      DO 770,I=1,NNOD
!C      FMN(I)=VMN(I)
!C      FFE(I)=VFE(I)
!C      FMG(I)=VMG(I)
!C      FCA(I)=VCA(I)
!CCC      XXC(I)=XHR(I)*1D4
!C  770 CONTINUE
!=====
!C      DO 800,I=1,NNOD
!C      FF=XHR(I)+(DRVHR(I)/2.0D0)
!C      REALVOL(I)=(4.0D0/3.0D0)*PI*FF**3
!C  800 CONTINUE
!C      VNODTOT=REALVOL(NNOD)*DBLE(NGARTOT)
!C      DO 802,I=2,NNOD
!C      VNOD(I)=(REALVOL(I)-REALVOL(I-1))*DBLE(NGARTOT)
!C  802 CONTINUE
!C      VNOD(1)=REALVOL(1)*DBLE(NGARTOT)
!
!----

!mar2015
      WRITE(UNIT=6,FMT='(''exit MAKELAYER'')')
      WRITE(UNIT=10,FMT='(''exit MAKELAYER'')')

      RETURN
      END
!********************************
      SUBROUTINE CALCGENER(VALLG,VGEN)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!-----
      INTEGER*4 MAXLAY,MAXGEN
      PARAMETER (MAXLAY=400,maxgen=50)
!-----END OF COMMON VARIABLES
      REAL*8 VOL1M(MAXGEN),GARNET(MAXGEN,4,MAXLAY),VLAY(MAXGEN,MAXLAY), &
      DLAY(MAXGEN,MAXLAY),XLAY(MAXGEN,MAXLAY),MOLAY(MAXGEN,MAXLAY), &
      TLAY(MAXGEN,MAXLAY),PLAY(MAXGEN,MAXLAY),VLAYTOT(MAXGEN), &
      MOLAYTOT(MAXGEN),VOLGAR,RAD0,NGARCCM(MAXGEN)
      INTEGER*4 NLAY(MAXGEN),NGARTOT(MAXGEN),NGENTOT, &
      NGEN,NCURAS,CURASNR,LAYASNR(MAXGEN,MAXLAY)
      CHARACTER*125 CURAS,LAYAS(MAXGEN,MAXLAY),CURASSES(MAXLAY)
      COMMON /LAYER/ VOL1M,GARNET,VLAY,DLAY,XLAY,MOLAY,TLAY,PLAY, &
      VLAYTOT,MOLAYTOT,VOLGAR,RAD0
      COMMON /LAYEI/ NLAY,NGARTOT,NGARCCM,NGENTOT,NGEN,NCURAS, &
      CURASNR,LAYASNR
      COMMON /LAYEC/ CURAS,LAYAS,CURASSES
!-----END OF LAYER COMMON VARIABLES
      REAL*8 VMN(MAXGEN,MAXLAY),VFE(MAXGEN,MAXLAY),VMG(MAXGEN,MAXLAY), &
      VCA(MAXGEN,MAXLAY),XXC(MAXGEN,MAXLAY),NODIST, &
      DRVHR(MAXGEN,MAXLAY),XHR(MAXGEN,MAXLAY),CVHR(MAXGEN,3*MAXLAY), &
      VNOD(MAXGEN,MAXLAY),MONOD(MAXGEN,MAXLAY),VNODTOT(MAXGEN), &
      MONODTOT(MAXGEN),MOSPELAST,MOALMLAST,MOPYRLAST,MOGROLAST, &
      BACKSPE,BACKALM,BACKPYR,BACKGRO,MNGENLA(MAXGEN),FEGENLA(MAXGEN), &
      MGGENLA(MAXGEN),CAGENLA(MAXGEN)
      INTEGER*4 NNOD(MAXGEN),NRHR(MAXGEN),DIFFCODE,DFCODE
      COMMON /PROFIR/ VMN,VFE,VMG,VCA,XXC,NODIST,DRVHR,CVHR,XHR, &
      VNOD,MONOD,VNODTOT,MONODTOT,MOSPELAST,MOALMLAST,MOPYRLAST, &
      MOGROLAST,BACKSPE,BACKALM,BACKPYR,BACKGRO,MNGENLA,FEGENLA, &
      MGGENLA,CAGENLA
      COMMON /PROFII/ NNOD,NRHR,DIFFCODE,DFCODE
!----- end of Profile Common variables
      REAL*8 RJ,RCAL,PI,DMNZERO,AEMN,AVMN,DFEZERO,AEFE,AVFE, &
      DMGZERO,AEMG,AVMG
      COMMON /GLODA/ RJ,RCAL,PI,DMNZERO,AEMN,AVMN,DFEZERO,AEFE,AVFE, &
      DMGZERO,AEMG,AVMG
!----- end of GLOBAL DATA Common variables
      REAL*8 VALLG,VGEN(MAXGEN),RAD,RAD1,RAD2,VSUM,FF,F1
      INTEGER*4 IG
!-----

!mar2015
      WRITE(UNIT=6,FMT='(''enter CALCGENER'')')
      WRITE(UNIT=10,FMT='(''enter CALCGENER'')')

!      WRITE (6,1000) VALLG
!      WRITE (40,1000) VALLG
! 1000 FORMAT (' search volume distriburion, vtot = ',1PE15.8)
      RAD1=0.0D0
      RAD2=NODIST
      VSUM=0.0D0
!=====
    1 VSUM=0.0D0
      DO 520,IG=1,NGEN
      VGEN(IG)=0.0D0
      IF (NLAY(IG).EQ.0) THEN
      VGEN(IG)=VGEN(IG)+(RAD2**3)*VOL1M(IG)
      ELSE
      FF=XLAY(IG,NLAY(IG))+RAD2
      F1=(FF**3)*VOL1M(IG)
      VGEN(IG)=VGEN(IG)+F1-VLAYTOT(IG)
      END IF
      VSUM=VSUM+VGEN(IG)
  520 CONTINUE
!C      WRITE (6,1002) RAD2,VSUM
!C      WRITE (40,1002) RAD2,VSUM
!C 1002 FORMAT (' too small, R,Vsum =',2(2X,1PE15.8))
      IF (VSUM.LT.VALLG) THEN
      RAD1=RAD2
      RAD2=RAD2+NODIST
      GOTO 1
      END IF
!=====
    2 IF ((RAD2-RAD1).LT.1D-6) GOTO 888
      RAD=(RAD1+RAD2)/2.0D0
      VSUM=0.0D0
      DO 530,IG=1,NGEN
      VGEN(IG)=0.0D0
      IF (NLAY(IG).EQ.0) THEN
      VGEN(IG)=VGEN(IG)+(RAD**3)*VOL1M(IG)
      ELSE
      FF=XLAY(IG,NLAY(IG))+RAD
      F1=(FF**3)*VOL1M(IG)
      VGEN(IG)=VGEN(IG)+F1-VLAYTOT(IG)
      END IF
      VSUM=VSUM+VGEN(IG)
  530 CONTINUE
!C      WRITE (6,1004) RAD,VSUM
!C      WRITE (40,1004) RAD,VSUM
!C 1004 FORMAT (' new R,Vsum =',2(2X,1PE15.8))
      IF (VSUM.LT.VALLG) THEN
      RAD1=RAD
      ELSE
      RAD2=RAD
      END IF
      GOTO 2
!----
  888 CONTINUE

!mar2015
      WRITE(UNIT=6,FMT='(''exit CALCGENER'')')
      WRITE(UNIT=10,FMT='(''exit CALCGENER'')')

      RETURN
      END
!-----
!********************************
      SUBROUTINE PRTNOD(TIMTOT)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!-----
      INTEGER*4 MAXLAY,MAXGEN
      PARAMETER (MAXLAY=400,maxgen=50)
!-----END OF COMMON VARIABLES
      REAL*8 VOL1M(MAXGEN),GARNET(MAXGEN,4,MAXLAY),VLAY(MAXGEN,MAXLAY), &
      DLAY(MAXGEN,MAXLAY),XLAY(MAXGEN,MAXLAY),MOLAY(MAXGEN,MAXLAY), &
      TLAY(MAXGEN,MAXLAY),PLAY(MAXGEN,MAXLAY),VLAYTOT(MAXGEN), &
      MOLAYTOT(MAXGEN),VOLGAR,RAD0,NGARCCM(MAXGEN)
      INTEGER*4 NLAY(MAXGEN),NGARTOT(MAXGEN),NGENTOT, &
      NGEN,NCURAS,CURASNR,LAYASNR(MAXGEN,MAXLAY)
      CHARACTER*125 CURAS,LAYAS(MAXGEN,MAXLAY),CURASSES(MAXLAY)
      COMMON /LAYER/ VOL1M,GARNET,VLAY,DLAY,XLAY,MOLAY,TLAY,PLAY, &
      VLAYTOT,MOLAYTOT,VOLGAR,RAD0
      COMMON /LAYEI/ NLAY,NGARTOT,NGARCCM,NGENTOT,NGEN,NCURAS, &
      CURASNR,LAYASNR
      COMMON /LAYEC/ CURAS,LAYAS,CURASSES
!-----END OF LAYER COMMON VARIABLES
      REAL*8 VMN(MAXGEN,MAXLAY),VFE(MAXGEN,MAXLAY),VMG(MAXGEN,MAXLAY), &
      VCA(MAXGEN,MAXLAY),XXC(MAXGEN,MAXLAY),NODIST, &
      DRVHR(MAXGEN,MAXLAY),XHR(MAXGEN,MAXLAY),CVHR(MAXGEN,3*MAXLAY), &
      VNOD(MAXGEN,MAXLAY),MONOD(MAXGEN,MAXLAY),VNODTOT(MAXGEN), &
      MONODTOT(MAXGEN),MOSPELAST,MOALMLAST,MOPYRLAST,MOGROLAST, &
      BACKSPE,BACKALM,BACKPYR,BACKGRO,MNGENLA(MAXGEN),FEGENLA(MAXGEN), &
      MGGENLA(MAXGEN),CAGENLA(MAXGEN)
      INTEGER*4 NNOD(MAXGEN),NRHR(MAXGEN),DIFFCODE,DFCODE
      COMMON /PROFIR/ VMN,VFE,VMG,VCA,XXC,NODIST,DRVHR,CVHR,XHR, &
      VNOD,MONOD,VNODTOT,MONODTOT,MOSPELAST,MOALMLAST,MOPYRLAST, &
      MOGROLAST,BACKSPE,BACKALM,BACKPYR,BACKGRO,MNGENLA,FEGENLA, &
      MGGENLA,CAGENLA
      COMMON /PROFII/ NNOD,NRHR,DIFFCODE,DFCODE
!----- end of Profile Common variables
      REAL*8 ORIGBUL(COMAX),GROWBUL(COMAX),BACKBUL(COMAX)
      COMMON /BULLR/ ORIGBUL,GROWBUL,BACKBUL
!-----END OF BULK COMMON VARIABLES
      INTEGER*4 N00X(MAXGEN)
      COMMON /GENIN/ N00X
!----- end of genprint Common variables
      REAL*8 RJ,RCAL,PI,DMNZERO,AEMN,AVMN,DFEZERO,AEFE,AVFE, &
      DMGZERO,AEMG,AVMG
      COMMON /GLODA/ RJ,RCAL,PI,DMNZERO,AEMN,AVMN,DFEZERO,AEFE,AVFE, &
      DMGZERO,AEMG,AVMG
!----- end of GLOBAL DATA Common variables
      INTEGER*4 I,IG,I1,I2,J
      REAL*8 TIMTOT,FF
      CHARACTER*20 FINA,FINA2
      CHARACTER*500 BUNA,BUNA2,BUNA3,BUNA4,ZTHERIN
!-----

!mar2015
      WRITE(UNIT=6,FMT='(''enter PRTNOD'')')
      WRITE(UNIT=10,FMT='(''enter PRTNOD'')')

!=====
!      DO 400,IG=1,NGEN
!=====
!      DO 285,I=1,NNOD(IG)
!      WRITE (42,3005) TC,P,TIMTOT,XXC(IG,I),VMN(IG,I),
!     >VFE(IG,I),VMG(IG,I),VCA(IG,I)
! 3005 FORMAT ('1  ',8(2X,1PE15.8))
!  285 CONTINUE
!=====
!  400 CONTINUE
!=====
!C      WRITE (43,3006) TC,P,(BULK(I),I=1,NUN)
!C 3006 FORMAT ('1  ',100(2X,1PE15.8))
!=====
      BUNA=' '
      BUNA2=' '
      BUNA3=' '
      ZTHERIN=' '
      IF (N00X(1).EQ.0) THEN
      OPEN (UNIT=43,FILE='garnet_bulk.txt',STATUS='UNKNOWN')
      OPEN (UNIT=45,FILE='garnet_bulktable.txt',STATUS='UNKNOWN')
      OPEN (UNIT=46,FILE='garnet_bulktherin.txt',STATUS='UNKNOWN')
      OPEN (UNIT=47,FILE='garnet_times.txt',STATUS='UNKNOWN')
      WRITE (43,2000)
 2000 FORMAT ('-----------------------------------------------', &
      '-----------------------------------------------', &
      '-----------------'/ &
      'THERIA_G - Bulk composition of matrix after fractionation ', &
      'of garnet and diffusional relaxation. "tot.time (my)" ',/ &
      'is the time that passed by after the first ', &
      'garnet shell was grown in the modelled system.'/ &
      '-----------------------------------------------', &
      '-----------------------------------------------', &
      '-----------------')
      WRITE (UNIT=BUNA,FMT='(A)') ' tot.time (my)'
      WRITE (UNIT=BUNA2,FMT='(A)') 'Time(my)'
      WRITE (UNIT=BUNA4,FMT='(A)') 'Time(my)  ,T(C)  ,P(Bar)'
      I1=19
      DO 400,I=1,NC
      IF (ORIGBUL(I).NE.0.0D0) THEN
       WRITE (UNIT=BUNA(I1:),FMT='(A)') OXYDE(I)
       WRITE (UNIT=BUNA2(I1:),FMT='('',n('',A)') OXYDE(I)
       CALL LABLA(BUNA2,I2)
       BUNA2(I2+1:I2+1)=')'
       I1=I1+18
      END IF
  400 CONTINUE
      I1=25
      DO I=1,NGENTOT
       WRITE (UNIT=BUNA4(I1:),FMT='(''  ,r'',I3.3,''  ,v'',i3.3)') I,I
       I1=I1+16
      END DO
      WRITE (UNIT=BUNA4(I1:),FMT='(''  ,assemblage'')')
      CALL PUST(43,BUNA)
      CALL PUST(45,BUNA2)
      CALL PUST(47,BUNA4)
!--
      WRITE (UNIT=BUNA,FMT='(A)') ' initial'
      WRITE (UNIT=BUNA2,FMT='(A)') '-1D-10'
      WRITE (UNIT=BUNA4,FMT='(1PE15.8)') TIMTOT
      I1=18
      DO 402,I=1,NC
      IF (ORIGBUL(I).NE.0.0D0) THEN
       WRITE (UNIT=BUNA(I1:),FMT='(1PE15.8)') ORIGBUL(I)
       WRITE (UNIT=BUNA2(I1:),FMT='('','',1PE15.8)') ORIGBUL(I)
       CALL LABLA(ZTHERIN,I2)
       WRITE (UNIT=ZTHERIN(I2+1:),FMT=2005) OXYDE(I),ORIGBUL(I)
 2005  FORMAT (A,'(',1PE15.8,')')
       I1=I1+18
      END IF
  402 CONTINUE
      I1=18
      WRITE (UNIT=BUNA4(I1:),FMT='(2(''  ,'',1PE15.8))') TC,P
      I1=I1+38
      DO IG=1,NGENTOT


!mar2015
       IF (NNOD(IG).GT.0) THEN
       FF=XHR(IG,NNOD(IG))+DRVHR(IG,NNOD(IG))/2.0D0
       WRITE (UNIT=BUNA4(I1:),FMT='(2(''  ,'',1PE15.8))') &
       FF,VLAYTOT(IG)
       I1=I1+38
       END IF


      END DO
      WRITE (UNIT=BUNA4(I1:),FMT='(''  ,'',I4)') CURASNR
      CALL PUST(43,BUNA)
      CALL PUST(45,BUNA2)
      CALL KOLLABIERE(ZTHERIN,J)
      BUNA3='-1.00000000D-10  '//ZTHERIN(1:J)//'    *'
      CALL PUST(46,BUNA3)
      CALL PUST(47,BUNA4)
!----
      ELSE
      OPEN (UNIT=43,FILE='garnet_bulk.txt',STATUS='OLD', &
      ACCESS='APPEND')
      OPEN (UNIT=45,FILE='garnet_bulktable.txt',STATUS='OLD', &
      ACCESS='APPEND')
      OPEN (UNIT=46,FILE='garnet_bulktherin.txt',STATUS='OLD', &
      ACCESS='APPEND')
      OPEN (UNIT=47,FILE='garnet_times.txt',STATUS='OLD', &
      ACCESS='APPEND')
!----
      WRITE (UNIT=BUNA,FMT='(1PE15.8)') TIMTOT
      WRITE (UNIT=BUNA2,FMT='(1PE15.8)') TIMTOT
      WRITE (UNIT=BUNA4,FMT='(1PE15.8)') TIMTOT
      I1=18
      DO 405,I=1,NC
      IF (ORIGBUL(I).NE.0.0D0) THEN
      WRITE (UNIT=BUNA(I1:),FMT='(1PE15.8)') CHEM(I)
      WRITE (UNIT=BUNA2(I1:),FMT='('','',1PE15.8)') CHEM(I)
      CALL LABLA(ZTHERIN,I2)
      WRITE (UNIT=ZTHERIN(I2+1:),FMT=2006) OXYDE(I),CHEM(I)
 2006 FORMAT (A,'(',1PE15.8,')')
      I1=I1+18
      END IF
  405 CONTINUE
      I1=18
      WRITE (UNIT=BUNA4(I1:),FMT='(2(''  ,'',1PE15.8))') TC,P
      I1=I1+38
      DO IG=1,NGENTOT


!mar2015
       IF (NNOD(IG).GT.0) THEN
       FF=XHR(IG,NNOD(IG))+DRVHR(IG,NNOD(IG))/2.0D0
       WRITE (UNIT=BUNA4(I1:),FMT='(2(''  ,'',1PE15.8))') &
       FF,VLAYTOT(IG)
       I1=I1+38
       END IF


      END DO
      WRITE (UNIT=BUNA4(I1:),FMT='(''  ,'',I4)') CURASNR
      CALL PUST(43,BUNA)
      CALL PUST(45,BUNA2)
      CALL KOLLABIERE(ZTHERIN,J)
      BUNA3=BUNA2(1:17)//ZTHERIN(1:J)//'    *'
      CALL PUST(46,BUNA3)
      CALL PUST(47,BUNA4)
!----
      END IF
      CLOSE (UNIT=43)
      CLOSE (UNIT=45)
      CLOSE (UNIT=46)
      CLOSE (UNIT=47)
!=====
!      WRITE (43,3006) TC,P,(CHEM(I),I=1,NC)
! 3006 FORMAT ('1  ',100(2X,1PE15.8))
!----
      DO 500,IG=1,NGEN
      WRITE (UNIT=FINA,FMT='(''garnet_gen'',I3.3,''.txt'')') IG
      CALL LABLA(FINA,I1)
      WRITE (UNIT=FINA2,FMT='(''garnet_gen'',I3.3,''a.txt'')') IG
      CALL LABLA(FINA2,I2)
      IF (N00X(IG).EQ.0) THEN
      OPEN (UNIT=53,FILE=FINA(1:I1),STATUS='UNKNOWN')
      OPEN (UNIT=63,FILE=FINA2(1:I2),STATUS='UNKNOWN')
      WRITE (53,1000)
 1000 FORMAT ('-----------------------------------------------', &
      '-----------------------------------------------', &
      '-----------------'/ &
      'THERIA_G - Compositional profile of a garnet generation', &
      ' (see filename) considering chemical fractionation and '/ &
      'intragranular diffusional relaxation. "tot.time (my)" ', &
      'is the time that passed by after the first'/ &
      'garnet shell was grown in the modelled system.'/ &
      '-----------------------------------------------', &
      '-----------------------------------------------', &
      '-----------------'/ &
      'gen',1X,'shell',2X,'temperature (¡C)',1X,'pressure (bar)', &
      3X,'tot.time (my)', &
      4X,'radius (cm)',6X,'X(node) (cm)',5X,'Xmn',14X,'Xfe', &
      14X,'Xmg',14X,'Xca')
      WRITE (63,1001)
 1001 FORMAT ('gener',5X,',shell',5X,',T(C)',5X,',P(Bar)', &
      5X,',Time(my)', &
      5X,',radius(cm)',5X,',node(cm)',5X,',x(Mn)',5X,',x(Fe)', &
      5X,',x(Mg)',5X,',x(Ca)',5X,',assemblage')
      N00X(IG)=1
      ELSE
      OPEN (UNIT=53,FILE=FINA(1:I1),STATUS='OLD',ACCESS='APPEND')
      OPEN (UNIT=63,FILE=FINA2(1:I2),STATUS='OLD',ACCESS='APPEND')
      END IF
      DO 287,I=1,NNOD(IG)
      FF=XHR(IG,I)+DRVHR(IG,I)/2.0D0
      WRITE (53,1005) IG,I,TC,P,TIMTOT,FF,XHR(IG,I),VMN(IG,I)/100.0D0, &
      VFE(IG,I)/100.0D0,VMG(IG,I)/100.0D0,VCA(IG,I)/100.0D0
 1005 FORMAT (2I4,9(2X,1PE15.8))
      WRITE (63,1006) IG,I,TC,P,TIMTOT,FF,XHR(IG,I),VMN(IG,I)/100.0D0, &
      VFE(IG,I)/100.0D0,VMG(IG,I)/100.0D0,VCA(IG,I)/100.0D0, &
      LAYASNR(IG,I)
 1006 FORMAT (I4,',',I4,9(2X,',',1PE15.8),' ,',I4)
  287 CONTINUE
      CLOSE (UNIT=53)
      CLOSE (UNIT=63)
  500 CONTINUE
!=====

!mar2015
      WRITE(UNIT=6,FMT='(''exit PRTNOD'')')
      WRITE(UNIT=10,FMT='(''exit PRTNOD'')')

      RETURN
      END
!-----
!******************************
      SUBROUTINE KOLLABIERE(CH,J)
      IMPLICIT NONE
      CHARACTER*(*) CH
      CHARACTER*500 CH1
      INTEGER*4 J,I,LAE
      LAE=LEN(CH)
      CH1=' '
      J=0
      DO 500,I=1,LAE
      IF (CH(I:I).NE.' ') THEN
      J=J+1
      CH1(J:J)=CH(I:I)
      END IF
  500 CONTINUE
      CH=CH1(1:J)
      RETURN
      END
!-----
!********************************
      SUBROUTINE PRTLAY(TIMTOT)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!-----
      INTEGER*4 MAXLAY,MAXGEN
      PARAMETER (MAXLAY=400,maxgen=50)
!-----END OF COMMON VARIABLES
      REAL*8 VOL1M(MAXGEN),GARNET(MAXGEN,4,MAXLAY),VLAY(MAXGEN,MAXLAY), &
      DLAY(MAXGEN,MAXLAY),XLAY(MAXGEN,MAXLAY),MOLAY(MAXGEN,MAXLAY), &
      TLAY(MAXGEN,MAXLAY),PLAY(MAXGEN,MAXLAY),VLAYTOT(MAXGEN), &
      MOLAYTOT(MAXGEN),VOLGAR,RAD0,NGARCCM(MAXGEN)
      INTEGER*4 NLAY(MAXGEN),NGARTOT(MAXGEN),NGENTOT, &
      NGEN,NCURAS,CURASNR,LAYASNR(MAXGEN,MAXLAY)
      CHARACTER*125 CURAS,LAYAS(MAXGEN,MAXLAY),CURASSES(MAXLAY)
      COMMON /LAYER/ VOL1M,GARNET,VLAY,DLAY,XLAY,MOLAY,TLAY,PLAY, &
      VLAYTOT,MOLAYTOT,VOLGAR,RAD0
      COMMON /LAYEI/ NLAY,NGARTOT,NGARCCM,NGENTOT,NGEN,NCURAS, &
      CURASNR,LAYASNR
      COMMON /LAYEC/ CURAS,LAYAS,CURASSES
!-----END OF LAYER COMMON VARIABLES
      REAL*8 VMN(MAXGEN,MAXLAY),VFE(MAXGEN,MAXLAY),VMG(MAXGEN,MAXLAY), &
      VCA(MAXGEN,MAXLAY),XXC(MAXGEN,MAXLAY),NODIST, &
      DRVHR(MAXGEN,MAXLAY),XHR(MAXGEN,MAXLAY),CVHR(MAXGEN,3*MAXLAY), &
      VNOD(MAXGEN,MAXLAY),MONOD(MAXGEN,MAXLAY),VNODTOT(MAXGEN), &
      MONODTOT(MAXGEN),MOSPELAST,MOALMLAST,MOPYRLAST,MOGROLAST, &
      BACKSPE,BACKALM,BACKPYR,BACKGRO,MNGENLA(MAXGEN),FEGENLA(MAXGEN), &
      MGGENLA(MAXGEN),CAGENLA(MAXGEN)
      INTEGER*4 NNOD(MAXGEN),NRHR(MAXGEN),DIFFCODE,DFCODE
      COMMON /PROFIR/ VMN,VFE,VMG,VCA,XXC,NODIST,DRVHR,CVHR,XHR, &
      VNOD,MONOD,VNODTOT,MONODTOT,MOSPELAST,MOALMLAST,MOPYRLAST, &
      MOGROLAST,BACKSPE,BACKALM,BACKPYR,BACKGRO,MNGENLA,FEGENLA, &
      MGGENLA,CAGENLA
      COMMON /PROFII/ NNOD,NRHR,DIFFCODE,DFCODE
!----- end of Profile Common variables
      REAL*8 ORIGBUL(COMAX),GROWBUL(COMAX),BACKBUL(COMAX)
      COMMON /BULLR/ ORIGBUL,GROWBUL,BACKBUL
!-----END OF BULK COMMON VARIABLES
      REAL*8 RJ,RCAL,PI,DMNZERO,AEMN,AVMN,DFEZERO,AEFE,AVFE, &
      DMGZERO,AEMG,AVMG
      COMMON /GLODA/ RJ,RCAL,PI,DMNZERO,AEMN,AVMN,DFEZERO,AEFE,AVFE, &
      DMGZERO,AEMG,AVMG
!----- end of GLOBAL DATA Common variables
      INTEGER*4 I1
!      INTEGER*4 I,IG
      REAL*8 TIMTOT
!-----
!=====
!      DO 400,IG=1,NGEN
!=====
!      WRITE (41,3000) TC,P,TIMTOT,XLAY(IG,NLAY(IG)),
!     >GARNET(IG,1,NLAY(IG)),GARNET(IG,2,NLAY(IG)),
!     >GARNET(IG,3,NLAY(IG)),GARNET(IG,4,NLAY(IG))
! 3000 FORMAT (8(2X,1PE15.8))
!      DO 280,I=1,NNOD(IG)
!      WRITE (42,3002) TC,P,TIMTOT,XXC(IG,I),VMN(IG,I),
!     >VFE(IG,I),VMG(IG,I),VCA(IG,I)
! 3002 FORMAT ('0  ',8(2X,1PE15.8))
!  280 CONTINUE
!=====
!  400 CONTINUE
!=====
!C      WRITE (43,3003) TC,P,(BULK(I),I=1,NUN)
!C 3003 FORMAT ('0  ',100(2X,1PE15.8))
!      WRITE (43,3006) TC,P,(CHEM(I),I=1,NC)
! 3006 FORMAT ('0  ',100(2X,1PE15.8))
!=====
      IF (NLAY(1).EQ.1) THEN
      OPEN (UNIT=54,FILE='garnet_frac.txt',STATUS='UNKNOWN')
      WRITE (54,1000)
 1000 FORMAT ('-----------------------------------------------', &
      '-----------------------------------------------', &
      '----------------------'/ &
      'THERIA_G - Compositional profile of the first generation', &
      ' garnet considering chemical fractionation. However, the'/ &
      'bulk composition may be modified by internal metasomatism', &
      ' (Spear, 1988) depending on the effectiveness of diffusion.'/ &
      'Pressure, Temperature, Time  and assemblage are for initial ', &
      'growth.'/ &
      '-----------------------------------------------', &
      '-----------------------------------------------', &
      '----------------------'/ &
      'shell',2X,'temperature (¡C)',1X,'pressure (bar)', &
      3X,'time (my)',8X,'radius (cm)',6X,'Xmn',14X,'Xfe',14X,'Xmg', &
      14X,'Xca',14X,'assemblage')
      ELSE
      OPEN (UNIT=54,FILE='garnet_frac.txt',STATUS='OLD',ACCESS='APPEND')
      END IF
      CALL LABLA(CURAS,I1)
      WRITE (54,1005) NLAY(1),TC,P,TIMTOT,XLAY(1,NLAY(1))/10000, &
      GARNET(1,1,NLAY(1))/100,GARNET(1,2,NLAY(1))/100, &
      GARNET(1,3,NLAY(1))/100,GARNET(1,4,NLAY(1))/100,CURAS(1:I1)
 1005 FORMAT (I4,8(2X,1PE15.8),3X,A)
      CLOSE (UNIT=54)
!----
      RETURN
      END
!-----
!********************************
      SUBROUTINE PRTGAR(TIMTOT)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!-----
      INTEGER*4 MAXLAY,MAXGEN
      PARAMETER (MAXLAY=400,maxgen=50)
!-----END OF COMMON VARIABLES
      REAL*8 VOL1M(MAXGEN),GARNET(MAXGEN,4,MAXLAY),VLAY(MAXGEN,MAXLAY), &
      DLAY(MAXGEN,MAXLAY),XLAY(MAXGEN,MAXLAY),MOLAY(MAXGEN,MAXLAY), &
      TLAY(MAXGEN,MAXLAY),PLAY(MAXGEN,MAXLAY),VLAYTOT(MAXGEN), &
      MOLAYTOT(MAXGEN),VOLGAR,RAD0,NGARCCM(MAXGEN)
      INTEGER*4 NLAY(MAXGEN),NGARTOT(MAXGEN),NGENTOT, &
      NGEN,NCURAS,CURASNR,LAYASNR(MAXGEN,MAXLAY)
      CHARACTER*125 CURAS,LAYAS(MAXGEN,MAXLAY),CURASSES(MAXLAY)
      COMMON /LAYER/ VOL1M,GARNET,VLAY,DLAY,XLAY,MOLAY,TLAY,PLAY, &
      VLAYTOT,MOLAYTOT,VOLGAR,RAD0
      COMMON /LAYEI/ NLAY,NGARTOT,NGARCCM,NGENTOT,NGEN,NCURAS, &
      CURASNR,LAYASNR
      COMMON /LAYEC/ CURAS,LAYAS,CURASSES
!-----END OF LAYER COMMON VARIABLES
      REAL*8 VMN(MAXGEN,MAXLAY),VFE(MAXGEN,MAXLAY),VMG(MAXGEN,MAXLAY), &
      VCA(MAXGEN,MAXLAY),XXC(MAXGEN,MAXLAY),NODIST, &
      DRVHR(MAXGEN,MAXLAY),XHR(MAXGEN,MAXLAY),CVHR(MAXGEN,3*MAXLAY), &
      VNOD(MAXGEN,MAXLAY),MONOD(MAXGEN,MAXLAY),VNODTOT(MAXGEN), &
      MONODTOT(MAXGEN),MOSPELAST,MOALMLAST,MOPYRLAST,MOGROLAST, &
      BACKSPE,BACKALM,BACKPYR,BACKGRO,MNGENLA(MAXGEN),FEGENLA(MAXGEN), &
      MGGENLA(MAXGEN),CAGENLA(MAXGEN)
      INTEGER*4 NNOD(MAXGEN),NRHR(MAXGEN),DIFFCODE,DFCODE
      COMMON /PROFIR/ VMN,VFE,VMG,VCA,XXC,NODIST,DRVHR,CVHR,XHR, &
      VNOD,MONOD,VNODTOT,MONODTOT,MOSPELAST,MOALMLAST,MOPYRLAST, &
      MOGROLAST,BACKSPE,BACKALM,BACKPYR,BACKGRO,MNGENLA,FEGENLA, &
      MGGENLA,CAGENLA
      COMMON /PROFII/ NNOD,NRHR,DIFFCODE,DFCODE
!----- end of Profile Common variables
      REAL*8 ORIGBUL(COMAX),GROWBUL(COMAX),BACKBUL(COMAX)
      COMMON /BULLR/ ORIGBUL,GROWBUL,BACKBUL
!-----END OF BULK COMMON VARIABLES
      REAL*8 RJ,RCAL,PI,DMNZERO,AEMN,AVMN,DFEZERO,AEFE,AVFE, &
      DMGZERO,AEMG,AVMG
      COMMON /GLODA/ RJ,RCAL,PI,DMNZERO,AEMN,AVMN,DFEZERO,AEFE,AVFE, &
      DMGZERO,AEMG,AVMG
!----- end of GLOBAL DATA Common variables
!-----
      REAL*8 TIMTOT,AVSPE,AVGRO,AVALM,AVPYR,MOSPE,MOGRO, &
      MOALM,MOPYR,MOLSPE,MOLALM,MOLPYR,MOLGRO,FF,F1,F2,F3,F4, &
      SUM1,SUM2,SUM3,SUM4,NOBU(4), &
      MNGEN(MAXGEN),FEGEN(MAXGEN),MGGEN(MAXGEN),CAGEN(MAXGEN)
!      REAL*8 VPROMOL,XMUE
      INTEGER*4 I,IG
      CHARACTER*150 CH150
!-----
      IF (NLAY(1).EQ.0) RETURN
!      WRITE (6,1010) TIMTOT,VOL1M(1)
!      WRITE (40,1010) TIMTOT,VOL1M(1)
! 1010 FORMAT (//
!     >'Growth layers'/
!     >'-------------',5X,
!     >' total time (my)=',F15.6,
!     >3X,'1 mue sphere1 (ccm)=',1PE12.5)
!---
      WRITE (scr,1010) TIMTOT
      WRITE (out,1010) TIMTOT
 1010 FORMAT (/ &
      ' Growth layers'/ &
      ' -------------',5X, &
      ' total time (my)=',F15.6)
!=====
      DO 800,IG=1,NGEN
      WRITE (scr,1012) IG
      WRITE (out,1012) IG
!      WRITE (40,1012) IG
 1012 FORMAT (/,' generation:',I5)
      DO 400,I=1,NLAY(IG)
      WRITE (UNIT=CH150,FMT=900) I,LAYAS(IG,I)
  900 FORMAT (' layer ',I5,' : ',A)
      CALL PUST (6,CH150)
!      CALL PUST (40,CH150)
  400 CONTINUE
      WRITE (UNIT=scr,FMT='('' '')')
      WRITE (UNIT=out,FMT='('' '')')
!      WRITE (UNIT=40,FMT='('' '')')
!-----
      WRITE (scr,1000) (I,I=1,NLAY(IG))
      WRITE (out,1000) (I,I=1,NLAY(IG))
!      WRITE (40,1000) (I,I=1,NLAY(IG))
 1000 FORMAT (' layer Nr.:',4X,I4,100(9X,I4))
      WRITE (scr,1002) (TLAY(IG,I),I=1,NLAY(IG))
      WRITE (out,1002) (TLAY(IG,I),I=1,NLAY(IG))
!      WRITE (40,1002) (TLAY(IG,I),I=1,NLAY(IG))
 1002 FORMAT (' T growth :',100(1X,1PE12.5))
      WRITE (6,1004) (PLAY(IG,I),I=1,NLAY(IG))
      WRITE (out,1004) (PLAY(IG,I),I=1,NLAY(IG))
!      WRITE (40,1004) (PLAY(IG,I),I=1,NLAY(IG))
 1004 FORMAT (' P growth :',100(1X,1PE12.5))
      WRITE (6,1020) (DLAY(IG,I),I=1,NLAY(IG))
      WRITE (out,1020) (DLAY(IG,I),I=1,NLAY(IG))
!      WRITE (40,1020) (DLAY(IG,I),I=1,NLAY(IG))
 1020 FORMAT (' thickness:',100(1X,1PE12.5))
      WRITE (6,1022) (XLAY(IG,I),I=1,NLAY(IG))
      WRITE (out,1022) (XLAY(IG,I),I=1,NLAY(IG))
!      WRITE (40,1022) (XLAY(IG,I),I=1,NLAY(IG))
 1022 FORMAT (' radius   :',100(1X,1PE12.5))
      WRITE (6,1024) (VLAY(IG,I)/MOLAY(IG,I),I=1,NLAY(IG))
      WRITE (out,1024) (VLAY(IG,I)/MOLAY(IG,I),I=1,NLAY(IG))
!      WRITE (40,1024) (VLAY(IG,I)/MOLAY(IG,I),I=1,NLAY(IG))
 1024 FORMAT (' vol/mol  :',100(1X,1PE12.5))
      WRITE (6,1026) (VLAY(IG,I),I=1,NLAY(IG))
      WRITE (out,1026) (VLAY(IG,I),I=1,NLAY(IG))
!      WRITE (40,1026) (VLAY(IG,I),I=1,NLAY(IG))
 1026 FORMAT (' volume   :',100(1X,1PE12.5))
      WRITE (6,1028) (MOLAY(IG,I),I=1,NLAY(IG))
      WRITE (out,1028) (MOLAY(IG,I),I=1,NLAY(IG))
!      WRITE (40,1028) (MOLAY(IG,I),I=1,NLAY(IG))
 1028 FORMAT (' moles    :',100(1X,1PE12.5))
      WRITE (6,1030) (GARNET(IG,1,I),I=1,NLAY(IG))
      WRITE (out,1030) (GARNET(IG,1,I),I=1,NLAY(IG))
!      WRITE (40,1030) (GARNET(IG,1,I),I=1,NLAY(IG))
 1030 FORMAT (' x sps   :',100(3X,F10.6))
      WRITE (6,1032) (GARNET(IG,2,I),I=1,NLAY(IG))
      WRITE (out,1032) (GARNET(IG,2,I),I=1,NLAY(IG))
!      WRITE (40,1032) (GARNET(IG,2,I),I=1,NLAY(IG))
 1032 FORMAT (' x alm   :',100(3X,F10.6))
      WRITE (6,1034) (GARNET(IG,3,I),I=1,NLAY(IG))
      WRITE (out,1034) (GARNET(IG,3,I),I=1,NLAY(IG))
!      WRITE (40,1034) (GARNET(IG,3,I),I=1,NLAY(IG))
 1034 FORMAT (' x pyr   :',100(3X,F10.6))
      WRITE (6,1036) (GARNET(IG,4,I),I=1,NLAY(IG))
      WRITE (out,1036) (GARNET(IG,4,I),I=1,NLAY(IG))
!      WRITE (40,1036) (GARNET(IG,4,I),I=1,NLAY(IG))
 1036 FORMAT (' x grs   :',100(3X,F10.6))
!=====
  800 CONTINUE
!-----
      MOLSPE=0.0D0
      MOLGRO=0.0D0
      MOLALM=0.0D0
      MOLPYR=0.0D0
      DO IG=1,NGEN
        DO I=1,NLAY(IG)
          F1=MOLAY(IG,I)
          MOLSPE=MOLSPE+GARNET(IG,1,I)*F1/100.0D0
          MOLALM=MOLALM+GARNET(IG,2,I)*F1/100.0D0
          MOLPYR=MOLPYR+GARNET(IG,3,I)*F1/100.0D0
          MOLGRO=MOLGRO+GARNET(IG,4,I)*F1/100.0D0
        END DO
      END DO
      FF=MOLSPE+MOLALM+MOLPYR+MOLGRO
      AVSPE=MOLSPE*100.0D0/FF
      AVALM=MOLALM*100.0D0/FF
      AVPYR=MOLPYR*100.0D0/FF
      AVGRO=MOLGRO*100.0D0/FF
!C      WRITE (6,2000) MOLAYTOT,VLAYTOT,XLAY(NLAY)
!C      WRITE (40,2000) MOLAYTOT,VLAYTOT,XLAY(NLAY)
!C 2000 FORMAT (/
!C     >'total   moles       :',F12.6/
!C     >'        volume [ccm]:',F12.6/
!C     >'        radius [mue]:',F12.6)
!      WRITE (6,2010) MOLSPE,AVSPE,MOLALM,AVALM,MOLPYR,AVPYR,
!     >MOLGRO,AVGRO
!      WRITE (out,2010) MOLSPE,AVSPE,MOLALM,AVALM,MOLPYR,AVPYR,
!     >MOLGRO,AVGRO
!      WRITE (40,2010) MOLSPE,AVSPE,MOLALM,AVALM,MOLPYR,AVPYR,
!     >MOLGRO,AVGRO
! 2010 FORMAT (/
!     >'composition of garnet:  sps :',1PE12.5,' mol  =',0PF10.6,' %'/
!     >'                        alm :',1PE12.5,' mol  =',0PF10.6,' %'/
!     >'                        pyr :',1PE12.5,' mol  =',0PF10.6,' %'/
!     >'                        grs :',1PE12.5,' mol  =',0PF10.6,' %')
!      VPROMOL=VLAY(NLAY)/MOLAY(NLAY)
!C      VPROMOL=VLAYTOT/MOLAYTOT
!C      DO 510,I=1,NNOD
!C  510 MONOD(I)=VNOD(I)/VPROMOL
!=====================================================
!=====================================================
!      WRITE (6,3010) TIMTOT,VOL1M(1)
!      WRITE (40,3010) TIMTOT,VOL1M(1)
! 3010 FORMAT (/
!     >'diffusion nodes'/
!     >'---------------',5X,
!     >'total time (my)=',F15.6,
!     >3X,'1 mue sphere (ccm)=',1PE12.5)
!--
      WRITE (6,3010) TIMTOT
      WRITE (out,3010) TIMTOT
 3010 FORMAT (/ &
      ' diffusion nodes'/ &
      ' ---------------',5X, &
      'total time (my)=',F15.6)
!=====
      DO 820,IG=1,NGEN
      WRITE (6,1014) IG
      WRITE (out,1014) IG
!      WRITE (40,1014) IG
 1014 FORMAT (/,' generation:',I5)
!-----
      WRITE (6,3000) (I,I=1,NNOD(IG))
      WRITE (out,3000) (I,I=1,NNOD(IG))
!      WRITE (40,3000) (I,I=1,NNOD(IG))
 3000 FORMAT (' node Nr.:',4X,I4,100(9X,I4))
!      WRITE (6,3022) (XXC(IG,I),I=1,NNOD(IG))
!      WRITE (out,3022) (XXC(IG,I),I=1,NNOD(IG))
!C      WRITE (40,3022) (XXC(IG,I),I=1,NNOD(IG))
! 3022 FORMAT (' X [mue] :',100(1X,1PE12.5))
      WRITE (6,3024) (XHR(IG,I),I=1,NNOD(IG))
      WRITE (out,3024) (XHR(IG,I),I=1,NNOD(IG))
!      WRITE (40,3024) (XHR(IG,I),I=1,NNOD(IG))
 3024 FORMAT (' X [cm]  :',100(1X,1PE12.5))
      WRITE (6,3026) (DRVHR(IG,I),I=1,NNOD(IG))
      WRITE (out,3026) (DRVHR(IG,I),I=1,NNOD(IG))
!      WRITE (40,3026) (DRVHR(IG,I),I=1,NNOD(IG))
 3026 FORMAT (' DX [cm] :',100(1X,1PE12.5))
!      WRITE (6,3028) (VNOD(IG,I),I=1,NNOD(IG))
!      WRITE (out,3028) (VNOD(IG,I),I=1,NNOD(IG))
!C      WRITE (40,3028) (VNOD(IG,I),I=1,NNOD(IG))
! 3028 FORMAT (' vol     :',100(1X,1PE12.5))
!      WRITE (6,3029) (MONOD(IG,I),I=1,NNOD(IG))
!      WRITE (out,3029) (MONOD(IG,I),I=1,NNOD(IG))
!C      WRITE (40,3029) (MONOD(IG,I),I=1,NNOD(IG))
! 3029 FORMAT (' mol~~   :',100(1X,1PE12.5))
      WRITE (6,3030) (VMN(IG,I),I=1,NNOD(IG))
      WRITE (out,3030) (VMN(IG,I),I=1,NNOD(IG))
!      WRITE (40,3030) (VMN(IG,I),I=1,NNOD(IG))
 3030 FORMAT (' x sps   :',100(3X,F10.6))
      WRITE (6,3032) (VFE(IG,I),I=1,NNOD(IG))
      WRITE (out,3032) (VFE(IG,I),I=1,NNOD(IG))
!      WRITE (40,3032) (VFE(IG,I),I=1,NNOD(IG))
 3032 FORMAT (' x alm   :',100(3X,F10.6))
      WRITE (6,3034) (VMG(IG,I),I=1,NNOD(IG))
      WRITE (out,3034) (VMG(IG,I),I=1,NNOD(IG))
!      WRITE (40,3034) (VMG(IG,I),I=1,NNOD(IG))
 3034 FORMAT (' x pyr   :',100(3X,F10.6))
      WRITE (6,3036) (VCA(IG,I),I=1,NNOD(IG))
      WRITE (out,3036) (VCA(IG,I),I=1,NNOD(IG))
!      WRITE (40,3036) (VCA(IG,I),I=1,NNOD(IG))
 3036 FORMAT (' x grs   :',100(3X,F10.6))
!-----
  820 CONTINUE
!=====
!C      XMUE=(XHR(NNOD)+DRVHR(NNOD)/2.0D0)*1D4
!C      F1=VNODTOT/VPROMOL
!C      WRITE (6,4000) VPROMOL,F1,VNODTOT,XMUE
!C      WRITE (40,4000) VPROMOL,F1,VNODTOT,XMUE
!C 4000 FORMAT (/
!C     >'assuming molar volume =',F12.6/
!C     >'total   moles       :',F12.6/
!C     >'        volume [ccm]:',F12.6/
!C     >'        radius [mue]:',F12.6)
      NOBU(1)=0.0D0
      NOBU(2)=0.0D0
      NOBU(3)=0.0D0
      NOBU(4)=0.0D0
      DO 830,I=1,NC
      IF (OXYDE(I).EQ.'MN') NOBU(1)=CHEM(I)
      IF (OXYDE(I).EQ.'FE') NOBU(2)=CHEM(I)
      IF (OXYDE(I).EQ.'MG') NOBU(3)=CHEM(I)
      IF (OXYDE(I).EQ.'CA') NOBU(4)=CHEM(I)
  830 CONTINUE
      MOSPE=0.0D0
      MOGRO=0.0D0
      MOALM=0.0D0
      MOPYR=0.0D0
      DO IG=1,NGEN
        MNGEN(IG)=0.0D0
        FEGEN(IG)=0.0D0
        MGGEN(IG)=0.0D0
        CAGEN(IG)=0.0D0
        DO I=1,NNOD(IG)
          F1=MONOD(IG,I)
          MOSPE=MOSPE+VMN(IG,I)*F1/100.0D0
          MNGEN(IG)=MNGEN(IG)+VMN(IG,I)*F1/100.0D0
          MOALM=MOALM+VFE(IG,I)*F1/100.0D0
          FEGEN(IG)=FEGEN(IG)+VFE(IG,I)*F1/100.0D0
          MOPYR=MOPYR+VMG(IG,I)*F1/100.0D0
          MGGEN(IG)=MGGEN(IG)+VMG(IG,I)*F1/100.0D0
          MOGRO=MOGRO+VCA(IG,I)*F1/100.0D0
          CAGEN(IG)=CAGEN(IG)+VCA(IG,I)*F1/100.0D0
        END DO
      END DO
      FF=MOSPE+MOALM+MOPYR+MOGRO
      AVSPE=MOSPE*100.0D0/FF
      AVALM=MOALM*100.0D0/FF
      AVPYR=MOPYR*100.0D0/FF
      AVGRO=MOGRO*100.0D0/FF
!      WRITE (6,4010) MOSPE,AVSPE,MOALM,AVALM,MOPYR,AVPYR,MOGRO,AVGRO
!      WRITE (40,4010) MOSPE,AVSPE,MOALM,AVALM,MOPYR,AVPYR,MOGRO,AVGRO
! 4010 FORMAT (/
!     >'composition of garnet:  sps :',1PE12.5,' mol  =',0PF10.6,' %'/
!     >'                        alm :',1PE12.5,' mol  =',0PF10.6,' %'/
!     >'                        pyr :',1PE12.5,' mol  =',0PF10.6,' %'/
!     >'                        grs :',1PE12.5,' mol  =',0PF10.6,' %')
!=====
      IF (DIFFCODE.EQ.1.AND.DFCODE.EQ.1) THEN
      F1=MOSPELAST-MOSPE
      F2=MOALMLAST-MOALM
      F3=MOPYRLAST-MOPYR
      F4=MOGROLAST-MOGRO
      FF=F1+F2+F3+F4
!      WRITE (6,4015) F1,NOBU(1),F2,NOBU(2),F3,NOBU(3),F4,NOBU(4),FF
!      WRITE (40,4015) F1,NOBU(1),F2,NOBU(2),F3,NOBU(3),F4,NOBU(4),FF
! 4015 FORMAT (/
!     >'bulk change by diffusion :  sps :',1PE12.5,' mol   (bulk: ',
!     >1PE12.5,' )'/
!     >'                            alm :',1PE12.5,' mol   (bulk: ',
!     >1PE12.5,' )'/
!     >'                            pyr :',1PE12.5,' mol   (bulk: ',
!     >1PE12.5,' )'/
!     >'                            grs :',1PE12.5,' mol   (bulk: ',
!     >1PE12.5,' )'/
!     >'                                 ------------'/
!     >'                    total:       ',1PE12.5)
      CALL BULBACKFLOW(F1,F2,F3,F4)
      BACKSPE=BACKSPE+F1
      BACKALM=BACKALM+F2
      BACKPYR=BACKPYR+F3
      BACKGRO=BACKGRO+F4
!-----
      DO 700,IG=1,NGEN
      F1=MNGENLA(IG)-MNGEN(IG)
      F2=FEGENLA(IG)-FEGEN(IG)
      F3=MGGENLA(IG)-MGGEN(IG)
      F4=CAGENLA(IG)-CAGEN(IG)
      FF=F1+F2+F3+F4
!      WRITE (6,4017) IG,F1,F2,F3,F4,FF
!      WRITE (40,4017) IG,F1,F2,F3,F4,FF
! 4017 FORMAT (/
!     >'bulk change gener.',I4,'   :  sps :',1PE12.5,' mol'/
!     >'                            alm :',1PE12.5,' mol'/
!     >'                            pyr :',1PE12.5,' mol'/
!     >'                            grs :',1PE12.5,' mol'/
!     >'                                 ------------'/
!     >'                    total:       ',1PE12.5)
  700 CONTINUE
!-----
      DIFFCODE=0
      END IF
!=====
      MOSPELAST=MOSPE
      MOALMLAST=MOALM
      MOPYRLAST=MOPYR
      MOGROLAST=MOGRO
      DO 705,IG=1,NGEN
      MNGENLA(IG)=MNGEN(IG)
      FEGENLA(IG)=FEGEN(IG)
      MGGENLA(IG)=MGGEN(IG)
      CAGENLA(IG)=CAGEN(IG)
  705 CONTINUE
!=====
!      WRITE (6,4020)
! 4020 FORMAT (/'summary of garnet composition:'/
!     >19X,'growth',4X,'tot. back-flow',3X,'in nodes',6X,'gr-bk-nd')
      FF=MOLSPE-BACKSPE-MOSPE
      SUM4=FF
!      WRITE (6,4022) MOLSPE,BACKSPE,MOSPE,FF
! 4022 FORMAT (10X,'sps:',4(2X,1PE12.5))
      FF=MOLALM-BACKALM-MOALM
      SUM4=SUM4+FF
!      WRITE (6,4024) MOLALM,BACKALM,MOALM,FF
! 4024 FORMAT (10X,'alm:',4(2X,1PE12.5))
      FF=MOLPYR-BACKPYR-MOPYR
      SUM4=SUM4+FF
!      WRITE (6,4026) MOLPYR,BACKPYR,MOPYR,FF
! 4026 FORMAT (10X,'pyr:',4(2X,1PE12.5))
      FF=MOLGRO-BACKGRO-MOGRO
      SUM4=SUM4+FF
!      WRITE (6,4028) MOLGRO,BACKGRO,MOGRO,FF
! 4028 FORMAT (10X,'grs:',4(2X,1PE12.5))
      SUM1=MOLSPE+MOLALM+MOLPYR+MOLGRO
      SUM2=BACKSPE+BACKALM+BACKPYR+BACKGRO
      SUM3=MOSPE+MOALM+MOPYR+MOGRO
!      WRITE (6,4030) SUM1,SUM2,SUM3,SUM4
! 4030 FORMAT (/10X,'tot:',4(2X,1PE12.5))
!---
      RETURN
      END
!-----
!********************************
      SUBROUTINE PRTBUL
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!-----
!      INTEGER*4 MAXLAY,MAXGEN
!      PARAMETER (MAXLAY=400,maxgen=50)
!-----END OF COMMON VARIABLES
      REAL*8 ORIGBUL(COMAX),GROWBUL(COMAX),BACKBUL(COMAX)
      COMMON /BULLR/ ORIGBUL,GROWBUL,BACKBUL
!-----END OF BULK COMMON VARIABLES
      REAL*8 RJ,RCAL,PI,DMNZERO,AEMN,AVMN,DFEZERO,AEFE,AVFE, &
      DMGZERO,AEMG,AVMG
      COMMON /GLODA/ RJ,RCAL,PI,DMNZERO,AEMN,AVMN,DFEZERO,AEFE,AVFE, &
      DMGZERO,AEMG,AVMG
!----- end of GLOBAL DATA Common variables
      REAL*8 F1,F2,F3,F4
      INTEGER*4 I
!-----
      F1=0.0D0
      F2=0.0D0
      F3=0.0D0
      F4=0.0D0
      WRITE (UNIT=6,FMT=112)
  112 FORMAT(/ &
      ' ------------'/ &
      ' composition:',5X,'original',8X,'grow',8X, &
      'back-flow',6X,'final'/ &
      ' ------------'/' ')
      DO 541,I=1,NC
      IF (ORIGBUL(I).NE.0.0D0) THEN
      WRITE (UNIT=6,FMT=114) I,OXYDE(I),ORIGBUL(I),GROWBUL(I), &
      BACKBUL(I),CHEM(I)
  114 FORMAT (1X,I3,2X,A8,4(1X,F13.8))
      F1=F1+ORIGBUL(I)
      F2=F2+GROWBUL(I)
      F3=F3+BACKBUL(I)
!!AUG2010
!      F4=F4+BULK(I)
      F4=F4+CHEM(I)
      END IF
  541 CONTINUE
      WRITE (UNIT=6,FMT=115) F1,F2,F3,F4
  115 FORMAT ( &
      15X,' ------------',1X,' ------------', &
      1X,' ------------',1X,' ------------',1X/ &
      '   total',6X,4(1X,F13.8))
!-----
      RETURN
      END
!-----
!********************************
      SUBROUTINE BULBACKFLOW(F1,F2,F3,F4)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!-----
!      INTEGER*4 MAXLAY,MAXGEN
!      PARAMETER (MAXLAY=400,maxgen=50)
!-----END OF COMMON VARIABLES
      REAL*8 ORIGBUL(COMAX),GROWBUL(COMAX),BACKBUL(COMAX)
      COMMON /BULLR/ ORIGBUL,GROWBUL,BACKBUL
!-----END OF BULK COMMON VARIABLES
      REAL*8 RJ,RCAL,PI,DMNZERO,AEMN,AVMN,DFEZERO,AEFE,AVFE, &
      DMGZERO,AEMG,AVMG
      COMMON /GLODA/ RJ,RCAL,PI,DMNZERO,AEMN,AVMN,DFEZERO,AEFE,AVFE, &
      DMGZERO,AEMG,AVMG
!----- end of GLOBAL DATA Common variables
      REAL*8 F1,F2,F3,F4
      INTEGER*4 I,I1,I2,I3,I4
      LOGICAL*4 MORE
!-----
      I1=0
      I2=0
      I3=0
      I4=0
      DO 500,I=1,NC
      CHE(I)=CHEM(I)
      IF (OXYDE(I).EQ.'MN') THEN
      I1=I
      CHE(I)=CHE(I)+F1
      IF (CHE(I).LT.1D-6) CHE(I)=0.0D0
      END IF
      IF (OXYDE(I).EQ.'FE') THEN
      I2=I
      CHE(I)=CHE(I)+F2
      IF (CHE(I).LT.1D-6) CHE(I)=0.0D0
      END IF
      IF (OXYDE(I).EQ.'MG') THEN
      I3=I
      CHE(I)=CHE(I)+F3
      IF (CHE(I).LT.1D-6) CHE(I)=0.0D0
      END IF
      IF (OXYDE(I).EQ.'CA') THEN
      I4=I
      CHE(I)=CHE(I)+F4
      IF (CHE(I).LT.1D-6) CHE(I)=0.0D0
      END IF
  500 CONTINUE
!=====
      MORE=.FALSE.
      DO I=1,NC
        IF (CHE(I).EQ.0.0D0.NEQV.CHEM(I).EQ.0.0D0) MORE=.TRUE.
        CHEM(I)=CHE(I)
      END DO
      IF (MORE) THEN
      CALL DBREAD
      CALL NURVONPT
      ELSE
      DO I=1,NUN
        BULK(I)=CHE(CHMCOD(I))
      END DO
      END IF
!=====
!-----
      BACKBUL(I1)=BACKBUL(I1)+F1
      BACKBUL(I2)=BACKBUL(I2)+F2
      BACKBUL(I3)=BACKBUL(I3)+F3
      BACKBUL(I4)=BACKBUL(I4)+F4
      CALL PRTBUL
!-----
      RETURN
      END
!-----
!********************************
      SUBROUTINE BULBACKFLOW2(F1,F2,F3,F4)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!-----
!      INTEGER*4 MAXLAY,MAXGEN
!      PARAMETER (MAXLAY=400,maxgen=50)
!-----END OF COMMON VARIABLES
      REAL*8 ORIGBUL(COMAX),GROWBUL(COMAX),BACKBUL(COMAX)
      COMMON /BULLR/ ORIGBUL,GROWBUL,BACKBUL
!-----END OF BULK COMMON VARIABLES
      REAL*8 RJ,RCAL,PI,DMNZERO,AEMN,AVMN,DFEZERO,AEFE,AVFE, &
      DMGZERO,AEMG,AVMG
      COMMON /GLODA/ RJ,RCAL,PI,DMNZERO,AEMN,AVMN,DFEZERO,AEFE,AVFE, &
      DMGZERO,AEMG,AVMG
!----- end of GLOBAL DATA Common variables
      REAL*8 F1,F2,F3,F4
      INTEGER*4 I,I1,I2,I3,I4
!-----
      I1=0
      I2=0
      I3=0
      I4=0
      DO 500,I=1,NUN
      IF (CHNAME(I).EQ.'MN') I1=I
      IF (CHNAME(I).EQ.'FE') I2=I
      IF (CHNAME(I).EQ.'MG') I3=I
      IF (CHNAME(I).EQ.'CA') I4=I
  500 CONTINUE
      IF (I1.EQ.0.OR.I2.EQ.0.OR.I3.EQ.0.OR.I4.EQ.0) THEN
      WRITE (6,1000) I1,I2,I3,I4
      WRITE (out,1000) I1,I2,I3,I4
 1000 FORMAT ('element not found for backflow!!!!!!!! ',4I3)
      STOP
      END IF
!-----
      BACKBUL(I1)=BACKBUL(I1)+F1
      BACKBUL(I2)=BACKBUL(I2)+F2
      BACKBUL(I3)=BACKBUL(I3)+F3
      BACKBUL(I4)=BACKBUL(I4)+F4
      BULK(I1)=BULK(I1)+F1
      BULK(I2)=BULK(I2)+F2
      BULK(I3)=BULK(I3)+F3
      BULK(I4)=BULK(I4)+F4
      DO I=1,NUN
        IF (BULK(I).LT.0.0D0) BULK(I)=0.0D0
      END DO
      CALL PRTBUL
!-----
      RETURN
      END
!-----
!********************************
      SUBROUTINE MAKEOX
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!-----
      INTEGER*4 I,MACHO
!-----
      IF (MAKEO.EQ.0) RETURN
      MACHO=0
      DO I=1,NC
       IF (OXYDE(I).EQ.'O') MACHO=I
      END DO
      IF (MACHO.GT.0) THEN
      CHEM(MACHO)=0.0D0
       DO I=1,NC
        CHEM(MACHO)=CHEM(MACHO)+OXANZ(I)*CHEM(I)
       END DO
      DO I=1,NUN
       BULK(I)=CHEM(CHMCOD(I))
      END DO
      END IF
!-----
      RETURN
      END
!-----
!********************************
      SUBROUTINE PRTSHORT
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!-----
      INTEGER*4 MAXLAY,MAXGEN
      PARAMETER (MAXLAY=400,maxgen=50)
!-----END OF COMMON VARIABLES
      REAL*8 VOL1M(MAXGEN),GARNET(MAXGEN,4,MAXLAY),VLAY(MAXGEN,MAXLAY), &
      DLAY(MAXGEN,MAXLAY),XLAY(MAXGEN,MAXLAY),MOLAY(MAXGEN,MAXLAY), &
      TLAY(MAXGEN,MAXLAY),PLAY(MAXGEN,MAXLAY),VLAYTOT(MAXGEN), &
      MOLAYTOT(MAXGEN),VOLGAR,RAD0,NGARCCM(MAXGEN)
      INTEGER*4 NLAY(MAXGEN),NGARTOT(MAXGEN),NGENTOT, &
      NGEN,NCURAS,CURASNR,LAYASNR(MAXGEN,MAXLAY)
      CHARACTER*125 CURAS,LAYAS(MAXGEN,MAXLAY),CURASSES(MAXLAY)
      COMMON /LAYER/ VOL1M,GARNET,VLAY,DLAY,XLAY,MOLAY,TLAY,PLAY, &
      VLAYTOT,MOLAYTOT,VOLGAR,RAD0
      COMMON /LAYEI/ NLAY,NGARTOT,NGARCCM,NGENTOT,NGEN,NCURAS, &
      CURASNR,LAYASNR
      COMMON /LAYEC/ CURAS,LAYAS,CURASSES
!-----END OF LAYER COMMON VARIABLES
      REAL*8 RJ,RCAL,PI,DMNZERO,AEMN,AVMN,DFEZERO,AEFE,AVFE, &
      DMGZERO,AEMG,AVMG
      COMMON /GLODA/ RJ,RCAL,PI,DMNZERO,AEMN,AVMN,DFEZERO,AEFE,AVFE, &
      DMGZERO,AEMG,AVMG
!----- end of GLOBAL DATA Common variables
      REAL*8 FF
      INTEGER*4 I,II,I1,IP,IL,I2
      CHARACTER*16 TEXT
!-----
      IF (LOO1.GE.LO1MAX) THEN
      PRTLOG(6)=.TRUE.
      CALL PRTCAL
      PRTLOG(6)=.FALSE.
      END IF
!+++++
      CURAS=' '
      I1=1
      DO 500,I=1,NUN2
      IF (NUMMER(I).LE.0) THEN
      TEXT=SOLNAM(EMCODE(I))
      ELSE
      TEXT=NAME(NUMMER(I))
      END IF
      CALL LABLA(TEXT,IL)
      CURAS(I1:)=TEXT
      I1=I1+IL+1
  500 CONTINUE
!+++++
      IF (NCURAS.EQ.0) THEN
       NCURAS=1
       CURASSES(1)=CURAS
      ELSE
       I2=0
       DO I=1,NCURAS
        IF (CURAS.EQ.CURASSES(I)) I2=I
       END DO
       IF (I2.EQ.0) THEN
        NCURAS=NCURAS+1
        CURASSES(NCURAS)=CURAS
        CURASNR=NCURAS
        OPEN (UNIT=50,FILE='garnet_assembl.txt',STATUS='UNKNOWN')
        DO II=1,NCURAS
         WRITE (UNIT=50,FMT='(I4,2X,A)') II,CURASSES(II)
        END DO
        CLOSE (UNIT=50)
       ELSE
        CURASNR=I2
       END IF
      END IF
!+++++
!+++++
      WRITE (6,102) P,TC
  102 FORMAT (/' P =',F15.8,' bar',/' T = ',F14.8,' C')
      WRITE (6,2000)
 2000 FORMAT (25X,'mol',6X,'volume',4X,'x',/ &
      25X,'---',6X,'------',4X,'-')
      DO 700,I=1,NUN2
      IF (NUMMER(I).LE.0) THEN
      TEXT=SOLNAM(EMCODE(I))
      ELSE
      TEXT=NAME(NUMMER(I))
      END IF
      IF (EMCODE(I).GT.0) THEN
      I1=NEND(EMCODE(I))
      WRITE (6,1000) TEXT,NN(I),VOLPH(I), &
      (XEM(I,II),II=1,I1)
 1000 FORMAT (1X,A16,F12.4,F12.4,100(2X,F6.4))
      ELSE
      WRITE (6,1002) TEXT,NN(I),VOLPH(I)
 1002 FORMAT (1X,A16,F12.4,F12.4)
      END IF
  700 CONTINUE
!======
      IP=0
      DO 800,I=1,NUN2
      IF (NUMMER(I).LE.0) THEN
      TEXT=SOLNAM(EMCODE(I))
      ELSE
      TEXT=NAME(NUMMER(I))
      END IF
      IF (TEXT.EQ.'GARNET') IP=I
  800 CONTINUE
      IF (IP.NE.0) THEN
      VOLGAR=VOLPH(IP)
      ELSE
      VOLGAR=0.0D0
      END IF
!-----
      FF=DBLE(NGARTOT(1))/VOLSOL
!      WRITE (6,3000) VOLSOL,VOLGAR,NGARTOT(1),FF
! 3000 FORMAT (/
!     >' volume of solids:  ',12X,F12.5,' ccm'/
!     >' volume of garnet:  ',12X,F12.5,' ccm'/
!     >' number of garnets1: ',I8,' = ',F12.5,' /ccm')
!      FF=((VLAYTOT(1)+VOLGAR)/VOL1M(1))**(1.0D0/3.0D0)
!     >-XLAY(1,NLAY(1))
!      WRITE (6,3010) FF
! 3010 FORMAT (' resulting garnet growth1: ',5X,1PE12.5,' mue')
!======
!-----
      RETURN
      END
!-----
!********************************
      SUBROUTINE DIFFUS(TNOW,PNOW,DIFTIM,OONR)
      IMPLICIT NONE
!-----
      INTEGER*4 MAXLAY,MAXGEN
      PARAMETER (MAXLAY=400,maxgen=50)
!-----END OF COMMON VARIABLES
      REAL*8 VOL1M(MAXGEN),GARNET(MAXGEN,4,MAXLAY),VLAY(MAXGEN,MAXLAY), &
      DLAY(MAXGEN,MAXLAY),XLAY(MAXGEN,MAXLAY),MOLAY(MAXGEN,MAXLAY), &
      TLAY(MAXGEN,MAXLAY),PLAY(MAXGEN,MAXLAY),VLAYTOT(MAXGEN), &
      MOLAYTOT(MAXGEN),VOLGAR,RAD0,NGARCCM(MAXGEN)
      INTEGER*4 NLAY(MAXGEN),NGARTOT(MAXGEN),NGENTOT, &
      NGEN,NCURAS,CURASNR,LAYASNR(MAXGEN,MAXLAY)
      CHARACTER*125 CURAS,LAYAS(MAXGEN,MAXLAY),CURASSES(MAXLAY)
      COMMON /LAYER/ VOL1M,GARNET,VLAY,DLAY,XLAY,MOLAY,TLAY,PLAY, &
      VLAYTOT,MOLAYTOT,VOLGAR,RAD0
      COMMON /LAYEI/ NLAY,NGARTOT,NGARCCM,NGENTOT,NGEN,NCURAS, &
      CURASNR,LAYASNR
      COMMON /LAYEC/ CURAS,LAYAS,CURASSES
!-----END OF LAYER COMMON VARIABLES
      REAL*8 VMN(MAXGEN,MAXLAY),VFE(MAXGEN,MAXLAY),VMG(MAXGEN,MAXLAY), &
      VCA(MAXGEN,MAXLAY),XXC(MAXGEN,MAXLAY),NODIST, &
      DRVHR(MAXGEN,MAXLAY),XHR(MAXGEN,MAXLAY),CVHR(MAXGEN,3*MAXLAY), &
      VNOD(MAXGEN,MAXLAY),MONOD(MAXGEN,MAXLAY),VNODTOT(MAXGEN), &
      MONODTOT(MAXGEN),MOSPELAST,MOALMLAST,MOPYRLAST,MOGROLAST, &
      BACKSPE,BACKALM,BACKPYR,BACKGRO,MNGENLA(MAXGEN),FEGENLA(MAXGEN), &
      MGGENLA(MAXGEN),CAGENLA(MAXGEN)
      INTEGER*4 NNOD(MAXGEN),NRHR(MAXGEN),DIFFCODE,DFCODE
      COMMON /PROFIR/ VMN,VFE,VMG,VCA,XXC,NODIST,DRVHR,CVHR,XHR, &
      VNOD,MONOD,VNODTOT,MONODTOT,MOSPELAST,MOALMLAST,MOPYRLAST, &
      MOGROLAST,BACKSPE,BACKALM,BACKPYR,BACKGRO,MNGENLA,FEGENLA, &
      MGGENLA,CAGENLA
      COMMON /PROFII/ NNOD,NRHR,DIFFCODE,DFCODE
!----- end of Profile Common variables
      REAL*8 TC,P,LENGTH,TT,TSS,TSSF,CMN1,CMN2,CFE1,CFE2,CMG1,CMG2, &
      TK,DMNTRACE,DFETRACE,DMGTRACE,DCATRACE, &
      CCA1,CCA2,XCA1,XCA2, &
      DMNMN(MAXLAY),DMNFE(MAXLAY),DMNMG(MAXLAY), &
      DFEMN(MAXLAY),DFEFE(MAXLAY),DFEMG(MAXLAY), &
      DMGMN(MAXLAY),DMGFE(MAXLAY),DMGMG(MAXLAY), &
      YEAR,DT,NT,K, &
      MNMNA(MAXLAY,MAXLAY),MNMNM(MAXLAY,MAXLAY), &
      MNFEA(MAXLAY,MAXLAY),MNFEM(MAXLAY,MAXLAY), &
      MNMGA(MAXLAY,MAXLAY),MNMGM(MAXLAY,MAXLAY), &
      FEMNA(MAXLAY,MAXLAY),FEMNM(MAXLAY,MAXLAY), &
      FEFEA(MAXLAY,MAXLAY),FEFEM(MAXLAY,MAXLAY), &
      FEMGA(MAXLAY,MAXLAY),FEMGM(MAXLAY,MAXLAY), &
      MGMNA(MAXLAY,MAXLAY),MGMNM(MAXLAY,MAXLAY), &
      MGFEA(MAXLAY,MAXLAY),MGFEM(MAXLAY,MAXLAY), &
      MGMGA(MAXLAY,MAXLAY),MGMGM(MAXLAY,MAXLAY), &
      A(3*MAXLAY,3*MAXLAY),M(3*MAXLAY,3*MAXLAY), &
      AIN(3*MAXLAY,3*MAXLAY)
      INTEGER*4 NR,FN
      COMMON /DIFRE/ TC,P,LENGTH,TT,TSS,TSSF,CMN1,CMN2,CFE1,CFE2,CMG1 &
      ,CMG2,TK,DMNTRACE,DFETRACE,DMGTRACE,DCATRACE, &
      CCA1,CCA2,XCA1,XCA2, &
      DMNMN,DMNFE,DMNMG,DFEMN,DFEFE, &
      DFEMG,DMGMN,DMGFE,DMGMG, &
      YEAR,DT,NT,K, &
      MNMNA,MNMNM,MNFEA,MNFEM, &
      MNMGA,MNMGM,FEMNA,FEMNM, &
      FEFEA,FEFEM,FEMGA,FEMGM, &
      MGMNA,MGMNM,MGFEA,MGFEM, &
      MGMGA,MGMGM,A,M,AIN
      COMMON /DIFIN/ NR,FN
!----- end of Common variables
      REAL*8 RJ,RCAL,PI,DMNZERO,AEMN,AVMN,DFEZERO,AEFE,AVFE, &
      DMGZERO,AEMG,AVMG
      COMMON /GLODA/ RJ,RCAL,PI,DMNZERO,AEMN,AVMN,DFEZERO,AEFE,AVFE, &
      DMGZERO,AEMG,AVMG
!----- end of GLOBAL DATA Common variables
      INTEGER*4 I,II,N,ILOOP,ILOOP2,IG,IFG,PIVOT
      REAL*8 XX(3*MAXLAY,3*MAXLAY),AA(3*MAXLAY,3*MAXLAY), &
      TSTEP,FORSTAT(3*MAXLAY,0:MAXLAY), &      
      FF,TNOW,PNOW
      REAL*8 DIFTIM,PKB,NOBU(4),MN0,FE0,MG0,CA0,MN1,FE1,MG1,CA1, &
      VT(MAXLAY),VR(MAXLAY),DMNMNMAX,TSSMAX,F1,TSSC,TSS10, &
      TCONTROL,CVOLD(MAXGEN,3*MAXLAY)
!      REAL*8 BB(3*MAXLAY,1),XNEW(3*MAXLAY),XOLD(3*MAXLAY),XTEST(3*MAXLAY,3*MAXLAY)
      INTEGER*4 I001,TSLOOP,ITSLOOP,OPSY(3),TRY,OONR
!      OPEN (UNIT=10,FILE='CVALUES',STATUS='UNKNOWN')
!1      OPEN (UNIT=12,FILE='amat',STATUS='UNKNOWN')
!1      OPEN (UNIT=14,FILE='ainvmat',STATUS='UNKNOWN')
!1      OPEN (UNIT=15,FILE='amalai',STATUS='UNKNOWN')
!1      OPEN (UNIT=16,FILE='mmat',STATUS='UNKNOWN')
!----
!     TC       temperature in ¡C
!     P        pressure in bar
!     length   adius of garnet in µm (10µm=0.01mm=0.001cm)
!     nr       number of spatial nodes
!     TT       total time of diffusion in 1Ma years
!     TSS      Time step size in years
!     TSSF     time step size in figure in 1000 years
!     FN       figure number
!----
      PIVOT=0
      DFCODE=1
      TT=DIFTIM
      TC=TNOW
      P=PNOW
      OPSY(1)=0
      OPSY(2)=0
      OPSY(3)=0
!+++++
      TRY=0
      DO IG=1,NGEN
        DO I=1,3*NRHR(IG)
          CVOLD(IG,I)=CVHR(IG,I)
        END DO
      END DO
      CALL CHECKFLOW(NOBU,MN0,FE0,MG0,CA0)
!      WRITE (UNIT=6,FMT='(''1'',4I6)') OPSY(1),OPSY(2),OPSY(3)
!      WRITE (UNIT=40,FMT='(''1'',4I6)') OPSY(1),OPSY(2),OPSY(3)
!+++++
    1 CONTINUE
      TRY=TRY+1
!+++++
!      WRITE (6,4050) TRY,OPSY(1),OPSY(2),OPSY(3)
!      WRITE (40,4050) TRY,OPSY(1),OPSY(2),OPSY(3)
! 4050 FORMAT (/,' TRY,OPSY = ',4(2X,I4))
      DO IG=1,NGEN
        DO I=1,3*NRHR(IG)
          CVHR(IG,I)=CVOLD(IG,I)
        END DO
        DO I=1,NRHR(IG)
          VMN(IG,I)=CVHR(IG,I)
          VFE(IG,I)=CVHR(IG,I+NRHR(IG))
          VMG(IG,I)=CVHR(IG,I+2*NRHR(IG))
          VCA(IG,I)=100.0D0-VMN(IG,I)-VFE(IG,I)-VMG(IG,I)
        END DO
      END DO
!+++++
      DO 400,IG=1,NGEN
      IFG=IG
      IF (NLAY(IG).LT.2) GOTO 400
      IF (TT.EQ.0.0D0) GOTO 400
!=====
      NR=NNOD(IG)
      WRITE (6,1000) TC,P,TT
      WRITE (OONR,1000) TC,P,TT
!      WRITE (40,1000) TC,P,TT
 1000 FORMAT ( &
      /' diffusion:    TC = ',F10.3,'  P = ',F10.3,2X, &
      'diftime = ',1PE15.8, &
      /' ==========')
      WRITE (6,1002) IFG
      WRITE (OONR,1002) IFG
!      WRITE (40,1002) IFG
 1002 FORMAT (' generation =',I5)
!--  kinetic parameters from Chakra + Gangu (1992) at fO2 defined by graphite buffer
!    DMnzero,DFezero,DMgzero      pre-exponential constant in cm^2/s
!    AEMn,AEFe,AEMg               activation energy for diffusion at 1 bar in cal/mol
!    AVMn,AVFe,AVMg               activation volume for diffusion in cm^3/mol
!--
!      DMNZERO=5.1D-4
!      AEMN=60569.0D0
!      AVMN=6.0D0
!      DFEZERO=6.4D-4
!      AEFE=65824.0D0
!      AVFE=5.6D0
!      DMGZERO=1.1D-3
!      AEMG=67997.0D0
!      AVMG=5.3D0
!--
!     TK         temperature in K
!     RJ         universal gas constant in J/(mol*K)
!     RCal       universal gas constant in cal/(mol*K)
!     DMntrace   pressure not considered
!     DFetrace   pressure not considered
!     DMgtrace   pressure not considered
!     DCatrace   after Dachs & Proyer (2002)
!--
      PKB=P/1D3
      TK=TC+273.15D0
!C      RJ=8.31451070D0
!C      RCAL=RJ/4.1868D0C
!-PKB-0.001
      DMNTRACE=DMNZERO*DEXP((-AEMN-(PKB-1.0D-3)*AVMN)/(RCAL*TK))
      DFETRACE=DFEZERO*DEXP((-AEFE-(PKB-1.0D-3)*AVFE)/(RCAL*TK))
      DMGTRACE=DMGZERO*DEXP((-AEMG-(PKB-1.0D-3)*AVMG)/(RCAL*TK))
      DCATRACE=0.5D0*DFETRACE
!
!      WRITE (6,1005) DMNTRACE,DFETRACE,DMGTRACE,DCATRACE
!      WRITE (12,1005) DMNTRACE,DFETRACE,DMGTRACE,DCATRACE
!      WRITE (40,1005) DMNTRACE,DFETRACE,DMGTRACE,DCATRACE
! 1005 FORMAT (' DMNTRACE,DFETRACE,DMGTRACE,DCATRACE =',4(2X,1PE15.8))
!---
!      WRITE (6,3050)
! 3050 FORMAT ('original concentrations:')
!      WRITE (6,3051) (VMN(IG,I),I=1,NR)
!      WRITE (6,3051) (VFE(IG,I),I=1,NR)
!      WRITE (6,3051) (VMG(IG,I),I=1,NR)
!      WRITE (6,3051) (VCA(IG,I),I=1,NR)
! 3051 FORMAT (10(F8.3,2X))
!
!C      PI=3.14159265358979D0
      DO I=1,NRHR(IG)-1
        VT(I)=4.0D0/3.0D0*PI*XHR(IG,I+1)**3
      END DO
!
      VR(1)=VT(1)
      DO I=2,NRHR(IG)-1
        VR(I)=VT(I)-VT(I-1)
      END DO
!---
!---
!    year     year in seconds
!    dt       size of timestep in seconds
!    nt       timespan in seconds
!    k        corresponds to gamma (Borse, 1997, p. 483), if multiplicated with D
!    index1   index for the movie

!======================================================
      CALL MAKED(IFG)
!======================================================
!      WRITE (UNIT=6,FMT='(''2'',4I6)') OPSY(1),OPSY(2),OPSY(3)
!      WRITE (UNIT=40,FMT='(''2'',4I6)') OPSY(1),OPSY(2),OPSY(3)
      TT=DIFTIM
!      FN=1
      YEAR=365.25D0*24.0D0*60.0D0*60.0D0
      DMNMNMAX=DMNMN(1)
      DO I=2,NRHR(IG)
        IF (DMNMN(I).GT.DMNMNMAX) DMNMNMAX=DMNMN(I)
      END DO
!C      K=0.1D0*DRVHR(IG,1)**2/DMNMNMAX
      K=0.1D0*(NODIST/1D4)**2/DMNMNMAX
      TSSC=K/YEAR
!      WRITE (6,3086) TSSC
!      WRITE (40,3086) TSSC
! 3086 FORMAT (/'diffusion: calculated timestep: ',1PE15.8,' yr')
      TSS10=TT*1.0D5
      TSSMAX=DMIN1(TSSC,TSS10)
      TSLOOP=IDINT(TT*1.0D6/TSSMAX)+1
      TSS=TT*1.0D6/DBLE(TSLOOP)
      WRITE (6,3087) TSS
      WRITE (OONR,3087) TSS
!      WRITE (40,3087) TSS
 3087 FORMAT (' diffusion: timestep used      : ',1PE15.8,' yr')
!-
!C      WRITE (6,3084) TSS
!C 3084 FORMAT ('enter timestep [yr] (maximal:',1PE12.5,')')
!C      READ (*,*) TSS
!C      WRITE (6,3085) TSS
!C 3085 FORMAT (' TSS =',1PE12.5,' years')
!-
      F1=TT*1.0D6/TSS
      I001=IDINT(F1/20.0D0)
      IF (I001.LT.1) I001=1
      DT=YEAR*TSS
      NT=YEAR*1000000*TT
!      WRITE (6,1005) YEAR,DT,NT,DR,K
!      WRITE (12,1005) YEAR,DT,NT,DR,K
! 1005 FORMAT (' YEAR,DT,NT,DR,K =',5(2X,1PE12.5))
!======================================================
      CALL MAKEXXAM(IFG,OPSY)
!      WRITE (UNIT=6,FMT='(''3'',4I6)') OPSY(1),OPSY(2),OPSY(3)
!      WRITE (UNIT=40,FMT='(''3'',4I6)') OPSY(1),OPSY(2),OPSY(3)
!C      WRITE (UNIT=6,FMT='('' makeexxam fertig'')')
      CALL MAKEAM(IFG)
!      WRITE (UNIT=6,FMT='(''4'',4I6)') OPSY(1),OPSY(2),OPSY(3)
!      WRITE (UNIT=40,FMT='(''4'',4I6)') OPSY(1),OPSY(2),OPSY(3)
!C      WRITE (UNIT=6,FMT='('' makeam fertig'')')
!-----
!      CALL PRINTDAS(IFG)
!      WRITE (UNIT=6,FMT='(''5'',4I6)') OPSY(1),OPSY(2),OPSY(3)
!      WRITE (UNIT=40,FMT='(''5'',4I6)') OPSY(1),OPSY(2),OPSY(3)
!C      WRITE (UNIT=6,FMT='('' printdas fertig'')')
      TCONTROL=0.0D0-DT
      ILOOP=0
      ILOOP2=0
      DO I=1,3*NRHR(IG)
        FORSTAT(I,ILOOP)=CVHR(IG,I)
      END DO
!=====
!C      DO 800,TSTEP=0.0D0,NT,DT
      TSTEP=0.0D0-DT
      DO 800,ITSLOOP=0,TSLOOP
      TSTEP=TSTEP+DT
      TCONTROL=TCONTROL+DT
      ILOOP=ILOOP+1
      WRITE (6,1010) ILOOP,TSTEP,NT,DT
      WRITE (OONR,1010) ILOOP,TSTEP,NT,DT
!1      WRITE (12,1010) ILOOP,TSTEP,NT,DT
!1      WRITE (14,1010) ILOOP,TSTEP,NT,DT
!1      WRITE (16,1010) ILOOP,TSTEP,NT,DT
 1010 FORMAT (' ILOOP=',I6,'  TSTEP =',1PE15.8, &
      ' DT,NT=',2(2X,1PE15.8))
      CALL MULMC(IFG)
!      WRITE (UNIT=6,FMT='(''6'',4I6)') OPSY(1),OPSY(2),OPSY(3)
!      WRITE (UNIT=40,FMT='(''6'',4I6)') OPSY(1),OPSY(2),OPSY(3)
      N=3*NRHR(IG)
!++++
!      DO 810,I=1,N
!      DO 810,II=1,N
!  810 AA(I,II)=A(I,II)
!      CALL MIGS(AA,N,XX,INDX)
!      DO 820,I=1,N
!      DO 820,II=1,N
!  820 AIN(I,II)=XX(I,II)
!      CALL MULAIN(IFG,OPSY)
!++++
      DO I=1,N
        DO II=1,N
          AA(I,II)=A(I,II)
        END DO
      END DO
      CALL MINVERT(N,AA,XX,PIVOT)
!      WRITE (UNIT=6,FMT='(''7'',4I6)') PIVOT
      IF (PIVOT.GT.0) RETURN
      DO I=1,N
        DO II=1,N
          AIN(I,II)=XX(I,II)
        END DO
      END DO
      CALL MULAIN(IFG,OPSY)
!      WRITE (UNIT=6,FMT='(''7'',4I6)') OPSY(1),OPSY(2),OPSY(3)
!      WRITE (UNIT=40,FMT='(''7'',4I6)') OPSY(1),OPSY(2),OPSY(3)
!C      CALL MMULT (N, N, N, A, AIN, XTEST)
!C      DO 830,IR=1,N
!C      WRITE (15,1030) (XTEST(IR,IC),IC=1,N)
!C 1030 FORMAT (300(1PE12.5,2X))
!C  830 CONTINUE
!++++
!      DO 810,I=1,N
!      BB(I,1)=CV(I)
!      DO 810,II=1,N
!  810 AA(I,II)=A(I,II)
!      CALL DGESV( N, 1, AA, N, INDX, BB, N, INFO )
!      DO 820,I=1,N
!  820 CV(I)=BB(I,1)
!++++
!      DO 810,I=1,N
!      XOLD(I)=CV(I)
!      DO 810,II=1,N
!  810 AA(I,II)=A(I,II)
!      CALL LEGS(AA,N,XOLD,XNEW,INDX)
!      DO 820,I=1,N
!  820 CV(I)=XNEW(I)
!++++
!C      CALL PRINTMAA(IFG)
      IF (MOD(ILOOP,I001).EQ.1) THEN
!      CALL PRINTDAS(IFG)
!      WRITE (UNIT=6,FMT='(''8'',4I6)') OPSY(1),OPSY(2),OPSY(3)
!      WRITE (UNIT=40,FMT='(''8'',4I6)') OPSY(1),OPSY(2),OPSY(3)
!      IF (MOD(ILOOP,10).EQ.0) THEN
      ILOOP2=ILOOP2+1
      DO I=1,3*NRHR(IG)
        FORSTAT(I,ILOOP2)=CVHR(IG,I)
      END DO
      END IF
!
      CALL MAKED(IFG)
!      WRITE (UNIT=6,FMT='(''9'',4I6)') OPSY(1),OPSY(2),OPSY(3)
!      WRITE (UNIT=40,FMT='(''9'',4I6)') OPSY(1),OPSY(2),OPSY(3)
      CALL MAKEXXAM(IFG,OPSY)
!      WRITE (UNIT=6,FMT='(''10'',4I6)') OPSY(1),OPSY(2),OPSY(3)
!      WRITE (UNIT=40,FMT='(''10'',4I6)') OPSY(1),OPSY(2),OPSY(3)
      CALL MAKEAM(IFG)
!      WRITE (UNIT=6,FMT='(''11'',4I6)') OPSY(1),OPSY(2),OPSY(3)
!      WRITE (UNIT=40,FMT='(''11'',4I6)') OPSY(1),OPSY(2),OPSY(3)
!=====
  800 CONTINUE
      TCONTROL=TCONTROL/YEAR/1.0D6
!      WRITE (6,2005) DIFTIM,TCONTROL
! 2005 FORMAT (/'total time (diftim) : ',1PE12.5,' m.y.',
!     >/'total time (control): ',1PE12.5,' m.y.')
!-----
!      OPEN (UNIT=18,FILE='FORST',STATUS='UNKNOWN')
      DO 750,I=1,3*NRHR(IG)
!      FF=DBLE(MOD(I-1,NRHR)+1)
      FF=XHR(IG,MOD(I-1,NRHR(IG))+1)
!      WRITE (18,2000) FF,(FORSTAT(I,II),II=1,ILOOP2)
! 2000 FORMAT (300(1PE12.5,2X))
  750 CONTINUE
!-----
!CC      NNOD=NRHR
!CC      DO 902,I=1,NRHR(IG)
!CC  902 XXC(IG,I)=XHR(IG,I)*1.0D4
      DO I=1,NRHR(IG)
        VMN(IG,I)=CVHR(IG,I)
        VFE(IG,I)=CVHR(IG,I+NRHR(IG))
        VMG(IG,I)=CVHR(IG,I+2*NRHR(IG))
        VCA(IG,I)=100.0D0-VMN(IG,I)-VFE(IG,I)-VMG(IG,I)
      END DO
!-----
!1      CLOSE (UNIT=12)
!1      CLOSE (UNIT=14)
!1      CLOSE (UNIT=15)
!1      CLOSE (UNIT=16)
!+++++
  400 CONTINUE
!+++++
!+++++ check flow
!+++++
!C      IF (TRY.GE.2) RETURN
      CALL CHECKFLOW(NOBU,MN1,FE1,MG1,CA1)
!      WRITE (UNIT=6,FMT='(''12'',4I6)') OPSY(1),OPSY(2),OPSY(3)
!      WRITE (UNIT=40,FMT='(''12'',4I6)') OPSY(1),OPSY(2),OPSY(3)
!
      IF (MN1-MN0.GT.NOBU(1)) THEN
      DFCODE=0
!      OPSY(1)=1
!      OPSY(2)=1
!      OPSY(3)=1
      WRITE (6,4000) MN1-MN0,NOBU(1)
      WRITE (OONR,4000) MN1-MN0,NOBU(1)
!      WRITE (40,4000) MN1-MN0,NOBU(1)
 4000 FORMAT (/ '      close system for MN,  flow = ',1PE15.8, &
      '  bulk = ',1PE15.8)
      ELSE
!      WRITE (6,4010) MN1-MN0,NOBU(1)
!      WRITE (40,4010) MN1-MN0,NOBU(1)
! 4010 FORMAT (/ 'test        system for MN,  flow = ',1PE15.8,
!     >'  bulk = ',1PE15.8)
      END IF
!
      IF (FE1-FE0.GT.NOBU(2)) THEN
      DFCODE=0
!      OPSY(1)=1
!      OPSY(2)=1
!      OPSY(3)=1
      WRITE (6,4002) FE1-FE0,NOBU(2)
      WRITE (OONR,4002) FE1-FE0,NOBU(2)
!      WRITE (40,4002) FE1-FE0,NOBU(2)
 4002 FORMAT (/ '      close system for FE,  flow = ',1PE15.8, &
      '  bulk = ',1PE15.8)
      ELSE
!      WRITE (6,4012) FE1-FE0,NOBU(2)
!      WRITE (40,4012) FE1-FE0,NOBU(2)
! 4012 FORMAT (/ 'test        system for FE,  flow = ',1PE15.8,
!     >'  bulk = ',1PE15.8)
      END IF
!
      IF (MG1-MG0.GT.NOBU(3)) THEN
      DFCODE=0
!      OPSY(1)=1
!      OPSY(2)=1
!      OPSY(3)=1
      WRITE (6,4004) MG1-MG0,NOBU(3)
      WRITE (OONR,4004) MG1-MG0,NOBU(3)
!      WRITE (40,4004) MG1-MG0,NOBU(3)
 4004 FORMAT (/ '      close system for MG,  flow = ',1PE15.8, &
      '  bulk = ',1PE15.8)
      ELSE
!      WRITE (6,4014) MG1-MG0,NOBU(3)
!      WRITE (40,4014) MG1-MG0,NOBU(3)
! 4014 FORMAT (/ 'test        system for MG,  flow = ',1PE15.8,
!     >'  bulk = ',1PE15.8)
      END IF
!
      IF (CA1-CA0.GT.NOBU(4)) THEN
      DFCODE=0
!      OPSY(1)=1
!      OPSY(2)=1
!      OPSY(3)=1
      WRITE (6,4006) CA1-CA0,NOBU(4)
      WRITE (OONR,4006) CA1-CA0,NOBU(4)
!      WRITE (40,4006) CA1-CA0,NOBU(4)
 4006 FORMAT (/ '      close entire garnet, CA-flow = ',1PE15.8, &
      '  bulk = ',1PE15.8)
      ELSE
!      WRITE (6,4016) CA1-CA0,NOBU(4)
!      WRITE (40,4016) CA1-CA0,NOBU(4)
! 4016 FORMAT (/ 'test        system for CA,  flow = ',1PE15.8,
!     >'  bulk = ',1PE15.8)
      END IF
!-----
      IF (TRY.GE.2) RETURN
      IF (OPSY(1)+OPSY(2)+OPSY(3).GT.0) THEN
      DFCODE=0
      GOTO 1
      END IF
!=====
      RETURN
      END
!-----
!********************************
      SUBROUTINE CHECKFLOW(NOBU,MOSPE,MOALM,MOPYR,MOGRO)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!-----
      INTEGER*4 MAXLAY,MAXGEN
      PARAMETER (MAXLAY=400,maxgen=50)
!-----END OF COMMON VARIABLES
      REAL*8 VOL1M(MAXGEN),GARNET(MAXGEN,4,MAXLAY),VLAY(MAXGEN,MAXLAY), &
      DLAY(MAXGEN,MAXLAY),XLAY(MAXGEN,MAXLAY),MOLAY(MAXGEN,MAXLAY), &
      TLAY(MAXGEN,MAXLAY),PLAY(MAXGEN,MAXLAY),VLAYTOT(MAXGEN), &
      MOLAYTOT(MAXGEN),VOLGAR,RAD0,NGARCCM(MAXGEN)
      INTEGER*4 NLAY(MAXGEN),NGARTOT(MAXGEN),NGENTOT, &
      NGEN,NCURAS,CURASNR,LAYASNR(MAXGEN,MAXLAY)
      CHARACTER*125 CURAS,LAYAS(MAXGEN,MAXLAY),CURASSES(MAXLAY)
      COMMON /LAYER/ VOL1M,GARNET,VLAY,DLAY,XLAY,MOLAY,TLAY,PLAY, &
      VLAYTOT,MOLAYTOT,VOLGAR,RAD0
      COMMON /LAYEI/ NLAY,NGARTOT,NGARCCM,NGENTOT,NGEN,NCURAS, &
      CURASNR,LAYASNR
      COMMON /LAYEC/ CURAS,LAYAS,CURASSES
!-----END OF LAYER COMMON VARIABLES
      REAL*8 VMN(MAXGEN,MAXLAY),VFE(MAXGEN,MAXLAY),VMG(MAXGEN,MAXLAY), &
      VCA(MAXGEN,MAXLAY),XXC(MAXGEN,MAXLAY),NODIST, &
      DRVHR(MAXGEN,MAXLAY),XHR(MAXGEN,MAXLAY),CVHR(MAXGEN,3*MAXLAY), &
      VNOD(MAXGEN,MAXLAY),MONOD(MAXGEN,MAXLAY),VNODTOT(MAXGEN), &
      MONODTOT(MAXGEN),MOSPELAST,MOALMLAST,MOPYRLAST,MOGROLAST, &
      BACKSPE,BACKALM,BACKPYR,BACKGRO,MNGENLA(MAXGEN),FEGENLA(MAXGEN), &
      MGGENLA(MAXGEN),CAGENLA(MAXGEN)
      INTEGER*4 NNOD(MAXGEN),NRHR(MAXGEN),DIFFCODE,DFCODE
      COMMON /PROFIR/ VMN,VFE,VMG,VCA,XXC,NODIST,DRVHR,CVHR,XHR, &
      VNOD,MONOD,VNODTOT,MONODTOT,MOSPELAST,MOALMLAST,MOPYRLAST, &
      MOGROLAST,BACKSPE,BACKALM,BACKPYR,BACKGRO,MNGENLA,FEGENLA, &
      MGGENLA,CAGENLA
      COMMON /PROFII/ NNOD,NRHR,DIFFCODE,DFCODE
!----- end of Profile Common variables
      REAL*8 ORIGBUL(COMAX),GROWBUL(COMAX),BACKBUL(COMAX)
      COMMON /BULLR/ ORIGBUL,GROWBUL,BACKBUL
!-----END OF BULK COMMON VARIABLES
      REAL*8 RJ,RCAL,PI,DMNZERO,AEMN,AVMN,DFEZERO,AEFE,AVFE, &
      DMGZERO,AEMG,AVMG
      COMMON /GLODA/ RJ,RCAL,PI,DMNZERO,AEMN,AVMN,DFEZERO,AEFE,AVFE, &
      DMGZERO,AEMG,AVMG
!----- end of GLOBAL DATA Common variables
!-----
      REAL*8 MOSPE,MOGRO,MOALM,MOPYR,F1,NOBU(4)
      INTEGER*4 I,IG
!      CHARACTER*150 CH150
!-----
      IF (NLAY(1).EQ.0) RETURN
!-----
!=====================================================
      NOBU(1)=0.0D0
      NOBU(2)=0.0D0
      NOBU(3)=0.0D0
      NOBU(4)=0.0D0
      DO 830,I=1,NC
      IF (OXYDE(I).EQ.'MN') NOBU(1)=CHEM(I)
      IF (OXYDE(I).EQ.'FE') NOBU(2)=CHEM(I)
      IF (OXYDE(I).EQ.'MG') NOBU(3)=CHEM(I)
      IF (OXYDE(I).EQ.'CA') NOBU(4)=CHEM(I)
  830 CONTINUE
      MOSPE=0.0D0
      MOGRO=0.0D0
      MOALM=0.0D0
      MOPYR=0.0D0
      DO IG=1,NGEN
        DO I=1,NNOD(IG)
          F1=MONOD(IG,I)
          MOSPE=MOSPE+VMN(IG,I)*F1/100.0D0
          MOALM=MOALM+VFE(IG,I)*F1/100.0D0
          MOPYR=MOPYR+VMG(IG,I)*F1/100.0D0
          MOGRO=MOGRO+VCA(IG,I)*F1/100.0D0
        END DO
      END DO
!---
      RETURN
      END
!-----
!********************************
      SUBROUTINE MAKED(IFG)
      IMPLICIT NONE
!-----
      INTEGER*4 MAXLAY,MAXGEN
      PARAMETER (MAXLAY=400,maxgen=50)
!-----END OF COMMON VARIABLES
      REAL*8 VMN(MAXGEN,MAXLAY),VFE(MAXGEN,MAXLAY),VMG(MAXGEN,MAXLAY), &
      VCA(MAXGEN,MAXLAY),XXC(MAXGEN,MAXLAY),NODIST, &
      DRVHR(MAXGEN,MAXLAY),XHR(MAXGEN,MAXLAY),CVHR(MAXGEN,3*MAXLAY), &
      VNOD(MAXGEN,MAXLAY),MONOD(MAXGEN,MAXLAY),VNODTOT(MAXGEN), &
      MONODTOT(MAXGEN),MOSPELAST,MOALMLAST,MOPYRLAST,MOGROLAST, &
      BACKSPE,BACKALM,BACKPYR,BACKGRO,MNGENLA(MAXGEN),FEGENLA(MAXGEN), &
      MGGENLA(MAXGEN),CAGENLA(MAXGEN)
      INTEGER*4 NNOD(MAXGEN),NRHR(MAXGEN),DIFFCODE,DFCODE
      COMMON /PROFIR/ VMN,VFE,VMG,VCA,XXC,NODIST,DRVHR,CVHR,XHR, &
      VNOD,MONOD,VNODTOT,MONODTOT,MOSPELAST,MOALMLAST,MOPYRLAST, &
      MOGROLAST,BACKSPE,BACKALM,BACKPYR,BACKGRO,MNGENLA,FEGENLA, &
      MGGENLA,CAGENLA
      COMMON /PROFII/ NNOD,NRHR,DIFFCODE,DFCODE
!----- end of Profile Common variables
      REAL*8 TC,P,LENGTH,TT,TSS,TSSF,CMN1,CMN2,CFE1,CFE2,CMG1,CMG2, &
      TK,DMNTRACE,DFETRACE,DMGTRACE,DCATRACE, &
      CCA1,CCA2,XCA1,XCA2, &
      DMNMN(MAXLAY),DMNFE(MAXLAY),DMNMG(MAXLAY), &
      DFEMN(MAXLAY),DFEFE(MAXLAY),DFEMG(MAXLAY), &
      DMGMN(MAXLAY),DMGFE(MAXLAY),DMGMG(MAXLAY), &
      YEAR,DT,NT,K, &
      MNMNA(MAXLAY,MAXLAY),MNMNM(MAXLAY,MAXLAY), &
      MNFEA(MAXLAY,MAXLAY),MNFEM(MAXLAY,MAXLAY), &
      MNMGA(MAXLAY,MAXLAY),MNMGM(MAXLAY,MAXLAY), &
      FEMNA(MAXLAY,MAXLAY),FEMNM(MAXLAY,MAXLAY), &
      FEFEA(MAXLAY,MAXLAY),FEFEM(MAXLAY,MAXLAY), &
      FEMGA(MAXLAY,MAXLAY),FEMGM(MAXLAY,MAXLAY), &
      MGMNA(MAXLAY,MAXLAY),MGMNM(MAXLAY,MAXLAY), &
      MGFEA(MAXLAY,MAXLAY),MGFEM(MAXLAY,MAXLAY), &
      MGMGA(MAXLAY,MAXLAY),MGMGM(MAXLAY,MAXLAY), &
      A(3*MAXLAY,3*MAXLAY),M(3*MAXLAY,3*MAXLAY), &
      AIN(3*MAXLAY,3*MAXLAY)
      INTEGER*4 NR,FN
      COMMON /DIFRE/ TC,P,LENGTH,TT,TSS,TSSF,CMN1,CMN2,CFE1,CFE2,CMG1 &
      ,CMG2,TK,DMNTRACE,DFETRACE,DMGTRACE,DCATRACE, &
      CCA1,CCA2,XCA1,XCA2, &
      DMNMN,DMNFE,DMNMG,DFEMN,DFEFE, &
      DFEMG,DMGMN,DMGFE,DMGMG, &
      YEAR,DT,NT,K, &
      MNMNA,MNMNM,MNFEA,MNFEM, &
      MNMGA,MNMGM,FEMNA,FEMNM, &
      FEFEA,FEFEM,FEMGA,FEMGM, &
      MGMNA,MGMNM,MGFEA,MGFEM, &
      MGMGA,MGMGM,A,M,AIN
      COMMON /DIFIN/ NR,FN
!----- end of Common variables
      REAL*8 RJ,RCAL,PI,DMNZERO,AEMN,AVMN,DFEZERO,AEFE,AVFE, &
      DMGZERO,AEMG,AVMG
      COMMON /GLODA/ RJ,RCAL,PI,DMNZERO,AEMN,AVMN,DFEZERO,AEFE,AVFE, &
      DMGZERO,AEMG,AVMG
!----- end of GLOBAL DATA Common variables
      INTEGER*4 I,IFG
      REAL*8 DENOM
!-----
!   calculation of the D-matrix after Lasaga (1979) assuming negligible activity
!   coefficient gradients;
!---
      DO 510,I=1,NRHR(IFG)
      DENOM=CVHR(IFG,I)/100*DMNTRACE+CVHR(IFG,NRHR(IFG)+I) &
      /100*DFETRACE &
      +CVHR(IFG,2*NRHR(IFG)+I)/100*DMGTRACE+CVHR(IFG,3*NRHR(IFG)+I) &
      /100*DCATRACE

      DMNMN(I) =DMNTRACE*1-((DMNTRACE*CVHR(IFG,0*NRHR(IFG)+I)/100) &
      /DENOM)*(DMNTRACE-DCATRACE)
      DMNFE(I) =DMNTRACE*0-((DMNTRACE*CVHR(IFG,0*NRHR(IFG)+I)/100) &
      /DENOM)*(DFETRACE-DCATRACE)
      DMNMG(I) =DMNTRACE*0-((DMNTRACE*CVHR(IFG,0*NRHR(IFG)+I)/100) &
      /DENOM)*(DMGTRACE-DCATRACE)
      DFEMN(I) =DFETRACE*0-((DFETRACE*CVHR(IFG,1*NRHR(IFG)+I)/100) &
      /DENOM)*(DMNTRACE-DCATRACE)
      DFEFE(I) =DFETRACE*1-((DFETRACE*CVHR(IFG,1*NRHR(IFG)+I)/100) &
      /DENOM)*(DFETRACE-DCATRACE)
      DFEMG(I) =DFETRACE*0-((DFETRACE*CVHR(IFG,1*NRHR(IFG)+I)/100) &
      /DENOM)*(DMGTRACE-DCATRACE)
      DMGMN(I) =DMGTRACE*0-((DMGTRACE*CVHR(IFG,2*NRHR(IFG)+I)/100) &
      /DENOM)*(DMNTRACE-DCATRACE)
      DMGFE(I) =DMGTRACE*0-((DMGTRACE*CVHR(IFG,2*NRHR(IFG)+I)/100) &
      /DENOM)*(DFETRACE-DCATRACE)
      DMGMG(I) =DMGTRACE*1-((DMGTRACE*CVHR(IFG,2*NRHR(IFG)+I)/100) &
      /DENOM)*(DMGTRACE-DCATRACE)
  510 CONTINUE
!-----
      RETURN
      END
!-----
!********************************
      SUBROUTINE MAKEXXAM(IFG,OPSY)
      IMPLICIT NONE
!-----
      INTEGER*4 MAXLAY,MAXGEN
      PARAMETER (MAXLAY=400,maxgen=50)
!-----END OF COMMON VARIABLES
      REAL*8 VMN(MAXGEN,MAXLAY),VFE(MAXGEN,MAXLAY),VMG(MAXGEN,MAXLAY), &
      VCA(MAXGEN,MAXLAY),XXC(MAXGEN,MAXLAY),NODIST, &
      DRVHR(MAXGEN,MAXLAY),XHR(MAXGEN,MAXLAY),CVHR(MAXGEN,3*MAXLAY), &
      VNOD(MAXGEN,MAXLAY),MONOD(MAXGEN,MAXLAY),VNODTOT(MAXGEN), &
      MONODTOT(MAXGEN),MOSPELAST,MOALMLAST,MOPYRLAST,MOGROLAST, &
      BACKSPE,BACKALM,BACKPYR,BACKGRO,MNGENLA(MAXGEN),FEGENLA(MAXGEN), &
      MGGENLA(MAXGEN),CAGENLA(MAXGEN)
      INTEGER*4 NNOD(MAXGEN),NRHR(MAXGEN),DIFFCODE,DFCODE
      COMMON /PROFIR/ VMN,VFE,VMG,VCA,XXC,NODIST,DRVHR,CVHR,XHR, &
      VNOD,MONOD,VNODTOT,MONODTOT,MOSPELAST,MOALMLAST,MOPYRLAST, &
      MOGROLAST,BACKSPE,BACKALM,BACKPYR,BACKGRO,MNGENLA,FEGENLA, &
      MGGENLA,CAGENLA
      COMMON /PROFII/ NNOD,NRHR,DIFFCODE,DFCODE
!----- end of Profile Common variables
      REAL*8 TC,P,LENGTH,TT,TSS,TSSF,CMN1,CMN2,CFE1,CFE2,CMG1,CMG2, &
      TK,DMNTRACE,DFETRACE,DMGTRACE,DCATRACE, &
      CCA1,CCA2,XCA1,XCA2, &
      DMNMN(MAXLAY),DMNFE(MAXLAY),DMNMG(MAXLAY), &
      DFEMN(MAXLAY),DFEFE(MAXLAY),DFEMG(MAXLAY), &
      DMGMN(MAXLAY),DMGFE(MAXLAY),DMGMG(MAXLAY), &
      YEAR,DT,NT,K, &
      MNMNA(MAXLAY,MAXLAY),MNMNM(MAXLAY,MAXLAY), &
      MNFEA(MAXLAY,MAXLAY),MNFEM(MAXLAY,MAXLAY), &
      MNMGA(MAXLAY,MAXLAY),MNMGM(MAXLAY,MAXLAY), &
      FEMNA(MAXLAY,MAXLAY),FEMNM(MAXLAY,MAXLAY), &
      FEFEA(MAXLAY,MAXLAY),FEFEM(MAXLAY,MAXLAY), &
      FEMGA(MAXLAY,MAXLAY),FEMGM(MAXLAY,MAXLAY), &
      MGMNA(MAXLAY,MAXLAY),MGMNM(MAXLAY,MAXLAY), &
      MGFEA(MAXLAY,MAXLAY),MGFEM(MAXLAY,MAXLAY), &
      MGMGA(MAXLAY,MAXLAY),MGMGM(MAXLAY,MAXLAY), &
      A(3*MAXLAY,3*MAXLAY),M(3*MAXLAY,3*MAXLAY), &
      AIN(3*MAXLAY,3*MAXLAY)
      INTEGER*4 NR,FN
      COMMON /DIFRE/ TC,P,LENGTH,TT,TSS,TSSF,CMN1,CMN2,CFE1,CFE2,CMG1 &
      ,CMG2,TK,DMNTRACE,DFETRACE,DMGTRACE,DCATRACE, &
      CCA1,CCA2,XCA1,XCA2, &
      DMNMN,DMNFE,DMNMG,DFEMN,DFEFE, &
      DFEMG,DMGMN,DMGFE,DMGMG, &
      YEAR,DT,NT,K, &
      MNMNA,MNMNM,MNFEA,MNFEM, &
      MNMGA,MNMGM,FEMNA,FEMNM, &
      FEFEA,FEFEM,FEMGA,FEMGM, &
      MGMNA,MGMNM,MGFEA,MGFEM, &
      MGMGA,MGMGM,A,M,AIN
      COMMON /DIFIN/ NR,FN
!----- end of Common variables
      REAL*8 RJ,RCAL,PI,DMNZERO,AEMN,AVMN,DFEZERO,AEFE,AVFE, &
      DMGZERO,AEMG,AVMG
      COMMON /GLODA/ RJ,RCAL,PI,DMNZERO,AEMN,AVMN,DFEZERO,AEFE,AVFE, &
      DMGZERO,AEMG,AVMG
!----- end of GLOBAL DATA Common variables
      INTEGER*4 I,II,IFG,OPSY(3)
      REAL*8 DR1,DR2,R,EL,EN,EP, &
      D11,D12,D13,D21,D22,D23,D31,D32,D33
!-----
      EN=1.0D0
      EP=1.0D0
      DO 602,I=1,NRHR(IFG)
      DO 600,II=1,NRHR(IFG)
      MNMNA(I,II)=0.0D0
      MNMNM(I,II)=0.0D0
      MNFEA(I,II)=0.0D0
      MNFEM(I,II)=0.0D0
      MNMGA(I,II)=0.0D0
      MNMGM(I,II)=0.0D0
      FEMNA(I,II)=0.0D0
      FEMNM(I,II)=0.0D0
      FEFEA(I,II)=0.0D0
      FEFEM(I,II)=0.0D0
      FEMGA(I,II)=0.0D0
      FEMGM(I,II)=0.0D0
      MGMNA(I,II)=0.0D0
      MGMNM(I,II)=0.0D0
      MGFEA(I,II)=0.0D0
      MGFEM(I,II)=0.0D0
      MGMGA(I,II)=0.0D0
      MGMGM(I,II)=0.0D0
  600 CONTINUE
  602 CONTINUE
!=====
      IF (NRHR(IFG).LT.3) GOTO 10
!=====
      DO 610,I=1,NRHR(IFG)-2
      DR1=DRVHR(IFG,I)/2.0D0+DRVHR(IFG,I+1)/2.0D0
      DR2=DRVHR(IFG,I+1)/2.0D0+DRVHR(IFG,I+2)/2.0D0
      R=0.0D0
      DO II=1,I+1
        R=R+DRVHR(IFG,II)
      END DO
      EL=DT/(R*DR2*DR1*(DR2+DR1))
      EN=DT/DRVHR(IFG,1)**2
      EP=DT/DRVHR(IFG,NRHR(IFG))**2
!
      D11=DMNMN(I+1)
      D12=DMNFE(I+1)
      D13=DMNMG(I+1)
      D21=DFEMN(I+1)
      D22=DFEFE(I+1)
      D23=DFEMG(I+1)
      D31=DMGMN(I+1)
      D32=DMGFE(I+1)
      D33=DMGMG(I+1)
!
      MNMNA(I+1,I)=EL*D11*(DR2**2-R*DR2)
      MNMNA(I+1,I+1)=1+EL*D11*(R*DR2+R*DR1-DR2**2+DR1**2)
      MNMNA(I+1,I+2)=EL*D11*(-DR1**2-R*DR1)
!
      MNMNM(I+1,I)=-EL*D11*(DR2**2-R*DR2)
      MNMNM(I+1,I+1)=1-EL*D11*(R*DR2+R*DR1-DR2**2+DR1**2)
      MNMNM(I+1,I+2)=-EL*D11*(-DR1**2-R*DR1)
!
      MNFEA(I+1,I)=EL*D12*(DR2**2-R*DR2)
      MNFEA(I+1,I+1)=EL*D12*(R*DR2+R*DR1-DR2**2+DR1**2)
      MNFEA(I+1,I+2)=EL*D12*(-DR1**2-R*DR1)
!
      MNFEM(I+1,I)=-EL*D12*(DR2**2-R*DR2)
      MNFEM(I+1,I+1)=-EL*D12*(R*DR2+R*DR1-DR2**2+DR1**2)
      MNFEM(I+1,I+2)=-EL*D12*(-DR1**2-R*DR1)
!
      MNMGA(I+1,I)=EL*D13*(DR2**2-R*DR2)
      MNMGA(I+1,I+1)=EL*D13*(R*DR2+R*DR1-DR2**2+DR1**2)
      MNMGA(I+1,I+2)=EL*D13*(-DR1**2-R*DR1)
!
      MNMGM(I+1,I)=-EL*D13*(DR2**2-R*DR2)
      MNMGM(I+1,I+1)=-EL*D13*(R*DR2+R*DR1-DR2**2+DR1**2)
      MNMGM(I+1,I+2)=-EL*D13*(-DR1**2-R*DR1)
!
      FEMNA(I+1,I)=EL*D21*(DR2**2-R*DR2)
      FEMNA(I+1,I+1)=EL*D21*(R*DR2+R*DR1-DR2**2+DR1**2)
      FEMNA(I+1,I+2)=EL*D21*(-DR1**2-R*DR1)
!
      FEMNM(I+1,I)=-EL*D21*(DR2**2-R*DR2)
      FEMNM(I+1,I+1)=-EL*D21*(R*DR2+R*DR1-DR2**2+DR1**2)
      FEMNM(I+1,I+2)=-EL*D21*(-DR1**2-R*DR1)
!
      FEFEA(I+1,I)=EL*D22*(DR2**2-R*DR2)
      FEFEA(I+1,I+1)=1+EL*D22*(R*DR2+R*DR1-DR2**2+DR1**2)
      FEFEA(I+1,I+2)=EL*D22*(-DR1**2-R*DR1)
!
      FEFEM(I+1,I)=-EL*D22*(DR2**2-R*DR2)
      FEFEM(I+1,I+1)=1-EL*D22*(R*DR2+R*DR1-DR2**2+DR1**2)
      FEFEM(I+1,I+2)=-EL*D22*(-DR1**2-R*DR1)
!
      FEMGA(I+1,I)=EL*D23*(DR2**2-R*DR2)
      FEMGA(I+1,I+1)=EL*D23*(R*DR2+R*DR1-DR2**2+DR1**2)
      FEMGA(I+1,I+2)=EL*D23*(-DR1**2-R*DR1)
!
      FEMGM(I+1,I)=-EL*D23*(DR2**2-R*DR2)
      FEMGM(I+1,I+1)=-EL*D23*(R*DR2+R*DR1-DR2**2+DR1**2)
      FEMGM(I+1,I+2)=-EL*D23*(-DR1**2-R*DR1)
!
      MGMNA(I+1,I)=EL*D31*(DR2**2-R*DR2)
      MGMNA(I+1,I+1)=EL*D31*(R*DR2+R*DR1-DR2**2+DR1**2)
      MGMNA(I+1,I+2)=EL*D31*(-DR1**2-R*DR1)
!
      MGMNM(I+1,I)=-EL*D31*(DR2**2-R*DR2)
      MGMNM(I+1,I+1)=-EL*D31*(R*DR2+R*DR1-DR2**2+DR1**2)
      MGMNM(I+1,I+2)=-EL*D31*(-DR1**2-R*DR1)
!
      MGFEA(I+1,I)=EL*D32*(DR2**2-R*DR2)
      MGFEA(I+1,I+1)=EL*D32*(R*DR2+R*DR1-DR2**2+DR1**2)
      MGFEA(I+1,I+2)=EL*D32*(-DR1**2-R*DR1)
!
      MGFEM(I+1,I)=-EL*D32*(DR2**2-R*DR2)
      MGFEM(I+1,I+1)=-EL*D32*(R*DR2+R*DR1-DR2**2+DR1**2)
      MGFEM(I+1,I+2)=-EL*D32*(-DR1**2-R*DR1)
!
      MGMGA(I+1,I)=EL*D33*(DR2**2-R*DR2)
      MGMGA(I+1,I+1)=1+EL*D33*(R*DR2+R*DR1-DR2**2+DR1**2)
      MGMGA(I+1,I+2)=EL*D33*(-DR1**2-R*DR1)
!
      MGMGM(I+1,I)=-EL*D33*(DR2**2-R*DR2)
      MGMGM(I+1,I+1)=1-EL*D33*(R*DR2+R*DR1-DR2**2+DR1**2)
      MGMGM(I+1,I+2)=-EL*D33*(-DR1**2-R*DR1)
!--
  610 CONTINUE
!=====
   10 CONTINUE
!-----%boundary conditions-------------------------------------------------------
!=====
      MNMNA(1,1)=1+3*DMNMN(1)*EN
      MNMNA(1,2)=-3*DMNMN(1)*EN
      MNMNA(NRHR(IFG),NRHR(IFG)-1)=0
      MNMNA(NRHR(IFG),NRHR(IFG))=1
      MNMNM(1,1)=1-3*DMNMN(1)*EN
      MNMNM(1,2)=3*DMNMN(1)*EN
      MNMNM(NRHR(IFG),NRHR(IFG)-1)=0
      MNMNM(NRHR(IFG),NRHR(IFG))=1
!-----
      MNFEA(1,1)=3*DMNFE(1)*EN
      MNFEA(1,2)=-3*DMNFE(1)*EN
      MNFEA(NRHR(IFG),NRHR(IFG)-1)=0
      MNFEA(NRHR(IFG),NRHR(IFG))=0
      MNFEM(1,1)=-3*DMNFE(1)*EN
      MNFEM(1,2)=3*DMNFE(1)*EN
      MNFEM(NRHR(IFG),NRHR(IFG)-1)=0
      MNFEM(NRHR(IFG),NRHR(IFG))=0
!-----
      MNMGA(1,1)=3*DMNMG(1)*EN
      MNMGA(1,2)=-3*DMNMG(1)*EN
      MNMGA(NRHR(IFG),NRHR(IFG)-1)=0
      MNMGA(NRHR(IFG),NRHR(IFG))=0
      MNMGM(1,1)=-3*DMNMG(1)*EN
      MNMGM(1,2)=3*DMNMG(1)*EN
      MNMGM(NRHR(IFG),NRHR(IFG)-1)=0
      MNMGM(NRHR(IFG),NRHR(IFG))=0
!-----
      IF (OPSY(1).EQ.1) THEN
      MNMNA(NRHR(IFG),NRHR(IFG)-1)=-DMNMN(NRHR(IFG))*EP
      MNMNA(NRHR(IFG),NRHR(IFG))=1+DMNMN(NRHR(IFG))*EP
      MNMNM(NRHR(IFG),NRHR(IFG)-1)=DMNMN(NRHR(IFG))*EP
      MNMNM(NRHR(IFG),NRHR(IFG))=1-DMNMN(NRHR(IFG))*EP
!-----
      MNFEA(NRHR(IFG),NRHR(IFG)-1)=-DMNFE(NRHR(IFG))*EP
      MNFEA(NRHR(IFG),NRHR(IFG))=DMNFE(NRHR(IFG))*EP
      MNFEM(NRHR(IFG),NRHR(IFG)-1)=DMNFE(NRHR(IFG))*EP
      MNFEM(NRHR(IFG),NRHR(IFG))=-DMNFE(NRHR(IFG))*EP
!-----
      MNMGA(NRHR(IFG),NRHR(IFG)-1)=-DMNMG(NRHR(IFG))*EP
      MNMGA(NRHR(IFG),NRHR(IFG))=DMNMG(NRHR(IFG))*EP
      MNMGM(NRHR(IFG),NRHR(IFG)-1)=DMNMG(NRHR(IFG))*EP
      MNMGM(NRHR(IFG),NRHR(IFG))=-DMNMG(NRHR(IFG))*EP
      END IF
!-----
      FEMNA(1,1)=3*DFEMN(1)*EN
      FEMNA(1,2)=-3*DFEMN(1)*EN
      FEMNA(NRHR(IFG),NRHR(IFG)-1)=0
      FEMNA(NRHR(IFG),NRHR(IFG))=0
      FEMNM(1,1)=-3*DFEMN(1)*EN
      FEMNM(1,2)=3*DFEMN(1)*EN
      FEMNM(NRHR(IFG),NRHR(IFG)-1)=0
      FEMNM(NRHR(IFG),NRHR(IFG))=0
!-----
      FEFEA(1,1)=1+3*DFEFE(1)*EN
      FEFEA(1,2)=-3*DFEFE(1)*EN
      FEFEA(NRHR(IFG),NRHR(IFG)-1)=0
      FEFEA(NRHR(IFG),NRHR(IFG))=1
      FEFEM(1,1)=1-3*DFEFE(1)*EN
      FEFEM(1,2)=3*DFEFE(1)*EN
      FEFEM(NRHR(IFG),NRHR(IFG)-1)=0
      FEFEM(NRHR(IFG),NRHR(IFG))=1
!-----
      FEMGA(1,1)=3*DFEMG(1)*EN
      FEMGA(1,2)=-3*DFEMG(1)*EN
      FEMGA(NRHR(IFG),NRHR(IFG)-1)=0
      FEMGA(NRHR(IFG),NRHR(IFG))=0
      FEMGM(1,1)=-3*DFEMG(1)*EN
      FEMGM(1,2)=3*DFEMG(1)*EN
      FEMGM(NRHR(IFG),NRHR(IFG)-1)=0
      FEMGM(NRHR(IFG),NRHR(IFG))=0
!-----
      IF (OPSY(2).EQ.1) THEN
      FEMNA(NRHR(IFG),NRHR(IFG)-1)=-DFEMN(NRHR(IFG))*EP
      FEMNA(NRHR(IFG),NRHR(IFG))=DFEMN(NRHR(IFG))*EP
      FEMNM(NRHR(IFG),NRHR(IFG)-1)=DFEMN(NRHR(IFG))*EP
      FEMNM(NRHR(IFG),NRHR(IFG))=-DFEMN(NRHR(IFG))*EP
!-----
      FEFEA(NRHR(IFG),NRHR(IFG)-1)=-DFEFE(NRHR(IFG))*EP
      FEFEA(NRHR(IFG),NRHR(IFG))=1+DFEFE(NRHR(IFG))*EP
      FEFEM(NRHR(IFG),NRHR(IFG)-1)=DFEFE(NRHR(IFG))*EP
      FEFEM(NRHR(IFG),NRHR(IFG))=1-DFEFE(NRHR(IFG))*EP
!-----
      FEMGA(NRHR(IFG),NRHR(IFG)-1)=-DFEMG(NRHR(IFG))*EP
      FEMGA(NRHR(IFG),NRHR(IFG))=DFEMG(NRHR(IFG))*EP
      FEMGM(NRHR(IFG),NRHR(IFG)-1)=DFEMG(NRHR(IFG))*EP
      FEMGM(NRHR(IFG),NRHR(IFG))=-DFEMG(NRHR(IFG))*EP
      END IF
!-----
      MGMNA(1,1)=3*DMGMN(1)*EN
      MGMNA(1,2)=-3*DMGMN(1)*EN
      MGMNA(NRHR(IFG),NRHR(IFG)-1)=0
      MGMNA(NRHR(IFG),NRHR(IFG))=0
      MGMNM(1,1)=-3*DMGMN(1)*EN
      MGMNM(1,2)=3*DMGMN(1)*EN
      MGMNM(NRHR(IFG),NRHR(IFG)-1)=0
      MGMNM(NRHR(IFG),NRHR(IFG))=0
!-----
      MGFEA(1,1)=3*DMGFE(1)*EN
      MGFEA(1,2)=-3*DMGFE(1)*EN
      MGFEA(NRHR(IFG),NRHR(IFG)-1)=0
      MGFEA(NRHR(IFG),NRHR(IFG))=0
      MGFEM(1,1)=-3*DMGFE(1)*EN
      MGFEM(1,2)=3*DMGFE(1)*EN
      MGFEM(NRHR(IFG),NRHR(IFG)-1)=0
      MGFEM(NRHR(IFG),NRHR(IFG))=0
!-----
      MGMGA(1,1)=1+3*DMGMG(1)*EN
      MGMGA(1,2)=-3*DMGMG(1)*EN
      MGMGA(NRHR(IFG),NRHR(IFG)-1)=0
      MGMGA(NRHR(IFG),NRHR(IFG))=1
      MGMGM(1,1)=1-3*DMGMG(1)*EN
      MGMGM(1,2)=3*DMGMG(1)*EN
      MGMGM(NRHR(IFG),NRHR(IFG)-1)=0
      MGMGM(NRHR(IFG),NRHR(IFG))=1
!-----
      IF (OPSY(3).EQ.1) THEN
      MGMNA(NRHR(IFG),NRHR(IFG)-1)=-DMGMN(NRHR(IFG))*EP
      MGMNA(NRHR(IFG),NRHR(IFG))=DMGMN(NRHR(IFG))*EP
      MGMNM(NRHR(IFG),NRHR(IFG)-1)=DMGMN(NRHR(IFG))*EP
      MGMNM(NRHR(IFG),NRHR(IFG))=-DMGMN(NRHR(IFG))*EP
!-----
      MGFEA(NRHR(IFG),NRHR(IFG)-1)=-DMGFE(NRHR(IFG))*EP
      MGFEA(NRHR(IFG),NRHR(IFG))=DMGFE(NRHR(IFG))*EP
      MGFEM(NRHR(IFG),NRHR(IFG)-1)=DMGFE(NRHR(IFG))*EP
      MGFEM(NRHR(IFG),NRHR(IFG))=-DMGFE(NRHR(IFG))*EP
!-----
      MGMGA(NRHR(IFG),NRHR(IFG)-1)=-DMGMG(NRHR(IFG))*EP
      MGMGA(NRHR(IFG),NRHR(IFG))=1+DMGMG(NRHR(IFG))*EP
      MGMGM(NRHR(IFG),NRHR(IFG)-1)=DMGMG(NRHR(IFG))*EP
      MGMGM(NRHR(IFG),NRHR(IFG))=1-DMGMG(NRHR(IFG))*EP
      END IF
      RETURN
      END
!-----
!********************************
      SUBROUTINE MAKEAM(IFG)
      IMPLICIT NONE
!-----
      INTEGER*4 MAXLAY,MAXGEN
      PARAMETER (MAXLAY=400,maxgen=50)
!-----END OF COMMON VARIABLES
      REAL*8 VMN(MAXGEN,MAXLAY),VFE(MAXGEN,MAXLAY),VMG(MAXGEN,MAXLAY), &
      VCA(MAXGEN,MAXLAY),XXC(MAXGEN,MAXLAY),NODIST, &
      DRVHR(MAXGEN,MAXLAY),XHR(MAXGEN,MAXLAY),CVHR(MAXGEN,3*MAXLAY), &
      VNOD(MAXGEN,MAXLAY),MONOD(MAXGEN,MAXLAY),VNODTOT(MAXGEN), &
      MONODTOT(MAXGEN),MOSPELAST,MOALMLAST,MOPYRLAST,MOGROLAST, &
      BACKSPE,BACKALM,BACKPYR,BACKGRO,MNGENLA(MAXGEN),FEGENLA(MAXGEN), &
      MGGENLA(MAXGEN),CAGENLA(MAXGEN)
      INTEGER*4 NNOD(MAXGEN),NRHR(MAXGEN),DIFFCODE,DFCODE
      COMMON /PROFIR/ VMN,VFE,VMG,VCA,XXC,NODIST,DRVHR,CVHR,XHR, &
      VNOD,MONOD,VNODTOT,MONODTOT,MOSPELAST,MOALMLAST,MOPYRLAST, &
      MOGROLAST,BACKSPE,BACKALM,BACKPYR,BACKGRO,MNGENLA,FEGENLA, &
      MGGENLA,CAGENLA
      COMMON /PROFII/ NNOD,NRHR,DIFFCODE,DFCODE
!----- end of Profile Common variables
      REAL*8 TC,P,LENGTH,TT,TSS,TSSF,CMN1,CMN2,CFE1,CFE2,CMG1,CMG2, &
      TK,DMNTRACE,DFETRACE,DMGTRACE,DCATRACE, &
      CCA1,CCA2,XCA1,XCA2, &
      DMNMN(MAXLAY),DMNFE(MAXLAY),DMNMG(MAXLAY), &
      DFEMN(MAXLAY),DFEFE(MAXLAY),DFEMG(MAXLAY), &
      DMGMN(MAXLAY),DMGFE(MAXLAY),DMGMG(MAXLAY), &
      YEAR,DT,NT,K, &
      MNMNA(MAXLAY,MAXLAY),MNMNM(MAXLAY,MAXLAY), &
      MNFEA(MAXLAY,MAXLAY),MNFEM(MAXLAY,MAXLAY), &
      MNMGA(MAXLAY,MAXLAY),MNMGM(MAXLAY,MAXLAY), &
      FEMNA(MAXLAY,MAXLAY),FEMNM(MAXLAY,MAXLAY), &
      FEFEA(MAXLAY,MAXLAY),FEFEM(MAXLAY,MAXLAY), &
      FEMGA(MAXLAY,MAXLAY),FEMGM(MAXLAY,MAXLAY), &
      MGMNA(MAXLAY,MAXLAY),MGMNM(MAXLAY,MAXLAY), &
      MGFEA(MAXLAY,MAXLAY),MGFEM(MAXLAY,MAXLAY), &
      MGMGA(MAXLAY,MAXLAY),MGMGM(MAXLAY,MAXLAY), &
      A(3*MAXLAY,3*MAXLAY),M(3*MAXLAY,3*MAXLAY), &
      AIN(3*MAXLAY,3*MAXLAY)
      INTEGER*4 NR,FN
      COMMON /DIFRE/ TC,P,LENGTH,TT,TSS,TSSF,CMN1,CMN2,CFE1,CFE2,CMG1 &
      ,CMG2,TK,DMNTRACE,DFETRACE,DMGTRACE,DCATRACE, &
      CCA1,CCA2,XCA1,XCA2, &
      DMNMN,DMNFE,DMNMG,DFEMN,DFEFE, &
      DFEMG,DMGMN,DMGFE,DMGMG, &
      YEAR,DT,NT,K, &
      MNMNA,MNMNM,MNFEA,MNFEM, &
      MNMGA,MNMGM,FEMNA,FEMNM, &
      FEFEA,FEFEM,FEMGA,FEMGM, &
      MGMNA,MGMNM,MGFEA,MGFEM, &
      MGMGA,MGMGM,A,M,AIN
      COMMON /DIFIN/ NR,FN
!----- end of Common variables
      REAL*8 RJ,RCAL,PI,DMNZERO,AEMN,AVMN,DFEZERO,AEFE,AVFE, &
      DMGZERO,AEMG,AVMG
      COMMON /GLODA/ RJ,RCAL,PI,DMNZERO,AEMN,AVMN,DFEZERO,AEFE,AVFE, &
      DMGZERO,AEMG,AVMG
!----- end of GLOBAL DATA Common variables
      INTEGER*4 IR,IC,IFG
!-----
      DO IR=1,3*MAXLAY
        DO IC=1,3*MAXLAY
          A(IR,IC)=0.0D0
          M(IR,IC)=0.0D0
        END DO
      END DO
!-----
      DO 502,IR=1,NRHR(IFG)
      DO 500,IC=1,NRHR(IFG)
!--
      A(IR,IC)=MNMNA(IR,IC)
      M(IR,IC)=MNMNM(IR,IC)
!--
      A(IR,IC+NRHR(IFG))=MNFEA(IR,IC)
      M(IR,IC+NRHR(IFG))=MNFEM(IR,IC)
!--
      A(IR,IC+2*NRHR(IFG))=MNMGA(IR,IC)
      M(IR,IC+2*NRHR(IFG))=MNMGM(IR,IC)
!--
      A(IR+NRHR(IFG),IC)=FEMNA(IR,IC)
      M(IR+NRHR(IFG),IC)=FEMNM(IR,IC)
!--
      A(IR+NRHR(IFG),IC+NRHR(IFG))=FEFEA(IR,IC)
      M(IR+NRHR(IFG),IC+NRHR(IFG))=FEFEM(IR,IC)
!--
      A(IR+NRHR(IFG),IC+2*NRHR(IFG))=FEMGA(IR,IC)
      M(IR+NRHR(IFG),IC+2*NRHR(IFG))=FEMGM(IR,IC)
!--
      A(IR+2*NRHR(IFG),IC)=MGMNA(IR,IC)
      M(IR+2*NRHR(IFG),IC)=MGMNM(IR,IC)
!--
      A(IR+2*NRHR(IFG),IC+NRHR(IFG))=MGFEA(IR,IC)
      M(IR+2*NRHR(IFG),IC+NRHR(IFG))=MGFEM(IR,IC)
!--
      A(IR+2*NRHR(IFG),IC+2*NRHR(IFG))=MGMGA(IR,IC)
      M(IR+2*NRHR(IFG),IC+2*NRHR(IFG))=MGMGM(IR,IC)
!--
  500 CONTINUE
  502 CONTINUE
!-----
      RETURN
      END
!-----
!********************************
      SUBROUTINE PRINTDAS(IFG)
      IMPLICIT NONE
!-----
      INTEGER*4 MAXLAY,MAXGEN
      PARAMETER (MAXLAY=400,maxgen=50)
!-----END OF COMMON VARIABLES
      REAL*8 VMN(MAXGEN,MAXLAY),VFE(MAXGEN,MAXLAY),VMG(MAXGEN,MAXLAY), &
      VCA(MAXGEN,MAXLAY),XXC(MAXGEN,MAXLAY),NODIST, &
      DRVHR(MAXGEN,MAXLAY),XHR(MAXGEN,MAXLAY),CVHR(MAXGEN,3*MAXLAY), &
      VNOD(MAXGEN,MAXLAY),MONOD(MAXGEN,MAXLAY),VNODTOT(MAXGEN), &
      MONODTOT(MAXGEN),MOSPELAST,MOALMLAST,MOPYRLAST,MOGROLAST, &
      BACKSPE,BACKALM,BACKPYR,BACKGRO,MNGENLA(MAXGEN),FEGENLA(MAXGEN), &
      MGGENLA(MAXGEN),CAGENLA(MAXGEN)
      INTEGER*4 NNOD(MAXGEN),NRHR(MAXGEN),DIFFCODE,DFCODE
      COMMON /PROFIR/ VMN,VFE,VMG,VCA,XXC,NODIST,DRVHR,CVHR,XHR, &
      VNOD,MONOD,VNODTOT,MONODTOT,MOSPELAST,MOALMLAST,MOPYRLAST, &
      MOGROLAST,BACKSPE,BACKALM,BACKPYR,BACKGRO,MNGENLA,FEGENLA, &
      MGGENLA,CAGENLA
      COMMON /PROFII/ NNOD,NRHR,DIFFCODE,DFCODE
!----- end of Profile Common variables
      REAL*8 TC,P,LENGTH,TT,TSS,TSSF,CMN1,CMN2,CFE1,CFE2,CMG1,CMG2, &
      TK,DMNTRACE,DFETRACE,DMGTRACE,DCATRACE, &
      CCA1,CCA2,XCA1,XCA2, &
      DMNMN(MAXLAY),DMNFE(MAXLAY),DMNMG(MAXLAY), &
      DFEMN(MAXLAY),DFEFE(MAXLAY),DFEMG(MAXLAY), &
      DMGMN(MAXLAY),DMGFE(MAXLAY),DMGMG(MAXLAY), &
      YEAR,DT,NT,K, &
      MNMNA(MAXLAY,MAXLAY),MNMNM(MAXLAY,MAXLAY), &
      MNFEA(MAXLAY,MAXLAY),MNFEM(MAXLAY,MAXLAY), &
      MNMGA(MAXLAY,MAXLAY),MNMGM(MAXLAY,MAXLAY), &
      FEMNA(MAXLAY,MAXLAY),FEMNM(MAXLAY,MAXLAY), &
      FEFEA(MAXLAY,MAXLAY),FEFEM(MAXLAY,MAXLAY), &
      FEMGA(MAXLAY,MAXLAY),FEMGM(MAXLAY,MAXLAY), &
      MGMNA(MAXLAY,MAXLAY),MGMNM(MAXLAY,MAXLAY), &
      MGFEA(MAXLAY,MAXLAY),MGFEM(MAXLAY,MAXLAY), &
      MGMGA(MAXLAY,MAXLAY),MGMGM(MAXLAY,MAXLAY), &
      A(3*MAXLAY,3*MAXLAY),M(3*MAXLAY,3*MAXLAY), &
      AIN(3*MAXLAY,3*MAXLAY)
      INTEGER*4 NR,FN
      COMMON /DIFRE/ TC,P,LENGTH,TT,TSS,TSSF,CMN1,CMN2,CFE1,CFE2,CMG1 &
      ,CMG2,TK,DMNTRACE,DFETRACE,DMGTRACE,DCATRACE, &
      CCA1,CCA2,XCA1,XCA2, &
      DMNMN,DMNFE,DMNMG,DFEMN,DFEFE, &
      DFEMG,DMGMN,DMGFE,DMGMG, &
      YEAR,DT,NT,K, &
      MNMNA,MNMNM,MNFEA,MNFEM, &
      MNMGA,MNMGM,FEMNA,FEMNM, &
      FEFEA,FEFEM,FEMGA,FEMGM, &
      MGMNA,MGMNM,MGFEA,MGFEM, &
      MGMGA,MGMGM,A,M,AIN
      COMMON /DIFIN/ NR,FN
!----- end of Common variables
      REAL*8 RJ,RCAL,PI,DMNZERO,AEMN,AVMN,DFEZERO,AEFE,AVFE, &
      DMGZERO,AEMG,AVMG
      COMMON /GLODA/ RJ,RCAL,PI,DMNZERO,AEMN,AVMN,DFEZERO,AEFE,AVFE, &
      DMGZERO,AEMG,AVMG
!----- end of GLOBAL DATA Common variables
      INTEGER*4 I,IFG
!-----
      WRITE (6,1000) (CVHR(IFG,I),I=1,3*NRHR(IFG))
!      WRITE (10,1000) (CVHR(IFG,I),I=1,3*NRHR(IFG))
 1000 FORMAT (300(F10.5,2X))
!-----
      RETURN
      END
!-----
!********************************
      SUBROUTINE PRINTMAA(IFG)
      IMPLICIT NONE
!-----
      INTEGER*4 MAXLAY,MAXGEN
      PARAMETER (MAXLAY=400,maxgen=50)
!-----END OF COMMON VARIABLES
      REAL*8 VMN(MAXGEN,MAXLAY),VFE(MAXGEN,MAXLAY),VMG(MAXGEN,MAXLAY), &
      VCA(MAXGEN,MAXLAY),XXC(MAXGEN,MAXLAY),NODIST, &
      DRVHR(MAXGEN,MAXLAY),XHR(MAXGEN,MAXLAY),CVHR(MAXGEN,3*MAXLAY), &
      VNOD(MAXGEN,MAXLAY),MONOD(MAXGEN,MAXLAY),VNODTOT(MAXGEN), &
      MONODTOT(MAXGEN),MOSPELAST,MOALMLAST,MOPYRLAST,MOGROLAST, &
      BACKSPE,BACKALM,BACKPYR,BACKGRO,MNGENLA(MAXGEN),FEGENLA(MAXGEN), &
      MGGENLA(MAXGEN),CAGENLA(MAXGEN)
      INTEGER*4 NNOD(MAXGEN),NRHR(MAXGEN),DIFFCODE,DFCODE
      COMMON /PROFIR/ VMN,VFE,VMG,VCA,XXC,NODIST,DRVHR,CVHR,XHR, &
      VNOD,MONOD,VNODTOT,MONODTOT,MOSPELAST,MOALMLAST,MOPYRLAST, &
      MOGROLAST,BACKSPE,BACKALM,BACKPYR,BACKGRO,MNGENLA,FEGENLA, &
      MGGENLA,CAGENLA
      COMMON /PROFII/ NNOD,NRHR,DIFFCODE,DFCODE
!----- end of Profile Common variables
      REAL*8 TC,P,LENGTH,TT,TSS,TSSF,CMN1,CMN2,CFE1,CFE2,CMG1,CMG2, &
      TK,DMNTRACE,DFETRACE,DMGTRACE,DCATRACE, &
      CCA1,CCA2,XCA1,XCA2, &
      DMNMN(MAXLAY),DMNFE(MAXLAY),DMNMG(MAXLAY), &
      DFEMN(MAXLAY),DFEFE(MAXLAY),DFEMG(MAXLAY), &
      DMGMN(MAXLAY),DMGFE(MAXLAY),DMGMG(MAXLAY), &
      YEAR,DT,NT,K, &
      MNMNA(MAXLAY,MAXLAY),MNMNM(MAXLAY,MAXLAY), &
      MNFEA(MAXLAY,MAXLAY),MNFEM(MAXLAY,MAXLAY), &
      MNMGA(MAXLAY,MAXLAY),MNMGM(MAXLAY,MAXLAY), &
      FEMNA(MAXLAY,MAXLAY),FEMNM(MAXLAY,MAXLAY), &
      FEFEA(MAXLAY,MAXLAY),FEFEM(MAXLAY,MAXLAY), &
      FEMGA(MAXLAY,MAXLAY),FEMGM(MAXLAY,MAXLAY), &
      MGMNA(MAXLAY,MAXLAY),MGMNM(MAXLAY,MAXLAY), &
      MGFEA(MAXLAY,MAXLAY),MGFEM(MAXLAY,MAXLAY), &
      MGMGA(MAXLAY,MAXLAY),MGMGM(MAXLAY,MAXLAY), &
      A(3*MAXLAY,3*MAXLAY),M(3*MAXLAY,3*MAXLAY), &
      AIN(3*MAXLAY,3*MAXLAY)
      INTEGER*4 NR,FN
      COMMON /DIFRE/ TC,P,LENGTH,TT,TSS,TSSF,CMN1,CMN2,CFE1,CFE2,CMG1 &
      ,CMG2,TK,DMNTRACE,DFETRACE,DMGTRACE,DCATRACE, &
      CCA1,CCA2,XCA1,XCA2, &
      DMNMN,DMNFE,DMNMG,DFEMN,DFEFE, &
      DFEMG,DMGMN,DMGFE,DMGMG, &
      YEAR,DT,NT,K, &
      MNMNA,MNMNM,MNFEA,MNFEM, &
      MNMGA,MNMGM,FEMNA,FEMNM, &
      FEFEA,FEFEM,FEMGA,FEMGM, &
      MGMNA,MGMNM,MGFEA,MGFEM, &
      MGMGA,MGMGM,A,M,AIN
      COMMON /DIFIN/ NR,FN
!----- end of Common variables
      REAL*8 RJ,RCAL,PI,DMNZERO,AEMN,AVMN,DFEZERO,AEFE,AVFE, &
      DMGZERO,AEMG,AVMG
      COMMON /GLODA/ RJ,RCAL,PI,DMNZERO,AEMN,AVMN,DFEZERO,AEFE,AVFE, &
      DMGZERO,AEMG,AVMG
!----- end of GLOBAL DATA Common variables
      INTEGER*4 N,IFG
!      INTEGER*4 IR,IC
!-----
      N=3*NRHR(IFG)
!1      WRITE (12,1010) (IR,IR=1,N)
!1      WRITE (14,1010) (IR,IR=1,N)
!1      WRITE (16,1010) (IR,IR=1,N)
!1 1010 FORMAT (300(I12,2X))
!1      DO 500,IR=1,N
!1      WRITE (12,1000) (A(IR,IC),IC=1,N)
!1      WRITE (14,1000) (AIN(IR,IC),IC=1,N)
!1      WRITE (16,1000) (M(IR,IC),IC=1,N)
!1 1000 FORMAT (300(1PE12.5,2X))
!1  500 CONTINUE
!-----
      RETURN
      END
!-----
!********************************
      SUBROUTINE MULMC(IFG)
      IMPLICIT NONE
!-----
      INTEGER*4 MAXLAY,MAXGEN
      PARAMETER (MAXLAY=400,maxgen=50)
!-----END OF COMMON VARIABLES
      REAL*8 VMN(MAXGEN,MAXLAY),VFE(MAXGEN,MAXLAY),VMG(MAXGEN,MAXLAY), &
      VCA(MAXGEN,MAXLAY),XXC(MAXGEN,MAXLAY),NODIST, &
      DRVHR(MAXGEN,MAXLAY),XHR(MAXGEN,MAXLAY),CVHR(MAXGEN,3*MAXLAY), &
      VNOD(MAXGEN,MAXLAY),MONOD(MAXGEN,MAXLAY),VNODTOT(MAXGEN), &
      MONODTOT(MAXGEN),MOSPELAST,MOALMLAST,MOPYRLAST,MOGROLAST, &
      BACKSPE,BACKALM,BACKPYR,BACKGRO,MNGENLA(MAXGEN),FEGENLA(MAXGEN), &
      MGGENLA(MAXGEN),CAGENLA(MAXGEN)
      INTEGER*4 NNOD(MAXGEN),NRHR(MAXGEN),DIFFCODE,DFCODE
      COMMON /PROFIR/ VMN,VFE,VMG,VCA,XXC,NODIST,DRVHR,CVHR,XHR, &
      VNOD,MONOD,VNODTOT,MONODTOT,MOSPELAST,MOALMLAST,MOPYRLAST, &
      MOGROLAST,BACKSPE,BACKALM,BACKPYR,BACKGRO,MNGENLA,FEGENLA, &
      MGGENLA,CAGENLA
      COMMON /PROFII/ NNOD,NRHR,DIFFCODE,DFCODE
!----- end of Profile Common variables
      REAL*8 TC,P,LENGTH,TT,TSS,TSSF,CMN1,CMN2,CFE1,CFE2,CMG1,CMG2, &
      TK,DMNTRACE,DFETRACE,DMGTRACE,DCATRACE, &
      CCA1,CCA2,XCA1,XCA2, &
      DMNMN(MAXLAY),DMNFE(MAXLAY),DMNMG(MAXLAY), &
      DFEMN(MAXLAY),DFEFE(MAXLAY),DFEMG(MAXLAY), &
      DMGMN(MAXLAY),DMGFE(MAXLAY),DMGMG(MAXLAY), &
      YEAR,DT,NT,K, &
      MNMNA(MAXLAY,MAXLAY),MNMNM(MAXLAY,MAXLAY), &
      MNFEA(MAXLAY,MAXLAY),MNFEM(MAXLAY,MAXLAY), &
      MNMGA(MAXLAY,MAXLAY),MNMGM(MAXLAY,MAXLAY), &
      FEMNA(MAXLAY,MAXLAY),FEMNM(MAXLAY,MAXLAY), &
      FEFEA(MAXLAY,MAXLAY),FEFEM(MAXLAY,MAXLAY), &
      FEMGA(MAXLAY,MAXLAY),FEMGM(MAXLAY,MAXLAY), &
      MGMNA(MAXLAY,MAXLAY),MGMNM(MAXLAY,MAXLAY), &
      MGFEA(MAXLAY,MAXLAY),MGFEM(MAXLAY,MAXLAY), &
      MGMGA(MAXLAY,MAXLAY),MGMGM(MAXLAY,MAXLAY), &
      A(3*MAXLAY,3*MAXLAY),M(3*MAXLAY,3*MAXLAY), &
      AIN(3*MAXLAY,3*MAXLAY)
      INTEGER*4 NR,FN
      COMMON /DIFRE/ TC,P,LENGTH,TT,TSS,TSSF,CMN1,CMN2,CFE1,CFE2,CMG1 &
      ,CMG2,TK,DMNTRACE,DFETRACE,DMGTRACE,DCATRACE, &
      CCA1,CCA2,XCA1,XCA2, &
      DMNMN,DMNFE,DMNMG,DFEMN,DFEFE, &
      DFEMG,DMGMN,DMGFE,DMGMG, &
      YEAR,DT,NT,K, &
      MNMNA,MNMNM,MNFEA,MNFEM, &
      MNMGA,MNMGM,FEMNA,FEMNM, &
      FEFEA,FEFEM,FEMGA,FEMGM, &
      MGMNA,MGMNM,MGFEA,MGFEM, &
      MGMGA,MGMGM,A,M,AIN
      COMMON /DIFIN/ NR,FN
!----- end of Common variables
      REAL*8 RJ,RCAL,PI,DMNZERO,AEMN,AVMN,DFEZERO,AEFE,AVFE, &
      DMGZERO,AEMG,AVMG
      COMMON /GLODA/ RJ,RCAL,PI,DMNZERO,AEMN,AVMN,DFEZERO,AEFE,AVFE, &
      DMGZERO,AEMG,AVMG
!----- end of GLOBAL DATA Common variables
      INTEGER*4 IR,IC,IFG
      REAL*8 XC(3*MAXLAY)
!-----
      DO IR=1,3*NRHR(IFG)
        XC(IR)=0.0D0
        DO IC=1,3*NRHR(IFG)
          XC(IR)=XC(IR)+M(IR,IC)*CVHR(IFG,IC)
        END DO
      END DO
      DO IR=1,3*NRHR(IFG)
        CVHR(IFG,IR)=XC(IR)
      END DO
!-----
      RETURN
      END
!-----
!********************************
      SUBROUTINE MULAIN(IFG,OPSY)
      IMPLICIT NONE
!-----
      INTEGER*4 MAXLAY,MAXGEN
      PARAMETER (MAXLAY=400,maxgen=50)
!-----END OF COMMON VARIABLES
      REAL*8 VMN(MAXGEN,MAXLAY),VFE(MAXGEN,MAXLAY),VMG(MAXGEN,MAXLAY), &
      VCA(MAXGEN,MAXLAY),XXC(MAXGEN,MAXLAY),NODIST, &
      DRVHR(MAXGEN,MAXLAY),XHR(MAXGEN,MAXLAY),CVHR(MAXGEN,3*MAXLAY), &
      VNOD(MAXGEN,MAXLAY),MONOD(MAXGEN,MAXLAY),VNODTOT(MAXGEN), &
      MONODTOT(MAXGEN),MOSPELAST,MOALMLAST,MOPYRLAST,MOGROLAST, &
      BACKSPE,BACKALM,BACKPYR,BACKGRO,MNGENLA(MAXGEN),FEGENLA(MAXGEN), &
      MGGENLA(MAXGEN),CAGENLA(MAXGEN)
      INTEGER*4 NNOD(MAXGEN),NRHR(MAXGEN),DIFFCODE,DFCODE
      COMMON /PROFIR/ VMN,VFE,VMG,VCA,XXC,NODIST,DRVHR,CVHR,XHR, &
      VNOD,MONOD,VNODTOT,MONODTOT,MOSPELAST,MOALMLAST,MOPYRLAST, &
      MOGROLAST,BACKSPE,BACKALM,BACKPYR,BACKGRO,MNGENLA,FEGENLA, &
      MGGENLA,CAGENLA
      COMMON /PROFII/ NNOD,NRHR,DIFFCODE,DFCODE
!----- end of Profile Common variables
      REAL*8 TC,P,LENGTH,TT,TSS,TSSF,CMN1,CMN2,CFE1,CFE2,CMG1,CMG2, &
      TK,DMNTRACE,DFETRACE,DMGTRACE,DCATRACE, &
      CCA1,CCA2,XCA1,XCA2, &
      DMNMN(MAXLAY),DMNFE(MAXLAY),DMNMG(MAXLAY), &
      DFEMN(MAXLAY),DFEFE(MAXLAY),DFEMG(MAXLAY), &
      DMGMN(MAXLAY),DMGFE(MAXLAY),DMGMG(MAXLAY), &
      YEAR,DT,NT,K, &
      MNMNA(MAXLAY,MAXLAY),MNMNM(MAXLAY,MAXLAY), &
      MNFEA(MAXLAY,MAXLAY),MNFEM(MAXLAY,MAXLAY), &
      MNMGA(MAXLAY,MAXLAY),MNMGM(MAXLAY,MAXLAY), &
      FEMNA(MAXLAY,MAXLAY),FEMNM(MAXLAY,MAXLAY), &
      FEFEA(MAXLAY,MAXLAY),FEFEM(MAXLAY,MAXLAY), &
      FEMGA(MAXLAY,MAXLAY),FEMGM(MAXLAY,MAXLAY), &
      MGMNA(MAXLAY,MAXLAY),MGMNM(MAXLAY,MAXLAY), &
      MGFEA(MAXLAY,MAXLAY),MGFEM(MAXLAY,MAXLAY), &
      MGMGA(MAXLAY,MAXLAY),MGMGM(MAXLAY,MAXLAY), &
      A(3*MAXLAY,3*MAXLAY),M(3*MAXLAY,3*MAXLAY), &
      AIN(3*MAXLAY,3*MAXLAY)
      INTEGER*4 NR,FN
      COMMON /DIFRE/ TC,P,LENGTH,TT,TSS,TSSF,CMN1,CMN2,CFE1,CFE2,CMG1 &
      ,CMG2,TK,DMNTRACE,DFETRACE,DMGTRACE,DCATRACE, &
      CCA1,CCA2,XCA1,XCA2, &
      DMNMN,DMNFE,DMNMG,DFEMN,DFEFE, &
      DFEMG,DMGMN,DMGFE,DMGMG, &
      YEAR,DT,NT,K, &
      MNMNA,MNMNM,MNFEA,MNFEM, &
      MNMGA,MNMGM,FEMNA,FEMNM, &
      FEFEA,FEFEM,FEMGA,FEMGM, &
      MGMNA,MGMNM,MGFEA,MGFEM, &
      MGMGA,MGMGM,A,M,AIN
      COMMON /DIFIN/ NR,FN
!----- end of Common variables
      REAL*8 RJ,RCAL,PI,DMNZERO,AEMN,AVMN,DFEZERO,AEFE,AVFE, &
      DMGZERO,AEMG,AVMG
      COMMON /GLODA/ RJ,RCAL,PI,DMNZERO,AEMN,AVMN,DFEZERO,AEFE,AVFE, &
      DMGZERO,AEMG,AVMG
!----- end of GLOBAL DATA Common variables
      INTEGER*4 IR,IC,IFG,OPSY(3)
      REAL*8 XC(3*MAXLAY),RHS(3*MAXLAY)
!-----
!      DO 500,IR=1,3*NRHR(IFG)
!      XC(IR)=0.0D0
!      DO 500,IC=1,3*NRHR(IFG)
!      XC(IR)=XC(IR)+AIN(IR,IC)*CVHR(IFG,IC)
!  500 CONTINUE
!      DO 510,IR=1,3*NRHR(IFG)
!  510 CVHR(IFG,IR)=XC(IR)
!
      DO IC=1,3*NRHR(IFG)
        RHS(IC)=CVHR(IFG,IC)
      END DO
!      IF (OPSY(1).EQ.1) RHS(NRHR(IFG))=0.0D0
!      IF (OPSY(2).EQ.1) RHS(2*NRHR(IFG))=0.0D0
!      IF (OPSY(3).EQ.1) RHS(3*NRHR(IFG))=0.0D0
!-----
      DO IR=1,3*NRHR(IFG)
        XC(IR)=0.0D0
        DO IC=1,3*NRHR(IFG)
          XC(IR)=XC(IR)+AIN(IR,IC)*RHS(IC)
        END DO
      END DO
      DO IR=1,3*NRHR(IFG)
        CVHR(IFG,IR)=XC(IR)
      END DO
!-----
      RETURN
      END
!=========================================================================
!
!      This file contains a number of simple matrix manipulation routines.
!      All of the routines operate on REAL*8 matrices.
!      The size is passed as the first parameters:  N rows by M columns.
!      If there is no M parameter then a N by N square matrix is assumed.
!
!    CALL MCOPY (N,M, X, Y)        Copies matrix: Y=X
!
!    CALL MADD (N,M, X, Y, Z)      Adds matrices: Z=X + Y
!                                    Z can be X or Y
!    CALL MSUB (N,M, X, Y, Z)      Subtracts matrices: Z=X - Y
!                                    Z can be X or Y
!    CALL MSCALARMULT (N,M, C, X, Y) Scalar multiply: Y=C*X
!                                    C is real scalar; Y can be X
!    CALL MZERO (N,M, X)           Zeros all elements in X
!
!    CALL MDIAG (N, V, X)          Formats a vector (V) into a diagonal
!                                    matrix (X)
!    CALL MIDENTITY (N, X)         Make identity matrix in X
!                                                   t
!    CALL MTRANSPOSE (N,M, X, Y)   Transposes: Y=X ;   Y cannot be X
!
!    CALL MMULT (N,M,L, X, Y, Z)   Matrix multiply: Z=X*Y
!                                    X is N by M and Y is M by L.
!                                    Z cannot be X or Y
!                                                          -1
!    CALL MINVERT (N, X, Y)        Matrix inversion:  Y=X
!                                    X gets LU decomposition.
!
      SUBROUTINE MCOPY (N, M, MATRIX1, MATRIX2)
      INTEGER  N, M, I
      REAL*8   MATRIX1(1), MATRIX2(1)
      DO 100 I=1, N*M
          MATRIX2(I)=MATRIX1(I)
100   CONTINUE
      RETURN
      END
!
      SUBROUTINE MADD (N, M, MATRIX1, MATRIX2, MATRIX3)
      INTEGER  N, M, I
      REAL*8   MATRIX1(1), MATRIX2(1),  MATRIX3(1)
      DO 100 I=1, N*M
          MATRIX3(I)=MATRIX1(I) + MATRIX2(I)
100   CONTINUE
      RETURN
      END
!
      SUBROUTINE MSUB (N, M, MATRIX1, MATRIX2, MATRIX3)
      INTEGER  N, M, I
      REAL*8   MATRIX1(1), MATRIX2(1),  MATRIX3(1)
      DO 100 I=1, N*M
          MATRIX3(I)=MATRIX1(I) - MATRIX2(I)
100   CONTINUE
      RETURN
      END
!
      SUBROUTINE MSCALARMULT (N, M, C, MATRIX1, MATRIX2)
      INTEGER  N, M, I
      REAL*8   C, MATRIX1(1), MATRIX2(1)
      DO 100 I=1, N*M
          MATRIX2(I)=C*MATRIX1(I)
100   CONTINUE
      RETURN
      END
!
      SUBROUTINE MZERO (N, M, MATRIX1)
      INTEGER  N, M, I
      REAL*8   MATRIX1(1)
      DO 100 I=1, N*M
          MATRIX1(I)=0.0D0
100   CONTINUE

      RETURN
      END
!
      SUBROUTINE MDIAG (N,  VECTOR, MATRIX)
      INTEGER  N,  I, J
      REAL*8   VECTOR(1), MATRIX(N,N)

      DO 110 I=1, N
        DO 100 J=1, N
          MATRIX(I,J)=0.0
100     CONTINUE
        MATRIX(I,I)=VECTOR(I)
110   CONTINUE
      RETURN
      END
!
      SUBROUTINE MIDENTITY (N, MATRIX)
      INTEGER  N, I, J
      REAL*8   MATRIX(N,N)
      DO I=1, N
        DO J=1, N
          MATRIX(I,J)=0.0
        END DO
        MATRIX(I,I)=1.0
      END DO
      RETURN
      END
!
      SUBROUTINE MTRANSPOSE (N, M, MATRIX1, MATRIX2)
      INTEGER  N, M, I, J
      REAL*8   MATRIX1(N,M), MATRIX2(M,N)
      DO I=1, N
        DO J=1, M
             MATRIX2(I,J)=MATRIX1(J,I)
        END DO
      END DO
      RETURN
      END
!
      SUBROUTINE MMULT (N, M, L, MATRIX1, MATRIX2, MATRIX3)
      INTEGER  N, M, L,  I, J, K, MAXLAY
      PARAMETER (MAXLAY=400)
!DC      REAL*8   MATRIX1(N,M), MATRIX2(M,L), MATRIX3(N,L),   SUM
      REAL*8 MATRIX1(3*MAXLAY,3*MAXLAY),MATRIX2(3*MAXLAY,3*MAXLAY), &
      MATRIX3(3*MAXLAY,3*MAXLAY),SUM
      DO I=1, N
        DO J=1, L
          SUM=0.0
          DO K=1, M
            SUM=SUM + MATRIX1(I,K)*MATRIX2(K,J)
          END DO
          MATRIX3(I,J)=SUM
        END DO
      END DO
      RETURN
      END
!
      SUBROUTINE MINVERT (N, MATRIX1, MATRIX2,PIVOT)
      INTEGER  N, MAXLAY
      PARAMETER (MAXLAY=400)
!DC      REAL*8   MATRIX1(N,N), MATRIX2(N,N)
      REAL*8   MATRIX1(3*MAXLAY,3*MAXLAY), MATRIX2(3*MAXLAY,3*MAXLAY)
      INTEGER  NMAX
      PARAMETER (NMAX=3*MAXLAY)
      INTEGER  I, J, INDX(NMAX), IZ, PIVOT
      REAL*8   DET(2), WORK(NMAX)
      PIVOT=0

      IF (N .GT. NMAX) THEN
          WRITE (*,'(1X,A,I3)') &
          'Exceeded maximum matrix size for inversion.  Max=', NMAX
          STOP
      ENDIF
      CALL dgefa (MATRIX1, N, N, INDX, IZ)
      IF (IZ .GT. 0) THEN
          WRITE (*,'(1X,A,I3)') &
          'Encountered a zero pivot at element ', IZ
!          STOP
      PIVOT=1
      RETURN
      ENDIF
      DO 100 I=1, N
        DO 110 J=1, N
           MATRIX2(I,J)=MATRIX1(I,J)
110     CONTINUE
100   CONTINUE
      CALL dgedi (MATRIX2, N, N, INDX, DET, WORK, 1 )

      RETURN
      END
!
!
!=========================================================================
      subroutine dgefa(a,lda,n,ipvt,info)
      integer lda,n,ipvt(*),info, MAXLAY
      PARAMETER (MAXLAY=400)
!DC      double precision a(lda,1)
      REAL*8 a(3*MAXLAY,*)
!
!     dgefa factors a double precision matrix by gaussian elimination.
!
!     dgefa is usually called by dgeco, but it can be called
!     directly with a saving in time if  rcond  is not needed.
!     (time for dgeco)=(1 + 9/n)*(time for dgefa) .
!
!     on entry
!
!        a       double precision(lda, n)
!                the matrix to be factored.
!
!        lda     integer
!                the leading dimension of the array  a .
!
!        n       integer
!                the order of the matrix  a .
!
!     on return
!
!        a       an upper triangular matrix and the multipliers
!                which were used to obtain it.
!                the factorization can be written  a=l*u  where
!                l  is a product of permutation and unit lower
!                triangular matrices and  u  is upper triangular.
!
!        ipvt    integer(n)
!                an integer vector of pivot indices.
!
!        info    integer
!     =0  normal value.
!     =k  if  u(k,k) .eq. 0.0 .  this is not an error
!                     condition for this subroutine, but it does
!                     indicate that dgesl or dgedi will divide by zero
!                     if called.  use  rcond  in dgeco for a reliable
!                     indication of singularity.
!
!     linpack. this version dated 08/14/78 .
!     cleve moler, university of new mexico, argonne national lab.
!
!     subroutines and functions
!
!     blas daxpy,dscal,idamax
!
!     internal variables
!
      double precision t
      integer idamax,j,k,kp1,l,nm1
!
!
!     gaussian elimination with partial pivoting
!
      info=0
      nm1=n - 1
      if (nm1 .lt. 1) go to 70
      do 60 k=1, nm1
         kp1=k + 1
!
!        find l=pivot index
!
         l=idamax(n-k+1,a(k,k),1) + k - 1
         ipvt(k)=l
!
!        zero pivot implies this column already triangularized
!
         if (a(l,k) .eq. 0.0d0) go to 40
!
!           interchange if necessary
!
            if (l .eq. k) go to 10
               t=a(l,k)
               a(l,k)=a(k,k)
               a(k,k)=t
   10       continue
!
!           compute multipliers
!
            t=-1.0d0/a(k,k)
            call dscal(n-k,t,a(k+1,k),1)
!
!           row elimination with column indexing
!
            do 30 j=kp1, n
               t=a(l,j)
               if (l .eq. k) go to 20
                  a(l,j)=a(k,j)
                  a(k,j)=t
   20          continue
               call daxpy(n-k,t,a(k+1,k),1,a(k+1,j),1)
   30       continue
         go to 50
   40    continue
            info=k
   50    continue
   60 continue
   70 continue
      ipvt(n)=n
      if (a(n,n) .eq. 0.0d0) info=n
      return
      end


!=========================================================================
      subroutine dgedi(a,lda,n,ipvt,det,work,job)
      integer lda,n,ipvt(*),job,MAXLAY
      PARAMETER (MAXLAY=400)
!DC      double precision a(lda,1),det(2),work(1)
      REAL*8 a(3*MAXLAY,*),det(*),work(*)
!
!     dgedi computes the determinant and inverse of a matrix
!     using the factors computed by dgeco or dgefa.
!
!     on entry
!
!        a       double precision(lda, n)
!                the output from dgeco or dgefa.
!
!        lda     integer
!                the leading dimension of the array  a .
!
!        n       integer
!                the order of the matrix  a .
!
!        ipvt    integer(n)
!                the pivot vector from dgeco or dgefa.
!
!        work    double precision(n)
!                work vector.  contents destroyed.
!
!        job     integer
!     =11   both determinant and inverse.
!     =01   inverse only.
!     =10   determinant only.
!
!     on return
!
!        a       inverse of original matrix if requested.
!                otherwise unchanged.
!
!        det     double precision(2)
!                determinant of original matrix if requested.
!                otherwise not referenced.
!                determinant=det(1) * 10.0**det(2)
!                with  1.0 .le. dabs(det(1)) .lt. 10.0
!                or  det(1) .eq. 0.0 .
!
!     error condition
!
!        a division by zero will occur if the input factor contains
!        a zero on the diagonal and the inverse is requested.
!        it will not occur if the subroutines are called correctly
!        and if dgeco has set rcond .gt. 0.0 or dgefa has set
!        info .eq. 0 .
!
!     linpack. this version dated 08/14/78 .
!     cleve moler, university of new mexico, argonne national lab.
!
!     subroutines and functions
!
!     blas daxpy,dscal,dswap
!     fortran dabs,mod
!
!     internal variables
!
      double precision t
      double precision ten
      integer i,j,k,kb,kp1,l,nm1
!
!
!     compute determinant
!
      if (job/10 .eq. 0) go to 70
         det(1)=1.0d0
         det(2)=0.0d0
         ten=10.0d0
         do 50 i=1, n
            if (ipvt(i) .ne. i) det(1)=-det(1)
            det(1)=a(i,i)*det(1)
!        ...exit
            if (det(1) .eq. 0.0d0) go to 60
   10       if (dabs(det(1)) .ge. 1.0d0) go to 20
               det(1)=ten*det(1)
               det(2)=det(2) - 1.0d0
            go to 10
   20       continue
   30       if (dabs(det(1)) .lt. ten) go to 40
               det(1)=det(1)/ten
               det(2)=det(2) + 1.0d0
            go to 30
   40       continue
   50    continue
   60    continue
   70 continue
!
!     compute inverse(u)
!
      if (mod(job,10) .eq. 0) go to 150
         do 100 k=1, n
            a(k,k)=1.0d0/a(k,k)
            t=-a(k,k)
            call dscal(k-1,t,a(1,k),1)
            kp1=k + 1
            if (n .lt. kp1) go to 90
            do 80 j=kp1, n
               t=a(k,j)
               a(k,j)=0.0d0
               call daxpy(k,t,a(1,k),1,a(1,j),1)
   80       continue
   90       continue
  100    continue
!
!        form inverse(u)*inverse(l)
!
         nm1=n - 1
         if (nm1 .lt. 1) go to 140
         do 130 kb=1, nm1
            k=n - kb
            kp1=k + 1
            do 110 i=kp1, n
               work(i)=a(i,k)
               a(i,k)=0.0d0
  110       continue
            do 120 j=kp1, n
               t=work(j)
               call daxpy(n,t,a(1,j),1,a(1,k),1)
  120       continue
            l=ipvt(k)
            if (l .ne. k) call dswap(n,a(1,k),1,a(1,l),1)
  130    continue
  140    continue
  150 continue
      return
      end


!=========================================================================


!
!  Authors:
!  ========
!
!> \author Univ. of Tennessee 
!> \author Univ. of California Berkeley 
!> \author Univ. of Colorado Denver 
!> \author NAG Ltd. 
!
!> \date November 2011
!
!> \ingroup double_blas_level1
!
!> \par Further Details:
!  =====================
!>
!> \verbatim
!>
!>     jack dongarra, linpack, 3/11/78.
!>     modified 12/3/93, array(1) declarations changed to array(*)
!> \endverbatim
!>
!  =====================================================================
      SUBROUTINE DAXPY(N,DA,DX,INCX,DY,INCY)
!
!  -- Reference BLAS level1 routine (version 3.4.0) --
!  -- Reference BLAS is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     November 2011
!
!     .. Scalar Arguments ..
      DOUBLE PRECISION DA
      INTEGER INCX,INCY,N
!     ..
!     .. Array Arguments ..
      DOUBLE PRECISION DX(*),DY(*)
!     ..
!
!  =====================================================================
!
!     .. Local Scalars ..
      INTEGER I,IX,IY,M,MP1
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC MOD
!     ..
      IF (N.LE.0) RETURN
      IF (DA.EQ.0.0d0) RETURN
      IF (INCX.EQ.1 .AND. INCY.EQ.1) THEN
!
!        code for both increments equal to 1
!
!
!        clean-up loop
!
         M = MOD(N,4)
         IF (M.NE.0) THEN
            DO I = 1,M
               DY(I) = DY(I) + DA*DX(I)
            END DO
         END IF
         IF (N.LT.4) RETURN
         MP1 = M + 1
         DO I = MP1,N,4
            DY(I) = DY(I) + DA*DX(I)
            DY(I+1) = DY(I+1) + DA*DX(I+1)
            DY(I+2) = DY(I+2) + DA*DX(I+2)
            DY(I+3) = DY(I+3) + DA*DX(I+3)
         END DO
      ELSE
!
!        code for unequal increments or equal increments
!          not equal to 1
!
         IX = 1
         IY = 1
         IF (INCX.LT.0) IX = (-N+1)*INCX + 1
         IF (INCY.LT.0) IY = (-N+1)*INCY + 1
         DO I = 1,N
          DY(IY) = DY(IY) + DA*DX(IX)
          IX = IX + INCX
          IY = IY + INCY
         END DO
      END IF
      RETURN
      END

!=========================================================================
      subroutine daxpy2(n,da,dx,incx,dy,incy)
!
!     constant times a vector plus a vector.
!     uses unrolled loops for increments equal to one.
!     jack dongarra, linpack, 3/11/78.
!
      double precision dx(1),dy(1),da
      integer i,incx,incy,ix,iy,m,mp1,n
!
      if(n.le.0)return
      if (da .eq. 0.0d0) return
      if(incx.eq.1.and.incy.eq.1)go to 20
!
!        code for unequal increments or equal increments
!          not equal to 1
!
      ix=1
      iy=1
      if(incx.lt.0)ix=(-n+1)*incx + 1
      if(incy.lt.0)iy=(-n+1)*incy + 1
      do 10 i=1,n
        dy(iy)=dy(iy) + da*dx(ix)
        ix=ix + incx
        iy=iy + incy
   10 continue
      return
!
!        code for both increments equal to 1
!
!
!        clean-up loop
!
   20 m=mod(n,4)
      if( m .eq. 0 ) go to 40
      do 30 i=1,m
        dy(i)=dy(i) + da*dx(i)
   30 continue
      if( n .lt. 4 ) return
   40 mp1=m + 1
      do 50 i=mp1,n,4
        dy(i)=dy(i) + da*dx(i)
        dy(i + 1)=dy(i + 1) + da*dx(i + 1)
        dy(i + 2)=dy(i + 2) + da*dx(i + 2)
        dy(i + 3)=dy(i + 3) + da*dx(i + 3)
   50 continue
      return
      end


!=========================================================================

!
!  Authors:
!  ========
!
!> \author Univ. of Tennessee 
!> \author Univ. of California Berkeley 
!> \author Univ. of Colorado Denver 
!> \author NAG Ltd. 
!
!> \date November 2011
!
!> \ingroup double_blas_level1
!
!> \par Further Details:
!  =====================
!>
!> \verbatim
!>
!>     jack dongarra, linpack, 3/11/78.
!>     modified 3/93 to return if incx .le. 0.
!>     modified 12/3/93, array(1) declarations changed to array(*)
!> \endverbatim
!>
!  =====================================================================
      SUBROUTINE DSCAL(N,DA,DX,INCX)
!
!  -- Reference BLAS level1 routine (version 3.4.0) --
!  -- Reference BLAS is a software package provided by Univ. of Tennessee,    --
!  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
!     November 2011
!
!     .. Scalar Arguments ..
      DOUBLE PRECISION DA
      INTEGER INCX,N
!     ..
!     .. Array Arguments ..
      DOUBLE PRECISION DX(*)
!     ..
!
!  =====================================================================
!
!     .. Local Scalars ..
      INTEGER I,M,MP1,NINCX
!     ..
!     .. Intrinsic Functions ..
      INTRINSIC MOD
!     ..
      IF (N.LE.0 .OR. INCX.LE.0) RETURN
      IF (INCX.EQ.1) THEN
!
!        code for increment equal to 1
!
!
!        clean-up loop
!
         M = MOD(N,5)
         IF (M.NE.0) THEN
            DO I = 1,M
               DX(I) = DA*DX(I)
            END DO
            IF (N.LT.5) RETURN
         END IF
         MP1 = M + 1
         DO I = MP1,N,5
            DX(I) = DA*DX(I)
            DX(I+1) = DA*DX(I+1)
            DX(I+2) = DA*DX(I+2)
            DX(I+3) = DA*DX(I+3)
            DX(I+4) = DA*DX(I+4)
         END DO
      ELSE
!
!        code for increment not equal to 1
!
         NINCX = N*INCX
         DO I = 1,NINCX,INCX
            DX(I) = DA*DX(I)
         END DO
      END IF
      RETURN
      END


!=========================================================================
      subroutine  dscal2(n,da,dx,incx)
!
!     scales a vector by a constant.
!     uses unrolled loops for increment equal to one.
!     jack dongarra, linpack, 3/11/78.
!     modified 3/93 to return if incx .le. 0.
!
      double precision da,dx(1)
      integer i,incx,m,mp1,n,nincx
!
      if( n.le.0 .or. incx.le.0 )return
      if(incx.eq.1)go to 20
!
!        code for increment not equal to 1
!
      nincx=n*incx
      do 10 i=1,nincx,incx
        dx(i)=da*dx(i)
   10 continue
      return
!
!        code for increment equal to 1
!
!
!        clean-up loop
!
   20 m=mod(n,5)
      if( m .eq. 0 ) go to 40
      do 30 i=1,m
        dx(i)=da*dx(i)
   30 continue
      if( n .lt. 5 ) return
   40 mp1=m + 1
      do 50 i=mp1,n,5
        dx(i)=da*dx(i)
        dx(i + 1)=da*dx(i + 1)
        dx(i + 2)=da*dx(i + 2)
        dx(i + 3)=da*dx(i + 3)
        dx(i + 4)=da*dx(i + 4)
   50 continue
      return
      end


!=========================================================================



!=========================================================================
      integer function idamax(n,dx,incx)
!
!     finds the index of element having max. absolute value.
!     jack dongarra, linpack, 3/11/78.
!     modified 3/93 to return if incx .le. 0.
!
      double precision dx(*),dmax
      integer i,incx,ix,n
!
      idamax=0
      if( n.lt.1 .or. incx.le.0 ) return
      idamax=1
      if(n.eq.1)return
      if(incx.eq.1)go to 20
!
!        code for increment not equal to 1
!
      ix=1
      dmax=dabs(dx(1))
      ix=ix + incx
      do 10 i=2,n
         if(dabs(dx(ix)).le.dmax) go to 5
         idamax=i
         dmax=dabs(dx(ix))
    5    ix=ix + incx
   10 continue
      return
!
!        code for increment equal to 1
!
   20 dmax=dabs(dx(1))
      do 30 i=2,n
         if(dabs(dx(i)).le.dmax) go to 30
         idamax=i
         dmax=dabs(dx(i))
   30 continue
      return
      end


      subroutine  dswap (n,dx,incx,dy,incy)
!
!     interchanges two vectors.
!     uses unrolled loops for increments equal one.
!     jack dongarra, linpack, 3/11/78.
!
      double precision dx(1),dy(1),dtemp
      integer i,incx,incy,ix,iy,m,mp1,n
!
      if(n.le.0)return
      if(incx.eq.1.and.incy.eq.1)go to 20
!
!       code for unequal increments or equal increments not equal
!         to 1
!
      ix=1
      iy=1
      if(incx.lt.0)ix=(-n+1)*incx + 1
      if(incy.lt.0)iy=(-n+1)*incy + 1
      do 10 i=1,n
        dtemp=dx(ix)
        dx(ix)=dy(iy)
        dy(iy)=dtemp
        ix=ix + incx
        iy=iy + incy
   10 continue
      return
!
!       code for both increments equal to 1
!
!
!       clean-up loop
!
   20 m=mod(n,3)
      if( m .eq. 0 ) go to 40
      do 30 i=1,m
        dtemp=dx(i)
        dx(i)=dy(i)
        dy(i)=dtemp
   30 continue
      if( n .lt. 3 ) return
   40 mp1=m + 1
      do 50 i=mp1,n,3
        dtemp=dx(i)
        dx(i)=dy(i)
        dy(i)=dtemp
        dtemp=dx(i + 1)
        dx(i + 1)=dy(i + 1)
        dy(i + 1)=dtemp
        dtemp=dx(i + 2)
        dx(i + 2)=dy(i + 2)
        dy(i + 2)=dtemp
   50 continue
      return
      end

