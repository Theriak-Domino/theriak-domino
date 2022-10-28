!-----Version: 28.05.2022
!               ***********
!               * THERIAQ *
!               ***********
!
!     Program written by Christian de Capitani
!     at the Department of Geological Sciences,
!     University of British Columbia, Vancouver, B.C. Canada
!     (May 1984 - Sept 1987)
!
!     revision: April 1987
!     minor changes: December 1987
!     revision: July 1993
!     addition of aqueous phases: July 1995
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
      include 'aqua.cmn'
!-----END OF COMMON VARIABLES
      INTEGER(4) MAXBL
      PARAMETER (MAXBL=10)
!-----
      CHARACTER(32) CH16
      REAL(8) X16,BULBL(MAXBL,COMAX),TCBL(MAXBL),PBL(MAXBL), &
      FLUBL(MAXBL,COMAX),SOLBL(MAXBL,COMAX),KGWBL(MAXBL)
      INTEGER(4) FLOWMOD,NBL
      CHARACTER(25) USEBL(MAXBL)
!-----END OF VARIABLES FOR MEGAMAT
      REAL(8) XKGH2O,F1,F2,F3
      INTEGER(4) AQFAIL,NAQLOOP,VERYSPEZ
!-----
      INTEGER(4) I001,I002,I,II,I1,COMAY
      REAL(8) FF,LABUL(COMAX),INFLOW(COMAX)
      CHARACTER(500) CH001,CH002,SYREC,CHIN(3),ZEITSTRING
      CHARACTER(8) CH8
!-----
      REAL(8) TCBK,PBK,FLUBK(COMAX),SOLBK(COMAX),KGWBK,PHBK,PEBK
      CHARACTER(25) USEBK
!---- END OF VARIABLES FOR AQ-EQUIL.
!----
      INTEGER(4) ierr,j
      progname='THERIAQ'
      vers='28.05.2022'
      task='"Computation of aqueous equilibriua"'
      call initialize('$THERIAQ-FILES',ierr)
      if(ierr.ne.0) STOP
!*****
      DO I=1,3
        CHIN(I)=' '
      END DO
!*****
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
      READ (UNIT=log,FMT='(I6,2X,E12.5,2X,E12.5,2X,E12.5,I3)',END=411) &
      NRLOOP,NRFIDI,NRMUZ,NRRED,I1
      IF (I1.EQ.0) DHPAR='sup'
      IF (I1.EQ.1) DHPAR='phr'
!
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
      READ (5,FMT='(A500)') CH001
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
!      IF (PRTLOG(1)) STOP
!     CLOSE(UNIT=dat)
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
      CALL TAXI(SYREC,USE)
!+++++
      CALL CHEMIE(COMAY,NC,OXYDE,OXANZ,FORMUL,CHEM)
      CALL LABLA(USE,LUSE)
!-----------------------------------------------------------------
!      NVAR=1
!      VARIS(1)='loop'
!=================================================================
!------------------
!     Open UNIT=ibk
!------------------
      j=ibk
      line=filename(j)(1:fnl(j))//ext(j)
      path=wpath
      akzess=' '
      state=' '
      call openfile(j,ierr)
      if(ierr.ne.0) STOP
      DO I=1,NC
        INFLOW(I)=0.0D0
      END DO
   10 READ (UNIT=ibk,FMT='(A500)',END=888) CH001
      CALL TAXI(CH001,CH8)
      CALL GELI(CH001,FF)
      I001=0
      DO 800,I=1,NC
      IF (CH8.EQ.OXYDE(I)) THEN
      I001=I
      GOTO 11
      END IF
  800 CONTINUE
   11 IF (I001.EQ.0) THEN
      WRITE (UNIT=scr,FMT='(''Unknown element:'',A8)') CH8
      STOP
      END IF
      INFLOW(I001)=FF
      GOTO 10
  888 CONTINUE
!     WRITE (UNIT=scr,FMT=160) (OXYDE(I),INFLOW(I),I=1,NC)
! 160 FORMAT (A8,1PE12.5)
!=================================================================
!=================================================================
!---- input in file "blocks"
!---- FLOWMOD: 0: one flow direction, [1: flow in both directions (not implemented)]
!---- for each block: TCBL:  Temperature, PBL:   Pressure, KGWBL: Kg of water
!----                 bulk: BULBL(i), i=1,NC, USEBL: use-variable
!---- end of input if TCBL or PBL = 0 (e.g. empty line)
      WRITE (UNIT=6,FMT='(/133A)') ('-',I=1,133)
      WRITE (UNIT=6,FMT='('' reading file: blocks'')')
      NBL=0
      OPEN (UNIT=40,FILE='blocks',STATUS='UNKNOWN')
      READ (UNIT=40,FMT='(A500)',END=77) CH001
!C      CALL PUST (6,CH001)
      CALL GELI(CH001,FF)
      FLOWMOD=IDINT(FF)
    7 READ (UNIT=40,FMT='(A500)',END=77) CH001
!C      CALL PUST (6,CH001)
      CALL GELI(CH001,F1)
      CALL GELI(CH001,F2)


      IF (F2.LT.0.0D0) THEN
       F3=F1+273.15D0
       CALL PSAT2(F3,F2)
      END IF



      IF (F1.EQ.0.0D0.OR.F2.EQ.0.0D0) GOTO 77
      NBL=NBL+1
      TCBL(NBL)=F1
      PBL(NBL)=F2
      CALL GELI(CH001,FF)
      KGWBL(NBL)=FF
      READ (UNIT=40,FMT='(A500)',END=77) CH001
      CALL GELI(CH001,F1)
      CALL TAXI(CH001,FORMUL)
      CALL TAXI(CH001,CH)
      USEBL(NBL)=CH
      CALL CHEMIE(COMAY,NC,OXYDE,OXANZ,FORMUL,CHEM)
      DO 700,I=1,NC
      BULBL(NBL,I)=CHEM(I)
  700 CONTINUE
      GOTO 7
   77 CONTINUE
      CLOSE (UNIT=40)
      DO 702,II=1,NBL
      WRITE (scr,1000) II
      WRITE (out,1000) II
 1000 FORMAT (/' block:',I3/' ---------')
      WRITE (scr,1004) TCBL(II),PBL(II),USEBL(II),KGWBL(II)
      WRITE (out,1004) TCBL(II),PBL(II),USEBL(II),KGWBL(II)
 1004 FORMAT (' T =',F8.2,' C','     P =',F9.2,' Bar', &
      '    use =',A8,'    KgW =',1PE15.8)
      DO I=1,NC
      IF (BULBL(II,I).NE.0.0D0) THEN
      WRITE (scr,1006) OXYDE(I),BULBL(II,I)
      WRITE (out,1006) OXYDE(I),BULBL(II,I)
 1006 FORMAT (1X,A10,F11.5)
      END IF
      END DO
  702 CONTINUE
      WRITE (UNIT=6,FMT='(133A)') ('-',I=1,133)
!=================================================================
!---- input: NAQLOOP: number of main loops, XKGH2O: Kg water flowing per loop
      VERYSPEZ=0
      WRITE (UNIT=6,FMT='('' '')')
      CALL LABLA(CHIN(2),I002)
      IF (I002.EQ.0) I002=1
      CH002='number of loops ? ('//CHIN(2)(1:I002)//'):'
      CALL PUST (6,CH002)
      READ (5,FMT='(A500)') CH001
      IF (CH001.EQ.' ') THEN
      CH001=CHIN(2)
      I001=I002
      ELSE
      CHIN(2)=CH001
      END IF
      CALL GELI(CH001,FF)
      NAQLOOP=IDINT(FF)
      IF (NAQLOOP.EQ.0) VERYSPEZ=1
      CALL LABLA(CHIN(3),I002)
      IF (I002.EQ.0) I002=1
      CH002='kg of water per loop ? ('//CHIN(3)(1:I002)//'):'
      CALL PUST (6,CH002)
      READ (5,FMT='(A500)') CH001
      IF (CH001.EQ.' ') THEN
      CH001=CHIN(3)
      I001=I002
      ELSE
      CHIN(3)=CH001
      END IF
      CALL GELI(CH001,XKGH2O)
!-----store terminal input
      CLOSE (UNIT=log)
!     OPEN (UNIT=log,FILE='theriaq.last',STATUS='OLD')
!------------------
!     Open UNIT=log
!------------------
      j=log
      line=filename(j)(1:fnl(j))//ext(j)
      path=wpath
      akzess=' '
      state='OLD'
      call openfile(j,ierr)
      if(ierr.ne.0) STOP
      i1=0
      IF (DHPAR.EQ.'sup') I1=0
      IF (DHPAR.EQ.'phr') I1=1
      WRITE (UNIT=log,FMT='(I6,2X,E12.5,2X,E12.5,2X,E12.5,I3)') &
       NRLOOP,NRFIDI,NRMUZ,NRRED,I1
      DO I=1,3
        CALL PUST(log,CHIN(I))
      END DO
      CLOSE (UNIT=log)
!=================================================================
!CCC      PRTLOG(9)=.TRUE.
      PRTLOG(9)=.FALSE.
      PRTLOG(2)=.TRUE.
      NVARTBL=0
      NROWTBL=0
!---
      IF (VERYSPEZ.EQ.1) THEN
      CALL SPEZSUB(AQFAIL)
      GOTO 711
      END IF
!---- SOLBL(ibl,): Bulk of solids in blodk
!---- FLUBL(ibl,): Bulk in fluid in blodk
!---- initial: all FLUBL(ibl,i)=0.0D0
      DO IBL=1,NBL
        DO I=1,NC
          SOLBL(IBL,I)=BULBL(IBL,I)
          FLUBL(IBL,I)=0.0D0
        END DO
      END DO
!-----------------------------------------------------------------
!--- start main aqueous loop
!-----------------------------------------------------------------
      DO 710,NBEO=1,NAQLOOP


      WRITE (UNIT=6,FMT='(/133A)') ('-',I=1,133)
      WRITE (UNIT=6,FMT='(133A)') ('-',I=1,133)
      WRITE (UNIT=6,FMT= &
      '('' aqueous loop '',I4)') NBEO
      WRITE (UNIT=6,FMT='(133A)') ('-',I=1,133)



!-----------------------------------------------------------------
!--- start block loop
!-----------------------------------------------------------------
      DO 720,IBL=1,NBL
      WRITE (UNIT=6,FMT='(/'' aqueous loop:'',I4)') NBEO
      WRITE (UNIT=out,FMT='(/'' aqueous loop:'',I4)') NBEO
      WRITE (UNIT=6,FMT='('' block:       '',I4)') IBL
      WRITE (UNIT=out,FMT='('' block:       '',I4)') IBL
!C      DO 722,I=1,NC
!C  722 FLUBL(IBL,I)=0.0D0
!-----
      NROWTBL=NROWTBL+1
      IF (NROWTBL.GT.MAXTBL) THEN
      WRITE (UNIT=6,FMT='(''too many rows: calculation stopped'')')
      WRITE (UNIT=out,FMT='(''too many rows: calculation stopped'')')
      STOP
      END IF
      DO I=1,NVARTBL
        OUTTBL(NROWTBL,I)=0.0D0
      END DO
!-----
!-----
      CH16='#loop'
      X16=DBLE(NBEO)
      CALL SETTBL(CH16,X16)
      CH16='#block'
      X16=DBLE(IBL)
      CALL SETTBL(CH16,X16)
!-----
!=============================================================
!---- for one block: SOLBK: bulk of solids, FLUBK: bulk in fluid
!----                TCBK: temperatuer, PBK: pressure, KGWBK: kg water
!----                PEBK: pE, PHBK: pH, USEBK: USE
      DO I=1,NC
        SOLBK(I)=SOLBL(IBL,I)
        FLUBK(I)=FLUBL(IBL,I)
      END DO
      TCBK=TCBL(IBL)
      PBK=PBL(IBL)
      KGWBK=KGWBL(IBL)
      PEBK=0.0D0
      PHBK=0.0D0
      USEBK=USEBL(IBL)
      CALL SEAQUIL(TCBK,PBK,FLUBK,SOLBK,USEBK,KGWBK,PHBK,PEBK)
      DO I=1,NC
        SOLBL(IBL,I)=SOLBK(I)
        FLUBL(IBL,I)=FLUBK(I)
        BULBL(IBL,I)=SOLBK(I)+FLUBK(I)
      END DO
!C      STOP
!C      GOTO 710
!=============================================================
!-----
  720 CONTINUE
!-----------------------------------------------------------------
!--- end block loop
!-----------------------------------------------------------------
!----- mix water
!-----
      DO 750,IBL=NBL,1,-1
      WRITE (6,2010) IBL
      WRITE (out,2010) IBL
 2010 FORMAT (/' fluid composition (FLUBL, BULBL) block:',I5)
      DO 752,I=1,NC
      IF (FLUBL(IBL,I).NE.0.0D0.OR.BULBL(IBL,I).NE.0.0D0) THEN
      WRITE (6,2012) OXYDE(I),FLUBL(IBL,I),BULBL(IBL,I)
      WRITE (out,2012) OXYDE(I),FLUBL(IBL,I),BULBL(IBL,I)
 2012 FORMAT (1X,A8,2(2X,1PE15.8))
      END IF
  752 CONTINUE
!==
      IF (IBL.GT.1) THEN
      DO 754,I=1,NC
      IF (OXYDE(I).NE.'O '.AND.OXYDE(I).NE.'H '.AND.OXYDE(I).NE.'E ') &
       THEN
        FF=FLUBL(IBL,I)/KGWBL(IBL)
        FLUBL(IBL,I)=FLUBL(IBL,I)-XKGH2O*FF
        IF (FLUBL(IBL,I).LT.0.0D0) FLUBL(IBL,I)=0.0D0
        FF=FLUBL(IBL-1,I)/KGWBL(IBL-1)
        FLUBL(IBL,I)=FLUBL(IBL,I)+XKGH2O*FF
!CCC      IF (BULBL(IBL,I).LT.0.0D0) BULBL(IBL,I)=0.0D0
       END IF
  754 CONTINUE
      END IF
!==
      IF (IBL.EQ.1) THEN
      DO 755,I=1,NC
      IF (OXYDE(I).NE.'O '.AND.OXYDE(I).NE.'H '.AND.OXYDE(I).NE.'E ') &
       THEN
      FF=FLUBL(IBL,I)/KGWBL(IBL)
      FLUBL(IBL,I)=FLUBL(IBL,I)-XKGH2O*FF
      IF (FLUBL(IBL,I).LT.0.0D0) FLUBL(IBL,I)=0.0D0
      END IF
  755 CONTINUE
      END IF
!==
      WRITE (6,2014)
      WRITE (out,2014)
 2014 FORMAT (/' new FLUBL')
      DO 756,I=1,NC
      IF (FLUBL(IBL,I).NE.0.0D0) THEN
      WRITE (6,2016) OXYDE(I),FLUBL(IBL,I)
      WRITE (out,2016) OXYDE(I),FLUBL(IBL,I)
 2016 FORMAT (1X,A8,(2X,1PE15.8))
      END IF
  756 CONTINUE
  750 CONTINUE
!-----
  710 CONTINUE
!-----------------------------------------------------------------
!--- end main aqueous loop
!-----------------------------------------------------------------
!---- the last call to DBREAD is apparently not necessary (??)
  711 CALL PRTTBL
!------------------
!     Open UNIT=lbk
!------------------
      j=lbk
      line=filename(j)(1:fnl(j))//ext(j)
      path=wpath
      akzess=' '
      state=' '
      call openfile(j,ierr)
      if(ierr.ne.0) STOP
      WRITE (UNIT=lbk,FMT='(''*** LAST BULK COMPOSITION ***'')')
      DO 770,I=1,NUN
      WRITE (UNIT=lbk,FMT=1114) CHNAME(I),LABUL(I)
 1114 FORMAT (1X,A8,1X,1PE12.5)
  770 CONTINUE
      WRITE (UNIT=lbk,FMT='(''*** AQUEOUS COMPOSITION ***'')')
      DO 772,I=1,NUN
      WRITE (UNIT=lbk,FMT=1115) OXYDE(CHMCOD(I)),QSUM(I)
 1115 FORMAT (2X,A8,1PE12.5)
  772 CONTINUE
      CLOSE(UNIT=lbk)
!-----------------------------------------------------------------
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
!     PRTLOG(11)=.TRUE.    (used to print short table (e.g. theriaq))
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
  150 FORMAT (/,'exit THERIAK',/,A)
      END
!-----
!********************************
      SUBROUTINE SEAQUIL(TCBK,PBK,FLUBK,SOLBK,USEBK,KGWBK,PHBK,PEBK)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
      include 'aqua.cmn'
!-----END OF COMMON VARIABLES
      REAL(8) TCBK,PBK,FLUBK(COMAX),SOLBK(COMAX),KGWBK,PHBK,PEBK, &
      BMDIS(COMAX),FAK
      CHARACTER(25) USEBK
      CHARACTER(32) CH16
      CHARACTER(125) AS0,AS1
!---- END OF VARIABLES FOR AQ-EQUIL.
      REAL(8) FF,TOTBK(COMAX),AQPOT0(COMAX),POTDIFF,FBULK(COMAX), &
      BULK0(COMAX),NNPOT(COMAX),MFBULK(COMAX),PROJG,X16,IONSUM
      INTEGER(4) I,II,AQFAIL,AQPHAS(COMAX),NDLOOP,NDLMAX,IDPOT(COMAX), &
      PR01A0,PR02A0,PR03A0,IMORE,I0
      LOGICAL(4) FIXPOT(COMAX),FINIS,MORE
!-----
!---- PRTEST=.TRUE. prints some additional information
!---- PR01(11) sets PRTLOG for initial theriak (bulk = solid+fluid)
!---- PR02(11) sets PRTLOG for intermediate calculations
!---- PR03(11) sets PRTLOG for final theriak in aq-loop
!---- PRSPE=1 prints all spezies in AQEQ
!---- PRH2O=1 prints properties of H2O in AQEQ
!---- PRELE=1 prints element summary in AQREQ!
      DO I=1,11
        PR01(I)=0
        PR02(I)=0
        PR03(I)=0
      END DO
      PR01A0=1
      PR02A0=1
      PR03A0=1

      PR01(3)=1
      PR01(2)=1
      PR02(2)=1

      PR03(2)=1
      PR03(6)=1
      PR03(8)=1
      PR03(11)=1

      PRTEST=.TRUE.
      PRSPE=1
      PRH2O=1
      PRELE=1
!
      FINIS=.FALSE.
      NDLMAX=10
      AS0=' '
      AS1=' '
!      IF (.NOT.PRTEST) THEN
!      DO 200,I=1,11
!  200 PRTLOG(I)=.FALSE.
!!      PRTLOG(2)=.TRUE.
!      END IF
!=============================================================
!---- first: TOTBK: total bulk of solids plus fluids
!----        AQPHAS: for each component a phase with G=0 (same as "ELEMENTS")
!----        AQPHAS(i)=NUMMER of phase for component i
!---- for ndloop=1: aqueous phases: NULL=.true. (not used)
!---- SOLBK(i) solid bulk of block (1 to NC)
!---- FLUBK(i) fluid bulk of block (1 to NC)
!---- TOTBK(i) total bulk of Block (solid+fluid) (1 to NC)
!----
!---- BULK(i) bulk used for THERIAK, is initially set in DBREAD from CHEM (1 to NUN)
!---- BULK0(i) total bulk of of block (1 to NUN)
!---- FBULK(i) fluid bulk of block (1 to NUN)
!---- MFBULK(i) maximum bulk for fluid (usually = BULK0)


      DO I=1,NC
        TOTBK(I)=SOLBK(I)+FLUBK(I)
        FLUBK(I)=0.0D0
        CHEM(I)=TOTBK(I)
      END DO


      USE=USEBK
      CALL LABLA(USE,LUSE)
      CALL DBREAD

      AQCOMMENT=' '
!---- make aqueous components with G(i)=0
      CALL POTPHAS(AQPHAS)
      DO I=1,NPHA
        IF (PHASID(I).EQ.'AQU'.OR.PHASID(I).EQ.'AQP') THEN
          NULL(I)=.TRUE.
        END IF
      END DO
      DO I=1,NUN
        BMDIS(I)=0.0D0
        IDPOT(I)=0
        BULK0(I)=BULK(I)
        MFBULK(I)=BULK(I)
        FBULK(I)=0.0D0
        NNPOT(I)=0.0D0
        FIXPOT(I)=.FALSE.
        AQPOT0(I)=0.0D0
      END DO
      NDLOOP=0
!====================================================================
!---- ndloop=0: call THERIAK with total bulk (solid + fluid)
!====================================================================
      DO I=1,11
       IF (PR01(I).EQ.0) THEN
        PRTLOG(I)=.FALSE.
       ELSE
        PRTLOG(I)=.TRUE.
       END IF
      END DO
!
      WRITE (UNIT=6,FMT='(/,'' loop = '',I3,'' (equil.)'')') NDLOOP
      TC=TCBK
      P=PBK
      WRITE (UNIT=6,FMT='(/,'' TC= '',F8.2,'' P ='',F8.2)') TC,P
      CALL NURVONPT
      CALL CALSTR
      WRITE (UNIT=6,FMT='(/133A)') ('=',I=1,133)
      WRITE (UNIT=6,FMT= &
      '('' CALL THERIAK (SEAQUIL) with total bulk (solid + fluid)'')')
      WRITE (UNIT=6,FMT='(133A)') ('=',I=1,133)
      IF (PRTLOG(2).OR.PRTLOG(3).OR.PRTLOG(4)) CALL PRININ
!================
      CALL THERIA
      IF (PR01A0.EQ.1) WRITE (UNIT=6,FMT='(133A)') ('=',I=1,133)
!----
!---- add to make input for eq3
!---- makes data0 and input
!---- e.g. put in eq_folder, run eqpt -> data1,data1f,slist,output
!---- run eq3 -> pickup,output 
      IF (NDLOOP.EQ.0.AND.NBEO.EQ.1.AND.IBL.EQ.1) THEN
      WRITE (UNIT=6,FMT='(133A)') ('=',I=1,133)
      WRITE (UNIT=6,FMT='('' writing input for EQ3'')')
      WRITE (UNIT=6,FMT='(133A)') ('=',I=1,133)
        CALL EQ36
      END IF
!----
!================
      CALL ANASS(AS0)
      WRITE (6,1000)
      WRITE (out,1000)
 1000 FORMAT (/' initial assemblage (total bulk):')
      CALL PUST(6,' '//AS0)
      CALL PUST(out,' '//AS0)
      AQCOMMENT='(total bulk) '//AS0
      IF (PR01A0.EQ.1) CALL PRTSHORT
      NPOT=0
      PROJG=0.0D0
      POTDIFF=0.0D0
!--
      DO I=1,NMAX
      DO II=1,NUN
      IF (NUMMER(I).EQ.AQPHAS(II)) THEN
      AQPOT(II)=G(I)
      END IF
      END DO
      END DO
!--
    1 CONTINUE
      IF (NDLOOP.GT.NDLMAX) GOTO 999
!====================================================================
      DO II=1,NUN
        AQPOT0(II)=AQPOT(II)
      END DO
      NDLOOP=NDLOOP+1
      WRITE (UNIT=6,FMT='(/,'' loop = '',I3,'' (aq. 1)'')') NDLOOP
!======================
      CALL AQEQ(AQFAIL,IONSUM)
!======================


!!      GOTO 789

!====================================================================
!----- calculate fbulk and substract from bulk0
!====================================================================


!!      789 CONTINUE


      DO 610,I=1,NUN
      FF=QSUM(I)*KGWBK
      IF (CHNAME(I).NE.'E') THEN
      FBULK(I)=FF
      END IF
  610 CONTINUE
!----- define new solid bulk
      MORE=.FALSE.
      IMORE=0
      WRITE (UNIT=6,FMT='(/'' BULK,FBULK'')')

      FAK=1.0D0
      I0=0
      DO I=1,NUN
       IF (CHNAME(I).NE.'E') THEN
       FF=BULK0(I)/FBULK(I)
       IF (FF.LT.FAK) THEN
        MORE=.TRUE.
        IMORE=I
        FAK=FF
        I0=I
       END IF
      END IF
      END DO



      DO 612,I=1,NUN
!!       IF (FIXPOT(I)) THEN
!!        BULK(I)=BULK0(I)
!!       ELSE
        BULK(I)=BULK0(I)-FBULK(I)*FAK
!!        IF (BULK(I).LE.0.0D0.AND.CHNAME(I).NE.'E') THEN
!!          BULK(I)=0.0D0
!!          MORE=.TRUE.
!!          IMORE=I
!!        END IF
!!       END IF
  612 CONTINUE
      IF (I0.NE.0) BULK(I0)=0.0D0
      DO I=1,NUN
!        IF (PRTEST) WRITE (6,3000) CHNAME(I),BULK(I),FBULK(I)
       WRITE (6,3000) CHNAME(I),BULK(I),FBULK(I)
       WRITE (out,3000) CHNAME(I),BULK(I),FBULK(I)
 3000  FORMAT (9X,A8,3(2X,1PE15.8))
       END DO
!
      DO I=1,11
       IF (PR02(I).EQ.0) THEN
        PRTLOG(I)=.FALSE.
       ELSE
        PRTLOG(I)=.TRUE.
       END IF
      END DO
!
      IF (MORE) THEN
      WRITE (UNIT=6,FMT='(/133A)') ('+',I=1,133)
      WRITE (UNIT=6,FMT= &
      '('' one component is zero '',A)') CHNAME(IMORE)
      WRITE (UNIT=6,FMT='(133A)') ('+',I=1,133)
      
      DO I=1,NUN
       CHEM(CHMCOD(I))=BULK(I)
      END DO
      CALL DBREAD
      CALL NURVONPT
      END IF




      CALL CALSTR
      WRITE (UNIT=6,FMT='(/133A)') ('=',I=1,133)
      WRITE (UNIT=6,FMT= &
      '('' CALL THERIAK (SEAQUIL) fbulk substracted from bulk0'')')
      WRITE (UNIT=6,FMT='(133A)') ('=',I=1,133)
      IF (PRTLOG(2).OR.PRTLOG(3).OR.PRTLOG(4)) CALL PRININ
!================
      CALL THERIA
      IF (PR02A0.EQ.1) WRITE (UNIT=6,FMT='(133A)') ('=',I=1,133)
!================
      CALL ANASS(AS1)
      WRITE (6,3010)
      WRITE (out,3010)
 3010 FORMAT (/' assemblages:')
      CALL PUST(6,' before: '//AS0)
      CALL PUST(out,' before: '//AS0)
      CALL PUST(6,' after:  '//AS1)
      CALL PUST(out,' after:  '//AS1)
      AQCOMMENT='(solid bulk) '//AS1
      IF (PR02A0.EQ.1) CALL PRTSHORT
!--
      DO I=1,NMAX
      DO II=1,NUN
      IF (NUMMER(I).EQ.AQPHAS(II)) THEN
      AQPOT(II)=G(I)
      END IF
      END DO
      END DO
!--
!====================================================================
!----- check if ass0 = ass1
!====================================================================
      IF (AS0.NE.AS1) THEN
      WRITE (6,3012)
      WRITE (out,3012)
 3012 FORMAT (/,' assemblage changes after dissolution')
      WRITE (UNIT=6,FMT='(/133A)') ('+',I=1,133)
      WRITE (UNIT=6,FMT= &
      '('' use solid bulk as new total bulk '')')
      WRITE (UNIT=6,FMT='(133A)') ('+',I=1,133)
      DO I=1,NUN
       FBULK(I)=0.0D0
      END DO
      GOTO 999
      ELSE
      WRITE (6,3013)
      WRITE (out,3013)
 3013 FORMAT (/,' assemblage does not change after dissolution')
      END IF
!====================================================================
!----- check chemical potentials
!====================================================================
      PROJG=0.0D0
      DO I=1,NUN
        PROJG=PROJG+AQPOT(I)*BULK0(I)
      END DO
      WRITE (6,3020) PROJG
      WRITE (out,3020) PROJG
 3020 FORMAT (/,' projected G = ',1PE15.8)
      WRITE (6,3022)
      WRITE (out,3022)
 3022 FORMAT (/' chemical potentials',/ &
      23X,'previous',12X,'now',9X,'difference')
      POTDIFF=0.0D0
      DO I=1,NUN
      FF=DABS(AQPOT(I)-AQPOT0(I))
      IF (FF.GT.POTDIFF) POTDIFF=FF
      WRITE (6,3024) CHNAME(I),AQPOT0(I),AQPOT(I), &
      AQPOT(I)-AQPOT0(I)
      WRITE (out,3024) CHNAME(I),AQPOT0(I),AQPOT(I), &
      AQPOT(I)-AQPOT0(I)
 3024 FORMAT (9X,A8,3(2X,1PE15.8))
      END DO
      WRITE (6,3026) POTDIFF
      WRITE (out,3026) POTDIFF
 3026 FORMAT (/,' potdiff = ',1PE15.8)



!================
!      WRITE (UNIT=6,FMT='(/,'' 1=continue, 0=end'')')
!      READ (*,*) I
!      IF (I.EQ.1) GOTO 1

      IF (POTDIFF.GT.1D-5) GOTO 1



  999 CONTINUE



!----- print last equilibrium
      WRITE (UNIT=6,FMT='(/133A)') ('-',I=1,133)
      WRITE (UNIT=6,FMT=1020) NBEO,IBL
 1020 FORMAT (' print last equilibrium, aq-loop =',I3,'  block =',I3)
      WRITE (UNIT=6,FMT='(133A)') ('-',I=1,133)
!
      DO I=1,11
       IF (PR03(I).EQ.0) THEN
        PRTLOG(I)=.FALSE.
       ELSE
        PRTLOG(I)=.TRUE.
       END IF
      END DO
!
      CALL PRTCAL
!
      DO I=1,11
       IF (PR01(I).EQ.0) THEN
        PRTLOG(I)=.FALSE.
       ELSE
        PRTLOG(I)=.TRUE.
       END IF
      END DO
!
      CALL AQEQ(AQFAIL,IONSUM)
      PRTEST=.TRUE.
!      PRTEST=.FALSE.
      WRITE (6,1008)
      WRITE (out,1008)
 1008 FORMAT (/' equilibrium compositions (after last aq-loop)',/ &
      24X,'solids',11X,'fluid',12X,'total')
      DO I=1,NUN
      WRITE (6,1010) CHNAME(I),BULK(I),FBULK(I),BULK0(I)
      WRITE (out,1010) CHNAME(I),BULK(I),FBULK(I),BULK0(I)
 1010 FORMAT (9X,A8,3(2X,1PE15.8))
      CH16='(blk)'//CHNAME(I)(1:8)
      X16=BULK(I)
      CALL SETTBL(CH16,X16)
      END DO
!----- before returning: set FLUBK and SOLBK
      DO I=1,NC
        SOLBK(I)=0.0D0
        FLUBK(I)=0.0D0
      END DO
      DO I=1,NUN
        SOLBK(CHMCOD(I))=BULK(I)
        FLUBK(CHMCOD(I))=FBULK(I)
      END DO
!*****
      END
!-----
!********************************
      SUBROUTINE AQEQ(AQFAIL,IONSUM)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
      include 'aqua.cmn'
!-----END OF COMMON VARIABLES
      REAL(8) LOGI(PHMAX),MOLW(PHMAX),DUM3(PHMAX),XM(PHMAX), &
      IONST,IONSUM,DUM1,DUM2,XRES(PHMAX),CC0(PHMAX), &
      JACOB(PHMAX,PHMAX),DX0(PHMAX), &
      LNMOL(PHMAX),LNXM(PHMAX),FF,AH2O,FINDIFF,DHAPHR,DHBPHR,TFURP, &
      DHASUP,DHBSUP
      INTEGER(4) I,IR,IC,N0,IH2O,ERRC,AQFAIL,IMEGA
      REAL(8) RH2O,AL,BE,DALDT,DBEDT,DBEDP,EE,DEDP,DEDT,DETT, &
      HAAG,HAAV,HAARHO,PSG,PSV,PSRHO,ZWERT,F2
      CHARACTER(16) CHXXX
      CHARACTER(32) CH16
      CHARACTER(132) CH132
!-----
!only needed for GAUSSEL:
      REAL(8) MAT(45,45)
      LOGICAL(4) SING
      INTEGER(4) J
!----
      WRITE (UNIT=6,FMT='(/133A)') ('-',I=1,133)
      CALL PUST(6,' Entering AQEQ: '//AQCOMMENT)
      WRITE (UNIT=6,FMT='(133A)') ('-',I=1,133)
      IF (NPOT.EQ.0) THEN
!!       WRITE (UNIT=6,FMT='('' no potentials to set'')')
       WRITE (UNIT=out,FMT='('' no potentials to set'')')
      ELSE
       DO I=1,NPOT
         WRITE (6,1001) CHNAME(POT(I)),CPOT(I)
         WRITE (out,1001) CHNAME(POT(I)),CPOT(I)
 1001    FORMAT (/,1X,A8,'set to ',1PE15.8)
       END DO
      END IF
      WRITE (UNIT=CH132,FMT='(1X,131A1)') ('-',I=1,131)
!----
      AQFAIL=0
      ZWERT=0.0D0
!     FINDIFF=1.0D-8
      FINDIFF=NRFIDI
      IZ=0
      ICH=0
      NROW=NUN
      NCOL=0
      DO 500,IR=1,NROW
        RNAME(IR)=CHNAME(IR)
        IF (RNAME(IR)(1:1).EQ.' ') THEN
        CH16=RNAME(IR)(2:)
        RNAME(IR)=CH16
        END IF
        IF (RNAME(IR).EQ.'E') IZ=IR
  500 CONTINUE
      IF (IZ.EQ.0) THEN
      WRITE (UNIT=6,FMT='(/''no component E'')')
      WRITE (UNIT=out,FMT='(/''no component E'')')
      RETURN
      ELSE
!!      WRITE (UNIT=6,FMT='('' component E ='',I3)') IZ
      WRITE (UNIT=out,FMT='('' component E ='',I3)') IZ
      END IF
!+++++
!+++++Copy all information into AA etc.
!+++++
      DO 550,IC=1,NUN
        IF (SUGG(IC).LE.NUN) THEN
        WRITE (6,1000)
        WRITE (out,1000)
 1000   FORMAT (/' Assemblage not buffered, no aqueous equilibrium ', &
        'is calculated')
        RETURN
!-----
        END IF
  550 CONTINUE
      WRITE (UNIT=6,FMT='('' Assemblage is buffered'')')
      WRITE (UNIT=out,FMT='('' Assemblage is buffered'')')
!-----
      IF (PRSPE.EQ.1) WRITE (6,1026) CH132,CH132
      IF (PRSPE.EQ.1) WRITE (out,1026) CH132,CH132
 1026 FORMAT (/,A,/ &
      ' aqueous equilibria   (only species with G<1E5)'/ &
      ,A)
      IH2O=0
      IEP=0
!---- include the following:
!---- NUMMER(IC).NE.0 (phase is not a solution)
!---- PHASID is AQU (supcrt) or AQP (phreeqc)
!---- G<1E5, except H+ and O2 (?)
      DO 600,IC=1,NMAX
      IF (NUMMER(IC).NE.0) THEN
       IF (ABK(NUMMER(IC)).EQ.'H2O') IH2O=IC
       IF (ABK(NUMMER(IC)).EQ.'E') IEP=IC
       IF ((PHASID(NUMMER(IC)).EQ.'AQU' &
       .OR.PHASID(NUMMER(IC)).EQ.'AQP')) THEN
!!---- exclude species with large G
!!        IF (G(IC).LT.1D5.OR.NAME(NUMMER(IC)).EQ.'H+' &
!!          .OR.NAME(NUMMER(IC)).EQ.'O2') THEN
        IF (G(IC).LT.1D5.OR.NAME(NUMMER(IC)).EQ.'H+') THEN
!-----
         NCOL=NCOL+1
         CNAME(NCOL)=NAME(NUMMER(IC))
         IF (CNAME(NCOL).EQ.'H+') ICH=NCOL
!-----
         GV(NCOL)=G(IC)
         ZZ(NCOL)=X(IC,IZ)
         PHRA(NCOL)=INDIVI(NUMMER(IC),1)
         PHRB(NCOL)=INDIVI(NUMMER(IC),2)
         DO IR=1,NROW
           AA(IR,NCOL)=X(IC,IR)
         END DO
!-----
        ELSE
         IF (PRSPE.EQ.1) WRITE (6,1028) NAME(NUMMER(IC)),G(IC)
         IF (PRSPE.EQ.1) WRITE (out,1028) NAME(NUMMER(IC)),G(IC)
 1028    FORMAT (' not used: ',A16,' G = ',1PE15.8)
        END IF
       END IF
      END IF
  600 CONTINUE


      IF (PRSPE.EQ.1) THEN
      WRITE (UNIT=6,FMT='('' '')')
      WRITE (UNIT=out,FMT='('' '')')
      DO I=1,NCOL
      WRITE (6,1029) I,CNAME(I),GV(I)
      WRITE (out,1029) I,CNAME(I),GV(I)
 1029 FORMAT (I3,': ',A16,' G = ',1PE15.8,2X)
      END DO
      END IF


      WRITE (UNIT=6,FMT='('' '')')
      WRITE (UNIT=out,FMT='('' '')')

      IF (ICH.EQ.0) THEN
      WRITE (6,1030)
      WRITE (out,1030)
 1030 FORMAT (/'no H+, no calculations')
      RETURN
      ELSE
!!       WRITE (UNIT=6,FMT='('' H+  is species:'',I3)') ICH
       WRITE (UNIT=out,FMT='('' H+  is species:'',I3)') ICH
      END IF
      IF (IH2O.EQ.0) THEN
      WRITE (6,1032)
      WRITE (out,1032)
 1032 FORMAT (/'no H2O, no calculations')
      RETURN
      ELSE
       AH2O=DEXP(-G(IH2O)/RT)
!!       WRITE (UNIT=6,FMT='('' H2O is phase:'',I3,'' in CALSTR'')') IH2O
       WRITE (UNIT=out,FMT='('' H2O is phase:'',I3,'' in CALSTR'')') IH2O
      END IF
!+++++ calculate Debye-Hueckel A and B as in Phreeq
      DHAPHR=0.0D0
      DHBPHR=0.0D0
      TFURP=T
      CALL ABPHREEQ(TFURP,DHAPHR,DHBPHR)
!+++++ calculate Debye-Hueckel A and B as in Supcrt
      L10=DLOG(10.0D0)
      CALL RHOETC(P,T,RH2O,AL,BE,DALDT,DBEDT,DBEDP)
      CALL JN91(T,RH2O,BE,AL,DALDT,EE,DEDP,DEDT,DETT)
      DHASUP=1.824829238D6*DSQRT(RH2O)/DSQRT((EE*T)**3)
      DHBSUP=50.29158649D8*DSQRT(RH2O)/DSQRT(EE*T)*1D-8
!======================================================================
!---- print properties of H2O
!======================================================================
      IF (PRH2O.EQ.1) WRITE (6,1050)
      IF (PRH2O.EQ.1) WRITE (out,1050)
 1050 FORMAT (/ &
      ' -----------------'/ &
      ' properties of H2O'/ &
      ' -----------------')
      IF (PRTEST) WRITE (UNIT=6,FMT=1051) P,PGAS,TC,T
      IF (PRTEST) WRITE (UNIT=out,FMT=1051) P,PGAS,TC,T
 1051 FORMAT ('  P =',F9.2,' bar',7X,'P(Gas) =',F9.2,' bar     T =', &
      F8.2,' C    =',F8.2,' K')
!

      CALL PSAT2(T,F2)
      IF (PRTEST) WRITE (UNIT=6,FMT=1045) F2
 1045 FORMAT ('  saturation pressure          = ',F12.5,' bar')


      FF=18.01528D0
!----- G,V and rho from Haar
!
      CHXXX='xxxx'
      CALL WHAAR2(CHXXX,P,T,HAAG,HAAV)
      HAAV=10.0D0*HAAV
      HAARHO=FF/HAAV
      IF (PRH2O.EQ.1) WRITE (6,1041) HAAG,HAAV,HAARHO
      IF (PRH2O.EQ.1) WRITE (out,1041) HAAG,HAAV,HAARHO
 1041 FORMAT (/ &
      ' Haar (1984), (WHAAR2):',/,2X, &
      'dG =',F15.3,' [J/mol]',5X,'vol = ',F12.6,' [ccm/mol]',5X, &
      'density = ',F12.6,' [g/ccm]')
!----- G,V and rho from Pitzer & Sterner
!
      CALL PS94H2O(P,T,PSG,PSV)
      PSV=10.0D0*PSV
      PSRHO=FF/PSV
      IF (PRH2O.EQ.1) WRITE (6,1042) PSG,PSV,PSRHO
      IF (PRH2O.EQ.1) WRITE (out,1042) PSG,PSV,PSRHO
 1042 FORMAT (/ &
      ' Pitzer & Sterner (1994) (PS94H2O):',/,2X, &
      'dG =',F15.3,' [J/mol]',5X,'vol = ',F12.6,' [ccm/mol]',5X, &
      'density = ',F12.6,' [g/ccm]')
!
      IF (PRH2O.EQ.1) WRITE (6,1052) AH2O,RH2O,AL,BE,EE
      IF (PRH2O.EQ.1) WRITE (out,1052) AH2O,RH2O,AL,BE,EE
 1052 FORMAT (/ &
      ' As in SUPCRT (RHOETC,JN91):',/, &
      '  activity:  ',7X,1PE12.5,7X,'density:',1PE12.5,' g/ml'/ &
      '  alpha:     ',7X,1PE12.5,7X,'beta:   ',1PE12.5/ &
      '  diel.const:',7X,0PF12.6)
!
      IF (DHPAR.EQ.'sup') THEN
      IF (PRH2O.EQ.1) WRITE (6,1053) DHASUP,DHBSUP
      IF (PRH2O.EQ.1) WRITE (out,1053) DHASUP,DHBSUP
 1053 FORMAT ( &
      '  Debye-Hueckel constants (SUPCRT):  A =',1PE12.5,5X, &
      'B =',1PE12.5,' <----- used')
      ELSE
      IF (PRH2O.EQ.1) WRITE (6,1054) DHASUP,DHBSUP
      IF (PRH2O.EQ.1) WRITE (out,1054) DHASUP,DHBSUP
 1054 FORMAT ( &
      '  Debye-Hueckel constants (SUPCRT):  A =',1PE12.5,5X, &
      'B =',1PE12.5)
      END IF
!
      IF (DHPAR.EQ.'phr') THEN
      IF (PRH2O.EQ.1) WRITE (6,1055) DHAPHR,DHBPHR
      IF (PRH2O.EQ.1) WRITE (out,1055) DHAPHR,DHBPHR
 1055 FORMAT (/ &
      ' As in PHREEQC 2.1 (ABPHREEQ):',/, &
      '  Debye-Hueckel constants (PHREEQ):  A =',1PE12.5,5X, &
      'B =',1PE12.5,' <----- used')
      ELSE
      IF (PRH2O.EQ.1) WRITE (6,1056) DHAPHR,DHBPHR
      IF (PRH2O.EQ.1) WRITE (out,1056) DHAPHR,DHBPHR
 1056 FORMAT (/ &
      ' As in PHREEQC 2.1 (ABPHREEQ):',/, &
      '  Debye-Hueckel constants (PHREEQ):  A =',1PE12.5,5X, &
      'B =',1PE12.5)
      END IF
!
      IF (DHPAR.EQ.'phr') THEN
        AGA=DHAPHR
        BGA=DHBPHR
      END IF
      IF (DHPAR.EQ.'sup') THEN
        AGA=DHASUP
        BGA=DHBSUP
      END IF
      IF (PRH2O.EQ.1) WRITE (6,1058) AGA,BGA
      IF (PRH2O.EQ.1) WRITE (out,1058) AGA,BGA
 1058 FORMAT (/ &
      '  Debye-Hueckel constants (USED)  :  A =',1PE12.5,5X, &
      'B =',1PE12.5)
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!     nrow: rows: number of components (=NUN)
!     ncol: cols: number of species (only phasid=AQU, and G<1D5)
!     AA(nrow,ncol)=X(phase,comp) !!!!transpose!!!
!     GV(i): chemical potential of species after THERIAK
!     GW(i): chemical potential of species after AQEQ
!     ZZ(i): charge of species
!     IZ: index of component "E"
!     IEP: index of phase "E" (in CALSTR)
!     IH2O: index of phase "H2O" (in CALSTR)
!     ICH: index of species "H+"
!     variables: Ln(mi) [i=1,ncol]
!                MUZ  (correction of chem.pot. of charge)
!                MUEG(i)  (correction of chem.pot. of components with fixed concentrations)
!     GW(i)=GV(i)+summe(AA(IR,IC)*MUEG(IR))
!
!     function F: i=1,ncol:
!                          F(i)=(GW(i)+ZZ(i)*MUZ)/RT+Ln(mi)+Ln(gammai)
!                          F(i)=(GW(I)+ZZ(I)*MUZ)/RT+DLOG(XM(I))+LOGI(I)*L10
!                 i=ncol+1:
!                          total charge - value: ZSUM - ZWERT
!                          summe(mj*zj) (j=1,ncol)
!                 i>ncol+1:
!                          conc. of component - value: QSUM(j) - CPOT(j)
!                          summe(mj*AA(IR,IC)) (j=1,ncol)
!     Newton-Raphson:
!                    F(xi)=0
!                    F(xi + dx) = F(xi) + J*dx       Jij=dFi/dxj
!                    solve: J*dx = -F
!                           xnew = xold + dx
!     F: CC0(n0)
!     J: JACOB(n0,n0)
!     dx: DX0(n0)
!
!     dFi/dxj:
!        i,j=1 to ncol:          finite difference of CC0 (subroutine RESID)
!
!        i=ncol+1, j=1 to ncol:        d(ZSUM-ZWERT)/dLn(xj) = ZZ(j)*MOLW(j)
!        i=ncol+1, j=ncol+1:           d(ZSUM-ZWERT)/dMUZ = 0
!        i=ncol+1, j=ncol+2 to n0:     d(ZSUM-ZWERT)/dMUEGj = 0
!
!        i=ncol+1+ii, j=1 to ncol:     d(QSUM-CPOT)ii/dLn(xj) = AA(POT(ii),j)*MOLW(j)
!        i=ncol+1+ii, j=ncol+1:        d(QSUM-CPOT)ii/dMUZ = 0
!        i=ncol+1+ii, j=ncol+2 to n0:  d(QSUM-CPOT)ii/dMUEGj = 0
!
!        i=1 to ncol, j=ncol+1:        dFi/dMUZ = ZZ(i)/RT
!        i=1 to ncol, j=ncol+1+ii:     dFi/dMUEGii = AA(POT(ii),i)/RT
!
!---- The initial guess
!     MUZ=0.0D0
      MUZ=NRMUZ
      DO IR=1,NUN
        MUEG(IR)=0.0D0
      END DO
      DO IC=1,NCOL
        GW(IC)=GV(IC)
      END DO
      DO I=1,NCOL
        LNMOL(I)=(-GW(I)-ZZ(I)*MUZ)/RT
        IF (LNMOL(I).GT.1.0D0) LNMOL(I)=1.0D0
        MOLW(I)=DEXP(LNMOL(I))
      END DO

!      WRITE (UNIT=6,FMT='(//,''initial guess'')')
!      CALL AQPRT(MOLW,LOGI,IONST,IONSUM)

!===== start the megaloop
      DO 900,IMEGA=1,NRLOOP
      MEGA=IMEGA
!----- the constant
!----- CC0(1 to ncol)=(GW(I)+ZZ(I)*MUZ)/RT+DLOG(XM(I))+LOGI(I)*L10
      DO I=1,NCOL
        LNXM(I)=LNMOL(I)
        XM(I)=DEXP(LNMOL(I))
      END DO
      CALL RESID(XM,IONST,IONSUM,LOGI,XRES)
      ZSUM=0.0D0
      DO I=1,NCOL
        CC0(I)=XRES(I)
        ZSUM=ZSUM+ZZ(I)*MOLW(I)
      END DO
      N0=NCOL+1
!----- CC0(ncol+1)=ZSUM-ZWERT
      CC0(N0)=ZSUM-ZWERT
      DO IR=1,NROW
        QSUM(IR)=0.0D0
        DO IC=1,NCOL
          QSUM(IR)=QSUM(IR)+AA(IR,IC)*MOLW(IC)
        END DO
      END DO
!----- CC0(ncol+1+i)=QSUM(POT(I))-CPOT(I)
      DO I=1,NPOT
        N0=N0+1
        CC0(N0)=QSUM(POT(I))-CPOT(I)
      END DO
!DC
!      WRITE (6,1046) MEGA,(CNAME(I),I=1,N0)
! 1046 FORMAT ('CNAME',I4,10(8(2X,A12),/,9X))
!      WRITE (6,1045) MEGA,(MOLW(I),I=1,N0)
! 1045 FORMAT ('MOLW ',I4,10(8(2X,1PE12.5),/,9X))
!      WRITE (UNIT=6,FMT='(''MUZ= '',1PE12.5)') MUZ
!!      WRITE (6,1047) MEGA,(ZZ(I),I=1,N0)
!! 1047 FORMAT ('ZZ   ',I4,10(8(2X,1PE12.5),/,9X))
!!      WRITE (6,1042) MEGA,(CC0(I),I=1,N0)
!! 1042 FORMAT ('CC0  ',I4,10(8(2X,1PE12.5),/,9X))
!!      WRITE (6,1043) MEGA,(GW(I),I=1,N0)
!!      WRITE (out,1043) MEGA,(GW(I),I=1,N0)
!! 1043 FORMAT ('GW   ',I4,10(8(2X,1PE12.5),/,9X))
!!      WRITE (6,1044) MEGA,(GV(I),I=1,NCOL)
!!      WRITE (out,1044) MEGA,(GV(I),I=1,NCOL)
!! 1044 FORMAT ('GV   ',I4,10(8(2X,1PE12.5),/,9X))
!---- test convergence
      FERTIG=0
      DO 725,I=1,NCOL
        FF=DABS(CC0(I)*RT)
        IF (FF.GT.1D-3.AND.MOLW(I).GT.1D-50) THEN
        FERTIG=FERTIG+1
        UNF(FERTIG)=I
        END IF
!...    DUM3(I)=FF
  725 CONTINUE
      IF (DABS(ZSUM).GT.1.0D-8) THEN
      FERTIG=FERTIG+1
      UNF(FERTIG)=NCOL+1
      END IF
      DO 726,I=1,NPOT
        FF=DABS(CC0(NCOL+I+1)/CPOT(I))
        IF (FF.GT.1D-5) THEN
        FERTIG=FERTIG+1
        UNF(FERTIG)=NCOL+I+1
        END IF
  726 CONTINUE
!...  DUM3(NCOL+1)=ZSUM
!...  WRITE (UNIT=6,FMT='(''tst '',20E12.4)') (DUM3(I),I=1,NCOL)
!!      CALL AQPRT(MOLW,LOGI,IONST,IONSUM)
      IF (FERTIG.EQ.0) GOTO 901
!-----
!----- the derivatives
!----- columns 1 to ncol
!----- d(XRES)/d(Ln(m)
      DO 730,IC=1,NCOL
      LNXM(IC)=LNXM(IC)+FINDIFF
      XM(IC)=DEXP(LNXM(IC))
      CALL RESID(XM,DUM1,DUM2,DUM3,XRES)
      DO IR=1,NCOL
        JACOB(IR,IC)=(XRES(IR)-CC0(IR))/(FINDIFF)
      END DO
!----- d(total charge)/d(Ln(mi)
      JACOB(NCOL+1,IC)=ZZ(IC)*MOLW(IC)
!----- d(component)/d(Ln(mi)
      DO I=1,NPOT
        JACOB(NCOL+I+1,IC)=AA(POT(I),IC)*MOLW(IC)
      END DO
      LNXM(IC)=LNMOL(IC)
      XM(IC)=MOLW(IC)
  730 CONTINUE
!----- column ncol+1
!----- d(XRES)/d(MUZ)
      DO IR=1,NCOL
        JACOB(IR,NCOL+1)=ZZ(IR)/RT
      END DO
!----- columns ncol+1 to n0
!----- d(XRES)/d(MUEGi)
      DO IR=1,NCOL
        DO I=1,NPOT
          JACOB(IR,NCOL+I+1)=AA(POT(I),IR)/RT
        END DO
      END DO
!----- columns ncol+1 to n0, and rows ncol+1 to n0
!----- d(total charge)/d(MUZ) and (components)/d(MUEGi)
      DO IR=NCOL+1,N0
        DO IC=NCOL+1,N0
          JACOB(IR,IC)=0.0D0
        END DO
      END DO
!---- print jacob
!      DO IR=1,N0
!        WRITE (UNIT=6,FMT='(''jac '',20E12.4)') (JACOB(IR,I),I=1,N0)
!      END DO
!----
!!      CALL LINSOFT(N0,JACOB,CC0,DX0,ERRC)
!!      CALL LINSOFT2(N0,JACOB,CC0,DX0,ERRC)
!
!--------------
! GAUSSEL and LINCOMP seem to give quite identical results
! decide later which one to use (7. Feb 2015)
! only needed for GAUSSEL:
!      WRITE (UNIT=scr,FMT='('' dx0LC'',I4,100(2X,1PE15.8))') &
!       K,(DX0(J),J=1,NUN)
!      COMAY=COMAX
      SING=.FALSE.
      ERRC=0
      DO I=1,N0
       DO J=1,N0
        MAT(I,J)=JACOB(I,J)
       END DO
       MAT(I,N0+1)=CC0(I)
      END DO
!
      CALL GAUSSEL(MAT,I,J,N0,DX0,SING)
      IF (SING) ERRC=1
      IF (SING) PRINT *, 'Matrix is (nearly) singular'
!      WRITE (UNIT=scr,FMT='('' dx0GA'',I4,100(2X,1PE15.8))') &
!       K,(DX0(J),J=1,NUN)
!--------------
!-----
      IF (ERRC.EQ.1) THEN
      AQFAIL=1
      DO 743,IR=1,N0
      WRITE (UNIT=6,FMT='(''jac '',20E12.4)') (JACOB(IR,I),I=1,N0)
      WRITE (UNIT=out,FMT='(''jac '',20E12.4)') (JACOB(IR,I),I=1,N0)
  743 CONTINUE
      END IF
!----
!----check solution
      DO IR=1,N0
        DUM3(IR)=0.0D0
        DO IC=1,N0
          DUM3(IR)=DUM3(IR)+DX0(IC)*JACOB(IR,IC)
        END DO
      END DO
      DO 742,I=1,N0
      FF=DABS(DUM3(I)-CC0(I))
      IF (FF.GT.1D-8) THEN
      WRITE (6,2000) I,FF
      WRITE (out,2000) I,FF
 2000 FORMAT (/'  LINSOFT test>1D-8: index =',I4,'  value =',1PE15.4)
      END IF
  742 CONTINUE
!-----
      IF (ERRC.EQ.1) THEN
      CALL AQPRT(MOLW,LOGI,IONST,IONSUM)
      RETURN
      END IF
!-----
      DO 745,I=1,NCOL
      LNMOL(I)=LNMOL(I)-DX0(I)/NRRED
      IF (LNMOL(I).GT.10.0D0) LNMOL(I)=10.0D0
      IF (LNMOL(I).LT.-200.0D0) LNMOL(I)=-200.0D0
      MOLW(I)=DEXP(LNMOL(I))
  745 CONTINUE
      MUZ=MUZ-DX0(NCOL+1)
      DO 746,I=1,NPOT
      MUEG(POT(I))=MUEG(POT(I))-DX0(NCOL+I+1)/NRRED
  746 CONTINUE
!DC
!      WRITE (6,1056) MEGA,(MUEG(I),I=1,NCOL)
!      WRITE (out,1056) MEGA,(MUEG(I),I=1,NCOL)
! 1056 FORMAT ('MUG',I4,100(2X,1PE12.5))
      DO IC=1,NCOL
        GW(IC)=GV(IC)
        DO IR=1,NROW
          GW(IC)=GW(IC)+AA(IR,IC)*MUEG(IR)
        END DO
      END DO
!     CALL AQPRT(MOLW,LOGI,IONST,IONSUM)
  900 CONTINUE
!===== end of megaloop
  901 CONTINUE
!
!C      IF (PRTEST) CALL AQPRT(MOLW,LOGI,IONST,IONSUM)
!C      IF (PRTEST) WRITE (UNIT=6,FMT='(A132)') CH132
      CALL AQPRT(MOLW,LOGI,IONST,IONSUM)
      WRITE (UNIT=6,FMT='(A132)') CH132
!-----
      RETURN
      END
!-----
!********************************
      SUBROUTINE RESID(XM,IONST,IONSUM,LOGI,XRES)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'aqua.cmn'
!-----END OF COMMON VARIABLES
!----
      REAL(8) XM(PHMAX),LOGI(PHMAX),IONST,IONSUM,XRES(PHMAX)
      INTEGER(4) I
!----
      IONST=0.0D0
      IONSUM=0.0D0
      DO I=1,NCOL
        IONSUM=IONSUM+XM(I)
        IONST=IONST+XM(I)*ZZ(I)**2
      END DO
      IONST=IONST/2.0D0
      DO 600,I=1,NCOL
      IF (ZZ(I).EQ.0.0D0) THEN
!       LOGI(I)=0.0D0
       IF (PHRB(I).EQ.0.0D0) THEN
        LOGI(I)=0.1D0*IONST
       ELSE
        LOGI(I)=PHRB(I)*IONST
       END IF
      ELSE
!====
!---- Davies equation ?
!      LOGI(I)=-AGA*ZZ(I)**2*(DSQRT(IONST) &
!      /(1.0D0+DSQRT(IONST))+0.2D0*IONST)
!---- Davies equation as in PHREEQ
      IF (PHRB(I).EQ.0.0D0.AND.PHRA(I).EQ.0.0D0) THEN
      LOGI(I)=-AGA*ZZ(I)**2*(DSQRT(IONST) &
      /(1.0D0+DSQRT(IONST))-0.3D0*IONST)
      ELSE
      LOGI(I)=-AGA*ZZ(I)**2*DSQRT(IONST) &
      /(1.0D0+BGA*PHRA(I)*DSQRT(IONST))+PHRB(I)*IONST
      END IF
!---- equation 2
!     LOGI(I)=-AGA*ZZ(I)**2*DSQRT(IONST)
!    >/(1.0D0+DSQRT(IONST))
!---- equation 3
!1    LOGI(I)=LOGI(I)
!1   >-DLOG(1.0D0+0.0180153D0*IONSUM)/L10
!====
      END IF
      XRES(I)=(GW(I)+ZZ(I)*MUZ)/RT+DLOG(XM(I))+LOGI(I)*L10
  600 CONTINUE
      RETURN
      END
!-----
!********************************
      SUBROUTINE AQPRT(MOLW,LOGI,IONST,IONSUM)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
      include 'aqua.cmn'
!-----END OF COMMON VARIABLES
      CHARACTER(32) CH16
      REAL(8) X16,FF
!-----END OF VARIABLES FOR MEGAMAT
!----
      REAL(8) LOGI(PHMAX),MOLW(PHMAX),GCORR(CALMAX), &
      IONST,IONSUM,ACT,LACT,EXPO,FF1,FF2, &
      LOGMI,GWCALC,PHW,ACC,LOGAI,XACC,PEW,AWAT
      INTEGER(4) I,II,I1,I2,ITOP(PHMAX)
!----
      WRITE (6,1050)
      WRITE (out,1050)
 1050 FORMAT (/ &
      ' ----------------------------'/ &
      ' Composition of aqueous phase'/ &
      ' ----------------------------')
      WRITE (6,1003) MEGA,FERTIG
      WRITE (out,1003) MEGA,FERTIG
 1003 FORMAT (/'  number of iterations:',I4,'   obvious errors =',I4)
      DO 300,I=1,FERTIG
      IF (UNF(I).GT.NCOL+1) THEN
      WRITE (UNIT=6,FMT='(''  not converging: pot'')')
      WRITE (UNIT=out,FMT='(''  not converging: pot'')')
      ELSE
      WRITE (UNIT=6,FMT='(''  not converging: '',A16)') CNAME(UNF(I))
      WRITE (UNIT=out,FMT='(''  not converging: '',A16)') CNAME(UNF(I))
      END IF
  300 CONTINUE
      IF (PRTEST) WRITE (6,1005) ZSUM,MUZ,IONST,IONSUM
      IF (PRTEST) WRITE (out,1005) ZSUM,MUZ,IONST,IONSUM
 1005 FORMAT (/ &
      '  total charge   =',1PE12.5,10X, &
      'chemical potential of charge = MUZ =',0PF15.4/ &
      '  ionic strength =',1PE12.5,10X, &
      'sum of molalities                  =   ',1PE12.5)
!
!      WRITE (UNIT=6,FMT='(''G(IEP)= '',F20.10)') G(IEP)
      WRITE (UNIT=6,FMT='(''  -G(E)-MUZ = '',F16.5)') -G(IEP)-MUZ
!
      PHW=-DLOG(MOLW(ICH))/L10-LOGI(ICH)
      PEW=(-G(IEP)-MUZ)/RT/L10
      IF (PRTEST) WRITE (6,1006) PHW,PEW
      IF (PRTEST) WRITE (out,1006) PHW,PEW
 1006 FORMAT (/ &
      '  pH =           =',F12.5,7X,' = log a(H+)'/ &
      '  pe =           =',F12.5,7X,' = (-G(E)-MUZ)/RT/ln(10)')
!-----
      CH16='pH'
      X16=PHW
      CALL SETTBL(CH16,X16)
      CH16='pe'
      X16=PEW
      CALL SETTBL(CH16,X16)
!-----
      ITOP(1)=1
      DO 400,I=2,NCOL
      DO I1=1,I-1
        IF (MOLW(ITOP(I1)).LT.MOLW(I)) THEN
        DO I2=I,I1+1,-1
          ITOP(I2)=ITOP(I2-1)
        END DO
        ITOP(I1)=I
        GOTO 411
        END IF
      END DO
      ITOP(I)=I
  411 CONTINUE
  400 CONTINUE
!-----
!     WRITE (6,1010)
!     WRITE (out,1010)
!1010 FORMAT (/
!    >'  species',10X,'z(i)',10X,'G',14X,'G(calc)',
!    >7X,'log g',9X,'log m',10X,'m(i)',10X,'a(i)'/
!    >'  -------',10X,'----',10X,'-',14X,'-------',
!    >7X,'-----',9X,'-----',10X,'----',10X,'----')
!     DO 500,I=1,NCOL
!     LOGMI=DLOG(MOLW(ITOP(I)))/L10
!     ACC=MOLW(ITOP(I))*DEXP(LOGI(ITOP(I))*L10)
!     GWCALC=-RT*(DLOG(MOLW(ITOP(I)))+LOGI(ITOP(I))*L10)-ZZ(ITOP(I))*MUZ
!     WRITE (6,1000) CNAME(ITOP(I)),ZZ(ITOP(I)),GW(ITOP(I)),GWCALC,
!    >LOGI(ITOP(I)),LOGMI,MOLW(ITOP(I)),ACC
!     WRITE (out,1000) CNAME(ITOP(I)),ZZ(ITOP(I)),GW(ITOP(I)),GWCALC,
!    >LOGI(ITOP(I)),LOGMI,MOLW(ITOP(I)),ACC
!1000 FORMAT (2X,A16,F4.1,2X,F15.4,2X,F15.4,
!    >2X,1PE12.5,2X,1PE12.5,2X,1PE12.5,2X,1PE12.5)
! 500 CONTINUE
!-----
      WRITE (6,1011)
      WRITE (out,1011)
 1011 FORMAT (/ &
      '  species',10X,'z(i)',6X,'m(i)',9X,'log m', &
      9X,'log g',10X,'a(i)',9X,'log a',8X,'x(i)', &
      13X,'G',10X,'G(calc)'/ &
      '  -------',10X,'----',6X,'----',9X,'-----', &
      9X,'-----',10X,'----',9X,'-----',8X,'----', &
      13X,'-',10X,'-------')
      DO 501,I=1,NCOL
      LOGMI=DLOG(MOLW(ITOP(I)))/L10
      ACC=MOLW(ITOP(I))*DEXP(LOGI(ITOP(I))*L10)
      GWCALC=-RT*(DLOG(MOLW(ITOP(I)))+LOGI(ITOP(I))*L10)-ZZ(ITOP(I)) &
      *MUZ
      LOGAI=DLOG(ACC)/L10
      XACC=MOLW(ITOP(I))/(IONSUM+55.508435062D0)
      WRITE (6,1001) CNAME(ITOP(I)),ZZ(ITOP(I)),MOLW(ITOP(I)),LOGMI, &
      LOGI(ITOP(I)),ACC,LOGAI,XACC,GW(ITOP(I)),GWCALC
      WRITE (out,1001) CNAME(ITOP(I)),ZZ(ITOP(I)),MOLW(ITOP(I)),LOGMI, &
      LOGI(ITOP(I)),ACC,LOGAI,XACC,GW(ITOP(I)),GWCALC
 1001 FORMAT (2X,A16,F4.1,2X,1PE12.5,2X,1PE12.5, &
      2X,1PE12.5,2X,1PE12.5,2X,1PE12.5,2X,1PE10.3, &
      1X,0PF13.2,1X,0PF13.2)
!-----
      CH16='log_m.'//CNAME(ITOP(I))(1:10)
      X16=LOGMI
      CALL SETTBL(CH16,X16)
!-----
  501 CONTINUE
!-----
      AWAT=1.0D0-0.017D0*IONSUM/55.508435062D0
      IF (PRTEST) WRITE (6,1015) AWAT
      IF (PRTEST) WRITE (out,1015) AWAT
 1015 FORMAT (/,'activity of water (Garrels & Christ, 1965): ',F10.6)
      AWAT=1.0D0-IONSUM/(IONSUM+55.508435062D0)
      IF (PRTEST) WRITE (6,1016) AWAT
      IF (PRTEST) WRITE (out,1016) AWAT
 1016 FORMAT ('concentration of water ( 1 - sum(x) )     : ',F10.6)
!-----
!----- ends after 700
      FF1=55.508435062D0
      FF2=2*FF1
      IF (PRELE.EQ.1) WRITE (6,1021) FF1,FF2
      IF (PRELE.EQ.1) WRITE (out,1021) FF1,FF2
 1021 FORMAT (/,'  m(O in H2O)',5X,1PE12.5,/ &
                '  m(H in H2O)',5X,1PE12.5)

      IF (PRELE.EQ.1) THEN
      IF (PRELE.EQ.1) WRITE (6,1020)
      IF (PRELE.EQ.1) WRITE (out,1020)
 1020 FORMAT (/ &
      '  element',14X,'m(i)',10X,'g/Kg',24X,'G(orig)',7X,'dG(corr)'/ &
      '  -------',14X,'----',10X,'----',24X,'-------',7X,'--------')
      FF1=0.0D0
      FF2=0.0D0
      DO 600,II=1,NROW
!      FF=0.0D0
!      DO 605,I=1,NCOL
!  605 FF=FF+AA(II,I)*MOLW(I)
      FF=0.0D0
      DO 607,I=1,NMAX
      IF (NUMMER(I).EQ.II) FF=G(I)
  607 CONTINUE
      FF1=FF1+QSUM(II)
      FF2=FF2+QSUM(II)*MOLWT(II)
      IF (PRELE.EQ.1) WRITE (6,1025) RNAME(II),QSUM(II), &
      QSUM(II)*MOLWT(II),CHNAME(II),FF,MUEG(II)
      IF (PRELE.EQ.1) WRITE (out,1025) RNAME(II),QSUM(II), &
      QSUM(II)*MOLWT(II),CHNAME(II),FF,MUEG(II)
 1025 FORMAT (2X,A16,2(1PE12.5,2X),A16,1PE12.5,2X,1PE15.8)
      CH16='(aq)'//RNAME(II)
      X16=QSUM(II)
      CALL SETTBL(CH16,X16)
  600 CONTINUE
      IF (PRELE.EQ.1) WRITE (6,1026) FF1,FF2
      IF (PRELE.EQ.1) WRITE (out,1026) FF1,FF2
 1026 FORMAT (18X,'------------',2X,'------------',/ &
              18X,2(1PE12.5,2X))
!
!-----
      DO I=1,NMAX
        GCORR(I)=G(I)
        DO II=1,NUN
          GCORR(I)=GCORR(I)+X(I,II)*MUEG(II)
        END DO
      END DO

      IF (PRELE.EQ.1) WRITE (6,1030)
      IF (PRELE.EQ.1) WRITE (out,1030)
 1030 FORMAT (/ &
      ' --------------------'/ &
      ' activities of phases'/ &
      ' --------------------'/ &
      /2X,'phase',19X,'N',13X,'G',9X,'Activity',7X,'Log(Act)', &
      /2X,'-----',19X,'-',13X,'-',9X,'--------',7X,'--------')
      DO 700,I=1,NMAX
      IF (NUMMER(I).GT.NUN) THEN
      IF ((PHASID(NUMMER(I)).NE.'AQU' &
      .AND.PHASID(NUMMER(I)).NE.'AQP')) THEN
!DC      EXPO=-G(I)/RT
      EXPO=-GCORR(I)/RT
      LACT=EXPO/2.302585093D0
      IF (EXPO.LT.-150.0D0) THEN
      ACT=0.0D0
      ELSE
      ACT=DEXP(DMIN1(150.0D0,EXPO))
      END IF
!DC      WRITE (scr,1035) NAME(NUMMER(I)),NN(I),G(I),ACT,LACT
!DC      WRITE (out,1035) NAME(NUMMER(I)),NN(I),G(I),ACT,LACT
      IF (PRELE.EQ.1) WRITE (scr,1035) NAME(NUMMER(I)),NN(I), &
      GCORR(I),ACT,LACT
      IF (PRELE.EQ.1) WRITE (out,1035) NAME(NUMMER(I)),NN(I), &
      GCORR(I),ACT,LACT
 1035 FORMAT (2X,A16,2X,1PE12.5,2X,1PE12.5,2X,1PE12.5,2X,1PE12.5)
!
!-----
      CH16='log_a.'//NAME(NUMMER(I))(1:10)
      X16=LACT
      CALL SETTBL(CH16,X16)
!-----
      END IF
      END IF
  700 CONTINUE
      END IF
      RETURN
      END
!-----
!********************************
      SUBROUTINE ABPHREEQ(T,A,B)
      IMPLICIT NONE
      REAL(8) T,A,B,tc,tk,s1,s2,s3,c1
!
!     compute temperature dependence of a and b for debye-huckel
!
!     Lines copied from PHREEQC file: model.c (Date: 2000/01/19 20:11:05)
!     in folder PHREEQC 2.1 (created 23. January, 2000)
!     in folder src (created 19. January, 2000)
!
      tc=T-273.15D0
      tk=T
      s1=374.11-tc
!      WRITE (UNIT=6,FMT='('' s1 ='',1PE15.8)') s1
!
      IF (s1.LT.0.0D0) THEN
      s2=-(-s1)**(1.0/3.0)
      ELSE
      s2=s1**(1.0/3.0)
      END IF
!      WRITE (UNIT=6,FMT='('' s2 ='',1PE15.8)') s2
      s3=1.0+0.1342489*s2-3.946263e-03*s1
!      WRITE (UNIT=6,FMT='('' s3 ='',1PE15.8)') s3
      s3=s3/(3.1975-0.3151548*s2-1.203374e-03*s1+ &
      7.48908e-13*(s1*s1*s1*s1))
!CCC      WRITE (UNIT=6,FMT='('' s3a='',1PE15.8)') s3
      s3=DSQRT(s3)
!CCC      WRITE (UNIT=6,FMT='('' s3b='',1PE15.8)') s3
      if (tk.GT.373.15) THEN
      c1=5321.0/tk+233.76-tk*(tk*(8.292e-07*tk-1.417e-03)+0.9297)
!C      WRITE (UNIT=6,FMT='('' c1a='',1PE15.8)') c1
      ELSE
!      /* replaced by wateq4f expression
!      c1=87.74-tc_x*(tc_x*(1.41e-06*tc_x-9.398e-04)+0.4008);
!      */
      c1 = 2727.586+0.6224107*tk-466.9151*DLOG(tk)-52000.87/tk
!C      WRITE (UNIT=6,FMT='('' c1b='',1PE15.8)') c1
      END IF
      c1=sqrt(c1*tk)
!CCC      WRITE (UNIT=6,FMT='('' c1c='',1PE15.8)') c1
!      /* replaced by wateq4f expressions
!      a=1824600.0*s3/(c1 * c1 * c1);
!      b=50.29*s3/c1;
!      */
      A = 1824827.7 * s3 / (c1 * c1 * c1)
!C      WRITE (UNIT=6,FMT='('' A  ='',1PE15.8)') A
      B = 50.2905 * s3 / c1
!C      WRITE (UNIT=6,FMT='('' B  ='',1PE15.8)') B
      RETURN
      END
!-----
!********************************
!---
      SUBROUTINE POTPHAS(AQPHAS)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!-----
      INTEGER(4) I,IN,AQPHAS(COMAX)
!-----
      DO 500,IN=1,NUN
!=====
      NPHA=NPHA+1
      AQPHAS(IN)=NPHA
      SUGNR=SUGNR+1
      NAME(NPHA)='aq-pot.'//CHNAME(IN)
      ABK(NPHA)='a-p.'//CHNAME(IN)(1:4)
      PHASID(NPHA)='POT'
      DO I=1,NUN
        XX(NPHA,I)=0.0D0
      END DO
      XX(NPHA,IN)=1.0D0
      EMSOL(NPHA)=0
      EMNR(NPHA)=0
      NULL(NPHA)=.TRUE.
      G0R=0.0D0
      H0R=0.0D0
      S0R=0.0D0
      V0R=0.0D0
      AA0=0.0D0
      AAT=0.0D0
      BB0=0.0D0
      BBT=0.0D0
      K1=0.0D0
      K2=0.0D0
      K3=0.0D0
      K4=0.0D0
      K5=0.0D0
      K6=0.0D0
      K7=0.0D0
      K8=0.0D0
      K9=0.0D0
      NLANDA=0
      VOLVO=0
      TRTYP=0
      NLANDA=0
      NCOM=0
      DO I=1,10
        ICOM(I)=0
        FFCOM(I)=0.0D0
      END DO
      DO I=1,4
        ASPK(I)=0.0D0
        BSPK(I)=0.0D0
        TQ1B(I)=0.0D0
        TEQ(I)=0.0D0
        DVDT(I)=0.0D0
        DVDP(I)=0.0D0
        TRE(I)=0.0D0
        DHTR(I)=0.0D0
        DVTR(I)=0.0D0
      END DO
      TD0=0.0D0
      TDMAX=0.0D0
      VADJ=0.0D0
      D1=0.0D0
      D2=0.0D0
      D3=0.0D0
      D4=0.0D0
      D5=0.0D0
      D6=0.0D0
      D7=0.0D0
      D8=0.0D0
      D9=0.0D0
      VTA=0.0D0
      VTB=0.0D0
      VPA=0.0D0
      VPB=0.0D0
      VAA=0.0D0
      VAB=0.0D0
      VB=0.0D0
      VL0=0.0D0
      VLA=0.0D0
      VLN=0.0D0
      VL2=0.0D0
      TKRI=0.0D0
      SMA=0.0D0
      NAT=0.0D0
      FALL=' '
      RDK=.FALSE.
      VDW=.FALSE.
      SPC=.FALSE.
      COM=.TRUE.
      DIS=.FALSE.
      VO1=.TRUE.
      VO2=.FALSE.
      VO3=.FALSE.
      AQU=.FALSE.
      FIX=.FALSE.
      TL1=.FALSE.
      CALL DASAVE(NPHA)
!=====
  500 CONTINUE
!-----
      RETURN
      END
!-----
!********************************
!---
      SUBROUTINE ADDAQ(XES,NDLOOP)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!-----
      INTEGER(4) I,NDLOOP
      REAL(8) XES(COMAX)
      CHARACTER(8) TEXT
!-----
!=====
      WRITE (UNIT=TEXT,FMT='(''fluid'',I3.3)') NDLOOP
      IF (NAME(NPHA)(1:5).NE.'fluid') NPHA=NPHA+1
      SUGNR=SUGNR+1
!      NAME(NPHA)='fluid'
      NAME(NPHA)=TEXT
      ABK(NPHA)='fl'
      PHASID(NPHA)='POT'
      DO I=1,NUN
        XX(NPHA,I)=XES(I)
      END DO
      EMSOL(NPHA)=0
      EMNR(NPHA)=0
      NULL(NPHA)=.FALSE.
      G0R=0.0D0
      H0R=0.0D0
      S0R=0.0D0
      V0R=0.0D0
      AA0=0.0D0
      AAT=0.0D0
      BB0=0.0D0
      BBT=0.0D0
      K1=0.0D0
      K2=0.0D0
      K3=0.0D0
      K4=0.0D0
      K5=0.0D0
      K6=0.0D0
      K7=0.0D0
      K8=0.0D0
      K9=0.0D0
      NLANDA=0
      VOLVO=0
      TRTYP=0
      NLANDA=0
      NCOM=0
      DO I=1,10
        ICOM(I)=0
        FFCOM(I)=0.0D0
      END DO
      DO I=1,4
        ASPK(I)=0.0D0
        BSPK(I)=0.0D0
        TQ1B(I)=0.0D0
        TEQ(I)=0.0D0
        DVDT(I)=0.0D0
        DVDP(I)=0.0D0
        TRE(I)=0.0D0
        DHTR(I)=0.0D0
        DVTR(I)=0.0D0
      END DO
      TD0=0.0D0
      TDMAX=0.0D0
      VADJ=0.0D0
      D1=0.0D0
      D2=0.0D0
      D3=0.0D0
      D4=0.0D0
      D5=0.0D0
      D6=0.0D0
      D7=0.0D0
      D8=0.0D0
      D9=0.0D0
      VTA=0.0D0
      VTB=0.0D0
      VPA=0.0D0
      VPB=0.0D0
      VAA=0.0D0
      VAB=0.0D0
      VB=0.0D0
      VL0=0.0D0
      VLA=0.0D0
      VLN=0.0D0
      VL2=0.0D0
      TKRI=0.0D0
      SMA=0.0D0
      FALL=' '
      RDK=.FALSE.
      VDW=.FALSE.
      SPC=.FALSE.
      COM=.TRUE.
      DIS=.FALSE.
      VO1=.TRUE.
      VO2=.FALSE.
      VO3=.FALSE.
      AQU=.FALSE.
      FIX=.FALSE.
      TL1=.FALSE.
      CALL DASAVE(NPHA)
!=====
!-----
      RETURN
      END
!********************************
!---
      SUBROUTINE ADDWATER(INH2O,IXH2O)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!-----
      INTEGER(4) I,INH2O,IXH2O
      CHARACTER(8) TEXT
!-----
!=====
      IXH2O=0
      DO 500,I=1,NPHA
      IF (NAME(I).EQ.'H2Opot') IXH2O=I
  500 CONTINUE
      IF (IXH2O.NE.0) RETURN
      NPHA=NPHA+1
      SUGNR=SUGNR+1
      NAME(NPHA)='H2Opot'
      NAME(NPHA)=TEXT
      ABK(NPHA)='H2Op'
      PHASID(NPHA)='POT'
      DO I=1,NUN
        XX(NPHA,I)=XX(INH2O,I)
      END DO
      EMSOL(NPHA)=0
      EMNR(NPHA)=0
      NULL(NPHA)=.FALSE.
      G0R=0.0D0
      H0R=0.0D0
      S0R=0.0D0
      V0R=0.0D0
      AA0=0.0D0
      AAT=0.0D0
      BB0=0.0D0
      BBT=0.0D0
      K1=0.0D0
      K2=0.0D0
      K3=0.0D0
      K4=0.0D0
      K5=0.0D0
      K6=0.0D0
      K7=0.0D0
      K8=0.0D0
      K9=0.0D0
      NLANDA=0
      VOLVO=0
      TRTYP=0
      NLANDA=0
      NCOM=0
      DO I=1,10
        ICOM(I)=0
        FFCOM(I)=0.0D0
      END DO
      DO I=1,4
        ASPK(I)=0.0D0
        BSPK(I)=0.0D0
        TQ1B(I)=0.0D0
        TEQ(I)=0.0D0
        DVDT(I)=0.0D0
        DVDP(I)=0.0D0
        TRE(I)=0.0D0
        DHTR(I)=0.0D0
        DVTR(I)=0.0D0
      END DO
      TD0=0.0D0
      TDMAX=0.0D0
      VADJ=0.0D0
      D1=0.0D0
      D2=0.0D0
      D3=0.0D0
      D4=0.0D0
      D5=0.0D0
      D6=0.0D0
      D7=0.0D0
      D8=0.0D0
      D9=0.0D0
      VTA=0.0D0
      VTB=0.0D0
      VPA=0.0D0
      VPB=0.0D0
      VAA=0.0D0
      VAB=0.0D0
      VB=0.0D0
      VL0=0.0D0
      VLA=0.0D0
      VLN=0.0D0
      VL2=0.0D0
      TKRI=0.0D0
      SMA=0.0D0
      FALL=' '
      RDK=.FALSE.
      VDW=.FALSE.
      SPC=.FALSE.
      COM=.TRUE.
      DIS=.FALSE.
      VO1=.TRUE.
      VO2=.FALSE.
      VO3=.FALSE.
      AQU=.FALSE.
      FIX=.FALSE.
      TL1=.FALSE.
      CALL DASAVE(NPHA)
!=====
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
      WRITE (UNIT=scr,FMT=1002)
      WRITE (UNIT=out,FMT=1002)
 1002 FORMAT (/ &
      ' ----------------'/ &
      ' stable assemblge'/ &
      ' ----------------'/ &
      9X,'phase',19X,'N',13X,'G',9X,'activity')
      CALL PRTSTR(1,NUN)
!-----
      RETURN
      END
!-----
!********************************
      SUBROUTINE ANASS(CURAS)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!-----
      CHARACTER(125) CURAS
      INTEGER(4) I,I1,IL
      CHARACTER(16) TEXT
!-----
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
!-----
      RETURN
      END
!-----
!********************************
!---
      SUBROUTINE SPEZSUB(AQFAIL)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
      include 'aqua.cmn'
!-----END OF COMMON VARIABLES
!-----Common block
      REAL(8) XXMIN,XXMAX,XXVAL,XXDELTA,IONSUM
      INTEGER(4) I,AQFAIL,NB,NAQLOOP
      CHARACTER(16) XXVAR
!-----
      XXVAR='Ox-Buffer'
      XXMIN=-35.0D0
      XXMAX=-45.0D0
      NAQLOOP=50+1
      XXDELTA=(XXMAX-XXMIN)/DBLE(NAQLOOP)
      XXVAL=XXMIN-XXDELTA
!-----
      DO 4500,NB=1,NAQLOOP
      WRITE (UNIT=6,FMT='(/''spezial loop:'',I4)') NB
      WRITE (UNIT=out,FMT='(/''spezial loop:'',I4)') NB
      CALL DBREAD
!-----------------------------------------------------------------
      XXVAL=XXVAL+XXDELTA
      DO 400,I=1,NPHA
      IF (NAME(I).EQ.XXVAR) THEN
      REDATA(3,I)=-R*XXVAL*2.302585093D0
      END IF
  400 CONTINUE
!-----------------------------------------------------------------
      DO 672,I=1,NPHA
      IF (PHASID(I).EQ.'AQU'.OR.PHASID(I).EQ.'AQP') THEN
      NULL(I)=.TRUE.
      END IF
  672 CONTINUE
      CALL NURVONPT
      CALL CALSTR
      WRITE (UNIT=6,FMT='(/133A)') ('=',I=1,133)
      WRITE (UNIT=6,FMT= &
      '(''CALL THERIAK (SPEZSUB)'')')
      WRITE (UNIT=6,FMT='(133A)') ('=',I=1,133)
      IF (PRTLOG(2).OR.PRTLOG(3).OR.PRTLOG(4)) CALL PRININ
      CALL THERIA
      WRITE (UNIT=6,FMT='(133A)') ('=',I=1,133)
!-----------------------------------------------------------------
!-----------------------------------------------------------------
!----
      NROWTBL=NROWTBL+1
      CALL AQEQ(AQFAIL,IONSUM)
!      DO 750,I=1,NC
!      SOLBUL(I)=0.0D0
!      LABUL(I)=BULK(I)
!  750 CHE(I)=0.0D0
!
!
!      DO 754,I=1,NUN2
!      ISASOLID(I)=.TRUE.
!      IF (NUMMER(I).LE.0) THEN
!      TEXT(I)=SOLNAM(EMCODE(I))
!      IS=EMCODE(I)
!      DO 755,II=1,NEND(IS)
!      IF (PHASID(EM(IS,II)).NE.'MIN') ISASOLID(I)=.FALSE.
!  755 CONTINUE
!      ELSE
!      TEXT(I)=NAME(NUMMER(I))
!      IF (PHASID(NUMMER(I)).NE.'MIN') ISASOLID(I)=.FALSE.
!      END IF
!  754 CONTINUE
!C-----
!      DO 757,I=1,NUN
!      IF (OXYDE(CHMCOD(I)).NE.'O '.AND.OXYDE(CHMCOD(I)).NE.'H '.
!     >AND.OXYDE(CHMCOD(I)).NE.'E ') THEN
!      CHE(CHMCOD(I))=BULK(I)-XKGH2O*55.0D+1*QSUM(I)
!      ELSE
!      CHE(CHMCOD(I))=BULK(I)
!      END IF
!      IF (CHE(CHMCOD(I)).LE.0.0D0) CHE(CHMCOD(I))=0.0D0
!  757 CONTINUE
!      MORE=.FALSE.
!      DO 760,I=1,NC
!      CHE(I)=CHE(I)+XKGH2O*55.0D+1*INFLOW(I)
!      IF (CHE(I).EQ.0.0D0.NEQV.CHEM(I).EQ.0.0D0) MORE=.TRUE.
!      CHEM(I)=CHE(I)
!  760 CONTINUE
!      IF (MORE) THEN
!      CALL DBREAD
!      ELSE
!      DO 765,I=1,NUN
!  765 BULK(I)=CHE(CHMCOD(I))
!      END IF
 4500 CONTINUE
!---- the last call to DBREAD is apparently not necessary (??)
!-----------------------------------------------------------------
!-----------------------------------------------------------------
      RETURN
      END









!-----
!********************************
      SUBROUTINE EQ36
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!-----END OF COMMON VARIABLES
!-----
!-----Common block 
      REAL(8) AA(40,1000),BB(40,1000),DET,GWERT(1000)
      INTEGER(4) NCOL,NROW,RANG,IDNR(1000)
      CHARACTER(16) CNAME(1000),RNAME(40)
      COMMON /AQMARE/ AA,BB,DET,GWERT
      COMMON /AQMAIN/ NCOL,NROW,RANG,IDNR
      COMMON /AQMACH/ CNAME,RNAME
!----
      INTEGER(4) I,I1,I2,IR,IC,IJX,NBASIS,NBEF,NAEF,NMEF, &
      NEEF,IJY,HIT(50),ICH,GEBRAU(1000),I001
      REAL(8) FF,F1,RH2O,AL,BE,DALDT,DBEDT,DBEDP,EE,DEDP,DEDT,DETT,  &
      AGA,BGA,ZERO,ONE,ZUG(100),TCGRID(8),COLSUM,COLWERT
      CHARACTER(16) BASIS(100),CH8,AZUG(100)
      CHARACTER(70) BLOCKC
      CHARACTER(250) CH001
!----
      OPEN (UNIT=15,FILE='basis',STATUS='OLD')
      OPEN (UNIT=16,FILE='data0',STATUS='UNKNOWN')
      OPEN (UNIT=17,FILE='input',STATUS='UNKNOWN')
      ZERO=0.0D0
      ONE=1.0D0
      TCGRID(1)=0.0D0
      TCGRID(2)=100.0D0
      TCGRID(3)=200.0D0
      TCGRID(4)=300.0D0
      TCGRID(5)=400.0D0
      TCGRID(6)=500.0D0
      TCGRID(7)=600.0D0
      TCGRID(8)=700.0D0
      DO I=1,70
        BLOCKC(I:I)='-'
      END DO
      BLOCKC(1:1)='+'
      NBASIS=0
      DO I=1,100
        NBASIS=NBASIS+1
        READ (UNIT=15,FMT='(A16)',END=444) BASIS(NBASIS)
      END DO
  444 NBASIS=NBASIS-1
!----
      NROW=NUN
      NCOL=0
      NBEF=0
      NAEF=0
      NMEF=0
      NEEF=0
      DO IR=1,NROW
        RNAME(IR)=CHNAME(IR)
        IF (RNAME(IR)(1:1).EQ.' ') THEN
        CH8=RNAME(IR)(2:)
        RNAME(IR)=CH8
        END IF
        CALL UPLOW(RNAME(IR))
      END DO
!+++++
!+++++Copy all information into AA and BB etc.
!+++++
      DO 600,IC=1,NMAX
      IF (IC.LE.NUN.OR.SUGG(IC).LE.NPHA) THEN
!-----
      NCOL=NCOL+1
      IF (NUMMER(IC).EQ.0) THEN
      CNAME(NCOL)=SOLNAM(EMCODE(IC))
      ELSE
      CNAME(NCOL)=NAME(NUMMER(IC))
      END IF
      CALL UPLOW(CNAME(NCOL))
      DO I=1,NCOL-1
        IF (CNAME(I).EQ.CNAME(NCOL)) THEN
        CH001='$'//CNAME(NCOL)(1:15)
        CNAME(NCOL)=CH001(1:16)
        END IF
      END DO
      IDNR(NCOL)=NUMMER(IC)
      GWERT(NCOL)=G(IC)
      DO IR=1,NROW
        AA(IR,NCOL)=X(IC,IR)
        BB(IR,NCOL)=X(IC,IR)
      END DO
!-----
      CH001=' '
      WRITE (UNIT=CH001,FMT=3029) NCOL,CNAME(NCOL),SUGG(IC), &
      PHASID(NUMMER(IC))
 3029 FORMAT ('   NCOL,CNAME(NCOL),SUGG,PHASID: ',I4,A20,I4,2X,A3)

      DO 650,I=1,NBASIS
      IF (CNAME(NCOL).EQ.BASIS(I)) THEN
      CH001(70:)='---->basis'
!      WRITE (6,3030) CNAME(IC)
! 3030 FORMAT (41X,A16,12X,'this is a basis')
      NBEF=NBEF+1
      CALL INSERT(NCOL,NBEF)
      GOTO 655
      END IF
  650 CONTINUE
!-----
      IF (SUGG(IC).LT.-NUN.AND.PHASID(IDNR(NCOL)).EQ.'AQU' &
      .OR.PHASID(IDNR(NCOL)).EQ.'AQP') THEN
      CH001(70:)='--------->aq.species'
!      WRITE (6,3031) CNAME(IC)
! 3031 FORMAT (41X,A16,12X,'this is an aq.species')
      NAEF=NAEF+1
      I2=NBEF+NAEF
      CALL INSERT(NCOL,I2)
      GOTO 655
      END IF
!-----
      IF (SUGG(IC).GT.0) THEN
      CH001(70:)='-------------->mineral'
!      WRITE (6,3032) CNAME(IC)
! 3032 FORMAT (41X,A16,12X,'this is a mineral')
      NMEF=NMEF+1
      I2=NBEF+NAEF+NMEF
      CALL INSERT(NCOL,I2)
      GOTO 655
      END IF
!-----
      IF (SUGG(IC).GE.-NUN.AND.SUGG(IC).LT.0) THEN
      CH001(70:)='element'
!      WRITE (6,3033) CNAME(IC)
! 3033 FORMAT (41X,A16,12X,'this is an element')
      NEEF=NEEF+1
      END IF
  655 CALL PUST(6,CH001)
      CONTINUE
!-----
      END IF
  600 CONTINUE
!+++++
      WRITE (UNIT=6,FMT='(133A)') ('-',I=1,133)
      WRITE (6,FMT='(''the input matrix A'')')
      CALL MATPRT
      WRITE (UNIT=6,FMT='(133A)') ('-',I=1,133)
      WRITE (6,1000) NEEF,NBEF,NAEF,NMEF,NCOL
! 1000 FORMAT (' nb, na, nm, ne, ntot:',5I7)
 1000 FORMAT(' elements: ',I3,' |  basis: ',I3,' |  aq.species: ', &
      I3,' |  minerals: ',I3,' |    total: ',I4)
      I1=1
      IJX=NBEF
      CALL AQREDUC(I1,NBEF)
      WRITE (UNIT=6,FMT='(133A)') ('-',I=1,133)
      WRITE (6,FMT='(''the reduced matrix A'')')
      CALL MATPRT
!-----
      DO 720,I=1,NBEF
      IF (CNAME(I).EQ.'steam') THEN
      CNAME(I)='h2o'
      I1=1
      CALL COLCH(I1,I)
      CALL ROWCH(I1,I)
      GOTO 721
      END IF
  720 CONTINUE
  721 CONTINUE
      DO 725,I=1,NBEF
!     IF (CNAME(I).EQ.'o2,aq') THEN
      IF (CNAME(I).EQ.'oxygen') THEN
      CNAME(I)='o2.g'
      I1=NBEF
      CALL COLCH(I1,I)
      CALL ROWCH(I1,I)
      GOTO 726
      END IF
  725 CONTINUE
  726 CONTINUE
!-----
      DO I=NBEF+NAEF+NMEF+1,NCOL
        CH8=CNAME(I)(2:)        
        I1=INDEX(CH8,'"')
        CNAME(I)=CH8(1:I1-1)
      END DO
      DO 740,I=1,NEEF
      I1=NBEF+NAEF+NMEF+I
      IF (CNAME(I1).NE.RNAME(I)) THEN
      DO 742,I2=I1+1,NCOL
      IF (CNAME(I2).EQ.RNAME(I)) THEN
      CALL COLCH(I1,I2)
      GOTO 739
      END IF
  742 CONTINUE
      END IF
  739 CONTINUE
  740 CONTINUE
      WRITE (UNIT=6,FMT='(133A)') ('-',I=1,133)
      WRITE (6,FMT='(''the output matrix A'')')
      CALL MATPRT
      WRITE (UNIT=6,FMT='(133A)') ('-',I=1,133)
      WRITE (6,FMT='(''the original matrix B'')')
      CALL MATPRTB
      WRITE (UNIT=6,FMT='(133A)') ('-',I=1,133)
!-----
      DO I=1,1000
        GEBRAU(I)=0
      END DO
!-----
      DO 750,IJX=2,NBEF
      COLWERT=0.0D0
      HIT(IJX)=1
      DO 760,IC=NBEF+NAEF+1,NBEF+NAEF+NMEF
      IF (GWERT(IC).NE.0.0D0.OR.GEBRAU(IC).NE.0) GOTO 759
      COLSUM=0.0D0
      DO I=1,NROW
        COLSUM=COLSUM+DABS(AA(I,IC))
      END DO
      IF (COLSUM.NE.0.0D0) THEN
      FF=DABS(AA(IJX,IC))/COLSUM
      IF (FF.GT.COLWERT) THEN
      COLWERT=FF
      HIT(IJX)=IC
      END IF
      END IF
  759 CONTINUE
  760 CONTINUE
      GEBRAU(HIT(IJX))=1
  750 CONTINUE
      WRITE (UNIT=6,FMT='(2X,10A16)') (CNAME(I),I=2,NBEF)
      WRITE (UNIT=6,FMT='(2X,10A16)') (CNAME(HIT(I)),I=2,NBEF)
      CALL RHOETC(P,T,RH2O,AL,BE,DALDT,DBEDT,DBEDP)
      CALL JN91(T,RH2O,BE,AL,DALDT,EE,DEDP,DEDT,DETT)
      AGA=1.824829238D6*DSQRT(RH2O)/DSQRT((EE*T)**3)
      BGA=50.29158649D8*DSQRT(RH2O)/DSQRT(EE*T)*1D-8
      ICH=0
      DO I=1,NROW
        IF (RNAME(I).EQ.'e') ICH=I
      END DO
      IF (ICH.EQ.0) THEN
      WRITE (6,FMT='(''no charge found'')')
      RETURN
      END IF
!++++++++++++++++++++++++++++++++++++++ write "input"
      WRITE (UNIT=6,FMT='(133A)') ('-',I=1,133)
      WRITE (UNIT=6,FMT='('' now writing file: input'')')
      CALL LABLA(DBNAME,I001)
      WRITE (17,1500) DBNAME(1:I001)
 1500 FORMAT ('EQ3NR Input file name = data0.cdc'/ &
      'direct pipeline from THERIAQ, using ',A,/'endit.')
      WRITE (17,1501) TC
 1501 FORMAT ('     tempc= ',F12.5)
      WRITE (17,1502) RH2O,ZERO,ZERO
 1502 FORMAT ('       rho= ',F12.5,'     tdspkg=',F12.5, &
      '      tdspl=',F12.5)
      WRITE (17,1503) ZERO
 1503 FORMAT ('       fep= ',F12.5,'     uredox=')
      WRITE (17,1504) ZERO,ZERO,ZERO
 1504 FORMAT ('     tolbt= ',F12.5,'      toldl=',F12.5, &
      '     tolsat=',F12.5)
      WRITE (17,1505) ZERO
 1505 FORMAT ('    itermix ',F12.5)
      WRITE (17,1506)
 1506 FORMAT( &
      '*               1    2    3    4    5    6    7    8    9   10'/ &
      '  iopt1-10=    -3    0    0    0    0    0    0    0    0    0'/ &
      '  iopg1-10=    -1   -1    0    0    0    0    0    0    0    0'/ &
      '  iopr1-10=     0    0    0    0    0    0    1    0    0    0'/ &
      ' iopr11-20=     0    0    0    0    0    0    0    0    0    0'/ &
      '  iodb1-10=     0    0    0    0    0    0    0    0    0    0'/ &
      '     uebal= h+'/)
      DO 770,IJX=2,NBEF
      IF (HIT(IJX).NE.1) THEN
      WRITE (17,1507) CNAME(IJX)
 1507 FORMAT ('data file master species= ',A16/ &
      '   switch with species=')
      IF (CNAME(IJX).EQ.'h+') THEN
      WRITE (17,1508)
 1508 FORMAT ('   jflag= 16   csp= -7.0')
      ELSE 
      WRITE (17,1509) CNAME(HIT(IJX))
 1509 FORMAT ('   jflag= 19   csp= 0.'/' mineral= ',A16)
      END IF
      END IF
  770 CONTINUE
      WRITE (17,1510)
 1510 FORMAT ('endit.')
!++++++++++++++++++++++++++++++++++++++
      WRITE (16,2000) DBNAME(1:i001),BLOCKC,BLOCKC
 2000 FORMAT ('data0.com.R10'/'THERMODYNAMIC DATABASE'/  &
      'generated by THERIAQ, using ',A,/A70/'data0 parameters'/A70)
!     WRITE (16,2001) TC,TC
!2001 FORMAT ('temperature limits'/5X,2F10.4)
      WRITE (16,2001) TCGRID(1),TCGRID(8)
 2001 FORMAT ('temperature limits'/5X,2F10.4)
!     WRITE (16,2002) (TC,I=1,8)
!2002 FORMAT ('temperatures'/5X,4F10.4/5X,4F10.4)
      WRITE (16,2002) (TCGRID(I),I=1,8)
 2002 FORMAT ('temperatures'/5X,4F10.4/5X,4F10.4)
      WRITE (16,2003) (P,I=1,8)
 2003 FORMAT ('pressures'/5X,4F10.4/5X,4F10.4)
!-----
      WRITE (16,2004) (AGA,I=1,8)
 2004 FORMAT ('debye huckel a (adh)'/5X,4F10.4/5X,4F10.4)
      WRITE (16,2005) (BGA,I=1,8)
 2005 FORMAT ('debye huckel b (bdh)'/5X,4F10.4/5X,4F10.4)
!-----
      WRITE (16,2006) (ZERO,I=1,8)
 2006 FORMAT ('bdot'/5X,4F10.4/5X,4F10.4)
      WRITE (16,2007) (ZERO,I=1,5)
 2007 FORMAT ('cco2 (coeff. for Drummond,1981)'/5X,F10.4,11X,F12.7, &
      /10X,F5.1,11X,F12.4/5X,F10.6)
      WRITE (16,2008) (ONE,I=1,8)
 2008 FORMAT ('log k for eh reaction)'/5X,4F10.4/5X,4F10.4)
      WRITE (UNIT=16,FMT='(A70)') BLOCKC
!-----
      WRITE (16,2010) BLOCKC
 2010 FORMAT ('bdot parameters'/A70)
      DO 800,IC=1,NBEF+NAEF
      WRITE (16,2011) CNAME(IC),ONE,IDINT(ZERO)
 2011 FORMAT (A16,8X,7X,F7.1,4X,I2)
  800 CONTINUE
      WRITE (UNIT=16,FMT='(A70)') BLOCKC
!-----
      WRITE (16,2012) BLOCKC
 2012 FORMAT ('elements'/A70)
      DO 805,I=1,NUN-1
      CH8=OXYDE(CHMCOD(I))
      CALL UPLOW(CH8)
      WRITE (16,2013) CH8,MOLGEW(CHMCOD(I))
 2013 FORMAT (A8,F10.5)
  805 CONTINUE
      WRITE (UNIT=16,FMT='(A70)') BLOCKC
!++++++++++++++++++++++++++++++++++++++
      WRITE (16,2014) BLOCKC
 2014 FORMAT ('basis species'/A70)
      DO 810,IC=1,NBEF
      WRITE (16,2015) CNAME(IC)
 2015 FORMAT (A16/'    date last revised = never'/  &
      ' keys = basis')
      WRITE (16,2016) BB(ICH,IC)
 2016 FORMAT ('    charge =  ',F5.1)
      IJX=0
      DO 815,I=1,NROW
      IF (I.NE.ICH) THEN
      IF (BB(I,IC).NE.0.0D0) THEN
      IJX=IJX+1
      ZUG(IJX)=BB(I,IC)
      AZUG(IJX)=CNAME(NBEF+NAEF+NMEF+I)
      END IF
      END IF
  815 CONTINUE
      WRITE (16,2017) IJX
 2017 FORMAT (4X,I2,' chemical elements =')
      WRITE (16,2018) (ZUG(I),AZUG(I),I=1,IJX)
 2018 FORMAT ('    ',3(F8.4,1X,A8,5X))
      WRITE (UNIT=16,FMT='(A70)') BLOCKC
  810 CONTINUE
!++++++++++++++++++++++++++++++++++++++
      WRITE (16,2019) BLOCKC
 2019 FORMAT ('auxiliary basis species'/A70)
      WRITE (16,2020) BLOCKC
 2020 FORMAT ('aqueous species'/A70)
!++++++++++++++++++++++++++++++++++++++
      DO 950,IC=NBEF+1,NBEF+NAEF+NMEF
!dC   DO 950,IC=NBEF+1,NCOL
      IF (IC.EQ.NBEF+NAEF+1) THEN
      WRITE (16,3020) BLOCKC
 3020 FORMAT ('solids'/A70)
      END IF
      IF (CNAME(IC).EQ.'charge') GOTO 949
      IF (CNAME(IC).EQ.'e') GOTO 949
      WRITE (16,2021) CNAME(IC),PHASID(IDNR(IC))
 2021 FORMAT (A16/'    date last revised = never'/ &
      ' keys = ',A3)
      WRITE (16,2022) BB(ICH,IC)
 2022 FORMAT ('    charge =  ',F5.1)
!-----
      IJX=0
      DO 820,I=1,NROW
      IF (I.NE.ICH) THEN
      IF (BB(I,IC).NE.0.0D0) THEN
      IJX=IJX+1
      ZUG(IJX)=BB(I,IC)
      AZUG(IJX)=CNAME(NBEF+NAEF+NMEF+I)
      END IF
      END IF
  820 CONTINUE
      WRITE (16,2023) IJX
 2023 FORMAT (4X,I2,' chemical elements =')
      IJY=MIN(3,IJX)
      WRITE (16,2024) (ZUG(I),AZUG(I),I=1,IJY)
 2024 FORMAT ('    ',3(F8.4,1X,A8,5X))
      IF (IJX.GT.3) THEN
      IJY=MIN(6,IJX)
      WRITE (16,2024) (ZUG(I),AZUG(I),I=4,IJY)
      END IF 
      IF (IJX.GT.6) THEN
      IJY=MIN(9,IJX)
      WRITE (16,2024) (ZUG(I),AZUG(I),I=7,IJY)
      END IF 
!-----
      IJX=1
      ZUG(1)=-1.0D0
      AZUG(1)=CNAME(IC)
      DO 830,I=1,NROW
      IF (AA(I,IC).NE.0.0D0) THEN
      IJX=IJX+1
      ZUG(IJX)=AA(I,IC)
      AZUG(IJX)=CNAME(I)
      END IF
  830 CONTINUE
      WRITE (16,2025) IJX
 2025 FORMAT (4X,I2,' species in reaction =')
      WRITE (16,2026) (ZUG(I),AZUG(I),I=1,IJX)
 2026 FORMAT (2(1X,F10.4,2X,A16,8X))
!-----
!-----
      FF=-GWERT(IC)
      DO 910,IR=1,NROW
      IF (AA(IR,IC).NE.0.0D0) THEN
!     WRITE (6,1110) AA(IR,IC),CNAME(IR)
!1110 FORMAT (F10.5,2X,A16)
      FF=FF+AA(IR,IC)*GWERT(IR)
      END IF
  910 CONTINUE
      F1=-FF/RT/2.302585092994D0
      WRITE (16,2027) F1,F1,F1,F1,F1,F1,F1,F1
 2027 FORMAT ('*     log k grid'/5X,4F10.4/5X,4F10.4)
!2027 FORMAT (30X,'G =',F20.6,4X,'LOGK =',F12.8)
      WRITE (UNIT=16,FMT='(A70)') BLOCKC
  949 CONTINUE
  950 CONTINUE
!++++++++++++++++++++++++++++++++++++++
      WRITE (16,2028) BLOCKC
 2028 FORMAT ('liquids'/A70)
      WRITE (16,2029) BLOCKC
 2029 FORMAT ('gases'/A70)
      WRITE (16,2030) BLOCKC
 2030 FORMAT ('solid solutions'/A70)
      WRITE (16,2031) BLOCKC
 2031 FORMAT ('references'/A70//'stop.')
!++++++++++++++++++++++++++++++++++++++
      END
!=====
!********************************
      SUBROUTINE MATPRT
      IMPLICIT NONE
!----
!----
!-----Common block 
      REAL(8) AA(40,1000),BB(40,1000),DET,GWERT(1000)
      INTEGER(4) NCOL,NROW,RANG,IDNR(1000)
      CHARACTER(16) CNAME(1000),RNAME(40)
      COMMON /AQMARE/ AA,BB,DET,GWERT
      COMMON /AQMAIN/ NCOL,NROW,RANG,IDNR
      COMMON /AQMACH/ CNAME,RNAME
!----
      INTEGER(4) I,II
      WRITE (UNIT=6,FMT='(10(14X,10A11/))') (CNAME(I),I=1,NCOL)
      WRITE (UNIT=6,FMT='(10(14X,10(1PE11.4)/))') (GWERT(I),I=1,NCOL)
      DO 500,II=1,NROW
      WRITE (UNIT=6,FMT='(/A10,10(1PE11.4),10(/10X,10(1PE11.4)))')  &
      RNAME(II),(AA(II,I),I=1,NCOL)
  500 CONTINUE
      RETURN
      END
!=====
!********************************
      SUBROUTINE MATPRTB
      IMPLICIT NONE
!----
!----
!-----Common block 
      REAL(8) AA(40,1000),BB(40,1000),DET,GWERT(1000)
      INTEGER(4) NCOL,NROW,RANG,IDNR(1000)
      CHARACTER(16) CNAME(1000),RNAME(40)
      COMMON /AQMARE/ AA,BB,DET,GWERT
      COMMON /AQMAIN/ NCOL,NROW,RANG,IDNR
      COMMON /AQMACH/ CNAME,RNAME
!----
      INTEGER(4) I,II
!!      WRITE (UNIT=6,FMT='(10(14X,10A10/))') (CNAME(I),I=1,NCOL)
!!      WRITE (UNIT=6,FMT='(10(14X,10F10.3/))') (GWERT(I),I=1,NCOL)
      WRITE (UNIT=6,FMT='(10(14X,10A11/))') (CNAME(I),I=1,NCOL)
      WRITE (UNIT=6,FMT='(10(14X,10(1PE11.4)/))') (GWERT(I),I=1,NCOL)
      DO 500,II=1,NROW
      WRITE (UNIT=6,FMT='(/A10,10F10.5,10(/10X,10F10.5))') &
!!      WRITE (UNIT=6,FMT='(/A10,10(1PE11.4),10(/10X,10(1PE11.4)))')  &
      RNAME(II),(BB(II,I),I=1,NCOL)
  500 CONTINUE
      RETURN
      END
!=====
!********************************
      SUBROUTINE DIVROW(I1,FF)
      IMPLICIT NONE
!----
!-----Common block 
      REAL(8) AA(40,1000),BB(40,1000),DET,GWERT(1000)
      INTEGER(4) NCOL,NROW,RANG,IDNR(1000)
      CHARACTER(16) CNAME(1000),RNAME(40)
      COMMON /AQMARE/ AA,BB,DET,GWERT
      COMMON /AQMAIN/ NCOL,NROW,RANG,IDNR
      COMMON /AQMACH/ CNAME,RNAME
!----
      INTEGER(4) I1,I
      REAL(8) FF
      DO I=1,NCOL
        AA(I1,I)=AA(I1,I)/FF
      END DO
      RETURN
      END
!=====
!********************************
      SUBROUTINE COLCH(I1,I2)
      IMPLICIT NONE
!----
!-----Common block 
      REAL(8) AA(40,1000),BB(40,1000),DET,GWERT(1000)
      INTEGER(4) NCOL,NROW,RANG,IDNR(1000)
      CHARACTER(16) CNAME(1000),RNAME(40)
      COMMON /AQMARE/ AA,BB,DET,GWERT
      COMMON /AQMAIN/ NCOL,NROW,RANG,IDNR
      COMMON /AQMACH/ CNAME,RNAME
!----
      INTEGER(4) I,I001,I1,I2
      REAL(8) FA(50),FB(50),F1
      CHARACTER(16) CH16
!----
      IF (I1.EQ.I2.OR.I1.GT.NCOL.OR.I2.GT.NCOL) RETURN
      I001=IDNR(I1)
      F1=GWERT(I1)
      CH16=CNAME(I1)
      DO I=1,NROW
        FB(I)=BB(I,I1)
        FA(I)=AA(I,I1)
      END DO
      IDNR(I1)=IDNR(I2)
      GWERT(I1)=GWERT(I2)
      CNAME(I1)=CNAME(I2)
      DO I=1,NROW
        BB(I,I1)=BB(I,I2)
        AA(I,I1)=AA(I,I2)
      END DO
      IDNR(I2)=I001
      GWERT(I2)=F1
      CNAME(I2)=CH16
      DO I=1,NROW
        BB(I,I2)=FB(I)
        AA(I,I2)=FA(I)
      END DO
      RETURN
      END
!=====
!********************************
      SUBROUTINE INSERT(IOUT,IIN)
      IMPLICIT NONE
!----
!-----Common block 
      REAL(8) AA(40,1000),BB(40,1000),DET,GWERT(1000)
      INTEGER(4) NCOL,NROW,RANG,IDNR(1000)
      CHARACTER(16) CNAME(1000),RNAME(40)
      COMMON /AQMARE/ AA,BB,DET,GWERT
      COMMON /AQMAIN/ NCOL,NROW,RANG,IDNR
      COMMON /AQMACH/ CNAME,RNAME
!----
      INTEGER(4) I,I001,I1,I2,IOUT,IIN,INC
      REAL(8) FA(50),FB(50),F1
      CHARACTER(16) CH16
!----
      IF (IOUT.EQ.IIN.OR.IOUT.GT.NCOL.OR.IIN.GT.NCOL) RETURN
      I2=1
      IF (IOUT.GT.IIN) THEN
        INC=-1
      ELSE
        INC=1
      END IF
      I001=IDNR(IOUT)
      F1=GWERT(IOUT)
      CH16=CNAME(IOUT)
      DO I=1,NROW
        FB(I)=BB(I,IOUT)
        FA(I)=AA(I,IOUT)
      END DO
!-----
      DO 505,I1=IOUT,IIN-INC,INC
      I2=I1+INC
      IDNR(I1)=IDNR(I2)
      GWERT(I1)=GWERT(I2)
      CNAME(I1)=CNAME(I2)
      DO I=1,NROW
        BB(I,I1)=BB(I,I2)
        AA(I,I1)=AA(I,I2)
      END DO
  505 CONTINUE
!-----
      IDNR(IIN)=I001
      GWERT(IIN)=F1
      CNAME(IIN)=CH16
      DO I=1,NROW
        BB(I,IIN)=FB(I)
        AA(I,I2)=FA(I)
      END DO
      RETURN
      END
!=====
!********************************
      SUBROUTINE ROWCH(I1,I2)
      IMPLICIT NONE
!----
!-----Common block 
      REAL(8) AA(40,1000),BB(40,1000),DET,GWERT(1000)
      INTEGER(4) NCOL,NROW,RANG,IDNR(1000)
      CHARACTER(16) CNAME(1000),RNAME(40)
      COMMON /AQMARE/ AA,BB,DET,GWERT
      COMMON /AQMAIN/ NCOL,NROW,RANG,IDNR
      COMMON /AQMACH/ CNAME,RNAME
!----
      INTEGER(4) I,I1,I2
      CHARACTER(16) CH16
      REAL(8) FA(1000),FB(1000)
!----
      CH16=RNAME(I1)
      DO I=1,NCOL
        FB(I)=BB(I1,I)
        FA(I)=AA(I1,I)
      END DO
      RNAME(I1)=RNAME(I2)
      DO I=1,NCOL
        BB(I1,I)=BB(I2,I)
        AA(I1,I)=AA(I2,I)
      END DO
      RNAME(I2)=CH16
      DO I=1,NCOL
        BB(I2,I)=FB(I)
        AA(I2,I)=FA(I)
      END DO
      RETURN
      END
!=====
!********************************
      SUBROUTINE SUBSTR(I1,F1,I2)
      IMPLICIT NONE
!----
!-----Common block 
      REAL(8) AA(40,1000),BB(40,1000),DET,GWERT(1000)
      INTEGER(4) NCOL,NROW,RANG,IDNR(1000)
      CHARACTER(16) CNAME(1000),RNAME(40)
      COMMON /AQMARE/ AA,BB,DET,GWERT
      COMMON /AQMAIN/ NCOL,NROW,RANG,IDNR
      COMMON /AQMACH/ CNAME,RNAME
!----
      INTEGER(4) I,I1,I2
      REAL(8) F1
!----
      DO 500,I=1,NCOL
        AA(I2,I)=AA(I2,I)-AA(I1,I)*F1
        IF (DABS(AA(I2,I)).LE.1.0D-10) AA(I2,I)=0.0D0
  500 CONTINUE
      RETURN
      END
!=====
!********************************
      SUBROUTINE AQREDUC(I1,MAL)
      IMPLICIT NONE
!----
!-----Common block 
      REAL(8) AA(40,1000),BB(40,1000),DET,GWERT(1000)
      INTEGER(4) NCOL,NROW,RANG,IDNR(1000)
      CHARACTER(16) CNAME(1000),RNAME(40)
      COMMON /AQMARE/ AA,BB,DET,GWERT
      COMMON /AQMAIN/ NCOL,NROW,RANG,IDNR
      COMMON /AQMACH/ CNAME,RNAME
!----
!!      INTEGER(4) PIV,I001,I002,NROW1,MAL,POS
      INTEGER(4) PIV,MAL,POS
!!      REAL(8) COLSUM(100),WERT
!----
!!      INTEGER(4) IR,IC,IX,IY,I,II,I1,I2
      INTEGER(4) IR,IY,II,I1,I2
      REAL(8) FF,F1
      RANG=0
      DO 600,PIV=1,MAL
      POS=PIV+I1-1
!     DO 400,IC=PIV,MAL
!     COLSUM(IC)=0.0D0
!     DO 400,IR=1,NROW
! 400 COLSUM(IC)=COLSUM(IC)+DABS(AA(IR,IC+I1-1))
!     IY=0
!     IX=0
!     WERT=0.0D0
!     DO 610,IR=PIV,MAL
!     DO 620,IC=PIV,MAL
!     IF (COLSUM(IC).LE.0.0D0) GOTO 609
!     FF=DABS(AA(IR,IC+I1-1)/COLSUM(IC))
!     IF (FF.GT.WERT) THEN
!     WERT=FF
!     IX=IC+I1-1
!     IY=IR
!     END IF
! 609 CONTINUE
! 620 CONTINUE
!     IF (WERT.NE.0.0D0) GOTO 611
! 610 CONTINUE
! 611 CONTINUE
!     IF (IX.EQ.0.OR.IY.EQ.0) RETURN
!     IF (IX.NE.POS) CALL COLCH(IX,POS)
      DO 410,IR=PIV,MAL
      IF (AA(IR,POS).NE.0.0D0) THEN
      IY=IR
      GOTO 411
      END IF
  410 CONTINUE
  411 CONTINUE
      IF (IY.NE.PIV) CALL ROWCH(IY,PIV)
      FF=AA(PIV,POS)
!     WRITE (UNIT=6,FMT='(''FF= '',F10.4)') FF
      RANG=PIV
      IF (PIV.EQ.1) THEN 
      DET=FF
      ELSE
      DET=DET*FF
      END IF
      CALL DIVROW(PIV,FF)
      DO 500,II=1,NROW
      IF (II.NE.PIV) THEN
      F1=AA(II,POS)
      I2=II
      CALL SUBSTR(PIV,F1,I2)
      END IF
  500 CONTINUE
!      WRITE (6,FMT='(''ON THE WAY matrix A'')')
!      CALL MATPRT
  600 CONTINUE
      RETURN
      END
!=====
!******************************
      SUBROUTINE UPLOW(REC)
      IMPLICIT NONE
! ----
      CHARACTER* (*) REC
      CHARACTER(250) CH001
      CHARACTER(26) UPPER,LOWER
      INTEGER(4) I1,I2,I,J
      DATA UPPER /'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/ 
      DATA LOWER /'abcdefghijklmnopqrstuvwxyz'/ 
! ----

!      RETURN

      CALL LABLA(REC,I1)
      CH001=' '
      I2=1
      DO 500,I=1,I1
      J=INDEX(UPPER,REC(I:I))
      IF (J.EQ.0) THEN 
      CH001(I2:I2)=REC(I:I)
      ELSE 
      CH001(I2:I2)=LOWER(J:J)
      END IF 
      I2=I2+1
  500 CONTINUE 
      REC=CH001
      RETURN
      END

