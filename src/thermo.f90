!-----Version: 28.05.2022
!               **********
!               * THERMO *
!               **********
!
!     Program written by Christian de Capitani
!     at the Department of Geology
!            Stanford University
!            Stanford, CA., 94305   (1989-1991)
!     and at Mineralogisch-Petrographisches Institut
!            Universitaet Basel     (since 1992)
!
!     revision: November 1993
!     revision: October 2002
!
!     for details of algorithm see: (not documented)
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
      INCLUDE 'files.cmn'
!-----END OF COMMON VARIABLES
      INTEGER(4) I001,I002,I,I0,I1,COMAY,ICO,ierr,j
      REAL(8) FF
      CHARACTER(500) CH001,CH002,CH003,SYREC,CHIN(2),ZEITSTRING
      CHARACTER(8) SORTSTRING(PHMAX)
      CHARACTER(4) CH3
!*****
      CALL CPUTIME(ZEITSTRING)
      progname='THERMO'
      vers='28.05.2022'
      task='"Computation of thermodynamic functions"'
      ierr=0
      call initialize('$THERMO-FILES',ierr)
      if(ierr.ne.0) STOP
      I1=6
!*****
      CHIN(1)=' '
      CHIN(2)=' '
!------------------
!     open UNIT=log
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
  411 CONTINUE
!-----
      WRITE (scr,112)
  112 FORMAT (/ &
      '-------------------'/ &
      'database definition'/ &
      '-------------------')
      CALL LABLA(CHIN(1),I002)
      IF (I002.EQ.0) I002=1
      CH002='Enter [ "?" | CR | "files" | database filename ] <'// &
      CHIN(1)(1:I002)//'>?'
!-----
  412 continue
      CALL PUST (6,CH002)
      READ (kbd,FMT='(A500)') CH001
      IF (CH001.EQ.' ') THEN
          CH001=CHIN(1)
          I001=I002
      else if (CH001.eq.'?') then
         call helpme('$THM-START')
         goto 412
      else if (VERGL(CH001,'files')) then
         call listfiles
         goto 412
      ELSE
          CHIN(1)=CH001
      END IF
!-----
      CALL LABLA(CH001,I001)
      IF (I001.EQ.0) THEN
      CH001=CHIN(1)
      I001=I002
      ELSE
      CHIN(1)=CH001
      END IF
!----
      CH002=CH001
      CALL TAXI(CH002,DBNAME)
!------------------
!     open UNIT=out
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
!*****
      REWIND (UNIT=out)
      REWIND (UNIT=dbs)
      REWIND (UNIT=dat)
      CLOSE (UNIT=log)
!*****
      COMAY=COMAX
      CALL PROREAD(SYREC)
!-----
!-----READ  PRTCOD, FORMUL AND USE FROM SYREC
!-----SET UP FIRST NUN COLUMNS OF MATRIX
      CALL GELI(SYREC,FF)
      PRTCOD=IDINT(FF)
      DO I=1,11
        PRTLOG(I)=.FALSE.
      END DO
      PRTLOG(3)=.TRUE.
      PRTLOG(4)=.TRUE.
!-----
      CALL TAXI(SYREC,FORMUL)
      CALL TAXI(SYREC,USE)
!     CALL CHEMIE(COMAY,NC,OXYDE,OXANZ,FORMUL,CHEM)
      CALL LABLA(USE,LUSE)
!     IF (PRTCOD.EQ.0) TEST=DABS(TEST)
      DO I=1,NC
        CHEM(I)=1.0D0
      END DO
      CALL DBREAD
!
      CALL CHECKFROMCAL(1)
!
      CLOSE(UNIT=dat)
!-----START LOOPING
    1 CONTINUE
      DO I=NUN+1,NPHA
        SORTSTRING(I-NUN)=ABK(I)
      END DO
      I001=NPHA-NUN
      CALL SORTIER(SORTSTRING,I001)
      WRITE (6,1010) I001
 1010 FORMAT ('List of phases in database: (',I4,')')
      WRITE (6,1000) (SORTSTRING(I),I=1,I001)
 1000 FORMAT (8A10)
      DO I=1,NPHA
        GGK(I)=0.0D0
      END DO
      PGAS=P*PRAT
      T=TC+273.15D0
    2 WRITE (UNIT=6,FMT=2020) P,PGAS,TC,T
      WRITE (UNIT=out,FMT=2020) P,PGAS,TC,T
 2020 FORMAT (/'P =',F9.2,' bar    P(Gas) =',F9.2,' bar', &
      '    T =',F8.2,' C   = ',F8.2,' K')
!     WRITE (6,2022) (CHNAME(I),I=1,NUN)
!2022 FORMAT ('0SYSTEM:  ',6A8)
      WRITE (6,2000)
 2000 FORMAT (/ &
      '----------------'/ &
      'phase definition'/ &
      '----------------')
      CALL LABLA(CHIN(2),I002)
      IF (I002.EQ.0) I002=1
   10 WRITE (scr,2011) CHIN(2)(1:I002)
 2011 FORMAT (/'Enter [ "?" | CR | "list" | "end" ', &
      '| "TP" t p |', &
      ' "g" | "v" | phases ] <',a,'>? ')
      READ (UNIT=5,FMT='(A500)') CH001
      IF (CH001.EQ.' ') THEN
      CH001=CHIN(2)
      else if (CH001.eq.'?') then
          call helpme('$THM-WAS')
          goto 10
      ELSE
      CH3=CH001(1:4)
      CALL LOWUP(CH3)
      IF (CH3.NE.'?   '.AND.CH3.NE.'END ' &
      .AND.CH3.NE.'TP  '.AND.CH3.NE.'G   ' &
      .AND.CH3.NE.'V   '.AND.CH3.NE.'TAB '.AND.CH3.NE.'LIST' &
      .AND.CH3.NE.'VG  '.AND.CH3.NE.'GC  ') THEN
      CHIN(2)=CH001
      END IF
      END IF
!*****
    3 CALL TAXI(CH001,NAM)
      IF (VERGL(NAM,'g')) THEN
      ICO=1
      CH003=CHIN(2)
      CALL GTABLE(CH003,ICO)
      GOTO 2
      END IF
      IF (VERGL(NAM,'v')) THEN
      ICO=2
      CH003=CHIN(2)
      CALL GTABLE(CH003,ICO)
      GOTO 2
      END IF
      IF (VERGL(NAM,'vg')) THEN
       ICO=3
       CH003=CHIN(2)
       CALL GTABLE(CH003,ICO)
       GOTO 2
      END IF


      IF (VERGL(NAM,'gc')) THEN
       ICO=5
       CH003=CHIN(2)
       CALL GTABLE(CH003,ICO)
       GOTO 2
      END IF




      IF (VERGL(NAM,'tab')) THEN
      CALL STATABLE
      GOTO 2
      END IF
      IF (VERGL(NAM,'list')) GOTO 1
      IF (NAM.EQ.' ') GOTO 2
      IF (VERGL(NAM,'end')) GOTO 2099
      IF (VERGL(NAM,'tp')) THEN
      CALL GELI(CH001,TC)
      CALL GELI(CH001,P)
      T=TC+273.15D0
      END IF
!-----
      IF (VERGL(NAM,'all')) THEN
      OPEN (UNIT=33,FILE='pth',STATUS='UNKNOWN')
      DO I=NUN+1,NPHA
        CALL GRECAL(I)
      END DO
      CLOSE (UNIT=33)
      GOTO 2
      END IF
!-----
      I0=0
      DO I=NUN+1,NPHA
        IF (NAM.EQ.ABK(I)) THEN
        I0=I
        END IF
      END DO
      IF (I0.EQ.0) THEN
      GOTO 2
      ELSE
      CALL GRECAL(I0)
      END IF
      GOTO 3
 2099 CONTINUE
!+++++store terminal input
      j=log
      line=filename(j)(1:fnl(j))//ext(j)
      path=wpath
      akzess=' '
      state='OLD'
      call openfile(j,ierr)
      if(ierr.ne.0) STOP
      DO I=1,2
        CALL PUST(log,CHIN(I))
      END DO
      CLOSE (UNIT=log)
      END
!-----
!******************************
      SUBROUTINE GRECAL(II)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      INCLUDE 'files.cmn'
!-----END OF COMMON VARIABLES
      INTEGER(4) I001,I,II
      REAL(8) FG,ZERO,FF
      CHARACTER(16) U16,SPNAM
!     CALL NURVONPT
      T=TC+273.15D0
      I001=1
!-----
      IF (T.LE.0.0) THEN
      CALL SHOUTF
      WRITE (6,1100) T
      WRITE (out,1100) T
 1100 FORMAT (//' T =',F8.2,'  The Temperature is less or equal 0 K')
      STOP
      END IF
      IF (P.LT.0.0) THEN
      CALL SHOUTF
      WRITE (6,1110) P
      WRITE (out,1110) P
 1110 FORMAT (//' P =',F9.2,'  The Pressure is less than zero')
      STOP
      END IF
!-----
      PGAS=P*PRAT
      RT=R*T
      TT=T*T
      SQT=DSQRT(T)
!+++++
      U16='----------------'
      CALL DAREST(II)
      GR=0.0D0
      HR=0.0D0
      SR=0.0D0
      CPR=0.0D0
      VOLUM=0.0D0
      DO I=16,1,-1
        IF (NAME(II)(I:I).NE.' ') THEN
        I001=I
        GOTO 791
        END IF
      END DO
  791 CONTINUE
      WRITE (UNIT=6,FMT='(//78(''=''))')
      WRITE (UNIT=out,FMT='(//78(''=''))')
      WRITE (6,2010) NAME(II),P,TC,T,(U16(I:I),I=1,I001)
      WRITE (out,2010) NAME(II),P,TC,T,(U16(I:I),I=1,I001)
 2010 FORMAT (' ',A16,2X,'P =',F9.2,' bar', &
      '   T =',F8.2,' C  =',F8.2,' K'/' ',16A1)
      WRITE (6,3000)
      WRITE (out,3000)
 3000 FORMAT ( &
      17X,'G[J/mol]',5X,'H[J/mol]',3X,'S[J/K/mol]', &
      2X,'CP[J/K/mol]',2X,'V[J/Bar/mol]'/ &
      17X,'--------',5X,'--------',3X,'----------', &
      2X,'-----------',2X,'------------')
!=====
      SPNAM=NAME(II)
      IF (SPC) THEN
      CALL GSPEC(SPNAM,P,PGAS,T,FALL,GR,VOLUM)
      WRITE (6,1002) GR,FALL
      WRITE (out,1002) GR,FALL
 1002 FORMAT (' EXTERNAL:',3X,F12.2,3X,'(',A16,')')
      GOTO 9
      ELSE
      LIQ=.FALSE.
!=====
      CALL GCALC(II)
      END IF
!=====
      ZERO=0.0D0
!      FG0=H0R-T0*S0R
      WRITE (6,2050) FG0,H0R,S0R,FCP0,V0R
      WRITE (out,2050) FG0,H0R,S0R,FCP0,V0R
 2050 FORMAT ('  25 C,1Bar:',F13.3,F13.3,1X,F12.6,1X, &
      F12.4,1X,F12.4)
      WRITE (6,2055) FGCP,FHCP,FSCP,FCPCP,ZERO
      WRITE (out,2055) FGCP,FHCP,FSCP,FCPCP,ZERO
 2055 FORMAT ('CP-FUNCTION:',F13.3,F13.3,1X,F12.6,1X, &
      F12.4,1X,F12.4)
      WRITE (6,2060) FGVOL,FVVOL
      WRITE (out,2060) FGVOL,FVVOL
 2060 FORMAT (' V-FUNCTION:',F13.3,40X,F12.4)
      WRITE (6,2065) FGDIS,FHDIS,FSDIS,FCPDIS,FVDIS
      WRITE (out,2065) FGDIS,FHDIS,FSDIS,FCPDIS,FVDIS
 2065 FORMAT ('   DISORDER:',F13.3,F13.3,1X,F12.6,1X, &
      F12.4,1X,F12.4)
      WRITE (6,2070) FGGAS,FVGAS
      WRITE (out,2070) FGGAS,FVGAS
 2070 FORMAT ('  GAS-PHASE:',F13.3,40X,F12.4)
      WRITE (6,2075) NLANDA,FGTR,FHTR,FSTR,FCPTR,FVTR
      WRITE (out,2075) NLANDA,FGTR,FHTR,FSTR,FCPTR,FVTR
 2075 FORMAT ('   LAMBDA',I2,':',F13.3,F13.3,1X,F12.6,1X, &
      F12.4,1X,F12.4)
      WRITE (6,2080) FGSOL,FHSOL,FSSOL,FCPSOL,FVSOL
      WRITE (out,2055) FGSOL,FHSOL,FSSOL,FCPSOL,FVSOL
 2080 FORMAT ('  SOLVATION:',F13.3,F13.3,1X,F12.6,1X, &
      F12.4,1X,F12.4)
!=====
      IF (COM) THEN
      DO I=1,NCOM
        FG=FFCOM(I)*GGK(ICOM(I))
        IF (FG.EQ.0.0D0) THEN
          WRITE (6,1010) NAME(ICOM(I)),FFCOM(I)
          WRITE (out,1010) NAME(ICOM(I)),FFCOM(I)
 1010 FORMAT ('ADD:',9X,'????????????',3X, &
        '(',A16,' * ',F9.4,')')
        ELSE
          WRITE (6,1012) FG,NAME(ICOM(I)),FFCOM(I)
          WRITE (out,1012) FG,NAME(ICOM(I)),FFCOM(I)
 1012 FORMAT ('ADD:',9X,F12.2,3X, &
        '(',A16,' * ',F9.4,')')
        END IF
        GR=GR+FFCOM(I)*GGK(ICOM(I))
      END DO
      END IF
    9 CONTINUE
      GGK(II)=GR
      WRITE (6,1022)
      WRITE (out,1022)
 1022 FORMAT (13X,'------------ ------------ ------------ ', &
      '------------ ------------')
      WRITE (6,1020) GR,HR,SR,CPR,VOLUM
      WRITE (out,1020) GR,HR,SR,CPR,VOLUM
 1020 FORMAT (' TOTAL:',5X,F13.3,F13.3,1X,F12.6,1X, &
      F12.4,1X,F12.4)
      WRITE (UNIT=6,FMT='(78(''=''))')
      WRITE (UNIT=out,FMT='(78(''=''))')
!
      FF=0.0D0
      CALL FROMCAL(II,GR,FF)
      WRITE (6,1025) FF
 1025 FORMAT (' TOTAL:',5X,F13.3,' [Cal/mol] ', &
      '(relative to G0, as in supcrt)')
!
      IF (TC.EQ.5025) THEN
      WRITE (UNIT=6,FMT='(''PTH = '',1PE15.7)') FSPEC(1)
      WRITE (UNIT=6,FMT='(''a   = '',1PE15.7)') FSPEC(2)
      WRITE (UNIT=6,FMT='(''b   = '',1PE15.7)') FSPEC(3)
      WRITE (UNIT=6,FMT='(''c   = '',1PE15.7)') FSPEC(4)
      WRITE (UNIT=33,FMT='(A)') NAME(II)
      WRITE (33,2020) FSPEC(2),FSPEC(3),FSPEC(4),FSPEC(1)/5000.0D0
 2020 FORMAT (' EX1 ',4(1PE15.7))
      END IF
!
!
      RETURN
      END
!-----
!********************************
      SUBROUTINE GTABLE(CH001,ICO)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      INCLUDE 'files.cmn'
!-----END OF COMMON VARIABLES
      INTEGER(4) I,I1,I2,IP,N0,IN(PHMAX),J,NT,NP,ICO,ILF,I3,I4
      REAL(8) DASG(10,18),DAST(10),DASP(18),TCORI,PORI, &
      DASVG(10,18),PVOR,DXP,DG1,DG2,GFROM,GTO
      CHARACTER(500) CH001
      CHARACTER(16) CH16(PHMAX)
      CHARACTER(50) TEXT,TEXT2
      CHARACTER(15) FORTX
      TCORI=TC
      PORI=P
      NT=10
      NP=18
      DAST(1)=25.0D0
      DAST(2)=100.0D0
      DAST(3)=200.0D0
      DAST(4)=300.0D0
      DAST(5)=500.0D0
      DAST(6)=800.0D0
      DAST(7)=1000.0D0
      DAST(8)=1200.0D0
      DAST(9)=1400.0D0
      DAST(10)=1600.0D0
!-----
      DASP(1)=1.0D0
      DASP(2)=1000.0D0
      DASP(3)=2000.0D0
      DASP(4)=3000.0D0
      DASP(5)=4000.0D0
      DASP(6)=5000.0D0
      DASP(7)=6000.0D0
      DASP(8)=7000.0D0
      DASP(9)=8000.0D0
      DASP(10)=9000.0D0
      DASP(11)=10000.0D0
      DASP(12)=20000.0D0
      DASP(13)=30000.0D0
      DASP(14)=40000.0D0
      DASP(15)=50000.0D0
      DASP(16)=80000.0D0
      DASP(17)=100000.0D0
      DASP(18)=120000.0D0
!*****
      TEXT='undefinded'
      TEXT2='undefinded'
      FORTX='F10.2,10F12.1'
      IF (ICO.EQ.1) THEN
      TEXT='Gibbs Free Energy [J/mol]'
      FORTX='(F10.2,10F12.1)'
      END IF
      IF (ICO.EQ.5) THEN
      TEXT='Gibbs Free Energy [Cal/mol] relative to G0'
      FORTX='(F10.2,10F12.1)'
      END IF
      IF (ICO.EQ.2.OR.ICO.EQ.3) THEN
      TEXT='Volume [J/Bar/mol]'
      FORTX='(F10.2,10F12.4)'
      END IF
      CALL LABLA(FORTX,ILF)
!*****
      CALL TAXI(CH001,CH16(1))
      IF (VERGL(CH16(1),'all')) THEN
      N0=NPHA-NUN
      DO J=1,N0
         IN(J)=NUN+J
      END DO
      GOTO 3
      ELSE
      N0=1
      END IF
!*****
    1 N0=N0+1
      CALL TAXI(CH001,CH16(N0))
      IF (CH16(N0).EQ.' ') THEN
      N0=N0-1
      GOTO 2
      ELSE
      GOTO 1
      END IF
!*****
    2 CONTINUE
      DO J=1,N0
        IN(J)=0
      END DO
      DO J=1,N0
        DO IP=NUN+1,NPHA
          IF (ABK(IP).EQ.CH16(J)) THEN
            IN(J)=IP
          END IF
        END DO
      END DO
!*****
    3 DO 500,J=1,N0
      IP=IN(J)
      IF (IP.EQ.0) GOTO 99
      DO 507,I1=1,NT
      DO 505,I2=1,NP
      TC=DAST(I1)
      P=DASP(I2)
      CALL NURVONPT
      DASG(I1,I2)=0.0D0
      IF (ICO.EQ.1) DASG(I1,I2)=GGK(IP)
      IF (ICO.EQ.5) THEN
       GTO=GGK(IP)
       CALL FROMCAL(IP,GTO,GFROM)
       DASG(I1,I2)=GFROM
      END IF
      IF (ICO.EQ.2.OR.ICO.EQ.3) DASG(I1,I2)=VV(IP)
      IF (ICO.EQ.3) THEN
        PVOR=P
        DXP=P/1D5
        P=PVOR+DXP
        CALL NURVONPT
        DG1=GGK(IP)
        P=PVOR-DXP
        CALL NURVONPT
        DG2=GGK(IP)
        DASVG(I1,I2)=(DG1-DG2)/(2.0D0*DXP)*1.0D0

        IF (PHASID(IP).EQ.'AQU'.OR.PHASID(IP).EQ.'AQP') THEN
         DASVG(I1,I2)=10.0D0*DASVG(I1,I2)
        END IF

        P=PVOR
        CALL NURVONPT
      END IF
  505 CONTINUE
  507 CONTINUE

      IF (PHASID(IP).EQ.'XXX') TEXT2='unknown type'
      IF (PHASID(IP).EQ.'MIN') TEXT2='mineral or gas'
      IF (PHASID(IP).EQ.'SOL') TEXT2='solution'
      IF (PHASID(IP).EQ.'ELE') TEXT2='component'
      IF (PHASID(IP).EQ.'AQU') TEXT2='aqueous species'
      IF (PHASID(IP).EQ.'AQP') TEXT2='aqueous species'
      IF (PHASID(IP).EQ.'AQ2') TEXT2='aqueous species'
      CALL LABLA(TEXT2,I4)

      CALL LABLA(TEXT,I3)
      WRITE (6,1000) NAME(IP),ABK(IP),TEXT(1:I3),TEXT2(1:I4)
      WRITE (out,1000) NAME(IP),ABK(IP),TEXT(1:I3),TEXT2(1:I4)
 1000 FORMAT(/A16,4X,A8,4X,A,2X,'(',A,')')
      WRITE (6,1001) (DAST(I),I=1,NT)
      WRITE (out,1001) (DAST(I),I=1,NT)
 1001 FORMAT ('P / T',4X,10F12.2)
!!      WRITE (UNIT=6,FMT='('' '')')
!!      WRITE (UNIT=out,FMT='('' '')')
      DO 600,I2=1,NP
      WRITE (UNIT=6,FMT=FORTX(1:ILF)) DASP(I2),(DASG(I,I2),I=1,NT)
      WRITE (UNIT=out,FMT=FORTX(1:ILF)) DASP(I2),(DASG(I,I2),I=1,NT)
      IF (ICO.EQ.3) THEN
       WRITE (UNIT=6,FMT=FORTX(1:ILF)) DASP(I2),(DASVG(I,I2),I=1,NT)
       WRITE (UNIT=out,FMT=FORTX(1:ILF)) DASP(I2),(DASVG(I,I2),I=1,NT)
       WRITE (UNIT=6,FMT='('' '')')
       WRITE (UNIT=out,FMT='('' '')')
      END IF
! 1002 FORMAT (F10.2,10F12.1)
  600 CONTINUE
   99 CONTINUE
  500 CONTINUE
      TC=TCORI
      P=PORI
      RETURN
      END
!-----
!********************************
      SUBROUTINE STATABLE
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      INCLUDE 'files.cmn'
!-----END OF COMMON VARIABLES
      INTEGER(4) I2,IP,JI,j,ierr
      CHARACTER(16) NAM1,NAM2
!---  makes a table of phases that occur twice in database
!---  for comparison (not yet finished)
!------------------
!     open UNIT=tab
!------------------
      j=tab
      line=filename(j)(1:fnl(j))//ext(j)
      path=wpath
      akzess=' '
      state=' '
      call openfile(j,ierr)
      if(ierr.ne.0) STOP
!*****
      DO 500,IP=NUN+1,NPHA
      NAM1=NAME(IP)
      CALL LOWUP(NAM1)
      DO 600,I2=IP+1,NPHA
      NAM2=NAME(I2)
      CALL LOWUP(NAM2)
      IF (NAM1.EQ.NAM2) THEN
      TC=25.0D0
      P=1.0D0
      CALL NURVONPT
      CALL DAREST(IP)
      CALL GCALC(IP)
      WRITE (tab,1000) NAME(IP),H0R,S0R,V0R,GGK(IP),VV(IP), &
      (XX(IP,JI),JI=1,NUN)
      CALL DAREST(I2)
      CALL GCALC(I2)
      WRITE (tab,1000) NAME(I2),H0R,S0R,V0R,GGK(I2),VV(I2), &
      (XX(I2,JI),JI=1,NUN)
 1000 FORMAT(A16,2X,F13.3,1X,F13.3,1X,F12.6,1X,F13.3,1X,F13.3, &
      50(1X,F6.2))
      END IF
  600 CONTINUE
  500 CONTINUE
      CLOSE (UNIT=tab)
      RETURN
      END
!-----
!********************************
