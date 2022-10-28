!-----Version: 28.05.2022
!               **********
!               * THALIA *
!               **********
!
!     Calculation of any thermodynamic function
!
!     Program written by Christian de Capitani
!     at the Department of Geology
!            Stanford University
!            Stanford, CA., 94305   (1989-1991)
!     and at Mineralogisch-Petrographisches Institut
!            Universitaet Basel     (since 1992)
!
!     revision: July 1993
!     revision: October 2002
!
!     for details of algorithm see: (not documented)
!
!     Any suggestions, complaints or comments are greatly appreciated
!     by the author and should be sent to:
!
!          Christian de Capitani
!          Mineralogisch-Petrographisches Institut
!          Universitaet Basel
!          Bernoullistrasse 30
!          CH-4056 BASEL
!
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      INCLUDE 'files.cmn'
      INTEGER(4) I001,I002,I,COMAY,j,ierr
      REAL(8) FF
      CHARACTER(500) CH001,CH002,SYREC,CHIN(11)
      COMMON /CHINPU/ CHIN
!-----END OF COMMON VARIABLES
!*****
      progname='THALIA'
      vers='28.05.2022'
      task='"Calculation and plot of thermodynamic functions"'
      ierr=0
      CALL initialize('$THALIA-FILES',ierr)
      if(ierr.ne.0) STOP
!*****
      DO I=1,11
        CHIN(I)=' '
      END DO
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
      DO I=1,11
        READ (UNIT=log,FMT='(A500)',END=411) CHIN(I)
      END DO
  411 CLOSE (UNIT=log)
!-----
      CALL LABLA(CHIN(1),I002)
      IF (I002.EQ.0) I002=1
      WRITE (scr,1000)
 1000 FORMAT ( &
      '-------------------'/ &
      'database definition'/ &
      '-------------------')
      CH002='Enter [ "?" | CR | "files" | database filename ] <'// &
      CHIN(1)(1:I002)//'>?'
  412 CONTINUE
      CALL PUST (scr,CH002)
      READ (kbd,FMT='(A500)') CH001
      if (CH001.eq.'?') then
       call helpme('$THA-START')
       goto 412
      end if
      if (VERGL(CH001,'files')) then
       call listfiles
       goto 412
      end if
      IF (CH001.EQ.' ') THEN
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
!------------------
!     open UNIT=plt
!------------------
      j=plt
      line=filename(j)(1:fnl(j))//ext(j)
      path=wpath
      akzess=' '
      state=' '
      call openfile(j,ierr)
      if(ierr.ne.0) STOP
!------------------
!     open UNIT=lpl
!------------------
      j=lpl
      line=filename(j)(1:fnl(j))//ext(j)
      path=wpath
      akzess=' '
      state=' '
      call openfile(j,ierr)
      if(ierr.ne.0) STOP
!-----
      REWIND (UNIT=out)
      REWIND (UNIT=dbs)
      REWIND (UNIT=dat)
      REWIND (UNIT=plt)
      REWIND (UNIT=lpl)
!*****
      COMAY=COMAX
      CALL PROREAD(SYREC)
!-----
!-----READ  PRTCOD, FORMUL AND USE FROM SYREC
!-----SET UP FIRST NUN COLUMNS OF MATRIX
      CALL GELI(SYREC,FF)
      PRTCOD=IDINT(FF)
      DO I=1,10
        PRTLOG(I)=.FALSE.
      END DO
      PRTLOG(3)=.TRUE.
      PRTLOG(4)=.TRUE.
!-----
      CALL TAXI(SYREC,FORMUL)
      CALL TAXI(SYREC,USE)
      DO I=1,NC
        CHEM(I)=1.0D0
      END DO
      CALL LABLA(USE,LUSE)
      CALL DBREAD
      IF (PRTLOG(1)) STOP
      CLOSE(UNIT=dat)
      TEST=DABS(TEST)
!-----START LOOPING
      DO I=1,NPHA
        GG(I)=0.0D0
        GGK(I)=0.0D0
      END DO
!      WRITE (scr,2022) (CHNAME(I),I=1,NUN)
! 2022 FORMAT (' SYSTEM:  ',6A8,100(/10X,6A8))
      CALL GRECAL
!*****
!+++++store terminal input
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
      DO I=1,11
        CALL PUST(log,CHIN(I))
      END DO
      CLOSE (UNIT=log)
!*****
      END
!-----
!****************************** 

      SUBROUTINE GRECAL
      IMPLICIT NONE
      CHARACTER(500) CHIN(11),SYREC
      COMMON /CHINPU/ CHIN
      INCLUDE 'theriak.cmn'
      INCLUDE 'files.cmn'
!-----END OF COMMON VARIABLES
      INTEGER(4) I,II,IINAM(10),I003(0:100),I001, &
      I1,NR,NRPH,ICH,I2,I3,I4,IDIA,I002,LA98,J,ILF
      REAL(8) XMINI,XMAXI,DELTX,YMINI,YMAXI,XCONST,SIZLAB,ANGE, &
      XVAL(0:100),YVAL(0:100),FFF,RCOEFF(10),YSCALE,FOF,FO1,X98,Y98, &
      SIZ98,ANG98,BREIT,HOCH,F3
      CHARACTER(8) SORTSTRING(PHMAX)
      CHARACTER(80) DATI
      CHARACTER(16) XVARI,YVARI,BLANK,RPHASE(10),CHAR1
      CHARACTER(170) LINTEX
      CHARACTER(25) XTXT,YTXT
      CHARACTER(5) FORTX,FORTY
      CHARACTER(15) FORTXY
      CHARACTER(2) CH98,CH97
      CHARACTER(100) FORTXYZ
      COMMON /DECRE/ RCOEFF
      COMMON /DECIN/ NRPH
      COMMON /DECCH/ RPHASE
!-----
      ICH=1
      BACKCODE='   '
      BLANK=' '
      DO I=1,100
        I003(I)=2
      END DO
      I003(0)=3
      IDIA=1
      CH97='97'
      CH98='98'
      X98=0.0D0
      Y98=0.0D0
      SIZ98=0.0D0
      ANG98=0.0D0
      LA98=0
      BREIT=15.0D0
      HOCH=15.0D0
!-----
      WRITE (scr,1000)
 1000 FORMAT (/ &
      '---------------------'/ &
      'X-variable definition'/ &
      '---------------------')
      CALL LABLA(CHIN(2),I002)
      IF (I002.EQ.0) I002=1
      CALL helpme('$THA-XS')
   10 CONTINUE
      WRITE (scr,1002) CHIN(2)(1:I002)
 1002 FORMAT(/,'Enter [ "?" | CR | "X" | X-variable xmin xmax] <', &
      a,'>?')
      READ (UNIT=kbd,FMT='(A500)') SYREC
      if(SYREC.eq.'?') then
         call helpme('$THA-X')
         goto 10
      end if
      IF (SYREC.EQ.' ') THEN
      SYREC=CHIN(2)
      ELSE
      IF (SYREC.NE.'END'.AND.SYREC.NE.'end' &
      .AND.SYREC.NE.'B'.AND.SYREC.NE.'b') THEN
      CHIN(2)=SYREC
      END IF
      END IF
      REC=SYREC
      CALL TAXI(REC,XVARI)
!-----
      CALL LOWUP(XVARI)
      IF (XVARI.NE.'TC'.AND.XVARI.NE.'TK'.AND.XVARI.NE.'P' &
      .AND.XVARI.NE.'1000/T'.AND.XVARI.NE.'X') THEN
      CALL SHOUTI
      WRITE (scr,1004)
 1004 FORMAT (//' X-variable not recognized by THALIA')
      GOTO 10
      END IF
!-----
      IF (XVARI.EQ.'X') THEN
      CALL GXDIA
      RETURN
      END IF
!-----
      CALL GELI(REC,XMINI)
      CALL GELI(REC,XMAXI)
      IF (XMINI.EQ.XMAXI) THEN
      CALL SHOUTI
      WRITE (scr,1006)
 1006 FORMAT (//' XMINI is equal to XMAXI')
      GOTO 10
      END IF
!-----
   11 IF (BACKCODE.EQ.'END') RETURN
      BACKCODE='   '
      WRITE (scr,1008)
 1008 FORMAT ( &
      '---------------------'/ &
      'Y-variable definition'/ &
      '---------------------')
      call helpme('$THA-Y1S')
  111 CONTINUE
      CALL LABLA(CHIN(3),I002)
      IF (I002.EQ.0) I002=1
      WRITE (scr,1010) CHIN(3)(1:I002)
 1010 FORMAT(/'Enter [ "?" | "B" | CR | "end" | Y-var ] <',a,'>?')
      READ (UNIT=kbd,FMT='(A500)') SYREC
      IF (SYREC.eq.'?') then
       call helpme('$THA-Y1L')
       goto 111
      END IF
      IF (SYREC.EQ.' ') THEN
      SYREC=CHIN(3)
      ELSE
      IF (SYREC.NE.'END'.AND.SYREC.NE.'end' &
      .AND.SYREC.NE.'B'.AND.SYREC.NE.'b') THEN
      CHIN(3)=SYREC
      END IF
      END IF
!-----
      CALL TAXI(SYREC,YVARI)
      CALL LOWUP(YVARI)
      IF (YVARI.EQ.'END'.OR.YVARI.EQ.'end' &
      .OR.YVARI.EQ.'B'.OR.YVARI.EQ.'b') RETURN
      YSCALE=1.0D0
      I1=INDEX(YVARI,'/')
      IF (I1.NE.0) THEN
      CHAR1=YVARI(I1+1:)
      READ (UNIT=CHAR1,FMT='(BN,D16.0)',ERR=999) YSCALE
      CHAR1=YVARI(1:I1-1)
      YVARI=CHAR1
      END IF
      GOTO 12
  999 CALL SHOUTI
      WRITE (scr,1012) YVARI
      WRITE (out,1012) YVARI
 1012 FORMAT (//' Troubles with reading the Y-variable: ',A16)
      WRITE (scr,1014) CHAR1
      WRITE (out,1014) CHAR1
 1014 FORMAT (' The following string cannot be converted', &
      ' to a real number: ',A16)
      GOTO 11
!-----
   12 IF (BACKCODE.EQ.'END') RETURN
      BACKCODE='   '
!-----
      WRITE (scr,1016)
 1016 FORMAT (/ &
      '------------------'/ &
      'Formula definition'/ &
      '------------------')
      CALL LABLA(CHIN(4),I002)
      IF (I002.EQ.0) I002=1
      WRITE (scr,1020) CHIN(4)(1:I002)
 1020 format(/, &
      'Enter [ "?" | "B" | CR | "end" | "list" | formula ] <',a,'>?')
      READ (UNIT=kbd,FMT='(A500)') SYREC
      IF (SYREC.EQ.' ') THEN
      SYREC=CHIN(4)
      else if (SYREC.eq.'?') then
         call helpme('$THA-FORMU')
         goto 12
      else IF (VERGL(SYREC,'list')) THEN
      DO I=NUN+1,NPHA
        SORTSTRING(I-NUN)=ABK(I)
      END DO
      I001=NPHA-NUN
      CALL SORTIER(SORTSTRING,I001)
!C      WRITE (6,1018) I001
!C 1018 FORMAT ('List of phases in database: (',I4,')')
      WRITE (UNIT=scr,FMT='(12(A8,2X))') (SORTSTRING(I),I=1,I001)
      GOTO 12
      ELSE
      IF (SYREC.NE.'END'.AND.SYREC.NE.'end' &
      .AND.SYREC.NE.'B'.AND.SYREC.NE.'b') THEN
      CHIN(4)=SYREC
      END IF
      END IF
!-----
      FORMUL=SYREC(1:170)
      IF (FORMUL.EQ.'END'.OR.FORMUL.EQ.'end' &
      .OR.FORMUL.EQ.'B'.OR.FORMUL.EQ.'b') THEN
      IF (FORMUL.EQ.'END'.OR.FORMUL.EQ.'end') THEN
      BACKCODE='END'
      ELSE
      BACKCODE='B  '
      END IF
      GOTO 11
      END IF
!-----
      CALL DECODE(FORMUL)
      IF (NRPH.EQ.0) GOTO 12
      DO 510,NR=1,NRPH
      NAM=RPHASE(NR)
      IINAM(NR)=0
      DO 512,I=1,NPHA
      IF (NAME(I).EQ.NAM.OR.ABK(I).EQ.NAM(1:8)) THEN
      IINAM(NR)=I
      END IF
  512 CONTINUE
      IF (IINAM(NR).EQ.0) THEN
      CALL SHOUTI
      WRITE (scr,FMT='(1X,A16,'': phase not known'')') NAM
      GOTO 12
      END IF
      I1=IINAM(NR)
      CALL DAREST(I1)
      IF (COM) THEN
      CALL SHOUTW
      WRITE (UNIT=scr,FMT=1021) NAM
 1021 FORMAT (1X,A16,' is a combination of others'/ &
      19X,'information of "COM" line will be skipped')
      END IF
  510 CONTINUE
!=====
   13 IF (XVARI.EQ.'TC'.OR.XVARI.EQ.'TK'.OR.XVARI.EQ.'1000/T') THEN
!      write(UNIT=scr,FMT='(64(''-''))')
!      write(UNIT=out,FMT='(64(''-''))')
      CALL LABLA(CHIN(5),I002)
      IF (I002.EQ.0) I002=1
      WRITE (UNIT=scr,FMT=1022) 'P', CHIN(5)(1:I002)
      CH=', P ='
      ICH=6
      END IF
!-----
      IF (XVARI.EQ.'P') THEN
!      write(UNIT=scr,FMT='(64(''-''))')
!      write(UNIT=out,FMT='(64(''-''))')
      CALL LABLA(CHIN(5),I002)
      WRITE (UNIT=scr,FMT=1022) 'TC', CHIN(5)(1:I002)
 1022 FORMAT(/'Enter [ CR | "B" | "end" | ',a,' ] <',a,'>?')
      CH=', T[C] ='
      ICH=9
      END IF
      READ (UNIT=kbd,FMT='(A500)') SYREC
      IF (SYREC.EQ.' ') THEN
      SYREC=CHIN(5)
      ELSE
      IF (SYREC.NE.'END'.AND.SYREC.NE.'end' &
      .AND.SYREC.NE.'B'.AND.SYREC.NE.'b') THEN
      CHIN(5)=SYREC
      END IF
      END IF
!-----
      LINTEX=SYREC(1:170)
      IF (LINTEX.EQ.'END'.OR.LINTEX.EQ.'end' &
      .OR.LINTEX.EQ.'B'.OR.LINTEX.EQ.'b') THEN
      IF (LINTEX.EQ.'END'.OR.LINTEX.EQ.'end') THEN
      BACKCODE='END'
      ELSE
      BACKCODE='B  '
      END IF
      GOTO 12
      END IF
!-----
      CALL GELI(LINTEX,XCONST)
      CALL LABLA(FORMUL,I1)
      I1=I1+1
      WRITE (UNIT=USE,FMT='(F16.7)') XCONST
      CALL FIBLA(USE,I2)
      I4=0
      DO 403,I=25,1,-1
      IF (USE(I:I).NE.' '.AND.USE(I:I).NE.'0') THEN
      I4=I
      GOTO 404
      END IF
  403 CONTINUE
  404 CONTINUE
      IF (I4.LT.I2) I4=I2
      IF (USE(I4:I4).EQ.'.') I4=I4-1
      CALL LABLA(YVARI,I3)
      I3=I3+1
      LINTEX=YVARI(1:I3)//FORMUL(1:I1)//CH(1:ICH)//USE(I2:I4)
!-----
      DO I=0,100
        YVAL(I)=0.0D0
      END DO
!-----
!     RPHASE loop
!
      DO 900,NR=1,NRPH
      DO II=1,NUN
        GG(II)=0.0D0
        GGK(II)=0.0D0
      END DO
      I1=IINAM(NR)
      CALL DAREST(I1)
      DELTX=(XMAXI-XMINI)/100.0D0
!-----
!     XMINI to XMAXI loop
!
      DO 910,I=0,100
      XVAL(I)=XMINI+DBLE(I)*DELTX
      IF (XVARI.EQ.'TC') THEN
      TC=XVAL(I)
      P=XCONST
      END IF
      IF (XVARI.EQ.'TK') THEN
      TC=XVAL(I)-273.15D0
      P=XCONST
      END IF
      IF (XVARI.EQ.'1000/T') THEN
      T=1000.0D0/XVAL(I)
      TC=T-273.15D0
      END IF
      IF (XVARI.EQ.'P') THEN
      P=XVAL(I)
      TC=XCONST
      END IF
      GR=0.0D0
      HR=0.0D0
      SR=0.0D0
      CPR=0.0D0
      VOLUM=0.0D0
      T=TC+273.15D0
      PGAS=P*PRAT
      RT=R*T
      TT=T*T
      SQT=DSQRT(T)
      IF (SPC) THEN
      CALL GSPEC(NAM,P,PGAS,T,FALL,GR,VOLUM)
      ELSE
      LIQ=.FALSE.
      CALL GCALC(I1)
      END IF
      CALL PSAT2(T,F3)
      FFF=0.0D0
      IF (YVARI.EQ.'PSAT') FFF=F3
      IF (YVARI.EQ.'CP') FFF=CPR
      IF (YVARI.EQ.'H') FFF=HR
      IF (YVARI.EQ.'S') FFF=SR
      IF (YVARI.EQ.'G') FFF=GR
      IF (YVARI.EQ.'LNK') FFF=-GR/RT
      IF (YVARI.EQ.'LOGK') FFF=-GR/(RT*2.302585093D0)
      IF (YVARI.EQ.'K') FFF=DEXP(-GR/RT)
      IF (YVARI.EQ.'V') FFF=VOLUM
      IF (YVARI.EQ.'CP-CP0') FFF=CPR-FCP0
      IF (YVARI.EQ.'H-H0') FFF=HR-H0R
      IF (YVARI.EQ.'S-S0') FFF=SR-S0R
      IF (YVARI.EQ.'G-G0') FFF=GR-FG0
      IF (YVARI.EQ.'V-V0') FFF=VOLUM-V0R
      IF (YVARI.EQ.'CP.CP') FFF=FCPCP
      IF (YVARI.EQ.'CP.DIS') FFF=FCPDIS
      IF (YVARI.EQ.'CP.TR') FFF=FCPTR
      IF (YVARI.EQ.'H.CP') FFF=FHCP
      IF (YVARI.EQ.'H.DIS') FFF=FHDIS
      IF (YVARI.EQ.'H.TR') FFF=FHTR
      IF (YVARI.EQ.'S.CP') FFF=FSCP
      IF (YVARI.EQ.'S.DIS') FFF=FSDIS
      IF (YVARI.EQ.'S.TR') FFF=FSTR
      IF (YVARI.EQ.'G.CP') FFF=FGCP
      IF (YVARI.EQ.'G.VOL') FFF=FGVOL
      IF (YVARI.EQ.'G.GAS') FFF=FGGAS
      IF (YVARI.EQ.'G.DIS') FFF=FGDIS
      IF (YVARI.EQ.'G.TR') FFF=FGTR
      IF (YVARI.EQ.'V.VOL') FFF=FVVOL
      IF (YVARI.EQ.'V.GAS') FFF=FVGAS
      IF (YVARI.EQ.'V.DIS') FFF=FVDIS
      IF (YVARI.EQ.'V.TR') FFF=FVTR
      IF (YVARI.EQ.'1') FFF=FSPEC(1)
      IF (YVARI.EQ.'2') FFF=FSPEC(2)
      IF (YVARI.EQ.'3') FFF=FSPEC(3)
      IF (YVARI.EQ.'4') FFF=FSPEC(4)
      IF (YVARI.EQ.'5') FFF=FSPEC(5)
      YVAL(I)=YVAL(I)+FFF*RCOEFF(NR)/YSCALE
  910 CONTINUE
  900 CONTINUE
!-----
      YMINI=1.0D20
      YMAXI=-1.0D20
      DO 920,I=0,100
        IF (YVAL(I).GT.YMAXI) YMAXI=YVAL(I)
        IF (YVAL(I).LT.YMINI) YMINI=YVAL(I)
  920 CONTINUE
      IF (YMAXI-YMINI.LT.1D-8) THEN
      CALL SHOUTW
      WRITE (UNIT=scr,FMT=2005) YMINI
 2005 FORMAT (/' The minimum and the maximum are both = ',1PE10.3)
      YMAXI=YMAXI+DABS(YMAXI*0.05D0+1.0D0)
      YMINI=YMINI-DABS(YMINI*0.05D0+1.0D0)
      END IF
!-----
      IF (IDIA.EQ.1) THEN
      XTXT='undefined'
      YTXT='undefined'
      IF (XVARI.EQ.'TC') XTXT='Temperature [C]'
      IF (XVARI.EQ.'TK') XTXT='Temperature [K]'
      IF (XVARI.EQ.'1000/T') XTXT='1000/T [1/K]'
      IF (XVARI.EQ.'P') XTXT='Pressure [Bars]'
      IF (YVARI.EQ.'CP') YTXT='Cp [J/mol/K]'
      IF (YVARI.EQ.'H') YTXT='H [J/mol]'
      IF (YVARI.EQ.'S') YTXT='S [J/mol/K]'
      IF (YVARI.EQ.'G') YTXT='G [J/mol]'
      IF (YVARI.EQ.'LNK') YTXT='-G/RT = ln(k)'
      IF (YVARI.EQ.'LOGK') YTXT='-G/RT/ln10 = log(k)'
      IF (YVARI.EQ.'K') YTXT='EXP(-G/RT) = k'
      IF (YVARI.EQ.'V') YTXT='Vol [J/Bar]'
      IF (YVARI.EQ.'CP-CP0') YTXT='Cp-Cp0 [J/mol/K]'
      IF (YVARI.EQ.'H-H0') YTXT='H-H0 [J/mol]'
      IF (YVARI.EQ.'S-S0') YTXT='S-S0 [J/mol/K]'
      IF (YVARI.EQ.'G-G0') YTXT='G-G0 [J/mol]'
      IF (YVARI.EQ.'V-V0') YTXT='V-V0 [J/Bar]'
      IF (YVARI.EQ.'CP.CP') YTXT='Cp(Cp) [J/mol/K]'
      IF (YVARI.EQ.'CP.DIS') YTXT='Cp(dis) [J/mol/K]'
      IF (YVARI.EQ.'CP.TR') YTXT='Cp(tr) [J/mol/K]'
      IF (YVARI.EQ.'H.CP') YTXT='H(Cp) [J/mol]'
      IF (YVARI.EQ.'H.DIS') YTXT='H(dis) [J/mol]'
      IF (YVARI.EQ.'H.TR') YTXT='H(tr) [J/mol]'
      IF (YVARI.EQ.'S.CP') YTXT='S(Cp) [J/mol/K]'
      IF (YVARI.EQ.'S.DIS') YTXT='S(dis) [J/mol/K]'
      IF (YVARI.EQ.'S.TR') YTXT='S(tr) [J/mol/K]'
      IF (YVARI.EQ.'G.CP') YTXT='G(Cp) [J/mol]'
      IF (YVARI.EQ.'G.VOL') YTXT='G(vol) [J/mol]'
      IF (YVARI.EQ.'G.GAS') YTXT='G(gas) [J/mol]'
      IF (YVARI.EQ.'G.DIS') YTXT='G(dis) [J/mol]'
      IF (YVARI.EQ.'G.TR') YTXT='G(tr) [J/mol]'
      IF (YVARI.EQ.'V.VOL') YTXT='V(vol) [J/Bar]'
      IF (YVARI.EQ.'V.GAS') YTXT='V(gas) [J/Bar]'
      IF (YVARI.EQ.'V.DIS') YTXT='V(dis) [J/Bar]'
      IF (YVARI.EQ.'V.TR') YTXT='V(tr) [J/Bar]'
      IF (YVARI.EQ.'1') YTXT=CHSPEC(1)
      IF (YVARI.EQ.'2') YTXT=CHSPEC(2)
      IF (YVARI.EQ.'3') YTXT=CHSPEC(3)
      IF (YVARI.EQ.'4') YTXT=CHSPEC(4)
      IF (YVARI.EQ.'5') YTXT=CHSPEC(5)
!----
!dC      FOF=0.005D0*DABS(XMAXI-XMINI)/20.0D0
      FOF=0.005D0*DABS(XMAXI-XMINI)/BREIT
      FO1=-DLOG10(FOF)+0.5D0
      I1=IDNINT(FO1)+1
      IF (I1.LT.1) I1=1
      IF (I1.GT.7) I1=7
      WRITE (UNIT=FORTX,FMT='(''F10.'',I1)') I1
!dC      FOF=0.005D0*DABS(YMAXI-YMINI)/15.0D0
      FOF=0.005D0*DABS(YMAXI-YMINI)/HOCH
      FO1=-DLOG10(FOF)+0.5D0
      I1=IDNINT(FO1)+1
      IF (I1.LT.1) I1=1
      IF (I1.GT.7) I1=7
      WRITE (UNIT=FORTY,FMT='(''F10.'',I1)') I1
      FORTXY=FORTX//','//FORTY
!-----
      WRITE (plt,FMT='(A25/A25)') XTXT,YTXT
      WRITE (lpl,FMT='(A25/A25)') 'L:'//XTXT,YTXT
      FORTXYZ='(2'//FORTX//',2'//FORTY//',2F10.2'//')'
      CALL LABLA(FORTXYZ,ILF)
      WRITE (plt,FMT=FORTXYZ(1:ILF)) XMINI,XMAXI,YMINI,YMAXI, &
      BREIT,HOCH
      WRITE (lpl,2010) XMINI,XMAXI,YMINI,YMAXI,BREIT,HOCH
 2010 FORMAT(4(1PE20.12),0P,2F10.2)
      END IF
!-----
      IF (IDIA.EQ.1) THEN
      WRITE (plt,FMT='(''    2    0    0    0    0    0'')')
      WRITE (lpl,FMT='(''    2    0    0    0    0    0'')')
      FORTXYZ='('//FORTXY//',0PF10.7,F10.4,I5,A2)'
      CALL LABLA(FORTXYZ,ILF)
      WRITE (plt,FMT=FORTXYZ(1:ILF)) X98,Y98,SIZ98,ANG98,LA98,CH97
      WRITE (lpl,2024) X98,Y98,SIZ98,ANG98,LA98,CH97
      WRITE (plt,FMT=FORTXYZ(1:ILF)) X98,Y98,SIZ98,ANG98,LA98,CH98
      WRITE (lpl,2024) X98,Y98,SIZ98,ANG98,LA98,CH98
 2024 FORMAT (2(1PE20.12),0PF10.7,F10.4,I5,A2)
      DATI=sdate
      CALL LABLA(DATI,j)
      END IF
!-----
      WRITE (plt,FMT='(''    2  101    0    0    0    0'')')
      WRITE (lpl,FMT='(''    2  101    0    0    0    0'')')
      FORTXYZ='(7('//FORTXY//',I2))'
      CALL LABLA(FORTXYZ,ILF)
      WRITE (plt,FMT=FORTXYZ(1:ILF)) (XVAL(I),YVAL(I),3-MIN0(1,I), &
      I=0,100)
      WRITE (lpl,2020) (XVAL(I),YVAL(I),3-MIN0(1,I),I=0,100)
 2020 FORMAT (7(2(1PE20.12),I2))
!-----
      SIZLAB=0.35D0
      ANGE=0.0D0
      I1=0
!-----
      FORTXYZ='('//FORTXY//',0PF10.7,F10.4,I5,A170)'
      CALL LABLA(FORTXYZ,ILF)
      WRITE (plt,FMT=FORTXYZ(1:ILF)) XVAL(50),YMAXI,SIZLAB,ANGE, &
      I1,BLANK
      WRITE (plt,FMT=FORTXYZ(1:ILF)) XVAL(50),YMINI,SIZLAB,ANGE,I1, &
      LINTEX
      WRITE (lpl,2025) XVAL(50),YMINI,SIZLAB,ANGE,I1,BLANK
      WRITE (lpl,2025) XVAL(50),YMAXI,SIZLAB,ANGE,I1,LINTEX
 2025 FORMAT (2(1PE20.12),0PF10.7,F10.4,I5,A170)
!-----
      CALL LABLA(YTXT,I1)
      WRITE(scr,2026) IDIA,YTXT(1:I1)
 2026 FORMAT(/,'Plot Nr.',I3,': ',20('-'),'>', &
      1x,a,'-diagram plotted')
      IDIA=IDIA+1
      GOTO 13
!-----
      END
!-----
!******************************
      SUBROUTINE GXDIA
      IMPLICIT NONE
      CHARACTER(500) CHIN(11),SYREC
      COMMON /CHINPU/ CHIN
      INCLUDE 'theriak.cmn'
      INCLUDE 'files.cmn'
!-----END OF COMMON VARIABLES
      INTEGER(4) I,II,IE,IS,IL,IN,I003(0:100),I1,NRPH,IIY, &
      IDIA,I2,I3,I002,LA98,ICH,ILF
      REAL(8) MUE(EMAX)
      REAL(8) XMINI,XMAXI,YMINI,YMAXI,SIZLAB,ANGE, &
      XVAL(0:100),YVAL(0:100),RCOEFF(10),FF1,FF2,FOF,FO1, &
      XX1(EMAX),XX2(EMAX),XXX(EMAX),AAA(EMAX),XM,XI,RTA, &
      GMECH,GCONF,GEX,BREIT,HOCH,FF,F1,F2,F3,F4, &
      EXPO,XACT(EMAX),GREAL,X98,Y98,SIZ98,ANG98,GFROMG,GREX
      CHARACTER(16) YVARI,BLANK,RPHASE(10)
      CHARACTER(25) XTXT,YTXT
      CHARACTER(250) PSBIN
      CHARACTER(5) FORTX,FORTY
      CHARACTER(8) CH8
      CHARACTER(15) FORTXY
      CHARACTER(100) FORTXYZ
      CHARACTER(2) CH98,CH2,CH97
      COMMON /DECRE/ RCOEFF
      COMMON /DECIN/ NRPH
      COMMON /DECCH/ RPHASE
!-----
      BLANK=' '
      DO I=1,100
        I003(I)=2
      END DO
      I003(0)=3
!-----
      IDIA=1
      CH97='97'
      CH98='98'
      X98=0.0D0
      Y98=0.0D0
      SIZ98=0.0D0
      ANG98=0.0D0
      LA98=0
      XMINI=0.0D0
      XMAXI=1.0D0
      BREIT=15.0D0
      HOCH=15.0D0
   10 IF (BACKCODE.EQ.'END') RETURN
      BACKCODE='   '
!-----
      WRITE (scr,1000)
 1000 FORMAT (/ &
      '-------------------'/ &
      'Solution definition'/ &
      '-------------------')
  111 CONTINUE
      WRITE (scr,1010)
 1010 FORMAT (/' The solution phases are:',/)
      WRITE (scr,1012) (SOLNAM(I),I=1,NSOL)
 1012 FORMAT (5(3X,A16))
!-----
      CALL LABLA(CHIN(7),I002)
      IF (I002.EQ.0) I002=1
      WRITE (scr,1014) CHIN(7)(1:I002)
 1014 FORMAT (/'Enter [ "?" | CR | "B" | "end" | solution-name ] <', &
      a,'>?')
      READ (UNIT=kbd,FMT='(A500)') SYREC
      IF (SYREC.EQ.'?') THEN
      call helpme('$THA-SOL')
      GOTO 111
      END IF
      IF (SYREC.EQ.' ') THEN
      SYREC=CHIN(7)
      ELSE
      IF (SYREC.NE.'END'.AND.SYREC.NE.'end' &
      .AND.SYREC.NE.'B'.AND.SYREC.NE.'b') THEN
      CHIN(7)=SYREC
      END IF
      END IF
!-----
      NAM=SYREC(1:16)
      IF (NAM.EQ.'END'.OR.NAM.EQ.'end' &
      .OR.NAM.EQ.'B'.OR.NAM.EQ.'b') THEN
      IF (NAM.EQ.'END'.OR.NAM.EQ.'end') THEN
      BACKCODE='END'
      ELSE
      BACKCODE='B  '
      END IF
      RETURN
      END IF
!-----
      IS=0
      DO 510,I=1,NSOL
      IF (SOLNAM(I).EQ.NAM) THEN
      IS=I
      END IF
  510 CONTINUE
      IF (IS.EQ.0) THEN
      CALL SHOUTI
      WRITE (scr,1016) NAM
 1016 FORMAT (/1X,A16,'unknown')
      GOTO 10
      END IF
      DO 512,IE=1,NEND(IS)
      I1=EM(IS,IE)
      CALL DAREST(I1)
      IF (COM) THEN
      CALL SHOUTW
      WRITE (UNIT=scr,FMT=1018) NAM
 1018 FORMAT (1X,A16,': phase is a combination of others'/ &
      19X,'information of "COM" line will be skipped'/ &
      19X,'(has an effect only if Y-variable = G)')
      END IF
  512 CONTINUE
!----
   11 IF (BACKCODE.EQ.'END') RETURN
      BACKCODE='   '
      CALL LABLA(CHIN(8),I002)
      IF (I002.EQ.0) I002=1
!-----
      WRITE (scr,1020)
 1020 FORMAT (/ &
      '----------------------'/ &
      'Composition definition'/ &
      '----------------------')
  112 CONTINUE
      WRITE (scr,1022) SOLNAM(IS)
 1022 FORMAT (/ &
      'Define "binary endmembers" as linear combination', &
      ' of the endmembers of ',A/)
      WRITE (scr,1024) (ABK(EM(IS,IE)),IE=1,NEND(IS))
 1024 FORMAT (7A10)
      WRITE (scr,1026) 1, CHIN(8)(1:I002)
 1026 FORMAT (/'Enter [ "?" | CR | "B" | "end" |  binary endmember', &
      i1,' ] <',a,'>?')
      READ (UNIT=kbd,FMT='(A500)') SYREC
      IF (SYREC.EQ.' ') THEN
      SYREC=CHIN(8)
      else if (SYREC.eq."?") then
         call helpme('$THA-ENDCOMP')
         goto 112
      ELSE
      IF (SYREC.NE.'END'.AND.SYREC.NE.'end' &
      .AND.SYREC.NE.'B'.AND.SYREC.NE.'b') THEN
      CHIN(8)=SYREC
      END IF
      END IF
!-----
      REC=SYREC
      IF (REC.EQ.'END'.OR.REC.EQ.'end' &
      .OR.REC.EQ.'B'.OR.REC.EQ.'b') THEN
      IF (REC.EQ.'END'.OR.REC.EQ.'end') THEN
      BACKCODE='END'
      ELSE
      BACKCODE='B  '
      END IF
      GOTO 10
      END IF
!-----
      DO IE=1,NEND(IS)
        CALL GELI(REC,FF1)
        XX1(IE)=FF1
      END DO
      CALL LABLA(CHIN(9),I002)
      IF (I002.EQ.0) I002=1
      WRITE (scr,1026) 2, CHIN(9)(1:I002)
      READ (UNIT=kbd,FMT='(A500)') SYREC
      IF (SYREC.EQ.' ') THEN
      SYREC=CHIN(9)
      else if (SYREC.eq."?") then
         call helpme('$THA-ENDCOMP')
         goto 112
      ELSE
      IF (SYREC.NE.'END'.AND.SYREC.NE.'end' &
      .AND.SYREC.NE.'B'.AND.SYREC.NE.'b') THEN
      CHIN(9)=SYREC
      END IF
      END IF
!-----
      REC=SYREC
      IF (REC.EQ.'END'.OR.REC.EQ.'end' &
      .OR.REC.EQ.'B'.OR.REC.EQ.'b') THEN
      IF (REC.EQ.'END'.OR.REC.EQ.'end') THEN
      BACKCODE='END'
      ELSE
      BACKCODE='B  '
      END IF
      GOTO 10
      END IF
!-----
      DO IE=1,NEND(IS)
        CALL GELI(REC,FF2)
        XX2(IE)=FF2
      END DO
      FF1=0.0D0
      FF2=0.0D0
      DO I=1,NEND(IS)
!DC        IF (XX1(I).LT.0.0D0) XX1(I)=0.0D0
!DC        IF (XX2(I).LT.0.0D0) XX2(I)=0.0D0
        FF1=FF1+XX1(I)
        FF2=FF2+XX2(I)
      END DO
      IF (FF1.EQ.0.0D0.OR.FF2.EQ.0.0D0) THEN
      CALL SHOUTI
      WRITE (scr,1028)
 1028 FORMAT (' one of the phases is zero')
      GOTO 11
      END IF
      DO I=1,NEND(IS)
        XX1(I)=XX1(I)/FF1
        XX2(I)=XX2(I)/FF2
      END DO
!-----
   12 IF (BACKCODE.EQ.'END') RETURN
      BACKCODE='   '
      WRITE (scr,1030)
 1030 FORMAT ( &
      '---------------------'/ &
      'Y-variable definition'/ &
      '---------------------',/)
      call helpme('$THA-Y2S')
      CALL LABLA(CHIN(10),I002)
      IF (I002.EQ.0) I002=1
  214 CONTINUE
      WRITE (scr,1032) CHIN(10)(1:I002)
 1032 FORMAT ( &
      /'Enter [ "?" | CR | "B" | "end" | Y-variable ] <',a,'>?')
      READ (UNIT=kbd,FMT='(A500)') SYREC
      IF (SYREC.EQ.' ') THEN
      SYREC=CHIN(10)
      else if (SYREC.eq."?") then
         call helpme('$THA-Y2L')
         goto 214
      ELSE
      IF (SYREC.NE.'END'.AND.SYREC.NE.'end' &
      .AND.SYREC.NE.'B'.AND.SYREC.NE.'b') THEN
      CHIN(10)=SYREC
      END IF
      END IF
!-----
      REC=SYREC
      IF (REC.EQ.'END'.OR.REC.EQ.'end' &
      .OR.REC.EQ.'B'.OR.REC.EQ.'b') THEN
      IF (REC.EQ.'END'.OR.REC.EQ.'end') THEN
      BACKCODE='END'
      ELSE
      BACKCODE='B  '
      END IF
      GOTO 11
      END IF
!-----
      CALL TAXI(REC,YVARI)
      CALL GELI(REC,FF1)
      IIY=IDINT(FF1)
      CALL LOWUP(YVARI)
      IF (YVARI.NE.'G'.AND.YVARI.NE.'GCON'.AND.YVARI.NE.'GEX' &
      .AND.YVARI.NE.'GMIX'.AND.YVARI.NE.'MUE'.AND.YVARI.NE.'ACT' &
      .AND.YVARI.NE.'GR'.AND.YVARI.NE.'GG'.AND.YVARI.NE.'REX') THEN
      CALL SHOUTI
      WRITE (UNIT=scr,FMT=1034) YVARI
 1034 FORMAT (/' Y-variable not recognized: ',A16)
      IF (YVARI(1:3).EQ.'MUE'.OR.YVARI(1:3).EQ.'ACT') THEN
      WRITE (UNIT=scr,FMT=1036)
 1036 FORMAT ('"MUE" and "ACT" must be followed by two blanks')
      END IF
      GOTO 12
      END IF
      IF (YVARI.EQ.'GEX'.AND.NMARG(IS).EQ.0.AND.NSMARG(IS).EQ.0) THEN
      CALL SHOUTW
      WRITE (UNIT=scr,FMT=1038) SOLNAM(IS)
 1038 FORMAT (/1X,A16,' has no excess function')
      GOTO 12
      END IF
!-----
   13 CALL LABLA(CHIN(11),I002)
      IF (I002.EQ.0) I002=1
      CONTINUE
      WRITE (scr,1040) CHIN(11)(1:I002)
 1040 FORMAT ( &
      //'Enter [ "?" | CR | "B" | "end" | temperature  pressure ] <', &
      a,'>?')
      READ (UNIT=kbd,FMT='(A500)') SYREC
      IF (SYREC.EQ.' ') THEN
      SYREC=CHIN(11)
      else if (SYREC.eq."?") then
         call helpme('$THA-PT')
         goto 13
      ELSE
      IF (SYREC.NE.'END'.AND.SYREC.NE.'end' &
      .AND.SYREC.NE.'B'.AND.SYREC.NE.'b') THEN
      CHIN(11)=SYREC
      END IF
      END IF
!-----
      REC=SYREC
      IF (REC.EQ.'END'.OR.REC.EQ.'end' &
      .OR.REC.EQ.'B'.OR.REC.EQ.'b') THEN
      IF (REC.EQ.'END'.OR.REC.EQ.'end') THEN
      BACKCODE='END'
      ELSE
      BACKCODE='B  '
      END IF
      GOTO 12
      END IF
!-----
      CALL GELI(REC,TC)
      CALL GELI(REC,P)
!=====
      GOTO 599
      T=TC+273.15D0
      IF (T.LE.0.0) THEN
      CALL SHOUTI
      WRITE (UNIT=scr,FMT='('' T ='',F8.2)') T
      GOTO 13
      END IF
      IF (P.LE.0.0) THEN
      CALL SHOUTI
      WRITE (UNIT=scr,FMT='('' P ='',F9.2)') P
      GOTO 13
      END IF
!+++++
      PGAS=P*PRAT
      RT=R*T
      TT=T*T
      SQT=DSQRT(T)
      DO II=1,NUN
        GG(II)=0.0D0
        GGK(II)=0.0D0
      END DO
      DO I=1,NMARG(IS)
        WG(IS,I)=WH(IS,I)+WCP(IS,I)*(T-T0) &
        -(WS(IS,I)+DLOG(T/T0)*WCP(IS,I))*T+WV(IS,I)*P
      END DO
!-----
      DO 600,II=1,NEND(IS)
      I1=EM(IS,II)
      CALL DAREST(I1)
      NAM=NAME(I1)
      IF (SPC) THEN
      CALL GSPEC(NAM,P,PGAS,T,FALL,GR,VOLUM)
      ELSE
      LIQ=.FALSE.
      CALL GCALC(I1)
      END IF
!     IF (COM) THEN
!     DO 605,I=1,NCOM
! 605 GR=GR+FFCOM(I)*GGK(ICOM(I))
!     END IF
      GGK(I1)=GR
      GG(I1)=GR
  600 CONTINUE
!!!!
  599 CONTINUE
      CALL NURVONPT
!!!!
!=====
      GMECH=0.0D0
      GCONF=0.0D0
      GEX=0.0D0
      RTA=RT*ALPHA(IS)
!*****
!     XMINI to XMAXI loop
!
      DO 850,IL=0,100
      XI=DBLE(IL)/100.0D0
      XVAL(IL)=XI
      DO IE=1,NEND(IS)
        XXX(IE)=XX2(IE)*XI+XX1(IE)*(1.0D0-XI)
      END DO
      IF (MODELL(IS).EQ.'I') THEN
      DO I1=1,NEND(IS)
        AAA(I1)=XXX(I1)
      END DO
      ELSE
!=-=-=-
      CALL ACTIVI(IS,XXX,AAA)
      END IF
!=-=-=-
      CALL MUECAL(IS,XXX,MUE)
!=-=-=-
      DO 754,IE=1,NEND(IS)
        EXPO=(MUE(IE)-GGK(EM(IS,IE)))/RTA
        IF (EXPO.LT.-150) THEN
        XACT(IE)=0.0D0
        ELSE
        XACT(IE)=DEXP(EXPO)
        END IF
  754 CONTINUE
!=-=-=-
      GMECH=0.0D0
      GCONF=0.0D0
      GEX=0.0D0
      GREAL=0.0D0
      DO 700,I=1,NEND(IS)
        IF (AAA(I).GT.0.0D0) THEN
        GMECH=GMECH+XXX(I)*GGK(EM(IS,I))
        GCONF=GCONF+RTA*XXX(I)*DLOG(AAA(I))
        GREAL=GREAL+XXX(I)*MUE(I)
        CALL GNONID(IS,XXX,GFROMG)
        END IF
  700 CONTINUE
      DO IN=1,NMARG(IS)
        XM=1.0D0
        DO I=1,POLY(IS,IN)
          XM=XM*XXX(INDX(IS,IN,I))
        END DO
        GEX=GEX+WG(IS,IN)*XM
      END DO
      GREX=GREAL-GMECH-GCONF
      IF (YVARI.EQ.'G') YVAL(IL)=GMECH+GCONF+GEX
      IF (YVARI.EQ.'GR') YVAL(IL)=GREAL
      IF (YVARI.EQ.'GG') YVAL(IL)=GFROMG
      IF (YVARI.EQ.'GCON') YVAL(IL)=GCONF
      IF (YVARI.EQ.'GEX') YVAL(IL)=GEX
      IF (YVARI.EQ.'REX') YVAL(IL)=GREX
      IF (YVARI.EQ.'GMIX') YVAL(IL)=GCONF+GEX
      IF (YVARI.EQ.'MUE') YVAL(IL)=MUE(IIY)
      IF (YVARI.EQ.'ACT') YVAL(IL)=XACT(IIY)
  850 CONTINUE
!*****
      IF (IDIA.EQ.1) THEN
      YMINI=1.0D20
      YMAXI=-1.0D20
      DO 910,I=0,100
        IF (YVAL(I).GT.YMAXI) YMAXI=YVAL(I)
        IF (YVAL(I).LT.YMINI) YMINI=YVAL(I)
  910 CONTINUE
!-----
      IF (YMAXI-YMINI.LT.1D-8) THEN
      CALL SHOUTW
      WRITE (UNIT=scr,FMT=2002) YMINI
 2002 FORMAT (/' The minimum and the maximum are both = ',1PE10.3)
      YMAXI=YMAXI+DABS(YMAXI*0.05D0+1.0D0)
      YMINI=YMINI-DABS(YMINI*0.05D0+1.0D0)
      END IF
!=====
      XWIDE=XMAXI-XMINI
      YHIGH=YMAXI-YMINI
      XPOSB=(XWIDE/BREIT)*1.5D0+XMAXI
!----
      FOF=0.005D0*DABS(XMAXI-XMINI)/BREIT
      FO1=-DLOG10(FOF)+0.5D0
      I1=IDNINT(FO1)+1
      IF (I1.LT.1) I1=1
      IF (I1.GT.7) I1=7
      WRITE (UNIT=FORTX,FMT='(''F10.'',I1)') I1
      FOF=0.005D0*DABS(YMAXI-YMINI)/HOCH
      FO1=-DLOG10(FOF)+0.5D0
      I1=IDNINT(FO1)+1
      IF (I1.LT.1) I1=1
      IF (I1.GT.7) I1=7
      WRITE (UNIT=FORTY,FMT='(''F10.'',I1)') I1
      FORTXY=FORTX//','//FORTY
      XTXT='x'
      YTXT='Y'
      IF (YVARI.EQ.'G') YTXT='G [J/mol]'
      IF (YVARI.EQ.'GR') YTXT='Gr [J/mol]'
      IF (YVARI.EQ.'GG') YTXT='Gg [J/mol]'
      IF (YVARI.EQ.'GCON') YTXT='G(conf) [J/mol]'
      IF (YVARI.EQ.'GEX') YTXT='G(ex) [J/mol]'
      IF (YVARI.EQ.'REX') YTXT='Gr(ex) [J/mol]'
      IF (YVARI.EQ.'GMIX') YTXT='G(mix) [J/mol]'
      IF (YVARI.EQ.'MUE') YTXT='m [J/mol]'
      IF (YVARI.EQ.'ACT') YTXT='a'
      WRITE (plt,FMT='(A25/A25)') XTXT,YTXT
      WRITE (lpl,FMT='(A25/A25)') 'L:'//XTXT,YTXT
!-----
      FORTXYZ='(2'//FORTX//',2'//FORTY//',2F10.2'//')'
      CALL LABLA(FORTXYZ,ILF)
      WRITE (plt,FMT=FORTXYZ(1:ILF)) XMINI,XMAXI,YMINI,YMAXI, &
      BREIT,HOCH
      WRITE (lpl,2010) XMINI,XMAXI,YMINI,YMAXI,BREIT,HOCH
 2010 FORMAT(4(1PE20.12),0P,2F10.2)
      END IF
!*****
      IF (IDIA.EQ.1) THEN
      WRITE (plt,FMT='(''    2    0    0    0    0    0'')')
      WRITE (lpl,FMT='(''    2    0    0    0    0    0'')')
      FORTXYZ='('//FORTXY//',0PF10.7,F10.4,I5,A2)'
      CALL LABLA(FORTXYZ,ILF)
      WRITE (plt,FMT=FORTXYZ(1:ILF)) X98,Y98,SIZ98,ANG98,LA98,CH97
      WRITE (lpl,2012) X98,Y98,SIZ98,ANG98,LA98,CH97
      WRITE (plt,FMT=FORTXYZ(1:ILF)) X98,Y98,SIZ98,ANG98,LA98,CH98
      WRITE (lpl,2012) X98,Y98,SIZ98,ANG98,LA98,CH98
 2012 FORMAT (2(1PE20.12),0PF10.7,F10.4,I5,A2)
      END IF
!*****
      CALL LABLA(SOLNAM(IS),I1)
      WRITE (UNIT=CH2,FMT='(I2.2)') IDIA
      PSBIN='('//CH2//'): '//SOLNAM(IS)(1:I1)//':'
      DO I=1,NEND(IS)
        CH8=ABK(EM(IS,I))
        FF=XX1(I)
        CALL EMANHANG(PSBIN,CH8,FF)
      END DO
      CALL LABLA(PSBIN,I1)
      PSBIN(I1+1:I1+2)=' -'
      DO I=1,NEND(IS)
        CH8=ABK(EM(IS,I))
        FF=XX2(I)
        CALL EMANHANG(PSBIN,CH8,FF)
      END DO
      CALL PUST(scr,PSBIN)
      CALL PUST(out,PSBIN)
!=====
      WRITE (plt,FMT='(''    1    0    0    0    0    0'')')
      WRITE (lpl,FMT='(''    1    0    0    0    0    0'')')
      F1=XPOSB
      F2=YMAXI-(DBLE(IDIA-1)*YHIGH/HOCH)*0.35D0
      F3=0.2D0
      F4=0.0D0
      ICH=0
      CALL LABLA(PSBIN,I)
      FORTXYZ='('//FORTXY//',0PF10.7,F10.4,I5,A)'
      CALL LABLA(FORTXYZ,ILF)
      WRITE (plt,FMT=FORTXYZ(1:ILF)) F1,F2,F3,F4,ICH,PSBIN(1:I)
      WRITE (lpl,2014) F1,F2,F3,F4,ICH,PSBIN(1:I)
 2014 FORMAT (2(1PE20.12),0PF10.7,F10.4,I5,A)
!=====
      WRITE (plt,FMT='(''    2  101    0    0    0    0'')')
      WRITE (lpl,FMT='(''    2  101    0    0    0    0'')')
      FORTXYZ='(7('//FORTXY//',I2))'
      CALL LABLA(FORTXYZ,ILF)
      WRITE (plt,FMT=FORTXYZ(1:ILF)) (XVAL(I),YVAL(I),3-MIN0(1,I),I=0,100)
      WRITE (lpl,2016) (XVAL(I),YVAL(I),3-MIN0(1,I),I=0,100)
 2016 FORMAT (7(2(1PE20.12),I2))
!-----
      SIZLAB=0.35D0
      ANGE=0.0D0
      CALL LABLA(YVARI,I2)
      FORMUL='('//CH2//'): '//YVARI(1:I2+1)//SOLNAM(IS)
!--
      WRITE (UNIT=CH,FMT='(F16.7)') TC
      CALL FIBLA(CH,I2)
      I3=0
      DO 403,I=25,1,-1
      IF (CH(I:I).NE.' '.AND.CH(I:I).NE.'0') THEN
      I3=I
      GOTO 404
      END IF
  403 CONTINUE
  404 CONTINUE
      IF (I3.LT.I2) I3=I2
      IF (CH(I3:I3).EQ.'.') I3=I3-1
      CALL LABLA(FORMUL,I1)
      FORMUL(I1+2:)='T[C]='//CH(I2:I3)
!--
      WRITE (UNIT=CH,FMT='(F16.7)') P
      CALL FIBLA(CH,I2)
      I3=0
      DO 405,I=25,1,-1
      IF (CH(I:I).NE.' '.AND.CH(I:I).NE.'0') THEN
      I3=I
      GOTO 406
      END IF
  405 CONTINUE
  406 CONTINUE
      IF (I3.LT.I2) I3=I2
      IF (CH(I3:I3).EQ.'.') I3=I3-1
      CALL LABLA(FORMUL,I1)
      FORMUL(I1+2:)='P='//CH(I2:I3)
!--
      I1=0
      FORTXYZ='('//FORTXY//',0PF10.7,F10.4,I5,A100)'
      CALL LABLA(FORTXYZ,ILF)
      WRITE (plt,FMT=FORTXYZ(1:ILF)) XVAL(50),YMAXI,SIZLAB,ANGE, &
      I1,BLANK
      WRITE (plt,FMT=FORTXYZ(1:ILF)) XVAL(50),YMINI,SIZLAB,ANGE,I1,FORMUL
      WRITE (lpl,2020) XVAL(50),YMAXI,SIZLAB,ANGE,I1,BLANK
      WRITE (lpl,2020) XVAL(50),YMINI,SIZLAB,ANGE,I1,FORMUL
 2020 FORMAT (2(1PE20.12),0PF10.7,F10.4,I5,A170)
!-----
!      WRITE (UNIT=scr,FMT='(11(''*''))')
      CALL LABLA(YTXT,I1)
      WRITE(scr,2022) IDIA,YTXT(1:I1)
 2022 FORMAT(/,'Plot Nr.',I3,': ',20('-'),'>', &
      1x,a,'-diagram plotted')
      IDIA=IDIA+1
      GOTO 13
!-----
      END
!-----
!******************************
      SUBROUTINE DECODE(FORMUL)
      IMPLICIT NONE
      include 'files.cmn'
!
      CHARACTER(170) FORMUL,FORM,CH001
      REAL(8) RCOEFF(10),FFF
      INTEGER(4) NRPH,I,IT,I1,I2
      CHARACTER(16) RPHASE(10),TEIL(60)
      COMMON /DECRE/ RCOEFF
      COMMON /DECIN/ NRPH
      COMMON /DECCH/ RPHASE
      FORM=FORMUL
      DO I=1,60
        TEIL(I)='+'
      END DO
      DO I=1,10
        RCOEFF(I)=0.0D0
      END DO
      IT=0
   10 CALL FIBLA(FORM,I1)
      IF (I1.EQ.0) GOTO 20
      CH001=FORM(I1:)
      I2=INDEX(CH001,' ')
      IT=IT+1
      IF (IT.GT.60) THEN
      CALL SHOUTI
      WRITE (UNIT=scr,FMT=1000)
 1000 FORMAT (/' The maximum number of strings in a formula', &
      ' is set to 60')
      NRPH=0
      RETURN
      END IF
      TEIL(IT)=CH001(1:I2)
      FORM=CH001(I2+1:)
      GOTO 10
!-----
   20 FFF=1.0D0
      NRPH=1
      IF (TEIL(1).EQ.'+'.OR.TEIL(1).EQ.'-') NRPH=0
      RCOEFF(1)=1.0D0
      DO 500,I=1,IT
!-----
      IF (TEIL(I).EQ.'+'.OR.TEIL(I).EQ.'-') THEN
      NRPH=NRPH+1
      IF (NRPH.GT.10) THEN
      CALL SHOUTI
      WRITE (UNIT=scr,FMT=1010)
 1010 FORMAT (/' The maximum number of phases in a formula', &
      ' is set to 10')
      NRPH=0
      RETURN
      END IF
      IF (TEIL(I).EQ.'+') FFF=1.0D0
      IF (TEIL(I).EQ.'-') FFF=-1.0D0
      RCOEFF(NRPH)=FFF
      GOTO 50
      END IF
!-----
      IF (TEIL(I+1).EQ.'+'.OR.TEIL(I+1).EQ.'-') THEN
      RPHASE(NRPH)=TEIL(I)
      GOTO 50
      END IF
!-----
      READ (UNIT=TEIL(I),FMT='(BN,D16.0)',ERR=999) RCOEFF(NRPH)
      RCOEFF(NRPH)=FFF*RCOEFF(NRPH)
   50 CONTINUE
  500 CONTINUE
!-----
      RETURN
  999 CALL SHOUTF
      CALL LABLA(FORMUL,I2)
      WRITE (scr,1012) (FORMUL(I1:I1),I1=1,I2)
 1012 FORMAT (//' Troubles with reading a coefficient'/ &
      ' Formula: ',170A1)
      WRITE (scr,1014) TEIL(I)
      WRITE (out,1014) TEIL(I)
 1014 FORMAT (' The following string cannot be converted', &
      ' to a real number: ',A16)
      NRPH=0
      RETURN
      END
!-----
!******************************
      SUBROUTINE CALSTR
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      INCLUDE 'files.cmn'
!-----END OF COMMON VARIABLES
      INTEGER(4) N,I
      DO 500,N=1,NPHA
      DO I=1,NUN
        X(N,I)=XX(N,I)
      END DO
      NN(N)=0.0D0
      GG(N)=GGK(N)
      G(N)=GGK(N)
      NUMMER(N)=N
      EMCODE(N)=0
      IF (NULL(N)) THEN
      SUGG(N)=-N
      ELSE
      SUGG(N)=N
      END IF
      DO I=1,EMAX
        XEM(N,I)=0.0D0
      END DO
  500 CONTINUE
      DO N=1,NUN
        SUGG(N)=-N
        NN(N)=BULK(N)
      END DO
      SUGNR=NPHA
      RETURN
      END
!-----
!******************************
      SUBROUTINE EMANHANG(CH001,CH8,FF)
      IMPLICIT NONE
!-----END OF COMMON VARIABLES
      CHARACTER*(*) CH001,CH8
      CHARACTER(8) CH
      REAL(8) FF
      INTEGER(4) I,I1,I2,I3
      IF (FF.EQ.0.0D0) RETURN
      CALL LABLA(CH001,I1)
      CALL LABLA(CH8,I2)
!-----
      IF (FF.EQ.1.0D0) THEN
      CH001(I1+1:)=' '//CH8(1:I2)
      RETURN
      END IF
!-----
      WRITE (UNIT=CH,FMT='(F8.5)') FF
      DO I=8,1,-1
        IF (CH(I:I).NE.'0') GOTO 501
        CH(I:I)=' '
      END DO
  501 CONTINUE
      CALL LABLA(CH,I3)
      IF (CH(I3:I3).EQ.'.') THEN
      CH(I3:I3)=' '
      I3=I3-1
      END IF
!-----
      CH001(I1+1:)=' '//CH8(1:I2)//'('//CH(1:I3)//')'
      RETURN
      END
!-----
