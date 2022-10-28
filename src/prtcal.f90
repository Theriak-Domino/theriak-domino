!-----Version: 28.05.2022
!               ************
!               * prtcal.f *
!               ************
!     Subroutines for printing the result
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
!
      SUBROUTINE PRTCAL
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!
      LOGICAL(4) ISASOLID(COMAX)
      COMMON /LOSOLID/ ISASOLID
!
!-----END OF COMMON VARIABLES
      INTEGER(4) I,II,I001,I002,IS,IL,LL,ILX,IP,J,IPH,I1,IE,IK
      LOGICAL(4) SPACE,AFAIL(EMAX),WHS,WHNS,WHH2O,WHSH2O,WHNSH2O
      CHARACTER(16) TEXT(COMAX)
      CHARACTER(200) CH001,CH002
      CHARACTER(1000) CH250
      CHARACTER(32) CH16,CHTXT,FORMA
      CHARACTER(24) CH12,CH24
      CHARACTER(2) CH2
      REAL(8) SUMM(COMAX),EXPO,ACT,XACT,SUMME,NNPC,AR001(EMAX),F001, &
      TOTAL,F002,F003,F004,VPRO,WPRO,DENS,X16,MUE(EMAX), &
      FEM1,FEM2,MGM1,MGM2,OPXKD,DENS2,ZERO,DXP,DG1,DG2,PVOR
!----- variables on last line are for the opx-kd
!----- 
!----- SECO is used to print solution compositions for ini
!----- is set false in PROREAD. True if prtcod>10
!----- VCHECK is used to check volumes of all phases with dG/dP
!----- is set false in PROREAD.
!----- VNEED: If false volumes not reliable. Is set in PROREAD
!----- should be set by main program, is usually true
!-----
!---- PRTLOG(10): pixelmaps
      IF (PRTLOG(10)) DRU=.FALSE.
!      SECO=.TRUE.
      VCHECK=.FALSE.
!!!      VCHECK=.TRUE.
      J=0
      ZERO=0.0D0
      DXP=P/1.0D5
      FEM1=0.0D0
      FEM2=0.0D0
      MGM1=0.0D0
      MGM2=0.0D0
      OPXKD=0.0D0
      WHS=.FALSE.
      WHNS=.FALSE.
      WHH2O=.FALSE.
      WHSH2O=.FALSE.
      WHNSH2O=.FALSE.
!
      DO II=1,EMAX
        MUE(II)=0.0D0
        AR001(II)=0.0D0
      END DO
!----


!     goto 987


!---- check for stable endmembers
!---- was for some time in 'CLEAN' in gmini
      DO I=1,NUN
      IF (EMCODE(I).EQ.0) THEN
      IF (EMSOL(NUMMER(I)).NE.0) THEN
!      WRITE (UNIT=6,FMT='(''stable endmember '',A16)') NAME(NUMMER(I))
      IS=EMSOL(NUMMER(I))
      DO II=1,NEND(IS)
      XXEM(II)=0.0D0
      END DO
      XXEM(EMNR(NUMMER(I)))=1.0D0
      CALL NEWPH(IS)
!      IF (G(I).NE.G(NMAX)) THEN
!      WRITE (UNIT=6,FMT='(''Gi, Gnmax  '',1PE14.7,2X,1PE14.7)') &
!      G(I),G(NMAX)
!      END IF
      I001=I
      I002=NMAX
      CALL COLCHG(I001,I002)
      NN(I)=NN(NMAX)
      NN(NMAX)=0.0D0
      END IF
      END IF
      END DO
!---- end check for stable endmembers
!----


!  987 continue


!---- ordnen nach Nummern (Solutions first)
      I=1
! GE replaced with GT, aug2009
   10 IF (I.GT.NUN-1) GOTO 1
      IF ((NUMMER(I+1).LT.NUMMER(I)).OR.(NUMMER(I).EQ.0.AND. &
      NUMMER(I+1).EQ.0.AND.EMCODE(I+1).LT.EMCODE(I))) THEN
      I001=I+1
      CALL COLCHG(I,I001)
      I=I-1
      ELSE
      I=I+1
      END IF
      IF (I.EQ.0) I=2
      GOTO 10
    1 CALL MULCHK
      CALL NAMESTPH
!cdcmar08
! GE replaced with GT, aug2009
!---- ordnen Solutions
      I=1
   11 IF (I.GT.NUN2-1) GOTO 2
      IF (EMCODE(I).EQ.EMCODE(I+1).AND. &
      STSOLCOD(I+1).LT.STSOLCOD(I)) THEN
      I001=I+1
      CALL COLCHG(I,I001)
      I002=STSOLCOD(I)
      STSOLCOD(I)=STSOLCOD(I001)
      STSOLCOD(I001)=I002
      CH24=STPHNAM(I)
      STPHNAM(I)=STPHNAM(I001)
      STPHNAM(I001)=CH24
      CH24=SHORTNAM(I)
      SHORTNAM(I)=SHORTNAM(I001)
      SHORTNAM(I001)=CH24
!     STPHCOD ist sowieso gleich
      I=I-1
      ELSE
      I=I+1
      END IF
      IF (I.EQ.0) I=2
      GOTO 11
    2 CONTINUE
!-----
      GGTOT=0.0D0
      DO I=1,NUN
        SUMME=0.0D0
        DO II=1,NUN2
          SUMME=SUMME+NN(II)*X(II,I)
        END DO


!987
!        WRITE (UNIT=scr,FMT='(''NAM,GG '',A,1PE12.5)')  SHORTNAM(I),GG(I)


        GGTOT=GGTOT+SUMME*GG(I)
      END DO
!DC09okt06      GGTOT=-GGTOT
      GGTOT=-GGTOT
      TOTAL=GGTOT
!*****
!*****
!      tests with 5 and 3 points
!      CALL DSDVTEST5
!      CALL DSDVTEST3
!*****
!*****
!---- with prtlog(11) (short table) the following has to be made 
!---- by the main program
!---- PRTLOG(9): print table
      IF (PRTLOG(9)) THEN
      NROWTBL=NROWTBL+1
      IF (NROWTBL.GT.MAXTBL) THEN
      WRITE (UNIT=6,FMT='(''too many rows: calculation stopped'')')
      NROWTBL=NROWTBL-1
!C      CALL PRTTBL
      PRTLOG(9)=.FALSE.
      GOTO 20
!C      STOP
      END IF
!---
      DO I=1,NVARTBL
        OUTTBL(NROWTBL,I)=0.0D0
      END DO
!-----general variables
      CH16=':NR(step)'
      X16=DBLE(NROWTBL)
      CALL SETTBL(CH16,X16)
      CH16=':Temperature'
      X16=TC
      CALL SETTBL(CH16,X16)
      CH16=':Pressure'
      X16=P
      CALL SETTBL(CH16,X16)
      CH16='G_system'
      X16=GGTOT
      CALL SETTBL(CH16,X16)
!
!---- DSDV calculates G_tot,S_tot,V_tot,H_tot,TS_tot,PV-_tot,U_tot
!---- for use with PRTLOG(9) table
      CALL DSDV
!
!-----bulk composition
      DO I=1,NUN
        CH16='blk_'//CHNAME(I)
        X16=BULK(I)
        CALL SETTBL(CH16,X16)
      END DO
      END IF
!*****
!*****
!---- PRTLOG(10): pixelmaps
      IF (PRTLOG(10)) THEN
       NROWTBL=NROWTBL+1
       CH16='G_system'
       X16=GGTOT
       CALL SETMAP(CH16,X16)
!----  DSDV10 calculates G_tot,S_tot,V_tot,H_tot,TS_tot,PV-_tot,U_tot
!----  for use with PRTLOG(10) pixelmaps
       CALL DSDV10
      END IF
!*****
      IF (TEST.LT.0.0D0) THEN
      DO IS=1,NSOL
       DO II=1,NEND(IS)
        IF (ZEND(IS,II).GT.0) THEN
         WRITE (scr,620) SOLNAM(IS),NAME(EM(IS,II)),ZEND(IS,II)
         WRITE (out,620) SOLNAM(IS),NAME(EM(IS,II)),ZEND(IS,II)
  620 FORMAT (1X,A,A,' is 0: ',I10)
        END IF
       END DO
      END DO
      END IF
!*****
   20 CALL VOLCALC
!----- falls nur solids, muss DSDV nach VOLCALC aufgerufen werden (warum ???)
!----- (auch in subroutine RECHG ... IF (ISASOLID(I)) .... (warum ???)
!----  PRTLOG(9): print table
!      IF (PRTLOG(9)) CALL DSDV
!=====
      IF (PRTLOG(6).OR.PRTLOG(7).OR.PRTLOG(8)) THEN
      SPACE=.TRUE.
      DO 502,I=1,NUN2
      IF (NUMMER(I).LE.0) THEN
      TEXT(I)=SOLNAM(EMCODE(I))
      ELSE
      TEXT(I)=NAME(NUMMER(I))
      END IF
      IF (NUMMER(I).LE.NUN.AND.NUMMER(I).NE.0) SPACE=.FALSE.
  502 CONTINUE
!     CALL TRENNE(132)
!10-A-
      IF (DRU) THEN
!-----
!----- determine which headers to print
      DO I=1,NUN2
        IF (ISASOLID(I)) THEN
          WHS=.TRUE.
          IF (H2OM(I).GT.0.0D0) THEN
            WHH2O=.TRUE.
            WHSH2O=.TRUE.
          END IF
        ELSE
          WHNS=.TRUE.
          IF (H2OM(I).GT.0.0D0) THEN
            WHH2O=.TRUE.
            WHNSH2O=.TRUE.
          END IF
        END IF
      END DO
!-----
!----- hier Volumen mit dG/dP, volume check
      IF (VCHECK) THEN
      PVOR=P
      DO I=1,NUN2
        IPH=I
        P=PVOR+DXP
        CALL SORTOFNURVONPT
        CALL RECHG2(IPH,DG1)
        P=PVOR-DXP
        CALL SORTOFNURVONPT
        CALL RECHG2(IPH,DG2)
        VOLPH2(I)=(DG1-DG2)/(2.0D0*DXP)*10.0D0
        VOLM2(I)=VOLPH2(I)/NN(I)
        P=PVOR
        CALL SORTOFNURVONPT
      END DO
      END IF
!-----
!-----bulk composition
!      DO I=1,NUN
!       WRITE (UNIT=6,FMT='(A2,2X,F20.10)') CHNAME(I),BULK(I)
!      END DO
      write (unit=6,fmt='('''')')
      write (unit=out,fmt='('''')')
!
      CALL PUST(6,' working directory = '//wpath)
      CALL PUST(out,' working directory = '//wpath)
!
      CALL PUST(6,' database          = '//DBNAME)
      CALL PUST(out,' database          = '//DBNAME)
!
      CALL BUBU(CH001)
      CALL PUST(6,' bulk              = '//CH001)
      CALL PUST(out,' bulk              = '//CH001)
!-----
      WRITE (*,100)
      WRITE (out,100)
  100 FORMAT (/ &
      ' -----------------------'/ &
      ' equilibrium assemblage:'/ &
      ' -----------------------')
      I001=LOO1-1
      I002=LOO1A
!23456789.123456789.123456789.123456789.123456789.123456789.123456789.12
      WRITE (*,102) P,PGAS,TC,T,NUN2,I001,I002,NMAXMAX,GNOM, &
      BLKSHIFT,SUGNR,NRIDI,TOTMIN,TOTNEW,TOTMINI,GTOT,TOTAL, &
      DISTAMAX,R,GROSSG
      WRITE (out,102) P,PGAS,TC,T,NUN2,I001,I002,NMAXMAX,GNOM, &
      BLKSHIFT,SUGNR,NRIDI,TOTMIN,TOTNEW,TOTMINI,GTOT,TOTAL, &
      DISTAMAX,R,GROSSG
  102 FORMAT (/2X,'P =',F9.2,' bar',7X,'P(Gas) =',F9.2,' bar     T =', &
      F8.2,' C    =',F8.2,' K'//2X,'stable phases: ',I4,4X,'loop =',I4, &
      5X,'loop2 =',I4,5X,'max.phases =   ',I7,5X,'gcalc =',I10,7X, &
      'blkshift =',1PE12.5, &
      /2X,'suggestions: ',I6,4X,'ridiculous phases:  ',I6, &
      5X,'non-ideal min.: ',I6,5X,'newton:  ',I8,7X,'ideal min.:   ',I8, &
      /2X,'G(-)  =',1PE12.5,0P,4X,'G(System) =',F15.2,5X,'stepsize =', &
      1PE12.5,0P,5X,'R =',F14.7,7X,'MaxG(-)  =',1PE12.5)
      IF (.NOT.SPACE) WRITE (UNIT=scr,FMT=104)
      IF (.NOT.SPACE) WRITE (UNIT=out,FMT=104)
  104 FORMAT (/' Composition may be outside space defined by phases')
      IF (I001.GE.LO1MAX) WRITE (UNIT=scr,FMT=106)
      IF (I001.GE.LO1MAX) WRITE (UNIT=out,FMT=106)
  106 FORMAT (/' Equilibrium not reached after maximum number of', &
      ' loops. Compare columns "activity" and "act.(x)" below')
      END IF
!10-B-
      IF (NUN2.LE.0) RETURN
      END IF
      IF (PRTLOG(6)) THEN
!10-A-
      IF (DRU) THEN
      WRITE (UNIT=scr,FMT=108)
      WRITE (UNIT=out,FMT=108)
  108 FORMAT(//9X,'phase',19X,'N',9X,'mol%',35X,'x',14X,'x', &
      9X,'activity',7X,'act.(x)'/ &
      9X,'-----',19X,'-',9X,'----',35X,'-',14X,'-', &
      9X,'--------',7X,'-------')
      END IF
!10-B-
      SUMME=0.0D0
      DO I=1,NUN2
        SUMME=SUMME+DABS(NN(I))
      END DO
!-----
!-510-
      DO 510,I=1,NUN2
      IF (TEST.LT.0) THEN
        WRITE (UNIT=6,FMT='(''      nr.'',I6)') SUGG(I)
      END IF

      NNPC=(NN(I)/SUMME)*100.0D0
      FORMA='F13.6'
      I1=5
      IF (NN(I).GT.100000.0D0) THEN
        FORMA='1PE13.6'
        I1=7
      END IF
      CH002='(1X,I3,I3,2X,A16,1X,'//FORMA(1:I1)//',1X,F11.6)'
      CALL LABLA(CH002,I1)
      WRITE (UNIT=CH001,FMT=CH002(1:I1)) NUMMER(I),EMCODE(I), &
      STPHNAM(I),NN(I),NNPC
!  110 FORMAT (1X,I3,I3,2X,A16,1X,FORMA(1:I1),1X,F11.6)
!+++++
!*****
!---- PRTLOG(9): print table
!---- PRTLOG(11): print short table
      IF (PRTLOG(9).OR.PRTLOG(11)) THEN
!wien09      CH16='n_'//STPHNAM(I)
      CH16='n_'//SHORTNAM(I)
      X16=NN(I)
      CALL SETTBL(CH16,X16)
!      IF (NUMMER(I).NE.0) THEN
!      IF (EMSOL(NUMMER(I)).NE.0) THEN
!      CH16='n_'//SOLNAM(EMSOL(NUMMER(I)))
!      X16=NN(I)
!      CALL SETTBL(CH16,X16)
!      CH16='x_'//TEXT(I)
!      X16=1.0D0
!      CALL SETTBL(CH16,X16)
!      CH16='act_'//TEXT(I)
!      X16=1.0D0
!      CALL SETTBL(CH16,X16)
!      END IF
!      END IF
!
      END IF
!*****
!---- PRTLOG(10): pixelmaps
      IF (PRTLOG(10)) THEN
!wien09      CH16='n_'//STPHNAM(I)
      CH16='n_'//SHORTNAM(I)
      X16=NN(I)
      CALL SETMAP(CH16,X16)
!      IF (NUMMER(I).NE.0) THEN
!      IF (EMSOL(NUMMER(I)).NE.0) THEN
!      CH16='n_'//SOLNAM(EMSOL(NUMMER(I)))
!      X16=NN(I)
!      CALL SETMAP(CH16,X16)
!      CH16='x_'//TEXT(I)
!      X16=1.0D0
!      CALL SETMAP(CH16,X16)
!C      CH16='act_'//TEXT(I)
!C      X16=1.0D0
!C      CALL SETMAP(CH16,X16)
!      END IF
!      END IF
!
      END IF
!*****
      IF (EMCODE(I).GT.0) THEN
      SPACE=.FALSE.
      IS=EMCODE(I)
      DO I001=1,EMAX
        AR001(I001)=XEM(I,I001)
      END DO
      CALL MUECAL(IS,AR001,MUE)
!+++++
!*****
!cdcFeb2011
      IF (SECO) THEN
!      IF (STPHNAM(I)(1:5).EQ.'ClAMP') THEN
!      OPEN (UNIT=60,FILE='seco',STATUS='UNKNOWN',ACCESS='APPEND')
!      WRITE (UNIT=60,FMT='(A16,100(3X,1PE12.5))') &
!      STPHNAM(I),(XEM(I,II),II=1,NEND(IS))
!      CLOSE (UNIT=60)
!      END IF
!=====Jul2011

!     statt ACCESS='APPEND':  POSITION='APPEND'


      CALL LABLA(SOLNAM(IS),I001)
      OPEN (UNIT=60,FILE='seco_'//SOLNAM(IS)(1:I001), &
      STATUS='UNKNOWN',POSITION='APPEND')
!      STATUS='UNKNOWN',ACCESS='APPEND')
      WRITE (UNIT=60,FMT='(F8.2,2X,F10.1,100(2X,A16))') &
      TC,P,(NAME(EM(IS,II)),II=1,NEND(IS))
      WRITE (UNIT=60,FMT='(100(3X,1PE15.8))') &
      (XEM(I,II),II=1,NEND(IS))
      CLOSE (UNIT=60)
!=====
      END IF
!*****
!*****
      DO 505,II=1,NEND(IS)
      AFAIL(II)=.FALSE.
      EXPO=-GG(EM(IS,II))/RT
      IF (EXPO.LT.-150) THEN
        ACT=0.0D0
      ELSE
!test
        IF (EXPO.GT.200) THEN
        WRITE (UNIT=6,FMT='(''prtcal01: expo= '',1PE12.5)') EXPO
        WRITE (UNIT=out,FMT='(''prtcal01: expo= '',1PE12.5)') EXPO
        END IF
!test
        ACT=DEXP(EXPO)
      END IF
      EXPO=(MUE(II)-GG(EM(IS,II)))/RT
        IF (EXPO.LT.-150) THEN
        XACT=0.0D0
      ELSE
!test
        IF (EXPO.GT.200) THEN
        WRITE (UNIT=6,FMT='(''prtcal02: expo= '',1PE12.5)') EXPO
        WRITE (UNIT=out,FMT='(''prtcal02: expo= '',1PE12.5)') EXPO
        END IF
!test
        XACT=DEXP(EXPO)
      END IF
      IF (DABS(ACT-XACT).GT.1D-4) THEN
      CH2='**'
      SPACE=.TRUE.
      AFAIL(II)=.TRUE.
      ELSE
      IF (ZEND(IS,II).EQ.0) THEN
       CH2='  '
      ELSE
       CH2='00'
      END IF
      END IF
      WRITE (UNIT=CH001(61:),FMT=112) NAME(EM(IS,II)),CH2, &
      XEM(I,II),XEM(I,II),ACT,XACT
  112 FORMAT (A16,A2,F9.6,2X,3(2X,1PE12.5))
!*****
      CALL LABLA(ABK(EM(IS,II)),I001)
!wien09      CHTXT=ABK(EM(IS,II))(1:I001)//'_'//STPHNAM(I)
      CHTXT=ABK(EM(IS,II))(1:I001)//'_'//SHORTNAM(I)
!---- PRTLOG(9): print table
!---- PRTLOG(11): print short table
      IF (PRTLOG(9).OR.PRTLOG(11)) THEN
      CH16='n_'//CHTXT
      X16=XEM(I,II)*NN(I)
!---- PRTLOG(9): print table
      IF (PRTLOG(9)) CALL SETTBL(CH16,X16)
      CH16='x_'//CHTXT
      X16=XEM(I,II)
      CALL SETTBL(CH16,X16)
      CH16='a_'//CHTXT
      X16=ACT
      CALL SETTBL(CH16,X16)
      END IF
!*****
!---- PRTLOG(10): pixelmaps
      IF (PRTLOG(10)) THEN
      CALL LABLA(ABK(EM(IS,II)),I001)
!wien09      CHTXT=ABK(EM(IS,II))(1:I001)//'_'//STPHNAM(I)
      CHTXT=ABK(EM(IS,II))(1:I001)//'_'//SHORTNAM(I)
!      CH16='nSOL_'//CHTXT
!      X16=XEM(I,II)*NN(I)
!      CALL SETMAP(CH16,X16)
      CH16='x_'//CHTXT
      X16=XEM(I,II)
      CALL SETMAP(CH16,X16)
!      CH16='actSOL_'//CHTXT
!      X16=ACT
!      CALL SETMAP(CH16,X16)
      END IF
!*****
!10-A-
      IF (DRU) THEN
      IF (CH001(1:4).EQ.'    ') THEN
      WRITE (UNIT=scr,FMT='(A133)') CH001
      WRITE (UNIT=out,FMT='(A133)') CH001
      ELSE
!dC
      WRITE (UNIT=scr,FMT='(A133)') CH001
      WRITE (UNIT=out,FMT='(A133)') CH001
      END IF
      END IF
!10-B-
      CH001=' '
  505 CONTINUE
!+++++
      IF (SPACE.AND.(DRU)) THEN
      WRITE (UNIT=scr,FMT='('' '')')
      WRITE (UNIT=out,FMT='('' '')')
      DO 506,II=1,NEND(IS)
      IF (AFAIL(II)) THEN
      WRITE (6,114) NAME(EM(IS,II)),MUE(II)
      WRITE (out,114) NAME(EM(IS,II)),MUE(II)
  114 FORMAT (76X,'** activity test: ',A16,': ', &
      ' mue = ',1PE12.5)
      END IF
  506 CONTINUE
      END IF
!+++++
      IF (NSITE(IS).GT.0.AND.CHSITE(IS,1).NE.' ') THEN
!!    das Folgende ist jetzt in VOLCALC  (nein, hier ev, nicht in VOLCALC)
!!    because XELSI is deifferent for coexisdting solutions 
      DO ILX=1,NSIEL(IS)
        XELSI(IS,ILX)=0.0D0
        DO IK=1,NEMQQ(IS,ILX)
          IE=EMQQ(IS,ILX,IK)
          XELSI(IS,ILX)=XELSI(IS,ILX)+EMXX(IS,ILX,IE)*XEM(I,IE)
        END DO
      END DO


!!    das Folgende muesste vielleicht noch hier hin kommen
!      IX1=0
!      DO II=1,NSITE(IS)
!       DO K=1,NELPS(IS,II)
!        IX1=IX1+1
!        XEL(IS,II,K)=XELSI(IS,IX1)
!       END DO
!      END DO
!!
!-----
      CH001=' '
      ILX=0
!---- for opx-kd
      FEM1=0.0D0
      FEM2=0.0D0
      MGM1=0.0D0
      MGM2=0.0D0
      OPXKD=0.0D0
!----
      DO 509,II=1,NSITE(IS)
!10-A-
      IF (DRU) THEN
      WRITE (UNIT=scr,FMT='(A133)') CH001
      WRITE (UNIT=out,FMT='(A133)') CH001
      END IF
!10-B-
      CH001=' '
      DO 508,IL=1,NELPS(IS,II)
      ILX=ILX+1
!      IF (XELSI(IS,ILX).NE.0.0D0.OR.XELSI(IS,ILX).EQ.0) THEN
      CH12=' '
!---- for opx-kd
      IF (SIEL(IS,ILX).EQ.'Fe(M1)') FEM1=XELSI(IS,ILX)
      IF (SIEL(IS,ILX).EQ.'Fe(M2)') FEM2=XELSI(IS,ILX)
      IF (SIEL(IS,ILX).EQ.'Mg(M1)') MGM1=XELSI(IS,ILX)
      IF (SIEL(IS,ILX).EQ.'Mg(M2)') MGM2=XELSI(IS,ILX)
!----
      WRITE (UNIT=CH12,FMT='(''['',A8)') SIEL(IS,ILX)
      CALL LABLA(CH12,LL)
      WRITE (UNIT=CH12(LL+1:),FMT='('']'')')
      CALL LABLA(CH001,LL)
      IF (LL.GT.110) THEN
!10-A-
      IF (DRU) THEN
        WRITE (UNIT=scr,FMT='(A133)') CH001
        WRITE (UNIT=out,FMT='(A133)') CH001
      END IF
!10-B-
      CH001=' '
      LL=0
      END IF
      IF (LL.EQ.0) LL=24
      WRITE (UNIT=CH001(LL+6:),FMT='(A12,''='',F9.6)') &
       CH12,XELSI(IS,ILX)
!*****
!---- PRTLOG(9): print table
      IF (PRTLOG(9)) THEN
      CALL LABLA(CH12,LL)
!wien09      CH16=CH12(1:LL)//'_'//STPHNAM(I)
      CH16='x_'//CH12(2:LL-1)//'_'//SHORTNAM(I)
      X16=XELSI(IS,ILX)
      CALL SETTBL(CH16,X16)
      END IF
!*****
!      END IF
  508 CONTINUE
  509 CONTINUE
!10-A-
!---- last line for site occupancies of solution phases:
      IF (DRU) THEN
      WRITE (UNIT=scr,FMT='(A133)') CH001
      WRITE (UNIT=out,FMT='(A133)') CH001
      END IF
      END IF
!+++
!---- for opx-kd
      IF (MGM2.NE.0.0D0.AND.FEM1.NE.0.0D0.AND. &
      MGM1.NE.0.0D0.AND.FEM2.NE.0.0D0) THEN
      OPXKD=DLOG(FEM2*MGM1/(MGM2*FEM1))
      IF (DRU) THEN
      WRITE (scr,115) OPXKD
      WRITE (out,115) OPXKD
  115 FORMAT(28X, &
      ' lnKD(ord) = ln[(Fe/Mg)M2 / (Fe/Mg)M1]  =',1PE12.5)
      END IF
!*****
!---- PRTLOG(9): print table
      IF (PRTLOG(9)) THEN
      CH16='lnKD(ord)'
      X16=OPXKD
      CALL SETTBL(CH16,X16)
      END IF
      END IF
!*****
!----
!---- for non-solution phases:
      ELSE
      IF (DRU) THEN
      WRITE (UNIT=scr,FMT='(A133)') CH001
      WRITE (UNIT=out,FMT='(A133)') CH001
      END IF
      END IF
!10-B-
      IF (DRU) THEN
      IF (MGFE(I).NE.0.0D0.AND.MGFE(I).NE.1.0D0) THEN
       WRITE (UNIT=scr,FMT='(29X,''Mg/(Fe+Mg)  ='',F9.6)') MGFE(I)
       WRITE (UNIT=out,FMT='(29X,''Mg/(Fe+Mg)  ='',F9.6)') MGFE(I)
      END IF
      IF (SIPFU(I).NE.0) THEN
       WRITE (UNIT=scr,FMT='(29X,''Si(pfu)     ='',F9.6)') SIPFU(I)
       WRITE (UNIT=out,FMT='(29X,''Si(pfu)     ='',F9.6)') SIPFU(I)
      END IF
      IF (ALPFU(I).NE.0) THEN
       WRITE (UNIT=scr,FMT='(29X,''Al(pfu)     ='',F9.6)') ALPFU(I)
       WRITE (UNIT=out,FMT='(29X,''Al(pfu)     ='',F9.6)') ALPFU(I)
      END IF
      WRITE (UNIT=scr,FMT='('' '')')
      WRITE (UNIT=out,FMT='('' '')')
      END IF
!+++++
!+++++
  510 CONTINUE
!
!+++++ check for KD (Fe-Mg)
!      DO I=1,NUN2
!       IF (MGFE(I).NE.0.0D0.AND.MGFE(I).NE.1.0D0) THEN
!        DO J=I+1,NUN2
!         IF (MGFE(J).NE.0.0D0.AND.MGFE(J).NE.1.0D0) THEN
!          F001=(MGFE(J)*(1.0D0-MGFE(I)))/(MGFE(I)*(1.0D0-MGFE(J)))
!!----
!      IF (DRU) THEN
!      CH001=' '
!      CALL LABLA(TEXT(I),I001)
!      CALL LABLA(TEXT(J),I002)
!      WRITE (CH001,156) TEXT(I)(1:I001),TEXT(J)(1:I002)
!  156 FORMAT(' (Fe/Mg): ',A,' - ',A)
!      WRITE (CH001(39:),158) F001,DLOG(F001)
!  158 FORMAT('KD = ',F10.4,5X,'ln(KD) = ',F9.4)
!      CALL PUST(scr,CH001)
!      END IF
!!*****
!!!---- PRTLOG(9): print table
!!      IF (PRTLOG(9)) THEN
!!      CALL LABLA(SHORTNAM(I),I001)
!!      CALL LABLA(SHORTNAM(J),I002)
!!      CH16='LnKD'//SHORTNAM(I)(1:i001)//SHORTNAM(J)(1:I002)
!!      X16=DLOG(F001)
!!      CALL SETTBL(CH16,X16)
!!      END IF
!!*****
!         END IF
!        END DO
!       END IF
!      END DO
!
!+++++ check for excluded phases
      IF (DRU) THEN
      IF (SEXCL.GT.0.OR.PEXCL.GT.0) THEN
       WRITE (UNIT=scr,FMT='(89A1)') ('-',I=1,89)
       WRITE (UNIT=out,FMT='(89A1)') ('-',I=1,89)
       WRITE (scr,160)
       WRITE (out,160)
  160  FORMAT(' Excluded phases',14X,'G[J/mol]',7X,'V[ccm]',30X,'x')
       WRITE (UNIT=scr,FMT='(89A1)') ('-',I=1,89)
       WRITE (UNIT=out,FMT='(89A1)') ('-',I=1,89)
      END IF
!----
!
!      DO I=1,NUN
!       WRITE (UNIT=scr,FMT='(89A1)') ('-',J=1,11)
!       WRITE (UNIT=out,FMT='(89A1)') ('-',J=1,11)
!         DO II=1,NUN
!          WRITE (UNIT=scr,FMT='(4X,I3,1X,1PE12.5)') II,X(I,II)
!          WRITE (UNIT=out,FMT='(4X,I3,1X,1PE12.5)') II,X(I,II)
!         END DO
!      END DO
!
!----- solution phases
      IF (SEXCL.GT.0) THEN
      DO I=1,NMAX
       IF (NUMMER(I).EQ.0) THEN
        IS=EMCODE(I)
        IF (EXSOL(IS).AND.SUGG(I).EQ.MINISUG(IS)) THEN
          WRITE (UNIT=scr,FMT='('' '')')
          WRITE (UNIT=out,FMT='('' '')')
         F001=0.0D0
         DO II=1,NEND(IS)
          F001=F001+XEM(I,II)*VV(EM(IS,II))*10.0D0
         END DO
         CH001=' '
         WRITE (CH001,162) NUMMER(I),EMCODE(I),SOLNAM(IS),G(I),F001
  162    FORMAT(1X,I3,I3,2X,A16,2x,1PE12.5,2x,0PF10.4)
         DO II=1,NEND(IS)
          WRITE (CH001(61:),FMT='(A16,1X,1PE12.5)') &
                 NAME(EM(IS,II)),XEM(I,II)
          CALL PUST(scr,CH001)
          CALL PUST(out,CH001)
          CH001=' '
         END DO
!-----
         IP=I
         CALL MINIREA(IP)
!-----
        END IF
       END IF
      END DO
      END IF
!----- non-solution phases
      IF (PEXCL.GT.0) THEN
      DO I=1,NMAX
       IF (NUMMER(I).NE.0) THEN
        IF (NAME(NUMMER(I))(1:1).EQ.'$') THEN
         WRITE (UNIT=scr,FMT='('' '')')
         WRITE (UNIT=out,FMT='('' '')')
         WRITE (scr,162) NUMMER(I),EMCODE(I), &
                NAME(NUMMER(I)),G(I),VV(I)*10.0D0
         WRITE (out,162) NUMMER(I),EMCODE(I), &
                NAME(NUMMER(I)),G(I),VV(I)*10.0D0
!-----
         IP=I
         CALL MINIREA(IP)
!-----
        END IF
       END IF
      END DO
      END IF
      END IF
!+++++
      END IF
!=====
!=====
!---- PRTLOG(9): print table
      IF (PRTLOG(9)) THEN
      DO I=1,NMAX
       IF (NUMMER(I).EQ.0) THEN
        IS=EMCODE(I)
        IF (EXSOL(IS).AND.SUGG(I).EQ.MINISUG(IS)) THEN
!      WRITE (6,121) SOLNAM(EMCODE(I))
!      WRITE (out,121) SOLNAM(EMCODE(I))
!  121 FORMAT (/,'GGG',1X,A)
         CH16='G_'//SOLNAM(EMCODE(I))
         X16=G(I)
         CALL SETTBL(CH16,X16)
        END IF
       ELSE
       IF (NAME(NUMMER(I))(1:1).EQ.'$') THEN
!      WRITE (6,121) NAME(NUMMER(I))
!      WRITE (out,121) NAME(NUMMER(I))
         CH16='G_'//NAME(NUMMER(I))(2:)
         X16=G(I)
         CALL SETTBL(CH16,X16)
       END IF
       END IF
      END DO
      END IF
!=====
!=====
!---- PRTLOG(10): pixelmaps
      IF (PRTLOG(10)) THEN
      DO I=1,NMAX
       IF (NUMMER(I).EQ.0) THEN
        IS=EMCODE(I)
        IF (EXSOL(IS).AND.SUGG(I).EQ.MINISUG(IS)) THEN
!      WRITE (6,121) SOLNAM(EMCODE(I))
!      WRITE (out,121) SOLNAM(EMCODE(I))
!  121 FORMAT (/,'GGG',1X,A)
         CH16='G_'//SOLNAM(EMCODE(I))
         X16=G(I)
         CALL SETMAP(CH16,X16)
         IF (G(I).LE.0.0D0) THEN
          X16=G(I)
          CH16='Gneg_'//SOLNAM(EMCODE(I))
          CALL SETMAP(CH16,X16)
         END IF
        END IF
        ELSE
       IF (NAME(NUMMER(I))(1:1).EQ.'$') THEN
!      WRITE (6,121) NAME(NUMMER(I))
!      WRITE (out,121) NAME(NUMMER(I))
         CH16='G_'//NAME(NUMMER(I))(2:)
         X16=G(I)
         CALL SETMAP(CH16,X16)
         IF (G(I).LE.0.0D0) THEN
          X16=G(I)
          CH16='Gneg_'//NAME(NUMMER(I))(2:)
          CALL SETMAP(CH16,X16)
         END IF
       END IF
       END IF
      END DO
      END IF
!=====

!=====
!      IF (PRTLOG(6).AND.(VOLSOL.NE.0.0D0.OR.WTSOL.NE.0.0D0)) THEN
      IF (PRTLOG(6)) THEN
!=====
!10-A-
      IF (DRU) THEN
      WRITE (6,120)
      WRITE (out,120)
  120 FORMAT (/ &
      ' ---------------------------------------'/ &
      ' volumes and densities of stable phases:'/ &
      ' ---------------------------------------')
      IF (WHS) THEN
      WRITE (6,121)
      WRITE (out,121)
  121 FORMAT ( &
      /'  solid phases',11X,'N',7X, &
      'volume/mol',2X,'volume[ccm]',4X,'vol%  |',4X,'wt/mol',7X, &
      'wt [g]',6X,'wt %  |',1X,'density [g/ccm]' &
      /'  ------------',11X,' ',7X, &
      '          ',2X,'          ',4X,'       |',4X,'      ',7X, &
      '      ',6X,'      |',1X,'               ' &
      )
      END IF
      END IF
!10-B-
!-----
      F001=0.0D0
      F002=0.0D0
      DO 700,I=1,NUN2
      IF (ISASOLID(I)) THEN
      IF (VOLSOL.EQ.0.0D0) THEN
      VPRO=0.0D0
      ELSE
      VPRO=VOLPH(I)/VOLSOL*100.0D0
      END IF
      F001=F001+VPRO
      IF (WTSOL.EQ.0.0D0) THEN
      WPRO=0.0D0
      ELSE
      WPRO=WTPH(I)/WTSOL*100.0D0
      END IF
      F002=F002+WPRO
      IF (VOLPH(I).EQ.0.0D0) THEN
      DENS=0.0D0
      DENS2=0.0D0
      ELSE
      DENS=WTPH(I)/VOLPH(I)
      END IF
!10-A-
      IF (DRU) THEN
      WRITE (6,122) STPHNAM(I),NN(I),VOLM(I),VOLPH(I),VPRO, &
      WTM(I),WTPH(I),WPRO,DENS
      WRITE (out,122) STPHNAM(I),NN(I),VOLM(I),VOLPH(I),VPRO, &
      WTM(I),WTPH(I),WPRO,DENS
  122 FORMAT (2X,A16,1X,F10.4,2X,F11.4,2X,F11.4,2X,F8.4,' | ', &
      F10.4,2X,F11.4,2X,F8.4,' | ',F12.6)
!---- und jetzt alles mit dG/dP
      IF (VCHECK) THEN
      IF (DABS(VOLM(I)-VOLM2(I)).GT.1D-6) THEN
       DENS2=WTPH(I)/VOLPH2(I)
       WRITE (6,123) 'volume check',VOLM2(I),VOLPH2(I),DENS2
  123  FORMAT (2X,A16,11X,2X,F11.4,2X,F11.4,49X,F12.6)
      END IF
      END IF
!----
      END IF
!10-B-
!*****
!---- PRTLOG(9): print table
      IF (PRTLOG(9)) THEN
!      CHTXT=TEXT(I)
!      IF (NUMMER(I).NE.0) THEN
!      IF (EMSOL(NUMMER(I)).NE.0) THEN
!      CHTXT=SOLNAM(EMSOL(NUMMER(I)))
!      ELSE
!      CHTXT=TEXT(I)
!      END IF
!      END IF
!wien09      CHTXT=STPHNAM(I)
      CHTXT=SHORTNAM(I)
      CH16='mvol_'//CHTXT
      X16=VOLM(I)
      CALL SETTBL(CH16,X16)
      CH16='V_'//CHTXT
      X16=VOLPH(I)
      CALL SETTBL(CH16,X16)
      CH16='rho_'//CHTXT
      X16=DENS
      CALL SETTBL(CH16,X16)
      IF (MGFE(I).NE.0.0D0) THEN
      CH16='Mg#_'//CHTXT
      X16=MGFE(I)
      CALL SETTBL(CH16,X16)
      END IF
      IF (SIPFU(I).NE.0.0D0) THEN
      CH16='Si_pfu_'//CHTXT
      X16=SIPFU(I)
      CALL SETTBL(CH16,X16)
      END IF
      IF (ALPFU(I).NE.0.0D0) THEN
      CH16='Al_pfu_'//CHTXT
      X16=ALPFU(I)
      CALL SETTBL(CH16,X16)
      END IF
      END IF
!*****
!---- PRTLOG(10): pixelmaps
      IF (PRTLOG(10)) THEN
!---- next line added 040606
!      CHTXT=TEXT(I)
!      IF (NUMMER(I).NE.0) THEN
!      IF (EMSOL(NUMMER(I)).NE.0) THEN
!      CHTXT=SOLNAM(EMSOL(NUMMER(I)))
!      ELSE
!      CHTXT=TEXT(I)
!      END IF
!      END IF
!wien09      CHTXT=STPHNAM(I)
      CHTXT=SHORTNAM(I)
!      CH16='mvol_'//CHTXT
!      X16=VOLM(I)
!      CALL SETMAP(CH16,X16)
      CH16='vol_'//CHTXT
      X16=VOLPH(I)
      CALL SETMAP(CH16,X16)
      CH16='rho_'//CHTXT
      X16=DENS
      CALL SETMAP(CH16,X16)
      IF (MGFE(I).NE.0.0D0) THEN
      CH16='Mg#_'//CHTXT
      X16=MGFE(I)
      CALL SETMAP(CH16,X16)
      END IF
      IF (SIPFU(I).NE.0.0D0) THEN
      CH16='Si_pfu_'//CHTXT
      X16=SIPFU(I)
      CALL SETMAP(CH16,X16)
      END IF
      IF (ALPFU(I).NE.0.0D0) THEN
      CH16='Al_pfu_'//CHTXT
      X16=ALPFU(I)
      CALL SETMAP(CH16,X16)
      END IF
      END IF
!*****
      END IF
  700 CONTINUE
      IF (VOLSOL.EQ.0.0D0) THEN
      F003=0.0D0
      ELSE
      F003=WTSOL/VOLSOL
      END IF
!10-A-
      IF (DRU.AND.WHS) THEN
      WRITE (6,124) VOLSOL,F001,WTSOL,F002,F003
      WRITE (out,124) VOLSOL,F001,WTSOL,F002,F003
  124 FORMAT (45X,'----------',2X,'-------- |',14X, &
      '----------',2X,'-------- |',3X,'----------'/ &
      '  total of solids',27X,F11.4,2X,F8.4,' | ',12X,F11.4, &
      2X,F8.4,' | ',F12.6)
      END IF
!10-B-
      END IF
!=====
!dC   IF (PRTLOG(6).AND.VOLSOL.NE.0.0D0) THEN
      IF (PRTLOG(6)) THEN
!=====
!10-A-
      IF (DRU.AND.WHNS) THEN
      IF (WHS) THEN
      WRITE (UNIT=6,FMT='('' '')')
      WRITE (UNIT=out,FMT='('' '')')
      END IF
      WRITE (6,126)
      WRITE (out,126)
  126 FORMAT (/ &
      '  gases and fluids',7X,'N',7X, &
      'volume/mol',2X,'volume[ccm]',15X,'wt/mol',7X, &
      'wt [g]',14X,'density [g/ccm]'/ &
      '  ----------------',7X,' ',7X, &
      '          ',2X,'           ',15X,'      ',7X, &
      '      ',14X,'               ')
      END IF
!10-B-
      DO 710,I=1,NUN2
      IF (.NOT.ISASOLID(I)) THEN
      IF (VOLPH(I).EQ.0.0D0) THEN
      DENS=0.0D0
      DENS2=0.0D0
      ELSE
      DENS=WTPH(I)/VOLPH(I)
      END IF
!10-A-
      IF (DRU) THEN
      WRITE (6,128) STPHNAM(I),NN(I),VOLM(I),VOLPH(I), &
      WTM(I),WTPH(I),DENS
      WRITE (out,128) STPHNAM(I),NN(I),VOLM(I),VOLPH(I), &
      WTM(I),WTPH(I),DENS
  128 FORMAT (2X,A16,F11.4,2X,F11.4,F13.3,10X,'   ', &
      F10.4,2X,F11.4,10X,'   ',F12.6)
!---- und jetzt alles mit dG/dP
      IF (VCHECK) THEN
      IF (DABS(VOLM(I)-VOLM2(I)).GT.1D-6) THEN
       IF (VOLPH2(I).EQ.0.0D0) THEN
        DENS2=0.0D0
       ELSE
        DENS2=WTPH(I)/VOLPH2(I)
       END IF
       WRITE (6,129) 'volume check',VOLM2(I),VOLPH2(I),DENS2
  129  FORMAT (2X,A16,11X,2X,F11.4,2X,F11.3,49X,F12.6)
      END IF
      END IF
!----
      END IF
!10-B-
!*****
!---- PRTLOG(9): print table
      IF (PRTLOG(9)) THEN
!C---- next line added 040606
!      CHTXT=TEXT(I)
!      IF (NUMMER(I).NE.0) THEN
!      IF (EMSOL(NUMMER(I)).NE.0) THEN
!      CHTXT=SOLNAM(EMSOL(NUMMER(I)))
!      ELSE
!      CHTXT=TEXT(I)
!      END IF
!      END IF
!wien09      CHTXT=STPHNAM(I)
      CHTXT=SHORTNAM(I)
      CH16='mvol_'//CHTXT
      X16=VOLM(I)
      CALL SETTBL(CH16,X16)
      CH16='V_'//CHTXT
      X16=VOLPH(I)
      CALL SETTBL(CH16,X16)
      CH16='rho_'//CHTXT
      X16=DENS
      CALL SETTBL(CH16,X16)
      END IF
!*****
!---- PRTLOG(10): pixelmaps
      IF (PRTLOG(10)) THEN
!C---- next line added 040606
!      CHTXT=TEXT(I)
!      IF (NUMMER(I).NE.0) THEN
!      IF (EMSOL(NUMMER(I)).NE.0) THEN
!      CHTXT=SOLNAM(EMSOL(NUMMER(I)))
!      ELSE
!      CHTXT=TEXT(I)
!      END IF
!      END IF
!wien09      CHTXT=STPHNAM(I)
      CHTXT=SHORTNAM(I)
!      CH16='mvol_'//CHTXT
!      X16=VOLM(I)
!      CALL SETMAP(CH16,X16)
      CH16='vol_'//CHTXT
      X16=VOLPH(I)
      CALL SETMAP(CH16,X16)
      CH16='rho_'//CHTXT
      X16=DENS
      CALL SETMAP(CH16,X16)
      END IF
!*****
      END IF
  710 CONTINUE
      END IF
!=====
!      IF (PRTLOG(6).AND.H2OSOL.NE.0.0D0.AND.WTSOL.NE.0.0D0) THEN
      IF (PRTLOG(6)) THEN
!=====
!10-A-
      IF (DRU.AND.WHH2O) THEN
      WRITE (6,130)
      WRITE (out,130)
  130 FORMAT (// &
      ' -----------------------------'/ &
      ' H2O content of stable phases:'/ &
      ' -----------------------------')
      IF (WHSH2O) THEN
      WRITE (6,131)
      WRITE (out,131)
  131 FORMAT ( &
      66X,'|   wt% of',5X,'wt% of',5X,'wt% of' &
      /'  solid phases',11X,'N',6X, &
      'H2O[pfu]',4X,'H2O[mol]',5X,'H2O [g]  |',3X,'phase',6X, &
      'solids',5X,'H2O.solid' &
      /'  ------------',11X,' ',7X, &
      '          ',2X,'          ',4X,'       |' &
      )
      END IF
      END IF
!10-B-
!-----
      DO 750,I=1,NUN2
      IF (ISASOLID(I).AND.(H2OM(I).GT.0.0D0)) THEN
      IF (WTPH(I).EQ.0.0D0) THEN
      F001=0.0D0
      ELSE
      F001=WTH2O(I)/WTPH(I)*100.0D0
      END IF
      F002=WTH2O(I)/WTSOL*100.0D0
      F003=WTH2O(I)/WH2OSOL*100.0D0
!10-A-
      IF (DRU) THEN
      WRITE (6,132) STPHNAM(I),NN(I),H2OM(I),H2OPH(I),WTH2O(I), &
      F001,F002,F003
      WRITE (out,132) STPHNAM(I),NN(I),H2OM(I),H2OPH(I),WTH2O(I), &
      F001,F002,F003
  132 FORMAT (2X,A16,1X,F10.4,2X,F8.3,2X,F11.4,2X,F11.4,' | ', &
      F9.5,2X,F9.5,2X,F9.4)
      END IF
!10-B-
!*****
!---- PRTLOG(9): print table
      IF (PRTLOG(9)) THEN
!C---- next line added 040606
!      CHTXT=TEXT(I)
!      IF (NUMMER(I).NE.0) THEN
!      IF (EMSOL(NUMMER(I)).NE.0) THEN
!      CHTXT=SOLNAM(EMSOL(NUMMER(I)))
!      ELSE
!      CHTXT=TEXT(I)
!      END IF
!      END IF
!wien09      CHTXT=STPHNAM(I)
      CHTXT=SHORTNAM(I)
      CH16='n_H2O_'//CHTXT
      X16=H2OPH(I)
      CALL SETTBL(CH16,X16)
      CH16='wt_H2O_'//CHTXT
      X16=WTH2O(I)
      CALL SETTBL(CH16,X16)
      END IF
!*****
!---- PRTLOG(10): pixelmaps
      IF (PRTLOG(10)) THEN
!C---- next line added 040606
!      CHTXT=TEXT(I)
!      IF (NUMMER(I).NE.0) THEN
!      IF (EMSOL(NUMMER(I)).NE.0) THEN
!      CHTXT=SOLNAM(EMSOL(NUMMER(I)))
!      ELSE
!      CHTXT=TEXT(I)
!      END IF
!      END IF
!wien09      CHTXT=STPHNAM(I)
      CHTXT=SHORTNAM(I)
      CH16='n_H2O_'//CHTXT
      X16=H2OPH(I)
      CALL SETMAP(CH16,X16)
      CH16='wt_H2O_'//CHTXT
      X16=WTH2O(I)
      CALL SETMAP(CH16,X16)
      END IF
!*****
      END IF
  750 CONTINUE
      F004=WH2OSOL/WTSOL*100.0D0
!10-A-
      IF (DRU.AND.WHSH2O) THEN
      WRITE (6,134) H2OSOL,WH2OSOL,F004
      WRITE (out,134) H2OSOL,WH2OSOL,F004
  134 FORMAT (44X,'--------',3X,'---------- |', &
      12X,' --------'/ &
      '  total H2O in solids',20X,F11.4,2X,F11.4,' |', &
      11X,F10.5)
      END IF
!10-B-
      END IF
!=====
!dC   IF (PRTLOG(6).AND.H2OSOL.NE.0.0D0) THEN
      IF (PRTLOG(6)) THEN
!=====
!10-A-
      IF (DRU.AND.WHNSH2O) THEN
      IF (WHSH2O) THEN
      WRITE (UNIT=6,FMT='('' '')')
      WRITE (UNIT=out,FMT='('' '')')
      END IF
      WRITE (6,136)
      WRITE (out,136)
  136 FORMAT ( &
      66X,'|   wt% of' &
      /'  gases and fluids',7X,'N',6X, &
      'H2O[pfu]',4X,'H2O[mol]',5X,'H2O [g]  |',3X,'phase' &
      /'  ----------------',7X,' ',7X, &
      '          ',2X,'          ',4X,'       |' &
      )
      END IF
!10-B-
!-----
      DO 752,I=1,NUN2
      IF (.NOT.ISASOLID(I).AND.(H2OM(I).GT.0.0D0)) THEN
      IF (WTPH(I).EQ.0.0D0) THEN
      F001=0.0D0
      ELSE
      F001=WTH2O(I)/WTPH(I)*100.0D0
      END IF
!10-A-
      IF (DRU) THEN
      WRITE (6,138) STPHNAM(I),NN(I),H2OM(I),H2OPH(I),WTH2O(I), &
      F001
      WRITE (out,138) STPHNAM(I),NN(I),H2OM(I),H2OPH(I),WTH2O(I), &
      F001
  138 FORMAT (2X,A16,1X,F10.4,2X,F8.3,2X,F11.4,2X,F11.4,' | ', &
      F9.5)
      END IF
!10-B-
!*****
!---- PRTLOG(9): print table
      IF (PRTLOG(9)) THEN
!C---- next line added 040606
!      CHTXT=TEXT(I)
!      IF (NUMMER(I).NE.0) THEN
!      IF (EMSOL(NUMMER(I)).NE.0) THEN
!      CHTXT=SOLNAM(EMSOL(NUMMER(I)))
!      ELSE
!      CHTXT=TEXT(I)
!      END IF
!      END IF
!wien09      CHTXT=STPHNAM(I)
      CHTXT=SHORTNAM(I)
      CH16='n_H2O_'//CHTXT
      X16=H2OPH(I)
      CALL SETTBL(CH16,X16)
      CH16='wt_H2O_'//CHTXT
      X16=WTH2O(I)
      CALL SETTBL(CH16,X16)
      END IF
!*****
!---- PRTLOG(10): pixelmaps
      IF (PRTLOG(10)) THEN
!---- next line added 040606
!      CHTXT=TEXT(I)
!      IF (NUMMER(I).NE.0) THEN
!      IF (EMSOL(NUMMER(I)).NE.0) THEN
!      CHTXT=SOLNAM(EMSOL(NUMMER(I)))
!      ELSE
!      CHTXT=TEXT(I)
!      END IF
!      END IF
!wien09      CHTXT=STPHNAM(I)
      CHTXT=SHORTNAM(I)
      CH16='n_H2O_'//CHTXT
      X16=H2OPH(I)
      CALL SETMAP(CH16,X16)
      CH16='wt_H2O_'//CHTXT
      X16=WTH2O(I)
      CALL SETMAP(CH16,X16)
      END IF
!*****
      END IF
  752 CONTINUE
!-----
      END IF
!=====
      IF (PRTLOG(7)) THEN
!=====
      WRITE (UNIT=scr,FMT=140)
      WRITE (UNIT=out,FMT=140)
  140 FORMAT (// &
      ' ---------------------------------------'/ &
      ' compositions of stable phases [ mol% ]:'/ &
      ' ---------------------------------------')
      WRITE (UNIT=scr,FMT='('' '')')
      WRITE (UNIT=out,FMT='('' '')')
      CH001=' '
      DO I001=1,NUN,10
        DO I=I001,MIN0(I001+9,NUN)
          I002=23+MOD(I-1,10)*11
          WRITE (UNIT=CH001(I002:),FMT='(A5)') CHNAME(I)
        END DO
        WRITE (UNIT=scr,FMT='(A133)') CH001
        WRITE (UNIT=out,FMT='(A133)') CH001
        CH001=' '
      END DO
      DO I001=1,COMAX
        SUMM(I001)=0.0D0
      END DO
      DO 520,I=1,NUN2
      WRITE (UNIT=CH001,FMT='(1X,A16)') STPHNAM(I)
      SUMME=0.0D0
      DO II=1,NUN
        SUMME=SUMME+DABS(X(I,II))
        SUMM(II)=SUMM(II)+X(I,II)*NN(I)
      END DO
      DO I001=1,NUN,10
        DO II=I001,MIN0(I001+9,NUN)
          I002=19+MOD(II-1,10)*11
          F001=X(I,II)/SUMME*100.0D0
          WRITE (UNIT=CH001(I002:),FMT='(F11.6)') F001
        END DO
        WRITE (UNIT=scr,FMT='(A133)') CH001
        WRITE (UNIT=out,FMT='(A133)') CH001
        CH001=' '
      END DO
  520 CONTINUE
      WRITE (UNIT=CH001,FMT='('' TOTAL:'')')
      SUMME=0.0D0
      DO I=1,NUN
        SUMME=SUMME+DABS(SUMM(I))
      END DO
      DO I001=1,NUN,10
        DO I=I001,MIN0(I001+9,NUN)
          I002=19+MOD(I-1,10)*11
          F001=SUMM(I)/SUMME*100.0D0
          WRITE (UNIT=CH001(I002:),FMT='(F11.6)') F001
        END DO
        WRITE (UNIT=scr,FMT='(A133)') CH001
        WRITE (UNIT=out,FMT='(A133)') CH001
        CH001=' '
      END DO
!=====
      WRITE (UNIT=scr,FMT=141)
      WRITE (UNIT=out,FMT=141)
  141 FORMAT (// &
      ' --------------------------'/ &
      ' elements in stable phases:'/ &
      ' --------------------------')
!      WRITE (UNIT=scr,FMT='('' '')')
!      WRITE (UNIT=out,FMT='('' '')')
      DO I=1,NUN
      SUMM(I)=0.0D0
      END DO
      WRITE (UNIT=scr,FMT='(10(:/16X,10(6X,A5)))') &
      (CHNAME(II),II=1,NUN)
      WRITE (UNIT=out,FMT='(10(:/16X,10(6X,A5)))') &
      (CHNAME(II),II=1,NUN)
      DO I=1,NUN2
      WRITE (UNIT=scr,FMT= &
      '(1X,A16,1X,10(F11.6),:10(/18X,10(F11.6)))') &
      STPHNAM(I),(X(I,II)*NN(I),II=1,NUN)
      WRITE (UNIT=out,FMT= &
      '(1X,A16,1X,10(F11.6),:10(/18X,10(F11.6)))') &
      STPHNAM(I),(X(I,II)*NN(I),II=1,NUN)
      DO II=1,NUN
      SUMM(II)=SUMM(II)+X(I,II)*NN(I)
      END DO
      END DO
      WRITE (UNIT=scr,FMT= &
      '(1X,''total: '',10X,10(F11.6),:10(/18X,10(F11.6)))') &
      (SUMM(II),II=1,NUN)
      WRITE (UNIT=out,FMT= &
      '(1X,''total: '',10X,10(F11.6),:10(/18X,10(F11.6)))') &
      (SUMM(II),II=1,NUN)
!=====
      WRITE (UNIT=scr,FMT=142)
      WRITE (UNIT=out,FMT=142)
  142 FORMAT (/ &
      ' elements per formula unit:')
!      WRITE (UNIT=scr,FMT='('' '')')
!      WRITE (UNIT=out,FMT='('' '')')
!      WRITE (UNIT=scr,FMT='(10(:/16X,10(6X,A5)))') &
!      (CHNAME(II),II=1,NUN)
!      WRITE (UNIT=out,FMT='(10(:/16X,10(6X,A5)))') &
!      (CHNAME(II),II=1,NUN)
      WRITE (UNIT=scr,FMT='('' '')')
      WRITE (UNIT=out,FMT='('' '')')
      DO I=1,NUN2
      WRITE (UNIT=scr,FMT= &
      '(1X,A16,1X,10(F11.6),:10(/18X,10(F11.6)))') &
      STPHNAM(I),(X(I,II),II=1,NUN)
      WRITE (UNIT=out,FMT= &
      '(1X,A16,1X,10(F11.6),:10(/18X,10(F11.6)))') &
      STPHNAM(I),(X(I,II),II=1,NUN)
      END DO
!
      END IF
!=====
      IF (PRTLOG(8)) THEN
!=====
      WRITE (UNIT=scr,FMT=143)
      WRITE (UNIT=out,FMT=143)
  143 FORMAT (// &
      ' -------------------------'/ &
      ' activities of all phases:'/ &
      ' -------------------------'/ &
      /10X,'phase',18X,'N',13X,'G',9X,'activity'/)
      CALL FINCLEAN
!=====
      WRITE (UNIT=scr,FMT=144)
      WRITE (UNIT=out,FMT=144)
  144 FORMAT (// &
      ' ----------------------------------'/ &
      ' chemical potentials of components:'/ &
      ' ----------------------------------'/)
      CALL COMPOS
!=====
!     write seeds to out-file
!-----
      WRITE (UNIT=scr,FMT='('' '')')
      WRITE (UNIT=out,FMT='('' '')')
      DO I=1,NUN2
       IF (EMCODE(I).GT.0) THEN
         IS=EMCODE(I)
         CH250=' '
         WRITE (UNIT=CH250,FMT='(A16)') SOLNAM(IS)
         DO II=1,NEND(IS)
           I001=INDEX(CH250,'     ')
           WRITE (UNIT=CH250(I001:),FMT='(2X,A16)') NAME(EM(IS,II))
           I001=INDEX(CH250,'     ')
           WRITE (UNIT=CH250(I001:),FMT='(2X,1PE15.8)') XEM(I,II)
         END DO
         CALL PUST(scr,CH250)
         CALL PUST(out,CH250)
       END IF
      END DO
!!=====
!!=====
      END IF
!=====
      RETURN
      END
!-----
!******************************
      SUBROUTINE FINCLEAN
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!-----END OF COMMON VARIABLES
      INTEGER(4) K,II,IS,I,I1,I2,N
!      REAL(8) FF
      CHARACTER(15) OSTR(PHMAX)
!-----
      K=NUN
    1 K=K+1
      IF (K.GT.NMAX) GOTO 88
      IF (NUMMER(K).EQ.0) THEN
      IS=EMCODE(K)
      IF (SUGG(K).NE.MINISUG(IS).AND.SUGCOD(K)(1:4).NE.'seed') THEN
      I1=K
      I2=NMAX
      CALL COLCHG(I1,I2)
      NMAX=NMAX-1
      K=K-1
      END IF
      END IF
      GOTO 1
   88 CONTINUE
!+++++
      N=0
      DO K=NUN+1,NMAX
        N=N+1
        WRITE (UNIT=OSTR(N),FMT='(3I5)') EMCODE(K),NUMMER(K),K
      END DO
      CALL SORTIER(OSTR,N)
!-----
      I2=NUN
      CALL PRTSTR(1,I2)
      WRITE (UNIT=scr,FMT='(68A1)') ('-',I=1,68)
      WRITE (UNIT=out,FMT='(68A1)') ('-',I=1,68)
      N=0
      DO K=NUN+1,NMAX
        N=N+1
        READ (UNIT=OSTR(N),FMT='(10X,I5)') II
        I2=II
        CALL PRTSTR(II,I2)
      END DO
!+++++
      RETURN
      END
!-----
!******************************
      SUBROUTINE COMPOS01
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!-----END OF COMMON VARIABLES
      INTEGER(4) I,LEOX,NLEOX,I1,INTX,NEL,NP0,NE0,NEL0,NS0
      REAL(8) RINTX,BULKEL(COMAX),GEL(COMAX),NELINC(COMAX), &
      FF(COMAX), &
      NOINC(COMAX),GCOMP(COMAX),OREMAIN,SUMOFG,SUMG1,GELTOT(COMAX)
      CHARACTER(40) COMPNAME(COMAX)
      CHARACTER(10) ELNAME(COMAX),CH1,CH2,CH3,CH4
!-----

      WRITE (UNIT=scr,FMT=1000) NUN-1,NUN2,GGTOT
 1000 FORMAT ( &
      ' number of elements (without E) = ',I3,/, &
      ' mumber of stable phases        = ',I3,/, &
      ' G(system) = ',F15.2)
!      IF (NUN2.EQ.(NUN-1)) THEN
!!!        WRITE (UNIT=scr,FMT=1001)
!!! 1001   FORMAT (' chemical potentials of all elements are defined')
!      ELSE
!        WRITE (UNIT=scr,FMT=1002)
! 1002   FORMAT (' chemical potentials of elements not defined')
!      END IF
!!

!      DO I=1,NUN
!        WRITE (UNIT=scr,FMT='(2I6)') NUMMER(I),EMCODE(I)
!      END DO


      NP0=0
      NE0=0
      NEL0=0
      NS0=0
      DO I=1,NUN
        IF (DABS(NN(I)).LT.1D-10) THEN
          IF (NUMMER(I).GT.0) THEN
            IF (NUMMER(I).GT.NUN.AND.NUMMER(I).LE.NPHA) NP0=NP0+1
            IF (NUMMER(I).LE.NUN.AND.NAME(NUMMER(I)).NE.'"E"') NEL0=NEL0+1
            IF (NUMMER(I).LE.NUN.AND.NAME(NUMMER(I)).EQ.'"E"') NE0=NE0+1
          ELSE
             NS0=NS0+1
          END IF
        END IF
      END DO

      WRITE(UNIT=scr,FMT='(/''phases with n ~ 0    '',i5)') NP0
      WRITE(UNIT=scr,FMT='(''elememts with n ~ 0  '',i5)') NEL0
      IF (NE0.NE.1) THEN
        WRITE(UNIT=scr,FMT='(''E ~zero '',i5)') NE0
      END IF
      WRITE(UNIT=scr,FMT='(''solutions with n ~ 0 '',i5)') NS0


      IF ((NP0+NEL0).GT.1) THEN
        WRITE (UNIT=scr,FMT='(/''oxydes probably not buffered'')')
      ELSE
        WRITE (UNIT=scr,FMT='(/''oxydes probably buffered'')')
      END IF


      NLEOX=0
      LEOX=0
      DO I=1,NMAX
       IF (NUMMER(I).GT.0) THEN
       IF (NAME(NUMMER(I)).EQ.'"O"') THEN
        LEOX=I
       END IF
       END IF
      END DO
!
!!      WRITE (UNIT=scr,FMT=100)
!!  100 FORMAT (/,2X,'number',3X,'name',11X,'G',9X,'N(tot)', &
!!      6X,'G*N(tot)',/)
!
      NEL=0
      SUMG1=0.0D0
      DO I=1,NMAX
       IF (NUMMER(I).GT.0.AND.NUMMER(I).LE.NUN) THEN
       IF (BULK(NUMMER(I)).GT.0.0D0) THEN
!
        NEL=NEL+1
        NELINC(NEL)=1.0D0
        NOINC(NEL)=OXNUM(NUMMER(I))
        BULKEL(NEL)=BULK(NUMMER(I))
        GEL(NEL)=G(I)
        GELTOT(NEL)=G(I)*BULK(NUMMER(I))
        SUMG1=SUMG1+GELTOT(NEL)
        IF (I.EQ.LEOX) NLEOX=NEL
        CALL LABLA(NAME(NUMMER(I)),I1)
        ELNAME(NEL)=NAME(NUMMER(I))(2:I1-1)
!
        IF (OXNUM(NUMMER(I)).GT.0.0D0) THEN
          INTX=IDINT(OXNUM(NUMMER(I)))
          RINTX=REAL(INTX)
          IF (RINTX.NE.OXNUM(NUMMER(I))) THEN 
            NELINC(NEL)=2.0D0*NELINC(NEL)
            NOINC(NEL)=2.0D0*NOINC(NEL)
          END IF
          CALL LABLA(NAME(NUMMER(I)),I1)
        END IF
!
!!        WRITE (UNIT=scr,FMT=2010) NUMMER(I),NAME(NUMMER(I)),G(I), &
!!        BULK(NUMMER(I)),GELTOT(NEL),GELTOT(NEL)
!! 2010   FORMAT (2X,I4,5X,A8,1X,1PE12.5,4X,0PF6.2,5X,1PE12.5, &
!!        5X,0P,F15.2)

       END IF
       END IF
      END DO
!!      WRITE (UNIT=scr,FMT=2020) SUMG1,SUMG1
!! 2020 FORMAT (47X,'------------',8X,'------------',/, &
!!      47X,1PE12.5,5X,0P,F15.2)
!
      OREMAIN=BULK(NUMMER(LEOX))
      SUMOFG=0.0D0
!
!!      WRITE (UNIT=scr,FMT=105)
!!  105 FORMAT (/,7X,'oxide',8X,'N(oxide)',5X,'G(oxide)',/)
!
      DO I=1,NEL
        IF (I.EQ.NLEOX) GOTO 777
        CH1=ELNAME(I)
        IF (NELINC(I).GT.1.0D0) THEN
          INTX=IDINT(NELINC(I))
          WRITE (UNIT=CH2,FMT='(I4)') INTX
        ELSE
          CH2=' '
        END IF
        IF (NOINC(I).NE.0.0D0) THEN
         CH3='O'
         IF (NOINC(I).GT.1.0D0) THEN
           INTX=IDINT(NOINC(I))
           WRITE (UNIT=CH4,FMT='(I4)') INTX
         ELSE
           CH4=' '
         END IF
        ELSE
         CH3=' '
         CH4=' '
        END IF
        COMPNAME(I)=CH1//CH2//CH3//CH4
        CALL COLLAPS(COMPNAME(I),I1)
        GCOMP(I)=BULKEL(I)*(GEL(I)*NELINC(I)+ &
                 GEL(NLEOX)*NOINC(I))/NELINC(I)
        OREMAIN=OREMAIN-BULKEL(I)*NOINC(I)/NELINC(I)
        SUMOFG=SUMOFG+GCOMP(I)
        FF(I)=(GEL(I)*NELINC(I)+GEL(NLEOX)*NOINC(I))
!!        WRITE (UNIT=scr,FMT=3005) COMPNAME(I),BULKEL(I)/NELINC(I), &
!!        GCOMP(I),GCOMP(I),FF(I)
!! 3005   FORMAT (8X,A10,3X,F6.2,5X,1PE12.5,5X,0P,F15.2,20X,F20.2)
  777 CONTINUE
      END DO

      COMPNAME(NLEOX)='O2'

!!
!!      WRITE (UNIT=scr,FMT=3020) SUMOFG,SUMOFG
!! 3020 FORMAT (32X,'------------',8X,'------------',/, &
!!      32X,1PE12.5,5X,0P,F15.2)
!
      IF (OREMAIN.NE.0.0D0) THEN
        FF(NLEOX)=G(LEOX)*2.0D0
!!        WRITE (UNIT=scr,FMT=3025) 'O',OREMAIN, &
!!        G(LEOX)*OREMAIN,G(LEOX)*OREMAIN,FF(NLEOX)
!! 3025   FORMAT (/,8X,A1,12X,F6.2,5X,1PE12.5,5X,0P,F15.2,20X,F20.2)
        SUMOFG=SUMOFG+G(LEOX)*OREMAIN
!!        WRITE (UNIT=scr,FMT=3030) SUMOFG,SUMOFG
!! 3030   FORMAT (32X,'------------',8X,'------------',/, &
!!        32X,1PE12.5,5X,0P,F15.2)
      END IF
!+++++
!+++++
      IF (DABS(SUMOFG+GGTOT).GT.1D-10) THEN
        WRITE (UNIT=scr,FMT='(/''SOMETHING WRONG, DG = '',1PE12.5,/)') SUMOFG+GGTOT
      ELSE
        WRITE (UNIT=scr,FMT='(/''ALL OK, DG = '',1PE12.5,/)') SUMOFG+GGTOT
      END IF
!+++++
!+++++
!!      DO I=1,NEL
!!        WRITE (UNIT=scr,FMT='(A,A)') ELNAME(I),COMPNAME(I)
!!      END DO
      DO I=1,NEL
        CALL LABLA(COMPNAME(I),I1)
        WRITE (UNIT=scr,FMT='(A,F20.2)') COMPNAME(I)(1:6),-FF(I)
      END DO
!+++++
!+++++
      RETURN
      END
!-----
!******************************
      SUBROUTINE COMPOS
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!-----END OF COMMON VARIABLES
      INTEGER(4) I,LEOX,NLEOX,I1,INTX,NEL,NP0,NEL0,NS0
      REAL(8) RINTX,BULKEL(COMAX),GEL(COMAX),NELINC(COMAX), &
      NOINC(COMAX),GCOMP(COMAX),OREMAIN,SUMOFG,SUMG1,GELTOT(COMAX), &
      FF(COMAX),X16
      CHARACTER(50) COMPNAME(COMAX)
      CHARACTER(32) CH16
      CHARACTER(10) ELNAME(COMAX),CH1,CH2,CH3,CH4
!-----

      WRITE (UNIT=scr,FMT=1000) NUN-1,NUN2
 1000 FORMAT ( &
      ' number of elements (without E) = ',I3,/, &
      ' mumber of stable phases        = ',I3)
      NP0=0
      NEL0=0
      NS0=0
      DO I=1,NUN
        IF (DABS(NN(I)).LT.1D-10) THEN
          IF (NUMMER(I).GT.0) THEN
            IF (NUMMER(I).GT.NUN.AND.NUMMER(I).LE.NPHA) NP0=NP0+1
            IF (NUMMER(I).LE.NUN.AND.NAME(NUMMER(I)).NE.'"E"') &
               NEL0=NEL0+1
          ELSE
             NS0=NS0+1
          END IF
        END IF
      END DO

      WRITE (UNIT=scr,FMT=1005) NP0,NEL0,NS0
 1005 FORMAT ( /, &
      ' number of phases with n ~ 0    = ',I3,/, &
      ' mumber of elememts with n ~ 0  = ',I3,/, &
      ' mumber of solutions with n ~ 0 = ',I3)
      IF ((NP0+NEL0).GT.1) THEN
        WRITE (UNIT=scr,FMT='(/'' oxydes probably not buffered'')')
      ELSE
        WRITE (UNIT=scr,FMT='(/'' oxydes probably buffered'')')
      END IF
!
      NLEOX=0
      LEOX=0
      DO I=1,NMAX
       IF (NUMMER(I).GT.0) THEN
       IF (NAME(NUMMER(I)).EQ.'"O"') THEN
        LEOX=I
       END IF
       END IF
      END DO
!
      NEL=0
      SUMG1=0.0D0
      DO I=1,NMAX
       IF (NUMMER(I).GT.0.AND.NUMMER(I).LE.NUN) THEN
       IF (BULK(NUMMER(I)).GT.0.0D0) THEN
!
        NEL=NEL+1
        NELINC(NEL)=1.0D0
        NOINC(NEL)=OXNUM(NUMMER(I))
        BULKEL(NEL)=BULK(NUMMER(I))
        GEL(NEL)=G(I)
        GELTOT(NEL)=G(I)*BULK(NUMMER(I))
        SUMG1=SUMG1+GELTOT(NEL)
        IF (I.EQ.LEOX) NLEOX=NEL
        CALL LABLA(NAME(NUMMER(I)),I1)
        ELNAME(NEL)=NAME(NUMMER(I))(2:I1-1)
!
        IF (OXNUM(NUMMER(I)).GT.0.0D0) THEN
          INTX=IDINT(OXNUM(NUMMER(I)))
          RINTX=REAL(INTX)
          IF (RINTX.NE.OXNUM(NUMMER(I))) THEN 
            NELINC(NEL)=2.0D0*NELINC(NEL)
            NOINC(NEL)=2.0D0*NOINC(NEL)
          END IF
        END IF
!
       END IF
       END IF
      END DO
!
      OREMAIN=BULK(NUMMER(LEOX))
      SUMOFG=0.0D0
!
      DO I=1,NEL
        IF (I.EQ.NLEOX) GOTO 777
        CH1=ELNAME(I)
        IF (NELINC(I).GT.1.0D0) THEN
          INTX=IDINT(NELINC(I))
          WRITE (UNIT=CH2,FMT='(I4)') INTX
        ELSE
          CH2=' '
        END IF
        IF (NOINC(I).NE.0.0D0) THEN
         CH3='O'
         IF (NOINC(I).GT.1.0D0) THEN
           INTX=IDINT(NOINC(I))
           WRITE (UNIT=CH4,FMT='(I4)') INTX
         ELSE
           CH4=' '
         END IF
        ELSE
         CH3=' '
         CH4=' '
        END IF
        COMPNAME(I)='"'//CH1//CH2//CH3//CH4//'"'
        CALL COLLAPS(COMPNAME(I),I1)
        CALL UPLOW2(COMPNAME(I))
        GCOMP(I)=BULKEL(I)*(GEL(I)*NELINC(I)+ &
                 GEL(NLEOX)*NOINC(I))/NELINC(I)
        OREMAIN=OREMAIN-BULKEL(I)*NOINC(I)/NELINC(I)
        SUMOFG=SUMOFG+GCOMP(I)
        FF(I)=(GEL(I)*NELINC(I)+GEL(NLEOX)*NOINC(I))
  777 CONTINUE
      END DO
      COMPNAME(NLEOX)='"o2"'
!
      IF (OREMAIN.NE.0.0D0) THEN
        FF(NLEOX)=G(LEOX)*2.0D0
        SUMOFG=SUMOFG+G(LEOX)*OREMAIN
      END IF
!+++++
      WRITE (UNIT=scr,FMT=2000)
      WRITE (UNIT=out,FMT=2000)
 2000 FORMAT (/, &
      ' component',3X,'chem.pot.',/ &
      ' ---------',3X,'---------')
!+++++
      DO I=1,NEL
        IF (DABS(FF(I)).GT.1D-10) THEN
        CALL LABLA(COMPNAME(I),I1)
        WRITE (UNIT=scr,FMT='(1X,A,3X,1PE12.5)') &
               COMPNAME(I)(1:8),-FF(I)
        WRITE (UNIT=out,FMT='(1X,A,3X,1PE12.5)') &
               COMPNAME(I)(1:8),-FF(I)
        IF (PRTLOG(9)) THEN
          CH16='M('//COMPNAME(I)(1:I1)//')'
          X16=-FF(I)
          CALL SETTBL(CH16,X16)
        END IF
        END IF
      END DO
!+++++
!+++++
      RETURN
      END
!-----
!******************************
      SUBROUTINE MULCHK
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!
!-----END OF COMMON VARIABLES
      INTEGER(4) I,II,III,K,I001,I002
      REAL(8) SUMME
!      REAL(8) G001,X001(EMAX)
!-----
      NUN2=NUN
      NUN3=NUN
      I=1
   10 IF (I.GT.NUN2) GO TO 11
      IF (DABS(NN(I)/BLKSUM).LT.1D-10) THEN
      DO K=I,(NUN2-1)
        I001=K+1
        CALL COLCHG(K,I001)
      END DO
      NUN2=NUN2-1
      I=I-1
      GO TO 999
      END IF
!====
      IF (EMCODE(I).GT.0) THEN
      DO 510,II=(I+1),NUN2
!--1
      IF (EMCODE(II).EQ.EMCODE(I)) THEN
      SUMME=0.0D0
      DO III=1,NEND(EMCODE(I))
        SUMME=SUMME+DABS(XEM(I,III)-XEM(II,III))
      END DO
!--2
      IF (SUMME.LT.EQUALX) THEN
      DO III=1,NEND(EMCODE(I))
       XEM(I,III)=(NN(I)*XEM(I,III)+NN(II)*XEM(II,III))/(NN(I)+NN(II))
      END DO
      NN(I)=NN(I)+NN(II)
      NN(II)=0.0D0
      I001=I
      CALL XSOL(I001)
      DO K=II,(NUN2-1)
        I001=K
        I002=K+1
        CALL COLCHG(I001,I002)
      END DO
      NUN2=NUN2-1
      NUN3=NUN3-1
      I=I-1
      GO TO 999
!--2else
!----- does not work in some complex solutions
!C      ELSE
!C      IS=EMCODE(I)
!C      DO 520,III=1,IS
!C  520 X001(III)=(XEM(I,III)+XEM(II,III))/2.0D0
!C      CALL GNONID(IS,X001,G001)
!CC--3
!C      IF (G001.LE.0.0D0) THEN
!C      DO 523,III=1,NEND(EMCODE(I))
!C  523 XEM(I,III)=(NN(I)*XEM(I,III)+NN(II)*XEM(II,III))/(NN(I)+NN(II))
!C      NN(I)=NN(I)+NN(II)
!C      NN(II)=0.0D0
!C      I001=I
!C      CALL XSOL(I001)
!C      DO 524,K=II,(NUN2-1)
!C      I001=K
!C      I002=K+1
!C  524 CALL COLCHG(I001,I002)
!C      NUN2=NUN2-1
!C      I=I-1
!C      GO TO 999
!C      END IF
!CC--3
      END IF
!--2
      END IF
!--1
  510 CONTINUE
      END IF
!=====
  999 I=I+1
      GO TO 10
   11 RETURN
      END
!-----
!******************************
!      SUBROUTINE PRPRPR(NROW,NCOL,RNR,CNR,ARR)
!!----- only used to test NAMESTPH
!      IMPLICIT NONE
!      INCLUDE 'theriak.cmn'
!      include 'files.cmn'
!      INTEGER(4) I,J,I1,I2,NROW,NCOL,RNR(0:COMAX),CNR(0:COMAX), &
!      IR,IC,IPIV,IRM,ICM,I001,IS,II
!      REAL(8) ARR(0:COMAX,0:EMAX),VALMAX
!      WRITE (6,1000) (CNR(IC),IC=1,NCOL)
! 1000 FORMAT (12X,100I12)
!      DO 500,IR=1,NROW
!      WRITE (6,1010) RNR(IR),(ARR(IR,IC),IC=1,NCOL)
! 1010 FORMAT (I12,100F12.5)
!  500 CONTINUE
!      RETURN
!      END
!-----
!******************************
      SUBROUTINE NAMESTPH
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!
!-----END OF COMMON VARIABLES
      INTEGER(4) I,J,I1,NROW,NCOL,RNR(0:COMAX),CNR(0:COMAX), &
      IR,IC,IPIV,IRM,ICM,I001,I002,IS,II
      REAL(8) ARR(0:COMAX,0:EMAX),VALMAX
!-----
!      STPHNAM: extended name of stable phase
!      SHORTNAM: short name of stable phase (=ABK if non-solution phase)
!                (= [ABK] (dominamt endmember) if solution phase)
!      STSOLCOD: number of dominant endmember in solution
!----- make STPHCOD:
!      0: non-solution phase,
!      1: single solution phase
!      n: exsolution to n coexisting phases
      DO 500,I=1,NUN2
      IF (NUMMER(I).GT.0) THEN
        STPHCOD(I)=0
        STSOLCOD(I)=0
      ELSE
        STPHCOD(I)=1
        IF (I.GT.1) THEN
          IF (EMCODE(I).EQ.EMCODE(I-1)) THEN
            I1=STPHCOD(I-1)
            STPHCOD(I)=I1
          DO J=I-I1,I
            STPHCOD(J)=STPHCOD(J)+1
          END DO
         END IF
        END IF
      END IF
  500 CONTINUE
!=====
      I1=1
    1 IF (I1.GT.NUN2) GOTO 11
!**
      IF (STPHCOD(I1).EQ.0) THEN
      STPHNAM(I1)=NAME(NUMMER(I1))
      SHORTNAM(I1)=ABK(NUMMER(I1))
!**
      ELSE
      NROW=STPHCOD(I1)
      NCOL=NEND(EMCODE(I1))
!----- make matrix
      DO IC=1,NCOL
        CNR(IC)=IC
      END DO
      DO IR=1,NROW
        RNR(IR)=I1+IR-1
        DO IC=1,NCOL
          ARR(IR,IC)=XEM(RNR(IR),IC)
        END DO
      END DO
!      WRITE (6,2000) SOLNAM(EMCODE(I1))
! 2000 FORMAT (/'start solution: ',A16)
!      CALL PRPRPR(NROW,NCOL,RNR,CNR,ARR)
!----- find maximum value
      IPIV=1
    2 IF (IPIV.GT.NROW) GOTO 22
      VALMAX=ARR(IPIV,IPIV)
      IRM=IPIV
      ICM=IPIV
      DO IR=IPIV,NROW
        DO IC=IPIV,NCOL
!cdcmar08 1d-6 added to avoid problems with equal compositions
          IF (ARR(IR,IC).GT.VALMAX+1D-6) THEN
          VALMAX=ARR(IR,IC)
          IRM=IR
          ICM=IC
          END IF
        END DO
      END DO
!----- switch maximum to ipiv,ipiv
      IF (IRM.NE.IPIV) THEN
      I001=RNR(IPIV)
      RNR(IPIV)=RNR(IRM)
      RNR(IRM)=I001
      DO IC=1,NCOL
        ARR(0,IC)=ARR(IPIV,IC)
      END DO
      DO IC=1,NCOL
        ARR(IPIV,IC)=ARR(IRM,IC)
      END DO
      DO IC=1,NCOL
        ARR(IRM,IC)=ARR(0,IC)
      END DO
      END IF
      IF (ICM.NE.IPIV) THEN
      I001=CNR(IPIV)
      CNR(IPIV)=CNR(ICM)
      CNR(ICM)=I001
      DO IR=1,NROW
        ARR(IR,0)=ARR(IR,IPIV)
      END DO
      DO IR=1,NROW
        ARR(IR,IPIV)=ARR(IR,ICM)
      END DO
      DO IR=1,NROW
        ARR(IR,ICM)=ARR(IR,0)
      END DO
      END IF
!--
      IPIV=IPIV+1
      GOTO 2
   22 CONTINUE
!=====
!      WRITE (6,2002) SOLNAM(EMCODE(I1))
! 2002 FORMAT (/'end solution  : ',A16)
!      CALL PRPRPR(NROW,NCOL,RNR,CNR,ARR)
!=====
      IS=EMCODE(I1)
      DO 640,J=1,NROW
      IR=RNR(J)
      II=CNR(J)
      CALL LABLA(SOLNAM(IS),I001)
      CALL LABLA(ABK(EM(IS,II)),I002)
      STPHNAM(IR)=SOLNAM(IS)(1:I001)//'_'//ABK(EM(IS,II))
      SHORTNAM(IR)='['//ABK(EM(IS,II))(1:I002)//']'
      STSOLCOD(IR)=II
  640 CONTINUE
!**
      END IF
      I1=I1+MAX(1,STPHCOD(I1))
      GOTO 1
   11 CONTINUE
      DO 650,I=1,NUN2
      CALL LABLA(STPHNAM(I),I1)
      DO 648,J=1,I1
      IF (STPHNAM(I)(J:J).EQ.' ') STPHNAM(I)(J:J)='_'
  648 CONTINUE
      CALL LABLA(SHORTNAM(I),I1)
      DO 649,J=1,I1
      IF (SHORTNAM(I)(J:J).EQ.' ') SHORTNAM(I)(J:J)='_'
  649 CONTINUE
  650 CONTINUE
!=====
!-----
      RETURN
      END
!-----
!******************************
      SUBROUTINE PRTSTR(N1,N2)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!-----END OF COMMON VARIABLES
      INTEGER(4) I,II,N1,N2,I001,I002,IS,IM
      REAL(8) EXPO,ACT
      CHARACTER(16) TEXT
      CHARACTER(32) TEXT2
      CHARACTER(133) CH001
!-----
      DO 510,I=N1,N2
      IF (SUGG(I).LE.NPHA) THEN
      CH001=' P'
      ELSE
      CH001=' S'
      END IF
      WRITE (UNIT=CH001(3:),FMT='(I6)') SUGG(I)
      IF (NUMMER(I).LE.0) THEN
      TEXT=SOLNAM(EMCODE(I))
      ELSE
      TEXT=NAME(NUMMER(I))
      END IF
!--
      CALL LABLA(TEXT,I002)
      IF (EMCODE(I).GT.0) THEN
       IS=EMCODE(I)
       IM=1
       DO I001=2,NEND(IS)
        IF (XEM(I,I001).GT.XEM(I,IM)) IM=I001
       END DO
       TEXT2=TEXT(1:I002)//'_'//ABK(EM(IS,IM))
      ELSE
        TEXT2=TEXT(1:I002)
      END IF
!--
      EXPO=-G(I)/RT
      IF (EXPO.LT.-150.0D0) THEN
      ACT=0.0D0
      ELSE
      ACT=DEXP(DMIN1(150.0D0,EXPO))
      END IF
!--
      WRITE (UNIT=CH001(9:),FMT=100) TEXT2,NN(I),G(I),ACT
! 100 FORMAT (2X,A16,2X,1PE12.5,2X,0P,F12.2,2X,1PE12.5)
  100 FORMAT (2X,A16,2X,1PE12.5,2X,1PE12.5,2X,1PE12.5)
!
      IF (EMCODE(I).GT.0) THEN
!====
      DO I001=1,NEND(EMCODE(I)),5
        DO II=I001,MIN0(I001+4,NEND(EMCODE(I)))
          I002=71+MOD(II-1,5)*12
          WRITE (UNIT=CH001(I002:),FMT='(1PE11.4)') XEM(I,II)
        END DO
!---
        IF (II.LT.NEND(EMCODE(I)).OR.TEST.GT.0.0D0) THEN
        WRITE (UNIT=scr,FMT='(A133)') CH001
        WRITE (UNIT=out,FMT='(A133)') CH001
        ELSE
        WRITE (UNIT=scr,FMT='(A133,5X,A16)') CH001,SUGCOD(I)
        WRITE (UNIT=out,FMT='(A133,5X,A16)') CH001,SUGCOD(I)
        END IF
!====
        CH001=' '
      END DO
      ELSE
      IF (TEST.GT.0.0D0) THEN
      WRITE (UNIT=scr,FMT='(A133)') CH001
      WRITE (UNIT=out,FMT='(A133)') CH001
      ELSE
      WRITE (UNIT=scr,FMT='(A133,5X,A16)') CH001,SUGCOD(I)
      WRITE (UNIT=out,FMT='(A133,5X,A16)') CH001,SUGCOD(I)
      END IF
!====
      END IF
!====
  510 CONTINUE
      RETURN
      END
!-----
!******************************
      SUBROUTINE PRTAAA(N1,N2)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!-----END OF COMMON VARIABLES
      INTEGER(4) I,II,N1,N2,I001,I002,IS,IM
      REAL(8) AR001(EMAX),MUE2(EMAX)
      CHARACTER(16) TEXT
      CHARACTER(32) TEXT2
      CHARACTER(133) CH001
!-----
      DO 510,I=N1,N2
      CH001=' '
      IF (SUGG(I).LE.NPHA) THEN
      CH001=' P'
      ELSE
      CH001=' S'
      END IF
      WRITE (UNIT=CH001(3:),FMT='(I6)') SUGG(I)
      IF (NUMMER(I).LE.0) THEN
      TEXT=SOLNAM(EMCODE(I))
      ELSE
      TEXT=NAME(NUMMER(I))
      END IF
!--
      IF (EMCODE(I).GT.0) THEN
      IS=EMCODE(I)
      IM=1
      DO I001=2,NEND(IS)
      IF (XEM(I,I001).GT.XEM(I,IM)) IM=I001
      END DO
      CALL LABLA(TEXT,I002)
      TEXT2=TEXT(1:I002)//'_'//ABK(EM(IS,IM))
      ELSE
      TEXT2=TEXT
      END IF
!--
      WRITE (UNIT=CH001(9:),FMT=100) TEXT2
! 100 FORMAT (2X,A16,2X,1PE12.5,2X,0P,F12.2,2X,1PE12.5)
! 100 FORMAT (2X,A16,2X,1PE12.5,2X,1PE12.5,2X,1PE12.5)
  100 FORMAT (2X,A16,2X,'mue(i) - G:          ')
      IF (EMCODE(I).GT.0) THEN
      IS=EMCODE(I)
      DO I001=1,NEND(IS)
        AR001(I001)=XEM(I,I001)
      END DO
      CALL MUECAL(IS,AR001,MUE2)
      DO I001=1,NEND(EMCODE(I)),5
        DO II=I001,MIN0(I001+4,NEND(EMCODE(I)))
          I002=71+MOD(II-1,5)*12
          WRITE (UNIT=CH001(I002:),FMT='(1PE11.4)') MUE2(II)-G(I)
        END DO
!---
        IF (II.LT.NEND(EMCODE(I)).OR.TEST.GT.0.0D0) THEN
        WRITE (UNIT=scr,FMT='(A133)') CH001
        WRITE (UNIT=out,FMT='(A133)') CH001
        ELSE
        WRITE (UNIT=scr,FMT='(A133,5X,A16)') CH001,SUGCOD(I)
        WRITE (UNIT=out,FMT='(A133,5X,A16)') CH001,SUGCOD(I)
        END IF
!---
        CH001=' '
      END DO
      ELSE
      IF (TEST.GT.0.0D0) THEN
      WRITE (UNIT=scr,FMT='(A133)') CH001
      WRITE (UNIT=out,FMT='(A133)') CH001
      ELSE
      WRITE (UNIT=scr,FMT='(A133,5X,A16)') CH001,SUGCOD(I)
      WRITE (UNIT=out,FMT='(A133,5X,A16)') CH001,SUGCOD(I)
      END IF
      END IF
  510 CONTINUE
      RETURN
      END
!-----
!******************************
      SUBROUTINE VOLCALC
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
      LOGICAL(4) ISASOLID(COMAX)
      COMMON /LOSOLID/ ISASOLID
!-----END OF COMMON VARIABLES
      INTEGER(4) I,II,IS,N,IELO,IELH,IELFE,IELMG,IELSI,IELAL,I2,IPH, &
      IE,IK,ILX,IX1
      REAL(8) XM,SJ,X16,FFE,FMG,FF,DG1,DG2,PVOR,DXP
      CHARACTER(16) TEXT(COMAX)
      CHARACTER(32) CH16
!-----
!---- finds index of O,H,FE,MG,SI,AL, assigns ISASOLID(I), calculates XELSI
!---- 
      DXP=P/1D5
      IELO=0
      IELH=0
      IELFE=0
      IELMG=0
      IELSI=0
      IELAL=0
      DO I=1,NUN
        IF (CHNAME(I).EQ.'O') IELO=I
        IF (CHNAME(I).EQ.'H') IELH=I
        IF (CHNAME(I).EQ.'FE'.OR.CHNAME(I).EQ.'Fe') IELFE=I
        IF (CHNAME(I).EQ.'MG'.OR.CHNAME(I).EQ.'Mg') IELMG=I
        IF (VERGL(CHNAME(I),'SI')) IELSI=I
        IF (VERGL(CHNAME(I),'AL')) IELAL=I
      END DO
!-----
      DO 500,I=1,NUN2
        ISASOLID(I)=.TRUE.
        IF (NUMMER(I).LE.0) THEN
          TEXT(I)=SOLNAM(EMCODE(I))
          IS=EMCODE(I)
          DO II=1,NEND(IS)
            IF (PHASID(EM(IS,II)).NE.'MIN') ISASOLID(I)=.FALSE.
          END DO
        ELSE
          TEXT(I)=NAME(NUMMER(I))
          IS=0
            IF (PHASID(NUMMER(I)).NE.'MIN') ISASOLID(I)=.FALSE.
        END IF
!*****
!*****
      IF (IS.GT.0) THEN
      IF (NSITE(IS).GT.0.AND.CHSITE(IS,1).NE.' ') THEN
      DO ILX=1,NSIEL(IS)
        XELSI(IS,ILX)=0.0D0
        DO IK=1,NEMQQ(IS,ILX)
          IE=EMQQ(IS,ILX,IK)
          XELSI(IS,ILX)=XELSI(IS,ILX)+EMXX(IS,ILX,IE)*XEM(I,IE)
        END DO
      END DO
      END IF
      END IF
!
!*****
!*****
  500 CONTINUE
      VOLSOL=0.0D0
      WTSOL=0.0D0
      H2OSOL=0.0D0
      WH2OSOL=0.0D0
      FFE=0.0D0
      FMG=0.0D0
!     VNEED should always be true
      IF (.NOT.VNEED) RETURN
!*****
!*****
      DO 600,I=1,NUN2
!-----
!===== volumes for non-solution phases
      IF (NUMMER(I).NE.0) THEN
        VOLM(I)=VV(NUMMER(I))*10.0D0
        VOLPH(I)=VOLM(I)*NN(I)
      END IF
!-----
!===== volumes for solution phases 1. linear
      IF (EMCODE(I).GT.0) THEN
        IS=EMCODE(I)
        VOLM(I)=0.0D0
        DO II=1,NEND(IS)
          VOLM(I)=VOLM(I)+XEM(I,II)*VV(EM(IS,II))*10.0D0
        END DO
        VOLPH(I)=VOLM(I)*NN(I)
!-----
!===== volumes for solution phases 2. Margules excess volume
        DO N=1,NMARG(IS)
          XM=1.0D0
          DO II=1,POLY(IS,N)
            XM=XM*XEM(I,INDX(IS,N,II))
          END DO
          SJ=0.0D0
          DO II=1,RANGE(IS,N)
            SJ=SJ+XEM(I,SJIND(IS,N,II))
          END DO
          IF (SJ.LE.0.0D0.OR.WK(IS,N).LE.0.0D0) THEN
            VOLM(I)=VOLM(I)+WV(IS,N)*XM*10.0D0
          ELSE
            VOLM(I)=VOLM(I)+WV(IS,N)*XM*10.0D0/(SJ**WK(IS,N))
          END IF
        END DO
        VOLPH(I)=VOLM(I)*NN(I)
!-----
!===== volumes for solution phases 3. dG/dP fuer komische Phasen
!===== note: Endglieder in GCALC
        IF (LAAR(IS).OR.MODELL(IS).EQ.'F') THEN
          IPH=I
          CALL SORTOFNURVONPT
          CALL RECHG2(IPH,DG1)
          PVOR=P
          P=P+DXP
          CALL SORTOFNURVONPT
          CALL RECHG2(IPH,DG2)
          VOLPH(I)=(DG2-DG1)/DXP*10.0D0
          VOLM(I)=VOLPH(I)/NN(I)
          P=PVOR
          CALL SORTOFNURVONPT
        END IF
!-----
      END IF
!-----
      WTM(I)=0.0D0
      DO II=1,NUN
        WTM(I)=WTM(I)+X(I,II)*MOLWT(II)
      END DO
      WTPH(I)=WTM(I)*NN(I)
!-----
      IF (IELO.NE.0.AND.IELH.NE.0) THEN
        H2OM(I)=X(I,IELH)/2.0D0
        H2OPH(I)=H2OM(I)*NN(I)
        WTH2O(I)=H2OPH(I)*(MOLWT(IELH)*2.0D0+MOLWT(IELO))
      END IF
!-----
      IF (IELFE.NE.0) FFE=X(I,IELFE)
      IF (IELMG.NE.0) FMG=X(I,IELMG)
      IF ((FFE+FMG).NE.0.0D0) THEN
        MGFE(I)=FMG/(FFE+FMG)
      ELSE
        MGFE(I)=0.0D0
      END IF
!+++++
      SIPFU(I)=0.0D0
      IF (EMCODE(I).GT.0.AND.IELSI.NE.0) THEN
       SIPFU(I)=X(I,IELSI)
       I2=0
       IS=EMCODE(I)
       DO II=1,NEND(IS)
         FF=XX(EM(IS,II),IELSI)
!!!!         WRITE (scr,fmt='(''sipfu '',i3,f10.4)') ii,ff
         IF (DABS(FF-SIPFU(I)).GT.1D-10) I2=1
       END DO
       IF (I2.EQ.0) SIPFU(I)=0.0D0
      END IF
!+++++
      ALPFU(I)=0.0D0
      IF (EMCODE(I).GT.0.AND.IELAL.NE.0) THEN
       ALPFU(I)=X(I,IELAL)
       I2=0
       IS=EMCODE(I)
       DO II=1,NEND(IS)
         FF=XX(EM(IS,II),IELAL)
         IF (DABS(FF-ALPFU(I)).GT.1D-10) I2=1
       END DO
       IF (I2.EQ.0) ALPFU(I)=0.0D0
      END IF
!+++++
!-----
      IF (ISASOLID(I)) THEN
        VOLSOL=VOLSOL+VOLPH(I)
        WTSOL=WTSOL+WTPH(I)
        H2OSOL=H2OSOL+H2OPH(I)
        WH2OSOL=WH2OSOL+WTH2O(I)
      END IF
  600 CONTINUE
!*****
!---- next line added 040606
      IF (VOLSOL.NE.0.0D0) THEN
!---- PRTLOG(9): print table
      IF (PRTLOG(9)) THEN
      CH16='V_solids'
      X16=VOLSOL
      CALL SETTBL(CH16,X16)
      CH16='rho_solids'
      X16=WTSOL/VOLSOL
      CALL SETTBL(CH16,X16)
      CH16='n_H2O_solids'
      X16=H2OSOL
      CALL SETTBL(CH16,X16)
      CH16='wt_H2O_solids'
      X16=WH2OSOL
      CALL SETTBL(CH16,X16)
      CH16='pcH2O_solids'
      X16=WH2OSOL/WTSOL*100.0D0
      CALL SETTBL(CH16,X16)
      END IF
!*****
!---- PRTLOG(10): pixelmaps
      IF (PRTLOG(10)) THEN
      CH16='V_solids'
      X16=VOLSOL
      CALL SETMAP(CH16,X16)
      CH16='rho_solids'
      X16=WTSOL/VOLSOL
      CALL SETMAP(CH16,X16)
      CH16='n_H2O_solids'
      X16=H2OSOL
      CALL SETMAP(CH16,X16)
      CH16='wt_H2O_solids'
      X16=WH2OSOL
      CALL SETMAP(CH16,X16)
      CH16='pcH2O_solids'
      X16=WH2OSOL/WTSOL*100.0D0
      CALL SETMAP(CH16,X16)
      END IF
!---- next line added 040606
      END IF
!*****
      RETURN
      END
!-----
!******************************
      SUBROUTINE RECHG2(I,DGX)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
!
!      LOGICAL(4) ISASOLID(COMAX)
!      COMMON /LOSOLID/ ISASOLID
!-----
      REAL(8) DGX,XXX(EMAX),GGG
      INTEGER(4) I,II,IS
!-----
      DGX=0.0D0
!++++
!      IF (ISASOLID(I)) THEN
!++++
       IF (NUMMER(I).GT.0) THEN
        DGX=DGX+NN(I)*GGK(NUMMER(I))
       ELSE
        IS=EMCODE(I)
        DO II=1,NEND(IS)
         XXX(II)=XEM(I,II)
        END DO
        CALL GNONID(IS,XXX,GGG)
        DO II=1,NEND(IS)
         GGG=GGG-XXX(II)*GG(EM(IS,II))+XXX(II)*GGK(EM(IS,II))
        END DO
        DGX=DGX+NN(I)*GGG
!++++
!       END IF
!++++
       END IF
!*****
      RETURN
      END
!-----
!******************************
      SUBROUTINE RECHG(DGX)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
!
!      LOGICAL(4) ISASOLID(COMAX)
!      COMMON /LOSOLID/ ISASOLID
!-----
      REAL(8) DGX,XXX(EMAX),GGG
      INTEGER(4) I,II,IS
!-----
      DGX=0.0D0
      DO I=1,NUN2
!++++
!      IF (ISASOLID(I)) THEN
!++++
       IF (NUMMER(I).GT.0) THEN
        DGX=DGX+NN(I)*GGK(NUMMER(I))
       ELSE
        IS=EMCODE(I)
        DO II=1,NEND(IS)
         XXX(II)=XEM(I,II)
        END DO
        CALL GNONID(IS,XXX,GGG)
        DO II=1,NEND(IS)
        GGG=GGG-XXX(II)*GG(EM(IS,II))+XXX(II)*GGK(EM(IS,II))
        END DO
        DGX=DGX+NN(I)*GGG
!++++
!       END IF
!++++
       END IF
      END DO
!*****
      RETURN
      END
!-----
!*************************************************************
!*************************************************************
      SUBROUTINE GETVAL(PH1,VDEF,FMF,IP,ISS,IE,WERT)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!
!-----END OF COMMON VARIABLES
      CHARACTER*(*) PH1,VDEF
      CHARACTER(32) TEXT
      INTEGER(4) I,IS,IP,IE,ISS,I1,ILX,IL,NSUCH,IMG,IFE
      REAL(8) WERT,FMF,FF,HH0,SS0,LECP
!=====
!---  PH1:  name of phase or solution phase(can be "bulk"
!---  VDEF: variable name, G,VG ...(IE=999) or element name (IE=element number)
!---  FMF:  variable will be divided by FMF
!---  IP:   phase number (in CALSTR) =-1 for "bulk"
!---  ISS:  solution number (0 for phase)
!---  WERT: value of variable
      WERT=0.0D0
      IP=0
      ISS=0
      IE=0
!=====
      IF (VERGL(PH1,'bulk')) THEN
        IP=-1
        IF (VERGL(VDEF,'G')) THEN
         WERT=GGTOT/FMF
         IE=999
        END IF
        IF (VERGL(VDEF,'VG')) THEN
         CALL VTOTOFG(FF)
         WERT=FF/FMF
         IE=999
        END IF
        IF (VERGL(VDEF,'HG')) THEN
         CALL HSCTOTOFG(HH0,SS0,LECP)
         WERT=HH0/FMF
         IE=999
        END IF
        IF (VERGL(VDEF,'H-H0')) THEN
         CALL HMINH0TOT(HH0)
         WERT=HH0/FMF
         IE=999
        END IF
        IF (VERGL(VDEF,'SG')) THEN
         CALL HSCTOTOFG(HH0,SS0,LECP)
         WERT=SS0/FMF
         IE=999
        END IF
        IF (VERGL(VDEF,'CPG')) THEN
         CALL HSCTOTOFG(HH0,SS0,LECP)
         WERT=LECP/FMF
         IE=999
        END IF
        IF (VERGL(VDEF,'VSOL')) THEN
         WERT=VOLSOL/FMF
         IE=999
        END IF
        IF (VERGL(VDEF,'WTSOL')) THEN
         WERT=WTSOL/FMF
         IE=999
        END IF
        IF (VERGL(VDEF,'RHOSOL')) THEN
         WERT=WTSOL/VOLSOL/FMF
         IE=999
        END IF
!-----check for elements
        IF (IE.EQ.0) THEN
        DO I=1,NUN
         IF (VERGL(CHNAME(I),VDEF)) THEN
          IE=I
          WERT=BULK(I)/FMF
         END IF
        END DO
        END IF
!-----return (phase='bulk')
        GOTO 999
      END IF
!=====
!      DO I=1,NMAX
!        I1=I
!        CALL GETNAME(I1,IS,TEXT)
!        WRITE (UNIT=6,FMT='(''TEXT '',A)') TEXT
!      END DO
!      NSUCH=NUN2
      NSUCH=NMAX
!      IF (VERGL(VDEF,'G')) NSUCH=NPHA
!=====
      IP=0
      ISS=0
      DO I=1,NSUCH
       I1=I
       CALL GETNAME(I1,IS,TEXT)
       IF (VERGL(TEXT,PH1)) THEN

         IF (IS.EQ.0) THEN
           IP=I
           ISS=0
           GOTO 5
         ELSE
           IF (SUGG(I).EQ.MINISUG(IS).OR.IP.LT.NUN2) THEN
             IP=I
             ISS=IS
             GOTO 5
           END IF
         END IF

       END IF
      END DO
!
    5 CONTINUE

!-----return if IP=0 ???
      IF (IP.EQ.0) GOTO 999
      IF (IP.GT.NUN2) GOTO 22
!-----check for special variables
      IF (VERGL(VDEF,'V')) THEN
         WERT=VOLM(IP)/FMF
         IE=999
         GOTO 999
      END IF
!--- following works only for non-solution phases
      IF (VERGL(VDEF,'V/V0').AND.NUMMER(IP).NE.0) THEN
         I1=NUMMER(IP)
         WERT=VOLM(IP)/FMF/REDATA(4,I1)/10.0D0
         IE=999
         GOTO 999
      END IF
      IF (VERGL(VDEF,'VG')) THEN
       CALL VOLOFG(IP,FF)
       WERT=FF/FMF
         IE=999
         GOTO 999
      END IF
      IF (VERGL(VDEF,'V2')) THEN
       CALL VOLOFGG(IP,FF)
       WERT=FF/FMF
         IE=999
         GOTO 999
      END IF
      IF (VERGL(VDEF,'HG')) THEN
       CALL HSCOFG(IP,HH0,SS0,LECP)
       WERT=HH0/FMF
         IE=999
         GOTO 999
      END IF
      IF (VERGL(VDEF,'H-H0')) THEN
       CALL HMINH0(IP,HH0)
       WERT=HH0/FMF
         IE=999
         GOTO 999
      END IF
      IF (VERGL(VDEF,'SG')) THEN
       CALL HSCOFG(IP,HH0,SS0,LECP)
       WERT=SS0/FMF
         IE=999
         GOTO 999
      END IF
      IF (VERGL(VDEF,'CPG')) THEN
       CALL HSCOFG(IP,HH0,SS0,LECP)
       WERT=LECP/FMF
         IE=999
         GOTO 999
      END IF
   22 IF (VERGL(VDEF,'N')) THEN
       WERT=NN(IP)/FMF
         IE=999
         GOTO 999
      END IF
      IF (VERGL(VDEF,'G')) THEN
       WERT=G(IP)/FMF
         IE=999
         GOTO 999
      END IF
      IF (VERGL(VDEF,'FE#').OR.VERGL(VDEF,'MG#')) THEN
       IMG=0
       IFE=0
       DO I=1,NUN
        IF (VERGL(CHNAME(I),'MG')) IMG=I
        IF (VERGL(CHNAME(I),'FE')) IFE=I
       END DO
       FF=X(IP,IMG)+X(IP,IFE)
       IF (FF.NE.0.0D0) THEN
        IF (VERGL(VDEF,'MG#')) WERT=X(IP,IMG)/FF/FMF
        IF (VERGL(VDEF,'FE#')) WERT=X(IP,IFE)/FF/FMF
       END IF
         IE=999
         GOTO 999
      END IF
!--- differences too small
      IF (VERGL(VDEF,'BETA')) THEN
       CALL BETA(IP,FF)
       WERT=FF/FMF
         IE=999
         GOTO 999
      END IF
!--- differences too small
!      IF (VERGL(VDEF,'KT')) THEN
!       CALL BETA(IP,FF)
!       WERT=-1.0D0/FF/FMF
!         IE=999
!         GOTO 999
!      END IF
!--- differences too small
!      IF (VERGL(VDEF,'ALPHA')) THEN
!       CALL ALP(IP,FF)
!       WERT=FF/FMF
!         IE=999
!         GOTO 999
!      END IF
!
!-----check for elements
      IE=0
      DO I=1,NUN
       IF (VERGL(CHNAME(I),VDEF)) IE=I
      END DO
!-----check for site elements
      IF (IE.EQ.0.AND.ISS.NE.0) THEN
       IS=ISS
       ILX=0
       DO I=1,NSITE(IS)
        DO IL=1,NELPS(IS,I)
         ILX=ILX+1
         IF (VERGL(SIEL(IS,ILX),VDEF)) IE=-ILX
        END DO
       END DO
      END IF
!--
!---- assign value
      IF (IP.NE.0.AND.IE.NE.0) THEN
       IF (IE.GT.0) WERT=X(IP,IE)/FMF
       IF (IE.LT.0) WERT=XELSI(IS,-IE)/FMF
      END IF
!----
  999 CONTINUE

!!
!!      WRITE (6,2000) TC,P
!! 2000 FORMAT ('     tc ',F20.10,'  p ',F20.10)
!!      WRITE (6,1000) PH1,VDEF,WERT
!! 1000 FORMAT (' ph1 ',A16,' vdef ',A16,' wert ',F20.10)
!!      WRITE (6,1010) IP,ISS,IE
!! 1010 FORMAT (' IP ',I5,' ISS ',I5,' IE ',I5)
!!


      RETURN
      END
!
!-----
!*************************************************************
!*************************************************************
      SUBROUTINE VOLOFG(I,VOG)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!
      REAL(8) VOG,DXP,DG1,DG2,PVOR
      INTEGER(4) I,IPH
!
      DG1=0.0D0
      DG2=0.0D0
      VOG=0.0D0
        DXP=P/1D5
        PVOR=P
        IPH=I
        P=PVOR+DXP
        CALL SORTOFNURVONPT
        CALL RECHG2(IPH,DG1)
        P=PVOR-DXP
        CALL SORTOFNURVONPT
        CALL RECHG2(IPH,DG2)
        VOG=(DG1-DG2)/(2.0D0*DXP)*10.0D0
!       WRITE (UNIT=35,FMT='(''NN I='',F7.2)') NN(I)
        VOG=VOG/NN(I)
!!      WRITE (UNIT=6,FMT='(''DG1-DG2 '',1PE20.12)') DG1-DG2
        P=PVOR
        CALL SORTOFNURVONPT
!
      RETURN
      END
!
!-----
!*************************************************************
!*************************************************************
! if VDEF='V2'
      SUBROUTINE VOLOFGG(I,VOG)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!
      REAL(8) VOG,DXP,DG1,DG2,DG0,PVOR
      INTEGER(4) I,IPH
!
      INTEGER(4) ROWMAX,COLMAX
      PARAMETER (ROWMAX=10,COLMAX=ROWMAX+1)
      REAL(8) MAT(45,45),XG(45)
      INTEGER(4) N,J
      LOGICAL(4) SING
!
      DG0=0.0D0
      DG1=0.0D0
      DG2=0.0D0
      VOG=0.0D0
        DXP=P/1D5
        PVOR=P
        IPH=I
        P=PVOR+DXP
        CALL SORTOFNURVONPT
        CALL RECHG2(IPH,DG1)
        P=PVOR-DXP
        CALL SORTOFNURVONPT
        CALL RECHG2(IPH,DG2)
!        VOG=(DG1-DG2)/(2.0D0*DXP)*10.0D0
!        VOG=VOG/NN(I)
        P=PVOR
        CALL SORTOFNURVONPT
        CALL RECHG2(IPH,DG0)
!
      N=3
      MAT(1,1)=PVOR*PVOR
      MAT(1,2)=PVOR
      MAT(1,3)=1.0D0
      MAT(1,4)=DG0
!
      MAT(2,1)=(PVOR+DXP)*(PVOR+DXP)
      MAT(2,2)=(PVOR+DXP)
      MAT(2,3)=1.0D0
      MAT(2,4)=DG1
!
      MAT(3,1)=(PVOR-DXP)*(PVOR-DXP)
      MAT(3,2)=(PVOR-DXP)
      MAT(3,3)=1.0D0
      MAT(3,4)=DG2
!
      CALL GAUSSEL(MAT,ROWMAX,COLMAX,N,XG,SING)
      IF (.NOT. SING) THEN
       WRITE (UNIT=6,FMT='(''From GAUSSEL'')')
       DO J=1,N
        WRITE (UNIT=6,FMT='(''X('',I2,'') ='',1PE20.12)') J,XG(J)
       END DO
      ELSE
       PRINT *, 'Matrix is (nearly) singular'
       VOG=0.0D0
       RETURN
      END IF
      VOG=2.0D0*XG(1)*(PVOR)+XG(2)
      VOG=VOG*10.0D0/NN(I)
      WRITE (UNIT=6,FMT='(''-20 XG(1)/Vol '',1PE20.12)') &
      -20.0D0*XG(1)/VOG
!
      RETURN
      END
!-----
!*************************************************************
!*************************************************************
      SUBROUTINE VTOTOFG(VOG)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!
      REAL(8) VOG,DXP,DG1,DG2,PVOR
!
        DXP=P/1D5
        PVOR=P
        P=PVOR+DXP
        CALL SORTOFNURVONPT
        CALL RECHG(DG1)
        P=PVOR-DXP
        CALL SORTOFNURVONPT
        CALL RECHG(DG2)
        VOG=(DG1-DG2)/(2.0D0*DXP)*10.0D0
        P=PVOR
        CALL SORTOFNURVONPT
!
      RETURN
      END
!
!-----
!*************************************************************
!*************************************************************
      SUBROUTINE BETA(IP,F1)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!--- differences too small
      INTEGER(4) IP,I1
      REAL(8) PNOW,VP0,VPP,VPM,DXP,F1
!
        F1=0.0D0
        I1=IP
        DXP=P/1D5
        PNOW=P
        CALL VOLOFG(I1,VP0)
      WRITE (UNIT=6,FMT='(''VP0 '',1PE20.12)') VP0
        P=PNOW-DXP
        CALL VOLOFG(I1,VPM)
      WRITE (UNIT=6,FMT='(''VPM '',1PE20.12)') VPM
        P=PNOW+DXP
        CALL VOLOFG(I1,VPP)
      WRITE (UNIT=6,FMT='(''VPP '',1PE20.12)') VPP
        F1=-(VPP-VPM)/(2.0D0*DXP)/VP0
        P=PNOW
        CALL SORTOFNURVONPT
      WRITE (UNIT=6,FMT='(''VPP-VPM '',1PE20.12)') VPP-VPM
!
      RETURN
      END
!
!-----
!*************************************************************
!*************************************************************
      SUBROUTINE ALP(IP,F1)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!--- differences too small
      INTEGER(4) IP,I1
      REAL(8) TNOW,VT0,VTP,VTM,DXT,F1
!
        F1=0.0D0
        I1=IP
        DXT=T/1D5
        TNOW=TC
        CALL VOLOFG(I1,VT0)
        TC=TNOW-DXT
        CALL VOLOFG(I1,VTM)
        TC=TNOW+DXT
        CALL VOLOFG(I1,VTP)
        F1=(VTP-VTM)/(2.0D0*DXT)/VT0
        TC=TNOW
        CALL SORTOFNURVONPT
!
      RETURN
      END
!
!-----
!*************************************************************
!*************************************************************
      SUBROUTINE HSCOFG(I,HH0,SS0,LECP)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!-----
      REAL(8) DG0,DG1,DG2,DGM1,DGM2,TVOR,DXT,SS0,SS1,SSM1, &
      TE0,TE1,TEM1,HH0,HH1,HHM1,LECP
      INTEGER(4) I,IPH
!-----
      DXT=T/1D4
      TVOR=TC
      IPH=I
!-----
      CALL RECHG2(IPH,DG0)
      TE0=T
!-----
      TC=TVOR+DXT
      CALL SORTOFNURVONPT
      CALL RECHG2(IPH,DG1)
      TE1=T
!-----
      TC=TVOR+2.0D0*DXT
      CALL SORTOFNURVONPT
      CALL RECHG2(IPH,DG2)
!-----
      TC=TVOR-DXT
      CALL SORTOFNURVONPT
      CALL RECHG2(IPH,DGM1)
      TEM1=T
!-----
      TC=TVOR-2.0D0*DXT
      CALL SORTOFNURVONPT
      CALL RECHG2(IPH,DGM2)
!=====
      SS0=-(DG1-DGM1)/(2.0D0*DXT)
      SS1=-(DG2-DG0)/(2.0D0*DXT)
      SSM1=-(DG0-DGM2)/(2.0D0*DXT)
      HH0=DG0+(TE0)*SS0
      HH1=DG1+(TE1)*SS1
      HHM1=DGM1+(TEM1)*SSM1
      LECP=(HH1-HHM1)/(2.0D0*DXT)
      HH0=HH0/NN(I)
      SS0=SS0/NN(I)
      LECP=LECP/NN(I)
!=====
!==============================
      TC=TVOR
      CALL SORTOFNURVONPT
!*****
      RETURN
      END
!
!-----
!*************************************************************
!*************************************************************
      SUBROUTINE HSCTOTOFG(HH0,SS0,LECP)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!-----
      REAL(8) DG0,DG1,DG2,DGM1,DGM2,TVOR,DXT,SS0,SS1,SSM1, &
      TE0,TE1,TEM1,HH0,HH1,HHM1,LECP
!-----
      DXT=T/1D5
      TVOR=TC
!-----
      CALL RECHG(DG0)
      TE0=T
!-----
      TC=TVOR+DXT
      CALL SORTOFNURVONPT
      CALL RECHG(DG1)
      TE1=T
!-----
      TC=TVOR+2.0D0*DXT
      CALL SORTOFNURVONPT
      CALL RECHG(DG2)
!-----
      TC=TVOR-DXT
      CALL SORTOFNURVONPT
      CALL RECHG(DGM1)
      TEM1=T
!-----
      TC=TVOR-2.0D0*DXT
      CALL SORTOFNURVONPT
      CALL RECHG(DGM2)
!=====
      SS0=-(DG1-DGM1)/(2.0D0*DXT)
      SS1=-(DG2-DG0)/(2.0D0*DXT)
      SSM1=-(DG0-DGM2)/(2.0D0*DXT)
      HH0=DG0+(TE0)*SS0
      HH1=DG1+(TE1)*SS1
      HHM1=DGM1+(TEM1)*SSM1
      LECP=(HH1-HHM1)/(2.0D0*DXT)
!=====
!==============================
      TC=TVOR
      CALL SORTOFNURVONPT
!*****
      RETURN
      END
!
!-----
!*************************************************************
!*************************************************************
      SUBROUTINE HMINH0(I,HH0)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!-----
      REAL(8) DG0,DG1,DGM1,TVOR,DXT,SS0, &
      HH0,HREF,TE0
      INTEGER(4) I,IPH
!-----
      DXT=300.0D0/1D5
      TVOR=TC
      IPH=I
!-----
      TC=25.0D0
      CALL SORTOFNURVONPT
      CALL RECHG2(IPH,DG0)
      TC=25.0D0+DXT
      CALL SORTOFNURVONPT
      CALL RECHG2(IPH,DG1)
      TC=25.0D0-DXT
      CALL SORTOFNURVONPT
      CALL RECHG2(IPH,DGM1)
      SS0=-(DG1-DGM1)/(2.0D0*DXT)
      HREF=DG0+298.15D0*SS0
!=====
      TC=TVOR
      CALL SORTOFNURVONPT
      CALL RECHG2(IPH,DG0)
      TE0=T
!-----
      TC=TVOR+DXT
      CALL SORTOFNURVONPT
      CALL RECHG2(IPH,DG1)
!-----
      TC=TVOR-DXT
      CALL SORTOFNURVONPT
      CALL RECHG2(IPH,DGM1)
!=====
      SS0=-(DG1-DGM1)/(2.0D0*DXT)
      HH0=DG0+(TE0)*SS0-HREF
      HH0=HH0/NN(I)
!=====
!==============================
      TC=TVOR
      CALL SORTOFNURVONPT
!*****
      RETURN
      END
!
!-----
!*************************************************************
!*************************************************************
      SUBROUTINE HMINH0TOT(HH0)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!-----
      REAL(8) DG0,DG1,DGM1,TVOR,DXT,SS0, &
      HH0,HREF,TE0
!-----
      DXT=300.0D0/1D5
      TVOR=TC
!-----
      TC=25.0D0
      CALL SORTOFNURVONPT
      CALL RECHG(DG0)
      TC=25.0D0+DXT
      CALL SORTOFNURVONPT
      CALL RECHG(DG1)
      TC=25.0D0-DXT
      CALL SORTOFNURVONPT
      CALL RECHG(DGM1)
      SS0=-(DG1-DGM1)/(2.0D0*DXT)
      HREF=DG0+298.15D0*SS0
!=====
      TC=TVOR
      CALL SORTOFNURVONPT
      CALL RECHG(DG0)
      TE0=T
!-----
      TC=TVOR+DXT
      CALL SORTOFNURVONPT
      CALL RECHG(DG1)
!-----
      TC=TVOR-DXT
      CALL SORTOFNURVONPT
      CALL RECHG(DGM1)
!=====
      SS0=-(DG1-DGM1)/(2.0D0*DXT)
      HH0=DG0+(TE0)*SS0-HREF
!=====
!==============================
      TC=TVOR
      CALL SORTOFNURVONPT
!*****
      RETURN
      END
!
!-----
!*************************************************************
!*************************************************************
      SUBROUTINE GETNAME(I,IS,TEXT)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!
!-----END OF COMMON VARIABLES
      CHARACTER*(*) TEXT
      INTEGER(4) I,IS
!----
      IF (NUMMER(I).LE.0) THEN
      TEXT=SOLNAM(EMCODE(I))
      IS=EMCODE(I)
      ELSE
      TEXT=NAME(NUMMER(I))
      IS=0
      END IF
!----
      RETURN
      END
!
!!-----
!!******************************
!      SUBROUTINE DSDVTEST5
!      IMPLICIT NONE
!      INCLUDE 'theriak.cmn'
!      include 'files.cmn'
!!-----
!      REAL(8) DG0,DG1,DG2,DGM1,DGM2,TVOR,PVOR,DXT,DXP,SS0,SS1,SSM1, &
!      TE0,TE1,TE2,TEM1,TEM2,HH0,HH1,HHM1,LECP
!!-----
!      DXP=P/1D5
!      DXT=TC/1D5
!      TVOR=TC
!      PVOR=P
!!-----
!      CALL RECHG(DG0)
!      TE0=T
!!-----
!      TC=TVOR+DXT
!      CALL SORTOFNURVONPT
!      CALL RECHG(DG1)
!      TE1=T
!!-----
!      TC=TVOR+2.0D0*DXT
!      CALL SORTOFNURVONPT
!      CALL RECHG(DG2)
!      TE2=T
!!-----
!      TC=TVOR-DXT
!      CALL SORTOFNURVONPT
!      CALL RECHG(DGM1)
!      TEM1=T
!!-----
!      TC=TVOR-2.0D0*DXT
!      CALL SORTOFNURVONPT
!      CALL RECHG(DGM2)
!      TEM2=T
!!=====
!      WRITE (UNIT=6,FMT='(/''using 5 points'')')
!      SS0=-(DG1-DGM1)/(2.0D0*DXT)
!      WRITE (UNIT=6,FMT='(''ss0 = '',F20.8)') SS0
!      SS1=-(DG2-DG0)/(2.0D0*DXT)
!      WRITE (UNIT=6,FMT='(''ss1 = '',F20.8)') SS1
!      SSM1=-(DG0-DGM2)/(2.0D0*DXT)
!      WRITE (UNIT=6,FMT='(''ssm1= '',F20.8)') SSM1
!      HH0=DG0+T*SS0
!      WRITE (UNIT=6,FMT='(''hh0 = '',F20.8)') HH0
!      HH1=DG1+TE1*SS1
!      WRITE (UNIT=6,FMT='(''hh1 = '',F20.8)') HH1
!      HHM1=DGM1+TEM1*SSM1
!      WRITE (UNIT=6,FMT='(''hhm1= '',F20.8)') HHM1
!      LECP=(HH1-HHM1)/(2.0D0*DXT)
!      WRITE (UNIT=6,FMT='(''lecp= '',F20.8)') LECP      
!!=====
!      TC=TVOR
!      P=P+DXP
!      CALL SORTOFNURVONPT
!!=====
!!==============================
!      TC=TVOR
!      P=PVOR
!      CALL SORTOFNURVONPT
!!*****
!      RETURN
!      END
!!-----
!!******************************
!      SUBROUTINE DSDVTEST3
!      IMPLICIT NONE
!      INCLUDE 'theriak.cmn'
!      include 'files.cmn'
!!-----
!      REAL(8) DG0,DG1,DG2,DGM1,DGM2,TVOR,PVOR,DXT,DXP,SS0,SS1,SSM1, &
!      TE0,TE1,TE2,TEM1,TEM2,HH0,HH1,HHM1,LECP
!!-----
!      DXP=P/1D5
!      DXT=TC/1D5
!      TVOR=TC
!      PVOR=P
!!-----
!      CALL RECHG(DG0)
!      TE0=T
!!-----
!      TC=TVOR+DXT
!      CALL SORTOFNURVONPT
!      CALL RECHG(DG1)
!      TE1=T
!!-----
!      TC=TVOR+2.0D0*DXT
!      CALL SORTOFNURVONPT
!      CALL RECHG(DG2)
!      TE2=T
!!-----
!      TC=TVOR-DXT
!      CALL SORTOFNURVONPT
!      CALL RECHG(DGM1)
!      TEM1=T
!!-----
!      TC=TVOR-2.0D0*DXT
!      CALL SORTOFNURVONPT
!      CALL RECHG(DGM2)
!      TEM2=T
!!=====
!      WRITE (UNIT=6,FMT='(/''using 3 points'')')
!      SS0=-(DG1-DGM1)/(2.0D0*DXT)
!      WRITE (UNIT=6,FMT='(''ss0 = '',F20.8)') SS0
!      SS1=-(DG1-DG0)/(DXT)
!      WRITE (UNIT=6,FMT='(''ss1 = '',F20.8)') SS1
!      SSM1=-(DG0-DGM1)/(DXT)
!      WRITE (UNIT=6,FMT='(''ssm1= '',F20.8)') SSM1
!      HH0=DG0+T*SS0
!      WRITE (UNIT=6,FMT='(''hh0 = '',F20.8)') HH0
!      HH1=DG1+(TE1)*SS1
!      WRITE (UNIT=6,FMT='(''hh1 = '',F20.8)') HH1
!      HHM1=DGM1+(TEM1)*SSM1
!      WRITE (UNIT=6,FMT='(''hhm1= '',F20.8)') HHM1
!      LECP=(HH1-HHM1)/(DXT)
!      WRITE (UNIT=6,FMT='(''lecp= '',F20.8)') LECP      
!!=====
!      TC=TVOR
!      P=P+DXP
!      CALL SORTOFNURVONPT
!!=====
!!==============================
!      TC=TVOR
!      P=PVOR
!      CALL SORTOFNURVONPT
!!*****
!      RETURN
!      END
!-----
!******************************
      SUBROUTINE DSDV
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!-----
      CHARACTER(32) CH16
      REAL(8) X16,DG1,DG2,TVOR,PVOR,DXT,DXP,HH1,KUH,PW,OU
!---- calculates G_tot,S_tot,V_tot,H_tot,TS_tot,PV-_tot,U_tot
!---- for use with PRTLOG(9) table
      DXP=P/1D5
      DXT=TC/1D5
!-----
      CALL RECHG(DG1)
      CH16='G_tot'
      X16=DG1
      CALL SETTBL(CH16,X16)
      HH1=DG1
      OU=DG1
!===== calculate S_tot and H_tot (=HH1)
      TVOR=TC
      TC=TC+DXT
      CALL SORTOFNURVONPT
!=====
      CALL RECHG(DG2)
      CH16='S_tot'
      X16=-(DG2-DG1)/DXT
      CALL SETTBL(CH16,X16)
      HH1=HH1+X16*T
      KUH=X16*T
      OU=OU+X16*T
!===== calculate V_tot
      TC=TVOR
      PVOR=P
      P=P+DXP
      CALL SORTOFNURVONPT
!=====
      CALL RECHG(DG2)
      CH16='V_tot'
      X16=(DG2-DG1)/DXP*10.0D0
      CALL SETTBL(CH16,X16)
      PW=-X16/10.0D0*P
      OU=OU-X16/10.0D0*P
!==============================
      TC=TVOR
      P=PVOR
      CALL SORTOFNURVONPT
!=====
      CH16='H_tot'
      X16=HH1
      CALL SETTBL(CH16,X16)
!=====
      CH16='TS_tot'
      X16=KUH
      CALL SETTBL(CH16,X16)
!=====
      CH16='PV-_tot'
      X16=PW
      CALL SETTBL(CH16,X16)
!=====
      CH16='U_tot'
      X16=OU
      CALL SETTBL(CH16,X16)
!*****
      RETURN
      END
!-----
!******************************
      SUBROUTINE DSDV10
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!
!      for pixelmaps
!-----
      CHARACTER(32) CH16
      REAL(8) X16,DG1,DG2,TVOR,PVOR,DXT,DXP,HHH,KUH,PW,OU
!-----
      DXT=1D-5
      DXP=1D-4
!-----
      CALL RECHG(DG1)
      CH16='G_tot'
      X16=DG1
      CALL SETMAP(CH16,X16)
      HHH=DG1
      OU=DG1
!=====
      TVOR=TC
      TC=TC+DXT
      CALL SORTOFNURVONPT
!=====
      CALL RECHG(DG2)
      CH16='S_tot'
      X16=-(DG2-DG1)/DXT
      CALL SETMAP(CH16,X16)
      HHH=HHH+X16*T
      KUH=X16*T
      OU=OU+X16*T
!=====
      TC=TVOR
      PVOR=P
      P=P+DXP
      CALL SORTOFNURVONPT
!=====
      CALL RECHG(DG2)
      CH16='V_tot'
      X16=(DG2-DG1)/DXP*10.0D0
      CALL SETMAP(CH16,X16)
      PW=-X16/10.0D0*P
      OU=OU-X16/10.0D0*P
!=====
      TC=TVOR
      P=PVOR
      CALL SORTOFNURVONPT
!=====
      CH16='H_tot'
      X16=HHH
      CALL SETMAP(CH16,X16)
!=====
      CH16='TS_tot'
      X16=KUH
      CALL SETMAP(CH16,X16)
!=====
      CH16='PV-_tot'
      X16=PW
      CALL SETMAP(CH16,X16)
!=====
      CH16='U_tot'
      X16=OU
      CALL SETMAP(CH16,X16)
!*****
      RETURN
      END
!-----
!******************************
      SUBROUTINE SORTOFNURVONPT
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
!-----END OF COMMON VARIABLES
      INTEGER(4) I001,I,II,IS
!-----
      T=TC+273.15D0
!-----
      PGAS=P*PRAT
      RT=R*T
      TT=T*T
      SQT=DSQRT(T)
      GR=0.0D0
      VOLUM=0.0D0
      DO II=1,NUN
!!        GG(II)=0.0D0
        GGK(II)=0.0D0
      END DO
!+++++
      DO 610,IS=1,NSOL
!+++++
      IF (LAAR(IS)) THEN
      DO II=1,NEND(IS)
        VLAA(IS,II)=VLAA0(IS,II)+VLAAT(IS,II)*T+VLAAP(IS,II)*P
      END DO
      END IF
!+++++
      DO I=1,NMARG(IS)
      WG(IS,I)=WH(IS,I)+WCP(IS,I)*(T-T0) &
      -(WS(IS,I)+DLOG(T/T0)*WCP(IS,I))*T+WV(IS,I)*P
      END DO
!
      DO II=1,NSMARG(IS)
      SWG(IS,II)=SWH(IS,II)-SWS(IS,II)*T+SWV(IS,II)*P
      END DO
!
  610 CONTINUE
!+++++
      DO 600,II=(NUN+1),NPHA
      I001=II
      CALL DAREST(I001)
      NAM=NAME(II)
      IF (SPC) THEN
      CALL GSPEC(NAM,P,PGAS,T,FALL,GR,VOLUM)
      ELSE
      LIQ=.FALSE.
      CALL GCALC(I001)
      END IF
      IF (COM) THEN
      DO I=1,NCOM
        GR=GR+FFCOM(I)*GGK(ICOM(I))
        VOLUM=VOLUM+FFCOM(I)*VV(ICOM(I))
      END DO
      END IF
      GGK(II)=GR
!!      GG(II)=GR
      VV(I001)=VOLUM
  600 CONTINUE
      RETURN
      END
!-----
!********************************
      SUBROUTINE SETTBL(CH16,X16)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!-----
      CHARACTER(32) CH16
      REAL(8) X16
!-----END OF VARIABLES FOR MEGAMAT
      INTEGER(4) I
      LOGICAL(4) NEW
!-----
      NEW=.TRUE.
      DO I=1,NVARTBL
        IF (CH16.EQ.VARTBL(I)) THEN
!DC        OUTTBL(NROWTBL,I)=OUTTBL(NROWTBL,I)+X16
        OUTTBL(NROWTBL,I)=X16
!
        NEW=.FALSE.
        GOTO 501
        END IF
      END DO
  501 CONTINUE
      IF (NEW) THEN
      NVARTBL=NVARTBL+1
      IF (NVARTBL.GT.MAXVARTBL) THEN
      CALL SHOUTD
      WRITE (6,1000) NVARTBL
      WRITE (out,1000) NVARTBL
 1000 FORMAT (// &
      ' The number of variables reached: ',I5/ &
      ' possible action:'/ &
      '   a) Increase the maximum value'/ &
      '   b) dont store all variables')
!---- PRTLOG(9): print table
!---- PRTLOG(11): print short table
      PRTLOG(9)=.FALSE.
      PRTLOG(11)=.FALSE.
      NVARTBL=NVARTBL-1
      RETURN
!C      STOP
      END IF
!
      VARTBL(NVARTBL)=CH16
      OUTTBL(NROWTBL,NVARTBL)=X16
!
      DO I=1,NROWTBL-1
        OUTTBL(I,NVARTBL)=0.0D0
      END DO
      END IF
      RETURN
      END
!-----
!********************************
      SUBROUTINE SETMAP(CH16,X16)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!-----
      CHARACTER(32) CH16
      REAL(8) X16
      INTEGER(4) I,j,ierr
      LOGICAL(4) NEW
!-----
      NEW=.TRUE.
      DO I=1,NVARTBL
        IF (CH16.EQ.VARTBL(I)) THEN
        NEW=.FALSE.
        GOTO 501
        END IF
      END DO
  501 CONTINUE
!---
      CALL LABLA(CH16,I)
      IF (NEW) THEN
      NVARTBL=NVARTBL+1
      IF (NVARTBL.GT.MAXVARTBL) THEN
      CALL SHOUTD
      WRITE (6,1000) NVARTBL
      WRITE (out,1000) NVARTBL
 1000 FORMAT (// &
      ' The number of variables reached: ',I5/ &
      ' possible action:'/ &
      '   a) Increase the maximum value'/ &
      '   b) dont store all variables')
!---- PRTLOG(10): pixelmaps
      PRTLOG(10)=.FALSE.
!C      STOP
      END IF
      VARTBL(NVARTBL)=CH16
!------------------
!     Open UNIT=fig
!------------------
      j=fig
      line=filename(fig)(1:fnl(fig))//CH16
!      line='pixelmaps/'//CH16
      path=wpath
      akzess=' '
      state=' '
      ierr=0
      call openfile(j,ierr)
      if(ierr.ne.0) STOP
!
      WRITE (UNIT=fig,FMT='(I6,2X,1PE20.12)') NROWTBL,X16
      CLOSE (UNIT=fig)
      ELSE
!------------------
!     Open UNIT=fig
!------------------
      j=fig
      line=filename(fig)(1:fnl(fig))//CH16
!      line='pixelmaps/'//CH16
      path=wpath
      akzess='append'
      state='old'
      ierr=0
      call openfile(j,ierr)
      if(ierr.ne.0) STOP
!
      WRITE (UNIT=fig,FMT='(I6,2X,1PE20.12)') NROWTBL,X16
      CLOSE (UNIT=fig)
      END IF
      RETURN
      END
!********************************
      SUBROUTINE PRTTBL
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!
!-----
      INTEGER(4) II,IB,I0,I1,I2,ierr,j
      CHARACTER(500) CH500
!     CHARACTER*1 TCH
!-----TCH is the tab character
!     CH=CHAR(9)
!------------------
!     Open UNIT=tab
!------------------
      j=tab
      line=filename(j)(1:fnl(j))//ext(j)
      path=wpath
      akzess=' '
      state=' '
      ierr=0
      call openfile(j,ierr)
      if(ierr.ne.0) STOP
!-----
      WRITE (tab,1000) (VARTBL(II),II=1,NVARTBL)
 1000 FORMAT (500(A32,:,','))
!-
      WRITE (6,1002) NVARTBL,NROWTBL
      WRITE (out,1002) NVARTBL,NROWTBL
 1002 FORMAT (' columns =',I5,2X,'rows =',I5)
!-
      DO IB=1,NROWTBL
        WRITE (tab,1010) (OUTTBL(IB,II),II=1,NVARTBL)
 1010 FORMAT (500(1PE14.7,:,','))
      END DO
      CLOSE (UNIT=tab)
!------------------
!     Open UNIT=tcp
!------------------
      j=tcp
      line=filename(j)(1:fnl(j))//ext(j)
      path=wpath
      akzess=' '
      state=' '
      ierr=0
      call openfile(j,ierr)
      if(ierr.ne.0) STOP
!-----
      WRITE (tcp, '(a)') 'TITLE = "PT LOOP"'
      CH500='VARIABLES = '
      I0=13
      DO I1=1,NVARTBL
        CALL LABLA(VARTBL(I1),I2)
        CH500(I0:)='"'//VARTBL(I1)(1:I2)//'",'
        I0=I0+I2+3
        IF (I0.GT.200) THEN
        CALL PUST(tcp,CH500)
        CH500=' '
        I0=1
        END IF
      END DO
      IF (I0.GT.1) CH500(I0-1:)=' '
      CALL PUST(tcp,CH500)
      WRITE (tcp,*) 'ZONE I=',NROWTBL,', F=POINT'
      DO IB=1,NROWTBL
        WRITE (tcp,1011) (OUTTBL(IB,II),II=1,NVARTBL)
 1011 FORMAT (501(1PE16.8))
      END DO
      CLOSE (UNIT=tcp)
!-----
      RETURN
      END
!-----
!******************************
      SUBROUTINE MINIREA(NP)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!-----END OF COMMON VARIABLES
      INTEGER(4) NP,NCOL,NROW,IR,RANG,I1,IP,IE,IS,SCHON
      INTEGER(4) COLNR(COMAX),CONR(50)
      REAL(8) AA(50,40)
      CHARACTER(16) TEXT,HEAD(50)
!-----
      NCOL=0
      NROW=NUN
      DO IP=1,NUN2
        IF (NUMMER(IP).GT.0) THEN
          SCHON=0
          TEXT=NAME(NUMMER(IP))
          DO I1=1,NCOL
            IF (TEXT.EQ.HEAD(I1)) SCHON=1
          END DO
          IF (SCHON.EQ.0) THEN
            NCOL=NCOL+1
            HEAD(NCOL)=NAME(NUMMER(IP))
            COLNR(NCOL)=NCOL
            CONR(NCOL)=NCOL
            DO IR=1,NROW
              AA(IR,NCOL)=XX(NUMMER(IP),IR)
            END DO
          END IF
        ELSE
          IS=EMCODE(IP)
          DO IE=1,NEND(IS)
            SCHON=0
            TEXT=NAME(EM(IS,IE))
            DO I1=1,NCOL
              IF (TEXT.EQ.HEAD(I1)) SCHON=1
            END DO
            IF (SCHON.EQ.0) THEN
              NCOL=NCOL+1
              HEAD(NCOL)=NAME(EM(IS,IE))
              COLNR(NCOL)=NCOL
              CONR(NCOL)=NCOL
              DO IR=1,NUN
                AA(IR,NCOL)=XX(EM(IS,IE),IR)
              END DO
            END IF
          END DO
        END IF
      END DO
!-----
      NCOL=NCOL+1
      HEAD(NCOL)='PHASE_X'
      COLNR(NCOL)=NCOL
      DO IR=1,NROW
       AA(IR,NCOL)=X(NP,IR)
      END DO
!-----
      CALL MINIMAT2(AA,NCOL,NROW,CONR,RANG,HEAD)
!.....
!..... cdc test
!      WRITE (scr,2012) (HEAD(IC),IC=1,NCOL)
!      WRITE (out,2012) (HEAD(IC),IC=1,NCOL)
! 2012 FORMAT (25A14)
!      DO IR=1,NROW
!        WRITE (scr,2010) (AA(IR,IC),IC=1,NCOL)
!        WRITE (out,2010) (AA(IR,IC),IC=1,NCOL)
! 2010 FORMAT(25F14.4)
!      END DO
!.....
!.....
      I1=0
      DO IR=RANG+1,NROW
       IF (DABS(AA(IR,NCOL)).GT.1D-10) I1=1
      END DO
!.....
!..... cdc test
!      WRITE (UNIT=scr,FMT='(90X,''rank: '',I4)') RANG
!      WRITE (UNIT=out,FMT='(90X,''rank: '',I4)') RANG
!.....
!.....
      IF (I1.EQ.0) THEN
      WRITE (scr,1000) 
      WRITE (out,1000) 
 1000 FORMAT (90X,'check: OK')
      ELSE
      WRITE (scr,2000)
      WRITE (out,2000)
 2000 FORMAT (90X,'check: may not be buffered')
      END IF
!.....
!..... cdc test (eigentlich IP=1,RANG, die letzten sollten=0 sein)
!      DO IP=1,NROW
!      WRITE (UNIT=scr,FMT='(90X,A16,2X,1PE14.7)') HEAD(IP),AA(IP,NCOL)
!      WRITE (UNIT=out,FMT='(90X,A16,2X,1PE14.7)') HEAD(IP),AA(IP,NCOL)
!      END DO
!.....
!.....
!*****
      RETURN
      END
!-----
!******************************
      SUBROUTINE MINIMAT2(AA,NCOL,NROW,CONR,RANG,HEAD)
      IMPLICIT NONE
      include 'files.cmn'
!-----minimat variables 
      REAL(8) AA(50,40)
      INTEGER(4) NCOL,NROW,RANG,CONR(50)
      CHARACTER(16) TEXT,HEAD(50)
!----
      INTEGER(4) PIV
      REAL(8) COLSUM(40),WERT,DET
!----
      INTEGER(4) IR,IC,IX,IY,I,II,I2,IJ
      REAL(8) FF,F1,FFF(50)
      RANG=0
!.....
!      WRITE (scr,1012) (HEAD(IC),IC=1,NCOL)
!      WRITE (out,1012) (HEAD(IC),IC=1,NCOL)
! 1012 FORMAT (25A14)
!      DO IR=1,NROW
!        WRITE (scr,1010) (AA(IR,IC),IC=1,NCOL)
!        WRITE (out,1010) (AA(IR,IC),IC=1,NCOL)
! 1010 FORMAT(25F14.4)
!      END DO
!.....
      DO 600,PIV=1,MIN(NROW,NCOL-1)
      DO IC=PIV,NCOL-1
        COLSUM(IC)=0.0D0
        DO IR=1,NROW
          COLSUM(IC)=COLSUM(IC)+DABS(AA(IR,IC))
        END DO
      END DO
      IY=0
      IX=0
      WERT=0.0D0
      DO IR=PIV,NROW
        DO IC=PIV,NCOL-1
          IF (COLSUM(IC).LE.0.0D0) GOTO 609
          FF=DABS(AA(IR,IC)/COLSUM(IC))
          IF (FF.GT.WERT) THEN
          WERT=FF
          IX=IC
          IY=IR
          END IF
  609     CONTINUE
        END DO
        IF (WERT.NE.0.0D0) GOTO 611
      END DO
  611 CONTINUE
      IF (IX.EQ.0.OR.IY.EQ.0) RETURN
      IF (IX.NE.PIV) THEN
!----colchg
!     CALL COLCHG(IX,PIV)
      IJ=CONR(IX)
      TEXT=HEAD(IX)
      HEAD(IX)=HEAD(PIV)
      HEAD(PIV)=TEXT
      DO I=1,NROW
        FFF(I)=AA(I,IX)
      END DO
      CONR(IX)=CONR(PIV)
      DO I=1,NROW
        AA(I,IX)=AA(I,PIV)
      END DO
      CONR(PIV)=IJ
      DO I=1,NROW
        AA(I,PIV)=FFF(I)
      END DO
!----
      END IF
      IF (IY.NE.PIV) THEN
!----rowchg
!     CALL ROWCHG(IY,PIV)
      DO I=1,NCOL
        FFF(I)=AA(IY,I)
      END DO
      DO I=1,NCOL
        AA(IY,I)=AA(PIV,I)
      END DO
      DO I=1,NCOL
        AA(PIV,I)=FFF(I)
      END DO
!----
      END IF
!----
      FF=AA(PIV,PIV)
!     WRITE (UNIT=6,FMT='(''FF= '',F10.4)') FF
      RANG=PIV
      IF (PIV.EQ.1) THEN 
      DET=FF
      ELSE
      DET=DET*FF
      END IF
!----divrow
!     CALL DIVROW(PIV,FF)
      DO I=1,NCOL
        AA(PIV,I)=AA(PIV,I)/FF
      END DO
!----
      DO 500,II=1,NROW
      IF (II.NE.PIV) THEN
      F1=AA(II,PIV)
      I2=II
!----substr
!     CALL SUBSTR(PIV,F1,I2)
      DO 710,I=1,NCOL
      AA(I2,I)=AA(I2,I)-AA(PIV,I)*F1
      IF (DABS(AA(I2,I)).LE.1.0D-10) AA(I2,I)=0.0D0
  710 CONTINUE
!----
      END IF
  500 CONTINUE
  600 CONTINUE
!-----
      RETURN
      END
!-----
!*************************************************************
!*************************************************************
      SUBROUTINE BUBU(CH001)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!      include 'checkdb.cmn'
!
      CHARACTER*(*) CH001
      CHARACTER(20) CH20
      INTEGER(4) I1,I2,I,J,I3
      CH001=' '
      DO I=1,NUN
      IF (BULK(I).NE.0.0D0) THEN
      WRITE (UNIT=CH20,FMT='(F20.10)') BULK(I)
!---
      CALL ZAHL(CH20,I1,I2)
!---
      CALL LABLA(CH001,I3)
      WRITE (UNIT=CH001(I3+1:),FMT='(A2,''('',A,'')'')') &
      OXYDE(CHMCOD(I)),CH20(I1:I2)
      END IF
      END DO
      CALL COLLAPS(CH001,J)
!      CALL PUST(6,'BUBU '//CH001)
!
      RETURN
      END
!-----
!*************************************************************
!*************************************************************
