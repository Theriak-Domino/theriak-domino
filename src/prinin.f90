!-----Version: 28.05.2022
!               ************
!               * prinin.f *
!               ************
!     Subroutines for printing a summary of the input
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
      SUBROUTINE PRININ
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!-----END OF COMMON VARIABLES
      INTEGER(4) K,I,II,IE,IS,IL00,IL02,N,I001,I002,I003,IM,IX0,JX,ISPR, &
      ISOL,IX1,J3,CDCTEST,IP,I1,I2
      REAL(8) NNPC,SUMME,FF,FFF
      CHARACTER(250) CH001
      CHARACTER(500) MODEL1,MODEL2,MODEL3
      CHARACTER(20) MODEL0
!------
!---- CDCTEST=1: printing additional informaion on solutions
      CDCTEST=0
      IF (DRU) THEN
      WRITE (UNIT=scr,FMT=110) P,PGAS,TC,T
      WRITE (UNIT=out,FMT=110) P,PGAS,TC,T
  110 FORMAT (/' P =',F9.2,' bar    P(Gas) =',F9.2,' bar    T =', &
      F8.2,' C   = ',F8.2,' K')
      END IF
      IF (PRTLOG(2)) THEN
      WRITE (UNIT=scr,FMT=112)
      WRITE (UNIT=out,FMT=112)
  112 FORMAT(/ &
      ' ------------'/ &
      ' composition:',8X,'N',11X,'N',13X,'mol%'/ &
      ' ------------'/' ')
      SUMME=0.0D0
      DO I=1,NUN
        SUMME=SUMME+DABS(NN(I))
      END DO
      DO 541,I=1,NUN
      NNPC=(NN(I)/SUMME)*100.0D0
      WRITE (UNIT=scr,FMT=114) I,CHNAME(I),NN(I),NN(I),NNPC
      WRITE (UNIT=out,FMT=114) I,CHNAME(I),NN(I),NN(I),NNPC
  114 FORMAT (1X,I3,2X,A8,1X,F11.6,2X,1PE12.5,2X,0PF11.6)
!dC   IF (CHNAME(I)(5:6).EQ.'  ') THEN
!dC   CH=CHNAME(I)
!dC   CHNAME(I)=' '//CH
!dC   END IF
  541 CONTINUE
      END IF
      IF (PRTLOG(3)) THEN
!==
      IF (NPICK.GT.0) THEN
      CH001='use:'
      I1=7
      DO I=1,NPICK
      CH001(I1:)=PICK(I)
      CALL LABLA(CH001,I2)
      I1=I2+3
      END DO
      WRITE (UNIT=scr,FMT='('' '')')
      WRITE (UNIT=out,FMT='('' '')')
      CALL PUSTCOL(scr,CH001,2,132)
      CALL PUSTCOL(out,CH001,2,132)
      CH001=' '
      END IF
!==
      I001=MIN0(15,NUN)
      WRITE (UNIT=scr,FMT=116)
      WRITE (UNIT=out,FMT=116)
  116 FORMAT(/ &
      ' ------------------'/ &
      ' considered phases:'/ &
      ' ------------------')
      WRITE (UNIT=scr,FMT=118) (CHNAME(I),I=1,I001)
      WRITE (UNIT=out,FMT=118) (CHNAME(I),I=1,I001)
  118 FORMAT (35X,'G',7X,15(A5,1X))
      DO I001=16,NUN,15
        WRITE (UNIT=out,FMT=120) (CHNAME(I),I=I001,MIN0(I001+14,NUN))
        WRITE (UNIT=scr,FMT=120) (CHNAME(I),I=I001,MIN0(I001+14,NUN))
      END DO
  120 FORMAT (42X,15(A5,1X))
      WRITE (UNIT=scr,FMT='('' '')')
      WRITE (UNIT=out,FMT='('' '')')
      DO 545,K=1,NPHA
      WRITE (UNIT=CH001,FMT=122) K,NAME(K),GG(K)
  122 FORMAT (1X,I3,2X,A16,1X,':',2X,F13.2)
      DO I001=1,NUN,15
        DO I=I001,MIN0(I001+14,NUN)
          I002=42+MOD(I-1,15)*6
          IF (DABS(XX(K,I)).LT.1D-20) THEN
          WRITE (UNIT=CH001(I002+3:),FMT='(''-'')')
          ELSE
          WRITE (UNIT=CH001(I002:),FMT='(F6.2)') XX(K,I)
          END IF
        END DO
        WRITE (UNIT=scr,FMT='(A133)') CH001
        WRITE (UNIT=out,FMT='(A133)') CH001
        CH001=' '
      END DO
  545 CONTINUE
      END IF
      IF (PRTLOG(4)) THEN
      IF (NSOL.NE.0) THEN
      WRITE (UNIT=scr,FMT=130)
      WRITE (UNIT=out,FMT=130)
  130 FORMAT (/ &
      ' ----------------'/ &
      ' solution phases:'/ &
      ' ----------------')
      END IF
!-----
      ISPR=1
      IF (PRTLOG(1)) ISPR=NSOL
      DO 600,IS=ISPR,NSOL
      WRITE (UNIT=CH001,FMT=132) IS,SOLNAM(IS)
  132 FORMAT (I3,2X,A16,' :',12X,'solution model: ')
      IF (MODELL(IS).EQ.'I') WRITE (UNIT=CH001(53:),FMT=134)
  134 FORMAT ('ideal one site mixing')
      IF (MODELL(IS).EQ.'S') WRITE (UNIT=CH001(53:),FMT=136) NSITE(IS)
  136 FORMAT ('"ideal"',I3,' site mixing')
      IF (MODELL(IS).EQ.'F') WRITE (UNIT=CH001(53:),FMT=138)
  138 FORMAT ('from external subroutine')
      IF (NMARG(IS).NE.0) THEN
      DO I001=250,1,-1
        IF (CH001(I001:I001).NE.' ') GO TO 20
      END DO
   20 WRITE (UNIT=CH001(I001+2:),FMT=140)
  140 FORMAT ('+ Margules type excess function')
      END IF
      WRITE (UNIT=scr,FMT='(/'' ---------------------'')')
      WRITE (UNIT=scr,FMT='(1X,A132)') CH001
      WRITE (UNIT=out,FMT='(1X,A132)') CH001
      WRITE (UNIT=scr,FMT='(36X,''info: '',A80)') SOLINFO(IS)
      WRITE (UNIT=out,FMT='(36X,''info: '',A80)') SOLINFO(IS)
!     CH=' '
!     I001=INDEX(SOLNAM(IS),'  ')
!     DO 551,I002=1,I001-1
! 551 CH(I002:I002)='-'
!     WRITE (UNIT=scr,FMT='(6X,A16)') CH
!     WRITE (UNIT=out,FMT='(6X,A16)') CH
!-----
      IF (NSITE(IS).GT.0) THEN
!-----
!-----
      IF (CDCTEST.EQ.1) THEN
      WRITE (6,1010) NSITE(IS)
      WRITE (out,1010) NSITE(IS)
 1010 FORMAT ('NSITE  ',10I10)
      WRITE (6,1011) (SITMUL(IS,IX1),IX1=1,NSITE(IS))
      WRITE (out,1011) (SITMUL(IS,IX1),IX1=1,NSITE(IS))
 1011 FORMAT ('SITMUL ',10F10.5)
      WRITE (6,1012) (CHSITE(IS,IX1),IX1=1,NSITE(IS))
      WRITE (out,1012) (CHSITE(IS,IX1),IX1=1,NSITE(IS))
 1012 FORMAT ('CHSITE ',10A10)
      WRITE (6,1013) (NELPS(IS,IX1),IX1=1,NSITE(IS))
      WRITE (out,1013) (NELPS(IS,IX1),IX1=1,NSITE(IS))
 1013 FORMAT ('NELPS  ',10I10)
      DO 810,IX1=1,NSITE(IS)
      WRITE (6,1014) (ELONS(IS,IX1,J3),J3=1,NELPS(IS,IX1))
      WRITE (out,1014) (ELONS(IS,IX1,J3),J3=1,NELPS(IS,IX1))
 1014 FORMAT ('ELONS  ',10A10)
  810 CONTINUE
!--
      DO IE=1,NEND(IS)
        WRITE (6,1015) NAME(EM(IS,IE))
        WRITE (out,1015) NAME(EM(IS,IE))
 1015 FORMAT ('  ',10A16)
        DO IX1=1,NSITE(IS)
          WRITE (6,1016) (ELSI(IS,IE,IX1,J3),J3=1,IDINT(SITMUL(IS,IX1)))
          WRITE (out,1016) (ELSI(IS,IE,IX1,J3),J3=1,IDINT(SITMUL(IS,IX1)))
 1016 FORMAT ('ELSI    ',10I10)
        END DO
      END DO
!--
      WRITE (6,1017) NSIEL(IS)
      WRITE (out,1017) NSIEL(IS)
 1017 FORMAT ('NSIEL  ',10I10)
      WRITE (6,1018) (SIEL(IS,IX1),IX1=1,NSIEL(IS))
      WRITE (out,1018) (SIEL(IS,IX1),IX1=1,NSIEL(IS))
 1018 FORMAT ('SIEL    ',10A10)
      WRITE (6,1019) (NEMQQ(IS,IX1),IX1=1,NSIEL(IS))
      WRITE (out,1019) (NEMQQ(IS,IX1),IX1=1,NSIEL(IS))
 1019 FORMAT ('NEMQQ   ',10I10)
!--
      DO 813,IX1=1,NSIEL(IS)
      WRITE (6,1025) (EMQQ(IS,IX1,J3),J3=1,NEMQQ(IS,IX1))
      WRITE (out,1025) (EMQQ(IS,IX1,J3),J3=1,NEMQQ(IS,IX1))
 1025 FORMAT ('EMQQ    ',10I10)
  813 CONTINUE
!--
      DO 814,IX1=1,NSIEL(IS)
!      WRITE (6,1020) NAME(EM(IS,IE))
!      WRITE (out,1020) NAME(EM(IS,IE))
! 1020 FORMAT ('  ',10A16)
      WRITE (6,1021) (EMXX(IS,IX1,IE),IE=1,NEND(IS))
      WRITE (out,1021) (EMXX(IS,IX1,IE),IE=1,NEND(IS))
 1021 FORMAT ('EMXX    ',10F10.5)
  814 CONTINUE
!
      DO IE=1,NEND(IS)
      WRITE (6,1023) NAME(EM(IS,IE))
 1023 FORMAT (A)
      DO II=1,NSITE(IS)
      WRITE (6,1022) CHSITE(IS,II), &
      (SIFEL(IS,II,K,IE),K=1,NELPS(IS,II))
 1022 FORMAT (A8,'  SIFEL ',10F10.5)
      END DO
      END DO
!
      END IF
!-----
!-----
      DO 510,IE=1,NEND(IS)
      MODEL1='     '//NAME(EM(IS,IE))
      I001=0
      IL02=0
      DO 512,II=1,NSITE(IS)
      I001=MAX0(5-MOD(IL02+2,5)+IL02+2,25)
      IF (I001.GT.100) THEN
      WRITE (UNIT=scr,FMT='(1X,A132)') MODEL1
      WRITE (UNIT=out,FMT='(1X,A132)') MODEL1
      MODEL1=' '
      I001=25
      END IF
      MODEL1(I001:)='['//CHSITE(IS,II)
      CALL LABLA(MODEL1,IL02)
      MODEL1(IL02+1:IL02+2)=']:'
      IL02=IL02+2
      DO 514,IM=1,IDINT(SITMUL(IS,II))
      MODEL1(IL02+1:)=ELONS(IS,II,ELSI(IS,IE,II,IM))
      CALL LABLA(MODEL1,IL02)
      MODEL1(IL02+1:IL02+1)=','
      IL02=IL02+1
  514 CONTINUE
      MODEL1(IL02:IL02)=' '
      IL02=IL02-1
  512 CONTINUE
      WRITE (UNIT=scr,FMT='(1X,A132)') MODEL1
      WRITE (UNIT=out,FMT='(1X,A132)') MODEL1
  510 CONTINUE
      END IF
!===== for each endmember
      FFF=1.0D0
      DO 599,IE=1,NEND(IS)
      MODEL1=' '
      MODEL2=' '
      MODEL0='A('//ABK(EM(IS,IE))
      I001=INDEX(MODEL0,'  ')
      MODEL0(I001:)=') ='
      IL00=I001+2
!-----
!----- ideal solutions
      IF (MODELL(IS).EQ.'I') THEN
      MODEL2='X('//MODEL0(3:IL00-2)
      IL02=IL00-2
!     WRITE (UNIT=CH,FMT='(F15.0)') ALPHA0(IS)
!     CALL FIBLA(CH,I001)
!     I002=INDEX(CH,'.')
!     CH(I002:)=ALPDIV(IS)
!     I002=I002+INDEX(ALPDIV(IS),' ')-1
!     IF (CH(I001-1:I002).NE.' 1 ') THEN
!     MODEL1(IL02+1:)=CH(I001:I002-1)
!     IL02=IL02+(I002-I001)
!     END IF
      IF ((DABS(ALPHA(IS))-1.0D0).GT.1D-20) THEN
      CH=' '
      WRITE (UNIT=CH,FMT='(F15.3)') ALPHA(IS)
      CALL FIBLA(CH,I001)
      CALL LABLA(CH,I002)
      MODEL2(IL02+1:)=' ) ** '//CH(I001:I002)
      IL02=IL02+(I002-I001+7)
      END IF
      END IF
!-----
!-----Achtung: Wenn die Modelle lang werden ev. Array  und
!-----MODEL3 zu kurz! (Schreibt ueber die Grenzen und crash)
      IF (MODELL(IS).EQ.'S') THEN
      FF=1.0D0
      IL02=0
      IX0=0
      DO II=1,NSITE(IS)
        IF (II.GT.1) IX0=IX0+NELPS(IS,II-1)
        DO IM=1,IDINT(SITMUL(IS,II))
          JX=IX0+ELSI(IS,IE,II,IM)
          MODEL2(IL02+1:)=' X['//SIEL(IS,JX)
          CALL LABLA(MODEL2,IL02)
          MODEL2(IL02+1:IL02+3)='] *'
          IL02=IL02+3
          FF=FF/EMXX(IS,JX,IE)
          FFF=FFF/EMXX(IS,JX,IE)
        END DO
      END DO
      MODEL2(IL02:IL02)=' '
!     IL02=IL02-1
      MODEL3=MODEL2
      WRITE (UNIT=MODEL2,FMT='(F8.2)') FF
      MODEL2(9:)=' * '//MODEL3(1:489)
      CALL LABLA(MODEL2,IL02)
      IF ((DABS(ALPHA(IS))-1.0D0).GT.1D-20) THEN
      MODEL3=MODEL2
      CH=' '
      WRITE (UNIT=CH,FMT='(F15.3)') ALPHA(IS)
      CALL FIBLA(CH,I001)
      CALL LABLA(CH,I002)
      MODEL2='( '//MODEL3(1:IL02)//' ) ** '//CH(I001:I002)
      IL02=IL02+(I002-I001+9)
      END IF
      END IF
!-----
!     IF (MODELL(IS).EQ.'S') THEN
!     IL02=0
!     DO 552,I=1,NSITE(IS)
!     MODEL2(IL02+1:)='['
!     IL02=IL02+1
!     DO 553,II=1,NEQEM(IS,IE,I)
!     MODEL2(IL02+1:)=' X('//ABK(EM(IS,EQEM(IS,IE,I,II)))
!     I001=INDEX(MODEL2(IL02+1:),'  ')
!     IL02=IL02+I001-1
!     MODEL2(IL02+1:)=') +'
!     IL02=IL02+3
! 553 CONTINUE
!     MODEL2(IL02:)=']'
!     WRITE (UNIT=CH,FMT='(F16.0)') SITMUL(IS,I)
!     CALL FIBLA(CH,I001)
!     I002=INDEX(CH,'.')
!     CH(I002:)=ALPDIV(IS)
!     I002=I002+INDEX(ALPDIV(IS),' ')-1
!     IF (CH(I001-1:I002).NE.' 1 ') THEN
!     MODEL1(IL02+1:)=CH(I001:I002-1)
!     IL02=IL02+(I002-I001)+1
!     END IF
! 552 CONTINUE
!     END IF
!-----
      IF (MODELL(IS).EQ.'F') THEN
      DO I=1,NEMBAS(IS)
        I001=I
        IF (EMBCOD(IS,I).EQ.IE) GO TO 21
      END DO
   21 CONTINUE
      CALL SOLMOD(SOLNAM(IS),I001,MODEL2)
      CALL LABLA(MODEL2,IL02)
!!!      IF (ALPHA(IS).NE.1.0D0) THEN
      IF ((DABS(ALPHA(IS))-1.0D0).GT.1D-20) THEN
      MODEL3=MODEL2
      CH=' '
      WRITE (UNIT=CH,FMT='(F15.3)') ALPHA(IS)
      CALL FIBLA(CH,I001)
      CALL LABLA(CH,I002)
      MODEL2='( '//MODEL3(1:IL02)//' ) ** '//CH(I001:I002)
      IL02=IL02+(I002-I001+9)
      END IF
      END IF
!-
      DO 555,I=1,IL02,85
      K=MIN0(85,IL02-I+1)
      N=IL00+37
      CH001=' '
      WRITE (UNIT=CH001(N+1:),FMT='(A85)') MODEL1(I:)
      WRITE (UNIT=scr,FMT='(A133)') CH001
      WRITE (UNIT=out,FMT='(A133)') CH001
      CH001=' '
      IF (I.EQ.1) THEN
      WRITE (UNIT=CH001,FMT=145) IE,NAME(EM(IS,IE)),EM(IS,IE), &
      MODEL0(I:)
  145 FORMAT (6X,I3,2X,A16,1X,'(',I3,')',3X,A15)
      END IF
      WRITE (UNIT=CH001(N+1:),FMT='(A85)') MODEL2(I:)
      WRITE (UNIT=scr,FMT='(A133)') CH001
      WRITE (UNIT=out,FMT='(A133)') CH001
  555 CONTINUE
  599 CONTINUE
!=====
      IF ((DABS(FFF)-1.0D0).GT.1D-20) THEN
      WRITE (UNIT=scr,FMT=150)
      WRITE (UNIT=out,FMT=150)
  150 FORMAT (/,6X,'site mixing model includes "endmembers"', &
      ' with inhomogeneous sites:', &
      ' this may result in non-unique equilibria' &
      /' ')
      END IF
!----
      IF (NMARG(IS).GT.0) WRITE (UNIT=CH001,FMT=152)
  152 FORMAT (6X,'Margules parameters:')
      DO I001=1,NMARG(IS),3
        I003=MIN0(I001+2,NMARG(IS))
        DO I=I001,I003
          I002=30+MOD(I-1,3)*28
          CH001(I002:I002+1)='W('
          I002=I002+2
          DO K=1,POLY(IS,I)
            WRITE (UNIT=CH001(I002:I002),FMT='(I1)') INDX(IS,I,K)
            I002=I002+1
          END DO
          WRITE (UNIT=CH001(I002:),FMT=154) WG(IS,I)
  154 FORMAT (') =',F13.2)
        END DO
        WRITE (UNIT=scr,FMT='(/A133)') CH001
        WRITE (UNIT=out,FMT='(/A133)') CH001
        WRITE (UNIT=scr,FMT=156) (WK(IS,I),I=I001,I003)
        WRITE (UNIT=out,FMT=156) (WK(IS,I),I=I001,I003)
  156 FORMAT (29X,3(:,'K=',F5.2,21X))
        CH001=' '
      END DO
!
!      DO I001=1,NMARG(IS)
!       CALL PUST(6,MARDEF(IS,I001))
!      END DO
!+++++
      IF (NSMARG(IS).GT.0) THEN
      WRITE (UNIT=scr,FMT=252)
      WRITE (UNIT=out,FMT=252)
      END IF
  252 FORMAT (/,6X,'SITE-Margules parameters:')
      DO IP=1,NSMARG(IS)
      CH001='           site: '//CHSITE(IS,SMSIT(IS,IP))
      I001=25
      CH001(I001:I001+1)='W('
      I001=I001+2
      DO K=1,SMPOLY(IS,IP)
       CH001(I001:)=ELONS(IS,SMSIT(IS,IP),SMEI(IS,IP,K))
       CALL LABLA(CH001,I001)
       I001=I001+2
      END DO
      CH001(I001-1:I001-1)=')'
      IF (I001.LT.40) THEN
      I001=43
      ELSE
      I001=I001+3
      END IF
      WRITE (UNIT=CH001(I001:),FMT=2012) SWG(IS,IP)
 2012 FORMAT ('= ',F13.2)
      CALL PUST(scr,CH001)
      CALL PUST(out,CH001)
!+++++
!+++++
!      DO K=1,SMPOLY(IS,IP)
!       WRITE (6,2033) K,SINDX(IS,IP,K)
!  2033 FORMAT (I3,' sindx=',I4)
!      END DO
!+++++
!+++++
!      WRITE (scr,2010) SMSIT(IS,IP),CHSITE(IS,SMSIT(IS,IP))
! 2010 FORMAT (' site Nr.: ',I3,2X,A)
!      WRITE (UNIT=CH001,FMT=2010) CHSITE(IS,SMSIT(IS,IP))
! 2010 FORMAT (' site: ',2X,A)
!      WRITE (scr,2020) (SMEI(IS,IP,K),K=1,SMPOLY(IS,IP))
! 2020 FORMAT (20X,'index: ',10I3)
!      WRITE (scr,2025) (ELONS(IS,SMSIT(IS,IP),SMEI(IS,IP,K)), &
!      K=1,SMPOLY(IS,IP))
! 2025 FORMAT (20X,'elements: ',10A4)
!      WRITE (scr,2030) SWH(IS,IP),SWS(IS,IP),SWV(IS,IP),SWG(IS,IP)
! 2030 FORMAT (20X,'parameters:',10F10.3)
      CH001=' '
      END DO
!
!      DO I001=1,NSMARG(IS)
!       CALL PUST(6,SMARDEF(IS,I001))
!      END DO
!+++++
      IF (LAAR(IS)) THEN
      WRITE (scr,161)
      WRITE (out,161)
  161 FORMAT (/' asymmetry parameters:')
      DO 560,IE=1,NEND(IS)
      WRITE (scr,160) NAME(EM(IS,IE)),VLAA(IS,IE)
      WRITE (out,160) NAME(EM(IS,IE)),VLAA(IS,IE)
  160 FORMAT (5X,A16,2X,F8.3)
  560 CONTINUE
      END IF
!+++++
      ISOL=IS
      CALL GIBBSTEST(ISOL,ISOL)
      WRITE (UNIT=scr,FMT='(/)')
      WRITE (UNIT=out,FMT='(/)')
  600 CONTINUE
      END IF
      RETURN
      END
