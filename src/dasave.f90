!-----Version: 28.05.2022
!               ************
!               * dasave.f *
!               ************
!     Subroutines for storing thermodynamic parameters in an array
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
      SUBROUTINE DASAVE(N)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
!-----END OF COMMON VARIABLES
      INTEGER(4) N,I,J
!-----
!--- LISTE WIRD VERWENDET IN: domino (subr. FIXA),
!--- LISTE WIRD VERWENDET IN: dbread (subr. MINNEW und FIXNEW)
!--- LISTE WIRD VERWENDET IN: theriaq (subr. POTPHAS)
      INDATA(1,N)=VOLVO
      INDATA(2,N)=TRTYP
      INDATA(3,N)=NLANDA
      INDATA(4,N)=NCOM
      DO I=1,10
        INDATA(I+4,N)=ICOM(I)
      END DO
!-----
!--- Achtung: Domino verwendet REDATA(3,I) fuer Aktivitaeten. Index nicht aendern!!!
!--- Achtung: Theriaq verwendet REDATA(3,I) fuer Aktivitaeten. Index nicht aendern!!!
!--- Achtung: prtcal verwendet REDATA(4,..) Index nicht aendern!!!
      REDATA(1,N)=G0R
      REDATA(2,N)=H0R
      REDATA(3,N)=S0R
      REDATA(4,N)=V0R
      REDATA(5,N)=AA0
      REDATA(6,N)=AAT
      REDATA(7,N)=BB0
      REDATA(8,N)=BBT
      REDATA(9,N)=K1
      REDATA(10,N)=K2
      REDATA(11,N)=K3
      REDATA(12,N)=K4
      REDATA(13,N)=K5
      REDATA(14,N)=K6
      REDATA(15,N)=K7
      REDATA(16,N)=K8
      REDATA(17,N)=K9
      DO I=1,4
        J=9*(I-1)
        REDATA(J+18,N)=ASPK(I)
        REDATA(J+19,N)=BSPK(I)
        REDATA(J+20,N)=TQ1B(I)
        REDATA(J+21,N)=TEQ(I)
        REDATA(J+22,N)=DVDT(I)
        REDATA(J+23,N)=DVDP(I)
        REDATA(J+24,N)=TRE(I)
        REDATA(J+25,N)=DHTR(I)
        REDATA(J+26,N)=DVTR(I)
      END DO
      REDATA(54,N)=TD0
      REDATA(55,N)=TDMAX
      REDATA(56,N)=VADJ
      REDATA(57,N)=D1
      REDATA(58,N)=D2
      REDATA(59,N)=D3
      REDATA(60,N)=D4
      REDATA(61,N)=D5
      REDATA(62,N)=D6
      REDATA(63,N)=D7
      REDATA(64,N)=D8
      REDATA(65,N)=D9
      DO I=1,10
        REDATA(I+65,N)=FFCOM(I)
      END DO
      REDATA(76,N)=VTA
      REDATA(77,N)=VTB
      REDATA(78,N)=VPA
      REDATA(79,N)=VPB
      REDATA(80,N)=VAA
      REDATA(81,N)=VAB
      REDATA(82,N)=VB
      REDATA(83,N)=VL0
      REDATA(84,N)=VLA
      REDATA(85,N)=VLN
      REDATA(86,N)=VL2
      REDATA(87,N)=TKRI
      REDATA(88,N)=SMA
      REDATA(89,N)=NAT
!-----
      CHDATA(N)=FALL
!-----
!--- Achtung: Domino verwendet LODATA(3,..) fuer SPC-Liste Index nicht aendern!!!
      LODATA(1,N)=RDK
      LODATA(2,N)=VDW
      LODATA(3,N)=SPC
      LODATA(4,N)=COM
      LODATA(5,N)=DIS
      LODATA(6,N)=VO1
      LODATA(7,N)=VO2
      LODATA(8,N)=VO3
      LODATA(9,N)=AQU
      LODATA(10,N)=FIX
      LODATA(11,N)=TL1
      RETURN
      END
!-----
!******************************
      SUBROUTINE DAREST(N)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
!-----END OF COMMON VARIABLES
      INTEGER(4) N,I,J
!-----
      VOLVO=INDATA(1,N)
      TRTYP=INDATA(2,N)
      NLANDA=INDATA(3,N)
      NCOM=INDATA(4,N)
      DO I=1,10
        ICOM(I)=INDATA(I+4,N)
      END DO
!-----
      G0R=REDATA(1,N)
      H0R=REDATA(2,N)
      S0R=REDATA(3,N)
      V0R=REDATA(4,N)
      AA0=REDATA(5,N)
      AAT=REDATA(6,N)
      BB0=REDATA(7,N)
      BBT=REDATA(8,N)
      K1=REDATA(9,N)
      K2=REDATA(10,N)
      K3=REDATA(11,N)
      K4=REDATA(12,N)
      K5=REDATA(13,N)
      K6=REDATA(14,N)
      K7=REDATA(15,N)
      K8=REDATA(16,N)
      K9=REDATA(17,N)
      DO I=1,4
        J=9*(I-1)
        ASPK(I)=REDATA(J+18,N)
        BSPK(I)=REDATA(J+19,N)
        TQ1B(I)=REDATA(J+20,N)
        TEQ(I)=REDATA(J+21,N)
        DVDT(I)=REDATA(J+22,N)
        DVDP(I)=REDATA(J+23,N)
        TRE(I)=REDATA(J+24,N)
        DHTR(I)=REDATA(J+25,N)
        DVTR(I)=REDATA(J+26,N)
      END DO
      TD0=REDATA(54,N)
      TDMAX=REDATA(55,N)
      VADJ=REDATA(56,N)
      D1=REDATA(57,N)
      D2=REDATA(58,N)
      D3=REDATA(59,N)
      D4=REDATA(60,N)
      D5=REDATA(61,N)
      D6=REDATA(62,N)
      D7=REDATA(63,N)
      D8=REDATA(64,N)
      D9=REDATA(65,N)
      DO I=1,10
        FFCOM(I)=REDATA(I+65,N)
      END DO
      VTA=REDATA(76,N)
      VTB=REDATA(77,N)
      VPA=REDATA(78,N)
      VPB=REDATA(79,N)
      VAA=REDATA(80,N)
      VAB=REDATA(81,N)
      VB=REDATA(82,N)
      VL0=REDATA(83,N)
      VLA=REDATA(84,N)
      VLN=REDATA(85,N)
      VL2=REDATA(86,N)
      TKRI=REDATA(87,N)
      SMA=REDATA(88,N)
      NAT=REDATA(89,N)
!-----
      FALL=CHDATA(N)
!-----
      RDK=LODATA(1,N)
      VDW=LODATA(2,N)
      SPC=LODATA(3,N)
      COM=LODATA(4,N)
      DIS=LODATA(5,N)
      VO1=LODATA(6,N)
      VO2=LODATA(7,N)
      VO3=LODATA(8,N)
      AQU=LODATA(9,N)
      FIX=LODATA(10,N)
      TL1=LODATA(11,N)
      RETURN
      END
