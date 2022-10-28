!-----Version: 28.05.2022
!               ************
!               * activi.f *
!               ************
!     Subroutine for site mixing activities
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
      SUBROUTINE ACTIVI(IS,XXX,AAA)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!
!-----END OF COMMON VARIABLES
      INTEGER(4) IS,IE,I,II,IK,IM,IX1,IX0
      REAL(8) XXX(EMAX),AAA(EMAX),XBAS(EMAX),ABAS(EMAX),SPARC(15)
!=====
!     REAL* SUMM
      DO I=1,15
      SPARC(I)=0.0D0
      END DO
!=====
      IF (MODELL(IS).EQ.'S') THEN
!----- das folgende ist an drei Stellen. kann vereinfacht werden.
      DO IX1=1,NSIEL(IS)
        XELSI(IS,IX1)=0.0D0
        DO IK=1,NEMQQ(IS,IX1)
          IE=EMQQ(IS,IX1,IK)
          XELSI(IS,IX1)=XELSI(IS,IX1)+EMXX(IS,IX1,IE)*XXX(IE)
        END DO
      END DO
!-----
!      WRITE (6,1010) (XELSI(IS,IX1),IX1=1,NSIEL(IS))
!      WRITE (out,1010) (XELSI(IS,IX1),IX1=1,NSIEL(IS))
! 1010 FORMAT ('XELSI  ',10F10.5)
!      WRITE (6,1011) ((EMXX(IS,IX1,IE),IX1=1,NSIEL(IS)),IE=1,NEND(IS))
!      WRITE (out,1011) ((EMXX(IS,IX1,IE),IX1=1,NSIEL(IS)),IE=1,NEND(IS))
! 1011 FORMAT ('EMXX  ',10F10.5)
!-----
      DO IE=1,NEND(IS)
        IX0=0
        AAA(IE)=1.0D0
        DO II=1,NSITE(IS)
          IF (II.GT.1) IX0=IX0+NELPS(IS,II-1)
          DO IM=1,IDINT(SITMUL(IS,II))
            IX1=IX0+ELSI(IS,IE,II,IM)
!dC versuch: seems important close to structural endmembers
            IF (XELSI(IS,IX1).LT.1D-50) XELSI(IS,IX1)=1D-50
            IF (AAA(IE).LT.1D-50) AAA(IE)=1D-50
            AAA(IE)=AAA(IE)*XELSI(IS,IX1)/EMXX(IS,IX1,IE)
          END DO
        END DO
      END DO
!-----
!      WRITE (6,1000) (XXX(I),I=1,NEND(IS))
!      WRITE (out,1000) (XXX(I),I=1,NEND(IS))
! 1000 FORMAT (/'XXX  ',10F10.5)
!      WRITE (6,1001) (AAA(I),I=1,NEND(IS))
!      WRITE (out,1001) (AAA(I),I=1,NEND(IS))
! 1001 FORMAT ('AAA  ',10F10.5)
!=====
      RETURN
      ELSE
      DO I=1,NEMBAS(IS)
        IF (EMBCOD(IS,I).EQ.0) THEN
          XBAS(I)=0.0D0
        ELSE
          XBAS(I)=XXX(EMBCOD(IS,I))
        END IF
      END DO
      CALL SOLCAL(SOLNAM(IS),P,T,NEMBAS(IS),XBAS,ABAS,SPARC)
      DO I=1,NEMBAS(IS)
        IF (EMBCOD(IS,I).NE.0) THEN
         AAA(EMBCOD(IS,I))=ABAS(I)
        END IF
      END DO
      RETURN
      END IF
      END
!-----
!******************************
!-----
!******************************
      SUBROUTINE MUECAL(IS,XXX,MUE)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
!-----END OF COMMON VARIABLES
      INTEGER(4) IS,IE,N,I,I001,IP,J,IX1,IK
      REAL(8) XXX(EMAX),AAA(EMAX),FF(MAMAX),SJ(MAMAX),RTA,F001, &
      MUE(EMAX),PIX(MAMAX),SUMPIX(MAMAX,EMAX),SUMSUM(MAMAX), &
      FSIFEL,FXEL
!-----
      IF (LAAR(IS)) THEN
      CALL MUELAAR(IS,XXX,MUE)
      RETURN
      END IF
!----- used for testing
!      IF (NSMARG(IS).GT.0) THEN
!      CALL MUEOFG(IS,XXX,MUE)
!      RETURN
!      END IF
!-----
      RTA=RT*ALPHA(IS)
!=====
      DO 501,N=1,NMARG(IS)
        SJ(N)=0.0D0
        DO I=1,RANGE(IS,N)
          SJ(N)=SJ(N)+XXX(SJIND(IS,N,I))
        END DO
        IF (WK(IS,N).LE.0.0D0.OR.SJ(N).LE.0.0D0) THEN
          FF(N)=WG(IS,N)
        ELSE
          FF(N)=WG(IS,N)/(SJ(N)**WK(IS,N))
        END IF
        DO,I=1,POLY(IS,N)
          IF (DABS(FF(N)).LT.1D-20) GO TO 1
          FF(N)=FF(N)*XXX(INDX(IS,N,I))
        END DO
    1 CONTINUE
  501 CONTINUE
!=====
      IF (NSMARG(IS).GT.0) THEN
!+++++
!----- das folgende ist an drei Stellen. kann vereinfacht werden.
      DO,IX1=1,NSIEL(IS)
      XELSI(IS,IX1)=0.0D0
        DO IK=1,NEMQQ(IS,IX1)
          IE=EMQQ(IS,IX1,IK)
          XELSI(IS,IX1)=XELSI(IS,IX1)+EMXX(IS,IX1,IE)*XXX(IE)
        END DO
      END DO
!-----
      DO IP=1,NSMARG(IS)
      PIX(IP)=SWG(IS,IP)
      DO J=1,SMPOLY(IS,IP)
      PIX(IP)=PIX(IP)*XELSI(IS,SINDX(IS,IP,J))
      END DO
      DO IE=1,NEND(IS)
       SUMPIX(IP,IE)=0.0D0
       DO J=1,SMPOLY(IS,IP)
        FSIFEL=EMXX(IS,SINDX(IS,IP,J),IE)
        FXEL=XELSI(IS,SINDX(IS,IP,J))
        IF (FSIFEL.GT.1D-5.AND.FXEL.GT.1D-10) THEN
        SUMPIX(IP,IE)=SUMPIX(IP,IE)+FSIFEL/FXEL
        END IF
       END DO
       SUMPIX(IP,IE)=SUMPIX(IP,IE)*PIX(IP)
      END DO
      SUMSUM(IP)=0.0D0
      DO IE=1,NEND(IS)
       F001=XXX(IE)
       IF (DABS(XXX(IE)).LT.1D-60) F001=1D-60
       SUMSUM(IP)=SUMSUM(IP)+F001*SUMPIX(IP,IE)
      END DO
!
      END DO
!+++++
      END IF
!=====
      IF (MODELL(IS).EQ.'I') THEN
      DO,I001=1,NEND(IS)
        AAA(I001)=XXX(I001)
      END DO
      ELSE
        CALL ACTIVI(IS,XXX,AAA)
      END IF
!=====
      DO 504,IE=1,NEND(IS)
!dC
!      IF (AAA(IE).LE.0.0D0.OR.XXX(IE).LE.0.0D0) THEN
!      IF (AAA(IE).LE.1.0D-60) THEN
!!      IF (AAA(IE).LE.0.0D0) THEN
!!        MUE(IE)=-1D20
!!      ELSE

      IF (AAA(IE).LE.0.0D0) AAA(IE)=1D-20

!
      F001=XXX(IE)
      IF (DABS(XXX(IE)).LT.1D-60) F001=1D-60
      MUE(IE)=GG(EM(IS,IE))+RTA*DLOG(AAA(IE))
      DO N=1,NMARG(IS)
        MUE(IE)=MUE(IE)+FF(N)*(QQ(IS,N,IE)/F001+(1-POLY(IS,N)))
        IF (SJ(N).GT.0.0D0) THEN
          MUE(IE)=MUE(IE)-FF(N)*WK(IS,N)*(DSJDX(IS,N,IE)-SJ(N))/SJ(N)
        END IF
      END DO
!
      DO IP=1,NSMARG(IS)
        MUE(IE)=MUE(IE)+PIX(IP)+SUMPIX(IP,IE)-SUMSUM(IP)
      END DO
!=====
 !!     END IF
  504 CONTINUE
!+++++
!+++++
!----  general equation for excess mue is: (used for site margules)
!----  mue(ie) = G + dG/dX(ie) - sum(i=1,ie):(x(ie)*dG/dX(i))
!----  x=concentrations on sites, X=endmember concentrations
!----  dG/dX = sum((i=1,SMPOLY):PIX*dx/dX
!----  dx/dX = fraction of x in endmember X (nicht so saubere Notation)
!----  1: calculate XEL from XELSI
!----  2: calculate PIX (=W*product of x=G contribution from W)
!----  3: calculate SUMPIX (=sum(i=1,SMPOLY): PIX*dx/dX
!----  4: calculate SUMSUM =sum(i=1,e):x*PIX
!---new:
!----  XEL not necessary
!+++++
!+++++
!=====
      RETURN
      END
!-----
!******************************
      SUBROUTINE MUELAAR(IS,XXX,MUE)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
!-----END OF COMMON VARIABLES
      INTEGER(4) IS,IE,N,I001,J
      REAL(8) XXX(EMAX),AAA(EMAX),RTA,F001, &
      SUMAX,SUMA,QI(EMAX),QIQI,MUE(EMAX),PRDA,AWG
!-----
      RTA=RT*ALPHA(IS)
      SUMAX=0.0D0
      DO IE=1,NEND(IS)
!=====DKT-Setting VLAA to Volume of em if VLAA0 < 0.0 - for fluids
        IF(VLAA0(IS,IE).LT.0.0D0) THEN
          VLAA(IS,IE)=VV(EM(IS,IE))
        END IF
!=====
        SUMAX=SUMAX+VLAA(IS,IE)*XXX(IE)
      END DO
      DO IE=1,NEND(IS)
        QI(IE)=VLAA(IS,IE)*XXX(IE)/SUMAX
      END DO
!=====
      IF (MODELL(IS).EQ.'I') THEN
        DO I001=1,NEND(IS)
          AAA(I001)=XXX(I001)
        END DO
      ELSE
        CALL ACTIVI(IS,XXX,AAA)
      END IF
!=====
      DO 504,IE=1,NEND(IS)
!dC
!      IF (AAA(IE).LE.0.0D0.OR.XXX(IE).LE.0.0D0) THEN
!      IF (AAA(IE).LE.0.0D0) THEN
!!      IF (AAA(IE).LE.1.0D-60) THEN
!!      MUE(IE)=-1D20
!!      ELSE
!      IF (AAA(IE).LE.1.0D-60) AAA(IE)=1D-60
      IF (AAA(IE).LE.0.0D0) AAA(IE)=1D-50
      F001=XXX(IE)
!dC
!      IF (XXX(IE).LT.1D-20) F001=1D-20
      IF (DABS(XXX(IE)).LT.1D-50) F001=1D-50
      MUE(IE)=GG(EM(IS,IE))+RTA*DLOG(AAA(IE))
!L
      DO 503,N=1,NMARG(IS)
      QIQI=1.0D0
      SUMA=0.0D0
      PRDA=1.0D0
!     AWG is temporary WG, which may be modified when using A interaction param
!     for fluid phase, where it is multiplied by aspecies volume coefficient.
      AWG=WG(IS,N)
      DO J=1,POLY(IS,N)
        I001=INDX(IS,N,J)
        PRDA=PRDA*VLAA(IS,I001)
        SUMA=SUMA+VLAA(IS,I001)
        IF (I001.EQ.IE) THEN
          QIQI=QIQI*(1.0D0-QI(I001))
        ELSE
          QIQI=QIQI*QI(I001)*(-1.0D0)
        END IF
      END DO
!----- ev. 2.0D0 = POLY(IS,N)
!     AWG modified by volume-related coeff. of fluid species
      IF(VLAA0(IS,IE).LT.0.0D0) THEN
        AWG=WG(IS,N)*SUMA/PRDA
      END IF
      MUE(IE)=MUE(IE)-QIQI*AWG*2.0D0*VLAA(IS,IE)/SUMA
  503 CONTINUE
!L
!!      END IF
  504 CONTINUE
!=====
!=====
      RETURN
      END
!-----
!******************************
      SUBROUTINE GNONID(IS,XXX,GGG)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
!-----END OF COMMON VARIABLES
      INTEGER(4) IS,N,I,I001,IP,J,IX1,IK,IE
      REAL(8) XXX(EMAX),AAA(EMAX),XM,GGG,RTA,SJ,OBEN,UNTEN,SUMA, &
      PLUSG,PRDA,AWG
!-----
      GNOM=GNOM+1
      GGG=0.0D0
      RTA=RT*ALPHA(IS)
      IF (MODELL(IS).EQ.'I') THEN
      DO I001=1,NEND(IS)
        AAA(I001)=XXX(I001)
      END DO
      ELSE
      CALL ACTIVI(IS,XXX,AAA)
      END IF
      DO I=1,NEND(IS)
        GGG=GGG+XXX(I)*GG(EM(IS,I))
        IF (AAA(I).GT.0.0D0) GGG=GGG+RTA*XXX(I)*DLOG(AAA(I))
      END DO
!+++++ here: if (laar) / else (kann ev zusammengefasst werden)
      IF (LAAR(IS)) THEN
      UNTEN=0.0D0
      DO I=1,NEND(IS)
!=====DKT-Setting VLAA to Volume of em if VLAA0 < 0.0 - for fluids
        IF(VLAA0(IS,I).LT.0.0D0) THEN
          VLAA(IS,I)=VV(EM(IS,I))
        END IF
!=====
        UNTEN=UNTEN+VLAA(IS,I)*XXX(I)
      END DO
      DO 604,N=1,NMARG(IS)
      XM=1.0D0
      OBEN=1.0D0
      SUMA=0.0D0
      PRDA=1.0D0
!     AWG is temporary WG, which may be modified when using A interaction param
!     for fluid phase, where it is multiplied by aspecies volume coefficient.
      AWG=WG(IS,N)
      DO I=1,POLY(IS,N)
        I001=INDX(IS,N,I)
        XM=XM*XXX(I001)
        OBEN=OBEN*VLAA(IS,I001)
        SUMA=SUMA+VLAA(IS,I001)
        PRDA=PRDA*VLAA(IS,I001)
      END DO
!     AWG modified by volume-related coeff. of fluid species. Not looping thru
!     NEND here, so need to just test 1st end-member of solution to see if
!     treated like fluid style. This is not safe. Find better way.
      IF(VLAA0(IS,1).LT.0.0D0) THEN
      AWG=WG(IS,N)*SUMA/PRDA
      END IF
      GGG=GGG+AWG*XM*2.0D0*OBEN/UNTEN/SUMA
  604 CONTINUE
!+++++  else
      ELSE
      DO 504,N=1,NMARG(IS)
      XM=1.0D0
      DO I=1,POLY(IS,N)
        XM=XM*XXX(INDX(IS,N,I))
      END DO
      SJ=0.0D0
      DO I=1,RANGE(IS,N)
        SJ=SJ+XXX(SJIND(IS,N,I))
      END DO
      IF (SJ.LE.0.0D0.OR.WK(IS,N).LE.0.0D0) THEN
      GGG=GGG+WG(IS,N)*XM
      ELSE
      GGG=GGG+WG(IS,N)*XM/(SJ**WK(IS,N))
      END IF
  504 CONTINUE
!==
      IF (NSMARG(IS).GT.0) THEN
!+++++
!----- das folgende ist an drei Stellen. kann vereinfacht werden.
      DO IX1=1,NSIEL(IS)
        XELSI(IS,IX1)=0.0D0
        DO IK=1,NEMQQ(IS,IX1)
          IE=EMQQ(IS,IX1,IK)
          XELSI(IS,IX1)=XELSI(IS,IX1)+EMXX(IS,IX1,IE)*XXX(IE)
        END DO
      END DO
!-----
      DO IP=1,NSMARG(IS)
       PLUSG=SWG(IS,IP)
       DO J=1,SMPOLY(IS,IP)
        PLUSG=PLUSG*XELSI(IS,SINDX(IS,IP,J))
       END DO
       GGG=GGG+PLUSG
      END DO
!+++++
      END IF
!==
      END IF
!+++++ END: if (laar) / else
      RETURN
      END
!-----
!****************************** only used for activity test, and to test MUECAL
      SUBROUTINE MUEOFG(IS,XX0,MUE)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
!-----END OF COMMON VARIABLES
      INTEGER(4) IS,IE,I
      REAL(8) MUE(EMAX),GGD0,GGD1,GGD2,XX0(EMAX),F1,F2, &
      XX1(EMAX),XX2(EMAX),FFX,MINXV(EMAX),DX,DXP,DXM
!-----
      DX=1D-7
      DO I=1,NEND(IS)
        MINXV(I)=0.0D0
      END DO
      IF (NNEGEM(IS).GT.0) THEN
      DO I=1,NNEGEM(IS)
        MINXV(NEGEM(IS,I))=-1.0D0
      END DO
      END IF
!---- find dxp and dxm (muss noch fuer negem angepasst werden)
      DXP=DX
      DXM=DX
      F1=(1.0D0-XX0(1))
      F2=XX0(1)-MINXV(1)
      DO I=2,NEND(IS)
        IF ((XX0(I)-MINXV(I)).LT.F1) F1=XX0(I)-MINXV(I)
        IF ((1.0D0-XX0(I)).LT.F2) F2=1.0D0-XX0(I)
      END DO
      F1=DABS(F1/2.0D0)
      F2=DABS(F2/2.0D0)
      IF (F1.LT.DXP) DXP=F1
      IF (F2.LT.DXM) DXM=F2
!====
      CALL GNONID(IS,XX0,GGD0)
      DO 710,IE=1,NEND(IS)
      DO I=1,NEND(IS)
        XX1(I)=XX0(I)
        XX2(I)=XX0(I)
      END DO
      XX1(IE)=XX0(IE)-DXM
      XX2(IE)=XX0(IE)+DXP
      DO I=1,NEND(IS)
        XX1(I)=XX1(I)/(1.0D0-DXM)
        XX2(I)=XX2(I)/(1.0D0+DXP)
      END DO
      FFX=XX2(IE)-XX1(IE)
      IF (DABS(FFX).GT.1.0D-12) THEN
      CALL GNONID(IS,XX1,GGD1)
      CALL GNONID(IS,XX2,GGD2)
      MUE(IE)=GGD0+(1.0D0-XX0(IE))*(GGD2-GGD1)/FFX
      ELSE
      MUE(IE)=GGD0
      END IF
!      MUE(IE)=MUE(IE)-GG(EM(IS,IE))
  710 CONTINUE
!-----
      RETURN
      END
