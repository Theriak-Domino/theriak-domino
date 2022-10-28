!-----Version: 28.05.2022
!               ***********
!               * gcalc.f *
!               ***********
!     Subroutines for calculating the Gibbs Free Energy
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
      SUBROUTINE GCALC(IP)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
!-----END OF COMMON VARIABLES
      REAL(8) CPRDT,CPRTDT,TD,VD,FV,FF,X001(EMAX),A00,KTT,VMA,Q2,Q4, &
      PK,DPK,DKDT,A22,KAPS,THE,UU0,UU,AA,BB,CC,XX0,PTH,A0, &
      K0,K01,K02,NN0,PK0,DXP, &
      PJ,GCH,HCH,SCH,VCH,CPCH,DG1,DG2,SII,FV1,A01
!      REAL(8) F1,F2,PXM,PXP
      INTEGER(4) I,I001,IP
! variables used by Doug Tinkham (+k0,dkdt,pth)
      REAL(8) pref,ivpo,ao,k0p,k0pp,tk,tref,vr,kt,v1t,a,b,c,ivdp, &
      pkb,vpt
!=====
      FGVOL=0.0D0
      FVVOL=0.0D0
      FGDIS=0.0D0
      FHDIS=0.0D0
      FSDIS=0.0D0
      FCPDIS=0.0D0
      FVDIS=0.0D0
      FGGAS=0.0D0
      FVGAS=0.0D0
      FGTR=0.0D0
      FHTR=0.0D0
      FSTR=0.0D0
      FCPTR=0.0D0
      FVTR=0.0D0
      FCPSOL=0.0D0
      FHSOL=0.0D0
      FSSOL=0.0D0
      FGSOL=0.0D0
      FVSOL=0.0D0
      FG0=0.0D0
      FGCP=0.0D0
      FHCP=0.0D0
      FSCP=0.0D0
      FCPCP=0.0D0
      GR=0.0D0
      HR=0.0D0
      SR=0.0D0
      VOLUM=0.0D0
      DO I=1,5
       FSPEC(I)=0.0D0
       CHSPEC(I)=' '
      END DO
!-----
!===== solution phases with fixed compositions
!-----
      IF (FIX) THEN
      I001=ISOFIX(IP)
      DO I=1,NEND(I001)
        X001(I)=XEMFIX(IP,I)
      END DO
      GNOM=0
      CALL GNONID(I001,X001,FF)
      GR=FF
      RETURN
      END IF
!-----
!===== aqueous phases with HKF equation
!===== AQ1 K1,K2,K8,K9,K7
!===== AQ2 D1,D2,D3,D4
!-----
!      IF (AQU) THEN
      IF (PHASID(IP).EQ.'AQU') THEN
      CALL AQUA
      RETURN
      END IF
!********************************************************************************
!-----
!===== normal Cp function and ST, must always be present
!===== ST G0R,H0R,S0R,V0R
!===== C1 K1,K4,K3,K8
!===== C2 K6,K2,K5,K7,K9
!===== C3 K1,K2,K3,K4
!-----
      FCP0=K1+K2*T0+K3/TT0+K4/SQT0+K5*TT0 &
      +K6/T0+K7*SQT0+K8/(T0**3)+K9*T0**3
      CPR=K1+K2*T+K3/TT+K4/SQT+K5*TT+K6/T+K7*SQT+K8/(TT*T)+K9*TT*T
      CPRDT=K1*(T-T0)+K2*(TT-TT0)/2.0D0 &
      -K3*(1.0D0/T-1.0D0/T0)+2D0*K4*(SQT-SQT0) &
      +K5*(TT*T-TT0*T0)/3.0D0+K6*DLOG(T/T0) &
      +K7*(T*SQT-T0*SQT0)*2.0D0/3.0D0 &
      -K8*(1.0D0/TT-1.0D0/TT0)/2.0D0 &
      +K9*(TT*TT-TT0*TT0)/4.0D0
      CPRTDT=K1*DLOG(T/T0)+K2*(T-T0) &
      -K3*(1.0D0/TT-1.0D0/TT0)/2.0D0 &
      -2D0*K4*(1.0D0/SQT-1.0D0/SQT0) &
      +K5*(TT-TT0)/2.0D0-K6*(1.0D0/T-1.0D0/T0) &
      +2D0*K7*(SQT-SQT0) &
      -K8*(1.0D0/(TT*T)-1.0D0/(TT0*T0))/3.0D0 &
      +K9*(TT*T-TT0*T0)/3.0D0
      FG0=H0R-T0*S0R
      GR=H0R+CPRDT-T*(S0R+CPRTDT)
      FGCP=GR-FG0
      FHCP=CPRDT
      FSCP=CPRTDT
      FCPCP=CPR-FCP0
      HR=H0R+CPRDT
      SR=S0R+CPRTDT
      VOLUM=V0R
!********************************************************************************
!-----
!===== van der Waals or Redlich-Kwong
!===== VDW AA0,AAT,BB0,BBT
!===== R-K AA0,AAT,BB0,BBT
!-----
      IF (RDK.OR.VDW) THEN
      CALL GAS
      END IF
!********************************************************************************
!-----
!===== CORK programmed by Doug Tinkham
!===== CS1 AA0,BB0
!-----
      IF (VOLVO.EQ.6) THEN
      CALL CSGAS
      END IF
!********************************************************************************
!-----
!===== CORK with cubic equation
!===== CS2 AA0,BB0
!-----
      IF (VOLVO.EQ.7) THEN
      CALL CORK
      END IF
!********************************************************************************
!-----
!===== volume function 1 (as in TWQ)
!===== V1 VTA,VTB,VPA,VPB (achtung Faktoren)
!-----
      IF (VO1) THEN
      FVVOL=VTA*(T-T0)+VTB*(T-T0)**2+VPA*(P-P0)+VPB*(P-P0)**2
      VOLUM=V0R+FVVOL
      FGVOL=(P-P0)*(V0R+VTA*(T-T0)+VTB*(T-T0)**2) &
      +VPA*(P**2/2.0D0-P*P0+P0**2/2.0D0) &
      +VPB*(P**3/3.0D0-P**2*P0+P*P0**2-P0**3/3.0D0)
      GR=GR+FGVOL
      END IF
!
!********************************************************************************
!====================== eduester===============================
!===== volume function Tait equation + modification 
!===== for Berman (1988) database
!===== VE  VTA,VTB,VPA,VPB (achtung Faktoren)
!===== VPA is now bar(-1),VTA is now K(-1)
!-----
      IF (VOLVO.EQ.11) THEN
      PK=P*1.0D-3 ! bar-->kbar
      PK0=1.0D-3  ! P0 =0.001 kbar
      DPK=PK-1D-3 ! P-P0
      A0=VTA      ! 1/K
      A01=VTB      ! 1/(K*K)
      K0=-1/VPA*1.0D-3 ! kbar    
      K01=2.0D0*VPB*(-1.0D0/VPA)*(-1.0D0/VPA)-1.0D0
      IF (DABS(VPB).LT.1D-20) K01=4.0D0
!-----K02 for Vinet EOS after Jeanloz (1988)
      K02=-1.0D0/K0*((0.5D0*K01)*(0.5D0*K01)+0.5D0*K01 & 
      -19.0D0/36.0D0)

!----- molar entropy
!      SII=S0R
!      NN0=0.0D0
!      DO I=1,NUN
!      NN0=NN0+XX(IP,I)
!      END DO
!----- Einstein temperature
!      THE=10636.0D0/((SII/NN0)+6.44D0)
!----- Tait equation
      AA=(1.0D0+K01)/(1.0D0+K01+K0*K02)
      BB=K01/K0-K02/(1.0D0+K01)
      CC=(1.0D0+K01+K0*K02)/(K01*K01+K01-K0*K02)
      
!----- PTH = integral[ ALPHA(T)*KAPPA(T)]dT
!        with ALPHA(T)=(A0+2*A1*(T-T0))/(1+A0*(T-T0)+A01*(T-T0)^2)
!        after Berman, 1988
!       integral[ALPHA(T)]=ln(1+A0*(T-T0)+A01*(T-T0)^2)
!        and KAPPA(T)=K0*DEXP(-K01*(integral[ALPHA(T)]))
!        after Helfrich and Connolly(2009)  
        
      PTH=(-K0/(K01))*(1.0D0+A0*(T-T0)+A01*(T-T0)*(T-T0))**(-K01) &
          -(-K0/(K01))
      
      
      VOLUM=V0R*(1.0D0-AA*(1.0D0-(1.0D0+BB*(PK-PTH))**(-CC)))
      FVVOL=VOLUM-V0R
!
!---- from maxima (not so good: rounding errors)
!      FGVOL=-((-BB*PTH+BB*PK+1.0D0)**CC*(AA*BB*PTH-AA*BB*PK-AA)+ &
!      (1.0D0-BB*PTH)**CC*(AA-AA*BB*PTH)+((AA-1.0D0)*BB*CC+ &
!      (AA-1.0D0)*BB)*PK)*V0R/(BB*CC+BB)
!      FGVOL=FGVOL*1000.0D0
!
!---- from Holland and Powell 2011
      FV=(1.0D0-BB*PTH)**(1.0D0-CC)-(1.0D0+BB*(PK-PTH))**(1.0D0-CC)
      FF=AA*FV/(BB*(CC-1.0D0)*PK)
      FF=FF+1.0D0-AA
!- achtung: hier P-P0 statt P
      FGVOL=(P-P0)*V0R*FF
      GR=GR+FGVOL
      END IF
!
!********************************************************************************
!-----
!===== volume function 2 (alpha = a + bT, beta = const.)
!===== v2 VAA,VAB,VB
!-----
      IF (VO2) THEN
      FVVOL=V0R*DEXP(VAA*(T-T0)+VAB*(TT-TT0)/2.0D0)
      IF (VB.GT.0.0D0) THEN
      FGVOL=(FVVOL/VB)*(1.0D0-DEXP(-VB*(P-P0)))
      GR=GR+FGVOL
      VOLUM=FVVOL*DEXP(-VB*(P-P0))
      FVVOL=VOLUM-V0R
      ELSE
      FGVOL=FVVOL*(P-P0)
      GR=GR+FGVOL
      VOLUM=FVVOL
      FVVOL=VOLUM-V0R
      END IF
      END IF
!********************************************************************************
!-----
!===== volume function 3 (experimental do not use)
!===== V3 VL0,VLA,VLN,VL2
!-----
      IF (VO3) THEN
      FVVOL=VL0+VLA*T
      IF (DABS(VL2).LT.1D-20.AND.DABS(VLN).GT.1D-20) THEN
      FF=1.0D-6/(0.7551D0+2.76D0*FVVOL/VLN)
      FGVOL=FVVOL*((P-P0)-FF*((P**2-P0**2)/2.0D0-FF*(P**3-P0**3)/3.0D0))
      GR=GR+FGVOL
      VOLUM=FVVOL*(1.0D0-FF*(P-FF*P**2/2.0D0))
      ELSE
      FGVOL=FVVOL*(P-P0)+VL2*(P**2-P0**2)
      GR=GR+FGVOL
      VOLUM=FVVOL+2.0D0*VL2*P
      END IF
      FVVOL=VOLUM-V0R
      END IF
!********************************************************************************
!-----
!===== volume function from Holland and Powell 1998.
!===== Includes Landau transition
!===== VHP  VTA, VTB,  TKRI,      SMA,   VPA
!           A0   K(T0) Tcrit(T0)  Smax   Vmax
!===== VH2  D1, D2,  D3
!           a2  K'   dkdt
!===== K'=4, a2=10, dkdt (dKdT/1D3 in table) =-1.5D-4
!-----
      IF (VOLVO.EQ.4.AND.DABS(VTB).GT.1D-20) THEN
!---- volume function
!-
      DXP=P/1D5
!      PXP=P+DXP
!      PXM=P-DXP
!---
      PK=P*1.0D-3
      PK0=1.0D-3
      DPK=PK-1D-3
      A00=VTA
!---- A22 ist nach hopo 1998 immer 10
!---- KAPS ist nach hopo 1998 immer 4
!---- dkdt ist nach hopo 1998 immer -1.5D-4
!---- dkdt in the dataset is not always -1.5D-4
      A22=D1
      KAPS=D2
      DKDT=D3
! comparing with G-table it seems that although DKDT in the
! database is not always -1.5D-4, the following is used
      A22=10.0D0
      KAPS=4.0D0
      DKDT=-1.5D-4
!
      IF (DABS(DKDT).LT.1D-20) DKDT=-1.5D-4
      KTT=VTB*(1.0D0+DKDT*(T-T0))
      VMA=VPA
      FV=V0R*(1.0D0+A00*(T-T0)-20.0D0*A00*(SQT-SQT0))
!      VOLUM=FV*((1.0D0-4.0D0*PK/(KTT+4.0D0*PK))**0.25D0)
!---- VOLUM geht auch einfacher
      VOLUM=FV*(KTT/(4.0D0*PK+KTT))**0.25D0
      FVVOL=VOLUM-V0R
!---- Formel genau nach  HOPO98  (ist bei 1 Bar ca. 2.0)
!      FGVOL=1D3*(FV*KTT/3.0D0)*((1.0D0+(4.0D0*PK/KTT))**0.75D0-1.0D0)
!---- Formel nach maxima (ist bei 1 Bar ca. 10-4)
      FGVOL=1D3*(FV*KTT/3.0D0)*(((4.0D0*PK+KTT)/KTT)**0.75D0- &
      ((4.0D0*PK0+KTT)/KTT)**0.75D0)
      GR=GR+FGVOL
!---
!      F1=1D3*(FV*KTT/3.0D0)*((1.0D0+(4.0D0*PXP/1D3/KTT))**0.75D0- &
!      (1.0D0+(4.0D0*PK0/KTT))**0.75D0)
!      F2=1D3*(FV*KTT/3.0D0)*((1.0D0+(4.0D0*PXM/1D3/KTT))**0.75D0- &
!      (1.0D0+(4.0D0*PK0/KTT))**0.75D0)
!      FF=(F1-F2)/2.0D0/DXP
!---- VOLUM seems +- OK (is not exactly V0R at 25 C 1 Bar)
!      WRITE (6,2000) VOLUM,FF,NAME(IP)
! 2000 FORMAT ( &
!      /,' volume from equation : ',1PE15.8, &
!      /,' volume from dG/dP +- : ',1PE15.8,5X,A)
!-----
!===== Landau transition from Holland and Powell 1998, with dG/dP)
!===== VHP  VTA, VTB,  TKRI,      SMA,   VPA
!           A0   K(T0) Tcrit(T0)  Smax   Vmax
!===== VH2  D1, D2,  D3
!           a2  K'   dkdt
!-----
      IF (DABS(TKRI).GT.1D-20.AND.DABS(SMA).GT.1D-20) THEN
!---- check for possible database errors (put to dbread)
!      IF (TKRI.LT.T0.OR.SMA.EQ.0.0D0) THEN
!      WRITE (6,1000) NAME(IP)
! 1000 FORMAT (/' TKRI less than T0 or SMA=0: ',A)
!      STOP
!      END IF
!-----
      DXP=P/1D5
      PJ=P
      CALL LANDHP98(PJ,GCH,HCH,SCH,CPCH,VCH)
      FGTR=FGTR+GCH
      FHTR=FHTR+HCH
      FSTR=FSTR+SCH
      FCPTR=FCPTR+CPCH
      GR=GR+GCH
      HR=HR+HCH
      SR=SR+SCH
      CPR=CPR+CPCH
!---- now dG/dP
      IF (VNEED) THEN
        DG1=GCH
        PJ=P+DXP
        CALL LANDHP98(PJ,GCH,HCH,SCH,CPCH,VCH)
        DG2=GCH
        VCH=(DG2-DG1)/DXP
      END IF
      VOLUM=VOLUM+VCH
      FVTR=FVTR+VCH
!-----
      END IF
!----
      END IF
!********************************************************************************
!-----
!===== volume function from Holland and Powell 2011.
!===== V11  VAA, D1,  D2,  D3,   VLN   VL2
!           A0   K0   K0'  K0''  (1=Landau, 2=Bragg-Williams, 3=aquous, 4=melt)
!                                       VL2=THETA (Einstein Temperature)
!-----
      IF (VOLVO.EQ.5) THEN
      IF (DABS(VLN).LT.1D-20.OR.(DABS(VLN)-1.0D0).LT.1D-20 &
      .OR.(DABS(VLN)-2.0D0).LT.1D-20) THEN
      PK=P*1.0D-3
      PK0=1.0D-3
      DPK=PK-1D-3
      A0=VAA
      K0=D1
      K01=D2
      K02=D3
!-K02 is normally = -K01/K0
! The data set includes a term for K02 to anticipate for the rare occasion 
! where a value other than K02=-K01/K0 might be warranted
!---      K02=-K01/K0
      SII=S0R
!---  NAT number of atoms stored because of phases with CODE = +...
      NN0=NAT
!
      THE=10636.0D0/((SII/NN0)+6.44D0)
!     try: THETA as input
!!      WRITE (UNIT=6,FMT='(''THETA= '',A,F20.10)') NAME(IP),THE
      IF (DABS(VL2).GT.1D-20) THE=VL2
!
      AA=(1.0D0+K01)/(1.0D0+K01+K0*K02)
      BB=K01/K0-K02/(1.0D0+K01)
      CC=(1.0D0+K01+K0*K02)/(K01*K01+K01-K0*K02)
      UU0=THE/T0
      UU=THE/T
      XX0=(UU0*UU0*DEXP(UU0)/((DEXP(UU0)-1.0D0)**2))
      PTH=(A0*K0*THE/XX0)* &
      (1.0D0/(DEXP(UU)-1.0D0)-(1.0D0/(DEXP(UU0)-1.0D0)))
      VOLUM=V0R*(1.0D0-AA*(1.0D0-(1.0D0+BB*(PK-PTH))**(-CC)))
      FVVOL=VOLUM-V0R
!
!---- from maxima (not so good: rounding errors?)
!      FGVOL=-((-BB*PTH+BB*PK+1.0D0)**CC*(AA*BB*PTH-AA*BB*PK-AA)+ &
!      (1.0D0-BB*PTH)**CC*(AA-AA*BB*PTH)+((AA-1.0D0)*BB*CC+ &
!      (AA-1.0D0)*BB)*PK)*V0R/(BB*CC+BB)
!      FGVOL=FGVOL*1000.0D0
!
!---- from Holland and Powell 2011
      FV=(1.0D0-BB*PTH)**(1.0D0-CC)-(1.0D0+BB*(PK-PTH))**(1.0D0-CC)
      FF=AA*FV/(BB*(CC-1.0D0)*PK)
      FF=FF+1.0D0-AA
!- achtung: hier P-P0 statt P
!21aug2015 doch nur P statt (P-P0) (damit dG/dP = V)
!      FGVOL=(P-P0)*V0R*FF
      FGVOL=(P)*V0R*FF
      GR=GR+FGVOL
! special variables for V11 Feb. 2012
      CHSPEC(1)='PTH'
      FSPEC(1)=PTH
      CHSPEC(2)='A'
      FSPEC(2)=AA
      CHSPEC(3)='B'
      FSPEC(3)=BB
      CHSPEC(4)='C'
      FSPEC(4)=CC
      CHSPEC(5)='1+B(P-PTH))'
      FSPEC(5)=(1.0D0+BB*(PK-PTH))
!
!
      END IF
      END IF
!
!********************************************************************************
!----- this is just a test
!-----
!===== SIMPLIFIED volume function from Holland and Powell 2011.
!===== V11  VAA, D1,  D2,  D3,   
!           A0   K0   K0'  K0''
!-----
      IF (VOLVO.EQ.10) THEN
      PK=P*1.0D-3
      PK0=1.0D-3
      DPK=PK-1D-3
      AA=VAA
      BB=D1
      CC=D2
      PTH=D3*(T-T0)
      VOLUM=V0R*(1.0D0-AA*(1.0D0-(1.0D0+BB*(PK-PTH))**(-CC)))
      FVVOL=VOLUM-V0R
!
!---- from Holland and Powell 2011
      FV=(1.0D0-BB*PTH)**(1.0D0-CC)-(1.0D0+BB*(PK-PTH))**(1.0D0-CC)
      FF=AA*FV/(BB*(CC-1.0D0)*PK)
      FF=FF+1.0D0-AA
!- achtung: hier P-P0 statt P
      FGVOL=(P-P0)*V0R*FF
      GR=GR+FGVOL
!
      END IF
!********************************************************************************
!-----
!===== Landau order function from Holland and Powell 2011.
!===== must be used with above volume function, VOLVO=5
!===== it is almost the same as 1998, DKDT always 1.5D-4
!===== and different variables used ....
!===== V11  VAA, D1,  D2,  D3,   VLN
!           A0   K0   K0'  K0''  (0=no trans, 1=Landau, 2=Bragg-Williams, 3=aquous, 4=melt)
!===== LA1  TKRI,      SMA,  VPA
!           Tcrit(T0)  Smax  Vmax
!-----
!!!      IF (VOLVO.EQ.5.AND.(DABS(VLN)-1.0D0).LT.1D-20) THEN
      IF (VOLVO.EQ.5.AND.(DABS(VLN-1.0D0)).LT.1D-20) THEN
!-----
      DXP=P/1D5
      PJ=P
      CALL LANDHP11(PJ,GCH,HCH,SCH,CPCH,VCH)
      FGTR=FGTR+GCH
      FHTR=FHTR+HCH
      FSTR=FSTR+SCH
      FCPTR=FCPTR+CPCH
      GR=GR+GCH
      HR=HR+HCH
      SR=SR+SCH
      CPR=CPR+CPCH
!---- now dG/dP
      IF (VNEED) THEN
        DG1=GCH
        PJ=P+DXP
        CALL LANDHP11(PJ,GCH,HCH,SCH,CPCH,VCH)
        DG2=GCH
        VCH=(DG2-DG1)/DXP
      END IF
      VOLUM=VOLUM+VCH
      FVTR=FVTR+VCH
!-----
      END IF
!********************************************************************************
!-----
!===== Bragg-Williams order function from Holland and Powell 2011.
!===== must be used with above volume function, VOLVO=5
!===== what is FAC???? (assume to divide DH, WW and DV by FAC)
!===== V11  VAA, D1,  D2,  D3,   VLN
!           A0   K0   K0'  K0''  (0=no trans, 1=Landau, 2=Bragg-Williams, 3=aquous, 4=melt)
!===== BW1  D4,     D5,     D6, D7,  D8,    D9
!           deltaH  deltaV  W   Wv   n(Si)  Factor
!-----
!!!      IF (VOLVO.EQ.5.AND.(DABS(VLN)-2.0D0).LT.1D-20) THEN
      IF (VOLVO.EQ.5.AND.(DABS(VLN-2.0D0)).LT.1D-20) THEN
!-----
      DXP=P/1D5
      PJ=P
      CALL BRAWIHP11(PJ,GCH,HCH,SCH,CPCH,VCH)
      FGTR=FGTR+GCH
      FHTR=FHTR+HCH
      FSTR=FSTR+SCH
      FCPTR=FCPTR+CPCH
      GR=GR+GCH
      HR=HR+HCH
      SR=SR+SCH
      CPR=CPR+CPCH
!---- now dG/dP
      IF (VNEED) THEN
        DG1=GCH
        PJ=P+DXP
        CALL BRAWIHP11(PJ,GCH,HCH,SCH,CPCH,VCH)
        DG2=GCH
        VCH=(DG2-DG1)/DXP
        PJ=P
      END IF
      VOLUM=VOLUM+VCH
      FVTR=FVTR+VCH
!-----
      END IF
!-----
!-----
!********************************************************************************
!  Holland and Powell 2011
!  melts testing with VLN=5
!  solution by Doug Tinkham
!
!===== use dkdt for melts from Holland and Powell 2011.
!===== needs data from volume function, VOLVO=5
!===== data needed: A0, K0, K0', DKDT
!===== is KAPS=4 or what??
!===== V11  VAA, D1,  D2,  D3,    VLN
!           A0   K0   K0'  K0''  (0=no trans, 1=Landau, 2=Bragg-Williams, 3=aquous, 4=melt)
!===== DK1  VL0
!           dkdt
!-----
!! variables used by Doug Tinkham (+k0,a0,dkdt)
!!      REAL(8) pref,ivpo,k0p,k0pp,tk,tref,vr,kt,v1t,a,b,c,ivdp,vdp,pkb
!!!      IF (VOLVO.EQ.5.AND.(DABS(VLN)-5.0D0).LT.1D-20) THEN
      IF (VOLVO.EQ.5.AND.(DABS(VLN-5.0D0)).LT.1D-20) THEN
      PK=P*1.0D-3
      PK0=1.0D-3
      A0=VAA
      K0=D1
      K01=D2
      DKDT=VL0
!
      pref=PK0
      ivpo=0.0D0
      ao=A0
      k0p=D2
      k0pp=D3
      tk=T
      tref=T0
      pkb=PK
      vr=V0R
!
!
      kt = k0 + dkdt*(tk-tref)
      v1t=vr*DEXP(ao*(tk-tref))
      a=(1.0+k0p)/(1.0+k0p+kt*k0pp)
      b=k0p/kt-k0pp/(1.0+k0p)
      c=(1.0+k0p+kt*k0pp)/(k0p*k0p+k0p-kt*k0pp)
      pth=0.0
      ivdp = v1t*(pkb-ivpo)*(1-a) &
         -v1t*(a*(1+b*(pkb-ivpo-pth))**(1 - c))/(b*(-1 + c)) &
         +v1t*(a*(1-b*pth)**(1-c))/(b*(-1 + c))
      vpt = v1t*(1 - a*(1 - (1 + b*((pkb-pref) - pth))**(-c)))
!
!!      WRITE (UNIT=6,FMT='(''v1t  '',F20.10)') v1t
!!      WRITE (UNIT=6,FMT='(''ivdp '',F20.10)') ivdp
!!      WRITE (UNIT=6,FMT='(''vpt  '',F20.10)') vpt
!
      VOLUM=vpt
      FVVOL=VOLUM-V0R
      FGVOL=1D3*ivdp
!
      GR=GR+FGVOL
!-----
      END IF
!-----
!-----
!******************************************************************************
!  Holland and Powell 2011:
!  The new thermal expansion expressions used in
!  TEOS are not suitable for melt end-members, as they
!  are based on a vibrational model. The experimental
!  data currently available do not warrant anything more
!  elaborate than constant thermal expansion at the
!  temperatures investigated (Lange, 1997), and so the
!  constant thermal expansion and linear temperature
!  dependence of bulk modulus as in HP98 is retained for
!  melt end-members.
!
!  From the above, the function below is guessed.
!  solution inspired by Doug Tinkham
!*************************************************************************
!===== use dkdt for melts from Holland and Powell 2011.
!===== needs data from volume function, VOLVO=5
!===== data needed: A0, K0, K0', DKDT
!===== is KAPS=4 or what??
!===== V11  VAA, D1,  D2,  D3,    VLN
!           A0   K0   K0'  K0''  (0=no trans, 1=Landau, 2=Bragg-Williams, 3=aquous, 4=melt)
!===== DK1  VL0
!           dkdt  (not dKdT/1D3 in table) 
!-----
!!!      IF (VOLVO.EQ.5.AND.(DABS(VLN)-4.0D0).LT.1D-20) THEN
      IF (VOLVO.EQ.5.AND.(DABS(VLN-4.0D0)).LT.1D-20) THEN
      PK=P*1.0D-3
      PK0=1.0D-3
      A0=VAA
      K0=D1
      K01=D2
      K02=D3
!
!! the following provided by Doug Tinkham
!! kt=KTT, tk=T, tref=T0, v1t-FV1,vr=V0R, pkb=PK,pref=PK0
!      kt = k0 + dkdt*(tk-tref)
!      v1t=vr*DEXP(ao*(tk-tref))
!      a=(1.0+k0p)/(1.0+k0p+kt*k0pp)
!      b=k0p/kt-k0pp/(1.0+k0p)
!      c=(1.0+k0p+kt*k0pp)/(k0p*k0p+k0p-kt*k0pp)
!      pth=0.0
!      ivdp = v1t*(pkb-ivpo)*(1-a) &
!         -v1t*(a*(1+b*(pkb-ivpo-pth))**(1 - c))/(b*(-1 + c)) &
!         +v1t*(a*(1-b*pth)**(1-c))/(b*(-1 + c))
!      vpt = v1t*(1 - a*(1 - (1 + b*((pkb-pref) - pth))**(-c)))
!!
!      VOLUM=vpt
!      FVVOL=VOLUM-V0R
!      FGVOL=1D3*ivdp
!!
!-K02 is normally = -K01/K0
! Holland and Powell 2011: "The data set includes a term for K02 to anticipate for
! the rare occasion where a value other than K02=-K01/K0 might be warranted"
! For melts K02=-K01/K0??
!!      K02=-K01/K0
!
!!      DKDT=VL0/1.0D3
      DKDT=VL0
!
!!      KTT=K0*(1.0D0+DKDT*(T-T0))
      KTT=K0+DKDT*(T-T0)

! Volume at T and 1 Bar (hopo 98) Thermal expansion
!       FV1=V0R*(1.0D0+A0*(T-T0)-20.0D0*A0*(SQT-SQT0))
! this is pure guesswork
!!      FV1=V0R*(1.0D0+A0*(T-T0))
      FV1=V0R*DEXP(A0*(T-T0))
! Variables for hopo 11 using K0 at T and 1 Bar (=KTT)
      AA=(1.0D0+K01)/(1.0D0+K01+KTT*K02)
      BB=K01/KTT-K02/(1.0D0+K01)
      CC=(1.0D0+K01+KTT*K02)/(K01*K01+K01-KTT*K02)
!
!! Variables for hopo 11 using K0 at 25 C and 1 Bar (=K0)
!!      AA=(1.0D0+K01)/(1.0D0+K01+K0*K02)
!!      BB=K01/K0-K02/(1.0D0+K01)
!!      CC=(1.0D0+K01+K0*K02)/(K01*K01+K01-K0*K02)
!
! Volume at T and P (hopo 11). No thermal Pressure term, PTH=0
      PTH=0.0D0
      VOLUM=FV1*(1.0D0-AA*(1.0D0-(1.0D0+BB*(PK-PK0-PTH))**(-CC)))
!21aug2015 leave out PK0
      VOLUM=FV1*(1.0D0-AA*(1.0D0-(1.0D0+BB*(PK-PTH))**(-CC)))
      FVVOL=VOLUM-V0R
!
! Integral VdP (hopo 11) (PTH=0.0D0)
!      FV=(1.0D0-BB*PTH)**(1.0D0-CC)-(1.0D0+BB*(PK-PTH))**(1.0D0-CC)
      FV=(1.0D0)-(1.0D0+BB*(PK))**(1.0D0-CC)
      FF=AA*FV/(BB*(CC-1.0D0)*PK)
      FF=FF+1.0D0-AA
!- achtung: hier P-P0 statt P
!21aug2015 doch nur P statt (P-P0) (damit dG/dP = V)
!!      FGVOL=(P-P0)*FV1*FF
      FGVOL=(P)*FV1*FF
!
      GR=GR+FGVOL
!
      END IF
!
!*************************************************************************
!-----
!===== volume function from Gottschalk 1997.
!===== VG VTA,VTB
!-----
      IF (VOLVO.EQ.8) THEN
      VTB=VTB/10.0D0
      VOLUM=V0R*DEXP(VTA*(T-T0)-VTB*(P-P0))
      FVVOL=VOLUM-V0R
!      FGVOL=VOLUM/VTB*DEXP(VTA*(T-T0))*(1.0D0-DEXP(-VTB*(P-P0)))
!      FGVOL=V0R/VTB*(DEXP(VTA*(T-T0))-DEXP(VTA*(T-T0)-VTB*(P-P0)))
      FGVOL=V0R/VTB*DEXP(VTA*(T-T0))*(1.0D0-DEXP(-VTB*(P-P0)))
      GR=GR+FGVOL
      END IF
!*************************************************************************
!-----
!===== lambda transitions 1 (Berman and Brown 1985, prototype for dG/dP)
!===== T1 TQ1B(I),TRE(I),ASPK(I),BSPK(I),DHTR(I)
!===== T2 TEQ(I),DVTR(I),DVDT(I),DVDP(I)
!-----
      IF (TRTYP.EQ.0) THEN
      DXP=P/1D5
      DO I=1,NLANDA
       I001=I
       PJ=P
       CALL LANDA(I001,PJ,GCH,HCH,SCH,CPCH,VCH)
       FGTR=FGTR+GCH
       FHTR=FHTR+HCH
       FSTR=FSTR+SCH
       FCPTR=FCPTR+CPCH
       GR=GR+GCH
       HR=HR+HCH
       SR=SR+SCH
       CPR=CPR+CPCH
!---- now dG/dP
       IF (VNEED) THEN
         DG1=GCH
         PJ=P+DXP
         CALL LANDA(I001,PJ,GCH,HCH,SCH,CPCH,VCH)
         DG2=GCH
         VCH=(DG2-DG1)/DXP
       END IF
       VOLUM=VOLUM+VCH
       FVTR=FVTR+VCH
      END DO
      END IF
!*************************************************************************
!-----
!===== lambda transitions 2 (Helgesson et al. 1978, SUPCRT)
!===== CSK ASPK(I),BSPK(I),DVDP(I),TQ1B(I),DHTR(I),DVTR(I),TEQ(I)
!-----
      IF (TRTYP.EQ.1) CALL SUPTRAN2
!-----
!===== lambda transitions ?
!===== TL1 TKRI,SMA
!-----
      IF (TL1.AND.(T.LT.TKRI)) THEN
      Q4=(1.0D0-T/TKRI)
      Q2=DSQRT(Q4)
      FGTR=-SMA*(TKRI-T)*Q2+SMA*TKRI*Q4*Q2/3.0D0
      GR=GR-SMA*(TKRI-T)*Q2+SMA*TKRI*Q4*Q2/3.0D0
      END IF
!*************************************************************************
!-----
!===== disorder contributions
!===== D1 D1,D4,D3,D8,D6
!===== D2 D2,D5,TD0,TDMAX,VADJ
!-----
      IF (DIS.AND.DABS(TD0).GT.1D-20.AND.DABS(TDMAX).GT.1D-20.AND.T.GT.TD0) THEN
      TD=DMIN1(T,TDMAX)
      FCPDIS=D1+D2*TD+D3/(TD*TD)+D4/DSQRT(TD)+D5*TD*TD+D6/TD &
      +D7*DSQRT(TD)+D8/(TD**3)+D9*(TD**3)
      CPRDT=D1*(TD-TD0)+D2*(TD*TD-TD0*TD0)/2.0D0 &
      -D3*(1.0D0/TD-1.0D0/TD0)+2D0*D4*(DSQRT(TD)-DSQRT(TD0)) &
      +D5*(TD**3-TD0**3)/3.0D0+D6*DLOG(TD/TD0) &
      +D7*(TD*DSQRT(TD)-TD0*DSQRT(TD0))*2.0D0/3.0D0 &
      -D8*(1.0D0/(TD*TD)-1.0D0/(TD0*TD0))/2.0D0 &
      +D9*(TD**4-TD0**4)/4.0D0
      CPRTDT=D1*DLOG(TD/TD0)+D2*(TD-TD0) &
      -D3*(1.0D0/(TD*TD)-1.0D0/(TD0*TD0))/2.0D0 &
      -2D0*D4*(1.0D0/DSQRT(TD)-1.0D0/DSQRT(TD0)) &
      +D5*(TD*TD-TD0*TD0)/2.0D0-D6*(1.0D0/TD-1.0D0/TD0) &
      +2D0*D7*(DSQRT(TD)-DSQRT(TD0)) &
      -D8*(1.0D0/(TD**3)-1.0D0/(TD0**3))/3.0D0 &
      +D9*(TD**3-TD0**3)/3.0D0
      IF (DABS(VADJ).GT.10.0D0) THEN
      VD=CPRDT/(10.0D0*VADJ)
      ELSE
      VD=0.0D0
      END IF
      FGDIS=CPRDT-(T*CPRTDT)+VD*(P-1.0D0)
      GR=GR+CPRDT-(T*CPRTDT)+VD*(P-1.0D0)
      FHDIS=CPRDT
      FSDIS=CPRTDT
      FVDIS=VD
      HR=HR+CPRDT
      SR=SR+CPRTDT
      CPR=CPR+FCPDIS
      VOLUM=VOLUM+VD
      END IF
!*************************************************************************
!-----
!===== that's all for now
!-----
      RETURN
      END
!-----
!******************************
      SUBROUTINE GAS
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
!-----END OF COMMON VARIABLES
      REAL(8) AA,BB,DGGAS
      COMMON /GORE/ AA,BB,DGGAS
      REAL(8) KUB,KUC,KUD,OFT,VREF,VOL,VGAS,VFL,X1,X2,X2I,X3,F001
!-----
      AA=AA0+T*AAT
      BB=BB0+T*BBT
      IF (PGAS.LE.0.0D0) THEN
      CALL SHOUTF
      WRITE (*,1010) PGAS
 1010 FORMAT (//' P(Gas) =',F9.2,'  The Pressure must be positive')
      STOP
      END IF
      IF (RDK) THEN
      OFT=P0*SQT
      KUB=-10.0D0*RT/P0
      KUC=AA/OFT-BB*BB+BB*KUB
      KUD=-AA*BB/OFT
      END IF
      IF (VDW) THEN
      KUB=-BB-10.0D0*RT/P0
      KUC=AA/P0
      KUD=-AA*BB/P0
      END IF
      CALL KUBIK(KUB,KUC,KUD,X1,X2,X2I,X3)
      IF (DABS(X2I).GT.1D-20) THEN
      VREF=X1
      ELSE
      VREF=DMAX1(X1,X2,X3)
      END IF
      IF (RDK) THEN
      OFT=PGAS*SQT
      KUB=-10.0D0*RT/PGAS
      KUC=AA/OFT-BB*BB+BB*KUB
      KUD=-AA*BB/OFT
      END IF
      IF (VDW) THEN
      KUB=-BB-10.0D0*RT/PGAS
      KUC=AA/PGAS
      KUD=-AA*BB/PGAS
      END IF
      CALL KUBIK(KUB,KUC,KUD,X1,X2,X2I,X3)
      IF (DABS(X2I).GT.1D-20) THEN
      VOL=X1
      ELSE
      VGAS=DMAX1(X1,X2,X3)
      VFL=DMIN1(X1,X2,X3)
      DGGAS=-1D0
      F001=PGAS
      IF (VFL.GT.BB) CALL DELGAS(VFL,VGAS,PGAS,F001)
      IF (DGGAS.GT.0.0D0) THEN
      LIQ=.TRUE.
      VOL=VFL
      ELSE
      VOL=VGAS
      END IF
      END IF
      CALL DELGAS(VREF,VOL,P0,PGAS)
      FGGAS=DGGAS
      GR=GR+DGGAS
      VOLUM=VOL/10.0D0
      FVGAS=VOL-V0R
      RETURN
      END
!-----
!******************************
      SUBROUTINE DELGAS(V1,V2,P1,P2)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
!-----END OF COMMON VARIABLES
      REAL(8) AA,BB,DGGAS
      COMMON /GORE/ AA,BB,DGGAS
      REAL(8) V1,V2,P1,P2
!-----
      IF (RDK) DGGAS=V2*P2-V1*P1-10*RT*DLOG((V2-BB)/(V1-BB)) &
      +(AA/(SQT*BB))*DLOG(V2*(V1+BB)/(V1*(V2+BB)))
      IF (VDW) DGGAS=V2*P2-V1*P1-10*RT*DLOG((V2-BB)/(V1-BB)) &
      -AA*(1.0D0/V2-1.0D0/V1)
      DGGAS=DGGAS/10.0D0
      RETURN
      END
!-----
!******************************
      SUBROUTINE KUBIK(B,C,D,X1,X2,X2I,X3)
      IMPLICIT NONE
      REAL(8) B,C,D,Q,P,R,PI,PHI3,FF,X1,X2,X2I,X3
      PI=3.14159263538979D0
      IF (DABS(C).LT.1D-20.AND.DABS(D).LT.1D-20) THEN
      X1=-B
      X2=0.0D0
      X2I=0.0D0
      X3=0.0D0
      RETURN
      END IF
      Q=((2.D0*B*B*B)/(27.D0)-(B*C)/(3.D0)+D)/2.D0
      P=(3.D0*C-B*B)/(9.D0)
      FF=DABS(P)
      R=DSQRT(FF)
      FF=R*Q
      IF (FF.LT.0.0D0) R=-R
      FF=Q/(R*R*R)
      IF (P.GT.0.0D0) THEN
      PHI3=DLOG(FF+DSQRT(FF*FF+1.D0))/3.D0
      X1=-R*(DEXP(PHI3)-DEXP(-PHI3))-B/(3.D0)
      X2I=1D0
      ELSE
      IF (Q*Q+P*P*P.GT.0.0D0) THEN
      PHI3=DLOG(FF+DSQRT(FF*FF-1.D0))/3.D0
      X1=-R*(DEXP(PHI3)+DEXP(-PHI3))-B/(3.D0)
      X2I=1D0
      ELSE
      PHI3=DATAN(DSQRT(1.D0-FF*FF)/FF)/3.D0
      X1=-2.D0*R*DCOS(PHI3)-B/(3.D0)
      X2=2.D0*R*DCOS(PI/3.D0-PHI3)-B/(3.D0)
      X2I=0.0D0
      X3=2.D0*R*DCOS(PI/3.D0+PHI3)-B/(3.D0)
      END IF
      END IF
      RETURN
      END
!-----
!****************************** CORK by Doug Tinkham
      SUBROUTINE CSGAS
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
!-----END OF COMMON VARIABLES
      REAL(8) AA,BB,DGGAS
      COMMON /GORE/ AA,BB,DGGAS
      REAL(8) VREF,VOL,FVHALF,THRHALF,TWOTHRD,RKJ,PREF
      REAL(8) CSA0,CSA1,CSB0,CSC0,CSC1,CSD0,CSD1
      REAL(8) PKB,CS1A,CS1B,CS1C,CS1D,DGGAS0,DGGAS1,PCRG,TCRG
      LOGICAL(4) CS1
!-----
      CS1=.TRUE.
      PCRG=AA0
      TCRG=BB0
!     Data for HP91 Corresponding States (units in kbar, kj)
      FVHALF=5.0D0/2.0D0
      THRHALF=3.0D0/2.0D0
      TWOTHRD=2.0D0/3.0D0
      RKJ=R*1.0D-3
      CSA0=5.45963D-5
      CSA1=-8.6392D-6
      CSB0=9.18301D-4
      CSC0=-3.30558D-5
      CSC1=2.30524D-6
      CSD0=6.93054D-7
      CSD1=-8.38293D-8
      IF (PGAS.LE.0.0D0) THEN
      CALL SHOUTF
      WRITE (*,1010) PGAS
 1010 FORMAT (//' P(Gas) =',F9.2,'  The Pressure must be positive')
      STOP
      END IF
!      Need to calc vol at P0,T here, then at P,T.
      IF (CS1) THEN
      CS1A=CSA0*(TCRG**FVHALF)/PCRG+CSA1*(TCRG**THRHALF)/PCRG*T
      CS1B=CSB0*TCRG/PCRG
      CS1C=CSC0*(TCRG/(PCRG**THRHALF))+CSC1/(PCRG**THRHALF)*T
      CS1D=CSD0*TCRG/(PCRG**2.0D0)+CSD1/(PCRG**2.0D0)*T
      PKB=PGAS*0.001D0
      PREF=P0*0.001D0
!     VREF is volume at ref. pressure = 1 bar
      VREF = (RKJ*T/PREF)+CS1B-(CS1A*RKJ*DSQRT(T))/((RKJ*T+CS1B*PREF) &
      *(RKJ*T+2.0D0*CS1B*PREF)) + CS1C*DSQRT(PREF)+CS1D*PREF
!     VOL at P=PGAS and T. PGAS could be specified as a fraction of P by user.
      VOL = (RKJ*T/PKB)+CS1B-(CS1A*RKJ*DSQRT(T))/((RKJ*T+CS1B*PKB) &
      *(RKJ*T+2.0D0*CS1B*PKB)) + CS1C*DSQRT(PKB)+CS1D*PKB
      END IF
!     Now, calculate volume/fugacity contribution to G from RTLnf
      IF (CS1) THEN
      DGGAS0=0.0D0
      DGGAS1=0.0D0
!      DGGAS0=RKJ*T*DLOG(PREF)+CS1B*PREF+(TWOTHRD)*CS1C*PREF*DSQRT(PREF) &
!      + CS1D*0.5D0*(PREF**2)+CS1A/(CS1B*DSQRT(T)) * (DLOG(RKJ*T+CS1B*PREF) &
!      - DLOG(RKJ*T+2.0D0*CS1B*PREF))
!      DGGAS1=RKJ*T*DLOG(PKB)+CS1B*PKB+(TWOTHRD)*CS1C*PKB*DSQRT(PKB) &
!      + CS1D*0.5D0*(PKB**2)+CS1A/(CS1B*DSQRT(T)) * (DLOG(RKJ*T+CS1B*PKB) &
!      - DLOG(RKJ*T+2.0D0*CS1B*PKB))
!      DGGAS=DGGAS1-DGGAS0
!     Straight HP eq out of paper, seems to be what tcalc does, but I don't like
      DGGAS=RKJ*T*DLOG(1.0D3*PKB)+CS1B*PKB+TWOTHRD*CS1C*PKB*DSQRT(PKB) &
      +CS1D/2.0D0*(PKB**2)+CS1A/(CS1B*DSQRT(T))*(DLOG(RKJ*T+CS1B*PKB) &
      -DLOG(RKJ*T+2.0D0*CS1B*PKB))
      DGGAS=1.0D3*DGGAS
      FGGAS=DGGAS
      GR=GR+DGGAS
      VOLUM=VOL
      FVGAS=VOL-V0R
      END IF
      RETURN
      END
!-----
!****************************** CORK with cubic equation
      SUBROUTINE CORK
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
!-----END OF COMMON VARIABLES
      REAL(8) AA,BB,DGGAS
      COMMON /GORE/ AA,BB,DGGAS
      REAL(8) PCRG,TCRG,CC,DD,PKB,RTKB,VMRK,DGGAS3,P00
      REAL(8) KUB,KUC,KUD,OFT,VREF,VOL,X1,X2,X2I,X3
!
      REAL(8) FVHALF,THRHALF,TWOTHRD,PREF
      REAL(8) CSA0,CSA1,CSB0,CSC0,CSC1,CSD0,CSD1
!      REAL(8) CS1A,CS1B,CS1C,CS1D,DGGAS0,DGGAS1
!-----
      IF (PGAS.LE.0.0D0) THEN
      CALL SHOUTF
      WRITE (*,1010) PGAS
 1010 FORMAT (//' P(Gas) =',F9.2,'  The Pressure must be positive')
      STOP
      END IF
!-----
      PCRG=AA0
      TCRG=BB0
      PKB=PGAS*0.001D0
      RTKB=T*R*0.001D0
      PREF=P0*0.001D0
      P00=0.0D0
!-----
!     Data for HP91 Corresponding States (units in kbar, kj)
      FVHALF=5.0D0/2.0D0
      THRHALF=3.0D0/2.0D0
      TWOTHRD=2.0D0/3.0D0
      CSA0=5.45963D-5
      CSA1=-8.6392D-6
      CSB0=9.18301D-4
      CSC0=-3.30558D-5
      CSC1=2.30524D-6
      CSD0=6.93054D-7
      CSD1=-8.38293D-8
!      Need to calc vol at P0,T here, then at P,T.
      AA=CSA0*(TCRG**FVHALF)/PCRG+CSA1*(TCRG**THRHALF)/PCRG*T
      BB=CSB0*TCRG/PCRG
      CC=CSC0*(TCRG/(PCRG**THRHALF))+CSC1/(PCRG**THRHALF)*T
      DD=CSD0*TCRG/(PCRG**2.0D0)+CSD1/(PCRG**2.0D0)*T
!=====reference volume at 1 Bar and T. Assume gas
      OFT=PREF*DSQRT(T)
      KUB=-RTKB/PREF
      KUC=AA/OFT-BB*BB+BB*KUB
      KUD=-AA*BB/OFT
      CALL KUBIK(KUB,KUC,KUD,X1,X2,X2I,X3)
      IF (DABS(X2I).GT.1D-20) THEN
      VREF=X1
      ELSE
      VREF=DMAX1(X1,X2,X3)
      END IF
!-
      IF (PREF.GT.P00) THEN
      VREF=VREF+DD*(PREF-P00)+CC*DSQRT(PREF-P00)
      END IF
!===== volume at P and T, Assume gas
      OFT=PKB*DSQRT(T)
      KUB=-RTKB/PKB
      KUC=AA/OFT-BB*BB+BB*KUB
      KUD=-AA*BB/OFT
      CALL KUBIK(KUB,KUC,KUD,X1,X2,X2I,X3)
      IF (DABS(X2I).GT.1D-20) THEN
      VMRK=X1
      ELSE
      VMRK=DMAX1(X1,X2,X3)
      END IF
!-
      IF (PKB.GT.P00) THEN
      VOL=VMRK+DD*(PKB-P00)+CC*DSQRT(PKB-P00)
      ELSE
      VOL=VMRK
      END IF
!=====
      DGGAS3=VMRK*PKB-VREF*PREF-RTKB*DLOG((VMRK-BB)/(VREF-BB)) &
      +(AA/(DSQRT(T)*BB))*DLOG(VMRK*(VREF+BB)/(VREF*(VMRK+BB)))
      IF (PKB.GT.P00) THEN
      DGGAS3=DGGAS3+DD/2.0D0*(PKB-P00)**2 &
      +2.0D0/3.0D0*CC*(PKB-P00)**1.5D0
      END IF
!-
      DGGAS=1.0D3*DGGAS3
      FGGAS=DGGAS
      GR=GR+DGGAS
      VOLUM=VOL
      FVGAS=VOL-V0R
      RETURN
      END
!-----
!******************************
!===== lambda transitions 1 (Berman and Brown 1985, prototype for dG/dP)
      SUBROUTINE LANDA(K,PJ,GCH,HCH,SCH,CPCH,VCH)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
!-----END OF COMMON VARIABLES
      REAL(8) A11,B11,C11,L22,CTR,T1,T2,DHSPK,DSSPK,GSPK, &
      TQP,DSTR,PJ,GCH,HCH,SCH,CPCH,VCH
      INTEGER(4) K
!-----
      L22=BSPK(K)**2
      CTR=TEQ(K)*(PJ-1.0D0)
      TQP=TQ1B(K)+CTR
      T2=DMIN1(T,TQP)
      IF (DABS(BSPK(K)).GT.1D-20) THEN
        T1=-ASPK(K)/BSPK(K)+CTR
      ELSE
        T1=T2
      END IF
      IF (T.LT.T1) THEN
        CPCH=0.0D0
        DHSPK=0.0D0
        DSSPK=0.0D0
        GSPK=0.0D0
        HCH=0.0D0
        SCH=0.0D0
      ELSE
        A11=-CTR*(T1**2)
        B11=T1*(2.0D0*CTR+T1)
        C11=-(CTR+2.0D0*T1)
        DHSPK=A11*(T2-T1)+B11*(T2**2-T1**2)/2.0D0 &
        +C11*(T2**3-T1**3)/3.0D0+(T2**4-T1**4)/4.0D0
        DSSPK=A11*(DLOG(T2/T1))+B11*(T2-T1) &
        +C11*(T2**2-T1**2)/2.0D0+(T2**3-T1**3)/3.0D0

!following not sure ??
        DSTR=DSSPK


        GSPK=(DHSPK-T*DSSPK)*L22
        HCH=DHSPK*L22
        SCH=DSSPK*L22
        IF (T.LT.TQP) THEN
          CPCH=((T-CTR)*(T-T1)**2)*L22
        ELSE
          CPCH=0.0D0
          SCH=DHTR(K)/TQ1B(K)
          GCH=GSPK-DSTR*(T-TQP)
          HCH=FHTR+DSTR*TQP
          SCH=FSTR+DSTR
        END IF
        GSPK=GSPK+DVDT(K)*(PJ-1.0D0)*(T2-298.15D0) &
        +(DVDP(K)/2.0D0)*(PJ*PJ-1.0D0)-DVDP(K)*(PJ-1.0D0)
      END IF
      VCH=DVDT(K)*(T2-298.15D0)+DVDP(K)*(PJ-1.0D0)
      GCH=GSPK
!----
      RETURN
      END
!-----
!******************************
!===== Landau order function from Holland and Powell 1998.
!===== must be used with volume function VOLVO=4
!===== VHP  VTA, VTB,  TKRI,      SMA,   VPA
!           A0   K(T0) Tcrit(T0)  Smax   Vmax
!===== VH2  D1, D2,  D3
!           a2  K'   dkdt
      SUBROUTINE LANDHP98(PJ,GCH,HCH,SCH,CPCH,VCH)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
!-----END OF COMMON VARIABLES
      REAL(8) PJ,GCH,HCH,SCH,CPCH,VCH,A00,Q40,Q4,Q20,Q2,Q60,Q6, &
      DGLA,FVLA,FVDP,FHLA,FSLA,DKDT,KTT,TKR,TEF,VMA,PK
!      INTEGER(4) K
!-----
!-  3.Jan 2012: Vergleich mit G-table zeigt, dass bei KTT und GCH, 
!-  T vewrwendet wird (wie gehabt)
!-  does not make any sense at all
      PK=PJ*1.0D-3
      A00=VTA
      DKDT=D3
      VMA=VPA

! comparing with G-table it seems that although DKDT in the
! database is not always -1.5D-4, the following is used
      DKDT=-1.5D-4

      IF (DABS(DKDT).LT.1D-20) DKDT=-1.5D-4
      TKR=TKRI+(VMA/SMA)*PJ
      IF (T.LT.TKR) THEN
      TEF=T
      ELSE
      TEF=TKR
      END IF
!-
      KTT=VTB*(1.0D0+DKDT*(T-T0))
!-
      Q40=1.0D0-T0/TKRI
      Q4=1.0D0-TEF/TKR
      Q20=DSQRT(Q40)
      Q2=DSQRT(Q4)
      Q60=Q20*Q40
      Q6=Q2*Q4
      DGLA=SMA*((TEF-TKR)*Q2+(1.0D0/3.0D0)*TKR*Q6)
      FVLA=VMA*Q20*(1.0D0+A00*(TEF-T0)-20.0D0*A00*(DSQRT(TEF)-SQT0))
      FVDP=(FVLA*KTT/3.0D0)*((1.0D0+4.0D0*PK/KTT)**0.75D0-1.0D0)
      FVDP=FVDP*1D3
      FHLA=SMA*TKRI*(Q20-1.0D0/3.0D0*Q60)
      FSLA=SMA*Q20
!-
      GCH=(FHLA-T*FSLA+FVDP+DGLA)
      CPCH=0.0D0
      HCH=0.0D0
      SCH=0.0D0
      CPCH=0.0D0
      VCH=0.0D0
!----
      RETURN
      END
!-----
!******************************
!===== Landau order function from Holland and Powell 2011.
!===== must be used with volume function VOLVO=5
!===== it is almost the same as 1998, DKDT always -1.5D-4
!===== and different variables used ....
!===== V11  VAA, D1,  D2,  D3,   VLN
!           A0   K0   K0'  K0''  (1=Landau, 2=Bragg-Williams, 3=aquous, 4=melt)
!===== LA1  TKRI,      SMA,  VPA
!           Tcrit(T0)  Smax  Vmax
      SUBROUTINE LANDHP11(PJ,GCH,HCH,SCH,CPCH,VCH)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
!-----END OF COMMON VARIABLES
      REAL(8) PJ,GCH,HCH,SCH,CPCH,VCH,A00,Q40,Q4,Q20,Q2,Q60,Q6, &
      DGLA,FVLA,FVDP,FHLA,FSLA,DKDT,KTT,TKR,TEF,VMA,PK
!      INTEGER(4) K
!-----
!-  3.Jan 2012: Vergleich mit G-table zeigt, dass bei KTT und GCH, 
!-  T vewrwendet wird (wie gehabt)
!-  wrong equations used in hopo98: (must be TKRI, not TKR
!-        Q4=1.0D0-TEF/TKR
!-                     ---
!-        DGLA=SMA*((TEF-TKR)*Q2+(1.0D0/3.0D0)*TKR*Q6)
!-                                             ---
      PK=PJ*1.0D-3
      A00=VAA
      DKDT=-1.5D-4
      VMA=VPA
      VTB=D1
!      IF (DKDT.EQ.0.0D0) DKDT=-1.5D-4
      TKR=TKRI+(VMA/SMA)*PJ
      IF (T.LT.TKR) THEN
      TEF=T
      ELSE
      TEF=TKR
      END IF
!-
      KTT=VTB*(1.0D0+DKDT*(T-T0))
!-
      Q40=1.0D0-T0/TKRI
      Q4=(TKR-TEF)/TKRI
      Q20=DSQRT(Q40)
      Q2=DSQRT(Q4)
      Q60=Q20*Q40
      Q6=Q2*Q4
      DGLA=SMA*((TEF-TKR)*Q2+(1.0D0/3.0D0)*TKRI*Q6)
      FVLA=VMA*Q20*(1.0D0+A00*(TEF-T0)-20.0D0*A00*(DSQRT(TEF)-SQT0))
      FVDP=(FVLA*KTT/3.0D0)*((1.0D0+4.0D0*PK/KTT)**0.75D0-1.0D0)
      FVDP=FVDP*1D3
      FHLA=SMA*TKRI*(Q20-1.0D0/3.0D0*Q60)
      FSLA=SMA*Q20
!-
      GCH=(FHLA-T*FSLA+FVDP+DGLA)
      CPCH=0.0D0
      HCH=0.0D0
      SCH=0.0D0
      CPCH=0.0D0
      VCH=0.0D0
!----
      RETURN
      END
!-----
!******************************
!===== Bragg-Williams order function from Holland and Powell 2011.
!===== equations in Holland and Powell 1996a, p 1414 and Appendix 2)
!===== must be used with volume function VOLVO=5
!===== what is FAC???? (assume to divide DH, WW and DV by FAC)
!===== V11  VAA, D1,  D2,  D3,   VLN
!           A0   K0   K0'  K0''  (0=no trans, 1=Landau, 2=Bragg-Williams, 3=aquous, 4=melt)
!===== BW1  D4,     D5,     D6, D7,  D8,    D9
!           deltaH  deltaV  W   Wv   n(Si)  Factor
      SUBROUTINE BRAWIHP11(PJ,GCH,HCH,SCH,CPCH,VCH)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
!-----END OF COMMON VARIABLES
      REAL(8) PJ,GCH,HCH,SCH,CPCH,VCH,PK,DH,DV,WW,NSI,FAC,TKR,AA,BB, &
      F1,F2,F3,AID1,AID2,RTLNAID1,RTLNAID2,RTLNA1,RTLNA2, &
      DIF,QOB,QUN,Q2,XX0
!      INTEGER(4) K
!-----
      PK=PJ*1.0D-3
      DH=D4
      DV=D5
      WW=D6+D7*PJ
      NSI=D8
      FAC=D9
      DH=DH/FAC
      WW=WW/FAC
      DV=DV/FAC
      TKR=2.0D0*WW/R/(1.0D0+NSI)
!----
      QUN=0.0D0
      QOB=1.0D0
      DIF=1.0D0
!      XX0=1.0D0
!---- 1D-15 is smallest possible in REAL(8)
!---- e.g. for anorthite: (P=1Bar) 1D-13 is necessary
!---- e.g. for anorthite: (P=0.001Bar) 1D-15 is necessary
!---- try different approach ???
      DO WHILE (DABS(DIF).GT.1D-14)
        Q2=(QOB+QUN)/2.0D0
        F1=(NSI-NSI*Q2)*(1.0D0-Q2)
        F2=(1+NSI*Q2)*(NSI+Q2)
        F3=(NSI/(NSI+1.0D0))*R*T*DLOG(F1/F2)
        AA=DH-WW+DV*PJ
        BB=2.0D0*WW
        XX0=AA+BB*Q2+F3
        IF (XX0.GT.0.0D0) THEN
         QUN=Q2
        ELSE
         QOB=Q2
        END IF
        DIF=DABS(QOB-QUN)
      END DO
!   99 CONTINUE
!!      WRITE (UNIT=6,FMT='(/,''NAM '',A)') NAM
!!      WRITE (UNIT=6,FMT='(''T,TC,PJ,TKR '',4F20.10)') T,TC,PJ,TKR
!!      WRITE (UNIT=6,FMT='(''WW,Q2,XX0   '',3F20.10)') WW,Q2,XX0
!----
      AID1=(1.0D0/(NSI+1.0D0)**(NSI+1.0D0))* &
      (1.0D0+NSI*Q2)*(NSI+Q2)**NSI
      AID2=(1.0D0/NSI**NSI)*(1.0D0+NSI*Q2)**(1.0D0/(NSI+1.0D0))* &
      (NSI-NSI*Q2)**(NSI/(NSI+1.0D0))* &
      (1.0D0-Q2)**(NSI/(NSI+1.0D0))* &
      (NSI+Q2)**(NSI*NSI/(NSI+1.0D0))
      RTLNAID1=R*T*DLOG(AID1)
      RTLNAID2=R*T*DLOG(AID2)
      RTLNA1=RTLNAID1+WW*(1.0D0-Q2)*(1.0D0-Q2)
      RTLNA2=RTLNAID2+WW*Q2*Q2
!----- attention: in database: ordered phase (not hight T)
      GCH=RTLNA1*FAC
      HCH=0.0D0
      SCH=0.0D0
      CPCH=0.0D0
      VCH=0.0D0
!      GR=GR+FGTR
!----
      RETURN
      END
!-----
!******************************
      SUBROUTINE SUPTRAN
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
!-----END OF COMMON VARIABLES
      REAL(8) A11,B11,C11,TTR0,HTRSUP,VTRSUP,TSLOP,TUN,TOB, &
      TTUN,TTOB,CPRDT,CPRTDT,PTRSUP,POB
      INTEGER(4) I
      LOGICAL(4) FERTIG
!
!---- for quartz and coesite see reac92d_v12.f (from supcrt95)
!---- subroutine: quartz
!
      FGCP=0.0D0
      FHCP=0.0D0
      FSCP=0.0D0
      FGTR=0.0D0
      FHTR=0.0D0
      FSTR=0.0D0
!---
      TUN=T0
!---
      DO 500,I=1,NLANDA
       A11=ASPK(I)
       B11=BSPK(I)
       C11=DVDP(I)
       TTR0=TQ1B(I)
       HTRSUP=DHTR(I)
       IF (HTRSUP.GE.999999.0D0) HTRSUP=0.0D0
       VTRSUP=DVTR(I)
       IF (VTRSUP.GE.999999.0D0) VTRSUP=0.0D0
       TSLOP=TEQ(I)
       IF (TSLOP.GE.999999.0D0) TSLOP=0.0D0
       IF (DABS(TTR0).LT.1D-20.OR.T.LT.TTR0) THEN
        TOB=T
        FERTIG=.TRUE.
       ELSE
        TOB=TTR0
        FERTIG=.FALSE.
       END IF
!
       TTOB=TOB*TOB
       TTUN=TUN*TUN
       CPR=A11+B11*TOB+C11/(TTOB)
       CPRDT=A11*(TOB-TUN)+B11*(TTOB-TTUN)/2.0D0 &
       -C11*(1.0D0/TOB-1.0D0/TUN)
       CPRTDT=A11*DLOG(TOB/TUN)+B11*(TOB-TUN) &
       -C11*(1.0D0/TTOB-1.0D0/TTUN)/2.0D0
       FHCP=FHCP+CPRDT
       FSCP=FSCP+CPRTDT
       IF (.NOT.FERTIG) THEN
        FHTR=FHTR+HTRSUP
        FSTR=FSTR+HTRSUP/TTR0
        PTRSUP=P0+(T-TTR0)*TSLOP
        IF (DABS(TSLOP).LT.1D-20) PTRSUP=1D20
        POB=P
        IF (P.GT.PTRSUP) POB=PTRSUP
        FVTR=FVTR+VTRSUP
        FGTR=FGTR+VTRSUP*(POB-P0)
       END IF
       TUN=TTR0
       IF (FERTIG) GOTO 501
  500 CONTINUE
!---
  501 CONTINUE
      FCP0=ASPK(1)+BSPK(1)*T0+DVDP(1)/TT0
      FCPCP=CPR-FCP0
      FG0=H0R-T0*S0R
      VOLUM=V0R
      HR=H0R+FHCP
      SR=S0R+FSCP
      GR=HR-T*SR
      FGCP=GR-FG0
!---
      FVVOL=0.0D0
      FGVOL=(P-P0)*V0R
      GR=GR+FGVOL
!---
      VOLUM=VOLUM+FVTR
      HR=HR+FHTR
      SR=SR+FSTR
      GR=GR+FHTR-T*FSTR+FGTR
!-----
      RETURN
      END
!-----
!******************************
      SUBROUTINE SUPTRAN2
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
!-----END OF COMMON VARIABLES
      REAL(8) A11(4),B11(4),C11(4),TTR0(4),HTRSUP(4),VTRSUP(4), &
      TSLOP(4),TUN,TOB,TTUN,TTOB,CPRDT,CPRTDT,TTRP(4),PTRT(4), &
      GRNOP
      INTEGER(4) I
      LOGICAL(4) FERTIG
      CHARACTER(20) CH001
!
!---- for quartz and coesite see reac92d_v12.f (from supcrt95)
!---- subroutine: quartz
!
      FGCP=0.0D0
      FGTR=0.0D0
      FHTR=0.0D0
      FSTR=0.0D0
!---
!---- assign variables and calculate TTRP (T of transition at P) and
!---- PTRT (P of transition at T)
      DO I=1,NLANDA
        A11(I)=ASPK(I)
        B11(I)=BSPK(I)
        C11(I)=DVDP(I)
        TTR0(I)=TQ1B(I)
        HTRSUP(I)=DHTR(I)
        IF (HTRSUP(I).GE.999999.0D0) HTRSUP(I)=0.0D0
        VTRSUP(I)=DVTR(I)/10.0D0
        IF (VTRSUP(I).GE.99999.0D0) VTRSUP(I)=0.0D0
        TSLOP(I)=TEQ(I)
        IF (TSLOP(I).GE.999999.0D0) TSLOP(I)=0.0D0
!       calculate TTRP
        IF (DABS(TSLOP(I)).LT.1D-20) THEN
          TTRP(I)=TTR0(I)
        ELSE
          TTRP(I)=TTR0(I)+(P-P0)/TSLOP(I)
        END IF
!       calculate PTRT
        IF (DABS(TSLOP(I)).LT.1D-20) THEN
          PTRT(I)=1D20
        ELSE
          PTRT(I)=P0+(T-TTR0(I))*TSLOP(I)
        END IF
!!      WRITE (6,1005) NAM,I,TTRP(I),PTRT(I)
!! 1005 FORMAT (' NAM,I,TTRP(I),PTRT(I) ',A,I4,2(F20.10))
      END DO
!---- 
!---- calculate CP and CP-integrals
      FHCP=0.0D0
      FSCP=0.0D0
      CPRDT=0.0D0
      CPRTDT=0.0D0
      HR=H0R
      SR=S0R
      GR=HR-T*SR
      DO I=1,NLANDA

        WRITE (UNIT=6,FMT='(''in loop nlanda='',i5)') NLANDA

        IF (I.EQ.1) THEN
          TUN=T0
        ELSE
           TUN=TTR0(I-1)
        END IF
        IF (DABS(TTR0(I)).LT.1D-20.OR.T.LT.TTR0(I)) THEN
         TOB=T
         FERTIG=.TRUE.
        ELSE
         TOB=TTR0(I)
         FERTIG=.FALSE.
        END IF
        TTUN=TUN*TUN
        TTOB=TOB*TOB
        CPR=A11(I)+B11(I)*T+C11(I)/(T*T)
        CPRDT=A11(I)*(TOB-TUN)+B11(I)*(TTOB-TTUN)/2.0D0 &
        -C11(I)*(1.0D0/TOB-1.0D0/TUN)
        CPRTDT=A11(I)*DLOG(TOB/TUN)+B11(I)*(TOB-TUN) &
        -C11(I)*(1.0D0/TTOB-1.0D0/TTUN)/2.0D0
        FHCP=FHCP+CPRDT
        FSCP=FSCP+CPRTDT
        HR=HR+CPRDT
        SR=SR+CPRTDT
        GR=GR+CPRDT-T*CPRTDT
        IF (.NOT.FERTIG) THEN
         FHCP=FHCP+HTRSUP(I)
         FSCP=FSCP+HTRSUP(I)/TTR0(I)
         HR=HR+HTRSUP(I)
         SR=SR+HTRSUP(I)/TTR0(I)
         GR=GR+HTRSUP(I)-T*HTRSUP(I)/TTR0(I)
        END IF
        IF (FERTIG) GOTO 101
      END DO
  101 CONTINUE
      GRNOP=GR

!!      HR=H0R+FHCP
!!      SR=S0R+FSCP
!!      GR=HR-T*SR
!----
!---- calculate integral (V)dP
      VOLUM=V0R
      DO I=1,NLANDA
        IF (T.GT.TTRP(I)) THEN
          VOLUM=VOLUM+VTRSUP(I)
        END IF
      END DO
      GR=GR+VOLUM*(P-P0)
      DO I=1,NLANDA
        IF (T.GT.TTR0(I).AND.P.GT.PTRT(I)) THEN
          GR=GR+VTRSUP(I)*(PTRT(I)-P0)
        END IF
      END DO



      IF (NAM.EQ.'QUARTZ'.OR.NAM.EQ.'COESITE') THEN
      CH001=NAM
      GR=GRNOP
      CALL quartz(CH001,P,T,TTR0(1),VOLUM,SR,HR,GR)

      END IF




!-----
      RETURN
      END
!-----
!********************************





!***********************************************************************

!*** quartz - Revises the standard molal Gibbs free energy (G), enthalpy
!***          (H), entropy (S), and volume (V) of quartz or coesite to
!***          account for V(T) > 0 using equations (109) through (115), 
!***          Helgeson et al. (1978). 


      SUBROUTINE quartz(mname,P,T,TtPr,V,S,H,G)

      IMPLICIT DOUBLE PRECISION (a-h,o-z)
!CdC.os9      IMPLICIT DOUBLE PRECISION (A-H,O-Z,a-h,o-z)

      CHARACTER(20)      mname 
      INTEGER           qphase
      DOUBLE PRECISION  k

      
!*** VPrTra = VPrTr(a-quartz) 
!*** Vdiff  = VPrTr(a-quartz) - VPrTr(coesite)
!*** k      = dPdTtr(a/b-quartz)  

      DATA VPrTra, Vdiff, k / 22.688d0, 2.047d0, 38.5d0 /

      DATA aa, ba, ca / 549.824D0,  0.65995D0, -4.973D-5 /
      DATA VPtTta, VPrTtb, Stran / 23.348D0, 23.72D0, 0.342D0 /

!***** set qphase = phase region of quartz
!CDC:
      Pr=1.0D0
      Tr=298.15D0


      IF ((T .LE. TtPr) .OR. (P .GE. (Pr + k*(T-TtPr)))) THEN
           qphase = 1
      ELSE
           qphase = 2
      END IF

!***** set Pstar and Sstar *****

      IF (T .LE. TtPr) THEN
           Pstar = Pr 
           Sstar = 0.0d0
      ELSE
           IF (qphase .EQ. 2) THEN
                Pstar = P
                Sstar = 0.0d0
           ELSE
                Pstar = Pr + k*(T-TtPr)
                Sstar = Stran
           END IF
      END IF

      IF (qphase .EQ. 2) THEN
!***** set volume to beta-quartz *****
           V = VPrTtb
      ELSE
!***** calculate volume of alpha-quartz per eqn (109) *****
      V = VPrTra + ca*(P-Pr) + (VPtTta - VPrTra - ca*(P-Pr))*(T-Tr) / &
          (TtPr + (P-Pr)/k - Tr)

      V=V/10.0D0

      END IF

      IF (mname .EQ. 'COESITE') V = V - Vdiff/10.0D0

!***** leading constant for [G,S]Vterm below 
!***** is a coversion factor (cal/cm**3/bar) 

      IF (mname .EQ. 'QUARTZ') THEN
        GVterm = 0.23901488d-1 * (VPrTra*(P-Pstar) + VPrTtb*(Pstar-Pr) - &
              0.5d0*ca*(2.0d0*Pr*(P-Pstar) - (P**2-Pstar**2)) - &
              ca*k*(T-Tr)*(P-Pstar) +  &
              k*(ba + aa*ca*k)*(T-Tr)*DLOG((aa + P/k)/(aa + Pstar/k)))
      ELSE
        GVterm = 0.23901488d-1 * ((VPrTra-Vdiff)*(P-Pstar) + &
              (VPrTtb-Vdiff)*(Pstar-Pr) - 0.5d0*ca*(2.0d0*Pr*(P-Pstar) -  &
              (P**2-Pstar**2)) - ca*k*(T-Tr)*(P-Pstar) +  &
              k*(ba + aa*ca*k)*(T-Tr)*DLOG((aa + P/k)/(aa + Pstar/k)))
      END IF

      SVterm = 0.23901488d-1 * (-k*(ba + aa*ca*k)* &
               DLOG((aa + P/k)/(aa + Pstar/k)) + ca*k*(P-Pstar)) -  &
               Sstar

      G = G + GVterm*4.184d0
      S = S + SVterm*4.184d0
      H = H + GVterm*4.184d0 + T*SVterm*4.184d0

      END

!************************************************************************





!-----
!********************************
      SUBROUTINE CHECKFROMCAL(ICOD)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      INCLUDE 'files.cmn'
!-----END OF COMMON VARIABLES
      INTEGER(4) II,I001,ICOD
      CHARACTER(16) CH001
      REAL(8) GVONG,GVONH,DELTG,ZAH,GDAT,HDAT,SDAT
!
      ZAH=999999.0D0*4.184D0
      DO II=(NUN+1),NPHA

       I001=II
       CH001=NAME(II)
       CALL DAREST(I001)
       GDAT=G0R
       HDAT=H0R
       SDAT=S0R
       IF (DABS(G0R).LT.1D-20) THEN
        WRITE (6,2000) CH001
 2000   FORMAT (' ',A,'    G0R = 0')
        GOTO 99
       END IF

       IF (DABS(G0R-ZAH).LT.1D-20) THEN
        WRITE (6,2002) CH001
 2002   FORMAT (' ',A,'    G0R = 999999')
        GOTO 99
       END IF
       IF (PHASID(II).EQ.'AQU') THEN
        GDAT=G0R*4.184D0
        HDAT=H0R*4.184D0
        SDAT=S0R*4.184D0
!        WRITE (6,2004) CH001
! 2004   FORMAT (' phase ',A,' is aqueous species from supcrt (in Cal)')
!        GOTO 99
       END IF
       GVONH=HDAT-298.15D0*SDAT
       CALL FROMCAL(I001,GVONH,GVONG)
       GVONG=GVONG*4.184D0
       DELTG=(GDAT-GVONG)
       WRITE (6,1000) CH001,GDAT,GVONG,DELTG,DELTG/4.184d0
 1000  FORMAT (1X,A,2X,'  G0R = ',f14.4, &
               '     G0(f) = ',f14.4, &
               '     dH0R = ',f15.5,' [J/mol] = ', &
                f15.5,' [Cal/mol]')

      IF (ICOD.EQ.1) THEN
      WRITE (6,3000)
 3000 FORMAT (' H0R is corrected according to G0R')
      H0R=H0R+DELTG
      CALL DASAVE(I001)
      END IF

   99 CONTINUE
      END DO
!
      RETURN
      END
!-----
!********************************
      SUBROUTINE FROMCAL(IP,GORIG,DELTG)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      INCLUDE 'files.cmn'
!-----END OF COMMON VARIABLES
      INTEGER(4) IP,IE,I,IOFE
      CHARACTER(2) ENAM(31)
      REAL(8) THES(31),DELTG,GORIG,THET0,DELTS,J2CAL
      DATA ENAM /'H',       'E',      'O',      'CA',    'MG', &
                 'NA',      'K',      'FE',     'MN',    'AL', &
                 'BA',      'SR',     'SI',     'CL',    'C',  &
                 'S',       'N',      'B',      'P',     'F',  &
                 'LI',      'BR',     'ZN',     'CD',    'PB', &
                 'CU',      'AG',     'TI',     'CO',    'NI', &
                 'ZR'/
      DATA THES / 65.34D0, -65.34D0, 102.575D0, 41.63D0,  32.68D0, &
                  51.30D0,  64.68D0,  27.28D0,  32.01D0,  28.35D0, &
                  62.42D0,  55.40D0,  18.81D0, 111.54D0,   5.74D0, &
                  31.80D0,  95.805D0,  5.90D0,  22.85D0, 101.395D0, &
                  29.12D0,  76.16D0,  41.63D0,  51.80D0,  65.06D0, &
                  33.15D0,  42.55D0,  30.63D0,  30.04D0,  29.87D0, &
                  38.99D0/
!---- in slop98.dat   O2.g: 205.137336    fuer O: 102.568668
!----                 C:      5.740448
!----                 S:     31.7984
!----                 H2.g: 130.683056    fuer H:  65.341528
!----                 N2.g: 191.610464    fuer N:  95.805232
!----                 Cu:    33.149832
      THET0=298.15D0
      J2CAL=4.184D0
      DELTS=0.0D0
      DO I=1,NUN
       IF (DABS(XX(IP,I)).GT.1D-20) THEN
       IOFE=0
!!       WRITE (UNIT=6,FMT='('' fromcal-'',A,''-'',F20.10)') &
!!              ABK(I),XX(IP,I)
        DO IE=1,31
         IF (ABK(I).EQ.ENAM(IE)) THEN
          IOFE=IE
          DELTS=DELTS+XX(IP,I)*THES(IE)
         END IF
        END DO
       IF (IOFE.EQ.0) GOTO 99
       END IF
      END DO
!
   99 CONTINUE
!---- if element is not found in ENAM, set DELTG=0
      IF (IOFE.EQ.0) THEN
       DELTG=0.0D0
      ELSE
       DELTG=(THET0*DELTS+GORIG)/J2CAL
      END IF
!!      WRITE (6,2000) NAME(IP),DELTS,DELTG
!! 2000 FORMAT (' fromcal: ',A,f20.10,f20.10)

!
      RETURN
      END
!-----
!******************************
      SUBROUTINE AQUA
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
!----
!---- input is: AQ1   K1,K2,K8, K9, K7
!----                       ZJ, RXJ=rx+Z*0.94 for Z>0
!----           AQ2   D1,D2,D3,D4
!----
      REAL(8) GF,DGDP,DGDT,DGTT, &
      ZJ,RXJ,REJ,REH,WJ,W0,EE,E0,DEDP,DEDT,DETT, &
      PPS,P0PS,TTH,T0TH,TTH1,T0TH1,TAU,TAU0,EE1,E01, &
      YY,QU,XH,YY0,QQ0,XX0,X1,X2,PSI,THET, &
      AGA,BGA, &
      RH2O,AL,BE,DALDT,DBEDT,DBEDP,XN
!-----
      DATA PSI/2600.0D0/
      DATA THET/228.0D0/
      DATA XN/1.66027D5/
!*************************
!     CALL RHOETC(P0,T0,RH2O,AL,BE,DALDT,DBEDT,DBEDP)
!     CALL GSHOK2(T0,P0,RH2O,BE,AL,DALDT,GF,DGDP,DGDT,DGTT)
!     CALL JN91(T0,RH2O,BE,AL,DALDT,EE,DEDP,DEDT,DETT)
!-----
!     WRITE (*,6000) T0,P0,RH2O,AL,BE,DALDT,DBEDT,DBEDP,
!    >GF,DGDP,DGDT,DGTT,EE,DEDP,DEDT,DETT
!6000 FORMAT (' TK =',F8.4,'      P =',F10.4/
!    >'     Dw =',1PE20.10,'     al =',1PE20.10,'     be =',1PE20.10/
!    >'  daldT =',1PE20.10,'  dbedT =',1PE20.10,'  dbedP =',1PE20.10/
!    >'      g =',1PE20.10,'   dgdP =',1PE20.10,'   dgdT =',1PE20.10/
!    >58X'  dgdTT =',1PE20.10/
!    >'      e =',1PE20.10,'   dedP =',1PE20.10,'   dedT =',1PE20.10/
!    >58X'  dedTT =',1PE20.10)
!-----
!     E0=EE
!     YY0=DEDT/(EE**2)
!     QQ0=DEDP/(EE**2)
!     XX0=(DETT-2.0D0/EE*DEDT**2)/(EE**2)
!-----
!     WRITE (*,6010) E0,YY0,XX0,QQ0
!6010 FORMAT (/' E0 =',1PE25.15/' YY0=',1PE25.15/
!    >' XX0=',1PE25.15/' QQ0=',1PE25.15)
!*****
      E0 =    7.824513393256387D+01
      YY0=   -5.798640894464519D-05
      XX0=   -3.074694672039906D-07
      QQ0=    6.634056997277747D-07
!-----
      E01=1.0D0/E0-1.0D0
!-----
      CALL RHOETC(P,T,RH2O,AL,BE,DALDT,DBEDT,DBEDP)
      CALL GSHOK2(T,P,RH2O,BE,AL,DALDT,GF,DGDP,DGDT,DGTT)
      CALL JN91(T,RH2O,BE,AL,DALDT,EE,DEDP,DEDT,DETT)
!-----
      AGA=1.824829238D6*DSQRT(RH2O)/DSQRT((EE*T)**3)
      BGA=50.29158649D8*DSQRT(RH2O)/DSQRT(EE*T)
!*****
!     WRITE (*,6020) TC,P,RH2O,AL,BE,DALDT,DBEDT,DBEDP,
!    >GF,DGDP,DGDT,DGTT,EE,DEDP,DEDT,DETT,AGA,BGA
!6020 FORMAT (/' TC =',F8.4,'      P =',F10.4/
!    >'     Dw =',1PE20.10,'     al =',1PE20.10,'     be =',1PE20.10/
!    >'  daldT =',1PE20.10,'  dbedT =',1PE20.10,'  dbedP =',1PE20.10/
!    >'      g =',1PE20.10,'   dgdP =',1PE20.10,'   dgdT =',1PE20.10/
!    >58X'  dgdTT =',1PE20.10/
!    >'      e =',1PE20.10,'   dedP =',1PE20.10,'   dedT =',1PE20.10/
!    >58X'  dedTT =',1PE20.10/
!    >'   A dh =',1PE20.10,'   B dh =',1PE20.10)
!*****
      EE1=1.0D0/EE-1.0D0
!-----
      PPS=P+PSI
      P0PS=P0+PSI
      TTH=T-THET
      T0TH=T0-THET
      TTH1=1.0D0/TTH
      T0TH1=1.0D0/T0TH
      TAU=T/TTH
      TAU0=T0/T0TH
!-----
      YY=DEDT/(EE**2)
      QU=DEDP/(EE**2)
      XH=(DETT-2.0D0/EE*DEDT**2)/(EE**2)
!*****
!     WRITE (*,6050) EE,YY,XH,QU
!6050 FORMAT (/' EE =',1PE25.15/' YY =',1PE25.15/
!    >' XH =',1PE25.15/' QU =',1PE25.15)
!*****
      ZJ=K8
      RXJ=K9
!-----
!dC   input is RXJ=rx+Z*0.94 for Z>0
!-----
      IF (DABS(ZJ).LT.1D-20) THEN
      WJ=K9
      W0=K9
      X1=0.0D0
      X2=0.0D0
      ELSE
      REJ=RXJ+DABS(ZJ)*GF
      REH=3.082D0+GF
      WJ=XN*ZJ*((ZJ/REJ)-1.0D0/REH)
      W0=XN*ZJ*((ZJ/RXJ)-1.0D0/(3.082D0))
      X1=-XN*(DABS(ZJ**3)*(1.0D0/REJ**2)-ZJ/REH**2)
      X2=XN*2.0D0*ZJ*((ZJ/REJ)**3-1.0D0/(REH**3))
      END IF
!*****
!     DWDP=DGDP*X1
!     DWDT=DGDT*X1
!     DWDTT=(DGDT**2)*X2+DGTT*X1
!     WRITE (*,6100) RXJ,REJ
!6100 FORMAT (/' RXJ,REJ................',2F15.6)
!     WRITE (*,6110) WJ,W0,X1,X2,DWDP,DWDT,DWDTT
!6110 FORMAT (' WJ,W0,X1,X2............',4F15.6/
!    >' dwdp,dwdt,dwdtt........',3F15.6)
!*****
!     F1=K1+K2/(T0TH**2)
!     F2=W0*T0*XX0
!     FCP0=F1+F2
      FCPCP=K1+K2/(TTH**2)
      FCPSOL=WJ*T*XH+2.0D0*T*YY*DGDT*X1-T*EE1*(DGDT**2*X2+DGTT*X1)
      CPR=FCPCP+FCPSOL
!dC   FCPCP=FCPCP-F1
!dC   FCPSOL=FCPSOL-F2
!     WRITE (*,4005) CPR,FCPCP,FCPSOL
!4005 FORMAT (/' CP,fcpcp,fcpsol....... ',3F15.6)
!*****
!     F1=D1+D2/P0PS+D3/T0TH+D4/P0PS/T0TH
!     F2=-W0*QQ0
!     VR0=F1+F2
      FVVOL=D1+D2/PPS+D3/TTH+D4/PPS/TTH
      FVSOL=-WJ*QU+EE1*DGDP*X1
      VOLUM=FVVOL+FVSOL
!dC   FVVOL=FVVOL-F1
!dC   FVSOL=FVSOL-F2
      VOLUM=VOLUM*41.83930D0
      FVVOL=FVVOL*41.83930D0
      FVSOL=FVSOL*41.83930D0
!     WRITE (*,4010) VOLUM,FVVOL,FVSOL
!4010 FORMAT (' VOLUM,fvvol,fvsol........ ',3F15.6)
!*****
      FHCP=K1*(T-T0)-K2*(TTH1-T0TH1)
      FHSOL=WJ*T*YY-W0*T0*YY0+WJ*EE1-W0*E01-T*EE1*DGDT*X1
      HR=H0R+FHCP+FHSOL
!     WRITE (*,4015) HR,H0R,FHCP,FHSOL
!4015 FORMAT (' HR,H0r,fhcp,fhsol..... ',4F15.6)
!*****
      FSCP=K1*DLOG(T/T0)-K2/(THET**2)*(TAU-TAU0+DLOG(TAU0/TAU))
      FSSOL=WJ*YY-EE1*DGDT*X1-W0*YY0
      SR=S0R+FSCP+FSSOL
!     WRITE (*,4020) SR,S0R,FSCP,FSSOL
!4020 FORMAT (' SR,s0r,fscp,fssol..... ',4F15.6)
!*****
      FG0=H0R-T*S0R
!!cdc      FG0=G0R-(T-T0)*S0R
      FGCP=FHCP-T*FSCP
      FGVOL=D1*(P-P0)+D2*DLOG(PPS/P0PS) &
      +1.0D0/TTH*(D3*(P-P0)+D4*DLOG(PPS/P0PS))
      FGSOL=WJ*EE1-W0*E01+W0*YY0*(T-T0)
      GR=FG0+FGCP+FGVOL+FGSOL
      GR=GR*4.184D0
!dC
!     WRITE (*,4025) GR,FG0,FGCP,FGVOL,FGSOL
!4025 FORMAT (' GR,fg0,fgcp,fgvol,fgsol',5F15.6)
!*****
      RETURN
      END
!-----
!******************************
      SUBROUTINE RHOETC(PQ,TQ,RH2O,AL,BE,DALDT,DBEDT,DBEDP)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      REAL(8) PQ,TQ,RH2O,AL,BE,DALDT,DBEDT,DBEDP
      DIMENSION TAUI(0:6),ERMI(0:9),GI(40),KI(40),LI(40),CI(18), &
      RHOI(37:40),TTTI(37:40),ALPI(37:40),BETI(37:40),TIT(7),PIT(7), &
      RHIT(7),DPRIT(7)
!-----GI ARE IN (bar cc / g)  =  10 * (J / g)
      DATA GI/-.53062968529023D4,.22744901424408D5,.78779333020687D4 &
      ,-.69830527374994D3,.17863832875422D6,-.39514731563338D6 &
      ,.33803884280753D6,-.13855050202703D6,-.25637436613260D7 &
      ,.48212575981415D7,-.34183016969660D7, .12223156417448D7 &
      ,.11797433655832D8,-.21734810110373D8, .10829952168620D8 &
      ,-.25441998064049D7,-.31377774947767D8,.52911910757704D8 &
      ,-.13802577177877D8,-.25109914369001D7, .46561826115608D8 &
      ,-.72752773275387D8,.41774246148294D7,.14016358244614D8 &
      ,-.31555231392127D8,.47929666384584D8,.40912664781209D7 &
      ,-.13626369388386D8, .69625220862664D7,-.10834900096447D8 &
      ,-.22722827401688D7,.38365486000660D7,.68833257944332D5 &
      ,.21757245522644D6,-.26627944829770D5,-.70730418082074D6 &
      ,-.225D1,-1.68D1,.055D1,-93.0D1/
      DATA KI/4*1,4*2,4*3,4*4,4*5,4*6,4*7,4*9,2*3,1,5,3*2,4/
      DATA LI/1,2,4,6,1,2,4,6,1,2,4,6,1,2,4,6,1,2,4,6,1,2,4,6,1,2,4,6 &
      ,1,2,4,6,0,3*3,0,2,0,0/
      DATA CI/.19730271018D2,.209662681977D2,-.483429455355D0 &
      ,.605743189245D1,22.56023885D0,-9.87532442D0,-.43135538513D1 &
      ,.458155781D0,-.47754901883D-1,.41238460633D-2,-.27929052852D-3 &
      ,.14481695261D-4,-.56473658748D-6,.16200446D-7,-.3303822796D-9 &
      ,.451916067368D-11,-.370734122708D-13,.137546068238D-15/
      DATA RHOI/0.319D0,0.310D0,0.310D0,1.550D0/
      DATA TTTI/640.0D0,640.0D0,641.6D0,270.0D0/
      DATA ALPI/34.0D0,40.0D0,30.0D0,1050.0D0/
      DATA BETI/2.0D4,2.0D4,4.0D4,25.0D0/
      R=4.6152D0
      T=TQ
      P=PQ
      RT=R*T
!
      PS=220.55D0
      IF (T.LE.647.25D0) THEN
      CALL PSAT4(T,PS)
      END IF
!
!-----SET INITIAL GUESS FOR RHO USING THB-FIT TO REDLICH-KWONG
      ARK=1.279186D8-2.241415D4*T
      BRK=1.428062D1+6.092237D-4*T
      RR=8.31441D0
      OFT=ARK/(P*DSQRT(T))
      BUK=-10D0*RR*T/P
      CUK=OFT-BRK*BRK+BRK*BUK
      DUK=-BRK*OFT
      CALL CUBIC2(BUK,CUK,DUK,X1,X2,X2I,X3)
      IF (DABS(X2I).GT.1D-20) THEN
      VOL=X1
      ELSE
      IF (P.LT.PS)  THEN
      VOL=DMAX1(X1,X2,X3)
      ELSE
      VOL=DMIN1(X1,X2,X3)
      END IF
      END IF
      IF (VOL.LE.0.0D0) THEN
      RHN=1.9D0
      ELSE
      RHN=1D0/VOL*18.0152D0
      END IF
!
!
      DELT=0.001D0
      DELP=0.01D0
      TIT(1)=TQ
      PIT(1)=PQ
      TIT(2)=TQ-DELT
      PIT(2)=PQ
      TIT(3)=TQ+DELT
      PIT(3)=PQ
      TIT(4)=TQ-2.0D0*DELT
      PIT(4)=PQ
      TIT(5)=TQ+2.0D0*DELT
      PIT(5)=PQ
      TIT(6)=TQ
      PIT(6)=PQ-DELP
      TIT(7)=TQ
      PIT(7)=PQ+DELP
      T0=647.073D0
!=====
      DO 100,ITER=1,7
      IF (ITER.NE.1) RHN=RHIT(1)
      T=TIT(ITER)
      P=PIT(ITER)
      RT=R*T
!-----The values (T/T0)**i are stored in the array TAUI(i)
      TAUI(0)=1.D0
      TAUI(1)=T/T0
      DO I=2,6
        TAUI(I)=TAUI(I-1)*TAUI(1)
      END DO
!
      B=-0.3540782D0*DLOG(TAUI(1))+0.7478629D0 &
      +0.007159876D0/TAUI(3)-0.003528426D0/TAUI(5)
      BB=1.1278334D0-0.5944001D0/TAUI(1) &
      -5.010996D0/TAUI(2)+0.63684256D0/TAUI(4)
!
!-----FIND THE TRUE(?) RH(T,P)
!-----NOTE: PR = PRESSURE CORRESPONDING TO GUESSED RH
!           DPR = (dP / dRH)
!           the values (1-EXP(-RH))**i are stored in the array ERMI(i)
      DO 20,LOO=1,100
      RH=RHN
      IF (RH.LE.1D-8) RH=1D-8
      IF (RH.GT.1.9D0) RH=1.9D0
      RH2=RH*RH
      Y=RH*B/4.D0
      ER=DEXP(-RH)
      Y3=(1D0-Y)**3
      ALY=11.D0*Y
      BETY=44.33333333333333D0*Y*Y
      F1=(1.D0+ALY+BETY)/Y3
      F2=4.D0*Y*(BB/B-3.5D0)
      ERMI(0)=1D0
      ERMI(1)=1D0-ER
      DO I=2,9
        ERMI(I)=ERMI(I-1)*ERMI(1)
      END DO
      PR=0.0D0
      DPR=0.0D0
      DO I=1,36
        S=GI(I)/TAUI(LI(I))*ERMI(KI(I)-1)
        PR=PR+S
        DPR=DPR+(2D0+RH*(KI(I)*ER-1D0)/ERMI(1))*S
      END DO
      DO I=37,40
        DEL=RH/RHOI(I)-1.0D0
        RHOI2=RHOI(I)*RHOI(I)
        TAU=T/TTTI(I)-1.0D0
        QHEX=(-ALPI(I)*DEL**LI(I)-BETI(I)*TAU*TAU)
        IF (QHEX.GT.-150.0D0) THEN
        Q10=GI(I)*DEL**LI(I)*DEXP(-ALPI(I)*DEL**LI(I)-BETI(I)*TAU*TAU)
        ELSE
        Q10=0.0D0
        END IF
        QM=LI(I)/DEL-KI(I)*ALPI(I)*DEL**(KI(I)-1)
        S=Q10*QM*RH2/RHOI(I)
        PR=PR+S
        DPR=DPR+S*(2.0D0/RH+QM/RHOI(I))-RH2/RHOI2*Q10* &
        (LI(I)/DEL/DEL+KI(I)*(KI(I)-1)*ALPI(I)*DEL**(KI(I)-2))
      END DO
      PR=RH*(RH*ER*PR+RT*(F1+F2))
      DPR=RH*ER*DPR+RT*((1D0+2D0*ALY+3D0*BETY)/Y3 &
      +3D0*Y*F1/(1D0-Y)+2D0*F2)
!---
      IF (DPR.LE.0.0D0) THEN
      IF (P.LE.PS) THEN
      RHN=RHN*0.95D0
      ELSE
      RHN=RHN*1.05D0
      END IF
      ELSE
      IF (DPR.LT.0.01D0) DPR=0.01D0
      S=(P-PR)/DPR
      IF (DABS(S).GT.0.1D0) S=0.1D0*S/DABS(S)
      RHN=RH+S
      END IF
      DP=DABS(1.0D0-PR/P)
      DR=DABS(1.0D0-RHN/RH)
      IF (DP.LT.1D-5.AND.DR.LT.1D-12) GOTO 30
   20 CONTINUE
!---
   30 RHIT(ITER)=RHN
      DPRIT(ITER)=DPR
  100 CONTINUE
!=====
      RH2O=RHIT(1)
      BE=1.0D0/DPRIT(1)/RHIT(1)
      F1=1.0D0/DPRIT(6)/RHIT(6)
      F2=1.0D0/DPRIT(7)/RHIT(7)
      DBEDP=(F2-F1)/DELP/2.0D0
      F1=1.0D0/DPRIT(2)/RHIT(2)
      F2=1.0D0/DPRIT(3)/RHIT(3)
      DBEDT=(F2-F1)/DELT/2.0D0
      AL=-(RHIT(3)-RHIT(2))/DELT/2.0D0/RHIT(1)
      F1=-(RHIT(1)-RHIT(4))/DELT/2.0D0/RHIT(2)
      F2=-(RHIT(5)-RHIT(1))/DELT/2.0D0/RHIT(3)
      DALDT=(F2-F1)/DELT/2.0D0
!=====
      RETURN
      END
!-----
!******************************
      SUBROUTINE PSAT4(T,PS)
      DOUBLE PRECISION T,PS,A(8),W,WSQ,V,FF
      DATA A/-7.8889166D0,2.5514255D0,-6.716169D0 &
      ,33.239495D0,-105.38479D0,174.35319D0,-148.39348D0 &
      ,48.631602D0/
      IF (T.LE.314.00D0) THEN
      PS=DEXP(6.3573118D0-8858.843D0/T+607.56335D0/(T**0.6D0))
      ELSE
      V=T/647.25D0
      W=DABS(1.0D0-V)
      WSQ=DSQRT(W)
      FF=0.0D0
      DO I=1,8
        FF=FF+A(I)*W
        W=W*WSQ
      END DO
      PS=220.93D0*DEXP(FF/V)
      END IF
      RETURN
      END
!-----
!******************************
      SUBROUTINE CUBIC2(B,C,D,X1,X2,X2I,X3)
      REAL(8) B,C,D,Q,P,R,PI,PHI3,FF,X1,X2,X2I,X3
      PI=3.14159263538979D0
      X2=0.0D0
      X2I=0.0D0
      X3=0.0D0
      IF (DABS(C).LT.1D-20.AND.DABS(D).LT.1D-20) THEN
      X1=-B
      RETURN
      END IF
      Q=((2.D0*B*B*B)/(27.D0)-(B*C)/(3.D0)+D)/2.D0
      P=(3.D0*C-B*B)/(9.D0)
      FF=DABS(P)
      R=DSQRT(FF)
      FF=R*Q
      IF (FF.LT.0.0D0) R=-R
      FF=Q/(R*R*R)
      IF (P.GT.0.0D0) THEN
      PHI3=DLOG(FF+DSQRT(FF*FF+1.D0))/3.D0
      X1=-R*(DEXP(PHI3)-DEXP(-PHI3))-B/(3.D0)
      X2I=1D0
      ELSE
      IF (Q*Q+P*P*P.GT.0.0D0) THEN
      PHI3=DLOG(FF+DSQRT(FF*FF-1.D0))/3.D0
      X1=-R*(DEXP(PHI3)+DEXP(-PHI3))-B/(3.D0)
      X2I=1D0
      ELSE
      PHI3=DATAN(DSQRT(1.D0-FF*FF)/FF)/3.D0
      X1=-2.D0*R*DCOS(PHI3)-B/(3.D0)
      X2=2.D0*R*DCOS(PI/3.D0-PHI3)-B/(3.D0)
      X2I=0.0D0
      X3=2.D0*R*DCOS(PI/3.D0+PHI3)-B/(3.D0)
      END IF
      END IF
      RETURN
      END
!-----
!******************************
      SUBROUTINE JN91(T,D,BETA,ALPHA,DALDT,EPS,DEDP,DEDT,D2EDT2)
!** JN91 - Compute (eps, dedP, dedT, d2edT2)(T,D) using equations
!**        given by Johnson and Norton (1991); fit parameters
!**        regressed from least squares fit to dielectric data
!**        consistent with the HK74 equation and low temperatures,
!**        and with the Pitz83 equation at high temperatures.
!**
!**          Units: T ............... K
!**                 D ............... g/cm**3
!**                 beta, dedP ...... bar**(-1)
!**                 alpha, dedT ..... K**(-1)
!**                 daldT, d2edT2 ... K**(-2)
!dC   IMPLICIT DOUBLE PRECISION (a-h,o-z)
!
!     from file h2o92D.f
!
      IMPLICIT REAL(8) (A-H,O-Z)
      DOUBLE PRECISION  A(10), C(5), DCDT(5), DC2DTT(5)
!     SAVE
      DATA TREF / 298.15D0 /
      DATA A / &
                0.1470333593D+02, &
                0.2128462733D+03, &
               -0.1154445173D+03, &
                0.1955210915D+02, &
               -0.8330347980D+02, &
                0.3213240048D+02, &
               -0.6694098645D+01, &
               -0.3786202045D+02, &
                0.6887359646D+02, &
               -0.2729401652D+02 /
      TN = T / TREF
      C(1)      = 1.0D0
      DCDT(1)   = 0.0D0
      DC2DTT(1) = 0.0D0
!-----
      C(2)      = A(1)/TN
      DCDT(2)   = -A(1)*TREF/T**2
      DC2DTT(2) = 2.0D0*A(1)*TREF/T**3
!-----
      C(3)      = A(2)/TN + A(3) + A(4)*TN
      DCDT(3)   = -A(2)*TREF/T**2 + A(4)/TREF
      DC2DTT(3) = 2.0D0*A(2)*TREF/T**3
!-----
      C(4)      = A(5)/TN + A(6)*TN + A(7)*TN**2
      DCDT(4)   = -A(5)*TREF/T**2 + A(6)/TREF &
                  + 2.0D0*A(7)*T/TREF**2
      DC2DTT(4) = 2.0D0*A(5)*TREF/T**3 + 2.0D0*A(7)/TREF**2
!-----
      C(5)      = A(8)/TN**2 + A(9)/TN + A(10)
      DCDT(5)   = -2.0D0*A(8)*TREF**2/T**3 - A(9)*TREF/T**2
      DC2DTT(5) = 6.0D0*A(8)*TREF**2/T**4 + 2.0D0*A(9)*TREF/T**3
!-----
      EPS = 0.0D0
      DO K=1,5
           EPS = EPS + C(K)*D**(K-1)
      END DO
      DEDP = 0.0D0
      DO  J = 0,4
           DEDP = DEDP + J*C(J+1)*D**J
      END DO
      DEDP = BETA * DEDP
      DEDT = 0.0D0
      DO  J = 0,4
           DEDT = DEDT + D**J*(DCDT(J+1) - J*ALPHA*C(J+1))
      END DO
      D2EDT2 = 0.0D0
      DO  J = 0,4
           D2EDT2 = D2EDT2 + D**J*(DC2DTT(J+1) - J*(ALPHA*DCDT(J+1) + &
               C(J+1)*DALDT) - J*ALPHA*(DCDT(J+1) - J*ALPHA*C(J+1)))
      END DO
      END
!-----
!******************************
      SUBROUTINE GSHOK2(TXX,P,D,BETA,ALPHA,DALDT,G,DGDP,DGDT,D2GDT2)
!** gShok2- Computes g, dgdP, dgdT, and d2gdT2 using equations given
!**         by Shock et al. (1991)
!**
!** units:   T ................. C
!**          D ................. g/cm**3
!**          beta, dgdP ........ bars**(-1)
!**          alpha, dgdT ....... K**(-1)
!**          daldT, d2gdT2 ..... K**(-2)
!     IMPLICIT DOUBLE PRECISION (a-h,o-z)
!
!     from file reac92D.f
!
      IMPLICIT REAL(8) (A-H,O-Z)
      REAL(8) C(6), CC(3)
!     SAVE
      DATA C /  -0.2037662D+01,  0.5747000D-02, -0.6557892D-05, &
                 0.6107361D+01, -0.1074377D-01,  0.1268348D-04 /
      DATA CC /  0.3666666D+02, -0.1504956D-9,   0.5017997D-13 /
!DC------
      T=TXX-273.15D0
!DC------
      IF (D .GE. 1.0D0) RETURN
      A = C(1) + C(2)*T + C(3)*T**2
      B = C(4) + C(5)*T + C(6)*T**2
      G = A*(1.0D0 - D)**B
      DGDD   = - A * B * (1.0D0 - D)**(B - 1.0D0)
      DGDD2  =   A * B * (B - 1.0D0) * (1.0D0 - D)**(B - 2.0D0)
      DADT   =   C(2) + 2.0D0*C(3)*T
      DADTT  =   2.0D0 * C(3)
      DBDT   =   C(5) + 2.0D0*C(6)*T
      DBDTT  =   2.0D0 * C(6)
      DDDT   = - D * ALPHA
      DDDP   =   D * BETA
      DDDTT  = - D * (DALDT - ALPHA**2)
      DB     = (1.0D0 - D) ** B
      DDBDT  = -B * (1.0D0 - D)**(B-1.0D0) * DDDT + &
               DLOG(1.0D0 - D) * DB  * DBDT
      DDBDTT = -(B * (1.0D0 - D)**(B-1.0D0) * DDDTT + &
                 (1.0D0 - D)**(B-1.0D0) * DDDT * DBDT + B * DDDT * &
                 (-(B-1.0D0) * (1.0D0 - D)**(B-2.0D0) * DDDT + &
                 DLOG(1.0D0 - D) * (1.0D0 - D)**(B-1.0D0) * DBDT)) + &
                 DLOG(1.0D0 - D) * (1.0D0 - D)**B * DBDTT - &
                 (1.0D0 - D)**B * DBDT * DDDT / (1.0D0 - D) + &
                 DLOG(1.0D0 - D) * DBDT * DDBDT
      DGDP   = DGDD * DDDP
      DGDT   = A*DDBDT + DB*DADT
      D2GDT2 = A*DDBDTT + 2.0D0*DDBDT*DADT + DB*DADTT
      IF ((T .LT. 155.0D0) .OR. (P .GT. 1000.0D0) .OR. &
          (T .GT. 355.0D0)) RETURN
      FT     = ((T - 155.0D0)/300.0D0)**4.8D0 + &
               CC(1)*((T - 155.0D0)/300.0D0)**16
      DFTDT  = 4.8D0/300.0D0*((T - 155.0D0)/300.0D0)**3.8D0 + &
              16.0D0/300.0D0*CC(1)*((T - 155.0D0)/300.0D0)**15
      DFTDTT = 3.8D0*4.8D0/300.0D0**2*((T - 155.0D0)/300.0D0)**2.8D0 + &
              15.0D0*16.0D0/300.0D0**2*CC(1)*((T - 155.0D0)/300.0D0)**14
      FP     = CC(2)*(1000.0D0 - P)**3 + CC(3)*(1000.0D0 - P)**4
      DFPDP  = -3.0D0*CC(2)*(1000.0D0 - P)**2 - &
                4.0D0*CC(3)*(1000.0D0 - P)**3
      F      = FT * FP
      DFDP   = FT * DFPDP
      DFDT   = FP * DFTDT
      D2FDT2 = FP * DFTDTT
      G      = G      - F
      DGDP   = DGDP   - DFDP
      DGDT   = DGDT   - DFDT
      D2GDT2 = D2GDT2 - D2FDT2
      RETURN
      END
!-----
!******************************
      SUBROUTINE NURVONPT
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
!-----END OF COMMON VARIABLES
      INTEGER(4) I001,I,II,IS
!-----
      T=TC+273.15D0
!-----
      IF (T.LE.0.0) THEN
      CALL SHOUTF
      write (*,1000) T
 1000 FORMAT (//' T =',F8.2,'  The Temperature is less or equal 0 K')
      STOP
      END IF
      IF (P.LT.0.0) THEN
      CALL SHOUTF
      write (*,1010) P
 1010 FORMAT (//' P =',F9.2,'  The Pressure is less than zero')
      STOP
      END IF
!-----
      PGAS=P*PRAT
      RT=R*T
      TT=T*T
      SQT=DSQRT(T)
      GR=0.0D0
      VOLUM=0.0D0
      DO II=1,NUN
        GG(II)=0.0D0
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
!      WRITE (UNIT=6,FMT='(''jetzt gcalc'',2(1PE15.8))') PGAS,T
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
      GG(II)=GR
      VV(I001)=VOLUM
  600 CONTINUE
      RETURN
      END
