!-----Version: 28.05.2022
!     *************************
!     * THERIAK - SUBROUTINES *
!     *************************
!
!     these subroutines (up to the comment "CdC end") were written by
!     Christian de Capitani
!     at the Department of Geological Sciences,
!     University of British Columbia, Vancouver B.C., Canada
!
!     constants that may cause an underflow are
!     the -150D0's in the subroutine WHAAR2
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
!-----
!achtung EMAX muss auch in theriak.cmn angepasst werden
!********************************
      SUBROUTINE SOLINI(SOLNAM,K,EMSTR,N,EMBCOD)
      INTEGER(4) EMAX
      PARAMETER (EMAX=15)
      CHARACTER(16) SOLNAM,NAME(EMAX),EMSTR(EMAX)
      INTEGER(4) N,EMBCOD(EMAX),I,II,K,CODE
      DO I=1,EMAX
        NAME(I)=' '
      END DO
      N=0
!-----**FELDSPARS (Fuhrman and Lindsley(1988))**
      IF (SOLNAM.EQ.'FSP'.OR.SOLNAM.EQ.'FSPc') THEN
      N=3
      NAME(1)='ALBITE'
      NAME(2)='K-FELDSPAR'
      NAME(3)='ANORTHITE'
      END IF
!-----**FELDSPARS (Ghiorso (1984))**
      IF (SOLNAM.EQ.'G-FSP') THEN
      N=3
      NAME(1)='albite'
      NAME(2)='anorthite'
      NAME(3)='sanidine'
      END IF
!-----** FELDSPARS (Holland and Powell (2003)) -- David Dolejs, 13-March-05 **
      IF (SOLNAM.EQ.'FSP-HP1') THEN
      N=3
      NAME(1)='microcline'
      NAME(2)='albite'
      NAME(3)='anorthite-C1'
      END IF
      IF (SOLNAM.EQ.'FSP-HP2') THEN
      N=3
      NAME(1)='sanidine'
      NAME(2)='albite'
      NAME(3)='anorthite-C1'
      END IF
      IF (SOLNAM.EQ.'FSP-HP3') THEN
      N=3
      NAME(1)='sanidine'
      NAME(2)='high-albite'
      NAME(3)='anorthite-C1'
      END IF
!-----** CORDIERITE, IDEAL RECIPROCAL -- David Dolejs, 13-March-05 **
      IF (SOLNAM.EQ.'CRD-HP') THEN
      N=4
      NAME(1)='cordierite'
      NAME(2)='Fe-cordierite'
      NAME(3)='hydr.cordierite'
      NAME(4)='h.Fe-cordierite'
      END IF
!-----** BIOTITE, Holland & Powell -- David Dolejs, 13-March-05 **
      IF (SOLNAM.EQ.'BIO-HP') THEN
      N=5
      NAME(1)='phlogopite'
      NAME(2)='annite'
      NAME(3)='eastonite'
      NAME(4)='obiotite'
      NAME(5)='siderophyllite'
      END IF
!-----** ORTHOPYROXENE, Holland & Powell -- David Dolejs, 13-March-05 **
      IF (SOLNAM.EQ.'OPX-HP') THEN
      N=5
      NAME(1)='enstatite'
      NAME(2)='ferrosilite'
      NAME(3)='fm.pyx'
      NAME(4)='Mg-Tscher.pyx'
      NAME(5)='Fe-Tscher.pyx'
      END IF
!-----** ORTHOPYROXENE, Holland & Powell 2011
      IF (SOLNAM.EQ.'SOPX') THEN
      N=4
      NAME(1)='Enstatite'
      NAME(2)='Ferrosilite'
      NAME(3)='Mg-tscher_pyrox'
      NAME(4)='FM.pyx'
      END IF
!-----** ORTHOPYROXENE, White et al. 2014
!-----** taken from tc-6NCKFMASHTO.txt in thermocalc_340i
!-----** added Mn according to tc-6axNCKFMASHTOm45.txt
      IF (SOLNAM.EQ.'OPX6') THEN
      N=7
      NAME(1)='Enstatite'
      NAME(2)='Ferrosilite'
      NAME(3)='FM_pyx'
      NAME(4)='Mg-tscher_pyrox'
      NAME(5)='fopx'
      NAME(6)='Mn-Opx'
      NAME(7)='Orthodiopside'
      END IF
      IF (SOLNAM.EQ.'OPXW14') THEN
      N=7
      NAME(1)='en'
      NAME(2)='fs'
      NAME(3)='fm'
      NAME(4)='mgts'
      NAME(5)='fopx'
      NAME(6)='mnopx'
      NAME(7)='odi'
      END IF
!-----**NEPHELINE**
      IF (SOLNAM.EQ.'NEPH-KS') THEN
      N=2
      NAME(1)='NEPHELINE'
      NAME(2)='KALSILITE'
      END IF
!-----**H2O-CO2: KERRICK AND JACOBS (1981)**
      IF (SOLNAM.EQ.'H2O-CO2'.OR.SOLNAM.EQ.'WCO2') THEN
      N=2
      NAME(1)='STEAM'
      NAME(2)='CARBON-DIOXIDE'
      END IF
!-----**H2O-CO2: Duan (1992)**
      IF (SOLNAM.EQ.'HC-DU92') THEN
      N=2
      NAME(1)='STEAM'
      NAME(2)='CARBON-DIOXIDE'
      END IF
!-----**H2O-CO2: Duan (2006)**
      IF (SOLNAM.EQ.'HC-DU06') THEN
      N=2
      NAME(1)='STEAM'
      NAME(2)='CARBON-DIOXIDE'
      END IF
!-----**G-OLIV (Ghiorso (1984))**
      IF (SOLNAM.EQ.'G-OLIV') THEN
      N=2
      NAME(1)='FAYALITE'
      NAME(2)='FORSTERITE'
      END IF
!-----**Quasi-Chemical models (Pelton (19..))**
      IF (SOLNAM.EQ.'MGSI.L') THEN
      N=2
      NAME(2)='TIO2.L'
      NAME(1)='MGO.L'
      NAME(2)='SIO2.L'
      END IF
      IF (SOLNAM.EQ.'MNTI.L') THEN
      N=2
      NAME(1)='MNO.L'
      NAME(2)='TIO2.L'
      END IF
      IF (SOLNAM.EQ.'MGTI.L') THEN
      N=2
      NAME(1)='MGO.L'
      END IF
      IF (SOLNAM.EQ.'FETI.L') THEN
      N=2
      NAME(1)='FEO.L2'
      NAME(2)='TIO2.L'
      END IF
!-----**Vidals thing**
      IF (SOLNAM.EQ.'MICA4o') THEN
      N=3
      NAME(1)='CELADONITE'
      NAME(2)='MUSCOVITE'
      NAME(3)='PYROPHYLLITE'
      END IF
!-----**Vidals thing**
      IF (SOLNAM.EQ.'MICA4') THEN
      N=5
      NAME(1)='MG-CEL'
      NAME(4)='FE-CEL'
      NAME(2)='MUSCOVITE'
      NAME(3)='PRL'
      NAME(5)='PG'
      END IF
!------**CHLORITE Vidal AJS2001**
      IF (SOLNAM.EQ.'CHLVIDAL') THEN
      N=4
      NAME(1)='CLINOCHLORE'
      NAME(2)='DAPHNITE'
      NAME(3)='Am-vid'
      NAME(4)='SUDOITE'
      END IF
!------**CHLORITE Lanari AJS2014**
      IF (SOLNAM.EQ.'CHLlwv') THEN
      N=4
      NAME(1)='clinochlore'
      NAME(2)='daphnite'
      NAME(3)='amesite'
      NAME(4)='sudLANARI'
      END IF
!------**CHLORITE Fe**
      IF (SOLNAM.EQ.'CHLFe') THEN
      N=4
      NAME(1)='CLINOCHLORE'
      NAME(2)='DAPHNITE'
      NAME(3)='Fe-Am'
      NAME(4)='SUDOITE'
      END IF
!-----**landau ordering**
      IF (SOLNAM.EQ.'P2/n') THEN
      N=2
      NAME(1)='AA'
      NAME(2)='BB'
      END IF
!-----**hopo liquid**
      IF (SOLNAM.EQ.'LIQtc') THEN
      N=8
      NAME(1)='Silica8.liq'
      NAME(2)='albite.liq'
      NAME(3)='K-feldspar.liq'
      NAME(4)='anorthite.liq'
      NAME(5)='sillimanite8.liq'
      NAME(6)='forsterite8.liq'
      NAME(7)='fayalite8.liq'
      NAME(8)='H2O.liq'
      END IF
!-----**hopo liquid**
      IF (SOLNAM.EQ.'LIQtc6') THEN
      N=8
      NAME(1)='Quartz8_liq'
      NAME(2)='Albite_liq'
      NAME(3)='K-feldspar_liq'
      NAME(4)='Anorthite_liq'
      NAME(5)='Sillimanite8_liq'
      NAME(6)='Forsterite8_liq'
      NAME(7)='Fayalite8_liq'
      NAME(8)='H2O_liq'
      END IF
!-----**hopo liquid**
      IF (SOLNAM.EQ.'LIQtc6F') THEN
      N=9
      NAME(1)='Quartz8_liq'
      NAME(2)='Albite_liq'
      NAME(3)='K-feldspar_liq'
      NAME(4)='Anorthite_liq'
      NAME(5)='Sillimanite8_liq'
      NAME(6)='Forsterite8_liq'
      NAME(7)='Fayalite8_liq'
      NAME(8)='H2O_liq'
      NAME(9)='KF_liq'
      END IF
!-----**Liquid. Tonalitic metabasite melt: NCKFMASH. Green et al. 2016**
      IF (SOLNAM.EQ.'LIQtc62') THEN
      N=9
      NAME(1)='Quartz2_liq'
      NAME(2)='Albite_liq'
      NAME(3)='K-feldspar_liq'
      NAME(4)='Wollast2_liq'
      NAME(5)='Sillimanite2_liq'
      NAME(6)='Fayalite2_liq'
      NAME(7)='Forsterite2_liq'
      NAME(8)='H2O_liq'
      NAME(9)='Anorthite2_liq'
      END IF
!-----**hopo spinel**
      IF (SOLNAM.EQ.'SPINtc') THEN
      N=4
      NAME(1)='hercynite'
      NAME(2)='spinel'
      NAME(3)='magnetite'
      NAME(4)='ulvospinel'
      END IF
!-----**PSI spinel**
      IF (SOLNAM.EQ.'SpPSI') THEN
      N=4
      NAME(1)='MAGNETITE'
      NAME(2)='CHROMITE'
      NAME(3)='TREVORITE'
      NAME(4)='NICHROMITE'
      END IF
!-----**chlorite, Hollands webpage**
!----this model fails with Gibbs-Duhem
      IF (SOLNAM.EQ.'CHLHOPO') THEN
      N=4
      NAME(1)='Al-free-chlorite'
      NAME(2)='clinochlore'
      NAME(3)='daphnite'
      NAME(4)='amesite'
      END IF
!-----**sapphirine, Kelsey et al 2004**
      IF (SOLNAM.EQ.'SAPPk') THEN
      N=3
      NAME(1)='sapphirine_221'
      NAME(2)='Fe-sapph_221'
      NAME(3)='sapphirine_351'
      END IF
!-----**osumilite, Kelsey et al 2004**
      IF (SOLNAM.EQ.'OSUMk') THEN
      N=3
      NAME(1)='osumilitE_1'
      NAME(2)='Fe-osumilite'
      NAME(3)='osumilitE_2'
      END IF
!
      IF (SOLNAM.EQ.'ORTHOPYROXENE') THEN
      N=3
      NAME(1)='ORTHOENSTATITE'
      NAME(2)='FERROSILITE'
      NAME(3)='ALUMINOOPX'
      END IF
!
      IF (SOLNAM.EQ.'FOQZ') THEN
      N=3
      NAME(1)='forsterite8.liq'
      NAME(2)='enstatite8.liq'
      NAME(3)='Silica8.liq'
      END IF
!
      IF (SOLNAM.EQ.'AmpDHP') THEN
      N=6
      NAME(1)='tremolite'
      NAME(2)='tschermakite2'
      NAME(3)='pargasite2'
      NAME(4)='glaucophane2'
      NAME(5)='ferroactinolite'
      NAME(6)='ferritschermak'
      END IF
!-----clinoamphibole: NCFMASHO Diener et al. 2007
      IF (SOLNAM.EQ.'ClAMP') THEN
      N=9
      NAME(1)='tremolite'
      NAME(2)='tschermakite2'
      NAME(3)='pargasite2'
      NAME(4)='glaucophane2'
      NAME(5)='cummingtonite2'
      NAME(6)='grunerite2'
      NAME(7)='acam'
      NAME(8)='bcam'
      NAME(9)='mgriebekite'
      END IF
!-----clinoamphibole: NCFMASHO Green et al. 2016
      IF (SOLNAM.EQ.'ClAMg') THEN
      N=11
      NAME(1)='tremolite'
      NAME(2)='tschermakite2'
      NAME(3)='pargasite2'
      NAME(4)='glaucophane2'
      NAME(5)='cummingtonite'
      NAME(6)='grunerite2'
      NAME(7)='acam'
      NAME(8)='bcam'
      NAME(9)='mgriebekite'
      NAME(10)='kpargasite'
      NAME(11)='ttschermakite'
      END IF
!-----clinoamphibole: NCFMASHO Green et al. 2016
      IF (SOLNAM.EQ.'CAMPG16') THEN
      N=11
      NAME(1)='tr'
      NAME(2)='ts1'
      NAME(3)='parg1'
      NAME(4)='gl1'
      NAME(5)='cumm1'
      NAME(6)='grun1'
      NAME(7)='a'
      NAME(8)='b'
      NAME(9)='mrb'
      NAME(10)='kprg'
      NAME(11)='tts'
      END IF
!-----orthoamphibole: NCFMASHO Diener et al. 2007/2012
      IF (SOLNAM.EQ.'OAMP') THEN
      N=9
      NAME(1)='anthophyllite'
      NAME(2)='gedrite2'
      NAME(3)='O-Mgpargasite'
      NAME(4)='O-glaucophane'
      NAME(5)='O-tremolite'
      NAME(6)='Fe-anthophyll2'
      NAME(7)='amo1'
      NAME(8)='amo2'
      NAME(9)='O-Mgriebeckite'
      END IF
!-----Nal-phase Holland et al. 2013 (mantle phases)
!     coded by Thorsten Nagel
      IF (SOLNAM.EQ.'NALP') THEN
      N=7
      NAME(1)='NaAlSi-Nal'    
      NAME(2)='CaMgAl-Nal'
      NAME(3)='MgAl-Nal'    
      NAME(4)='MgSi-Nal'       
      NAME(5)='FeSi-Nal'    
      NAME(6)='O1-Nal'           
      NAME(7)='O2-Nal'          
      END IF
!
!-----Cpx Holland et al. 2013 ("Green et al. 2012")
!     coded by Thorsten Nagel      
      IF (SOLNAM.EQ.'CPXHO13') THEN
      N=6
      NAME(1)='Diopside'      
      NAME(2)='fsfud'     
      NAME(3)='Ca-tscher-Pyrox'
      NAME(4)='Jadeite'        
      NAME(5)='cenfud'      
      NAME(6)='fm-cpx'           
      END IF
!
!-----Clinopyroxene: NCFMASHO Green et al. 2016
      IF (SOLNAM.EQ.'CPXg') THEN
      N=8
      NAME(1)='diopside'      
      NAME(2)='cenh'     
      NAME(3)='cfs'
      NAME(4)='jadeite2'        
      NAME(5)='acmite2'      
      NAME(6)='ocats'
      NAME(7)='dcats'
      NAME(8)='fmc'           
      END IF
!
!-----Opx Holland et al. 2013 ("Green et al. 2012")
!     coded by Thorsten Nagel
      IF (SOLNAM.EQ.'OPXHO13') THEN
      N=5
      NAME(1)='Enstatite'     
      NAME(2)='Ferrosilite'
      NAME(3)='fm-opx'          
      NAME(4)='or-dio'        
      NAME(5)='Mg-tscher-Pyrox'
      END IF
!
!-----HPX Holland et al. 2013 ("Green et al. 2012")
!     coded by Thorsten Nagel
      IF (SOLNAM.EQ.'HPX') THEN
      N=5
      NAME(1)='Enstatite_HP'     
      NAME(2)='Ferrosilite_HP'
      NAME(3)='fm-opx'          
      NAME(4)='or-dio'        
      NAME(5)='Mg-tscher-hpx'   
      END IF
!
!
!-----Ca-Ferrite Holland et al. 2013 ("Green et al. 2012")
!     coded by Thorsten Nagel
      IF (SOLNAM.EQ.'CA_FER') THEN
      N=6   
      NAME(1)='MgAl-ferrite'     
      NAME(2)='CaAl-ferrite'  
      NAME(3)='MgSi-ferrite'    
      NAME(4)='FeSi-ferrite'  
      NAME(5)='oMF-ferrite'     
      NAME(6)='NaAlSi-ferrite2' 
      END IF
!
!     This is just a test for Gibbs-Duhem
      IF (SOLNAM.EQ.'TRI') THEN
      N=3
      NAME(1)='aaa'     
      NAME(2)='bbb'
      NAME(3)='ccc'          
      END IF
!
!-----*********************** START ************************
      DO I=1,EMAX
        EMBCOD(I)=0
      END DO
      DO 503,I=1,K
      CODE=0
      DO II=1,N
        IF (EMSTR(I).EQ.NAME(II)) THEN
        EMBCOD(II)=I
        CODE=1
        GOTO 900
        END IF
      END DO
  900 IF (CODE.EQ.0) THEN
      N=-I
      RETURN
      END IF
  503 CONTINUE
      RETURN
      END
!-----
!********************************
      SUBROUTINE SOLMOD(SOLNAM,K,MODELL)
      CHARACTER(16) SOLNAM
      CHARACTER(500) MODELL
      INTEGER(4) K
      MODELL='NOT EXPLICITLY DEFINED'
!-----**FELDSPARS (Fuhrman and Lindsley(1988))**
      IF (SOLNAM.EQ.'FSP'.OR.SOLNAM.EQ.'FSPc') THEN
      IF (K.EQ.1) MODELL='X(Ab)*(1-X(An)**2)'
      IF (K.EQ.2) MODELL='X(Or)*(1-X(An)**2)'
      IF (K.EQ.3) MODELL='(X(An)*(1+X(An))**2)/4'
      END IF
!-----**FELDSPARS (Ghiorso (1984))**
      IF (SOLNAM.EQ.'G-FSP') THEN
      MODELL='Feldspars: Ghiorso (1984)'
      END IF
!-----**FELDSPARS (Holland & Powell 2003) -- DavDol 09-Mar-05
      IF (SOLNAM.EQ.'FSP-HP1' .OR. SOLNAM.EQ.'FSP-HP2' .OR. &
      SOLNAM.EQ.'FSP-HP3') THEN
      MODELL='Feldspars: Holland & Powell (2003)'
      END IF
!-----**CORDIERITE, IDEAL-RECIPROCAL -- DavDol 13-Mar-05
      IF (SOLNAM.EQ.'CRD-HP') THEN
      MODELL='Cordierite: ideal reciprocal'
      END IF
!-----**BIOTITE, Holland & Powell -- DavDol 13-Mar-05
      IF (SOLNAM.EQ.'BIO-HP') THEN
      MODELL='Biotite: Holland & Powell'
      END IF
!-----**ORTHOPYROXENE, Holland & Powell -- DavDol 13-Mar-05
      IF (SOLNAM.EQ.'OPX-HP') THEN
      MODELL='Orthopyroxene: Holland & Powell'
      END IF
!-----**ORTHOPYROXENE, Holland & Powell 2011
      IF (SOLNAM.EQ.'SOPX') THEN
      MODELL='Orthopyroxene: Holland & Powell 2011'
      END IF
      IF (SOLNAM.EQ.'OPX6') THEN
      MODELL='Orthopyroxene: White et al. 2014'
      END IF
      IF (SOLNAM.EQ.'OPXW14') THEN
      MODELL='Orthopyroxene: White et al. 2014'
      END IF
!-----**NEPHELINE**
      IF (SOLNAM.EQ.'NEPH-KS') THEN
      IF (K.EQ.1) MODELL='X(neph)**4'
      IF (K.EQ.2) MODELL='[(X(ks)+.25X(neph)-.25)/.75]**4'
      END IF
!-----**H2O-CO2: KERRICK AND JACOBS (1981)**
      IF (SOLNAM.EQ.'H2O-CO2'.OR.SOLNAM.EQ.'WCO2') THEN
      MODELL='Kerrick & Jacobs(1981)'
      END IF
!-----**H2O-CO2: Duan (1992)**
      IF (SOLNAM.EQ.'HC-DU92') THEN
      MODELL='Duan et al.(1992)'
      END IF
!-----**H2O-CO2: Duan (2006)**
      IF (SOLNAM.EQ.'HC-DU06') THEN
      MODELL='Duan et al.(2006)'
      END IF
!-----**G-OLIV (Ghiorso (1984))**
      IF (SOLNAM.EQ.'G-OLIV') THEN
      MODELL='Olivine: (Ghiorso (1984))'
      END IF
!-----**Quasi-Chemical models (Pelton (19..))**
      IF (SOLNAM.EQ.'MGSI.L') THEN
      MODELL='quasi-chemical model (Pelton (19..))'
      END IF
      IF (SOLNAM.EQ.'MNTI.L') THEN
      MODELL='quasi-chemical model (Pelton (19..))'
      END IF
      IF (SOLNAM.EQ.'MGTI.L') THEN
      MODELL='quasi-chemical model (Pelton (19..))'
      END IF
      IF (SOLNAM.EQ.'FETI.L') THEN
      MODELL='quasi-chemical model (Pelton (19..))'
      END IF
!-----**Vidals thing**
      IF (SOLNAM.EQ.'MICA4') THEN
      MODELL='Vidal mica'
      END IF
      IF (SOLNAM.EQ.'MICA4o') THEN
      MODELL='Vidal mica'
      END IF
      IF (SOLNAM.EQ.'CHLVIDAL') THEN
      MODELL='CHLVIDAL'
      END IF
      IF (SOLNAM.EQ.'CHLlwv') THEN
      MODELL='CHLlwv'
      END IF
      IF (SOLNAM.EQ.'CHLFe') THEN
      MODELL='CHLFe'
      END IF
!-----**landau ordering**
      IF (SOLNAM.EQ.'P2/n') THEN
      MODELL='landau ordering'
      END IF
!-----**hopo liquid**
      IF (SOLNAM.EQ.'LIQtc') THEN
      MODELL='as usual'
      END IF
!-----**hopo liquid**
      IF (SOLNAM.EQ.'LIQtc6') THEN
      MODELL='as usual'
      END IF
!-----**hopo liquid**
      IF (SOLNAM.EQ.'LIQtc6F') THEN
      MODELL='as usual'
      END IF
!-----**Liquid. Tonalitic metabasite melt: NCKFMASH. Green et al. 2016**
      IF (SOLNAM.EQ.'LIQtc62') THEN
      MODELL='as usual'
      END IF
!-----**hopo spinel**
      IF (SOLNAM.EQ.'SPINtc') THEN
      MODELL='as usual'
      END IF
!-----**PSI spinel**
      IF (SOLNAM.EQ.'SpPSI') THEN
      MODELL='for 290 C only'
      END IF
!-----**chlorite, Hollands webpage**
!----this model fails with Gibbs-Duhem
      IF (SOLNAM.EQ.'CHLHOPO') THEN
      MODELL='chlorite, Hollands webpage'
      END IF
!-----**sapphirine, Kelsey et al 2004**
      IF (SOLNAM.EQ.'SAPPk') THEN
      MODELL='sapphirine, Kelsey et al 2004'
      END IF
!-----**osumilite, Kelsey et al 2004**
      IF (SOLNAM.EQ.'OSUMk') THEN
      MODELL='osumilite, Kelsey et al 2004'
      END IF
!-----**fo-qz, hopo 2003**
      IF (SOLNAM.EQ.'FOQZ') THEN
      MODELL='fo-qz melt, hopo 2003'
      END IF
!-----**amphiboles Dale et al. 2005**
      IF (SOLNAM.EQ.'AmpDHP') THEN
      MODELL='amphiboles Dale et al. 2005'
      END IF
!-----**clinoamphibole: NCFMASHO Diener et al. 2007
!     coded by Dave Kelsey
      IF (SOLNAM.EQ.'ClAMP') THEN
      MODELL='clinoamphibole: Diener et al. 2007'
      END IF
!-----**clinoamphibole: NCFMASHO Green et al. 2016
      IF (SOLNAM.EQ.'ClAMg') THEN
      MODELL='clinoamphibole: Green et al. 2016'
      END IF
!-----**clinoamphibole: NCFMASHO Green et al. 2016
      IF (SOLNAM.EQ.'CAMPG16') THEN
      MODELL='clinoamphibole: Green et al. 2016'
      END IF
!-----**orthoamphibole: NCFMASHO Diener et al. 2007
      IF (SOLNAM.EQ.'OAMP') THEN
      MODELL='orthoamphibole: Diener et al. 2007/2012'
      END IF
!-----**Nal phase Holland et al. 2013
      IF (SOLNAM.EQ.'NALP') THEN
      MODELL='Nal phase: Holland et al. 2013'
      END IF
!-----**Cpx Holland et al. 2013
      IF (SOLNAM.EQ.'CPXHO13') THEN
      MODELL='Cpx: Holland et al. 2013'
      END IF
!-----**clinopyroxene: NCFMASHO Green et al. 2016
      IF (SOLNAM.EQ.'CPXg') THEN
      MODELL='clinoapyroxene: Green et al. 2016'
      END IF
!-----**Opx Holland et al. 2013
      IF (SOLNAM.EQ.'OPXHO13') THEN
      MODELL='Opx: Holland et al. 2013'
      END IF
!-----**Hpx Holland et al. 2013
      IF (SOLNAM.EQ.'HPX') THEN
      MODELL='Hpx: Holland et al. 2013'
      END IF
!-----**Ca-ferrite Holland et al. 2013
      IF (SOLNAM.EQ.'CA_FER') THEN
      MODELL='Ca-ferrite: Holland et al. 2013'
      END IF
!*****
      RETURN
      END
!-----
!********************************
      SUBROUTINE SOLCAL(SOLNAM,P,T,N,X,A,SAR)
      IMPLICIT NONE
      INTEGER(4) EMAX
      PARAMETER (EMAX=15)
      CHARACTER(16) SOLNAM
      REAL(8) P,T,R,X(EMAX),A(EMAX),XCO2,F1,F2,F3,V1,V2,V3, &
      A1,A2,A3,A4,A5,LNG1,LNG2,LNG3,LNG4,LNG5,FF,YY,XX,ZZ, &
      XHP,YHP,NHP,QHP,P1,P2,P3,P4,FEM1,MGM1,ALT1,FET1,MGT1,ALT2,SIT2, &
      SiC,AltC,AlM1C,MgM1C,FeM1C,vM1C,MgM2C,FeM2C,AlM2C,VA,FE3M2, &
      VCA,NAA,CAM4,NAM4,MGM2,ALM2,FEM2,SIT1,F3M2,ALM1,ALM4,FEM4,MGM4, &
      FE,AL,TI,MG,VAT,AC1,AC2,SAR(15),F1PLO,F2PLO,F3M1,CAM2,NAM2, &
      CAM3,MGM3,FEM3,NAM3,SIM1,CAM1,NAM1,SIM2,VM1,KA,TIM2,OHV,OV, &
      MNM1,MNM2, &
      F1PHI,F2PHI,F1PHI2000,F2PHI2000,F1PLO2000,F2PLO2000
      INTEGER(4) N
      DATA R/8.3143D0/
!-----this is just a test
      IF (SOLNAM.EQ.'TRI') THEN
      A(1)=X(1)**7.0D0
      A(2)=X(2)**7.0D0
      A(3)=X(3)**2.0D0
      RETURN
      END IF
!-----**FELDSPARS (Fuhrman and Lindsley(1988))**
      IF (SOLNAM.EQ.'FSP'.OR.SOLNAM.EQ.'FSPc') THEN
      A(1)=X(1)*(1.0D0-(X(3)**2))
      A(2)=X(2)*(1.0D0-(X(3)**2))
      A(3)=(X(3)*(1.0D0+X(3))**2)/4.0D0
      RETURN
      END IF
!-----**FELDSPARS (Ghiorso (1984))**
      IF (SOLNAM.EQ.'G-FSP') THEN
      F1=X(2)**2*(28211.0974D0-39485.4998D0*X(1))/(R*T)
      F2=X(1)**2*(8468.3475D0+39485.4998D0*X(2))/(R*T)
      A(1)=X(1)*(2.0D0-X(1))*DEXP(F1)
      A(2)=0.25D0*X(2)*(1.0D0+X(2))*DEXP(F2)
      A(3)=X(3)
      RETURN
      END IF
!-----**FELDSPARS (Holland & Powell 2003), David Dolejs 09-Mar-05 **
      IF (SOLNAM.EQ.'FSP-HP1' .OR. SOLNAM.EQ.'FSP-HP2' .OR. &
      SOLNAM.EQ.'FSP-HP3') THEN
      V1=1.0
      V2=0.643
      V3=1.0
      F1=X(1)*V1/(X(1)*V1+X(2)*V2+X(3)*V3)
      F2=X(2)*V2/(X(1)*V1+X(2)*V2+X(3)*V3)
      F3=X(3)*V3/(X(1)*V1+X(2)*V2+X(3)*V3)
      A1=(1-F1)*F2*2.0*V1/(V1+V2)*(25100.0-10.8*T+0.343*P) &
      +(1-F1)*F3*2.0*V1/(V1+V3)*40000.0-F2*F3*2.0*V1/(V2+V3)*3100.0
      A2=(1-F2)*F1*2.0*V2/(V2+V1)*(25100.0-10.8*T+0.343*P) &
      +(1-F2)*F3*2.0*V2/(V2+V3)*3100.0-F1*F3*2.0*V2/(V1+V3)*40000.0
      A3=(1-F3)*F1*2.0*V3/(V1+V3)*40000.0+ &
      (1-F3)*F2*2.0*V3/(V2+V3)*3100. &
      -F1*F2*2.0*V3/(V2+V1)*(25100.0-10.8*T+0.343*P)
      A(1)=X(1)*DEXP(A1/(R*T))
      A(2)=X(2)*DEXP(A2/(R*T))
      A(3)=X(3)*DEXP(A3/(R*T))
      RETURN
      END IF
!-----**CORDIERITE, IDEAL/RECIPROCAL, David Dolejs 12-Mar-05 **
      IF (SOLNAM.EQ.'CRD-HP') THEN
      A(1)=(X(1)+X(3))**2*(X(1)+X(2))
      A(2)=(X(2)+X(4))**2*(X(1)+X(2))
      A(3)=(X(1)+X(3))**2*(X(3)+X(4))
      A(4)=(X(2)+X(4))**2*(X(3)+X(4))
      RETURN
      END IF
!-----**BIOTITE, Holland & Powell, David Dolejs 13-Mar-05 **
      IF (SOLNAM.EQ.'BIO-HP') THEN
! ----calculate compositional variables
      XHP=(3.0*X(2)+X(4)+2.0*X(5))/(3.0*X(2)+X(4)+2.0*X(5)+3.0*X(1)+ &
      2.0*X(3)+2.0*X(4))
      YHP=X(3)+X(5)
      NHP=3.0*(XHP-X(2)-X(5))
! ----ideal mixing activities
      A1=4.0*((1.0-XHP)*(1.0-YHP)-2.0/3.0*NHP)*(1.0-XHP+NHP/3.0)**2 &
      *(0.5+0.5*YHP)*(0.5-0.5*YHP)
      A2=4.0*(XHP*(1.0-YHP)+2.0/3.0*NHP)*(XHP-NHP/3.0)**2 &
      *(0.5+0.5*YHP)*(0.5-0.5*YHP)
      A3=YHP*(1.0-XHP+NHP/3.0)**2*(0.5+0.5*YHP)**2
      A4=4.0*(XHP*(1.0-YHP)+2.0/3.0*NHP)*(1.0-XHP+NHP/3.0)**2 &
      *(0.5+0.5*YHP)*(0.5-0.5*YHP)
      A5=YHP*(XHP-NHP/3.0)**2*(0.5+0.5*YHP)**2
!-----independent mole fractions
      P1=(1.0-XHP)*(1.0-YHP)-2.0/3.0*NHP
      P2=XHP-NHP/3.0
      P3=YHP
      P4=-XHP*YHP+NHP
!-----activity coefficients
      LNG1=P2*(1.0-P1)*9000.0+P4*(1.0-P1)*3000.0+P3*(1.0-P1)*10000.0 &
      -P3*P2*(-1000.0)-P4*P3*10000.0-P4*P2*6000.0
      LNG2=P1*(1.0-P2)*9000.0+P4*(1.0-P2)*6000.0+ &
      P3*(1.0-P2)*(-1000.0) &
      -P4*P1*3000.0-P4*P3*10000.0-P3*P1*10000.0
      LNG3=P1*(1.0-P3)*10000.0+P2*(1.0-P3)*(-1000.0) &
      +P4*(1.0-P3)*10000.0 &
      -P1*P2*9000.0-P4*P2*6000.0-P4*P1*3000.0
      LNG4=P2*(1.0-P4)*6000.0+P1*(1.0-P4)*3000.0+P3*(1.0-P4)*10000.0 &
      -P1*P2*9000.0-P3*P1*10000.0-P2*P3*(-1000.0)
      LNG5=P1*(1.0-P2)*9000.0+P1*(1.0-P3)*10000.0-P1*(1.0+P4)*3000.0 &
      -(1.0-P2)*(1.0-P3)*(-1000.0)+(1.0-P2)*(1.0+P4)*6000.0 &
      +(1.0-P3)*(1.0+P4)*10000.0
!-----activities
      A(1)=A1*DEXP(LNG1/(R*T))
      A(2)=A2*DEXP(LNG2/(R*T))
      A(3)=A3*DEXP(LNG3/(R*T))
      A(4)=A4*DEXP(LNG4/(R*T))
      A(5)=A5*DEXP(LNG5/(R*T))
      RETURN
      END IF
!-----**ORTHOPYROXENE, Holland & Powell 2011
      IF (SOLNAM.EQ.'SOPX') THEN
      MGM1=X(1)+X(4)
      FEM1=X(2)
      ALM1=X(3)
      MGM2=X(1)+X(3)
      FEM2=X(2)+X(4)
      SIT1=X(1)+X(2)+X(3)/2.0D0+X(4)
      ALT1=X(3)/2.0D0
      A(1)=MGM1*MGM2*SIT1**0.5D0
      A(2)=FEM1*FEM2*SIT1**0.5D0
      A(3)=DSQRT(2.0D0)*ALM1*MGM2*SIT1**0.25D0*ALT1**0.25D0
      A(4)=MGM1*FEM2*SIT1**0.5D0
      END IF
!-----** ORTHOPYROXENE, White et al. 2014
!-----** taken from tc-6NCKFMASHTO.txt in thermocalc_340i
      IF (SOLNAM.EQ.'OPX6'.OR.SOLNAM.EQ.'OPXW14') THEN
! OPX6                     M1 - M2 - T1
!   1  enstatite           Mg - Mg - Si,Si       1.0   0   0
!   2  ferrosilite         Fe - Fe - Si,Si       1.0   0   0
!   3  FM_pyx              Mg - Fe - Si,Si       1.0   0   0
!   4  Mg-tscher_pyrox     Al - Mg - Al,Si       1.0   0   0
!   5  fopx                F3 - Mg - Al,Si       1.0   0   0
!   6  Mn-Opx              Mn - Mn - Si,Si       1.0   0   0
!   7  Orthodiopside       Mg - Ca - Si,Si       1.2   0   0
!
      MGM1=X(1)+X(3)+x(7)
      FEM1=X(2)
      F3M1=X(5)
      ALM1=X(4)
      MNM1=X(6)
      MGM2=X(1)+X(4)+X(5)
      FEM2=X(2)+X(3)
      CAM2=X(7)
      MNM2=X(6)
      ALT1=(X(4)+X(5))/2.0D0
      SIT1=X(1)+X(2)+X(3)+X(4)/2.0D0+X(5)/2.0D0+X(6)+X(7)
      A(1)=MGM1*MGM2*SIT1**0.5D0
      A(2)=FEM1*FEM2*SIT1**0.5D0
      A(3)=MGM1*FEM2*SIT1**0.5D0
      A(4)=ALM1*MGM2*ALT1**0.25D0*SIT1**0.25D0*DSQRT(2.0D0)
      A(5)=F3M1*MGM2*ALT1**0.25D0*SIT1**0.25D0*DSQRT(2.0D0)
      A(6)=MNM1*MNM2*SIT1**0.5D0
      A(7)=MGM1*CAM2*SIT1**0.5D0
      END IF
!-----**ORTHOPYROXENE, Holland & Powell -- David Dolejs 13-Mar-05
      IF (SOLNAM.EQ.'OPX-HP') THEN
!-----compositional variables
      XHP=(2.0*X(2)+X(3)+X(5))/(2.0-X(5)-X(4))
      YHP=X(4)+X(5)
      QHP=2.0*(X(2)+X(3)+X(5)-(2.0*X(2)+X(3)+X(5))/(2.0-X(5)-X(4)))
!-----ideal mixing activities
      A1=(1.0-XHP-QHP/2.0)*(1.0-YHP+QHP/2.0-XHP*(1.0-YHP))
      A2=(XHP+QHP/2.0)*(-QHP/2.0+XHP*(1.0-YHP))
      A3=(XHP+QHP/2.0)*(1.0-YHP+QHP/2.0-XHP*(1-YHP))
      A4=(1.0-XHP-QHP/2.0)*YHP
      A5=(XHP+QHP/2.0)*YHP
!-----independent mole fractions
      P1=1.0-XHP-YHP-QHP/2.0
      P2=-QHP/2.0+XHP*(1.0-YHP)
      P3=QHP+XHP*YHP
      P4=YHP
!-----activity coefficients
      LNG1=(1.0-P1)*P2*6800.0+(1.0-P1)*P3*4500.0-P2*P4*(-1000.0) &
      -P2*P3*4500.0-P4*P3*1200.0
      LNG2=P1*(1.0-P2)*6800.0-P1*P3*4500.0+(1.0-P2)*P4*(-1000.0) &
      +(1.0-P2)*P3*4500.0-P4*P3*1200.0
      LNG3=-P1*P2*6800.0+P1*(1.0-P3)*4500.0-P2*P4*(-1000.0) &
      +P2*(1.0-P3)*4500.0+P4*(1.0-P3)*1200.0
      LNG4=-P1*P2*6800.0-P1*P3*4500.0+P2*(1.0-P4)*(-1000.0) &
      -P2*P3*4500.0+(1.0-P4)*P3*1200.0
      LNG5=-(1.0+P1)*P2*6800.0+(1.0+P1)*(1.0-P3)*4500.0 &
      +P2*(1.0-P4)*(-1000.0)+P2*(1.0-P3)*4500-(1.0-P4)*(1.0-P3)*1200
!-----activities
      A(1)=A1*DEXP(LNG1/(R*T))
      A(2)=A2*DEXP(LNG2/(R*T))
      A(3)=A3*DEXP(LNG3/(R*T))
      A(4)=A4*DEXP(LNG4/(R*T))
      A(5)=A5*DEXP(LNG5/(R*T))
      RETURN
      END IF
!-----**NEPHELINE**
      IF (SOLNAM.EQ.'NEPH-KS') THEN
      A(1)=X(1)**4
      A(2)=((X(2)+0.25D0*X(1)-0.25D0)/0.75D0)**4
      RETURN
      END IF
!-----**H2O-CO2: KERRICK AND JACOBS (1981)**
      IF (SOLNAM.EQ.'H2O-CO2'.OR.SOLNAM.EQ.'WCO2') THEN
      XCO2=X(2)
      CALL MIXKJ(T,P,XCO2,F1,F2,A1,A2)
      A(1)=A2
      A(2)=A1
      RETURN
      END IF
!-----**H2O-CO2: Duan (1992)**
      IF (SOLNAM.EQ.'HC-DU92') THEN
      F1PLO=0.0D0
      F2PLO=0.0D0
      CALL DUAN92EQ(T,P,X,VAT,AC1,AC2,F1PLO,F2PLO)
      A(1)=AC1
      A(2)=AC2
      RETURN
      END IF
!-----**H2O-CO2: Duan (2006)**
      IF (SOLNAM.EQ.'HC-DU06') THEN
      IF (SAR(1).EQ.T.AND.SAR(2).EQ.P) THEN
!!      WRITE (UNIT=6,FMT='('' PLUS P T'',2(F20.10))') T,P
      F1PLO=SAR(3)
      F2PLO=SAR(4)
      F1PHI=SAR(5)
      F2PHI=SAR(6)
      F1PHI2000=SAR(7)
      F2PHI2000=SAR(8)
      F1PLO2000=SAR(9)
      F2PLO2000=SAR(10)
      ELSE
!!      WRITE (UNIT=6,FMT='('' MINUS P T'',2(F20.10))') T,P
      F1PLO=0.0D0
      F2PLO=0.0D0
      F1PHI=0.0D0
      F2PHI=0.0D0
      F1PHI2000=0.0D0
      F2PHI2000=0.0D0
      F1PLO2000=0.0D0
      F2PLO2000=0.0D0
      END IF
      CALL DUAN06HL(T,P,X,VAT,AC1,AC2,F1PLO,F2PLO, &
      F1PHI,F2PHI,F1PHI2000,F2PHI2000,F1PLO2000,F2PLO2000)
      SAR(1)=T
      SAR(2)=P
      SAR(3)=F1PLO
      SAR(4)=F2PLO
      SAR(5)=F1PHI
      SAR(6)=F2PHI
      SAR(7)=F1PHI2000
      SAR(8)=F2PHI2000
      SAR(9)=F1PLO2000
      SAR(10)=F2PLO2000
      A(1)=AC1
      A(2)=AC2
      END IF
!-----**G-OLIV (Ghiorso (1984))**
!-----Formel noch nicht richtig
      IF (SOLNAM.EQ.'G-OLIV') THEN
      F1=DEXP((4.1819D3*(1.0D0+2.0D0*X(1))*(1.0D0-X(1))**2)/(R*T))
      F2=DEXP((8.3638D3*(1.0D0-X(2))*(1.0D0-X(2))**2)/(R*T))
      A(1)=(X(1)*F1)**2
      A(2)=(X(2)*F2)**2
!-----
      RETURN
      END IF
!-----**Quasi-Chemical models (Pelton (19..))**
      IF (SOLNAM.EQ.'MGSI.L') CALL MGSI(SOLNAM,P,T,N,X,A)
      IF (SOLNAM.EQ.'MNTI.L') CALL MNTI(SOLNAM,P,T,N,X,A)
      IF (SOLNAM.EQ.'MGTI.L') CALL MGTI(SOLNAM,P,T,N,X,A)
      IF (SOLNAM.EQ.'FETI.L') CALL FETI(SOLNAM,P,T,N,X,A)
      IF (SOLNAM.EQ.'MICA4o') CALL MICA4O(SOLNAM,P,T,N,X,A)
      IF (SOLNAM.EQ.'MICA4') CALL MICA4(SOLNAM,P,T,N,X,A)
!-----**landau ordering**
      IF (SOLNAM.EQ.'P2/n') CALL LANDAU1(SOLNAM,P,T,N,X,A)
      IF (SOLNAM.EQ.'hochlo') THEN
      A(1)=(X(1)+X(2))*X(1)*(X(1)+X(2)/2)**2
      A(2)=4.0*(X(1)+X(2))*(X(3)+X(2))*(X(1)+X(2)/2)*(X(3)+X(2)/2)
      A(3)=X(3)*(X(3)+X(2))*(X(3)+X(2)/2)**2
      END IF
!-----**hopo liquid**
      IF (SOLNAM.EQ.'LIQtc') THEN
      FF=(X(6)+X(7))
      IF (FF.EQ.0.0D0) FF=1.0D0
      F1=X(6)/FF
      F2=X(7)/FF
      A(1)=(1.0D0-X(8))*X(1)
      A(2)=(1.0D0-X(8))*X(2)
      A(3)=(1.0D0-X(8))*X(3)
      A(4)=(1.0D0-X(8))*X(4)
      A(5)=(1.0D0-X(8))*X(5)
!      A(6)=(1.0D0-X(8))*X(6)*F1**4
!      A(7)=(1.0D0-X(8))*X(7)*F2**4
! according to white et al 2007, equation is ...(FF)*F1**5
! this is identical to X(6)*F1**4, because F1=X(6)/FF -> X(6) = F1*FF
!      A(6)=(1.0D0-X(8))*(FF)*F1**5
!      A(7)=(1.0D0-X(8))*(FF)*F2**5
      A(6)=(1.0D0-X(8))*X(6)*F1**4
      A(7)=(1.0D0-X(8))*X(7)*F2**4
! according to white et al 2007, equation is X(8)**5
! probably a typing error.
      A(8)=X(8)**2
      END IF
!-----**hopo liquid**
      IF (SOLNAM.EQ.'LIQtc6') THEN
      FF=(X(6)+X(7))
      IF (FF.EQ.0.0D0) FF=1.0D0
      F1=X(6)/FF
      F2=X(7)/FF
      A(1)=(1.0D0-X(8))*X(1)
      A(2)=(1.0D0-X(8))*X(2)
      A(3)=(1.0D0-X(8))*X(3)
      A(4)=(1.0D0-X(8))*X(4)
      A(5)=(1.0D0-X(8))*X(5)
!      A(6)=(1.0D0-X(8))*X(6)*F1**4
!      A(7)=(1.0D0-X(8))*X(7)*F2**4
! according to white et al 2007, equation is ...(FF)*F1**5
! this is identical to X(6)*F1**4, because F1=X(6)/FF -> X(6) = F1*FF
!      A(6)=(1.0D0-X(8))*(FF)*F1**5
!      A(7)=(1.0D0-X(8))*(FF)*F2**5
      A(6)=(1.0D0-X(8))*X(6)*F1**4
      A(7)=(1.0D0-X(8))*X(7)*F2**4
! according to white et al 2007, equation is X(8)**5
! probably a typing error.
      A(8)=X(8)**2
      END IF
!-----**hopo liquid**
      IF (SOLNAM.EQ.'LIQtc6F') THEN
      FF=(X(6)+X(7))
      IF (FF.EQ.0.0D0) FF=1.0D0
      F1=X(6)/FF
      F2=X(7)/FF
      A(1)=(1.0D0-X(8))*X(1)
      A(2)=(1.0D0-X(8))*X(2)
      A(3)=(1.0D0-X(8))*X(3)
      A(4)=(1.0D0-X(8))*X(4)
      A(5)=(1.0D0-X(8))*X(5)
!      A(6)=(1.0D0-X(8))*X(6)*F1**4
!      A(7)=(1.0D0-X(8))*X(7)*F2**4
! according to white et al 2007, equation is ...(FF)*F1**5
! this is identical to X(6)*F1**4, because F1=X(6)/FF -> X(6) = F1*FF
!      A(6)=(1.0D0-X(8))*(FF)*F1**5
!      A(7)=(1.0D0-X(8))*(FF)*F2**5
      A(6)=(1.0D0-X(8))*X(6)*F1**4
      A(7)=(1.0D0-X(8))*X(7)*F2**4
! according to white et al 2007, equation is X(8)**5
! probably a typing error.
      A(8)=X(8)**2
      A(9)=X(9)*(1.0D0-X(8))
      END IF
!
!-----**Liquid. Tonalitic metabasite melt: NCKFMASH. Green et al. 2016**
!
!-1   Quartz_liq           X(1)
!-2   Albite_liq           X(2)
!-3   K-feldespar_liq      X(3)
!-4   Wollastonite_liq     X(4)
!-5   Sillimanite_liq      X(5)
!-6   Fayalite_liq         X(6)
!-7   Forsterite_liq       X(7)
!-8   H2O_liq              X(8)
!-9   Anorthite_liq        X(9)
!
      IF (SOLNAM.EQ.'LIQtc62') THEN
      FF=(X(6)+X(7))
      IF (FF.EQ.0.0D0) FF=1.0D0
      F1=X(6)/FF
      F2=X(7)/FF
      A(1)=(1.0D0-X(8))*X(1)
      A(2)=(1.0D0-X(8))*X(2)
      A(3)=(1.0D0-X(8))*X(3)
      A(4)=(1.0D0-X(8))*X(4)
      A(5)=(1.0D0-X(8))*X(5)
      A(6)=(1.0D0-X(8))*(X(6)+X(7))*F1**5
      A(7)=(1.0D0-X(8))*(X(6)+X(7))*F2**5
      A(8)=X(8)**2
      A(9)=(1.0D0-X(8))*X(9)
      END IF
!-----**hopo spinel** white et al 2007
      IF (SOLNAM.EQ.'SPINtc') THEN
      FE=(X(1)+X(3)+2.0D0*X(4))/(1.0D0+X(4))
      MG=X(2)/(1.0D0+X(4))
      AL=(X(1)+X(2))
      F3=X(3)
      TI=X(4)
      A(1)=AL*FE
      A(2)=AL*MG
      A(3)=F3*FE
      A(4)=TI*FE
      END IF
!-----**PSI spinel**
      IF (SOLNAM.EQ.'SpPSI') CALL PSISPIN(SOLNAM,P,T,N,X,A)




!-----**CHLORITE LANARI**
!                            T1(2)      T2(2)     M1(1)    M23(4)
!                            -----      -----     -----    ------
!      1  clinochlore        SiSi       SiAl      Mg       MgMgMgMg
!      2  daphnite           SiSi       SiAl      Fe       FeFeFeFe
!      3  amesite            SiSi       AlAl      Al       MgMgMgMg
!      4  sudLANARI          SiSi       SiAl      v        MgMgAlAl
!
      IF (SOLNAM.EQ.'CHLlwv') THEN
      XX=5.0D0*X(2)/(5.0D0*X(2)+4.0D0*X(3)+5.0D0*X(1)+2.0D0*X(4))
!      YY=X(3)+0.5*(X(1)+X(2)+X(4))
      YY=X(3)
      ZZ=X(4)

      FEM2=XX*(1.0-0.5*ZZ)
      MGM2=(1-XX)*(1.0-0.5*ZZ)
      ALM2=0.5*ZZ
      FEM1=XX*(1.0-YY-ZZ)
      MGM1=(1.0-XX)*(1.0-YY-ZZ)
      ALM1=YY
      VM1=ZZ
      ALT2=1.0-0.5*(1.0-YY)
      SIT2=0.5*(1.0-YY)


!      FEM2=X(2)
!      MGM2=X(1)+X(3)+X(4)/2.0D0
!      ALM2=X(4)/2.0D0
!      FEM1=X(2)
!      MGM1=X(1)
!      ALM1=X(3)
!      VM1=X(4)
!      ALT2=X(3)+(X(1)+X(2)+X(4))/2.0D0
!      SIT2=(X(1)+X(2)+X(4))/2.0D0
!
!----- clino
      A(1)=4*MGM2**4*MGM1*ALT2*SIT2
!----- daph
      A(2)=4*FEM2**4*FEM1*ALT2*SIT2
!----- Mg-ames
      A(3)=MGM2**4*ALM1*ALT2**2
!----- sud
      A(4)=64.0*ALM2**2*MGM2**2*VM1*ALT2*SIT2
      RETURN
      END IF




!**********CHLORITEVIDAL*******************
      IF (SOLNAM.EQ.'CHLVIDAL') THEN
      SiC=(X(1)+X(2)+X(4))/2
      AltC=(X(1)+X(2)+X(4))/2+(X(3))
      AlM1C=X(3)
      MgM1C=X(1)
      FeM1C=X(2)
      vM1C=X(4)
      MgM2C=(X(1)+X(3))+0.5*X(4)
      FeM2C=X(2)
      AlM2C=X(4)/2
!----- clino
      A(1)=4*SiC*AltC*MgM1C*MgM2C**4
!----- daph
      A(2)=4*SiC*AltC*FeM1C*FeM2C**4
!----- Mg-ames
      A(3)=AltC**2*AlM1C*MgM2C**4
!----- sud
      A(4)=64*SiC*AltC*vM1C*MgM2C**2*AlM2C**2
      RETURN
      END IF
!**********CHLORITEFe*******************
      IF (SOLNAM.EQ.'CHLFe') THEN
      SiC=(X(1)+X(2)+X(4))/2
      AltC=(X(1)+X(2)+X(4))/2+(X(3))
      AlM1C=X(3)
      MgM1C=X(1)
      FeM1C=X(2)
      vM1C=X(4)
      MgM2C=(X(1))+0.5*X(4)
      FeM2C=X(2)+X(3)
      AlM2C=X(4)/2
!C      SiC=(X(1)+X(2))/2
!C      AltC=(X(1)+X(2))/2+X(3)
!C      AlM1C=X(3)
!C      MgM1C=X(1)
!C      FeM1C=X(2)
!C      MgM2C=(X(1))
!C      FeM2C=X(2)+X(3)
!----- clino
      A(1)=4*SiC*AltC*MgM1C*MgM2C**4
!----- daph
      A(2)=4*SiC*AltC*FeM1C*FeM2C**4
!----- Fe-ames
      A(3)=AltC**2*AlM1C*FeM2C**4
!----- sud
      A(4)=64*SiC*AltC*vM1C*MgM2C**2*AlM2C**2
      RETURN
      END IF
!-----**chlorite, Hollands webpage**
!----this model fails with Gibbs-Duhem, (typing error??)
!     IF (SOLNAM.EQ.'CHLHOPO') THEN
!      YY=(1.0D0+X(4)-X(1))/2.0D0
!      NN=(1.0D0-X(1)-X(4))/2.0D0
!      XX=(5.0D0*X(3)+1.0D0+X(4)+X(1))/6.0D0
!      A(1)=(1.0D0-XX)**6*(1.0D0-YY+NN)*(1.0D0-YY-NN)*(1.0D0-YY)**2
!      A(2)=4.0D0*(1.0D0-XX)**5*(1.0D0-YY+NN)*(YY+NN)*(1.0D0-YY)*YY
!      A(3)=(1.0D0-XX)**4*(YY-NN)*(YY+NN)*YY**2
!      A(4)=4.0D0*XX**5*(1.0D0-YY+NN)*(YY+NN)*(1.0D0-YY)*YY
!      END IF
!-----**chlorite, Hollands webpage NEW FORMULATION**
!----this model fails with Gibbs-Duhem, (typing error??)
      IF (SOLNAM.EQ.'CHLHOPO') THEN
!C                                    M2  M1  M4   T2
!C      NAME(1)='Al-free-chlorite'   MG4  MG  MG  SI2
!C      NAME(2)='clinochlore'        MG4  MG  AL  SIAL
!C      NAME(3)='daphnite'           FE4  FE  AL  SIAL
!C      NAME(4)='amesite'            MG4  AL  AL  AL2
      XX=X(3)
      YY=(X(2)+X(3))/2.0D0+X(4)
      QHP=(X(2)+X(3)+X(4)-X(4))/2.0D0
      FEM2=XX
      MGM2=1.0D0-XX
      ALM1=YY-QHP
      FEM1=XX*(1.0D0-YY+QHP)
      MGM1=(1.0D0-XX)*(1.0D0-YY+QHP)
      ALM4=YY+QHP
      FEM4=XX*(1.0D0-YY-QHP)
      MGM4=(1.0D0-XX)*(1.0D0-YY-QHP)
      ALT2=YY
      SIT2=1.0D0-YY
!C
      A(1)=MGM2**4*MGM1*MGM4*SIT2**2
      A(2)=4.0D0*MGM2**4*MGM1*ALM4*ALT2*SIT2
      A(3)=4.0D0*FEM2**4*FEM1*ALM4*ALT2*SIT2
      A(4)=MGM2**4*ALM1*ALM4*ALT2**2
      END IF
!-----**sapphirine, Kelsey et al 2004**
      IF (SOLNAM.EQ.'SAPPk') THEN
      YY=X(3)
      XX=4.0D0*X(2)/(4.0D0-X(3))
      A(1)=(1.0D0-XX)*(1.0D0-YY)*((1.0D0-XX)**3)*(1.0D0-YY)
      A(2)=XX*(1.0D0-YY)*(XX**3)*(1.0D0-YY)
      A(3)=YY*((1.0D0-XX)**3)*YY
      END IF
!-----**osumilite, Kelsey et al 2004**
      IF (SOLNAM.EQ.'OSUMk') THEN
      YY=X(3)
      XX=2.0D0*X(2)/(2.0D0+X(3))
      FEM1=XX
      MGM1=(1.0D0-XX)
      ALT1=(1.0D0-YY/3.0D0)
      FET1=XX*YY/3.0D0
      MGT1=(1.0D0-XX)*YY/3.0D0
      ALT2=(1.0D0-YY/2.0D0)
      SIT2=YY/2.0D0
      A(1)=(MGM1**2)*(ALT1**3)*(ALT2**2)
      A(2)=(FEM1**2)*(ALT1**3)*(ALT2**2)
      A(3)=27.0D0*(MGM1**2)*(MGT1)*(ALT1**2)*(ALT2)*(SIT2)
      END IF
!-----**fo-qz melt,IDEAL hopo 2003**
      IF (SOLNAM.EQ.'FOQZ') THEN
      A(1)=X(1)
      A(2)=X(2)
      A(3)=X(3)
      END IF
!-----**amphiboles Dale et al. 2005**
!- AmpDHP            A(1) - M4(2) - M13(3) - M2(2) - T1(4)
!-1 tremolite         v   - Ca2   - Mg3    - Mg,Mg - Si4
!-2 tschermakite2     v   - Ca2   - Mg3    - Al,Al - Al2Si2
!-3 pargasite2       Na   - Ca2   - Mg3    - Mg,Al - Al2Si2
!-4 glaucophane2      v   - Na2   - Mg3    - Al,Al - Si4
!-5 ferroactinolite   v   - Ca2   - FE3    - Fe,Fe - Si4
!-6 ferritschermak    v   - Ca2   - Mg3    - F3,F3 - Al2Si2
!=
!--- with the following: Gibbs-Duehm test fails
!      XX=5.0D0*X(5)/(5.0D0*X(1)+3.0D0*X(2)+4.0D0*X(3)
!     >+3.0D0*X(4)+5.0D0*X(5))
!      AA=X(3)
!      ZZ=X(4)
!      YY=X(2)+X(3)/2.0D0+X(4)
!      NAA=AA
!      VCA=1.0D0-AA
!      NAM4=ZZ
!      CAM4=1.0D0-ZZ
!      FEM1=XX
!      MGM1=1.0D0-XX
!      ALM2=YY
!      FEM2=(1.0D0-YY)*XX
!      MGM2=(1.0D0-YY)*(1.0D0-XX)
!      ALT1=YY/2.0D0-ZZ/2.0D0+AA/4.0D0
!      SIT1=1.0D0-YY/2.0D0+XX/2.0D0-AA/4.0D0
!=
      IF (SOLNAM.EQ.'AmpDHP') THEN
      VCA=1.0D0-X(3)
      NAA=X(3)
      CAM4=1.0D0-X(4)
      NAM4=X(4)
      MGM1=1.0D0-X(5)
      FEM1=X(5)
      MGM2=X(1)+X(3)/2.0D0
      ALM2=X(2)+X(3)/2.0D0+X(4)
      FEM2=X(5)
      F3M2=X(6)
      SIT1=X(1)+X(4)+X(5)+(X(2)+X(3)+X(6))/2.0D0
      ALT1=(X(2)+X(3)+X(6))/2.0D0
!----
      A(1)=VCA*CAM4**2*MGM1**3*MGM2**2*SIT1
      A(2)=VCA*CAM4**2*MGM1**3*ALM2**2*DSQRT(SIT1*ALT1)*2.0D0
      A(3)=NAA*CAM4**2*MGM1**3*MGM2*ALM2*DSQRT(SIT1*ALT1)*8.0D0
      A(4)=VCA*NAM4**2*MGM1**3*ALM2**2*SIT1
      A(5)=VCA*CAM4**2*FEM1**3*FEM2**2*SIT1
      A(6)=VCA*CAM4**2*MGM1**3*F3M2**2*DSQRT(SIT1*ALT1)*2.0D0
      END IF
!------------------------------------------------
!-----clinoamphibole: NCFMASHO Diener et al. 2007
!------------------------------------------------
!                      A      M13      M2      M4         T1
!-1  tremolite         v - Mg,Mg,Mg - Mg,Mg - Ca,Ca - Si,Si,Si,Si       1.0  0  0
!-2  tschermakite2     v - Mg,Mg,Mg - Al,Al - Ca,Ca - Si,Si,Al,Al       1.5  0  0
!-3  pargasite2       Na - Mg,Mg,Mg - Mg,Al - Ca,Ca - Si,Si,Al,Al       1.7  0  0
!-4  glaucophane2      v - Mg,Mg,Mg - Al,Al - Na,Na - Si,Si,Si,Si       0.8  0  0
!-5  cummingtonite2    v - Mg,Mg,Mg - Mg,Mg - Mg,Mg - Si,Si,Si,Si       1.0  0  0
!-6  grunerite2        v - Fe,Fe,Fe - Fe,Fe - Fe,Fe - Si,Si,Si,Si       1.0  0  0
!-7  acam              v - Mg,Mg,Mg - Fe,Fe - Fe,Fe - Si,Si,Si,Si       1.0  0  0
!-8  bcam              v - Fe,Fe,Fe - Mg,Mg - Fe,Fe - Si,Si,Si,Si       1.0  0  0
!-9  mgriebekite       v - Mg,Mg,Mg - F3,F3 - Na,Na - Si,Si,Si,Si       0.8  0  0
!=
      IF (SOLNAM.EQ.'ClAMP') THEN
      VA=1.0D0-X(3)
      NAA=X(3)
      MGM1=1.0D0-X(6)-X(8)
      FEM1=X(6)+X(8)
      MGM2=X(1)+X(5)+X(8)+X(3)/2.0D0
      FEM2=X(6)+X(7)
      ALM2=X(2)+X(4)+X(3)/2.0D0
      FE3M2=X(9)
      CAM4=X(1)+X(2)+X(3)
      MGM4=X(5)
      FEM4=X(6)+X(7)+X(8)
      NAM4=X(4)+X(9)
      SIT1=1.0D0-(X(2)+X(3))/2.0D0
      ALT1=(X(2)+X(3))/2.0D0
!-
      A(1)=VA*MGM1**3*MGM2**2*CAM4**2*SIT1
      A(2)=VA*MGM1**3*ALM2**2*CAM4**2*DSQRT(SIT1*ALT1)*2.0D0
      A(3)=NAA*MGM1**3*MGM2*ALM2*CAM4**2*DSQRT(SIT1*ALT1)*8.0D0
      A(4)=VA*MGM1**3*ALM2**2*NAM4**2*SIT1
      A(5)=VA*MGM1**3*MGM2**2*MGM4**2*SIT1
      A(6)=VA*FEM1**3*FEM2**2*FEM4**2*SIT1
      A(7)=VA*MGM1**3*FEM2**2*FEM4**2*SIT1
      A(8)=VA*FEM1**3*MGM2**2*FEM4**2*SIT1
      A(9)=VA*MGM1**3*FE3M2**2*NAM4**2*SIT1
      END IF
!-----
!-----clinoamphibole: NCFMASHO Green et al. 2016

!ClAMg        (-EXT,MARGULES)  A(1):v,Na - M1(3):Mg,Fe - M2(2):Mg,Al,Fe,F3,Ti - M4(2):Ca,Na,Mg,Fe - T(4):Si,Al - V(2):OH,O
!                      A      M13      M2      M4         T1         V
!-01  tremolite        v - Mg,Mg,Mg - Mg,Mg - Ca,Ca - Si,Si,Si,Si - OH       1.0  0  0
!-02  tschermakite     v - Mg,Mg,Mg - Al,Al - Ca,Ca - Si,Si,Al,Al - OH       1.5  0  0
!-03  pargasite       Na - Mg,Mg,Mg - Mg,Al - Ca,Ca - Si,Si,Al,Al - OH       1.7  0  0
!-04  glaucophane      v - Mg,Mg,Mg - Al,Al - Na,Na - Si,Si,Si,Si - OH       0.8  0  0
!-05  cummingtonite    v - Mg,Mg,Mg - Mg,Mg - Mg,Mg - Si,Si,Si,Si - OH       1.0  0  0
!-06  grunerite        v - Fe,Fe,Fe - Fe,Fe - Fe,Fe - Si,Si,Si,Si - OH       1.0  0  0
!-07  acam             v - Mg,Mg,Mg - Fe,Fe - Fe,Fe - Si,Si,Si,Si - OH       1.0  0  0
!-08  bcam             v - Fe,Fe,Fe - Mg,Mg - Fe,Fe - Si,Si,Si,Si - OH       1.0  0  0
!-09  mgriebekite      v - Mg,Mg,Mg - F3,F3 - Na,Na - Si,Si,Si,Si - OH       0.8  0  0 
!-10  kpargasite       K - Mg,Mg,Mg - Mg,Al - Ca,Ca - Si,Si,Al,Al - OH       1.7  0  0
!-11  ttschermakite    v - Mg,Mg,Mg - Ti,Ti - Ca,Ca - Si,Si,Al,Al -  O       1.5  0  0
!
      IF (SOLNAM.EQ.'ClAMg'.OR.SOLNAM.EQ.'CAMPG16') THEN
      VA=1.0D0-X(3)-X(10)
      NAA=X(3)
      KA=X(10)
      MGM1=1.0D0-X(6)-X(8)
      FEM1=X(6)+X(8)
      MGM2=X(1)+X(5)+X(8)+(X(3)+X(10))/2.0D0
      FEM2=X(6)+X(7)
      ALM2=X(2)+X(4)+(X(3)+X(10))/2.0D0
      FE3M2=X(9)
      TIM2=X(11)
      CAM4=X(1)+X(2)+X(3)+X(10)+X(11)
      MGM4=X(5)
      FEM4=X(6)+X(7)+X(8)
      NAM4=X(4)+X(9)
      SIT1=1.0D0-(X(2)+X(3)+X(10)+X(11))/2.0D0
      ALT1=(X(2)+X(3)+X(10)+X(11))/2.0D0
      OHV=1.0D0-X(11)
      OV=X(11)
!     
      A(1)=VA*MGM1**3*MGM2**2*CAM4**2*SIT1*OHV**2
      A(2)=VA*MGM1**3*ALM2**2*CAM4**2*DSQRT(SIT1*ALT1)*OHV**2*2.0D0
      A(3)=NAA*MGM1**3*MGM2*ALM2*CAM4**2*DSQRT(SIT1*ALT1)*OHV**2*8.0D0
      A(4)=VA*MGM1**3*ALM2**2*NAM4**2*SIT1*OHV**2
      A(5)=VA*MGM1**3*MGM2**2*MGM4**2*SIT1*OHV**2
      A(6)=VA*FEM1**3*FEM2**2*FEM4**2*SIT1*OHV**2
      A(7)=VA*MGM1**3*FEM2**2*FEM4**2*SIT1*OHV**2
      A(8)=VA*FEM1**3*MGM2**2*FEM4**2*SIT1*OHV**2
      A(9)=VA*MGM1**3*FE3M2**2*NAM4**2*SIT1*OHV**2
      A(10)=8.0D0*KA*MGM1**3*MGM2*ALM2*CAM4**2*DSQRT(SIT1*ALT1)*OHV**2
      A(11)=2.0D0*VA*MGM1**3*TIM2**2*CAM4**2*DSQRT(SIT1*ALT1)*OV**2
      END IF
!
!
!
!-----orthoamphibole: NCFMASHO Diener et al. 2007/2012
!                          A     M4       M13       M2        T1
!-01  anthophyllite        v - Mg,Mg - Mg,Mg,Mg - Mg,Mg - Si,Si,Si,Si       1.0  0  0
!-02  gedrite2             v - Mg,Mg - Mg,Mg,Mg - Al,Al - Si,Si,Al,Al       1.5  0  0
!-03  O-Mgpargasite       Na - Mg,Mg - Mg,Mg,Mg - Mg,Al - Si,Si,Al,Al       1.7  0  0
!-04  O-glaucophane        v - Na,Na - Mg,Mg,Mg - Al,Al - Si,Si,Si,Si       0.8  0  0
!-05  O-tremolite          v - Ca,Ca - Mg,Mg,Mg - Mg,Mg - Si,Si,Si,Si       1.0  0  0
!-06  Fe-anthophyll2       v - Fe,Fe - Fe,Fe,Fe - Fe,Fe - Si,Si,Si,Si       1.0  0  0
!-07  amo1                 v - Fe,Fe - Mg,Mg,Mg - Fe,Fe - Si,Si,Si,Si       1.0  0  0
!-08  amo2                 v - Fe,Fe - Fe,Fe,Fe - Mg,Mg - Si,Si,Si,Si       1.0  0  0
!-09  O-Mgriebeckite       v - Na,Na - Mg,Mg,Mg - F3,F3 - Si,Si,Si,Si       0.8  0  0 
!=
      IF (SOLNAM.EQ.'OAMP') THEN
      VA=1.0D0-X(3)
      NAA=X(3)

      CAM4=X(5)
      MGM4=X(1)+X(2)+X(3)
      FEM4=X(6)+X(7)+X(8)
      NAM4=X(4)+X(9)

      MGM1=1.0D0-X(6)-X(8)
      FEM1=X(6)+X(8)

      MGM2=X(1)+X(5)+X(8)+X(3)/2.0D0
      FEM2=X(6)+X(7)
      ALM2=X(2)+X(4)+X(3)/2.0D0
      FE3M2=X(9)

      SIT1=1.0D0-(X(2)+X(3))/2.0D0
      ALT1=(X(2)+X(3))/2.0D0
!-
      A(1)=VA*MGM4**2*MGM1**3*MGM2**2*SIT1
      A(2)=VA*MGM4**2*MGM1**3*ALM2**2*DSQRT(SIT1*ALT1)*2.0D0
      A(3)=NAA*MGM4**2*MGM1**3*MGM2*ALM2*DSQRT(SIT1*ALT1)*8.0D0
      A(4)=VA*NAM4**2*MGM1**3*ALM2**2*SIT1
      A(5)=VA*CAM4**2*MGM1**3*MGM2**2*SIT1
      A(6)=VA*FEM4**2*FEM1**3*FEM2**2*SIT1

      A(7)=VA*FEM4**2*MGM1**3*FEM2**2*SIT1
      A(8)=VA*FEM4**2*FEM1**3*MGM2**2*SIT1
      A(9)=VA*NAM4**2*MGM1**3*FE3M2**2*SIT1
      END IF
! 
!
!-------Nal phase; Holland et al. 2013
!       coded by Thorsten Nagel
!                       M3   M2      M1
!-1    NaAlSi-Nal       Na - Mg,Mg - Al,Al,Al,Al,Al,Si
!-2    CaMgAl-Nal       Ca - Mg,Mg - Al,Al,Al,Al,Al,Al
!-3    MgAl-Nal         Mg - Mg,Mg - Al,Al,Al,Al,Al,Al
!-4    MgSi-Nal         Mg - Mg,Mg - Mg,Mg,Mg,Si,Si,Si
!-5    FeSi-Nal         Fe - Fe,Fe - Fe,Fe,Fe,Si,Si,Si
!-6    O1-Nal           Fe - Mg,Mg - Mg,Mg,Mg,Si,Si,Si
!-7    O2-Nal           Fe - Fe,Fe - Mg,Mg,Mg,Si,Si,Si
!
      IF (SOLNAM.EQ.'NALP') THEN
      CAM3=X(2)
      MGM3=X(3)+X(4)
      FEM3=X(5)+X(6)+X(7)
      NAM3=X(1)
      MGM2=X(1)+X(2)+X(3)+X(4)+X(6)
      FEM2=X(5)+X(7)
      MGM1=(X(4)+X(6)+X(7))/2
      FEM1=X(5)/2
      ALM1=X(1)*5/6+X(2)+X(3)
      SIM1=X(1)/6+(X(4)+X(5)+X(6)+X(7))/2
!-
      A(1)=NAM3*MGM2**2*ALM1**(2.5D0)*SIM1**(0.5D0)*3.86393D0
      A(2)=CAM3*MGM2**2*ALM1**3
      A(3)=MGM3*MGM2**2*ALM1**3
      A(4)=MGM3*(MGM2**2)*(MGM1**(1.5D0))*SIM1**(1.5D0)*8.0D0
      A(5)=FEM3*FEM2**2*FEM1**(1.5D0)*SIM1**(1.5D0)*8.0D0
      A(6)=FEM3*MGM2**2*MGM1**(1.5D0)*SIM1**(1.5D0)*8.0D0
      A(7)=FEM3*FEM2**2*MGM1**(1.5D0)*SIM1**(1.5D0)*8.0D0
      END IF
!
!-------Cpx Holland et al. 2013
!       coded by Thorsten Nagel
! CPXHO13   (IDEAL,MARGULES)   M1(1):Mg,Fe,Al - M2(1):Mg,Fe,Ca,Na - T(2):Si,Al
!    Diopside            Mg - Ca - Si,Si        1.2  0  0
!    fsfud               Fe - Fe - Si,Si        1.0  0  0
!    Ca-tscher-Pyrox     Al - Ca - Si,Al        1.9  0  0
!    Jadeite             Al - Na - Si,Si        1.2  0  0
!    cenfud              Mg - Mg - Si,Si        1.0  0  0
!    fm-cpx              Mg - Fe - Si,Si        1.0  0  0
!
      IF (SOLNAM.EQ.'CPXHO13') THEN
!
      MGM1=X(1)+X(5)+X(6)     
      FEM1=X(2)
      ALM1=X(3)+X(4)
      MGM2=X(5)
      FEM2=X(2)+X(6)
      CAM2=X(1)+X(3)
      NAM2=X(4)
      SIT1=X(1)+X(2)+X(3)/2.0D0+X(4)+X(5)+X(6)
      ALT1=X(3)/2.0D0
!
      A(1)=MGM1*CAM2*SIT1**0.5D0
      A(2)=FEM1*FEM2*SIT1**0.5D0
      A(3)=ALM1*CAM2*SIT1**0.25D0*ALT1**0.25D0*DSQRT(2.0D0)
      A(4)=ALM1*NAM2*SIT1**0.5D0
      A(5)=MGM1*MGM2*SIT1**0.5D0
      A(6)=MGM1*FEM2*SIT1**0.5D0
!
      ENDIF
!
!
!-----clinopyroxene: NCFMASHO Green et al. 2016
! CPXg   (-SITE,MARGULES)1/2   M1(1):Mg,Fe,F3,Al - M2(1):Mg,Fe,Ca,Na - T1(1):Si,Al - T2(1):Si,Al
!-1   diopside       Mg - Ca - Si - Si                      1.2   0   0
!-2   cenh           Mg - Mg - Si - Si                      1.0   0   0
!-3   cfs            Fe - Fe - Si - Si                      1.0   0   0
!-4   jadeite2        Al - Na - Si - Si                      1.2   0   0
!-5   acmite2         F3 - Na - Si - Si                      1.2   0   0
!-6   ocats          Al - Ca - Si - Al                      1.9   0   0
!-7   dcats          Al - Ca - 0.5Si,0.5Al - 0.5Si,0.5Al    1.9   0   0
!-8   fmc            Mg - Fe - Si - Si                      1.0   0   0
!
      IF (SOLNAM.EQ.'CPXg') THEN
!
      MGM1=X(1)+X(2)+X(8)
      FEM1=X(3)
      F3M1=X(5)
      ALM1=X(4)+X(6)+X(7)
      MGM2=X(2)
      FEM2=X(3)+X(8)
      CAM2=X(1)+X(6)+X(7)
      NAM2=X(4)+X(5)
      SIT1=1.0D0-X(7)/2.0D0
      ALT1=X(7)/2.0D0
      SIT2=1.0D0-X(6)-X(7)/2.0D0
      ALT2=X(6)+X(7)/2.0D0
!
      A(1)=MGM1*CAM2*SIT1**0.25D0*SIT2**0.25D0
      A(2)=MGM1*MGM2*SIT1**0.25D0*SIT2**0.25D0
      A(3)=FEM1*FEM2*SIT1**0.25D0*SIT2**0.25D0
      A(4)=ALM1*NAM2*SIT1**0.25D0*SIT2**0.25D0
      A(5)=F3M1*NAM2*SIT1**0.25D0*SIT2**0.25D0
      A(6)=ALM1*CAM2*SIT1**0.25D0*ALT2**0.25D0
      A(7)=DSQRT(2.0D0)*ALM1*CAM2*SIT1**0.125D0*ALT1**0.125D0 &
      *SIT2**0.125D0*ALT2**0.125D0
      A(8)=MGM1*FEM2*SIT1**0.25D0*SIT2**0.25D0
!
      END IF
!
!
!-------Opx Holland et al. 2013
!       coded by Thorsten Nagel
! OPXHO13   (-EXT,MARGULES)   M1(1):Mg,Fe,Al - M2(1):Ca,Mg,Fe - T1(2):Si,Al
!    Enstatite        Mg - Mg - Si,Si        1.0  0  0
!    Ferrosilite      Fe - Fe - Si,Si        1.0  0  0
!    fm-opx           Mg - Fe - Si,Si        1.0  0  0
!    or-dio           Mg - Ca - Si,Si        1.2  0  0
!    Mg-tscher-Pyrox  Al - Mg - Si,Al        1.0  0  0
!
      IF (SOLNAM.EQ.'OPXHO13') THEN
!
      MGM1=X(1)+X(3)+X(4)     
      FEM1=X(2)
      ALM1=X(5)
      CAM2=X(4)
      MGM2=X(1)+X(5)
      FEM2=X(2)+X(3)
      SIT1=X(1)+X(2)+X(3)+X(4)+X(5)/2
      ALT1=X(5)/2
!
      A(1)=MGM1*MGM2*SIT1**0.5D0
      A(2)=FEM1*FEM2*SIT1**0.5D0
      A(3)=MGM1*FEM2*SIT1**0.5D0
      A(4)=MGM1*CAM2*SIT1**0.5D0
      A(5)=ALM1*MGM2*SIT1**0.25D0*ALT1**0.25D0*DSQRT(2.0D0)
!
      END IF
!
!
!-------Hpx Holland et al. 2013
!       coded by Thorsten Nagel
!  HPX      (-EXT,MARGULES)   M1(1):Mg,Fe,Al - M2(1):Ca,Mg,Fe - T1(2):Si,Al
!    Enstatite_HP     Mg - Mg - Si,Si        1.0  0  0
!    Ferrosilite_HP   Fe - Fe - Si,Si        1.0  0  0
!    fm-opx           Mg - Fe - Si,Si        1.0  0  0
!    or-dio           Mg - Ca - Si,Si        1.2  0  0
!    Mg-tscher-hpx    Al - Mg - Si,Al        1.0  0  0
!
      IF (SOLNAM.EQ.'HPX') THEN
!
      MGM1=X(1)+X(3)+X(4)
      FEM1=X(2)
      ALM1=X(5)
      CAM2=X(4)
      MGM2=X(1)+X(5)
      FEM2=X(2)+X(3)
      SIT1=X(1)+X(2)+X(3)+X(4)+X(5)/2
      ALT1=X(5)/2
!
      A(1)=MGM1*MGM2*SIT1**0.5D0
      A(2)=FEM1*FEM2*SIT1**0.5D0
      A(3)=MGM1*FEM2*SIT1**0.5D0
      A(4)=MGM1*CAM2*SIT1**0.5D0
      A(5)=ALM1*MGM2*SIT1**0.25D0*ALT1**0.25D0*DSQRT(2.0D0)
!
      END IF
!
!-------Hpx Holland et al. 2013
!       coded by Thorsten Nagel
!CA_FER    (-EXT,MARGULES)   M1(1):Ca,Mg,Fe,Na - M2(2):Mg,Fe,Al,Si
!   MgAl-ferrite          Mg - Al,Al
!   CaAl-ferrite          Ca - Al,Al
!   MgSi-ferrite          Mg - Mg,Si
!   FeSi-ferrite          Fe - Fe,Si
!   oMF-ferrite           Fe - Mg,Si
!   NaAlSi-ferrite2       Na - Al,Si
!
      IF (SOLNAM.EQ.'CA_FER') THEN
!
      CAM1=X(2)
      MGM1=X(1)+X(3)
      FEM1=X(4)+X(5)
      NAM1=X(6)
      MGM2=X(3)/2+X(5)/2
      FEM2=X(4)/2
      ALM2=X(1)+X(2)+X(6)/2
      SIM2=(X(3)+X(4)+X(5)+X(6))/2
!
      A(1)=MGM1*ALM2
      A(2)=CAM1*ALM2
      A(3)=MGM1*MGM2**0.5D0*SIM2**0.5D0*2.0D0
      A(4)=FEM1*FEM2**0.5D0*SIM2**0.5D0*2.0D0
      A(5)=FEM1*MGM2**0.5D0*SIM2**0.5D0*2.0D0
      A(6)=NAM1*ALM2**0.5D0*SIM2**0.5D0*2.0D0
!
      END IF
!
      RETURN
      END
!-----
!********************************
!-----**PSIspinels thing**
      SUBROUTINE PSISPIN(SOLNAM,P,T,N,X,A)
      INTEGER(4) EMAX
      PARAMETER (EMAX=15)
      CHARACTER(16) SOLNAM
      REAL(8) P,T,X(EMAX),A(EMAX)
      REAL(8) Y1,Y2,Y3,Y4,Z1,Z2,Z3,Z4,B11,B21,B31,B41, &
      B12,B22,B32,B42,B13,B23,B33,B43,B14,B24,B34,B44, &
      B15,B25,B35,B45,B16,B26,B36,B46, &
      LNG1,LNG2,LNG3,LNG4
      Y1=X(2)+X(4)
      Y2=1.0D0-X(2)-X(4)
      Y3=X(2)+X(4)
      Y4=1.0D0-X(2)-X(4)
      Z1=X(3)+X(4)
      Z2=X(3)+X(4)
      Z3=1.0D0-X(3)-X(4)
      Z4=1.0D0-X(3)-X(4)
      B11=-4.6416D0
      B21=-13.4379D0
      B31=-16.9689D0
      B41=21.4181D0
      B12=29.2483D0
      B22=47.0794D0
      B32=45.2720D0
      B42=-43.9963D0
      B13=-24.1520D0
      B23=-32.4267D0
      B33=-27.0821D0
      B43=22.7413D0
      B14=-35.2936D0
      B24=38.6054D0
      B34=34.8018D0
      B44=-19.1106D0
      B15=51.0881D0
      B25=-63.6511D0
      B35=-50.3732D0
      B45=36.1278D0
      B16=-20.0526D0
      B26=28.4136D0
      B36=19.7474D0
      B46=-16.9187D0
      LNG1=Y1**2*(B11+B14*Z1)+Y1**3*(B12+B15*Z1)+Y1**4*(B13+B16*Z1)
      LNG2=Y2**2*(B21+B24*Z2)+Y2**3*(B22+B25*Z2)+Y2**4*(B23+B26*Z2)
      LNG3=Y3**2*(B31+B34*Z3)+Y3**3*(B32+B35*Z3)+Y3**4*(B33+B36*Z3)
      LNG4=Y4**2*(B41+B44*Z4)+Y4**3*(B42+B45*Z4)+Y4**4*(B43+B46*Z4)
      A(1)=X(1)*DEXP(LNG1)
      A(2)=X(2)*DEXP(LNG2)
      A(3)=X(3)*DEXP(LNG3)
      A(4)=X(4)*DEXP(LNG4)
      RETURN
      END
!-----
!********************************
!-----**Vidals thing**
      SUBROUTINE MICA4O(SOLNAM,P,T,N,X,A)
      INTEGER(4) EMAX
      PARAMETER (EMAX=15)
      CHARACTER(16) SOLNAM
      REAL(8) P,T,R,X(EMAX),A(EMAX)
      REAL(8) Si,Alt,Alo,Mg,K,v, &
      gamAl,gamMgc,gamAlc,gamv,gamK,WAlMg,WKKv,WKvv, &
      gamcel,gammus,gamprl
      INTEGER(4) N
      DATA R/8.3143D0/
      WAlMg=-30500D0+15D0*T+78D-2*P
      WKKv=35000D0-25D0*T-85D-2*P
      WKvv=45000D0-10D0*T-85D-2*P
      Si=(2.0D0*X(1)+2.0D0*X(3)+X(2))/2.0D0
      Alt=(X(2))/2.0D0
      Alo=(X(1)+2.0D0*X(2)+2.0D0*X(3))/2.0D0
      Mg=(X(1))/2.0D0
      K=(X(1)+X(2))
      v=X(3)
!-----RTlngamas:-------------------
!dC   gamMgc=WAlMg*Alo*(0.5D0-Mg)
!dC   gamAlc=WAlMg*Mg*(0.5D0-Alo)
      gamMgc=WAlMg*(Alo-0.5D0)*(0.5D0-Mg)
!2    gamMgc=WAlMg*(0.5D0-Alo*Mg)
      gamAlc=0.0D0
      gamAl=WAlMg*Mg*(1.0D0-Alo)
!2    gamAl=WAlMg*Mg**2
      gamK=WKKv*(2.0D0*K*v-2.0D0*K*K*v)+WKvv*(v*v-2.0D0*K*v*v)
      gamv=WKvv*(2.0D0*K*v-2.0D0*K*v*v)+WKKv*(K*K-2.0D0*K*K*v)
      gamK=0.0D0
      gamv=0.0D0
!-----gamas-------------------------
      gamcel=DEXP((gamAlc+gamMgc+gamK)/(R*T))
      gammus=DEXP((gamAl+gamK)/(R*T))
      gamprl=DEXP((gamAl+gamv)/(R*T))
!----- celadonite
      A(1)=4.0D0*(Si)**2*(Alo)*(Mg)*(K)*gamcel
!----- musc
      A(2)=4.0D0*(Si)*(Alt)*(Alo)**2*(K)*gammus
!----- prl
      A(3)=Si**2*Alo**2*v*gamprl
!-----
      RETURN
      END
!-----
!********************************
!-----**Vidals thing**
      SUBROUTINE MICA4(SOLNAM,P,T,N,X,A)
      INTEGER(4) EMAX
      PARAMETER (EMAX=15)
      CHARACTER(16) SOLNAM
      REAL(8) P,T,R,X(EMAX),A(EMAX)
      REAL(8) Si,Alt,Alo,Mg,K,Fe,Na,v
      REAL(8) gamAl,gamMgc,gamFec,gamAlc,gamv,gamK,gamNa,WAlMg, &
      WAlFe,WKKv,WKvv,WKKNa,WKNaNa,WNaNav,WNavv,WKNav,gammcel, &
      gamfcel,gammus,gamprl,gampg
      INTEGER(4) N
      DATA R/8.3143D0/
      WAlMg=-30500D0-15D0*T+78D-2*P
      WAlFe=-5500D0-15D0*T+65D-2*P
      WKKNa=12230D0+5D0*T+67D-2*P
      WKNaNa=19456D0+165D0-2D0*T-46D-2*P
      WKKv=35000D0-25D0*T-85D-2*P
      WKvv=45000D0-10D0*T-85D-2*P
      WNaNav=40000D0-5D0*T-0D-2*P
      WNavv=40000D0-5D0*T-0D-2*P
      WKNav=(WKKNa+WKNaNa+WKKv+WKvv+WNaNav+WNavv)/2.0D0
      Si=(2.0D0*X(1)+2.0D0*X(3)+2.0D0*X(4)+X(2)+X(5))/2.0D0
      Alt=(X(2)+X(5))/2.0D0
      Alo=(X(1)+2.0D0*X(2)+2.0D0*X(5)+X(4)+2.0D0*X(3))/2.0D0
      Mg=(X(1))/2.0D0
      Fe=(X(4))/2.0D0
      K=(X(1)+X(2)+X(4))
      Na=X(5)
      v=X(3)
!-----RTlngamas:-------------------
!  new Olivier 10/2010
      gamMgc=WAlMg*Alo*(0.5D0-Mg)+WAlFe*Alo*(0.5D0-Fe)-0.5D0*WAlFe*Alo
      gamFec=WAlMg*Mg*(0.5D0-Alo)+WAlFe*Alo*(0.5D0-Fe)-0.5D0*WAlMg*Mg
      gamAlc=WAlMg*Mg*(0.5D0-Alo)+WAlFe*Fe*(0.5D0-Alo)
      gamAl=WAlMg*Mg*(1-Alo)+WAlFe*Alo*(1-Alo)
      gamK=WKKNa*(2.0D0*K*Na-2.0D0*K*K*Na)+ &
      WKNaNa*(Na*Na-2.0D0*K*Na*Na)+WKKv*(2.0D0*K*v-2.0D0*K*K*v)+ &
      WKvv*(v*v-2.0D0*K*v*v)-WNaNav*(2.0D0*Na*Na*v)- &
      WNavv*(2.0D0*Na*v*v)+WKNav*(Na*v-2.0D0*Na*K*v)
      gamv=WKvv*(2.0D0*K*v-2.0D0*K*v*v)+WKKv*(K*K-2.0D0*K*K*v)+ &
      WNavv*(2.0D0*Na*v-2.0D0*Na*v*v)+WNaNav*(Na*Na-2.0D0*Na*Na*v)- &
      WKKNa*(2.0D0*K*K*Na)-WKNaNa*(2.0D0*K*Na*Na)+ &
      WKNav*(K*Na-2.0D0*Na*K*v)
      gamNa=WNaNav*(2.0D0*v*Na-2.0D0*Na*Na*v)+ &
      WNavv*(v*v-2.0D0*v*v*Na)+WKNaNa*(2.0D0*K*Na-2.0D0*K*Na*Na)+ &
      WKKNa*(K*K-2.0D0*Na*K*K)-WKvv*(2.0D0*K*v*v)- &
      WKKv*(2.0D0*v*K*K)+WKNav*(K*v-2.0D0*Na*K*v)

!-----gamas-------------------------
      gammcel=DEXP((gamAlc+gamMgc+gamK)/(R*T))
      gamfcel=DEXP((gamAlc+gamFec+gamK)/(R*T))
      gammus=DEXP((gamAl+gamK)/(R*T))
      gampg=DEXP((gamAl+gamNa)/(R*T))
      gamprl=DEXP((gamAl+gamv)/(R*T))
!----- Mg-celadonite
      A(1)=4.0D0*(Si)**2*(Alo)*(Mg)*(K)*gammcel
!----- Fe-celadonite
      A(4)=4.0D0*(Si)**2*(Alo)*(Fe)*(K)*gamfcel
!----- musc
      A(2)=4.0D0*(Si)*(Alt)*(Alo)**2*(K)*gammus
!----- parag
      A(5)=4.0D0*(Si)*(Alt)*(Alo)**2*(Na)*gampg
!----- prl
      A(3)=Si**2*Alo**2*v*gamprl
!-----
      RETURN
      END
!-----
!********************************
      SUBROUTINE LANDAU1(SOLNAM,P,T,N,X,A)
      INTEGER(4) EMAX
      PARAMETER (EMAX=15)
      CHARACTER(16) SOLNAM
      REAL(8) P,T,X(EMAX),A(EMAX),XA,XB,XC,TC,G,DG,R, &
      MU1,MU2,GA1,GA2,FF
      INTEGER(4) N
      TC=1138.0D0
      R=3.143D0
      A(1)=X(1)*1.01D0
      A(2)=X(2)*1.01D0
      IF (T.GT.TC) RETURN
      XC=DSQRT(TC*(TC-T))/(2.0D0*TC)
      XA=0.5D0-XC
      XB=0.5D0+XC
      IF (X(1).LT.XA.OR.X(1).GT.XB) RETURN
      FF=12.0D0*T
!     FF=100.0D0
      G=FF*(X(1)-XA)**2*(X(1)-XB)**2
      DG=FF*4.0D0*(-0.125D0+0.5D0*XC**2+0.75D0*X(1) &
      -XC**2*X(1)-1.5D0*X(1)**2+X(1)**3)
      MU1=G+(1.0D0-X(1))*DG
      MU2=G-X(1)*DG
      GA1=DEXP(-MU1/(R*T))
      GA2=DEXP(-MU2/(R*T))
      A(1)=X(1)*GA1
      A(2)=X(2)*GA2
      RETURN
      END
!-----
!********************************
      SUBROUTINE MGSI(SOLNAM,P,T,N,X,A)
      INTEGER(4) EMAX
      PARAMETER (EMAX=15)
      CHARACTER(16) SOLNAM
      REAL(8) P,T,X(EMAX),A(EMAX),B(2),W(0:10),E(0:10),ZZ
      INTEGER(4) N
      DATA B/0.6887D0,1.3774D0/
      DATA ZZ/2.0D0/
      DATA W/-86571.0D0,-57789.0D0,0.0D0,0.0D0,0.0D0,0.0D0, &
      0.0D0,272353.0D0,0.0D0,0.0D0,0.0D0/
      DATA E/0.0D0,-43.489D0,0.0D0,0.0D0,0.0D0,0.0D0, &
      0.0D0,99.583D0,0.0D0,0.0D0,0.0D0/
      CALL QUASI(SOLNAM,P,T,N,X,A,B,ZZ,W,E)
      RETURN
      END
!-----
!********************************
      SUBROUTINE MNTI(SOLNAM,P,T,N,X,A)
      INTEGER(4) EMAX
      PARAMETER (EMAX=15)
      CHARACTER(16) SOLNAM
      REAL(8) P,T,X(EMAX),A(EMAX),B(2),W(0:10),E(0:10),ZZ
      INTEGER(4) N
      DATA B/0.6887D0,1.3774D0/
      DATA ZZ/2.0D0/
      DATA W/-33874.0D0,0.0D0,-24987.0D0,0.0D0,0.0D0,0.0D0, &
      0.0D0,0.0D0,0.0D0,0.0D0,0.0D0/
      DATA E/0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0, &
      0.0D0,0.0D0,0.0D0,0.0D0,0.0D0/
      CALL QUASI(SOLNAM,P,T,N,X,A,B,ZZ,W,E)
      RETURN
      END
!-----
!********************************
      SUBROUTINE MGTI(SOLNAM,P,T,N,X,A)
      INTEGER(4) EMAX
      PARAMETER (EMAX=15)
      CHARACTER(16) SOLNAM
      REAL(8) P,T,X(EMAX),A(EMAX),B(2),W(0:10),E(0:10),ZZ
      INTEGER(4) N
      DATA B/0.6887D0,1.3774D0/
      DATA ZZ/2.0D0/
      DATA W/-51764.0D0,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0, &
      0.0D0,0.0D0,0.0D0,0.0D0,0.0D0/
      DATA E/0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0, &
      0.0D0,0.0D0,0.0D0,0.0D0,0.0D0/
      CALL QUASI(SOLNAM,P,T,N,X,A,B,ZZ,W,E)
      RETURN
      END
!-----
!********************************
      SUBROUTINE FETI(SOLNAM,P,T,N,X,A)
      INTEGER(4) EMAX
      PARAMETER (EMAX=15)
      CHARACTER(16) SOLNAM
      REAL(8) P,T,X(EMAX),A(EMAX),B(2),W(0:10),E(0:10),ZZ
      INTEGER(4) N
      DATA B/0.6887D0,1.3774D0/
      DATA ZZ/2.0D0/
      DATA W/-12405.0D0,0.0D0,-10227.0D0,0.0D0,0.0D0,0.0D0, &
      0.0D0,0.0D0,0.0D0,0.0D0,0.0D0/
      DATA E/0.0D0,0.0D0,0.0D0,0.0D0,0.0D0,0.0D0, &
      0.0D0,0.0D0,0.0D0,0.0D0,0.0D0/
      CALL QUASI(SOLNAM,P,T,N,X,A,B,ZZ,W,E)
      RETURN
      END
!-----
!********************************
      SUBROUTINE QUASI(SOLNAM,P,T,N,X,A,B,ZZ,W,E)
      INTEGER(4) EMAX
      PARAMETER (EMAX=15)
      CHARACTER(16) SOLNAM
      REAL(8) P,T,X(EMAX),A(EMAX),B(2),ZZ,W(0:10),E(0:10),Z2,R,RT, &
      X1,X2,Y1,Y2,X11,X22,X122,WET,DWEDY,XI,F1,F2,F3,YY
      INTEGER(4) I,N
      DATA R/8.3143D0/
      RT=R*T
      Z2=ZZ/2.0D0
      X1=X(1)
      X2=X(2)
      F1=B(1)*X1+B(2)*X2
      Y1=B(1)*X1/F1
      Y2=B(2)*X2/F1
      YY=1.0D0
      WET=W(0)-T*E(0)
      DWEDY=0.0D0
      DO 500,I=1,10
      F1=DBLE(I)
      F2=W(I)-T*E(I)
      DWEDY=DWEDY+F1*YY*F2
      IF (YY.LT.1D-20.OR.Y2.LT.1D-20) GOTO 600
      YY=YY*Y2
      WET=WET+YY*F2
  500 CONTINUE
  600 CONTINUE
      F1=DEXP((WET)/(Z2*RT))-1.0D0
      XI=DSQRT(1.0D0+4.0D0*Y1*Y2*F1)
      X122=2.0D0*Y1*Y2/(1.0D0+XI)
      X11=Y1-X122
      X22=Y2-X122
      IF (X11.GT.0.0D0.AND.X1.GT.0.0D0) THEN
      F1=RT*DLOG(X1)
      F2=B(1)*Z2*RT*DLOG(X11/Y1**2)
      F3=B(1)*X122*Y2*DWEDY
      A(1)=DEXP((F1+F2-F3)/RT)
      ELSE
      A(1)=0.0D0
      END IF
      IF (X22.GT.0.0D0.AND.X2.GT.0.0D0) THEN
      F1=RT*DLOG(X2)
      F2=B(2)*Z2*RT*DLOG(X22/Y2**2)
      F3=B(2)*X122*Y1*DWEDY
      A(2)=DEXP((F1+F2+F3)/RT)
      ELSE
      A(2)=0.0D0
      END IF
      RETURN
      END
!-----
!-----*************** end of solution definitions ****************
!-----
!********************************
      SUBROUTINE GSPEC(NAME,P,PGAS,T,FALL,G,V)
      CHARACTER(16) NAME,FALL
      REAL(8) P,PGAS,T,G,V,F1
      INTEGER(4) IH2O,ICO2
      DATA IH2O/1/
      DATA ICO2/2/
      G=0.0D0
      V=0.0D0
      IF (FALL.EQ.'HAAR') CALL WHAAR2(NAME,PGAS,T,G,V)
      IF (FALL.EQ.'ABSAL') CALL ALBSAL(P,T,G,V)
      IF (FALL.EQ.'CK&J') CALL KUNDJ(ICO2,PGAS,T,G,V)
      IF (FALL.EQ.'WK&J') CALL KUNDJ(IH2O,PGAS,T,G,V)
      IF (FALL.EQ.'HHP98') CALL HHP91(PGAS,T,G,V)
      IF (FALL.EQ.'CHP98') CALL CHP91(PGAS,T,G,V)
      IF (FALL.EQ.'PS94H2O') CALL PS94H2O(PGAS,T,G,V)
      IF (FALL.EQ.'PS94CO2') CALL PS94CO2(PGAS,T,G,V)
      IF (FALL.EQ.'CORDI') CALL CORD(P,T,G,V)
      IF (FALL.EQ.'DU92H2O') CALL DUAN92H2O(T,PGAS,G,V,F1)
      IF (FALL.EQ.'DU92CO2') CALL DUAN92CO2(T,PGAS,G,V,F1)
      IF (FALL.EQ.'DU06H2O') THEN
        IF (P.LE.2000.0D0) THEN
          CALL DUAN06H2OLO(T,PGAS,G,V,F1)
        ELSE
          CALL DUAN06H2OHI(T,PGAS,G,V,F1)
        END IF
      END IF
      IF (FALL.EQ.'DU06CO2') THEN
        IF (P.LE.2000.0D0) THEN
          CALL DUAN06CO2LO(T,PGAS,G,V,F1)
        ELSE
          CALL DUAN06CO2HI(T,PGAS,G,V,F1)
        END IF
      END IF
      RETURN
      END
!-----
!********************************
      SUBROUTINE CORD(P,TK,G,V)
      REAL(8) H,S,V,A,B,C,D,E,F,X1,X2,X3,X4,X5,X6,X7,P,TK,TR,G,Z
      TR=298.15D0
        H = -563493.8
        S = 156.665
        V = 23.06
        A = 270.7386
        B = -.009046
        C = -8152144.
        D = -9633.768
        E = 0.0
        F = 133752.
!
        X1 = -A * TK * (DLOG(TK) - DLOG(TR))
        X2 = A * (TK - TR)
        X3 = (B/2.0D00*(TK**2 - (TR**2))) - (B*TK*(TK - TR))
        X4 = (C/2.0D00*((1/TK) - (TK/TR**2))) - (C*(1/TK - 1/TR))
        X5 = 2.0D00 * D * ((TK**0.5) - (TR**0.5)) + 2.0D00 * D * TK * ( &
        TK**(-0.5) - TR**(-0.5))
        X6 = E * (TK**3 - TR**3) / 3.0D00-E * TK * (TK**2 - TR**2) / &
        2.0D00
        X7 = F * DLOG(TK/TR) + F * (1.0D00-TK/TR)
        Z = X1 + X2 + X3 + X4 + X5 + X6 + X7
        G = (H - (TK*S) + Z) / 4.184
        G = G + ((P - 1.0)*V*0.02390064D00)
        G=G*4.184D0
        RETURN
        END
!-----
!********************************
      SUBROUTINE HHP91(PB,T,G,VOL)
!     Holland and Powell (1991,1998)
      IMPLICIT NONE
!hp91 REAL(8) C,C0,C1,D,D0,D1
      REAL(8) P0,DELP,R,RT,AVIR,BVIR,CVIR,TK,TA
!     COMMON /HOPREAL/ P0,DELP,R,RT,AVIR,BVIR,CVIR,TK,TA
!=====
      REAL(8) B,A,A0,A1,A2,A3,A4,A5,A6,A7,A8,A9,AGAS,GT,SQT, &
      PB,P,T,G,VOL,PSA,VMRK,CUB,CUC,CUD,X1,X2,X2I,X3, &
      V1,V2,AF,RTLNP,DGGAS1,DGGAS2,DGGAS3,DGGAS,VREF,PREF
!     REAL(8) GA1,GA2,GA3,GF
!-----
      DATA AVIR,BVIR,CVIR/1.9853D-3,-8.9090D-2,8.0331D-2/
      DATA R,P0,TK,TA/8.3142D-3,2.0D0,695.0D0,673.0D0/
!hp91 DATA C0,C1/-3.02565D-2,-5.343144D-6/
!hp91 DATA D0,D1/-3.2297554D-3,2.2215221D-6/
      DATA A0,B/1113.4D0,1.465D0/
      DATA A1,A2,A3/-0.88517D0,4.5300D-3,-1.3183D-5/
      DATA A4,A5,A6/-0.22291D0,-3.8022D-4,1.7791D-7/
      DATA A7,A8,A9/5.8487D0,-2.1370D-2,6.8133D-5/
!-----
      G=0.0D0
      DGGAS=0.0D0
      DGGAS1=0.0D0
      DGGAS2=0.0D0
      DGGAS3=0.0D0
!     GA1=0.0D0
!     GA2=0.0D0
!     GA3=0.0D0
      V1=0.0D0
      V2=0.0D0
      RT=R*T
      SQT=DSQRT(T)
      P=PB/1000.0D0
      DELP=P-P0
      PREF=0.001D0
      PSA=-13.627D-3+7.29395D-7*T**2-2.34622D-9*T**3+4.83607D-15*T**5
      IF (T.LT.TA) THEN
      A=A0+A1*(TA-T)+A2*(TA-T)**2+A3*(TA-T)**3
      AGAS=A0+A7*(TA-T)+A8*(TA-T)**2+A9*(TA-T)**3
      ELSE
      A=A0+A4*(T-TA)+A5*(T-TA)**2+A6*(T-TA)**3
      AGAS=0.0D0
      END IF
!hp91 C=C0+C1*T
!hp91 D=D0+D1*T
!=====reference volume at 1 Bar and T
!     IF (T.LT.TA) THEN
!     AF=AGAS
!     ELSE
!     AF=A
!     END IF
!     AF=0.0D0
!     CUB=-RT/PREF
!     CUC=-(B*RT+B*B*PREF-AF/SQT)/PREF
!     CUD=-AF*B/SQT/PREF
!     CALL CUBIC(CUB,CUC,CUD,X1,X2,X2I,X3)
!     IF (X2I.NE.0.0D0) THEN
!     VREF=X1
!     ELSE
!     IF (P.LT.PSA.AND.T.LT.TK)  THEN
!     VREF=DMAX1(X1,X2,X3)
!     ELSE
!     VREF=DMIN1(X1,X2,X3)
!     IF (VREF.LT.B) VREF=DMAX1(X1,X2,X3)
!     END IF
!     END IF
! offensichtlich einfacher:
      VREF=RT/PREF
!=====volume at P and T
      IF (T.LT.TA.AND.P.LT.PSA) THEN
      AF=AGAS
      ELSE
      AF=A
      END IF
      CUB=-RT/P
      CUC=-(B*RT+B*B*P-AF/SQT)/P
      CUD=-AF*B/SQT/P
      CALL CUBIC(CUB,CUC,CUD,X1,X2,X2I,X3)
      IF (X2I.NE.0.0D0) THEN
      VMRK=X1
      ELSE
      IF (P.LT.PSA.AND.T.LT.TK)  THEN
      VMRK=DMAX1(X1,X2,X3)
      ELSE
      VMRK=DMIN1(X1,X2,X3)
      IF (VMRK.LT.B) VMRK=DMAX1(X1,X2,X3)
      END IF
      END IF
      IF (P.GT.P0) THEN
!hp91 VOL=VMRK+C*DSQRT(P-P0)+D*(P-P0)
      VOL=VMRK+AVIR*(P-P0)+BVIR*DSQRT(P-P0)+CVIR*(P-P0)**0.25D0
      ELSE
      VOL=VMRK
      END IF
!-----
!     CALL GAGA(AF,B,P,VMRK,T,GA3)
!-----
      DGGAS3=VMRK*P-VREF*PREF-RT*DLOG((VMRK-B)/(VREF-B)) &
      +(AF/(SQT*B))*DLOG(VMRK*(VREF+B)/(VREF*(VMRK+B)))
      IF (P.GT.P0) THEN
      DGGAS3=DGGAS3+AVIR/2.0D0*(P-P0)**2 &
      +2.0D0/3.0D0*BVIR*(P-P0)**1.5D0 &
      +0.8D0*CVIR*(P-P0)**1.25D0
      END IF
!=====volume of gas at T and PSAT
      IF (T.LT.TK.AND.P.GT.PSA) THEN
      IF (T.GT.TA) THEN
      AF=A
      ELSE
      AF=AGAS
      END IF
      CUB=-RT/PSA
      CUC=-(B*RT+B*B*PSA-AF/SQT)/PSA
      CUD=-AF*B/SQT/PSA
      CALL CUBIC(CUB,CUC,CUD,X1,X2,X2I,X3)
      IF (X2I.NE.0.0D0) THEN
      V1=X1
      ELSE
      V1=DMAX1(X1,X2,X3)
      END IF
!-----
!     CALL GAGA(AF,B,PSA,V1,T,GA1)
!-----
      DGGAS1=V1*PSA-VREF*PREF-RT*DLOG((V1-B)/(VREF-B)) &
      +(AF/(SQT*B))*DLOG(V1*(VREF+B)/(VREF*(V1+B)))
!=====volume of liquid at a T and PSAT
      CUB=-RT/PSA
      CUC=-(B*RT+B*B*PSA-A/SQT)/PSA
      CUD=-A*B/SQT/PSA
      CALL CUBIC(CUB,CUC,CUD,X1,X2,X2I,X3)
      IF (X2I.NE.0.0D0) THEN
      V2=X1
      ELSE
      V2=DMIN1(X1,X2,X3)
      END IF
!-----
!     CALL GAGA(A,B,PSA,V2,T,GA2)
!-----
      DGGAS2=VMRK*P-V2*PSA-RT*DLOG((VMRK-B)/(V2-B)) &
      +(A/(SQT*B))*DLOG(VMRK*(V2+B)/(V2*(VMRK+B)))
      IF (P.GT.P0) THEN
      DGGAS2=DGGAS2+AVIR/2.0D0*(P-P0)**2 &
      +2.0D0/3.0D0*BVIR*(P-P0)**1.5D0 &
      +0.8D0*CVIR*(P-P0)**1.25D0
      END IF
!     GF=RT*(GA1-GA2+GA3)
      DGGAS=1.0D3*(DGGAS2+DGGAS1)
!=====
      ELSE
!     GF=RT*GA3
      DGGAS=1.0D3*DGGAS3
      END IF
!=====
      CALL HHP91GT(PB,T,GT)
      RTLNP=1D3*RT*DLOG(P*1D3)
!     GF=1D3*GF
!     G=GT+RTLNP+GF
      G=GT+DGGAS
!=====
      RETURN
      END
!C-----
!C********************************
!      SUBROUTINE GAGA(A,B,P,V,T,GA)
!C     Holland and Powell (1991)
!      IMPLICIT NONE
!      REAL(8) P0,DELP,R,RT,AVIR,BVIR,CVIR,TK,TA
!      COMMON /HOPREAL/ P0,DELP,R,RT,AVIR,BVIR,CVIR,TK,TA
!      REAL(8) A,B,P,V,T,GA,AA,BB,Z
!      Z=(P*V)/(RT)
!      AA=A/(B*R*(T**1.5D0))
!      BB=(B*P)/(RT)
!      GA=Z-1.0D0-DLOG(Z-BB)-AA*DLOG(1.0D0+(BB/Z))
!      IF (P.GT.P0) THEN
!Chp91 GA=GA+(2.0/3.0*C*(P-P0)**1.5D0+D/2.0D0*(P-P0)**2)/(RT)
!      GA=GA+(AVIR/2.0D0*(P-P0)**2+2.0D0/3.0D0*BVIR*(P-P0)**1.5D0
!     >+0.8D0*CVIR*(P-P0)**1.25D0)/(RT)
!      END IF
!      RETURN
!      END
!-----
!********************************
      SUBROUTINE HHP91GT(P,T,GT)
!     Holland and Powell (1991,1998)
      IMPLICIT NONE
      REAL(8) P,PK,T,T0,GT,K1,K2,K3,K4,CPRDT,CPRTDT, &
      H0,S0,TT,SQT,TT0,SQT0
      DATA H0,S0/-241.81D0,188.80D-3/
      DATA K1,K2,K3,K4/0.0401D0,0.8656D-5,487.5D0,-0.2512D0/
      DATA T0,TT0/298.15D0,88893.4225D0/
      SQT0=DSQRT(T0)
      TT=T*T
      SQT=DSQRT(T)
      PK=P/1.0D3
      CPRDT=K1*(T-T0)+K2*(TT-TT0)/2.0D0 &
      -K3*(1.0D0/T-1.0D0/T0)+2D0*K4*(SQT-SQT0)
      CPRTDT=K1*DLOG(T/T0)+K2*(T-T0) &
      -K3*(1.0D0/(TT)-1.0D0/(TT0))/2.0D0 &
      -2D0*K4*(1.0D0/SQT-1.0D0/SQT0)
      GT=1.0D3*(H0+CPRDT-T*(S0+CPRTDT))
      RETURN
      END
!-----
!********************************
      SUBROUTINE CHP91(PB,T,G,VOL)
!     Holland and Powell (1991,1998)
      IMPLICIT NONE
      REAL(8) P0,R,RT,AVIR,AVIR0,AVIRT,BVIR,BVIR0,BVIRT,CVIR
!=====
      REAL(8) B,A,A0,A1,A2,A3,GT,SQT,PB,P,T,G,VOL,VMRK, &
      CUB,CUC,CUD,X1,X2,X2I,X3,DGGAS3,DGGAS,VREF,PREF
!     REAL(8) C,C0,C1,D,D0,D1,RTLNP
!-----
      DATA AVIR0,AVIRT/5.40776D-3,-1.59046D-6/
      DATA BVIR0,BVIRT,CVIR/-1.78198D-1,2.45317D-5,0.0D0/
      DATA R,P0/8.3142D-3,5.0D0/
      DATA A0,B/741.2D0,3.057D0/
      DATA A1,A2,A3/-0.10891D0,-3.4203D-4,0.0D0/
!hp91 DATA C0,C1/-2.26924D-1,7.73793D-5/
!hp91 DATA D0,D1/1.33790D-2,-1.01740D-5/
!-----
      G=0.0D0
      DGGAS=0.0D0
      DGGAS3=0.0D0
      RT=R*T
      SQT=DSQRT(T)
      P=PB/1000.0D0
      PREF=0.001D0
      AVIR=AVIR0+AVIRT*T
      BVIR=BVIR0+BVIRT*T
!hp91 AVIR=D0+D1*T
!hp91 BVIR=C0+C1*T
      A=A0+A1*(T)+A2*(T)**2+A3*(T)**3
!=====reference volume at 1 Bar and T
!     CUB=-RT/PREF
!     CUC=-(B*RT+B*B*PREF-A/SQT)/PREF
!     CUD=-A*B/SQT/PREF
!     CALL CUBIC(CUB,CUC,CUD,X1,X2,X2I,X3)
!     IF (X2I.NE.0.0D0) THEN
!     VREF=X1
!     ELSE
!     VREF=DMIN1(X1,X2,X3)
!     IF (VREF.LT.B) VREF=DMAX1(X1,X2,X3)
!     END IF
! offensichtlich einfacher:
      VREF=RT/PREF
!=====volume at P and T
      CUB=-RT/P
      CUC=-(B*RT+B*B*P-A/SQT)/P
      CUD=-A*B/SQT/P
      CALL CUBIC(CUB,CUC,CUD,X1,X2,X2I,X3)
      IF (X2I.NE.0.0D0) THEN
      VMRK=X1
      ELSE
      VMRK=DMIN1(X1,X2,X3)
      IF (VMRK.LT.B) VMRK=DMAX1(X1,X2,X3)
      END IF
      IF (P.GT.P0) THEN
      VOL=VMRK+AVIR*(P-P0)+BVIR*DSQRT(P-P0)+CVIR*(P-P0)**0.25D0
      ELSE
      VOL=VMRK
      END IF
!=====
      DGGAS3=VMRK*P-VREF*PREF-RT*DLOG((VMRK-B)/(VREF-B)) &
      +(A/(SQT*B))*DLOG(VMRK*(VREF+B)/(VREF*(VMRK+B)))
      IF (P.GT.P0) THEN
      DGGAS3=DGGAS3+AVIR/2.0D0*(P-P0)**2 &
      +2.0D0/3.0D0*BVIR*(P-P0)**1.5D0 &
      +0.8D0*CVIR*(P-P0)**1.25D0
      END IF
      DGGAS=1.0D3*DGGAS3
!=====
      CALL CHP91GT(PB,T,GT)
!     RTLNP=1D3*RT*DLOG(P*1D3)
      G=GT+DGGAS
!=====
      RETURN
      END
!-----
!********************************
      SUBROUTINE CHP91GT(P,T,GT)
!     Holland and Powell (1991)
      IMPLICIT NONE
      REAL(8) P,PK,T,T0,GT,K1,K2,K3,K4,CPRDT,CPRTDT, &
      H0,S0,TT,SQT,TT0,SQT0
      DATA H0,S0/-393.51D0,213.70D-3/
      DATA K1,K2,K3,K4/0.0878D0,-0.2644D-5,706.4D0,-0.9989D0/
      DATA T0,TT0/298.15D0,88893.4225D0/
      SQT0=DSQRT(T0)
      TT=T*T
      SQT=DSQRT(T)
      PK=P/1.0D3
      CPRDT=K1*(T-T0)+K2*(TT-TT0)/2.0D0 &
      -K3*(1.0D0/T-1.0D0/T0)+2D0*K4*(SQT-SQT0)
      CPRTDT=K1*DLOG(T/T0)+K2*(T-T0) &
      -K3*(1.0D0/(TT)-1.0D0/(TT0))/2.0D0 &
      -2D0*K4*(1.0D0/SQT-1.0D0/SQT0)
      GT=1.0D3*(H0+CPRDT-T*(S0+CPRTDT))
      RETURN
      END
!-----
!********************************
      SUBROUTINE HOPOH2O(P,T,GF)
!     Holland and Powell (1990)
      IMPLICIT NONE
      REAL(8) A,B,C,A1,A2,A3,A4,A5,B1,B2,B3,B4,B5,B6, &
      C1,C2,C3,C4,C5,C6,P,PK,T,T0,RTLNF,GF,P2,P3,SQP, &
      K1,K2,K3,K4,CPRDT,CPRTDT,H0,S0,TT,SQT,TT0,SQT0
!
      DATA H0,S0/-241.81D0,188.80D-3/
      DATA K1,K2,K3,K4/0.0401D0,0.8656D-5,487.5D0,-0.2512D0/
      DATA T0,TT0/298.15D0,88893.4225D0/
!
      DATA A1,A2,A3/-40.338D0,1.6474D0,-0.0062115D0/
      DATA A4,A5/2.0068D0,0.0562929D0/
      DATA B1,B2,B3/0.117372D0,0.0D0,0.0D0/
      DATA B4,B5,B6/-0.00046710D0,0.0D0,0.0D0/
      DATA C1,C2,C3/-7.3681D-6,1.10295D-7,-9.8774D-7/
      DATA C4,C5,C6/-2.4819D-5,8.2948D-6,8.33667D-8/
      SQT0=DSQRT(T0)
      TT=T*T
      SQT=DSQRT(T)
      PK=P/1.0D3
      P2=PK*PK
      P3=P2*PK
      SQP=DSQRT(PK)
!
      CPRDT=K1*(T-T0)+K2*(TT-TT0)/2.0D0 &
      -K3*(1.0D0/T-1.0D0/T0)+2D0*K4*(SQT-SQT0)
      CPRTDT=K1*DLOG(T/T0)+K2*(T-T0) &
      -K3*(1.0D0/(TT)-1.0D0/(TT0))/2.0D0 &
      -2D0*K4*(1.0D0/SQT-1.0D0/SQT0)
!
      A=A1+A2*PK+A3*P2+A4/PK+A5/P2
      B=B1+B2*PK+B3/PK+B4/P2+B5/SQP+B6/P3
      C=C1+C2*PK+C3/P2+C4/SQP+C5/PK+C6/P3
      RTLNF=A+B*T+C*TT
      GF=1.0D3*(H0+CPRDT-T*(S0+CPRTDT)+RTLNF)
      RETURN
      END
!-----
!********************************
      SUBROUTINE WHAAR2(NAME,P,T,GH2O,VH2O)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      CHARACTER(16) NAME
      DIMENSION TAUI(0:6),ERMI(0:9),GI(40),KI(40),LI(40),CI(18), &
      RHOI(37:40),TTTI(37:40),ALPI(37:40),BETI(37:40)
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
      RT=R*T
!     GREF=-729.0855D0*18.0152D0*4.184D0
!---  GREF CALCULATED WITH THIS ROUTINE AT 1 BAR AND 25 DEG. C
      GREF=-54955.2356146119409D0
      T0=647.073D0
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
      NAME='STEAM'
      PS=220.55D0
      IF (T.LE.647.25D0) THEN
      CALL PSAT2(T,PS)
      IF (P.GE.PS) NAME='STEAM'
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
      CALL CUBIC(BUK,CUK,DUK,X1,X2,X2I,X3)
      IF (X2I.NE.0.0D0) THEN
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
      IF (DP.LT.1D-5.AND.DR.LT.1D-5) GOTO 30
   20 CONTINUE
!---
   30 RH=RHN
      Y=RH*B/4.D0
      X=1D0-Y
      ER=DEXP(-RH)
      ERMI(0)=1D0
      ERMI(1)=1D0-ER
      DO I=2,9
        ERMI(I)=ERMI(I-1)*ERMI(1)
      END DO
!
!-----CALCULATE BASE FUNCTION
      AA=RT*(-DLOG(X)-43.33333333333333D0/X+28.16666666666667D0/X/X &
      +4D0*Y*(BB/B-3.5D0)+15.16666666666667D0+DLOG(RH*RT/1.01325D0))
!
!-----CALCULATE RESIDUAL FUNCTION
      DO I=1,36
        AA=AA+GI(I)/KI(I)/TAUI(LI(I))*ERMI(KI(I))
      END DO
      DO I=37,40
        DEL=RH/RHOI(I)-1.0D0
        TAU=T/TTTI(I)-1.0D0
        QHEX=(-ALPI(I)*DEL**KI(I)-BETI(I)*TAU*TAU)
        IF (QHEX.GT.-150.0D0) THEN
        AA=AA+GI(I)*DEL**LI(I)*DEXP(QHEX)
        END IF
      END DO
!
!-----CALCULATE IDEAL GAS FUNCTION
      TR=T/1.0D2
      W=TR**(-3)
      AID=1.D0+(CI(1)/TR+CI(2))*DLOG(TR)
      DO I=3,18
        AID=AID+CI(I)*W
        W=W*TR
      END DO
      AA=AA-RT*AID
!
!-----CALCULATE G = AA + P/RH  AND  V = 1/RH
!-----G CORRECTED TO DH-T*S FOR WATER AT 1 BAR AND 25 DEG. C
!-----ACCORDING TO ROBIE ET AL. 1978
      GH2O=((AA+P/RH)*1.80152D0-GREF-306685.5925D0)
      VH2O=(1.0D0/RH)*1.80152D0
      RETURN
      END
!-----
!********************************
      SUBROUTINE PSAT2(T,PS)
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
!********************************
      SUBROUTINE CUBIC(B,C,D,X1,X2,X2I,X3)
      REAL(8) B,C,D,Q,P,R,PI,PHI3,FF,X1,X2,X2I,X3
      PI=3.14159263538979D0
      X2=0.0D0
      X2I=0.0D0
      X3=0.0D0
      IF (C.EQ.0.0D0.AND.D.EQ.0.0D0) THEN
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
!********************************
      SUBROUTINE ALBSAL(P,T,GR,VOLUM)
      IMPLICIT NONE
      REAL(8) T,T0,TT,TT0,SQT,SQT0, &
      P,P0,VPA,VPB,VTA,VTB,V0R, &
      K1,K4,K3,K8,CPRDT,CPRTDT,H0R,S0R,GR, &
      FGR,FHR,FSR,FCPR,FVR,VOLUM
!
      DATA P0,T0,TT0/1.0D0,298.15D0,88893.4225D0/
      DATA K1,K4,K3,K8/ &
      393.63574D0,-2415.498D0,-7892826.0D0,1070636032.0D0/
      DATA H0R,S0R,V0R/-3921618.2D0,224.412D0,10.083D0/
      DATA VTA,VTB,VPA,VPB/ &
      2.652555298656D-4,3.26759781D-8,-1.96083415356D-5,4.90144713D-11/
!-----
!     DATA VTAI,VTBI,VPAI,VPBI/
!    >2.63072032D0,0.00032407D0,-0.19446932D0,0.00048611D0/
!     VTA=VTAI/100000.0D0*V0R
!     VTB=VTBI/100000.0D0*V0R
!     VPA=VPAI/100000.0D0*V0R
!     VPB=VPBI/100000000.0D0*V0R
!-----
      SQT0=DSQRT(T0)
      TT=T*T
      SQT=DSQRT(T)
!-----
      CPRDT=K1*(T-T0)-K3*(1.0D0/T-1.0D0/T0)+2D0*K4*(SQT-SQT0) &
      -K8*(1.0D0/(TT)-1.0D0/(TT0))/2.0D0
      CPRTDT=K1*DLOG(T/T0)-K3*(1.0D0/(TT)-1.0D0/(TT0))/2.0D0 &
      -2D0*K4*(1.0D0/SQT-1.0D0/SQT0) &
      -K8*(1.0D0/(TT*T)-1.0D0/(TT0*T0))/3.0D0
      GR=H0R+CPRDT-T*(S0R+CPRTDT)
!-----
      VOLUM=V0R+VTA*(T-T0)+VTB*(T-T0)**2+VPA*(P-P0)+VPB*(P-P0)**2
!-----
      GR=GR+(P-P0)*(V0R+VTA*(T-T0)+VTB*(T-T0)**2) &
      +VPA*(P**2/2.0D0-P*P0+P0**2/2.0D0) &
      +VPB*(P**3/3.0D0-P**2*P0+P*P0**2-P0**3/3.0D0)
!*****
      CALL ALBITE (P,T,FGR,FHR,FSR,FCPR,FVR)
      GR=GR+FGR
      VOLUM=VOLUM+FVR
      RETURN
      END
!-----
!-----CdC end
!-----
!********************************
      SUBROUTINE ALBITE(P, T, G, H, S, CP, V)
!     routine to compute thermodynamic properties of albite
!     ordering relative to low albite standard state
!     *** function of Salje (1985) uses monalbite std state ***
!
      IMPLICIT NONE
      INTEGER(4) ICOUNT
      REAL(8) P,T,G,H,S,CP,V,A,A0,A02,A03,A04,A05,AOD0, &
      BOD,COD,B,D,D3,D5,DD0,DD1,DD2,DD3,B2,B3,B4,B5, &
      E1,E2,E3,E4,E5,E6,E7,E8,T1,T2,T12,T13,T14,T15,TC,TOD, &
      QOD,F,FLAST,Q,QLAST,QINT
!
      G = 0.0D0
      H = 0.0D0
      S = 0.0D0
      CP = 0.0D0
      V = 0.0D0
      IF (T .GE. 1290.0D0) RETURN
!
!   **** Q parameters for solving eqn. (19) *****
!
      A0 = 5.479D0
      B = 6854.0D0
      BOD = -9301.0D0
      COD = 43600.0D0
      DD0 = -2.171D0
      DD1 = -3.043D0
      DD2 = -0.001569D0
      DD3 = 0.000002109D0
!
      AOD0 = 41.620D0
      D = DD0 + DD1 * T + DD2 * T ** 2 + DD3 * T ** 3
!
      T1 = T - 1251.0D0
      T2 = T - 824.1D0
      D3 = D ** 3
      D5 = D ** 5
      T12 = T1 ** 2
      T13 = T1 ** 3
      T14 = T1 ** 4
      T15 = T1 ** 5
      B2 = B ** 2
      B3 = B ** 3
      B4 = B ** 4
      B5 = B ** 5
      A02 = A0 ** 2
      A03 = A0 ** 3
      A04 = A0 ** 4
      A05 = A0 ** 5
!
!   *******************************
!
!
!
!    Now iteratively find the solution (Q) to the following equation
!
      E1 = A0 * AOD0 * T1 * T2 / D - D
      E2 = AOD0 * B * T2 / D + BOD * A03 * T13 / D3
      E3 = 3.0D0 * BOD * A02 * B * T12 / D3 + COD * A05 * T15 / D5
      E4 = 3.0D0 * BOD * A0 * B2 * T1 / D3 + 5.0D0 * A04 * COD * B * &
      T14 / D5
      E5 = BOD * B3 / D3 + 10.0D0 * COD * A03 * B2 * T13 / D5
      E6 = 10.0D0 * COD * A02 * B3 * T12 / D5
      E7 = 5.0D0 * COD * A0 * B4 * T1 / D5
      E8 = COD * B5 / D5
!
      ICOUNT = 0
      QINT = 0.10D0
      FLAST = -1.0
      IF (T .LT. 1100.0D0) THEN
        QLAST = 1.10D0
        Q = 1.1D0
      ELSE
        QLAST = 0.60D0
        Q = 0.60D0
      END IF
   10 Q = Q - QINT
      IF (Q .LT. 0.00001) Q = 0.0001D0
   20 F = E1 * Q + E2 * Q ** 3 + E3 * Q ** 5 + E4 * Q ** 7 + E5 * Q ** &
      9 + E6 * Q ** 11 + E7 * Q ** 13 + E8 * Q ** 15
      ICOUNT = ICOUNT + 1
      IF (DABS(Q - QLAST) .LT. 0.0001) GO TO 30
      IF (ICOUNT .GT. 30) GO TO 30
      IF (F*FLAST .LT. 0.001) THEN
        QINT = QINT / 2.0D0
        Q = Q + QINT
        GO TO 20
      ELSE
        QLAST = Q
        FLAST = F
        GO TO 10
      END IF
!
!
!   *****  NOW  calculate Qod value from eqn. (4)
!
   30 A = A0 * (T - 1251.0D0)
      TC = 1251.0D0
      TOD = 824.1D0
!
      QOD = (-A*Q - B*Q**3) / D
!
!
!   *** NOW Calculate dS and dH from eqns. (21) and (22) ***
!
      S = 0.5D0 * A0 * Q ** 2 + 0.5D0 * AOD0 * QOD ** 2 + (DD1 + 2.0D0* &
      DD2*T + 3.0D0*DD3*T**2) * Q * QOD
      S = -S
      H = -0.5D0 * A0 * TC * Q ** 2 + 0.25D0 * B * Q ** 4 - 0.5D0 * &
      AOD0 * TOD * QOD ** 2 + 0.25D0 * BOD * QOD ** 4 + COD / 6.0D0 * &
      QOD ** 6 + (DD0 - DD2*T**2 - 2.0D0*DD3*T**3) * Q * QOD
      CP = A02 * T / (2.0D0*B)
!
      V = H / 335282.925D0
!
!   above factor gives .04 J/bar as diff between high and low albite
!      with shape of vol(disorder) following shape of H(disorder)
!
      G = H - T * S + (P - 1.0D0) * V
!
!     WRITE (6,99) ICOUNT, T, Q, F, QOD, G, H ,S
!   40 FORMAT (I5, 4F15.5, /, 3F15.2)
      RETURN
      END
!-----
!********************************
      SUBROUTINE KUNDJ(JJ,P,T,G,VOLUM)
      REAL(8) P,T,G,H0,S0,K1,K2,K3,K4,K6, &
      CPDT,CPTDT,T0,TT,TT0,SQT,SQT0,FUGCF,VOLUM
      INTEGER(4) JJ
      T0=298.15D0
      TT=T*T
      TT0=T0*T0
      SQT=DSQRT(T)
      SQT0=DSQRT(T0)
      IF (JJ.EQ.2) THEN
      H0=-393510.010
      S0=213.6770D0
      K1=93.0000D0
      K4=-1340.900D0
      K3=123800.000D0
      K6=6336.20D0
      K2=-0.002876D0
      ELSE
      H0=-241816.00D0
      S0=188.7200D0
      K1=115.45000D0
      K4=-3799.900D0
      K3=-2871300.00D0
      K6=51055.16D0
      K2=-0.002062D0
      END IF
      CPDT=K1*(T-T0)+K2*(TT-TT0)/2.0D0 &
      -K3*(1.0D0/T-1.0D0/T0)+2D0*K4*(SQT-SQT0) &
      +K6*DLOG(T/T0)
      CPTDT=K1*DLOG(T/T0)+K2*(T-T0) &
      -K3*(1.0D0/(TT)-1.0D0/(TT0))/2.0D0 &
      -2D0*K4*(1.0D0/SQT-1.0D0/SQT0) &
      -K6*(1.0D0/T-1.0D0/T0)
      G=H0+CPDT-T*(S0+CPTDT)
      CALL WCKJ(T,P,JJ,FUGCF,VOLUM)
      G=G+8.3143*T*DLOG(FUGCF*P)
      VOLUM=VOLUM/10.0D0
      RETURN
      END
!-----
!********************************
!     ********************************************************
!
      SUBROUTINE WCKJ (TDEGK,PB,JJ,FUGCF,VOLUM)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
!
!             PROGRAM WRITTEN BY KERRICK AND JACOBS (1981)
!             FOR CALCULATION OF FREE ENERGY OF CO2, H2O
!             AND FOR THEIR MIXTURES
!
!             MIXTURES ARE RESTRICTED TO 325 - 1050 C BECAUSE VALUES OF
!             C, D, OR E BECOME NEGATIVE AND THUS CAN'T TAKE THE
!             SQUARE ROOT
!
      COMMON /MVAR/BC,CC,DC,EC,BW,CW,DW,EW,J,L,M
      COMMON /MNRVAR/PK,P,T,R,T12
!
!       VARIABLES AND RULES ARE AS FOLLOWS:
!
!        ACO2,AH2O....ACTIVITY OF CO2 AND H2O, RESPECTIVELY
!        BC,BW,BM.....COVOLUME OF CO2,H2O AND MIXTURE; CC/MOLE
!        CC,CW,CM.....ATTRACTIVE TERM FOR CO2, H2O, AND MIXTURE IN
!                     MRK EQUATION ; BAR*(CC**2)**2SQRT(T)/MOLE**2
!        DC, DW,,DM...ATTRACTIVE TERM FOR CO2, H2O, AND MIXTURE IN
!                     MRK EQUATION; BAR*(CC**3)*SQRT(T)/MOLE**3
!        EC,EW,EM.....ATTRACTIVE TERM FOR CO2, H2O, AND MIXTURE IN
!                     MRK EQUATION; BAR*(CC**4)*SQRT(T)/MOLE**4
!        CIJ,DIJ,EIJ..CROSS COEFFICIENTS OF C,D,E
!        FKCM,FKWM....FUGACITY COEFFICIENT OF CO2 AND H2O IN
!                     THE FLUID MIXTURE
!        FKCP,FKWP....FUGACITY COEFFICIENTS OF PURE CO2 AND PURE
!                     H2O, RESPECTIVELY
!        PK,P.........PRESSURE; KBARS,BARS, RESPECTIVELY
!        R............GAS CONSTANT; 83.14 CC*BARS/MOLE*K
!        TC,T.........TEMPERATURE; CELSIUS,KELVIN, RESPECTIVELY
!        VC,VW,VM.....MOLAR VOLUME OF CO2, H2O, AND MIXTURE; CC/MOLE
!        XC,XCO2......MOLE FRACTION OF CO2 IN THE FLUID MIXTURE
!        XW,XH2O......MOLE FRACTION OF H2O IN THE FLUID MIXTURE
!        Y............B/V4; VARIABLE IN HARD SPHERE-EQUATION
!        ZC,ZW,ZM.....cmnPRESSIBILITY OF CO2, H2O, AND MIXTURE
!
!
!        DEFINITION OF CONSTANRS AND INPUT FOR THE CALCULATION OF
!    THE P, T, XCO2 VALUES WHICH ARE USED THROUGHOUT THE PROGRAM.
!
!
      R=83.14D0
      BW=29.00D0
      BC=58.00D0
!
!     *** INDEXING SO THAT KERRICK AND JACOBS IS COMPATIBLE
!         WITH UBC --
!             H2O = 1
!             CO2 = 2
!
      IF (JJ.EQ.1) J=2
      IF (JJ.EQ.2) J=1
      L = J
      M = J
!
!        CALCULATION OF PARAMETERS USED IN THE PROGRAM.
!
      P   = PB
      PK  = P/1000.0D0
      T   = TDEGK
      TC  = T-273.15D0
      T15 = SQRT(T**3)
      T12 = SQRT(T)
      RT  = R * T15
      CC  = (28.31D0+0.10721D0*T-0.00000881D0*T*T)*1000000.0D0
      DC  = (9380.0D0-8.53D0*T+0.001189D0*T*T)*1000000.0D0
      EC  = (-368654.0D0+715.9D0*T+0.1534D0*T*T)*1000000.0D0
      CW  = (290.78D0-0.30276D0*T+0.00014774D0*T*T)*1000000.0D0
      DW  = (-8374.0D0+19.437D0*T-0.008148D0*T*T)*1000000.0D0
      EW  = (76600.0D0-133.9D0*T+0.1071D0*T*T)*1000000.0D0
!
!         ROUTINES ZPURE, FPURE ARE CALLED TO CALCULATE
!
!        1) Z OF PURE CO2 AND H2O RESPECTIVELY
!        2) FUGACITY COEFFICIENTS OF CO2 AND H2O RESPECTIVELY
!
!    AT EACH P, T, XCO2 CONDITION.
!
      CALL ZPURE (PK, ZC, VC, ZW, VW, TC)
      CALL FPURE (P, RT, FKWP, FKCP, ZC, VC, ZW, VW)
!
      FUGCF = FKWP
      VOLUM = VW
      IF (JJ.EQ.2) FUGCF=FKCP
      IF (JJ.EQ.2) VOLUM=VC
!
!   REFERENCES:
!
!       KERRICK, D. M. AND JACOBS, G. K. IN PRESS; A MODIFIED
!   REDLICH-KWONG EQUATION FOR H2O, CO2, AND H2O-CO2 MIXTURES AT
!   ELEVATED PRESSURES AND TEMPURATURES, AM. JOUR. SCI.,IN PRESS.
!
!
       RETURN
       END
!-----
!********************************
      SUBROUTINE ZPURE(PK,ZC,VC,ZW,VW,TC)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /MVAR/BC,CC,DC,EC,BW,CW,DW,EW,J,L,M
!
!        A SUBPROGRAM TO CALCULATE THE VOLUME OF CO2 AND H2O
!    FOR EACH PRESSURE AND TEMPERATURE OF THE SERIES. AN INITIAL GUESS
!    OF THE VOLUME(VI) IS CHOSEN FOR A GIVEN PRESSURE RANGE. THIS VALUE
!    IS THEN USED IN THE ROUTINE NEWRAP, WHICH SOLVES FOR THE EXACT
!    VOLUME BY MEANS OF AN ITERATIVE NEWTON - RAPHSON TECHNIQUE.
!
!
      IF (J.EQ.1) THEN
          B = BC
          C = CC
          D = DC
          E = EC
          IF (PK.GE.1.0D0)                      VI=35.00
          IF (PK.GE.0.10D0 .AND. PK.LT.1.0D0)   VI=100.00
          IF (PK.GE.0.005D0 .AND. PK.LT.0.10D0) VI=500.00
          IF (PK.LT.0.005D0)                    VI=5000.0
      ELSE
          B = BW
          C = CW
          D = DW
          E = EW
          IF (PK.GE.1.0D0)                      VI =   22.0
          IF(PK.GE.0.90D0 .AND. PK.LT.1.0D0)    VI =   24.2
          IF(PK.GE.0.60D0 .AND. PK.LT.0.9D0)    VI =   31.2
          IF(PK.GE.0.21D0 .AND. PK.LT.0.60D0 .AND. TC.GE.550.0D0) &
                                                VI =   75.00
          IF(PK.GE.0.21D0 .AND. PK.LT.0.60D0 .AND. TC.LT.550.0D0) &
                                                VI =   35.00
          IF(PK.GE.0.10D0 .AND. PK.LT.0.21D0 .AND. TC.LT.400.0D0) &
                                                VI =   15.00
          IF(PK.GE.0.10D0 .AND. PK.LT.0.21D0 .AND. TC.GE.400.0D0) &
                                                VI =  100.00
          IF(PK.GE.0.005D0 .AND. PK.LT.0.10D0)  VI =  500.00
          IF(PK.LT.0.005D0)                     VI = 1000.00
      ENDIF
!
      CALL NEWRAP(B,C,D,E,Z,V,VI)
!
      IF (J.EQ.1) THEN
          ZC=Z
          VC=V
      ELSE
          ZW=Z
          VW=V
      ENDIF
      RETURN
      END
!-----
!********************************
      SUBROUTINE ZMIX(XC,XW,VM,ZM,VC,VW,BM,CM,DM,EM)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /MVAR/BC,CC,DC,EC,BW,CW,DW,EW,J,L,M
!
!        A PROGRAM TO CALCULATE THE VOLUME OF A MIXTURE
!    OF CO2 AND H2O AT EACH PRESSURE, TEMPERATURE, AND XCO2.
!    THE MOLAR VOLUMES OF CO2 AND H2O AS CALCULATED IN ZPURE ARE
!    USED TO DEFINE THE INITIAL ESTIMATE. ROUTINE NEWRAP
!    IS THEN USED TO CALCULATE THE VOLUME OF THE MIXTURE.
!
!
      VI = (VC*XC) + (VW*XW)
      B  = BM
      C  = CM
      D  = DM
      E  = EM
      CALL NEWRAP(B,C,D,E,Z,V,VI)
      VM  = V
      ZM  = Z
      RETURN
      END
!-----
!********************************
      SUBROUTINE NEWRAP(B,C,D,E,Z,V,VI)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /MNRVAR/PK,P,T,R,T12
!
!             A SUBPROGRAM CALLED BY ZPURE AND ZMIX TO CALCULATE THE
!     VOLUME OF CO2, H2O, AND A MIXTURE OF CO2-H2O FOR EACH PRESSURE,
!     TEMPERATURE, AND XCO2. THE METHOD OF NEWTON-RAPHSON IS
!     EMPLOYED HERE, SUMMARIZED AS FOLLOWS:
!
!
!             F(X) = 0;  X(K+1) = X(K) - F(X)/DF(X)
!     WHERE DF(X) IS THE PARTIAL DIFFERENTIAL OF F(X) WITH
!     RESPECT TO X. X(K+1) IS CALCULATED FOR "K" ITERATIONS,
!     UNTIL NO CHANGE IN X(K+1) OCCURS.
!
!
!        INITIALIZATION OF PARAMETERS:
!
!
      DO 10 K=1,50
        Y   = B/(4.0D0*VI)
        X   = (1.0D0 - Y)
        BI  = (VI + B)
        BI2 = (VI + B)**2
!
!
!        DEFINITION OF THE F(X) FOR NEWRAP:
!
!        F(X) = 0 = P(REPULSIVE) - P(ATTRACTIVE) - P
!
!        WHERE F(X) IS A REARRANGEMENT OF KERRICK AND JACOBS'(1980)
!        EQUATION (14).
!
!
        PN  = 1.0D0 + Y + (Y**2) -(Y**3)
        PR  = (PN / (VI * (X**3))) * R * T
        PA1 = C + (D / VI) + (E / (VI * VI))
        PA2 = PA1 / (T12 * VI * BI)
        F   = PR - PA2 - P
!
!        DEFINITION OF THE DIFFERENTIAL OF F(X) FOR NEWARP:
!
!        DF(X) = DP(REPULSIVE) - DP(ATTRACTIVE)
!
        D1  = (-3.0D0*B) / (4.0D0*(VI**3)*X**4)
        D2  = -1.0D0 / ((VI**2)*(X**3))
        D3  =  1.0D0 / (VI*(X**3))
        D4  = -B / (4.0D0*VI**2)
        D5  = -2.0D0*(B**2) / (16.0D0*(VI**3))
        D6  =  3.0D0*(B**3) / (64.0D0*(VI**4))
        DPR = ((PN*(D1+D2)) + (D3*(D4+D5+D6)))*R*T
        D7  = (-1.0D0/(VI*BI2)) + (-1.0D0/(VI**2*BI))
        D8  = 1.0D0 / (VI*BI)
        D9  = (-D/VI**2) + ((-2.0D0*E)/(VI**3))
        DPA = (PA1*D7+D8*D9) / T12
        DF  = DPR - DPA
!
!        CALCULATION OF V(K+1) AN CONTINUATION OR END OF ITERATIONS
!      FOR NEWARP
!
           V    = VI - (F/DF)
           DIFF = DABS(V-VI)
           IF (DIFF .LT. 0.01D0) THEN
               Z = (V*P) / (R*T)
               RETURN
           ENDIF
           IF (V   .GT. 1000000.0) V = 1000000.0
           IF (V   .LT.       9.9) V = 10.0
           VI = V
   10  CONTINUE
       Z = (V*P) / (R*T)
       RETURN
       END
!-----
!********************************
      SUBROUTINE FPURE(P, RT, FKWP, FKCP, ZC, VC, ZW, VW)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /MVAR/BC,CC,DC,EC,BW,CW,DW,EW,J,L,M
!
!        A SUBPROGRAM TO CALCULATE FUGACITY COEFFICIENTS
!    OF PURE CO2 AND PURE H2O AT EACH PRESSURE AND TEMPERATURE.
!    SEE KERRICK AND JACOBS (IN PRESS) FOR A DERIVATION OF THE PURE
!    FUGACITY COEFFICIENT EXPRESSION (FCP)
!
!
      IF (L.EQ.1) THEN
          B = BC
          C = CC
          D = DC
          E = EC
          V = VC
          Z = ZC
      ELSE
          B = BW
          C = CW
          D = DW
          E = EW
          V = VW
          Z = ZW
      ENDIF
      Y   = B / (4.0D0*V)
      FCP = 8.0D0*Y-9.0D0*Y*Y+3.0D0*Y**3
      FCP = FCP / ((1.0D0-Y)**3)
      FCP = FCP - DLOG(Z)
      FCP = FCP -  (C/(RT*(V+B)))-(D/(RT*V*(V+B)))
      FCP = FCP -  (E/(RT*V*V*(V+B)))+((C/(RT*B))*(DLOG(V/(V+B))))
      FCP = FCP -  (D/(RT*B*V))+((D/(RT*B*B))*(DLOG((V+B)/V)))
      FCP = FCP -  (E/(RT*2.0D0*B*V*V))+(E/(RT*B*B*V))
      FCP = FCP - ((E/(RT*B**3))*(DLOG((V+B)/V)))
!dC
      IF (FCP.GT.50) FCP=50
!dC
      FCP = DEXP(FCP)
      IF (L.EQ.1) THEN
          FKCP = FCP
      ELSE
          FKWP = FCP
      ENDIF
      RETURN
      END
!-----
!********************************
      SUBROUTINE FMIX(P,RT,CIJ,DIJ,EIJ,XC,XW,BM,FCCM,FCWM,FCM,FWM,VM,ZM, &
                 CM,DM,EM)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /MVAR/BC,CC,DC,EC,BW,CW,DW,EW,J,L,M
!
!        A SUBPROGRAM TO CACLULATE THE FUGACITY COEFFICIENTS
!     OF CO2 AND H2O IN A N2O-CO2 MIXTURE.  THESE
!     TOGETHER WITH THE FUGACITY COEFFICIENTS OF PURE CO2 AND PURE
!     H2O CAN BE USED TO CALCULATE ACTIVITIES OF CO2 AND H2O IN THE
!     MIXTURE FOR EACH PRESSURE, TEMPERATURE, AND XCO2.
!     SEE KERRICK AND JACOBS (IN PRESS) FOR A DERIVATION OF FUGACITY
!     COEFFICIENT EXPRESSION (FCM)
!
!
      B = BM
      V = VM
      Z = ZM
      C = CM
      D = DM
      E = EM
      Y = B / (4.0D0*V)
      IF (M.EQ.1) THEN
          B1 = BC
          C1 = CC
          D1 = DC
          E1 = EC
          X1 = XC
          X2 = XW
      ELSE
          B1 = BW
          C1 = CW
          D1 = DW
          E1 = EW
          X1 = XW
          X2 = XC
      ENDIF
      FCM = (4.0D0*Y-3.0D0*Y*Y)/((1.0D0-Y)**2)
      FCM = FCM + ((B1/B)*((4.0D0*Y-2.0D0*Y*Y)/((1.0D0-Y)**3)))
      FCM = FCM - (((2.0D0*C1*X1+2.0D0*CIJ*X2)/(RT*B))*(DLOG((V+B)/V)))
      FCM = FCM - ((C*B1)/(RT*B*(V+B)))
      FCM = FCM + (((C*B1)/(RT*B*B))*(DLOG((V+B)/V)))
      FCM = FCM - ((2.0D0*D1*X1+2.0D0*DIJ*X2+D)/(RT*B*V))
      FCM = FCM + (((2.0D0*X1*D1+2.0D0*DIJ*X2+D)/(RT*B*B))* &
                       (DLOG((V+B)/V)))
      FCM = FCM + ((D*B1)/(RT*V*B*(V+B)))
      FCM = FCM + ((2.0D0*B1*D)/(RT*B*B*(V+B)))
      FCM = FCM - (((2.0D0*B1*D)/(RT*(B**3)))*(DLOG((V+B)/V)))
      FCM = FCM - ((2.0D0*E1*X1+2.0D0*EIJ*X2+2.0D0*E)/(RT*2.0D0*B*V*V))
      FCM = FCM + ((2.0D0*E1*X1+2.0D0*EIJ*X2+2.0D0*E)/(RT*B*B*V))
      FCM = FCM - (((2.0D0*E1*X1+2.0D0*EIJ*X2+2.0D0*E)/(RT*(B**3))) &
                              *(DLOG((V+B)/V)))
      FCM = FCM + ((E*B1)/(RT*2.0D0*B*V*V*(V+B)))
      FCM = FCM - ((3.0D0*E*B1)/(RT*2.0D0*B*B*V*(V+B)))
      FCM = FCM + (((3.0D0*E*B1)/(RT*(B**4)))*(DLOG((V+B)/V)))
      FCM = FCM - ((3.0D0*E*B1)/(RT*(B**3)*(V+B)))
      FCM = FCM - (DLOG(Z))
      FCM = DEXP(FCM)
      IF (M.EQ.1) THEN
          FCCM = FCM
      ELSE
          FCWM = FCM
      ENDIF
      RETURN
      END
!-----
!********************************
      SUBROUTINE MIXKJ (TEMP, PBAR, XCO2, FGCO, FGH2O, ACO, AH2O)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      COMMON /MVAR/BC,CC,DC,EC,BW,CW,DW,EW,J,L,M
      COMMON /MNRVAR/PK,P,T,R,T12
      P = PBAR
      T=TEMP
!
!       VARIABLES AND RULES ARE AS FOLLOWS:
!
!        ACO2,AH2O....ACTIVITY OF CO2 AND H2O, RESPECTIVELY
!        BC,BW,BM.....COVOLUME OF CO2,H2O AND MIXTURE; CC/MOLE
!        CC,CW,CM.....ATTRACTIVE TERM FOR CO2, H2O, AND MIXTURE IN
!                     MRK EQUATION ; BAR*(CC**2)**2SQRT(T)/MOLE**2
!        DC, DW,,DM...ATTRACTIVE TERM FOR CO2, H2O, AND MIXTURE IN
!                     MRK EQUATION; BAR*(CC**3)*SQRT(T)/MOLE**3
!        EC,EW,EM.....ATTRACTIVE TERM FOR CO2, H2O, AND MIXTURE IN
!                     MRK EQUATION; BAR*(CC**4)*SQRT(T)/MOLE**4
!        CIJ,DIJ,EIJ..CROSS COEFFICIENTS OF C,D,E
!        FKCM,FKWM....FUGACITY COEFFICIENT OF CO2 AND H2O IN
!                     THE FLUID MIXTURE
!        FKCP,FKWP....FUGACITY COEFFICIENTS OF PURE CO2 AND PURE
!                     H2O, RESPECTIVELY
!        PK,P.........PRESSURE; KBARS,BARS, RESPECTIVELY
!        R............GAS CONSTANT; 83.14 CC*BARS/MOLE*K
!        TC,T.........TEMPERATURE; CELSIUS,KELVIN, RESPECTIVELY
!        VC,VW,VM.....MOLAR VOLUME OF CO2, H2O, AND MIXTURE; CC/MOLE
!        XC,XCO2......MOLE FRACTION OF CO2 IN THE FLUID MIXTURE
!        XW,XH2O......MOLE FRACTION OF H2O IN THE FLUID MIXTURE
!        Y............B/V4; VARIABLE IN HARD SPHERE-EQUATION
!        ZC,ZW,ZM.....COMPRESSIBILITY OF CO2, H2O, AND MIXTURE
!
!        DEFINITION OF CONSTANRS AND INPUT FOR THE CALCULATION OF
!    THE P, T, XCO2 VALUES WHICH ARE USED THROUGHOUT THE PROGRAM.
!
      R  = 83.14D0
      BW = 29.00D0
      BC = 58.00D0
!
!        CALCULATION OF PARAMETERS USED IN THE PROGRAM.
!
      PK   = P / 1000.0D0
      TC   = T - 273.15D0
      XC   = XCO2
      XW   = 1.0D0 - XC
      ACO  = XC
      AH2O = XW
      T15  = SQRT(T**3)
      T12  = SQRT(T)
      RT   = R * T15
      CC = (28.31D0+0.10721D0*T-0.00000881D0*T*T)*1000000.0D0
      DC = (9380.0D0-8.53D0*T+0.001189D0*T*T)*1000000.0D0
      EC = (-368654.0D0+715.9D0*T+0.1534D0*T*T)*1000000.0D0
      CW = (290.78D0-0.30276D0*T+0.00014774D0*T*T)*1000000.0D0
      DW = (-8374.0D0+19.437D0*T-0.008148D0*T*T)*1000000.0D0
      EW = (76600.0D0-133.9D0*T+0.1071D0*T*T)*1000000.0D0
      IF (TC.GE.325.0D0 .AND. TC.LE.1050.0D0) THEN
          BM  = (BC*XC)+(BW*XW)
          CIJ = SQRT(CC*CW)
          DIJ = SQRT(DC*DW)
          EIJ = SQRT(EC*EW)
          CM  = (CC*XC*XC)+(CW*XW*XW)+(2.0D0*XC*XW*CIJ)
          DM  = (DC*XC*XC)+(DW*XW*XW)+(2.0D0*XC*XW*DIJ)
          EM  = (EC*XC*XC)+(EW*XW*XW)+(2.0D0*XC*XW*EIJ)
      ENDIF
!
!    ROUTINES ZPURE, ZMIX, FPURE, AND FMIX ARE CALLED TO
!    CALCULATE:
!
!        1) Z OF PURE CO2 AND H2O RESPECTIVELY
!        2) Z OF CO2-H2O MIXTURES
!        3) FUGACITY COEFFICIENTS OF CO2 AND H2O RESPECTIVELY
!        4) FUGACITY COEFFICIENTS OF CO2 AN H2O IN THE MIXTURE
!
!    AT EACH P, T, XCO2 CONDITION.
!
      DO 20 J=1,2
        CALL ZPURE( PK, ZC, VC, ZW, VW, TC)
   20 CONTINUE
      IF (TC.GE.325.0D0 .AND. TC.LE.1050.0D0) THEN
          CALL ZMIX( XC, XW, VM, ZM, VC, VW, BM, CM, DM, EM)
      ENDIF
      DO 40 L = 1, 2
          CALL FPURE( P, RT, FKWP, FKCP, ZC, VC, ZW, VW)
   40 CONTINUE
      FGCO  = FKCP
      FGH2O = FKWP
!
!     CALCULATION AND DEFINITION OF ARRAYS FOR ACTIVITIES,
!     FUGACITIES IN THE MIXTURE, AND MOLE FRACTION.
!
      IF (TC.GE.325.0D0 .AND. TC.LE.1050.0D0) THEN
          DO 50 M = 1, 2
                CALL FMIX(P, RT, CIJ, DIJ, EIJ, XC, XW, BM, FCCM, &
                              FCWM, FCM, FWM, VM, ZM, CM, DM, EM)
   50     CONTINUE
          ACO  = FCCM * XC / FKCP
          AH2O = FCWM * XW / FKWP
      ELSE
          ACO  = XCO2
          AH2O = XW
      ENDIF
!
!   REFERENCES:
!
!       KERRICK, D. M. AND JACOBS, G. K. IN PRESS; A MODIFIED
!   REDLICH-KWONG EQUATION FOR H2O, CO2, AND H20-CO2 MIXTURES AT
!     ELEVATED PRESSURES AND TEMPURATURES, AM. JOUR. SCI.,IN PRESS.
!
      RETURN
      END
!--------------------------------
!--------------------------------
      SUBROUTINE DUAN06HL(T,P,XF,VOLUM,ACTIV1,ACTIV2,F1PLO,F2PLO, &
      F1PHI,F2PHI,F1PHI2000,F2PHI2000,F1PLO2000,F2PLO2000)
      IMPLICIT NONE
!----
!-----
      REAL(8) T,P,XF(2),VAT,AC1,AC2,VOLUM, &
      ACTIV1,ACTIV2,FH2OP,FCO2P,FH2OX,FCO2X, &
      F1XHI,F2XHI,F1PHI,F2PHI,P2000, &
      F1XHI2000,F2XHI2000,F1PHI2000,F2PHI2000, &
      F1XLO2000,F2XLO2000,F1PLO2000,F2PLO2000, &
      F1PLO,F2PLO,F1XLO,F2XLO
      P2000=2000.0D0
!===
      IF (P.LE.P2000) THEN
!===
!      WRITE (6,2002)
! 2002 FORMAT (/,' calling H2O-CO2 mix at P with lo', &
!              /,' ================================')
!      F1PLO=0.0D0
!      F2PLO=0.0D0
      CALL DUAN06EQLO(T,P,XF,VAT,AC1,AC2,F1PLO,F2PLO, &
      F1XLO,F2XLO)
      VOLUM=VAT*100.0D0
      FH2OP=F1PLO
      FCO2P=F2PLO
      ACTIV1=F1XLO*XF(1)/F1PLO
      ACTIV2=F2XLO*XF(2)/F2PLO
!===
      ELSE
!      WRITE (6,2008)
! 2008 FORMAT (/,' calling H2O-CO2 mix at P with hi', &
!              /,' ================================')
!      F1PHI=0.0D0
!      F2PHI=0.0D0
      CALL DUAN06EQHI(T,P,XF,VAT,AC1,AC2,F1PHI,F2PHI,F1XHI,F2XHI)
      VOLUM=VAT*100.0D0
!      WRITE (6,2010)
! 2010 FORMAT (/,' calling H2O-CO2 mix at 2000 with hi', &
!              /,' ===================================')
!      F1PHI2000=0.0D0
!      F2PHI2000=0.0D0
      CALL DUAN06EQHI(T,P2000,XF,VAT,AC1,AC2,F1PHI2000,F2PHI2000, &
      F1XHI2000,F2XHI2000)
!      WRITE (6,2012)
! 2012 FORMAT (/,' calling H2O-CO2 mix at 2000 with lo', &
!              /,' ===================================')
!      F1PLO2000=0.0D0
!      F2PLO2000=0.0D0
      CALL DUAN06EQLO(T,P2000,XF,VAT,AC1,AC2,F1PLO2000,F2PLO2000, &
      F1XLO2000,F2XLO2000)
      FH2OP=F1PHI*F1PLO2000/F1PHI2000
      FCO2P=F2PHI*F2PLO2000/F2PHI2000
!
      FH2OX=F1XHI*F1XLO2000/F1XHI2000
      FCO2X=F2XHI*F2XLO2000/F2XHI2000
!
      ACTIV1=XF(1)*FH2OX/FH2OP
      ACTIV2=XF(2)*FCO2X/FCO2P
!
      ACTIV1=(F1XHI*F1XLO2000/F1XHI2000)/(F1PHI*F1PLO2000/F1PHI2000)
      ACTIV1=ACTIV1*XF(1)
      ACTIV2=(F2XHI*F2XLO2000/F2XHI2000)/(F2PHI*F2PLO2000/F2PHI2000)
      ACTIV2=ACTIV2*XF(2)
!===
      END IF
!
      END
!--------------------------------
!--------------------------------
!--------------------------------
      SUBROUTINE DUAN06EQHI(T,P,XF,VAT,AC1,AC2,FUGH2O,FUGCO2,ACH,ACC)
      IMPLICIT NONE
!--
      REAL(8) BVC,CVC2,DVC4,EVC5,FVC2,BETA1,GAMMAVC2,R,RT,MWT, &
      BST1,CST1,DST1,EST1,FST1,BESTST1,GAMST1, &
      BST2,CST2,DST2,EST2,FST2,BESTST2,GAMST2
      COMMON /DTHINGS/ BVC,CVC2,DVC4,EVC5,FVC2,BETA1,GAMMAVC2,R,RT, &
      MWT, &
      BST1,CST1,DST1,EST1,FST1,BESTST1,GAMST1, &
      BST2,CST2,DST2,EST2,FST2,BESTST2,GAMST2
!--
      REAL(8) P,T,VATP,INTATP, &
      XF(2),ZATP,PHI1,PHI2,VAT,ACH,ACC, &
      V01,PH01,V02,PH02, &
      FUGH2O,FUGCO2,AH2O,ACO2,AC1,AC2,G
!      INTEGER(4) I
!--
!
      IF (FUGH2O.EQ.0.0D0.AND.FUGCO2.EQ.0.0D0) THEN
!      WRITE (6,1900)
! 1900 FORMAT (/,' pure H2O', &
!              /,' --------')
      CALL DUAN06H2OHI(T,P,G,V01,PH01)
      FUGH2O=PH01
!      WRITE (6,2000) FUGH2O
! 2000 FORMAT (' act.coeff     H2O    =',F20.10)
!!
!      WRITE (6,1902)
! 1902 FORMAT (/,' pure CO2', &
!              /,' --------')
      CALL DUAN06CO2HI(T,P,G,V02,PH02)
      FUGCO2=PH02
!      WRITE (6,2012) FUGCO2
! 2012 FORMAT (' act.coeff     CO2    =',F20.10)
!
      END IF
!
!
!      WRITE (6,1904)
! 1904 FORMAT (/,' H2O-CO2 mix', &
!              /,' -----------')
      CALL MIXRULESHI(T,P,XF)
      CALL DUANZ(T,P,ZATP,VATP,INTATP)
      CALL EQUA9(T,VATP,ZATP,PHI1,PHI2)
!
      VAT=VATP
      ACH=DEXP(PHI1)
      ACC=DEXP(PHI2)
!
!      WRITE (6,1008) (XF(I),I=1,2)
! 1008 FORMAT (/,' composition =',F10.5,' H2O',F10.5,' CO2',F10.5,' CH4')
!!
!      WRITE (6,1026) PHI1
! 1026 FORMAT (' ln(g)     H2O    =',F20.10)
!      WRITE (6,1028) RT*PHI1
! 1028 FORMAT (' RT*ln(g)  H2O    =',F20.10)
!      WRITE (6,1029) RT*PHI1+RT*DLOG(P)
! 1029 FORMAT (' RT*ln(f)  H2O    =',F20.10)
!      WRITE (6,1030) ACH
! 1030 FORMAT (' act.coeff H2O    =',F20.10)
!      WRITE (6,1032) PHI2
! 1032 FORMAT (' ln(g)     CO2         =',F20.10)
!      WRITE (6,1034) RT*PHI2
! 1034 FORMAT (' RT*ln(g)  CO2         =',F20.10)
!      WRITE (6,1035) RT*PHI2+RT*DLOG(P)
! 1035 FORMAT (' RT*ln(f)  CO2         =',F20.10)
!      WRITE (6,1036) ACC
! 1036 FORMAT (' act.coeff CO2         =',F20.10)
!
!
      AH2O=ACH*XF(1)/FUGH2O
      ACO2=ACC*XF(2)/FUGCO2
!      WRITE (6,1050) AH2O
! 1050 FORMAT (/,' activity H2O    =',F20.10)
!      WRITE (6,1052) ACO2
! 1052 FORMAT (' activity CO2    =',F20.10)
      AC1=AH2O
      AC2=ACO2
!
      RETURN
      END
!--------------------------------
!--------------------------------
      SUBROUTINE DUAN06EQLO(T,P,XF,VAT,AC1,AC2,FUGH2O,FUGCO2,ACH,ACC)
      IMPLICIT NONE
!--
      REAL(8) BVC,CVC2,DVC4,EVC5,FVC2,BETA1,GAMMAVC2,R,RT,MWT, &
      BST1,CST1,DST1,EST1,FST1,BESTST1,GAMST1, &
      BST2,CST2,DST2,EST2,FST2,BESTST2,GAMST2
      COMMON /DTHINGS/ BVC,CVC2,DVC4,EVC5,FVC2,BETA1,GAMMAVC2,R,RT, &
      MWT, &
      BST1,CST1,DST1,EST1,FST1,BESTST1,GAMST1, &
      BST2,CST2,DST2,EST2,FST2,BESTST2,GAMST2
!--
      REAL(8) P,T,VATP,INTATP, &
      XF(2),ZATP,PHI1,PHI2,VAT,ACH,ACC, &
      V01,PH01,V02,PH02, &
      FUGH2O,FUGCO2,AH2O,ACO2,AC1,AC2,G
!      INTEGER(4) I
!--
!
      IF (FUGH2O.EQ.0.0D0.AND.FUGCO2.EQ.0.0D0) THEN
!      WRITE (6,1900)
! 1900 FORMAT (/,' pure H2O', &
!              /,' --------')
      CALL DUAN06H2OLO(T,P,G,V01,PH01)
      FUGH2O=PH01
!      WRITE (6,2000) FUGH2O
! 2000 FORMAT (' act.coeff     H2O    =',F20.10)
!!
!      WRITE (6,1902)
! 1902 FORMAT (/,' pure CO2', &
!              /,' --------')
      CALL DUAN06CO2LO(T,P,G,V02,PH02)
      FUGCO2=PH02
!      WRITE (6,2012) FUGCO2
! 2012 FORMAT (' act.coeff     CO2    =',F20.10)
!
      END IF
!
!
!      WRITE (6,1904)
! 1904 FORMAT (/,' H2O-CO2 mix', &
!              /,' -----------')
      CALL MIXRULESLO(T,P,XF)
      CALL DUANZ(T,P,ZATP,VATP,INTATP)
      CALL EQUA9(T,VATP,ZATP,PHI1,PHI2)
!
      VAT=VATP
      ACH=DEXP(PHI1)
      ACC=DEXP(PHI2)
!
!      WRITE (6,1008) (XF(I),I=1,2)
! 1008 FORMAT (/,' composition =',F10.5,' H2O',F10.5,' CO2',F10.5,' CH4')
!!
!      WRITE (6,1026) PHI1
! 1026 FORMAT (' ln(g)     H2O    =',F20.10)
!      WRITE (6,1028) RT*PHI1
! 1028 FORMAT (' RT*ln(g)  H2O    =',F20.10)
!      WRITE (6,1029) RT*PHI1+RT*DLOG(P)
! 1029 FORMAT (' RT*ln(f)  H2O    =',F20.10)
!      WRITE (6,1030) ACH
! 1030 FORMAT (' act.coeff H2O    =',F20.10)
!      WRITE (6,1032) PHI2
! 1032 FORMAT (' ln(g)     CO2         =',F20.10)
!      WRITE (6,1034) RT*PHI2
! 1034 FORMAT (' RT*ln(g)  CO2         =',F20.10)
!      WRITE (6,1035) RT*PHI2+RT*DLOG(P)
! 1035 FORMAT (' RT*ln(f)  CO2         =',F20.10)
!      WRITE (6,1036) ACC
! 1036 FORMAT (' act.coeff CO2         =',F20.10)
!
!
      AH2O=ACH*XF(1)/FUGH2O
      ACO2=ACC*XF(2)/FUGCO2
!      WRITE (6,1050) AH2O
! 1050 FORMAT (/,' activity H2O    =',F20.10)
!      WRITE (6,1052) ACO2
! 1052 FORMAT (' activity CO2    =',F20.10)
      AC1=AH2O
      AC2=ACO2
!
      RETURN
      END
!--------------------------------
!--------------------------------
      SUBROUTINE DUAN06H2OHI(T,P,G,VOLUM,FUGCF)
      IMPLICIT NONE
!--
      REAL(8) BVC,CVC2,DVC4,EVC5,FVC2,BETA1,GAMMAVC2,R,RT,MWT, &
      BST1,CST1,DST1,EST1,FST1,BESTST1,GAMST1, &
      BST2,CST2,DST2,EST2,FST2,BESTST2,GAMST2
      COMMON /DTHINGS/ BVC,CVC2,DVC4,EVC5,FVC2,BETA1,GAMMAVC2,R,RT, &
      MWT, &
      BST1,CST1,DST1,EST1,FST1,BESTST1,GAMST1, &
      BST2,CST2,DST2,EST2,FST2,BESTST2,GAMST2
!---
      REAL(8) TR,TR2,TR3,VC,VC2,VC4,VC5,EU3,T,P,Z,V,INTE, &
      V2,V4,V5,EXPON,PHI1,G,H0,S0,K1,K2,K3,K4,K6, &
      CPDT,CPTDT,T0,TT,TT0,SQT,SQT0,FUGCF,VOLUM
!--
      T0=298.15D0
      TT=T*T
      TT0=T0*T0
      SQT=DSQRT(T)
      SQT0=DSQRT(T0)
!---
      R=0.08314467D0
      RT=R*T
      EU3=1.0D0/3.0D0
!      PR=P/221.19D0
      TR=T/647.25D0
      TR2=TR*TR
      TR3=TR2*TR
      VC=R*647.25D0/221.19D0
      VC2=VC*VC
      VC4=VC2*VC2
      VC5=VC4*VC
      BVC=(4.68071541D-02-2.81275941D-01/TR2-2.43926365D-01/TR3)*VC
      CVC2=(1.10016958D-02-3.86603525D-02/TR2+9.30095461D-02/TR3)*VC2
      DVC4=(-1.15747171D-05+4.19873848D-04/TR2-5.82739501D-04/TR3)*VC4
      EVC5=(1.00936000D-06-1.01713593D-05/TR2+1.63934213D-05/TR3)*VC5
      FVC2=(-4.49505919D-02/TR3)*VC2
      BETA1=-3.15028174D-01
      GAMMAVC2=1.25000000D-02*VC2
      MWT=0.0180154D0
      BST1=2.0D0*BVC
      CST1=3.0D0*CVC2
      DST1=5.0D0*DVC4
      EST1=6.0D0*EVC5
      FST1=2.0D0*FVC2
      BESTST1=BETA1
      GAMST1=3.0D0*GAMMAVC2
      BST2=0.0D0
      CST2=0.0D0
      DST2=0.0D0
      EST2=0.0D0
      FST2=0.0D0
      BESTST2=0.0D0
      GAMST2=0.0D0
!---- find the volume
!--
      CALL DUANZ(T,P,Z,V,INTE)
!--
!---- EQ9 for H2O
!--
      V2=V*V
      V4=V2*V2
      V5=V4*V
      EXPON=DEXP(-GAMMAVC2/V2)
      PHI1=-DLOG(Z)+BST1/V+CST1/(2.0D0*V2)+ &
          DST1/(4.0D0*V4)+EST1/(5.0D0*V5)
      PHI1=PHI1+((FST1*BETA1+BESTST1*FVC2)/ &
          (2.0D0*GAMMAVC2))*(1.0D0-EXPON)
      PHI1=PHI1+((FST1*GAMMAVC2+GAMST1*FVC2- &
          FVC2*BETA1*(GAMST1-GAMMAVC2))/(2.0D0*GAMMAVC2**2))* &
          (1.0D0-((GAMMAVC2/V2)+1.0D0)*EXPON)-(((GAMST1-GAMMAVC2)* &
          FVC2)/(2.0D0*GAMMAVC2**2))*(2.-(GAMMAVC2**2/V4+ &
          2.0D0*GAMMAVC2/V2+2.0D0)*EXPON)
!--
!---- G for ideal gas
!--
      H0=-241816.00D0
      S0=188.7200D0
      K1=115.45000D0
      K4=-3799.900D0
      K3=-2871300.00D0
      K6=51055.16D0
      K2=-0.002062D0
      CPDT=K1*(T-T0)+K2*(TT-TT0)/2.0D0 &
      -K3*(1.0D0/T-1.0D0/T0)+2D0*K4*(SQT-SQT0) &
      +K6*DLOG(T/T0)
      CPTDT=K1*DLOG(T/T0)+K2*(T-T0) &
      -K3*(1.0D0/(TT)-1.0D0/(TT0))/2.0D0 &
      -2D0*K4*(1.0D0/SQT-1.0D0/SQT0) &
      -K6*(1.0D0/T-1.0D0/T0)
      G=H0+CPDT-T*(S0+CPTDT)
!--
!---- correct for fugacity
!---
      FUGCF=DEXP(PHI1)
      G=G+RT*100.0D0*DLOG(FUGCF*P)
      VOLUM=V*100.0D0
!---
      RETURN
      END
!
!--------------------------------
!--------------------------------
      SUBROUTINE DUAN06H2OLO(T,P,G,VOLUM,FUGCF)
      IMPLICIT NONE
!--
      REAL(8) BVC,CVC2,DVC4,EVC5,FVC2,BETA1,GAMMAVC2,R,RT,MWT, &
      BST1,CST1,DST1,EST1,FST1,BESTST1,GAMST1, &
      BST2,CST2,DST2,EST2,FST2,BESTST2,GAMST2
      COMMON /DTHINGS/ BVC,CVC2,DVC4,EVC5,FVC2,BETA1,GAMMAVC2,R,RT, &
      MWT, &
      BST1,CST1,DST1,EST1,FST1,BESTST1,GAMST1, &
      BST2,CST2,DST2,EST2,FST2,BESTST2,GAMST2
!---
      REAL(8) TR,TR2,TR3,VC,VC2,VC4,VC5,EU3,T,P,Z,V,INTE, &
      V2,V4,V5,EXPON,PHI1,G,H0,S0,K1,K2,K3,K4,K6, &
      CPDT,CPTDT,T0,TT,TT0,SQT,SQT0,FUGCF,VOLUM
!--
      T0=298.15D0
      TT=T*T
      TT0=T0*T0
      SQT=DSQRT(T)
      SQT0=DSQRT(T0)
!---
      R=0.08314467D0
      RT=R*T
      EU3=1.0D0/3.0D0
!      PR=P/221.19D0
      TR=T/647.25D0
      TR2=TR*TR
      TR3=TR2*TR
      VC=R*647.25D0/221.19D0
      VC2=VC*VC
      VC4=VC2*VC2
      VC5=VC4*VC
      BVC=(4.38269941D-02-1.68244362D-01/TR2-2.36923373D-01/TR3)*VC
      CVC2=(1.13027462D-02-7.67764181D-02/TR2+9.71820593D-02/TR3)*VC2
      DVC4=(6.62674916D-05+1.06637349D-03/TR2-1.23265258D-03/TR3)*VC4
      EVC5=(-8.93953948D-06-3.88124606D-05/TR2+5.61510206D-05/TR3)*VC5
      FVC2=(7.51274488D-03/TR3)*VC2
      BETA1=2.51598931D+00
      GAMMAVC2=3.94000000D-02*VC2
      MWT=0.0180154D0
      BST1=2.0D0*BVC
      CST1=3.0D0*CVC2
      DST1=5.0D0*DVC4
      EST1=6.0D0*EVC5
      FST1=2.0D0*FVC2
      BESTST1=BETA1
      GAMST1=3.0D0*GAMMAVC2
      BST2=0.0D0
      CST2=0.0D0
      DST2=0.0D0
      EST2=0.0D0
      FST2=0.0D0
      BESTST2=0.0D0
      GAMST2=0.0D0
!---- find the volume
!--
      CALL DUANZ(T,P,Z,V,INTE)
!--
!---- EQ9 for H2O
!--
      V2=V*V
      V4=V2*V2
      V5=V4*V
      EXPON=DEXP(-GAMMAVC2/V2)
      PHI1=-DLOG(Z)+BST1/V+CST1/(2.0D0*V2)+ &
          DST1/(4.0D0*V4)+EST1/(5.0D0*V5)
      PHI1=PHI1+((FST1*BETA1+BESTST1*FVC2)/ &
          (2.0D0*GAMMAVC2))*(1.0D0-EXPON)
      PHI1=PHI1+((FST1*GAMMAVC2+GAMST1*FVC2- &
          FVC2*BETA1*(GAMST1-GAMMAVC2))/(2.0D0*GAMMAVC2**2))* &
          (1.0D0-((GAMMAVC2/V2)+1.0D0)*EXPON)-(((GAMST1-GAMMAVC2)* &
          FVC2)/(2.0D0*GAMMAVC2**2))*(2.-(GAMMAVC2**2/V4+ &
          2.0D0*GAMMAVC2/V2+2.0D0)*EXPON)
!--
!---- G for ideal gas
!--
      H0=-241816.00D0
      S0=188.7200D0
      K1=115.45000D0
      K4=-3799.900D0
      K3=-2871300.00D0
      K6=51055.16D0
      K2=-0.002062D0
      CPDT=K1*(T-T0)+K2*(TT-TT0)/2.0D0 &
      -K3*(1.0D0/T-1.0D0/T0)+2D0*K4*(SQT-SQT0) &
      +K6*DLOG(T/T0)
      CPTDT=K1*DLOG(T/T0)+K2*(T-T0) &
      -K3*(1.0D0/(TT)-1.0D0/(TT0))/2.0D0 &
      -2D0*K4*(1.0D0/SQT-1.0D0/SQT0) &
      -K6*(1.0D0/T-1.0D0/T0)
      G=H0+CPDT-T*(S0+CPTDT)
!--
!---- correct for fugacity
!---
      FUGCF=DEXP(PHI1)
      G=G+RT*100.0D0*DLOG(FUGCF*P)
      VOLUM=V*100.0D0
!---
      RETURN
      END
!
!--------------------------------
!--------------------------------
      SUBROUTINE DUAN06CO2HI(T,P,G,VOLUM,FUGCF)
      IMPLICIT NONE
!--
      REAL(8) BVC,CVC2,DVC4,EVC5,FVC2,BETA1,GAMMAVC2,R,RT,MWT, &
      BST1,CST1,DST1,EST1,FST1,BESTST1,GAMST1, &
      BST2,CST2,DST2,EST2,FST2,BESTST2,GAMST2
      COMMON /DTHINGS/ BVC,CVC2,DVC4,EVC5,FVC2,BETA1,GAMMAVC2,R,RT, &
      MWT, &
      BST1,CST1,DST1,EST1,FST1,BESTST1,GAMST1, &
      BST2,CST2,DST2,EST2,FST2,BESTST2,GAMST2
!---
      REAL(8) TR,TR2,TR3,VC,VC2,VC4,VC5,EU3,T,P,Z,V,INTE, &
      V2,V4,V5,EXPON,PHI2,G,H0,S0,K1,K2,K3,K4,K6, &
      CPDT,CPTDT,T0,TT,TT0,SQT,SQT0,FUGCF,VOLUM
!--
      T0=298.15D0
      TT=T*T
      TT0=T0*T0
      SQT=DSQRT(T)
      SQT0=DSQRT(T0)
!---
      R=0.08314467D0
      RT=R*T
      EU3=1.0D0/3.0D0
!      PR=P/221.19D0
      TR=T/304.1282D0
      TR2=TR*TR
      TR3=TR2*TR
      VC=R*304.1282D0/73.773D0
      VC2=VC*VC
      VC4=VC2*VC2
      VC5=VC4*VC
      BVC=(5.72573440D-03+7.94836769D+00/TR2-3.84236281D+01/TR3)*VC
      CVC2=(3.71600369D-02-1.92888994D+00/TR2+6.64254770D+00/TR3)*VC2
      DVC4=(-7.02203950D-06+1.77093234D-02/TR2-4.81892026D-02/TR3)*VC4
      EVC5=(3.88344869D-06-5.54833167D-04/TR2+1.70489748D-03/TR3)*VC5
      FVC2=(-4.13039220D-01/TR3)*VC2
      BETA1=-8.47988634D+00
      GAMMAVC2=2.80000000D-02*VC2
      MWT=0.0440098D0
      BST2=2.0D0*BVC
      CST2=3.0D0*CVC2
      DST2=5.0D0*DVC4
      EST2=6.0D0*EVC5
      FST2=2.0D0*FVC2
      BESTST2=BETA1
      GAMST2=3.0D0*GAMMAVC2
      BST1=0.0D0
      CST1=0.0D0
      DST1=0.0D0
      EST1=0.0D0
      FST1=0.0D0
      BESTST1=0.0D0
      GAMST1=0.0D0
!---- find the volume
!--
      CALL DUANZ(T,P,Z,V,INTE)
!--
!---- EQ9 for CO2
!--
      V2=V*V
      V4=V2*V2
      V5=V4*V
      EXPON=DEXP(-GAMMAVC2/V2)
      PHI2=-DLOG(Z)+BST2/V+CST2/(2.0D0*V2)+ &
          DST2/(4.0D0*V4)+EST2/(5.0D0*V5)
      PHI2=PHI2+((FST2*BETA1+BESTST2*FVC2)/ &
          (2.0D0*GAMMAVC2))*(1.0D0-EXPON)
      PHI2=PHI2+((FST2*GAMMAVC2+GAMST2*FVC2- &
          FVC2*BETA1*(GAMST2-GAMMAVC2))/(2.0D0*GAMMAVC2**2))* &
          (1.0D0-((GAMMAVC2/V2)+1.0D0)*EXPON)-(((GAMST2-GAMMAVC2)* &
          FVC2)/(2.0D0*GAMMAVC2**2))*(2.-(GAMMAVC2**2/V4+ &
          2.0D0*GAMMAVC2/V2+2.0D0)*EXPON)
!--
!---- G for ideal gas
!--
      H0=-393510.010
      S0=213.6770D0
      K1=93.0000D0
      K4=-1340.900D0
      K3=123800.000D0
      K6=6336.20D0
      K2=-0.002876D0
      CPDT=K1*(T-T0)+K2*(TT-TT0)/2.0D0 &
      -K3*(1.0D0/T-1.0D0/T0)+2D0*K4*(SQT-SQT0) &
      +K6*DLOG(T/T0)
      CPTDT=K1*DLOG(T/T0)+K2*(T-T0) &
      -K3*(1.0D0/(TT)-1.0D0/(TT0))/2.0D0 &
      -2D0*K4*(1.0D0/SQT-1.0D0/SQT0) &
      -K6*(1.0D0/T-1.0D0/T0)
      G=H0+CPDT-T*(S0+CPTDT)
!--
!---- correct for fugacity
!---
      FUGCF=DEXP(PHI2)
      G=G+RT*100.0D0*DLOG(FUGCF*P)
      VOLUM=V*100.0D0
!---
      RETURN
      END
!
!--------------------------------
!--------------------------------
      SUBROUTINE DUAN06CO2LO(T,P,G,VOLUM,FUGCF)
      IMPLICIT NONE
!--
      REAL(8) BVC,CVC2,DVC4,EVC5,FVC2,BETA1,GAMMAVC2,R,RT,MWT, &
      BST1,CST1,DST1,EST1,FST1,BESTST1,GAMST1, &
      BST2,CST2,DST2,EST2,FST2,BESTST2,GAMST2
      COMMON /DTHINGS/ BVC,CVC2,DVC4,EVC5,FVC2,BETA1,GAMMAVC2,R,RT, &
      MWT, &
      BST1,CST1,DST1,EST1,FST1,BESTST1,GAMST1, &
      BST2,CST2,DST2,EST2,FST2,BESTST2,GAMST2
!---
      REAL(8) TR,TR2,TR3,VC,VC2,VC4,VC5,EU3,T,P,Z,V,INTE, &
      V2,V4,V5,EXPON,PHI2,G,H0,S0,K1,K2,K3,K4,K6, &
      CPDT,CPTDT,T0,TT,TT0,SQT,SQT0,FUGCF,VOLUM
!--
      T0=298.15D0
      TT=T*T
      TT0=T0*T0
      SQT=DSQRT(T)
      SQT0=DSQRT(T0)
!---
      R=0.08314467D0
      RT=R*T
      EU3=1.0D0/3.0D0
!      PR=P/221.19D0
      TR=T/304.1282D0
      TR2=TR*TR
      TR3=TR2*TR
      VC=R*304.1282D0/73.773D0
      VC2=VC*VC
      VC4=VC2*VC2
      VC5=VC4*VC
      BVC=(1.14400435D-01-9.38526684D-01/TR2+7.21857006D-01/TR3)*VC
      CVC2=(8.81072902D-03+6.36473911D-02/TR2-7.70822213D-02/TR3)*VC2
      DVC4=(9.01506064D-04-6.81834166D-03/TR2+7.32364258D-03/TR3)*VC4
      EVC5=(-1.10288237D-04+1.26524193D-03/TR2-1.49730823D-03/TR3)*VC5
      FVC2=(7.81940730D-03/TR3)*VC2
      BETA1=-4.22918013D+00
      GAMMAVC2=1.58500000D-01*VC2
      MWT=0.0440098D0
      BST2=2.0D0*BVC
      CST2=3.0D0*CVC2
      DST2=5.0D0*DVC4
      EST2=6.0D0*EVC5
      FST2=2.0D0*FVC2
      BESTST2=BETA1
      GAMST2=3.0D0*GAMMAVC2
      BST1=0.0D0
      CST1=0.0D0
      DST1=0.0D0
      EST1=0.0D0
      FST1=0.0D0
      BESTST1=0.0D0
      GAMST1=0.0D0
!---- find the volume
!--
      CALL DUANZ(T,P,Z,V,INTE)
!--
!---- EQ9 for CO2
!--
      V2=V*V
      V4=V2*V2
      V5=V4*V
      EXPON=DEXP(-GAMMAVC2/V2)
      PHI2=-DLOG(Z)+BST2/V+CST2/(2.0D0*V2)+ &
          DST2/(4.0D0*V4)+EST2/(5.0D0*V5)
      PHI2=PHI2+((FST2*BETA1+BESTST2*FVC2)/ &
          (2.0D0*GAMMAVC2))*(1.0D0-EXPON)
      PHI2=PHI2+((FST2*GAMMAVC2+GAMST2*FVC2- &
          FVC2*BETA1*(GAMST2-GAMMAVC2))/(2.0D0*GAMMAVC2**2))* &
          (1.0D0-((GAMMAVC2/V2)+1.0D0)*EXPON)-(((GAMST2-GAMMAVC2)* &
          FVC2)/(2.0D0*GAMMAVC2**2))*(2.-(GAMMAVC2**2/V4+ &
          2.0D0*GAMMAVC2/V2+2.0D0)*EXPON)
!--
!---- G for ideal gas
!--
      H0=-393510.010
      S0=213.6770D0
      K1=93.0000D0
      K4=-1340.900D0
      K3=123800.000D0
      K6=6336.20D0
      K2=-0.002876D0
      CPDT=K1*(T-T0)+K2*(TT-TT0)/2.0D0 &
      -K3*(1.0D0/T-1.0D0/T0)+2D0*K4*(SQT-SQT0) &
      +K6*DLOG(T/T0)
      CPTDT=K1*DLOG(T/T0)+K2*(T-T0) &
      -K3*(1.0D0/(TT)-1.0D0/(TT0))/2.0D0 &
      -2D0*K4*(1.0D0/SQT-1.0D0/SQT0) &
      -K6*(1.0D0/T-1.0D0/T0)
      G=H0+CPDT-T*(S0+CPTDT)
!--
!---- correct for fugacity
!---
      FUGCF=DEXP(PHI2)
      G=G+RT*100.0D0*DLOG(FUGCF*P)
      VOLUM=V*100.0D0
!---
      RETURN
      END
!
!--------------------------------
!--------------------------------
      SUBROUTINE MIXRULESHI(T,P,XF)
      IMPLICIT NONE
!--
      REAL(8) BVC,CVC2,DVC4,EVC5,FVC2,BETA1,GAMMAVC2,R,RT,MWT, &
      BST1,CST1,DST1,EST1,FST1,BESTST1,GAMST1, &
      BST2,CST2,DST2,EST2,FST2,BESTST2,GAMST2
      COMMON /DTHINGS/ BVC,CVC2,DVC4,EVC5,FVC2,BETA1,GAMMAVC2,R,RT, &
      MWT, &
      BST1,CST1,DST1,EST1,FST1,BESTST1,GAMST1, &
      BST2,CST2,DST2,EST2,FST2,BESTST2,GAMST2
!--
!---- 1=H2O, 2=CO2, 3=CH4
      REAL(8) A1(2),A2(2),A3(2),A4(2),A5(2),A6(2),A7(2),A8(2),A9(2), &
      A10(2),A11(2),A12(2),ALPHA(2),BETA(2),GAMMA(2),TC(2),PC(2), &
      BB(2),CC(2),DD(2),EE(2),FF(2), &
      BS(2),CS(2),DS(2),ES(2),FS(2), &
      PM(2), &
      VC(2),VC2(2),VC4(2),VC5(2), &
      TR(2),TR2(2),TR3(2),PR(2),P,T,XF(2),EU3
      INTEGER(4) I,J,K,II,L,M,N
      REAL(8) K1(2,2),K2(2,2,2),K3(2,2,2), &
      BIJ(2,2),VCIJ(2,2),CIJK(2,2,2),VCIJK(2,2,2), &
      DIJKLM(2,2,2,2,2),VCIJKLM(2,2,2,2,2), &
      EIJKLMN(2,2,2,2,2,2),VCIJKLMN(2,2,2,2,2,2), &
      FIJ(2,2),GAMIJK(2,2,2)
!-----
      DATA A1  / 4.68071541D-02, 5.72573440D-03/
      DATA A2  /-2.81275941D-01, 7.94836769D+00/
      DATA A3  /-2.43926365D-01,-3.84236281D+01/
      DATA A4  / 1.10016958D-02, 3.71600369D-02/
      DATA A5  /-3.86603525D-02,-1.92888994D+00/
      DATA A6  / 9.30095461D-02, 6.64254770D+00/
      DATA A7  /-1.15747171D-05,-7.02203950D-06/
      DATA A8  / 4.19873848D-04, 1.77093234D-02/
      DATA A9  /-5.82739501D-04,-4.81892026D-02/
      DATA A10 / 1.00936000D-06, 3.88344869D-06/
      DATA A11 /-1.01713593D-05,-5.54833167D-04/
      DATA A12 / 1.63934213D-05, 1.70489748D-03/
!-
      DATA ALPHA /-4.49505919D-02,-4.13039220D-01/
      DATA BETA  /-3.15028174D-01,-8.47988634D+00/
      DATA GAMMA / 1.25000000D-02, 2.80000000D-02/
!-
      DATA TC /647.25D0,304.1282D0/
      DATA PC /221.19D0,73.773D0/
      DATA PM /0.0180154D0,0.0440098D0/
!-----
      R=0.08314467D0
      RT=R*T
      EU3=1.0D0/3.0D0
      II=1
!-----
      DO I=1,2
       PR(I)=P/PC(I)
       TR(I)=T/TC(I)
       TR2(I)=TR(I)*TR(I)
       TR3(I)=TR2(I)*TR(I)
       VC(I)=R*TC(I)/PC(I)
       VC2(I)=VC(I)*VC(I)
       VC4(I)=VC2(I)*VC2(I)
       VC5(I)=VC4(I)*VC(I)
!
       BB(I)=A1(I)+A2(I)/TR2(I)+A3(I)/TR3(I)
       CC(I)=A4(I)+A5(I)/TR2(I)+A6(I)/TR3(I)
       DD(I)=A7(I)+A8(I)/TR2(I)+A9(I)/TR3(I)
       EE(I)=A10(I)+A11(I)/TR2(I)+A12(I)/TR3(I)
       FF(I)=ALPHA(I)/TR3(I)
!-----
        IF (BB(I).LT.0.0D0) THEN
         BB(I)=-BB(I)
         BS(I)=-1.0D0
        ELSE
         BS(I)=1.0D0
        END IF
        IF (CC(I).LT.0.0D0) THEN
         CC(I)=-CC(I)
         CS(I)=-1.0D0
        ELSE
         CS(I)=1.0D0
        END IF
        IF (DD(I).LT.0.0D0) THEN
         DD(I)=-DD(I)
         DS(I)=-1.0D0
        ELSE
         DS(I)=1.0D0
        END IF
        IF (EE(I).LT.0.0D0) THEN
         EE(I)=-EE(I)
         ES(I)=-1.0D0
        ELSE
         ES(I)=1.0D0
        END IF
        IF (FF(I).LT.0.0D0) THEN
         FF(I)=-FF(I)
         FS(I)=-1.0D0
        ELSE
         FS(I)=1.0D0
        END IF
!-----
      END DO
!-----
!-----
!-----
      CALL MAKEK1HI(T,K1,K2,K3)
!-----
      DO I=1,2
       DO J=1,2
      BIJ(I,J)=(((BS(I)*BB(I)**EU3+BS(J)*BB(J)**EU3) &
               /2.0D0)**3)*K1(I,J)
      VCIJ(I,J)=((VC(I)**EU3+VC(J)**EU3)/2.0D0)**3
       END DO
      END DO
      BVC=0.0D0
      DO I=1,2
       DO J=1,2
      BVC=BVC+XF(I)*XF(J)*BIJ(I,J)*VCIJ(I,J)
       END DO
      END DO
      BST1=0.0D0
      BST2=0.0D0
      DO J=1,2
       BST1=BST1+2.0D0*XF(J)*BIJ(1,J)*VCIJ(1,J)
       BST2=BST2+2.0D0*XF(J)*BIJ(J,2)*VCIJ(J,2)
      END DO
!---
      DO I=1,2
       DO J=1,2
        DO K=1,2
      CIJK(I,J,K)=(((CS(I)*CC(I)**EU3+CS(J)*CC(J)**EU3+CC(K)**EU3) &
                  /3.0D0)**3)*K2(I,J,K)
      VCIJK(I,J,K)=((VC(I)**EU3+VC(J)**EU3+VC(K)**EU3)/3.0D0)**3
        END DO
       END DO
      END DO
      CVC2=0.0D0
      DO I=1,2
       DO J=1,2
        DO K=1,2
      CVC2=CVC2+XF(I)*XF(J)*XF(K)*CIJK(I,J,K)*VCIJK(I,J,K)**2
        END DO
       END DO
      END DO
      CST1=0.0D0
      CST2=0.0D0
      DO J=1,2
       DO K=1,2
        CST1=CST1+3.0D0*XF(J)*XF(K)*CIJK(1,J,K)*VCIJK(1,J,K)**2
        CST2=CST2+3.0D0*XF(J)*XF(K)*CIJK(J,2,K)*VCIJK(J,2,K)**2
       END DO
      END DO
!---
      DO I=1,2
       DO J=1,2
        DO K=1,2
         DO L=1,2
          DO M=1,2
      DIJKLM(I,J,K,L,M)=((DS(I)*DD(I)**EU3+DS(J)*DD(J)**EU3+ &
                          DS(K)*DD(K)**EU3+DS(L)*DD(L)**EU3+ &
                          DS(M)*DD(M)**EU3)/5.0D0)**3
      VCIJKLM(I,J,K,L,M)=((VC(I)**EU3+VC(J)**EU3+VC(K)**EU3+ &
                         VC(L)**EU3+VC(M)**EU3)/5.0D0)**3
          END DO
         END DO
        END DO
       END DO
      END DO
      DVC4=0.0D0
      DO I=1,2
       DO J=1,2
        DO K=1,2
         DO L=1,2
          DO M=1,2
      DVC4=DVC4+XF(I)*XF(J)*XF(K)*XF(L)*XF(M)*DIJKLM(I,J,K,L,M)* &
           VCIJKLM(I,J,K,L,M)**4
          END DO
         END DO
        END DO
       END DO
      END DO
      DST1=0.0D0
      DST2=0.0D0
      DO J=1,2
       DO K=1,2
        DO L=1,2
         DO M=1,2
      DST1=DST1+5.0D0*XF(J)*XF(K)*XF(L)*XF(M)*DIJKLM(1,J,K,L,M)* &
          VCIJKLM(1,J,K,L,M)**4
      DST2=DST2+5.0D0*XF(J)*XF(K)*XF(L)*XF(M)*DIJKLM(J,2,K,L,M)* &
          VCIJKLM(J,2,K,L,M)**4
         END DO
        END DO
       END DO
      END DO
!---
      DO I=1,2
       DO J=1,2
        DO K=1,2
         DO L=1,2
          DO M=1,2
           DO N=1,2
      EIJKLMN(I,J,K,L,M,N)=((ES(I)*EE(I)**EU3+ES(J)*EE(J)**EU3+ &
                             ES(K)*EE(K)**EU3+ES(L)*EE(L)**EU3+ &
                             ES(M)*EE(M)**EU3+ &
                             ES(N)*EE(N)**EU3)/6.0D0)**3
      VCIJKLMN(I,J,K,L,M,N)=((VC(I)**EU3+VC(J)**EU3+ &
                              VC(K)**EU3+VC(L)**EU3+ &
                              VC(M)**EU3+VC(N)**EU3)/6.0D0)**3
           END DO
          END DO
         END DO
        END DO
       END DO
      END DO
      EVC5=0.0D0
      DO I=1,2
       DO J=1,2
        DO K=1,2
         DO L=1,2
          DO M=1,2
           DO N=1,2
      EVC5=EVC5+XF(I)*XF(J)*XF(K)*XF(L)*XF(M)*XF(N)* &
      EIJKLMN(I,J,K,L,M,N)*VCIJKLMN(I,J,K,L,M,N)**5
           END DO
          END DO
         END DO
        END DO
       END DO
      END DO
      EST1=0.0D0
      EST2=0.0D0
      DO J=1,2
       DO K=1,2
        DO L=1,2
         DO M=1,2
          DO N=1,2
     EST1=EST1+6.0D0*XF(J)*XF(K)*XF(L)*XF(M)*XF(N)* &
     EIJKLMN(1,J,K,L,M,N)*VCIJKLMN(1,J,K,L,M,N)**5
     EST2=EST2+6.0D0*XF(J)*XF(K)*XF(L)*XF(M)*XF(N)* &
     EIJKLMN(J,2,K,L,M,N)*VCIJKLMN(J,2,K,L,M,N)**5
          END DO
         END DO
        END DO
       END DO
      END DO
!---
      DO I=1,2
       DO J=1,2
      FIJ(I,J)=((FS(I)*FF(I)**EU3+FS(J)*FF(J)**EU3)/2.0D0)**3
       END DO
      END DO
      FVC2=0.0D0
      DO I=1,2
       DO J=1,2
      FVC2=FVC2+XF(I)*XF(J)*FIJ(I,J)*VCIJ(I,J)**2
       END DO
      END DO
      FST1=0.0D0
      FST2=0.0D0
      DO J=1,2
      FST1=FST1+2.0D0*XF(J)*FIJ(1,J)*VCIJ(1,J)**2
      FST2=FST2+2.0D0*XF(J)*FIJ(J,2)*VCIJ(J,2)**2
      END DO
!---
      BETA1=0.0D0
      DO I=1,2
      BETA1=BETA1+XF(I)*BETA(I)
      END DO
      BESTST1=BETA(1)
      BESTST2=BETA(2)
!---
      DO I=1,2
       DO J=1,2
        DO K=1,2
      GAMIJK(I,J,K)=(((GAMMA(I)**EU3+GAMMA(J)**EU3+ &
      GAMMA(K)**EU3)/3.0D0)**3)*K3(I,J,K)
        END DO
       END DO
      END DO
      GAMMAVC2=0.0D0
      DO I=1,2
       DO J=1,2
        DO K=1,2
        GAMMAVC2=GAMMAVC2+XF(I)*XF(J)*XF(K)*GAMIJK(I,J,K)* &
        VCIJK(I,J,K)**2
        END DO
       END DO
      END DO
      GAMST1=0.0D0
      GAMST2=0.0D0
      DO J=1,2
      DO K=1,2
       GAMST1=GAMST1+3.0D0*XF(J)*XF(K)*GAMIJK(1,J,K)* &
       VCIJK(1,J,K)**2
       GAMST2=GAMST2+3.0D0*XF(J)*XF(K)*GAMIJK(J,2,K)* &
       VCIJK(J,2,K)**2
       END DO
      END DO
!-----
!-----
      MWT=XF(1)*PM(1)+XF(2)*PM(2)
!-----
!-----
!-----
      RETURN
      END
!--------------------------------
!--------------------------------
      SUBROUTINE MAKEK1HI(T,K1,K2,K3)
      IMPLICIT NONE
!-----
      REAL(8) T,K1(2,2),K2(2,2,2),K3(2,2,2),T2
      INTEGER(4) I,J,K
!-----
!--- bis jetzt nur index 1 und 2
!---- 1=H2O, 2=CO2, 3=CH4
      DO I=1,2
       DO J=1,2
        K1(I,J)=0.0D0
       END DO
      END DO
      DO I=1,2
       DO J=1,2
        DO K=1,2
         K2(I,J,K)=0.0D0
         K3(I,J,K)=0.0D0
        END DO
       END DO
      END DO
      T2=T*T
      K1(1,1)=1.0D0
      K1(2,2)=1.0D0
      K2(1,1,1)=1.0D0
      K2(2,2,2)=1.0D0
      K3(1,1,1)=1.0D0
      K3(2,2,2)=1.0D0
!--
!----- binary H2O-CO2
!--
      IF (T.LE.373.15) THEN
      K1(1,2)=0.20611+0.0006*T
      K1(2,1)=K1(1,2)
      K2(1,1,2)=0.8023278-0.0022206*T+184.76824/T
      K2(1,2,1)=K2(1,1,2)
      K2(2,1,1)=K2(1,1,2)
      K2(1,2,2)=K2(1,1,2)
      K2(2,1,2)=K2(1,1,2)
      K2(2,2,1)=K2(1,1,2)
      K3(1,1,2)=1.80544-0.0032605*T
      K3(1,2,1)=K3(1,1,2)
      K3(2,1,1)=K3(1,1,2)
      K3(1,2,2)=K3(1,1,2)
      K3(2,1,2)=K3(1,1,2)
      K3(2,2,1)=K3(1,1,2)
      END IF
!--
      IF (T.GT.373.15.AND.T.LE.495.15) THEN
      K1(1,2)=-10084.5042-4.27134485*T+256477.783/T+ &
            0.00166997474*T2+1816.78*DLOG(T)
      K1(2,1)=K1(1,2)
      K2(1,1,2)=9.000263-0.00623494*T-2307.7125/T
      K2(1,2,1)=K2(1,1,2)
      K2(2,1,1)=K2(1,1,2)
      K2(1,2,2)=K2(1,1,2)
      K2(2,1,2)=K2(1,1,2)
      K2(2,2,1)=K2(1,1,2)
      K3(1,1,2)=-74.1163+0.1800496*T-1.40904946E-04*T2+ &
            10130.5246/T
      K3(1,2,1)=K3(1,1,2)
      K3(2,1,1)=K3(1,1,2)
      K3(1,2,2)=K3(1,1,2)
      K3(2,1,2)=K3(1,1,2)
      K3(2,2,1)=K3(1,1,2)
      END IF
!--
      IF (T.GT.495.15.AND.T.LE.623.15) THEN
      K1(1,2)=-0.3568+7.8888E-04*T+333.399/T
      K1(2,1)=K1(1,2)
      K2(1,1,2)=-19.97444+0.0192515*T+5707.4229/T
      K2(1,2,1)=K2(1,1,2)
      K2(2,1,1)=K2(1,1,2)
      K2(1,2,2)=K2(1,1,2)
      K2(2,1,2)=K2(1,1,2)
      K2(2,2,1)=K2(1,1,2)
      K3(1,1,2)=12.1308-0.0099489*T-3042.09583/T
      K3(1,2,1)=K3(1,1,2)
      K3(2,1,1)=K3(1,1,2)
      K3(1,2,2)=K3(1,1,2)
      K3(2,1,2)=K3(1,1,2)
      K3(2,2,1)=K3(1,1,2)
      END IF
!--
      IF (T.GT.623.15.AND.T.LE.672.15) THEN
      K1(1,2)=-4.53122+0.0042113*T+1619.7/T
      K1(2,1)=K1(1,2)
      K2(1,1,2)=-163.4855+0.190552*T-7.228514E-05*T2+ &
            46082.885/T
      K2(1,2,1)=K2(1,1,2)
      K2(2,1,1)=K2(1,1,2)
      K2(1,2,2)=K2(1,1,2)
      K2(2,1,2)=K2(1,1,2)
      K2(2,2,1)=K2(1,1,2)
      K3(1,1,2)=1.7137-6.7136E-04*T
      K3(1,2,1)=K3(1,1,2)
      K3(2,1,1)=K3(1,1,2)
      K3(1,2,2)=K3(1,1,2)
      K3(2,1,2)=K3(1,1,2)
      K3(2,2,1)=K3(1,1,2)
      END IF
!--
!!!!      IF (T.GT.672.15) THEN
      K1(1,2)=9.034D0-7.9212D-3*T+2.3285D-6*T2-2.4221D+03/T
      K1(2,1)=K1(1,2)
      K2(1,1,2)=-1.068D0+1.8756D-3*T-4.9371D-7*T2+6.6180D2/T
      K2(1,2,1)=K2(1,1,2)
      K2(2,1,1)=K2(1,1,2)
      K2(1,2,2)=K2(1,1,2)
      K2(2,1,2)=K2(1,1,2)
      K2(2,2,1)=K2(1,1,2)
      K3(1,1,2)=1.0D0
      K3(1,2,1)=K3(1,1,2)
      K3(2,1,1)=K3(1,1,2)
      K3(1,2,2)=K3(1,1,2)
      K3(2,1,2)=K3(1,1,2)
      K3(2,2,1)=K3(1,1,2)
!!!!      END IF
!-----
      RETURN
      END
!--------------------------------
!--------------------------------
      SUBROUTINE MIXRULESLO(T,P,XF)
      IMPLICIT NONE
!--
      REAL(8) BVC,CVC2,DVC4,EVC5,FVC2,BETA1,GAMMAVC2,R,RT,MWT, &
      BST1,CST1,DST1,EST1,FST1,BESTST1,GAMST1, &
      BST2,CST2,DST2,EST2,FST2,BESTST2,GAMST2
      COMMON /DTHINGS/ BVC,CVC2,DVC4,EVC5,FVC2,BETA1,GAMMAVC2,R,RT, &
      MWT, &
      BST1,CST1,DST1,EST1,FST1,BESTST1,GAMST1, &
      BST2,CST2,DST2,EST2,FST2,BESTST2,GAMST2
!--
!---- 1=H2O, 2=CO2, 3=CH4
      REAL(8) A1(2),A2(2),A3(2),A4(2),A5(2),A6(2),A7(2),A8(2),A9(2), &
      A10(2),A11(2),A12(2),ALPHA(2),BETA(2),GAMMA(2),TC(2),PC(2), &
      BB(2),CC(2),DD(2),EE(2),FF(2), &
      BS(2),CS(2),DS(2),ES(2),FS(2), &
      PM(2), &
      VC(2),VC2(2),VC4(2),VC5(2), &
      TR(2),TR2(2),TR3(2),PR(2),P,T,XF(2),EU3
      INTEGER(4) I,J,K,II,L,M,N
      REAL(8) K1(2,2),K2(2,2,2),K3(2,2,2), &
      BIJ(2,2),VCIJ(2,2),CIJK(2,2,2),VCIJK(2,2,2), &
      DIJKLM(2,2,2,2,2),VCIJKLM(2,2,2,2,2), &
      EIJKLMN(2,2,2,2,2,2),VCIJKLMN(2,2,2,2,2,2), &
      FIJ(2,2),GAMIJK(2,2,2)
!-----
      DATA A1  / 4.38269941D-02, 1.14400435D-01/
      DATA A2  /-1.68244362D-01,-9.38526684D-01/
      DATA A3  /-2.36923373D-01, 7.21857006D-01/
      DATA A4  / 1.13027462D-02, 8.81072902D-03/
      DATA A5  /-7.67764181D-02, 6.36473911D-02/
      DATA A6  / 9.71820593D-02,-7.70822213D-02/
      DATA A7  / 6.62674916D-05, 9.01506064D-04/
      DATA A8  / 1.06637349D-03,-6.81834166D-03/
      DATA A9  /-1.23265258D-03, 7.32364258D-03/
      DATA A10 /-8.93953948D-06,-1.10288237D-04/
      DATA A11 /-3.88124606D-05, 1.26524193D-03/
      DATA A12 / 5.61510206D-05,-1.49730823D-03/
!-
      DATA ALPHA / 7.51274488D-03, 7.81940730D-03/
      DATA BETA  / 2.51598931D+00,-4.22918013D+00/
      DATA GAMMA / 3.94000000D-02, 1.58500000D-01/
!-
      DATA TC /647.25D0,304.1282D0/
      DATA PC /221.19D0,73.773D0/
      DATA PM /0.0180154D0,0.0440098D0/
!-----
      R=0.08314467D0
      RT=R*T
      EU3=1.0D0/3.0D0
      II=1
!-----
      DO I=1,2
       PR(I)=P/PC(I)
       TR(I)=T/TC(I)
       TR2(I)=TR(I)*TR(I)
       TR3(I)=TR2(I)*TR(I)
       VC(I)=R*TC(I)/PC(I)
       VC2(I)=VC(I)*VC(I)
       VC4(I)=VC2(I)*VC2(I)
       VC5(I)=VC4(I)*VC(I)
!
       BB(I)=A1(I)+A2(I)/TR2(I)+A3(I)/TR3(I)
       CC(I)=A4(I)+A5(I)/TR2(I)+A6(I)/TR3(I)
       DD(I)=A7(I)+A8(I)/TR2(I)+A9(I)/TR3(I)
       EE(I)=A10(I)+A11(I)/TR2(I)+A12(I)/TR3(I)
       FF(I)=ALPHA(I)/TR3(I)
!-----
        IF (BB(I).LT.0.0D0) THEN
         BB(I)=-BB(I)
         BS(I)=-1.0D0
        ELSE
         BS(I)=1.0D0
        END IF
        IF (CC(I).LT.0.0D0) THEN
         CC(I)=-CC(I)
         CS(I)=-1.0D0
        ELSE
         CS(I)=1.0D0
        END IF
        IF (DD(I).LT.0.0D0) THEN
         DD(I)=-DD(I)
         DS(I)=-1.0D0
        ELSE
         DS(I)=1.0D0
        END IF
        IF (EE(I).LT.0.0D0) THEN
         EE(I)=-EE(I)
         ES(I)=-1.0D0
        ELSE
         ES(I)=1.0D0
        END IF
        IF (FF(I).LT.0.0D0) THEN
         FF(I)=-FF(I)
         FS(I)=-1.0D0
        ELSE
         FS(I)=1.0D0
        END IF
!-----
      END DO
!-----
!-----
!-----
      CALL MAKEK1LO(T,K1,K2,K3)
!-----
      DO I=1,2
       DO J=1,2
      BIJ(I,J)=(((BS(I)*BB(I)**EU3+BS(J)*BB(J)**EU3) &
               /2.0D0)**3)*K1(I,J)
      VCIJ(I,J)=((VC(I)**EU3+VC(J)**EU3)/2.0D0)**3
       END DO
      END DO
      BVC=0.0D0
      DO I=1,2
       DO J=1,2
      BVC=BVC+XF(I)*XF(J)*BIJ(I,J)*VCIJ(I,J)
       END DO
      END DO
      BST1=0.0D0
      BST2=0.0D0
      DO J=1,2
       BST1=BST1+2.0D0*XF(J)*BIJ(1,J)*VCIJ(1,J)
       BST2=BST2+2.0D0*XF(J)*BIJ(J,2)*VCIJ(J,2)
      END DO
!---
      DO I=1,2
       DO J=1,2
        DO K=1,2
      CIJK(I,J,K)=(((CS(I)*CC(I)**EU3+CS(J)*CC(J)**EU3+CC(K)**EU3) &
                  /3.0D0)**3)*K2(I,J,K)
      VCIJK(I,J,K)=((VC(I)**EU3+VC(J)**EU3+VC(K)**EU3)/3.0D0)**3
        END DO
       END DO
      END DO
      CVC2=0.0D0
      DO I=1,2
       DO J=1,2
        DO K=1,2
      CVC2=CVC2+XF(I)*XF(J)*XF(K)*CIJK(I,J,K)*VCIJK(I,J,K)**2
        END DO
       END DO
      END DO
      CST1=0.0D0
      CST2=0.0D0
      DO J=1,2
       DO K=1,2
        CST1=CST1+3.0D0*XF(J)*XF(K)*CIJK(1,J,K)*VCIJK(1,J,K)**2
        CST2=CST2+3.0D0*XF(J)*XF(K)*CIJK(J,2,K)*VCIJK(J,2,K)**2
       END DO
      END DO
!---
      DO I=1,2
       DO J=1,2
        DO K=1,2
         DO L=1,2
          DO M=1,2
      DIJKLM(I,J,K,L,M)=((DS(I)*DD(I)**EU3+DS(J)*DD(J)**EU3+ &
                          DS(K)*DD(K)**EU3+DS(L)*DD(L)**EU3+ &
                          DS(M)*DD(M)**EU3)/5.0D0)**3
      VCIJKLM(I,J,K,L,M)=((VC(I)**EU3+VC(J)**EU3+VC(K)**EU3+ &
                         VC(L)**EU3+VC(M)**EU3)/5.0D0)**3
          END DO
         END DO
        END DO
       END DO
      END DO
      DVC4=0.0D0
      DO I=1,2
       DO J=1,2
        DO K=1,2
         DO L=1,2
          DO M=1,2
      DVC4=DVC4+XF(I)*XF(J)*XF(K)*XF(L)*XF(M)*DIJKLM(I,J,K,L,M)* &
           VCIJKLM(I,J,K,L,M)**4
          END DO
         END DO
        END DO
       END DO
      END DO
      DST1=0.0D0
      DST2=0.0D0
      DO J=1,2
       DO K=1,2
        DO L=1,2
         DO M=1,2
      DST1=DST1+5.0D0*XF(J)*XF(K)*XF(L)*XF(M)*DIJKLM(1,J,K,L,M)* &
          VCIJKLM(1,J,K,L,M)**4
      DST2=DST2+5.0D0*XF(J)*XF(K)*XF(L)*XF(M)*DIJKLM(J,2,K,L,M)* &
          VCIJKLM(J,2,K,L,M)**4
         END DO
        END DO
       END DO
      END DO
!---
      DO I=1,2
       DO J=1,2
        DO K=1,2
         DO L=1,2
          DO M=1,2
           DO N=1,2
      EIJKLMN(I,J,K,L,M,N)=((ES(I)*EE(I)**EU3+ES(J)*EE(J)**EU3+ &
                             ES(K)*EE(K)**EU3+ES(L)*EE(L)**EU3+ &
                             ES(M)*EE(M)**EU3+ &
                             ES(N)*EE(N)**EU3)/6.0D0)**3
      VCIJKLMN(I,J,K,L,M,N)=((VC(I)**EU3+VC(J)**EU3+ &
                              VC(K)**EU3+VC(L)**EU3+ &
                              VC(M)**EU3+VC(N)**EU3)/6.0D0)**3
           END DO
          END DO
         END DO
        END DO
       END DO
      END DO
      EVC5=0.0D0
      DO I=1,2
       DO J=1,2
        DO K=1,2
         DO L=1,2
          DO M=1,2
           DO N=1,2
      EVC5=EVC5+XF(I)*XF(J)*XF(K)*XF(L)*XF(M)*XF(N)* &
      EIJKLMN(I,J,K,L,M,N)*VCIJKLMN(I,J,K,L,M,N)**5
           END DO
          END DO
         END DO
        END DO
       END DO
      END DO
      EST1=0.0D0
      EST2=0.0D0
      DO J=1,2
       DO K=1,2
        DO L=1,2
         DO M=1,2
          DO N=1,2
     EST1=EST1+6.0D0*XF(J)*XF(K)*XF(L)*XF(M)*XF(N)* &
     EIJKLMN(1,J,K,L,M,N)*VCIJKLMN(1,J,K,L,M,N)**5
     EST2=EST2+6.0D0*XF(J)*XF(K)*XF(L)*XF(M)*XF(N)* &
     EIJKLMN(J,2,K,L,M,N)*VCIJKLMN(J,2,K,L,M,N)**5
          END DO
         END DO
        END DO
       END DO
      END DO
!---
      DO I=1,2
       DO J=1,2
      FIJ(I,J)=((FS(I)*FF(I)**EU3+FS(J)*FF(J)**EU3)/2.0D0)**3
       END DO
      END DO
      FVC2=0.0D0
      DO I=1,2
       DO J=1,2
      FVC2=FVC2+XF(I)*XF(J)*FIJ(I,J)*VCIJ(I,J)**2
       END DO
      END DO
      FST1=0.0D0
      FST2=0.0D0
      DO J=1,2
      FST1=FST1+2.0D0*XF(J)*FIJ(1,J)*VCIJ(1,J)**2
      FST2=FST2+2.0D0*XF(J)*FIJ(J,2)*VCIJ(J,2)**2
      END DO
!---
      BETA1=0.0D0
      DO I=1,2
      BETA1=BETA1+XF(I)*BETA(I)
      END DO
      BESTST1=BETA(1)
      BESTST2=BETA(2)
!---
      DO I=1,2
       DO J=1,2
        DO K=1,2
      GAMIJK(I,J,K)=(((GAMMA(I)**EU3+GAMMA(J)**EU3+ &
      GAMMA(K)**EU3)/3.0D0)**3)*K3(I,J,K)
        END DO
       END DO
      END DO
      GAMMAVC2=0.0D0
      DO I=1,2
       DO J=1,2
        DO K=1,2
        GAMMAVC2=GAMMAVC2+XF(I)*XF(J)*XF(K)*GAMIJK(I,J,K)* &
        VCIJK(I,J,K)**2
        END DO
       END DO
      END DO
      GAMST1=0.0D0
      GAMST2=0.0D0
      DO J=1,2
      DO K=1,2
       GAMST1=GAMST1+3.0D0*XF(J)*XF(K)*GAMIJK(1,J,K)* &
       VCIJK(1,J,K)**2
       GAMST2=GAMST2+3.0D0*XF(J)*XF(K)*GAMIJK(J,2,K)* &
       VCIJK(J,2,K)**2
       END DO
      END DO
!-----
!-----
      MWT=XF(1)*PM(1)+XF(2)*PM(2)
!-----
!-----
!-----
      RETURN
      END
!--------------------------------
!--------------------------------
      SUBROUTINE MAKEK1LO(T,K1,K2,K3)
      IMPLICIT NONE
!-----
      REAL(8) T,K1(2,2),K2(2,2,2),K3(2,2,2),T2
      INTEGER(4) I,J,K
!-----
!--- bis jetzt nur index 1 und 2
!---- 1=H2O, 2=CO2, 3=CH4
      DO I=1,2
       DO J=1,2
        K1(I,J)=0.0D0
       END DO
      END DO
      DO I=1,2
       DO J=1,2
        DO K=1,2
         K2(I,J,K)=0.0D0
         K3(I,J,K)=0.0D0
        END DO
       END DO
      END DO
      T2=T*T
      K1(1,1)=1.0D0
      K1(2,2)=1.0D0
      K2(1,1,1)=1.0D0
      K2(2,2,2)=1.0D0
      K3(1,1,1)=1.0D0
      K3(2,2,2)=1.0D0
!--
!----- binary H2O-CO2
!--
      IF (T.LE.373.15) THEN
      K1(1,2)=0.20611+0.0006*T
      K1(2,1)=K1(1,2)
      K2(1,1,2)=0.8023278-0.0022206*T+184.76824/T
      K2(1,2,1)=K2(1,1,2)
      K2(2,1,1)=K2(1,1,2)
      K2(1,2,2)=K2(1,1,2)
      K2(2,1,2)=K2(1,1,2)
      K2(2,2,1)=K2(1,1,2)
      K3(1,1,2)=1.80544-0.0032605*T
      K3(1,2,1)=K3(1,1,2)
      K3(2,1,1)=K3(1,1,2)
      K3(1,2,2)=K3(1,1,2)
      K3(2,1,2)=K3(1,1,2)
      K3(2,2,1)=K3(1,1,2)
      END IF
!--
      IF (T.GT.373.15.AND.T.LE.495.15) THEN
      K1(1,2)=-10084.5042-4.27134485*T+256477.783/T+ &
            0.00166997474*T2+1816.78*DLOG(T)
      K1(2,1)=K1(1,2)
      K2(1,1,2)=9.000263-0.00623494*T-2307.7125/T
      K2(1,2,1)=K2(1,1,2)
      K2(2,1,1)=K2(1,1,2)
      K2(1,2,2)=K2(1,1,2)
      K2(2,1,2)=K2(1,1,2)
      K2(2,2,1)=K2(1,1,2)
      K3(1,1,2)=-74.1163+0.1800496*T-1.40904946E-04*T2+ &
            10130.5246/T
      K3(1,2,1)=K3(1,1,2)
      K3(2,1,1)=K3(1,1,2)
      K3(1,2,2)=K3(1,1,2)
      K3(2,1,2)=K3(1,1,2)
      K3(2,2,1)=K3(1,1,2)
      END IF
!--
      IF (T.GT.495.15.AND.T.LE.623.15) THEN
      K1(1,2)=-0.3568+7.8888E-04*T+333.399/T
      K1(2,1)=K1(1,2)
      K2(1,1,2)=-19.97444+0.0192515*T+5707.4229/T
      K2(1,2,1)=K2(1,1,2)
      K2(2,1,1)=K2(1,1,2)
      K2(1,2,2)=K2(1,1,2)
      K2(2,1,2)=K2(1,1,2)
      K2(2,2,1)=K2(1,1,2)
      K3(1,1,2)=12.1308-0.0099489*T-3042.09583/T
      K3(1,2,1)=K3(1,1,2)
      K3(2,1,1)=K3(1,1,2)
      K3(1,2,2)=K3(1,1,2)
      K3(2,1,2)=K3(1,1,2)
      K3(2,2,1)=K3(1,1,2)
      END IF
!--
      IF (T.GT.623.15.AND.T.LE.672.15) THEN
      K1(1,2)=-4.53122+0.0042113*T+1619.7/T
      K1(2,1)=K1(1,2)
      K2(1,1,2)=-163.4855+0.190552*T-7.228514E-05*T2+ &
            46082.885/T
      K2(1,2,1)=K2(1,1,2)
      K2(2,1,1)=K2(1,1,2)
      K2(1,2,2)=K2(1,1,2)
      K2(2,1,2)=K2(1,1,2)
      K2(2,2,1)=K2(1,1,2)
      K3(1,1,2)=1.7137-6.7136E-04*T
      K3(1,2,1)=K3(1,1,2)
      K3(2,1,1)=K3(1,1,2)
      K3(1,2,2)=K3(1,1,2)
      K3(2,1,2)=K3(1,1,2)
      K3(2,2,1)=K3(1,1,2)
      END IF
!--
!!!!      IF (T.GT.672.15) THEN
      K1(1,2)=3.131D0-5.0624D-3*T+1.8641D-6*T2-31.409D0/T
      K1(2,1)=K1(1,2)
      K2(1,1,2)=-46.646D0+4.2877D-2*T-1.0892D-5*T2+1.5782D4/T
      K2(1,2,1)=K2(1,1,2)
      K2(2,1,1)=K2(1,1,2)
      K2(1,2,2)=K2(1,1,2)
      K2(2,1,2)=K2(1,1,2)
      K2(2,2,1)=K2(1,1,2)
      K3(1,1,2)=0.9D0
      K3(1,2,1)=K3(1,1,2)
      K3(2,1,1)=K3(1,1,2)
      K3(1,2,2)=K3(1,1,2)
      K3(2,1,2)=K3(1,1,2)
      K3(2,2,1)=K3(1,1,2)
!!!!      END IF
!-----
      RETURN
      END
!--------------------------------
!--------------------------------
      SUBROUTINE DUANZ(T,P,ZST,VOLST,INTST)
      IMPLICIT NONE
!--
      REAL(8) BVC,CVC2,DVC4,EVC5,FVC2,BETA1,GAMMAVC2,R,RT,MWT, &
      BST1,CST1,DST1,EST1,FST1,BESTST1,GAMST1, &
      BST2,CST2,DST2,EST2,FST2,BESTST2,GAMST2
      COMMON /DTHINGS/ BVC,CVC2,DVC4,EVC5,FVC2,BETA1,GAMMAVC2,R,RT, &
      MWT, &
      BST1,CST1,DST1,EST1,FST1,BESTST1,GAMST1, &
      BST2,CST2,DST2,EST2,FST2,BESTST2,GAMST2
!--
      REAL(8) T,P,Z, &
      DELTA,START,XX,INTP(5),VOLP(5),FF, &
      VOLST,INTST,ZP(5),ZST
      INTEGER(4) I,NP,NSTAB
      LOGICAL(4) FOUND
!--
!      WRITE (6,3010) BVC,CVC2,DVC4,EVC5,FVC2
! 3010 FORMAT (10(F20.10))
!!--
!      OPEN (UNIT=12,FILE='duan_table',STATUS='UNKNOWN')
!      WRITE (12,3000)
! 3000 FORMAT (' Vol   ,log(Vol)   ,Z    ,P   ,Pab   ,Pint')
      DELTA=0.001D0
!---
      NP=0
      START=0.001D0
      DELTA=0.001D0
      FOUND=.FALSE.
      DO I=1,5
       CALL FINDPT(START,DELTA,FOUND,P,T,Z,XX)
       IF (FOUND) THEN
         NP=NP+1
         VOLP(NP)=XX
         ZP(NP)=Z
         CALL INTEGRALF(T,XX,FF)
         INTP(NP)=FF
         START=XX+DELTA
         FOUND=.FALSE.
       ELSE
        GOTO 888
       END IF
      END DO
!
  888 CONTINUE
!      CLOSE (UNIT=12)
!---
      IF (NP.EQ.1) NSTAB=1
      IF (NP.EQ.2) THEN
       IF (INTP(2).GT.INTP(1)) THEN
        NSTAB=2
       ELSE
        NSTAB=1
       END IF
      END IF
      IF (NP.GT.2) THEN
      WRITE (UNIT=6,FMT= &
      '(2(F20.10),'' more phases than expected'')') T,P
       IF (INTP(NP).GT.INTP(1)) THEN
        NSTAB=NP
       ELSE
        NSTAB=1
       END IF
!     NSTAB=NP
      END IF
      IF (NP.EQ.0) THEN
      WRITE (UNIT=6,FMT= &
      '(2(F20.10),'' less phases than expected'')') T,P
      NSTAB=1
      VOLP(1)=1000.0D0
      INTP(1)=0.0d0
      END IF
!---
!      WRITE (6,2000) VOLP(NSTAB),NP
! 2000 FORMAT (/,' The stable Volume is: ',F20.10,5X,I5)
      VOLST=VOLP(NSTAB)
      INTST=INTP(NSTAB)
      ZST=ZP(NSTAB)
!---
      RETURN
      END
!--------------------------------
!--------------------------------
      SUBROUTINE JUSTF(T,V,Z,P1)
      IMPLICIT NONE
      REAL(8) T,V,Z,P1, &
      V2,V4,V5
!--
      REAL(8) BVC,CVC2,DVC4,EVC5,FVC2,BETA1,GAMMAVC2,R,RT,MWT, &
      BST1,CST1,DST1,EST1,FST1,BESTST1,GAMST1, &
      BST2,CST2,DST2,EST2,FST2,BESTST2,GAMST2
      COMMON /DTHINGS/ BVC,CVC2,DVC4,EVC5,FVC2,BETA1,GAMMAVC2,R,RT, &
      MWT, &
      BST1,CST1,DST1,EST1,FST1,BESTST1,GAMST1, &
      BST2,CST2,DST2,EST2,FST2,BESTST2,GAMST2
!--
       V2=V*V
       V4=V2*V2
       V5=V4*V
       Z=1.0D0+BVC/V+CVC2/V2+DVC4/V4+EVC5/V5+ &
         (FVC2/V2)*(BETA1+GAMMAVC2/V2)*DEXP(-GAMMAVC2/V2)
       P1=Z*RT/V
!--
      RETURN
      END
!--------------------------------
!--------------------------------
      SUBROUTINE EQUA9(T,V,Z,PHI1,PHI2)
      IMPLICIT NONE
      REAL(8) T,V,Z,PHI1,PHI2, &
      V2,V4,V5,EXPON
!--
      REAL(8) BVC,CVC2,DVC4,EVC5,FVC2,BETA1,GAMMAVC2,R,RT,MWT, &
      BST1,CST1,DST1,EST1,FST1,BESTST1,GAMST1, &
      BST2,CST2,DST2,EST2,FST2,BESTST2,GAMST2
      COMMON /DTHINGS/ BVC,CVC2,DVC4,EVC5,FVC2,BETA1,GAMMAVC2,R,RT, &
      MWT, &
      BST1,CST1,DST1,EST1,FST1,BESTST1,GAMST1, &
      BST2,CST2,DST2,EST2,FST2,BESTST2,GAMST2
!--
      V2=V*V
      V4=V2*V2
      V5=V4*V
!
      EXPON=DEXP(-GAMMAVC2/V2)
      PHI1=-DLOG(Z)+BST1/V+CST1/(2.0D0*V2)+ &
          DST1/(4.0D0*V4)+EST1/(5.0D0*V5)
      PHI1=PHI1+((FST1*BETA1+BESTST1*FVC2)/ &
          (2.0D0*GAMMAVC2))*(1.0D0-EXPON)
      PHI1=PHI1+((FST1*GAMMAVC2+GAMST1*FVC2- &
          FVC2*BETA1*(GAMST1-GAMMAVC2))/(2.0D0*GAMMAVC2**2))* &
          (1.0D0-((GAMMAVC2/V2)+1.0D0)*EXPON)-(((GAMST1-GAMMAVC2)* &
          FVC2)/(2.0D0*GAMMAVC2**2))*(2.-(GAMMAVC2**2/V4+ &
          2.0D0*GAMMAVC2/V2+2.0D0)*EXPON)
!
      PHI2=-DLOG(Z)+BST2/V+CST2/(2.0D0*V2)+ &
          DST2/(4.0D0*V4)+EST2/(5.0D0*V5)
      PHI2=PHI2+((FST2*BETA1+BESTST2*FVC2)/ &
          (2.0D0*GAMMAVC2))*(1.0D0-EXPON)
      PHI2=PHI2+((FST2*GAMMAVC2+GAMST2*FVC2- &
          FVC2*BETA1*(GAMST2-GAMMAVC2))/(2.0D0*GAMMAVC2**2))* &
          (1.0D0-((GAMMAVC2/V2)+1.0D0)*EXPON)-(((GAMST2-GAMMAVC2)* &
          FVC2)/(2.0D0*GAMMAVC2**2))*(2.-(GAMMAVC2**2/V4+ &
          2.0D0*GAMMAVC2/V2+2.0D0)*EXPON)
!
!--
      RETURN
      END
!--------------------------------
!--------------------------------
      SUBROUTINE INTEGRALF(T,V,PST)
      IMPLICIT NONE
      REAL(8) T,V,PST, &
      V2,V4,V5
!--
      REAL(8) BVC,CVC2,DVC4,EVC5,FVC2,BETA1,GAMMAVC2,R,RT,MWT, &
      BST1,CST1,DST1,EST1,FST1,BESTST1,GAMST1, &
      BST2,CST2,DST2,EST2,FST2,BESTST2,GAMST2
      COMMON /DTHINGS/ BVC,CVC2,DVC4,EVC5,FVC2,BETA1,GAMMAVC2,R,RT, &
      MWT, &
      BST1,CST1,DST1,EST1,FST1,BESTST1,GAMST1, &
      BST2,CST2,DST2,EST2,FST2,BESTST2,GAMST2
!--
       V2=V*V
       V4=V2*V2
       V5=V4*V
       PST=(DEXP(-GAMMAVC2/V2))*(FVC2/V2+BETA1*FVC2/GAMMAVC2+ &
       FVC2/GAMMAVC2)/2.0D0+DLOG(V)&
       -BVC/V-CVC2/(2.0D0*V2)-DVC4/(4.0D0*V4)-EVC5/(5.0D0*V5)
       PST=PST*RT
!--
      RETURN
      END
!--------------------------------
!--------------------------------
      SUBROUTINE ABLEITF(T,V,PST)
      IMPLICIT NONE
      REAL(8) T,V,PST, &
      V2,V3,V4
!--
      REAL(8) BVC,CVC2,DVC4,EVC5,FVC2,BETA1,GAMMAVC2,R,RT,MWT, &
      BST1,CST1,DST1,EST1,FST1,BESTST1,GAMST1, &
      BST2,CST2,DST2,EST2,FST2,BESTST2,GAMST2
      COMMON /DTHINGS/ BVC,CVC2,DVC4,EVC5,FVC2,BETA1,GAMMAVC2,R,RT, &
      MWT, &
      BST1,CST1,DST1,EST1,FST1,BESTST1,GAMST1, &
      BST2,CST2,DST2,EST2,FST2,BESTST2,GAMST2
!--
       V2=V*V
       V3=V2*V
       V4=V2*V2
       PST=(DEXP(-GAMMAVC2/V2))*(-3.0D0*BETA1*FVC2+ &
       2.0D0*BETA1*FVC2*GAMMAVC2/V2- &
       5.0D0*FVC2*GAMMAVC2/V2+ &
       (2.0D0*FVC2*GAMMAVC2**2)/V4)/V4+ &
       (-V-2.0D0*BVC-3.0D0*CVC2/V-5.0D0*DVC4/V3-6.0D0*EVC5/V4)/V3
       PST=PST*RT
!--
      RETURN
      END
!--------------------------------
!--------------------------------
      SUBROUTINE FINDPT(START,DELTA,FOUND,P,T,Z,XX)
      IMPLICIT NONE
!--
      REAL(8) BVC,CVC2,DVC4,EVC5,FVC2,BETA1,GAMMAVC2,R,RT,MWT, &
      BST1,CST1,DST1,EST1,FST1,BESTST1,GAMST1, &
      BST2,CST2,DST2,EST2,FST2,BESTST2,GAMST2
      COMMON /DTHINGS/ BVC,CVC2,DVC4,EVC5,FVC2,BETA1,GAMMAVC2,R,RT, &
      MWT, &
      BST1,CST1,DST1,EST1,FST1,BESTST1,GAMST1, &
      BST2,CST2,DST2,EST2,FST2,BESTST2,GAMST2
!--
      REAL(8) T,P,V,Z, &
      START,DELTA,P1,P2,P3,X1,X2,X3, &
      VMAXI,PMINI,PDEL,DX,XX,XDEL,VXMINI,AB1,AB2,VVOR
      LOGICAL(4) COD1,COD2,COD3
      LOGICAL(4) JUM,FOUND
!--
      JUM=.FALSE.
      FOUND=.FALSE.
      XX=START
      VMAXI=500.0D0
      PMINI=0.00001D0
      VXMINI=1.0D-5
      DX=DELTA
      V=START
      X1=V
      CALL JUSTF(T,V,Z,P1)
      CALL ABLEITF(T,V,AB1)
      COD1=(P.GT.P1)
!      WRITE (6,2000) V,DLOG10(V),Z,P1,AB1
!      WRITE (12,2000)  V,DLOG10(V),Z,P1,AB1
      P2=P1
      AB2=AB1
      COD2=COD1
!-----
      DO WHILE(V.LE.VMAXI)
       DX=V/5.0D0
       VVOR=V
    3  V=VVOR+DX
       X2=V
       CALL JUSTF(T,V,Z,P2)
       CALL ABLEITF(T,V,AB2)
       COD2=(P.GT.P2)
!       WRITE (6,2000) V,DLOG10(V),Z,P1,AB2
!       WRITE (12,2000)  V,DLOG10(V),Z,P1,AB2
       IF (COD1.AND.COD2.AND.(AB1.GT.0.0D0).AND.(AB2.LT.0.0D0) &
       .AND.DX.GT.1D-5) THEN
        DX=DX/2.0D0
!!!        WRITE (UNIT=6,FMT='(/,''DX '',F20.10)') DX
        GOTO 3
       END IF
! 2000 FORMAT (500(1PE14.7,:,','))
!--- ...OR.COD1 ist damit positive Ableitungen uebersprungen werden
       IF ((COD1.EQV.COD2).OR.COD1) THEN
        X1=X2
        P1=P2
        AB1=AB2
        COD1=COD2
       ELSE
        JUM=.TRUE.
        GOTO 10
       END IF
!---
       
      END DO
!---
   10 CONTINUE
      IF (.NOT.JUM) THEN
!       WRITE (UNIT=6,FMT='('' end of search: v='',F15.6)') V
       RETURN
      END IF
!-----
   11 PDEL=DABS(P2-P1)
      XDEL=DABS(X2-X1)
      IF ((PDEL.LT.PMINI).AND.(XDEL.LT.VXMINI)) GOTO 15
      V=(X1+X2)/2.0D0
      CALL JUSTF(T,V,Z,P3)
      X3=V
      COD3=(P.GT.P3)
!-----
      IF (COD3.EQV.COD1) THEN
       X1=X3
       P1=P3
       COD1=COD3
      ELSE
       X2=X3
       P2=P3
       COD2=COD3
      END IF
      GOTO 11
!-----
   15 CONTINUE
      V=(X1+X2)/2.0D0
      CALL JUSTF(T,V,Z,P1)
      X1=V
!      WRITE (6,1000) X1,P1,X1*1000.0D0,0.0180154D0/(X1/1000.0D0)
! 1000 FORMAT (/,' X1,P1= ',F20.10,2X,F20.10,2X,F20.10,2X,F20.10)
!      CALL PRINTDUAN(P,P1,T,X1,Z)
      FOUND=.TRUE.
      XX=X1
!-----
      RETURN
      END
!--------------------------------
!--------------------------------
!--------------------------------
!--------------------------------
!--------------------------------
!--------------------------------
!--------------------------------
!--------------------------------
!--------------------------------
!--------------------------------
!--------------------------------
!--------------------------------
!--------------------------------
!--------------------------------
!--------------------------------
!--------------------------------
!--------------------------------
!--------------------------------
!--------------------------------
!--------------------------------
      SUBROUTINE DUAN92EQ(T,P,XF,VAT,AC1,AC2,FUGH2O,FUGCO2)
      IMPLICIT NONE
!--
      REAL(8) BVC,CVC2,DVC4,EVC5,FVC2,BETA1,GAMMAVC2,R,RT,MWT, &
      BST1,CST1,DST1,EST1,FST1,BESTST1,GAMST1, &
      BST2,CST2,DST2,EST2,FST2,BESTST2,GAMST2
      COMMON /DTHINGS/ BVC,CVC2,DVC4,EVC5,FVC2,BETA1,GAMMAVC2,R,RT, &
      MWT, &
      BST1,CST1,DST1,EST1,FST1,BESTST1,GAMST1, &
      BST2,CST2,DST2,EST2,FST2,BESTST2,GAMST2
!--
      REAL(8) P,T,VATP,INTATP, &
      XF(2),ZATP,PHI1,PHI2,VAT,ACH,ACC, &
      V01,PH01,V02,PH02, &
      FUGH2O,FUGCO2,AH2O,ACO2,AC1,AC2,G
!      INTEGER(4) I
!--
!
      IF (FUGH2O.EQ.0.0D0.AND.FUGCO2.EQ.0.0D0) THEN
!      WRITE (6,1900)
! 1900 FORMAT (/,' pure H2O', &
!              /,' --------')
      CALL DUAN92H2O(T,P,G,V01,PH01)
      FUGH2O=PH01
!      WRITE (6,2000) FUGH2O
! 2000 FORMAT (' act.coeff     H2O    =',F20.10)
!!
!      WRITE (6,1902)
! 1902 FORMAT (/,' pure CO2', &
!              /,' --------')
      CALL DUAN92CO2(T,P,G,V02,PH02)
      FUGCO2=PH02
!      WRITE (6,2012) FUGCO2
! 2012 FORMAT (' act.coeff     CO2    =',F20.10)
!
      END IF
!
!
!
!      WRITE (6,1904)
! 1904 FORMAT (/,' H2O-CO2 mix', &
!              /,' -----------')
      CALL MIXRULES92(T,P,XF)
      CALL DUANZ(T,P,ZATP,VATP,INTATP)
      CALL EQUA9(T,VATP,ZATP,PHI1,PHI2)
!
      VAT=VATP
      ACH=DEXP(PHI1)
      ACC=DEXP(PHI2)
!
!      WRITE (6,1008) (XF(I),I=1,2)
! 1008 FORMAT (/,' composition =',F10.5,' H2O',F10.5,' CO2',F10.5,' CH4')
!!
!      WRITE (6,1026) PHI1
! 1026 FORMAT (' ln(g)     H2O    =',F20.10)
!      WRITE (6,1028) RT*PHI1
! 1028 FORMAT (' RT*ln(g)  H2O    =',F20.10)
!      WRITE (6,1029) RT*PHI1+RT*DLOG(P)
! 1029 FORMAT (' RT*ln(f)  H2O    =',F20.10)
!      WRITE (6,1030) ACH
! 1030 FORMAT (' act.coeff H2O    =',F20.10)
!      WRITE (6,1032) PHI2
! 1032 FORMAT (' ln(g)     CO2         =',F20.10)
!      WRITE (6,1034) RT*PHI2
! 1034 FORMAT (' RT*ln(g)  CO2         =',F20.10)
!      WRITE (6,1035) RT*PHI2+RT*DLOG(P)
! 1035 FORMAT (' RT*ln(f)  CO2         =',F20.10)
!      WRITE (6,1036) ACC
! 1036 FORMAT (' act.coeff CO2         =',F20.10)
!!
!!
      AH2O=ACH*XF(1)/FUGH2O
      ACO2=ACC*XF(2)/FUGCO2
!      WRITE (6,1050) AH2O
! 1050 FORMAT (/,' activity H2O    =',F20.10)
!      WRITE (6,1052) ACO2
! 1052 FORMAT (' activity CO2    =',F20.10)
      AC1=AH2O
      AC2=ACO2
!
      RETURN
      END
!--------------------------------
!--------------------------------
      SUBROUTINE DUAN92H2O(T,P,G,VOLUM,FUGCF)
      IMPLICIT NONE
!--
      REAL(8) BVC,CVC2,DVC4,EVC5,FVC2,BETA1,GAMMAVC2,R,RT,MWT, &
      BST1,CST1,DST1,EST1,FST1,BESTST1,GAMST1, &
      BST2,CST2,DST2,EST2,FST2,BESTST2,GAMST2
      COMMON /DTHINGS/ BVC,CVC2,DVC4,EVC5,FVC2,BETA1,GAMMAVC2,R,RT, &
      MWT, &
      BST1,CST1,DST1,EST1,FST1,BESTST1,GAMST1, &
      BST2,CST2,DST2,EST2,FST2,BESTST2,GAMST2
!---
      REAL(8) TR,TR2,TR3,VC,VC2,VC4,VC5,EU3,T,P,Z,V,INTE, &
      V2,V4,V5,EXPON,PHI1,G,H0,S0,K1,K2,K3,K4,K6, &
      CPDT,CPTDT,T0,TT,TT0,SQT,SQT0,FUGCF,VOLUM
!--
      T0=298.15D0
      TT=T*T
      TT0=T0*T0
      SQT=DSQRT(T)
      SQT0=DSQRT(T0)
!---
      R=0.08314467D0
      RT=R*T
      EU3=1.0D0/3.0D0
!      PR=P/221.19D0
      TR=T/647.25D0
      TR2=TR*TR
      TR3=TR2*TR
      VC=R*647.25D0/221.19D0
      VC2=VC*VC
      VC4=VC2*VC2
      VC5=VC4*VC
      BVC=(8.64449220D-02-3.96918955D-01/TR2-5.73334886D-02/TR3)*VC
      CVC2=(-2.93893000D-04-4.15775512D-03/TR2+1.99496791D-02/TR3)*VC2
      DVC4=(1.18901426D-04+1.55212063D-04/TR2-1.06855859D-04/TR3)*VC4
      EVC5=(-4.93197687D-06-2.73739155D-06/TR2+2.65571238D-06/TR3)*VC5
      FVC2=(8.96079018D-03/TR3)*VC2
      BETA1=4.02D+00
      GAMMAVC2=2.57D-02*VC2
      MWT=0.0180154D0
      BST1=2.0D0*BVC
      CST1=3.0D0*CVC2
      DST1=5.0D0*DVC4
      EST1=6.0D0*EVC5
      FST1=2.0D0*FVC2
      BESTST1=BETA1
      GAMST1=3.0D0*GAMMAVC2
      BST2=0.0D0
      CST2=0.0D0
      DST2=0.0D0
      EST2=0.0D0
      FST2=0.0D0
      BESTST2=0.0D0
      GAMST2=0.0D0
!---- find the volume
!--
      CALL DUANZ(T,P,Z,V,INTE)
!--
!---- EQ9 for H2O
!--
      V2=V*V
      V4=V2*V2
      V5=V4*V
      EXPON=DEXP(-GAMMAVC2/V2)
      PHI1=-DLOG(Z)+BST1/V+CST1/(2.0D0*V2)+ &
          DST1/(4.0D0*V4)+EST1/(5.0D0*V5)
      PHI1=PHI1+((FST1*BETA1+BESTST1*FVC2)/ &
          (2.0D0*GAMMAVC2))*(1.0D0-EXPON)
      PHI1=PHI1+((FST1*GAMMAVC2+GAMST1*FVC2- &
          FVC2*BETA1*(GAMST1-GAMMAVC2))/(2.0D0*GAMMAVC2**2))* &
          (1.0D0-((GAMMAVC2/V2)+1.0D0)*EXPON)-(((GAMST1-GAMMAVC2)* &
          FVC2)/(2.0D0*GAMMAVC2**2))*(2.-(GAMMAVC2**2/V4+ &
          2.0D0*GAMMAVC2/V2+2.0D0)*EXPON)
!--
!---- G for ideal gas
!--
      H0=-241816.00D0
      S0=188.7200D0
      K1=115.45000D0
      K4=-3799.900D0
      K3=-2871300.00D0
      K6=51055.16D0
      K2=-0.002062D0
      CPDT=K1*(T-T0)+K2*(TT-TT0)/2.0D0 &
      -K3*(1.0D0/T-1.0D0/T0)+2D0*K4*(SQT-SQT0) &
      +K6*DLOG(T/T0)
      CPTDT=K1*DLOG(T/T0)+K2*(T-T0) &
      -K3*(1.0D0/(TT)-1.0D0/(TT0))/2.0D0 &
      -2D0*K4*(1.0D0/SQT-1.0D0/SQT0) &
      -K6*(1.0D0/T-1.0D0/T0)
      G=H0+CPDT-T*(S0+CPTDT)
!--
!---- correct for fugacity
!---
      FUGCF=DEXP(PHI1)
      G=G+RT*100.0D0*DLOG(FUGCF*P)
      VOLUM=V*100.0D0
!---
      RETURN
      END
!
!--------------------------------
!--------------------------------
      SUBROUTINE DUAN92CO2(T,P,G,VOLUM,FUGCF)
      IMPLICIT NONE
!--
      REAL(8) BVC,CVC2,DVC4,EVC5,FVC2,BETA1,GAMMAVC2,R,RT,MWT, &
      BST1,CST1,DST1,EST1,FST1,BESTST1,GAMST1, &
      BST2,CST2,DST2,EST2,FST2,BESTST2,GAMST2
      COMMON /DTHINGS/ BVC,CVC2,DVC4,EVC5,FVC2,BETA1,GAMMAVC2,R,RT, &
      MWT, &
      BST1,CST1,DST1,EST1,FST1,BESTST1,GAMST1, &
      BST2,CST2,DST2,EST2,FST2,BESTST2,GAMST2
!---
      REAL(8) TR,TR2,TR3,VC,VC2,VC4,VC5,EU3,T,P,Z,V,INTE, &
      V2,V4,V5,EXPON,PHI2,G,H0,S0,K1,K2,K3,K4,K6, &
      CPDT,CPTDT,T0,TT,TT0,SQT,SQT0,FUGCF,VOLUM
!--
      T0=298.15D0
      TT=T*T
      TT0=T0*T0
      SQT=DSQRT(T)
      SQT0=DSQRT(T0)
!---
      R=0.08314467D0
      RT=R*T
      EU3=1.0D0/3.0D0
!      PR=P/221.19D0
      TR=T/304.20D0
      TR2=TR*TR
      TR3=TR2*TR
      VC=R*304.20D0/73.825D0
      VC2=VC*VC
      VC4=VC2*VC2
      VC5=VC4*VC
      BVC=(8.99288497D-02-4.94783127D-01/TR2+4.77922245D-02/TR3)*VC
      CVC2=(1.03808883D-02-2.82516861D-02/TR2+9.49887563D-02/TR3)*VC2
      DVC4=(5.20600880D-04-2.93540971D-04/TR2-1.77265112D-03/TR3)*VC4
      EVC5=(-2.51101973D-05+8.93353441D-05/TR2+7.88998563D-05/TR3)*VC5
      FVC2=(-1.66727022D-02/TR3)*VC2
      BETA1=1.398D+00
      GAMMAVC2=2.96D-02*VC2
      MWT=0.0440098D0
      BST2=2.0D0*BVC
      CST2=3.0D0*CVC2
      DST2=5.0D0*DVC4
      EST2=6.0D0*EVC5
      FST2=2.0D0*FVC2
      BESTST2=BETA1
      GAMST2=3.0D0*GAMMAVC2
      BST1=0.0D0
      CST1=0.0D0
      DST1=0.0D0
      EST1=0.0D0
      FST1=0.0D0
      BESTST1=0.0D0
      GAMST1=0.0D0
!---- find the volume
!--
      CALL DUANZ(T,P,Z,V,INTE)
!--
!---- EQ9 for CO2
!--
      V2=V*V
      V4=V2*V2
      V5=V4*V
      EXPON=DEXP(-GAMMAVC2/V2)
      PHI2=-DLOG(Z)+BST2/V+CST2/(2.0D0*V2)+ &
          DST2/(4.0D0*V4)+EST2/(5.0D0*V5)
      PHI2=PHI2+((FST2*BETA1+BESTST2*FVC2)/ &
          (2.0D0*GAMMAVC2))*(1.0D0-EXPON)
      PHI2=PHI2+((FST2*GAMMAVC2+GAMST2*FVC2- &
          FVC2*BETA1*(GAMST2-GAMMAVC2))/(2.0D0*GAMMAVC2**2))* &
          (1.0D0-((GAMMAVC2/V2)+1.0D0)*EXPON)-(((GAMST2-GAMMAVC2)* &
          FVC2)/(2.0D0*GAMMAVC2**2))*(2.-(GAMMAVC2**2/V4+ &
          2.0D0*GAMMAVC2/V2+2.0D0)*EXPON)
!--
!---- G for ideal gas
!--
      H0=-393510.010
      S0=213.6770D0
      K1=93.0000D0
      K4=-1340.900D0
      K3=123800.000D0
      K6=6336.20D0
      K2=-0.002876D0
      CPDT=K1*(T-T0)+K2*(TT-TT0)/2.0D0 &
      -K3*(1.0D0/T-1.0D0/T0)+2D0*K4*(SQT-SQT0) &
      +K6*DLOG(T/T0)
      CPTDT=K1*DLOG(T/T0)+K2*(T-T0) &
      -K3*(1.0D0/(TT)-1.0D0/(TT0))/2.0D0 &
      -2D0*K4*(1.0D0/SQT-1.0D0/SQT0) &
      -K6*(1.0D0/T-1.0D0/T0)
      G=H0+CPDT-T*(S0+CPTDT)
!--
!---- correct for fugacity
!---
      FUGCF=DEXP(PHI2)
      G=G+RT*100.0D0*DLOG(FUGCF*P)
      VOLUM=V*100.0D0
!---
      RETURN
      END
!
!--------------------------------
!--------------------------------
      SUBROUTINE MIXRULES92(T,P,XF)
      IMPLICIT NONE
!--
      REAL(8) BVC,CVC2,DVC4,EVC5,FVC2,BETA1,GAMMAVC2,R,RT,MWT, &
      BST1,CST1,DST1,EST1,FST1,BESTST1,GAMST1, &
      BST2,CST2,DST2,EST2,FST2,BESTST2,GAMST2
      COMMON /DTHINGS/ BVC,CVC2,DVC4,EVC5,FVC2,BETA1,GAMMAVC2,R,RT, &
      MWT, &
      BST1,CST1,DST1,EST1,FST1,BESTST1,GAMST1, &
      BST2,CST2,DST2,EST2,FST2,BESTST2,GAMST2
!--
!---- 1=H2O, 2=CO2, 3=CH4
      REAL(8) A1(2),A2(2),A3(2),A4(2),A5(2),A6(2),A7(2),A8(2),A9(2), &
      A10(2),A11(2),A12(2),ALPHA(2),BETA(2),GAMMA(2),TC(2),PC(2), &
      BB(2),CC(2),DD(2),EE(2),FF(2), &
      BS(2),CS(2),DS(2),ES(2),FS(2), &
      PM(2), &
      VC(2),VC2(2),VC4(2),VC5(2), &
      TR(2),TR2(2),TR3(2),PR(2),P,T,XF(2),EU3
      INTEGER(4) I,J,K,II,L,M,N
      REAL(8) K1(2,2),K2(2,2,2),K3(2,2,2), &
      BIJ(2,2),VCIJ(2,2),CIJK(2,2,2),VCIJK(2,2,2), &
      DIJKLM(2,2,2,2,2),VCIJKLM(2,2,2,2,2), &
      EIJKLMN(2,2,2,2,2,2),VCIJKLMN(2,2,2,2,2,2), &
      FIJ(2,2),GAMIJK(2,2,2)
!-----
      DATA A1  / 8.64449220D-02, 8.99288497D-02/
      DATA A2  /-3.96918955D-01,-4.94783127D-01/
      DATA A3  /-5.73334886D-02, 4.77922245D-02/
      DATA A4  /-2.93893000D-04, 1.03808883D-02/
      DATA A5  /-4.15775512D-03,-2.82516861D-02/
      DATA A6  / 1.99496791D-02, 9.49887563D-02/
      DATA A7  / 1.18901426D-04, 5.20600880D-04/
      DATA A8  / 1.55212063D-04,-2.93540971D-04/
      DATA A9  /-1.06855859D-04,-1.77265112D-03/
      DATA A10 /-4.93197687D-06,-2.51101973D-05/
      DATA A11 /-2.73739155D-06, 8.93353441D-05/
      DATA A12 / 2.65571238D-06, 7.88998563D-05/
!-
      DATA ALPHA / 8.96079018D-03,-1.66727022D-02/
      DATA BETA  / 4.02D+00,       1.398D+00/
      DATA GAMMA / 2.57D-02,       2.96D-02/
!-
      DATA TC /647.25D0,304.20D0/
      DATA PC /221.19D0,73.825D0/
      DATA PM /0.0180154D0,0.0440098D0/
!-----
      R=0.08314467D0
      RT=R*T
      EU3=1.0D0/3.0D0
      II=1
!-----
      DO I=1,2
       PR(I)=P/PC(I)
       TR(I)=T/TC(I)
       TR2(I)=TR(I)*TR(I)
       TR3(I)=TR2(I)*TR(I)
       VC(I)=R*TC(I)/PC(I)
       VC2(I)=VC(I)*VC(I)
       VC4(I)=VC2(I)*VC2(I)
       VC5(I)=VC4(I)*VC(I)
!
       BB(I)=A1(I)+A2(I)/TR2(I)+A3(I)/TR3(I)
       CC(I)=A4(I)+A5(I)/TR2(I)+A6(I)/TR3(I)
       DD(I)=A7(I)+A8(I)/TR2(I)+A9(I)/TR3(I)
       EE(I)=A10(I)+A11(I)/TR2(I)+A12(I)/TR3(I)
       FF(I)=ALPHA(I)/TR3(I)
!-----
        IF (BB(I).LT.0.0D0) THEN
         BB(I)=-BB(I)
         BS(I)=-1.0D0
        ELSE
         BS(I)=1.0D0
        END IF
        IF (CC(I).LT.0.0D0) THEN
         CC(I)=-CC(I)
         CS(I)=-1.0D0
        ELSE
         CS(I)=1.0D0
        END IF
        IF (DD(I).LT.0.0D0) THEN
         DD(I)=-DD(I)
         DS(I)=-1.0D0
        ELSE
         DS(I)=1.0D0
        END IF
        IF (EE(I).LT.0.0D0) THEN
         EE(I)=-EE(I)
         ES(I)=-1.0D0
        ELSE
         ES(I)=1.0D0
        END IF
        IF (FF(I).LT.0.0D0) THEN
         FF(I)=-FF(I)
         FS(I)=-1.0D0
        ELSE
         FS(I)=1.0D0
        END IF
!-----
      END DO
!-----
!-----
!-----
      CALL MAKEK192(T,K1,K2,K3)
!-----
      DO I=1,2
       DO J=1,2
      BIJ(I,J)=(((BS(I)*BB(I)**EU3+BS(J)*BB(J)**EU3) &
               /2.0D0)**3)*K1(I,J)
      VCIJ(I,J)=((VC(I)**EU3+VC(J)**EU3)/2.0D0)**3
       END DO
      END DO
      BVC=0.0D0
      DO I=1,2
       DO J=1,2
      BVC=BVC+XF(I)*XF(J)*BIJ(I,J)*VCIJ(I,J)
       END DO
      END DO
      BST1=0.0D0
      BST2=0.0D0
      DO J=1,2
       BST1=BST1+2.0D0*XF(J)*BIJ(1,J)*VCIJ(1,J)
       BST2=BST2+2.0D0*XF(J)*BIJ(J,2)*VCIJ(J,2)
      END DO
!---
      DO I=1,2
       DO J=1,2
        DO K=1,2
      CIJK(I,J,K)=(((CS(I)*CC(I)**EU3+CS(J)*CC(J)**EU3+CC(K)**EU3) &
                  /3.0D0)**3)*K2(I,J,K)
      VCIJK(I,J,K)=((VC(I)**EU3+VC(J)**EU3+VC(K)**EU3)/3.0D0)**3
        END DO
       END DO
      END DO
      CVC2=0.0D0
      DO I=1,2
       DO J=1,2
        DO K=1,2
      CVC2=CVC2+XF(I)*XF(J)*XF(K)*CIJK(I,J,K)*VCIJK(I,J,K)**2
        END DO
       END DO
      END DO
      CST1=0.0D0
      CST2=0.0D0
      DO J=1,2
       DO K=1,2
        CST1=CST1+3.0D0*XF(J)*XF(K)*CIJK(1,J,K)*VCIJK(1,J,K)**2
        CST2=CST2+3.0D0*XF(J)*XF(K)*CIJK(J,2,K)*VCIJK(J,2,K)**2
       END DO
      END DO
!---
      DO I=1,2
       DO J=1,2
        DO K=1,2
         DO L=1,2
          DO M=1,2
      DIJKLM(I,J,K,L,M)=((DS(I)*DD(I)**EU3+DS(J)*DD(J)**EU3+ &
                          DS(K)*DD(K)**EU3+DS(L)*DD(L)**EU3+ &
                          DS(M)*DD(M)**EU3)/5.0D0)**3
      VCIJKLM(I,J,K,L,M)=((VC(I)**EU3+VC(J)**EU3+VC(K)**EU3+ &
                         VC(L)**EU3+VC(M)**EU3)/5.0D0)**3
          END DO
         END DO
        END DO
       END DO
      END DO
      DVC4=0.0D0
      DO I=1,2
       DO J=1,2
        DO K=1,2
         DO L=1,2
          DO M=1,2
      DVC4=DVC4+XF(I)*XF(J)*XF(K)*XF(L)*XF(M)*DIJKLM(I,J,K,L,M)* &
           VCIJKLM(I,J,K,L,M)**4
          END DO
         END DO
        END DO
       END DO
      END DO
      DST1=0.0D0
      DST2=0.0D0
      DO J=1,2
       DO K=1,2
        DO L=1,2
         DO M=1,2
      DST1=DST1+5.0D0*XF(J)*XF(K)*XF(L)*XF(M)*DIJKLM(1,J,K,L,M)* &
          VCIJKLM(1,J,K,L,M)**4
      DST2=DST2+5.0D0*XF(J)*XF(K)*XF(L)*XF(M)*DIJKLM(J,2,K,L,M)* &
          VCIJKLM(J,2,K,L,M)**4
         END DO
        END DO
       END DO
      END DO
!---
      DO I=1,2
       DO J=1,2
        DO K=1,2
         DO L=1,2
          DO M=1,2
           DO N=1,2
      EIJKLMN(I,J,K,L,M,N)=((ES(I)*EE(I)**EU3+ES(J)*EE(J)**EU3+ &
                             ES(K)*EE(K)**EU3+ES(L)*EE(L)**EU3+ &
                             ES(M)*EE(M)**EU3+ &
                             ES(N)*EE(N)**EU3)/6.0D0)**3
      VCIJKLMN(I,J,K,L,M,N)=((VC(I)**EU3+VC(J)**EU3+ &
                              VC(K)**EU3+VC(L)**EU3+ &
                              VC(M)**EU3+VC(N)**EU3)/6.0D0)**3
           END DO
          END DO
         END DO
        END DO
       END DO
      END DO
      EVC5=0.0D0
      DO I=1,2
       DO J=1,2
        DO K=1,2
         DO L=1,2
          DO M=1,2
           DO N=1,2
      EVC5=EVC5+XF(I)*XF(J)*XF(K)*XF(L)*XF(M)*XF(N)* &
      EIJKLMN(I,J,K,L,M,N)*VCIJKLMN(I,J,K,L,M,N)**5
           END DO
          END DO
         END DO
        END DO
       END DO
      END DO
      EST1=0.0D0
      EST2=0.0D0
      DO J=1,2
       DO K=1,2
        DO L=1,2
         DO M=1,2
          DO N=1,2
     EST1=EST1+6.0D0*XF(J)*XF(K)*XF(L)*XF(M)*XF(N)* &
     EIJKLMN(1,J,K,L,M,N)*VCIJKLMN(1,J,K,L,M,N)**5
     EST2=EST2+6.0D0*XF(J)*XF(K)*XF(L)*XF(M)*XF(N)* &
     EIJKLMN(J,2,K,L,M,N)*VCIJKLMN(J,2,K,L,M,N)**5
          END DO
         END DO
        END DO
       END DO
      END DO
!---
      DO I=1,2
       DO J=1,2
      FIJ(I,J)=((FS(I)*FF(I)**EU3+FS(J)*FF(J)**EU3)/2.0D0)**3
       END DO
      END DO
      FVC2=0.0D0
      DO I=1,2
       DO J=1,2
      FVC2=FVC2+XF(I)*XF(J)*FIJ(I,J)*VCIJ(I,J)**2
       END DO
      END DO
      FST1=0.0D0
      FST2=0.0D0
      DO J=1,2
      FST1=FST1+2.0D0*XF(J)*FIJ(1,J)*VCIJ(1,J)**2
      FST2=FST2+2.0D0*XF(J)*FIJ(J,2)*VCIJ(J,2)**2
      END DO
!---
      BETA1=0.0D0
      DO I=1,2
      BETA1=BETA1+XF(I)*BETA(I)
      END DO
      BESTST1=BETA(1)
      BESTST2=BETA(2)
!---
      DO I=1,2
       DO J=1,2
        DO K=1,2
      GAMIJK(I,J,K)=(((GAMMA(I)**EU3+GAMMA(J)**EU3+ &
      GAMMA(K)**EU3)/3.0D0)**3)*K3(I,J,K)
        END DO
       END DO
      END DO
      GAMMAVC2=0.0D0
      DO I=1,2
       DO J=1,2
        DO K=1,2
        GAMMAVC2=GAMMAVC2+XF(I)*XF(J)*XF(K)*GAMIJK(I,J,K)* &
        VCIJK(I,J,K)**2
        END DO
       END DO
      END DO
      GAMST1=0.0D0
      GAMST2=0.0D0
      DO J=1,2
      DO K=1,2
       GAMST1=GAMST1+3.0D0*XF(J)*XF(K)*GAMIJK(1,J,K)* &
       VCIJK(1,J,K)**2
       GAMST2=GAMST2+3.0D0*XF(J)*XF(K)*GAMIJK(J,2,K)* &
       VCIJK(J,2,K)**2
       END DO
      END DO
!-----
!-----
      MWT=XF(1)*PM(1)+XF(2)*PM(2)
!-----
!-----
      RETURN
      END
!--------------------------------
!--------------------------------
      SUBROUTINE MAKEK192(T,K1,K2,K3)
      IMPLICIT NONE
!-----
      REAL(8) T,K1(2,2),K2(2,2,2),K3(2,2,2),T2
      INTEGER(4) I,J,K
!-----
!--- bis jetzt nur index 1 und 2
!---- 1=H2O, 2=CO2, 3=CH4
      DO I=1,2
       DO J=1,2
        K1(I,J)=0.0D0
       END DO
      END DO
      DO I=1,2
       DO J=1,2
        DO K=1,2
         K2(I,J,K)=0.0D0
         K3(I,J,K)=0.0D0
        END DO
       END DO
      END DO
      T2=T*T
      K1(1,1)=1.0D0
      K1(2,2)=1.0D0
      K2(1,1,1)=1.0D0
      K2(2,2,2)=1.0D0
      K3(1,1,1)=1.0D0
      K3(2,2,2)=1.0D0
!--
!----- binary H2O-CO2
!--
      IF (T.LE.373.15) THEN
      K1(1,2)=0.20611+0.0006*T
      K1(2,1)=K1(1,2)
      K2(1,1,2)=0.8023278-0.0022206*T+184.76824/T
      K2(1,2,1)=K2(1,1,2)
      K2(2,1,1)=K2(1,1,2)
      K2(1,2,2)=K2(1,1,2)
      K2(2,1,2)=K2(1,1,2)
      K2(2,2,1)=K2(1,1,2)
      K3(1,1,2)=1.80544-0.0032605*T
      K3(1,2,1)=K3(1,1,2)
      K3(2,1,1)=K3(1,1,2)
      K3(1,2,2)=K3(1,1,2)
      K3(2,1,2)=K3(1,1,2)
      K3(2,2,1)=K3(1,1,2)
      END IF
!--
      IF (T.GT.373.15.AND.T.LE.495.15) THEN
      K1(1,2)=-10084.5042-4.27134485*T+256477.783/T+ &
            0.00166997474*T2+1816.78*DLOG(T)
      K1(2,1)=K1(1,2)
      K2(1,1,2)=9.000263-0.00623494*T-2307.7125/T
      K2(1,2,1)=K2(1,1,2)
      K2(2,1,1)=K2(1,1,2)
      K2(1,2,2)=K2(1,1,2)
      K2(2,1,2)=K2(1,1,2)
      K2(2,2,1)=K2(1,1,2)
      K3(1,1,2)=-74.1163+0.1800496*T-1.40904946E-04*T2+ &
            10130.5246/T
      K3(1,2,1)=K3(1,1,2)
      K3(2,1,1)=K3(1,1,2)
      K3(1,2,2)=K3(1,1,2)
      K3(2,1,2)=K3(1,1,2)
      K3(2,2,1)=K3(1,1,2)
      END IF
!--
      IF (T.GT.495.15.AND.T.LE.623.15) THEN
      K1(1,2)=-0.3568+7.8888E-04*T+333.399/T
      K1(2,1)=K1(1,2)
      K2(1,1,2)=-19.97444+0.0192515*T+5707.4229/T
      K2(1,2,1)=K2(1,1,2)
      K2(2,1,1)=K2(1,1,2)
      K2(1,2,2)=K2(1,1,2)
      K2(2,1,2)=K2(1,1,2)
      K2(2,2,1)=K2(1,1,2)
      K3(1,1,2)=12.1308-0.0099489*T-3042.09583/T
      K3(1,2,1)=K3(1,1,2)
      K3(2,1,1)=K3(1,1,2)
      K3(1,2,2)=K3(1,1,2)
      K3(2,1,2)=K3(1,1,2)
      K3(2,2,1)=K3(1,1,2)
      END IF
!--
      IF (T.GT.623.15) THEN
      K1(1,2)=-4.53122+0.0042113*T+1619.7/T
      K1(2,1)=K1(1,2)
      K2(1,1,2)=-163.4855+0.190552*T-7.228514E-05*T2+ &
            46082.885/T
      K2(1,2,1)=K2(1,1,2)
      K2(2,1,1)=K2(1,1,2)
      K2(1,2,2)=K2(1,1,2)
      K2(2,1,2)=K2(1,1,2)
      K2(2,2,1)=K2(1,1,2)
      K3(1,1,2)=1.7137-6.7136E-04*T
      K3(1,2,1)=K3(1,1,2)
      K3(2,1,1)=K3(1,1,2)
      K3(1,2,2)=K3(1,1,2)
      K3(2,1,2)=K3(1,1,2)
      K3(2,2,1)=K3(1,1,2)
      END IF
!--
!!!!      IF (T.GT.623.15) THEN
!!      K1(1,2)=0.5D0
!!      K1(2,1)=K1(1,2)
!!      K2(1,1,2)=1.0D0
!!      K2(1,2,1)=K2(1,1,2)
!!      K2(2,1,1)=K2(1,1,2)
!!      K2(1,2,2)=K2(1,1,2)
!!      K2(2,1,2)=K2(1,1,2)
!!      K2(2,2,1)=K2(1,1,2)
!!      K3(1,1,2)=1.0D0
!!      K3(1,2,1)=K3(1,1,2)
!!      K3(2,1,1)=K3(1,1,2)
!!      K3(1,2,2)=K3(1,1,2)
!!      K3(2,1,2)=K3(1,1,2)
!!      K3(2,2,1)=K3(1,1,2)
!!!!      END IF
!-----
!!      CALL FAKEK(T,1,FF)
!!      K1(1,2)=FF
!!      K1(2,1)=K1(1,2)
!!      CALL FAKEK(T,2,FF)
!!      K2(1,1,2)=FF
!!      K2(1,2,1)=K2(1,1,2)
!!      K2(2,1,1)=K2(1,1,2)
!!      K2(1,2,2)=K2(1,1,2)
!!      K2(2,1,2)=K2(1,1,2)
!!      K2(2,2,1)=K2(1,1,2)
!!      CALL FAKEK(T,3,K3)
!!      K3(1,1,2)=FF
!!      K3(1,2,1)=K3(1,1,2)
!!      K3(2,1,1)=K3(1,1,2)
!!      K3(1,2,2)=K3(1,1,2)
!!      K3(2,1,2)=K3(1,1,2)
!!      K3(2,2,1)=K3(1,1,2)
!-----
      RETURN
      END
!=================================
!=================================
      SUBROUTINE FAKEK(T,KN,YY)
!      
      IMPLICIT NONE
      REAL(4) X(12),Y(12),Y2(12),YP1,YPN,YY,T
      INTEGER(4) N,KN
      N=12
      YP1=0.0D0
      YPN=0.0D0
!!
      IF (KN.EQ.1) THEN
      X(  1)= 0.0000000E+00
      Y(  1)= 3.7000001E-01
      X(  2)= 5.0000000E+01
      Y(  2)= 4.0000001E-01
      X(  3)= 1.0000000E+02
      Y(  3)= 4.4012231E-01
      X(  4)= 1.5000000E+02
      Y(  4)= 6.0012841E-01
      X(  5)= 2.0000000E+02
      Y(  5)= 7.3013507E-01
      X(  6)= 2.5000000E+02
      Y(  6)= 6.9319396E-01
      X(  7)= 3.0000000E+02
      Y(  7)= 6.7704244E-01
      X(  8)= 3.5000000E+02
      Y(  8)= 6.6981262E-01
      X(  9)= 4.0000000E+02
      Y(  9)= 7.0976679E-01
      X( 10)= 4.5000000E+02
      Y( 10)= 7.5396588E-01
      X( 11)= 5.0000000E+02
      Y( 11)= 8.1968291E-01
      X( 12)= 5.5000000E+02
      Y( 12)= 9.0299673E-01
      END IF
!!
      IF (KN.EQ.2) THEN
      X(  1)= 0.0000000E+00
      Y(  1)= 8.7220600E-01
      X(  2)= 5.0000000E+01
      Y(  2)= 6.5651330E-01
      X(  3)= 1.0000000E+02
      Y(  3)= 4.8928581E-01
      X(  4)= 1.5000000E+02
      Y(  4)= 9.0829734E-01
      X(  5)= 2.0000000E+02
      Y(  5)= 1.1728634E+00
      X(  6)= 2.5000000E+02
      Y(  6)= 1.0067077E+00
      X(  7)= 3.0000000E+02
      Y(  7)= 1.0175488E+00
      X(  8)= 3.5000000E+02
      Y(  8)= 1.1811193E+00
      X(  9)= 4.0000000E+02
      Y(  9)= 4.8851040E-01
      X( 10)= 4.5000000E+02
      Y( 10)= 2.3616421E-01
      X( 11)= 5.0000000E+02
      Y( 11)= 2.3460973E-01
      X( 12)= 5.5000000E+02
      Y( 12)= 3.7228455E-01
      END IF
!!
      IF (KN.EQ.3) THEN
      X(  1)= 0.0000000E+00
      Y(  1)= 9.1483437E-01
      X(  2)= 5.0000000E+01
      Y(  2)= 7.5180936E-01
      X(  3)= 1.0000000E+02
      Y(  3)= 5.9813593E-01
      X(  4)= 1.5000000E+02
      Y(  4)= 7.8256099E-01
      X(  5)= 2.0000000E+02
      Y(  5)= 9.4045446E-01
      X(  6)= 2.5000000E+02
      Y(  6)= 1.1110739E+00
      X(  7)= 3.0000000E+02
      Y(  7)= 1.1209097E+00
      X(  8)= 3.5000000E+02
      Y(  8)= 1.0493396E+00
      X(  9)= 4.0000000E+02
      Y(  9)= 1.2617741E+00
      X( 10)= 4.5000000E+02
      Y( 10)= 1.2282061E+00
      X( 11)= 5.0000000E+02
      Y( 11)= 1.1946381E+00
      X( 12)= 5.5000000E+02
      Y( 12)= 1.1610701E+00
      END IF
      CALL spline(X,Y,N,YP1,YPN,Y2)
      CALL splint(X,Y,Y2,N,T,YY)
!!
!
      END
!==========
!      CUBIC SPLINE INTERPOLATION ROUTINES
!      taken form Numerical Recipes in Fortran (website version)
!
!      Note:
!      It is important to understand that the program spline is called only once 
!      to process an entire tabulated function in arrays xi and yi. 
!      Once this has been done, values of the interpolated function for any value of x
!      are obtained by calls (as many as desired) to a separate routine splint 
!      (for “spline interpolation”)
!
      SUBROUTINE spline(x,y,n,yp1,ypn,y2) 
      INTEGER n,NMAX 
      REAL yp1,ypn,x(n),y(n),y2(n) 
      PARAMETER (NMAX=500)
!      Given arrays x(1:n) and y(1:n) containing a tabulated function, 
!      i.e., yi = f(xi), with x1 < x2 < ... < xN, 
!      and given values yp1 and ypn for the first derivative of the 
!      interpolating function at points 1 and n, respectively, 
!      this routine returns an array y2(1:n) of length n 
!      which contains the second derivatives of the interpolating function 
!      at the tabulated points xi. 
!      If yp1 and/or ypn are equal to 10**30 or larger, the routine 
!      is signaled to set the corresponding boundary condition for a 
!      natural spline, with zero second derivative on that boundary.
!      Parameter: NMAX is the largest anticipated value of n. 
      INTEGER i,k
      REAL p,qn,sig,un,u(NMAX) 
!      The lower boundary condition is set either to be “natural”
!      or else to have a specified first derivative.
      if (yp1.gt..99e30) then
         y2(1)=0.
         u(1)=0. 
      else
         y2(1)=-0.5
         u(1)=(3./(x(2)-x(1)))*((y(2)-y(1))/(x(2)-x(1))-yp1) 
      endif
      do i=2,n-1 
         sig=(x(i)-x(i-1))/(x(i+1)-x(i-1)) 
         p=sig*y2(i-1)+2. 
         y2(i)=(sig-1.)/p 
         u(i)=(6.*((y(i+1)-y(i))/(x(i+1)-x(i))-(y(i)-y(i-1)) &
              /(x(i)-x(i-1)))/(x(i+1)-x(i-1))-sig*u(i-1))/p
      end do
      if (ypn.gt..99e30) then
!      The upper boundary condition is set either to be “natural”
!      or else to have a specified first derivative.
        qn=0.
        un=0. 
      else
        qn=0.5
        un=(3./(x(n)-x(n-1)))*(ypn-(y(n)-y(n-1))/(x(n)-x(n-1)))
      endif
      y2(n)=(un-qn*u(n-1))/(qn*y2(n-1)+1.)
      do k=n-1,1,-1
        y2(k)=y2(k)*y2(k+1)+u(k) 
      end do
      return 
      END
!
!      Values of the interpolated function for any value of x are 
!      obtained by calls (as many as desired) to a separate routine 
!      splint (for “spline interpolation”)
!
      SUBROUTINE splint(xa,ya,y2a,n,x,y) 
      INTEGER n 
      REAL x,y,xa(n),y2a(n),ya(n)
!      Given the arrays xa(1:n) and ya(1:n) of length n, which tabulate a 
!      function (with the xai’s in order), and given the array y2a(1:n), 
!      which is the output from spline above, and given a value of x, 
!      this routine returns a cubic-spline interpolated value y.
      INTEGER k,khi,klo 
      REAL a,b,h 
!      We will find the right place in the table by means of bisection. 
!      This is optimal if sequential calls to this routine are at random 
!      values of x. If sequential calls are in order, and closely spaced, 
!      one would do better to store previous values of klo and khi and 
!      test if they remain appropriate on the next call.
      klo=1 
      khi=n
  1   if (khi-klo.gt.1) then 
         k=(khi+klo)/2
         if(xa(k).gt.x)then 
            khi=k
         else 
            klo=k
         endif 
      goto 1
      endif 
      h=xa(khi)-xa(klo)
 !      The Xa's must be distinct:'
 !!!!!!     if (h.eq.0.) pause ’bad xa input in splint’
 !      Cubic spline polynomial is now evaluated:
      a=(xa(khi)-x)/h
      b=(x-xa(klo))/h 
      y=a*ya(klo)+b*ya(khi)+ &
        ((a**3-a)*y2a(klo)+(b**3-b)*y2a(khi))*(h**2)/6.
      return 
      END
!===============================================================
!===============================================================
!--------------------------------
!--------------------------------
      SUBROUTINE PS94H2O(P,T,GRE,VRE)
      IMPLICIT NONE
!-- Subroutines by CdC and Erik Düsterhoeft
!-- K.S. Pitzer and S.M. Sterner, 1994
!-- J. Chem. Phys. 101(4),1994,3111-3116
!-- critical point H2O: 373.946 C, 220.64 Bar
!-- critical density H2O: 0.017873993893977 mol/ccm
      REAL(8) CC(10),RT
      COMMON /PISTRE/ CC,RT
!--
      REAL(8) T,P,R,ZZ,MI,MA,LIM1,LIM2,RHINI, &
      VOL1,VOL2,VOL0,VDP1,VDP2,PDV,F1,RH0,RH1,RH2,VRE,GRE
      REAL(8) ARES,LOF
      INTEGER(4) I,I0,I1,I2,IM,HOW
!--
      REAL(8) GF,K1,K2,K3,K4,CPRDT,CPRTDT,H0,S0,TT,SQT,TT0,SQT0,T0
!-H2O
      DATA H0,S0/-241.81D0,188.80D-3/
      DATA K1,K2,K3,K4/0.0401D0,0.8656D-5,487.5D0,-0.2512D0/
      DATA T0,TT0/298.15D0,88893.4225D0/
!-CO2
!      DATA H0,S0/-393.51D0,213.70D-3/
!      DATA K1,K2,K3,K4/0.0878D0,-0.2644D-5,706.4D0,-0.9989D0/
!      DATA T0,TT0/298.15D0,88893.4225D0/
!--
      R=8.3143D0
      SQT0=DSQRT(T0)
      TT=T*T
      SQT=DSQRT(T)
      RT=R*T
!
      CPRDT=K1*(T-T0)+K2*(TT-TT0)/2.0D0 &
      -K3*(1.0D0/T-1.0D0/T0)+2D0*K4*(SQT-SQT0)
      CPRTDT=K1*DLOG(T/T0)+K2*(T-T0) &
      -K3*(1.0D0/(TT)-1.0D0/(TT0))/2.0D0 &
      -2D0*K4*(1.0D0/SQT-1.0D0/SQT0)
      GF=1.0D3*(H0+CPRDT-T*(S0+CPRTDT))
!
      I0=0
      I1=0
      I2=0
      VRE=0.0D0
      GRE=0.0D0
!----
      CALL PISTH2ODEF(T)
!      CALL PISTCO2DEF(T)
!=====
!----- (0) low RH at 1 Bar. If fail, use ideal gas
      ZZ=1.0D0/RT/10.0D0
      LIM1=-1D20
      LIM2=0.03D0
      RHINI=1.0D0/RT
      CALL PISTFINDRH(ZZ,LIM1,LIM2,RHINI,RH0)
      IF (RH0.EQ.0.0D0) THEN
      VOL0=RT/P
      ELSE
      VOL0=1.0D0/RH0/10.0D0
      I0=1
      END IF
!
!----- (1) try low RH If fail, use ideal gas
      ZZ=P/RT/10.0D0
      LIM1=-1D20
      LIM2=0.03D0
      RHINI=1D-5
      CALL PISTFINDRH(ZZ,LIM1,LIM2,RHINI,RH1)
      IF (RH1.EQ.0.0D0) THEN
      VOL1=RT/P
      RH1=1.0D0/VOL1/10.0D0
      ELSE
      VOL1=1.0D0/RH1/10.0D0
      I1=1
      END IF
!
!----- (2) try high RH If fail, use ideal gas
!      ZZ=P/RT/10.0D0
      LIM1=0.01D0
      LIM2=1D20
      RHINI=6D-2
      CALL PISTFINDRH(ZZ,LIM1,LIM2,RHINI,RH2)
      IF (RH2.EQ.0.0D0) THEN
      VOL2=RT/P
      RH2=1.0D0/VOL2/10.0D0
      ELSE
      VOL2=1.0D0/RH2/10.0D0
      I2=1
      END IF
!
!===== if all fails (RH1=0 and RH2=0) try interval
      IF (I1.EQ.0.AND.I2.EQ.0) THEN
      RH2=0.0D0
      ZZ=P/R/T/10.0D0
      I=0
      MI=1D-7
      MA=10.0D0
      CALL INTERV(ZZ,MI,MA,I,RH2)
      I2=1
      VOL2=1.0D0/RH2/10.0D0
      END IF
!=====
      VDP1=0.0D0
      VDP2=0.0D0
!
      HOW=1
!==========================================================
!==========================================================
!----- IF HOW=1: brute force method
!==========================================================
!==========================================================
      IF (HOW.EQ.1) THEN
!=====
      IF (I1.GT.0) THEN
      CALL INTEGBET(VOL1,VOL0,PDV,I,IM)
      F1=-(VOL0-VOL1)+(P-1.0D0)*VOL1
      VDP1=PDV+F1+GF
      END IF
!
      IF (I2.GT.0) THEN
      CALL INTEGBET(VOL2,VOL0,PDV,I,IM)
      F1=-(VOL0-VOL2)+(P-1.0D0)*VOL2
      VDP2=PDV+F1+GF
      END IF
!=====
      END IF
!==========================================================
!==========================================================
!----- IF HOW=2: calulate ln(f), gas equation (no liquid)
!----- assumes that standard state and Cp are for ideal gas
!==========================================================
!==========================================================
      IF (HOW.EQ.2) THEN
!=====
!
      IF (I1.GT.0) THEN
      CALL PISTARES(RH1,ARES)
      LOF=(DLOG(RH2*10.0D0)+ARES+P/RH2/RT/10.0D0)+DLOG(RT)-1.0D0
      VDP1=GF+RT*LOF
      END IF
!
      IF (I2.GT.0) THEN
      CALL PISTARES(RH2,ARES)
      LOF=(DLOG(RH2*10.0D0)+ARES+P/RH2/RT/10.0D0)+DLOG(RT)-1.0D0
      VDP2=GF+RT*LOF
      END IF
!=====
      END IF
!==========================================================
!==========================================================
      IF (VDP1.LT.VDP2) THEN
      VRE=VOL1
      GRE=VDP1
      ELSE
      VRE=VOL2
      GRE=VDP2
      END IF
!-----
      RETURN
      END
!-----
!--------------------------------
!--------------------------------
      SUBROUTINE PS94CO2(P,T,GRE,VRE)
      IMPLICIT NONE
!-- Subroutines by CdC and Erik Düsterhoeft
!-- K.S. Pitzer and S.M. Sterner, 1994
!-- J. Chem. Phys. 101(4),1994,3111-3116
!-- critical point CO2: 31.04 C, 73.8 Bar
!-- critical density CO2: 0.010656668938878 mol/ccm
      REAL(8) CC(10),RT
      COMMON /PISTRE/ CC,RT
!--
      REAL(8) T,P,R,ZZ,MI,MA,LIM1,LIM2,RHINI, &
      VOL1,VOL2,VOL0,VDP1,VDP2,PDV,F1,RH0,RH1,RH2,VRE,GRE
      REAL(8) ARES,LOF
      INTEGER(4) I,I0,I1,I2,IM,HOW
!--
      REAL(8) GF,K1,K2,K3,K4,CPRDT,CPRTDT,H0,S0,TT,SQT,TT0,SQT0,T0
!-H2O
!      DATA H0,S0/-241.81D0,188.80D-3/
!      DATA K1,K2,K3,K4/0.0401D0,0.8656D-5,487.5D0,-0.2512D0/
!      DATA T0,TT0/298.15D0,88893.4225D0/
!-CO2
      DATA H0,S0/-393.51D0,213.70D-3/
      DATA K1,K2,K3,K4/0.0878D0,-0.2644D-5,706.4D0,-0.9989D0/
      DATA T0,TT0/298.15D0,88893.4225D0/
!--
      R=8.3143D0
      SQT0=DSQRT(T0)
      TT=T*T
      SQT=DSQRT(T)
      RT=R*T
!
      CPRDT=K1*(T-T0)+K2*(TT-TT0)/2.0D0 &
      -K3*(1.0D0/T-1.0D0/T0)+2D0*K4*(SQT-SQT0)
      CPRTDT=K1*DLOG(T/T0)+K2*(T-T0) &
      -K3*(1.0D0/(TT)-1.0D0/(TT0))/2.0D0 &
      -2D0*K4*(1.0D0/SQT-1.0D0/SQT0)
      GF=1.0D3*(H0+CPRDT-T*(S0+CPRTDT))
!
      I0=0
      I1=0
      I2=0
!----
!      CALL PISTH2ODEF(T)
      CALL PISTCO2DEF(T)
!
!----- (0) low RH at 1 Bar. If fail, use ideal gas
      ZZ=1.0D0/RT/10.0D0
      LIM1=-1D20
      LIM2=0.03D0
      RHINI=1.0D0/RT
      CALL PISTFINDRH(ZZ,LIM1,LIM2,RHINI,RH0)
      IF (RH0.EQ.0.0D0) THEN
      VOL0=RT/P
      ELSE
      VOL0=1.0D0/RH0/10.0D0
      I0=1
      END IF
!
!----- (1) try low RH If fail, use ideal gas
      ZZ=P/RT/10.0D0
      LIM1=-1D20
      LIM2=0.03D0
      RHINI=1D-5
      CALL PISTFINDRH(ZZ,LIM1,LIM2,RHINI,RH1)
      IF (RH1.EQ.0.0D0) THEN
      VOL1=RT/P
      RH1=1.0D0/VOL1/10.0D0
      ELSE
      VOL1=1.0D0/RH1/10.0D0
      I1=1
      END IF
!
!----- (2) try high RH If fail, use ideal gas
!      ZZ=P/RT/10.0D0
      LIM1=0.01D0
      LIM2=1D20
      RHINI=6D-2
      CALL PISTFINDRH(ZZ,LIM1,LIM2,RHINI,RH2)
      IF (RH2.EQ.0.0D0) THEN
      VOL2=RT/P
      RH2=1.0D0/VOL2/10.0D0
      ELSE
      VOL2=1.0D0/RH2/10.0D0
      I2=1
      END IF
!
!===== if all fails try interval
      IF (I1.EQ.0.AND.I2.EQ.0) THEN
      RH2=0.0D0
      ZZ=P/R/T/10.0D0
      I=0
      MI=1D-7
      MA=10.0D0
      CALL INTERV(ZZ,MI,MA,I,RH2)
      I2=1
      VOL2=1.0D0/RH2/10.0D0
      END IF
!=====
!=====
      VDP1=0.0D0
      VDP2=0.0D0
!
      HOW=1
!==========================================================
!==========================================================
!----- IF HOW=1: brute force method
!==========================================================
!==========================================================
      IF (HOW.EQ.1) THEN
!=====
      IF (I1.GT.0) THEN
      CALL INTEGBET(VOL1,VOL0,PDV,I,IM)
      F1=-(VOL0-VOL1)+(P-1.0D0)*VOL1
      VDP1=PDV+F1+GF
      END IF
!
      IF (I2.GT.0) THEN
      CALL INTEGBET(VOL2,VOL0,PDV,I,IM)
      F1=-(VOL0-VOL2)+(P-1.0D0)*VOL2
      VDP2=PDV+F1+GF
      END IF
!=====
      END IF
!==========================================================
!==========================================================
!----- IF HOW=2: calulate ln(f), gas equation (no liquid)
!----- assumes that standard state and Cp are for ideal gas
!==========================================================
!==========================================================
      IF (HOW.EQ.2) THEN
!=====
!
      IF (I1.GT.0) THEN
      CALL PISTARES(RH1,ARES)
      LOF=(DLOG(RH2*10.0D0)+ARES+P/RH2/RT/10.0D0)+DLOG(RT)-1.0D0
      VDP1=GF+RT*LOF
      END IF
!
      IF (I2.GT.0) THEN
      CALL PISTARES(RH2,ARES)
      LOF=(DLOG(RH2*10.0D0)+ARES+P/RH2/RT/10.0D0)+DLOG(RT)-1.0D0
      VDP2=GF+RT*LOF
      END IF
!=====
      END IF
!==========================================================
!==========================================================
      IF (VDP1.LT.VDP2) THEN
      VRE=VOL1
      GRE=VDP1
      ELSE
      VRE=VOL2
      GRE=VDP2
      END IF
!=====
!-----
      RETURN
      END
!-----
!--------------------------------
!--------------------------------
      SUBROUTINE INTERV(ZZ,MI,MA,I,RH)
      IMPLICIT NONE
      REAL(8) ZZ,MI,MA,RH,RH1,RH2,RH3,Z1,Z2,Z3,IST,DX,X,Y
      INTEGER(4) I
!----
      DX=(MA-MI)/10000.0D0
      X=MI
      DO WHILE(X.LE.MA)
       CALL PISTZ(X,Y)
      X=X+DX
      END DO
!-----
      I=0
      RH1=MI
      CALL PISTZ(RH1,Z1)
      RH2=MA
      CALL PISTZ(RH2,Z2)
      IST=0.0D0
      IF (Z1.LT.ZZ.AND.Z2.GT.ZZ) IST=1.0D0
      IF (Z1.GT.ZZ.AND.Z2.LT.ZZ) IST=-1.0D0
      IF (IST.EQ.0.0D0) THEN
       RH=0.0D0
       RETURN
      END IF
!----
      DO WHILE (DABS(Z2-Z1).GT.ZZ/1D6)
      RH=(RH1+RH2)/2.0D0
!----
       I=I+1
       IF (I.GT.100) RETURN
       RH3=(RH1+RH2)/2.0D0
       CALL PISTZ(RH3,Z3)
        IF ((Z3-ZZ)*IST.GT.0.0D0) THEN
         RH2=RH3
         Z2=Z3
        ELSE
         RH1=RH3
         Z1=Z3
        END IF
      END DO
!----
      RETURN
      END
!----
!--------------------------------
!--------------------------------
      SUBROUTINE PISTFINDRH(ZZ,LIM1,LIM2,RH1,RH)
      IMPLICIT NONE
!--
!-- initial guess for 1 Bar, and gas: V=RT, RH=1/RT
!-- falls Ableitung negativ: fuer gas:RH=RH/2
!-- optimistic limits: 10 > RH > 10-7
!-- LIM1: lower limit OF RH, LIM2: upper limit OF RH
!-- RH1 : initial; guess
      REAL(8) CC(10),RT
      COMMON /PISTRE/ CC,RT
      REAL(8) RH,RH1,RH2,Z1,Z2,ZS,DX,ZZ,DXTEST,ZS2,LIM1,LIM2
      INTEGER(4) I
!-----
      DX=100.0D0
      DXTEST=ZZ/1D6
      CALL PISTZ(RH1,Z1)
      I=0
      DO WHILE(DX.GT.DXTEST.AND.I.LT.100)
      I=I+1
       IF (DX.LT.DXTEST) GOTO 90
       CALL PISTZS(RH1,ZS)
       RH2=RH1+(ZZ-Z1)/ZS
       CALL PISTZS(RH2,ZS2)
       IF (ZS2.LT.0.0D0.OR.RH2.LT.LIM1.OR.RH2.GT.LIM2) THEN
        RH=0.0D0
        RETURN
       END IF
       DX=DABS(ZZ-Z2)
       CALL PISTZ(RH2,Z2)
       DX=DABS(ZZ-Z2)
       RH1=RH2
       Z1=Z2
      END DO
   90 CONTINUE
      RH=RH1
!=====
!----
      RETURN
      END
!----
!--------------------------------
!--------------------------------
      SUBROUTINE PISTARES(RH,ARES)
      IMPLICIT NONE
!--
      REAL(8) CC(10),RT
      COMMON /PISTRE/ CC,RT
      REAL(8) RH,RH2,RH3,RH4,F1,ARES
!-----
      RH2=RH*RH
      RH3=RH2*RH
      RH4=RH3*RH
      F1=CC(2)+CC(3)*RH+CC(4)*RH2+CC(5)*RH3+CC(6)*RH4
      ARES=CC(1)*RH+(1.0D0/F1-1.0D0/CC(2)) &
      -(CC(7)/CC(8))*(DEXP(-CC(8)*RH)-1.0D0) &
      -(CC(9)/CC(10))*(DEXP(-CC(10)*RH)-1.0D0)
!=====
!----
      RETURN
      END
!----
!--------------------------------
!--------------------------------
      SUBROUTINE PISTZ(RH,Z)
      IMPLICIT NONE
!--
      REAL(8) CC(10),RT
      COMMON /PISTRE/ CC,RT
      REAL(8) Z,RH,RH2,RH3,RH4,F1,F2
!-----
      RH2=RH*RH
      RH3=RH2*RH
      RH4=RH3*RH
      F1=CC(3)+2.0D0*CC(4)*RH+3.0D0*CC(5)*RH2+4.0D0*CC(6)*RH3
      F2=CC(2)+CC(3)*RH+CC(4)*RH2+CC(5)*RH3+CC(6)*RH4
      Z=RH+CC(1)*RH2-RH2*(F1/F2**2)+CC(7)*RH2*DEXP(-CC(8)*RH)+ &
      CC(9)*RH2*DEXP(-CC(10)*RH)
!=====
!----
      RETURN
      END
!----
!--------------------------------
!--------------------------------
      SUBROUTINE INTEGBET(VMIN,VMAX,G,I,IM)
      IMPLICIT NONE
!--
      REAL(8) CC(10),RT
      COMMON /PISTRE/ CC,RT
      REAL(8) VMIN,VMAX,G,DX,DXMIN,XU,XO,YU,YO, &
      X(100),Y(100),YPREC,F1,XD
      INTEGER(4) I,IMEGA,II,IC,IM
!----- primary interval e.g. (max-min)/100 DX
!----- minimal interval e.g. (max-min)/10000 DXMIN
!----- Y precision e.g. 1D-6
!----- COMMENT Apr. 2014: DXMIN=(VMAX-VMIN)/1.0D6 is slower but slightly better
!----- COMMENT Apr. 2014: DXMIN=(VMAX-VMIN)/1.0D4 is faster but not good at very high pressures
!----- X(10) and Y(10) probably sufficient
      G=0.0D0
      DX=(VMAX-VMIN)/100.0D0
      DXMIN=(VMAX-VMIN)/1.0D5
      YPREC=1D-5
!-- outer loop
      XU=VMIN
      CALL PISTPV(XU,YU)
      I=0
!-- begin outer loop
      DO IMEGA=1,100
      XO=XU+DX
      CALL PISTPV(XO,YO)
!====
      X(1)=XU
      Y(1)=YU
      X(2)=XO
      Y(2)=YO
      IC=2
      IM=2
!-- begin inner loop
      DO WHILE (IC.GT.1)
!-add a point
      DO II=IC,2,-1
      X(II+1)=X(II)
      Y(II+1)=Y(II)
      END DO
      X(2)=(X(1)+X(3))/2.0D0
      CALL PISTPV(X(2),Y(2))
      IC=IC+1
      I=I+1
      IF (IC.GT.IM) IM=IC
      XD=X(2)-X(1)
      F1=Y(2)-(Y(1)+Y(3))/2.0D0
      IF (DABS(F1).LT.YPREC.OR.DABS(XD).LT.DXMIN) THEN
!
!      G=G+XD*(Y(1)+Y(2))/2.0D0
!      G=G+XD*(Y(2)+Y(3))/2.0D0
!Simpson
       G=G+(X(3)-X(1))/6.0D0*(Y(1)+4.0D0*Y(2)+Y(3))
!-substract 2 points
      DO II=1,IC-2
      X(II)=X(II+2)
      Y(II)=Y(II+2)
      END DO
      IC=IC-2
      END IF
!-- end inner loop: back to add a point
      END DO
 !====
       XU=XO
       YU=YO
 !-- end outer loop
     END DO
!=====
!----
      RETURN
      END
!----
!--------------------------------
!--------------------------------
      SUBROUTINE PISTPV(V,P)
      IMPLICIT NONE
!--
      REAL(8) CC(10),RT
      COMMON /PISTRE/ CC,RT
      REAL(8) Z,RH,RH2,RH3,RH4,F1,F2,V,P
!-----
      RH=1.0D0/V/10.0D0
      RH2=RH*RH
      RH3=RH2*RH
      RH4=RH3*RH
      F1=CC(3)+2.0D0*CC(4)*RH+3.0D0*CC(5)*RH2+4.0D0*CC(6)*RH3
      F2=CC(2)+CC(3)*RH+CC(4)*RH2+CC(5)*RH3+CC(6)*RH4
      Z=RH+CC(1)*RH2-RH2*(F1/F2**2)+CC(7)*RH2*DEXP(-CC(8)*RH)+ &
      CC(9)*RH2*DEXP(-CC(10)*RH)
      P=Z*RT*10.0D0
!=====
      RETURN
      END
!----
!--------------------------------
!--------------------------------
      SUBROUTINE PISTZS(RH,ZS)
      IMPLICIT NONE
!--
      REAL(8) CC(10),RT
      COMMON /PISTRE/ CC,RT
      REAL(8) RH,ZS
!----- mit maxima
      ZS=-CC(7)*CC(8)*RH**2*DEXP(-CC(8)*RH)+2.0D0*CC(7)*RH* &
      DEXP(-CC(8)*RH)-CC(10)*CC(9)*RH**2*DEXP(-CC(10)*RH)+ &
      2.0D0*CC(9)*RH*DEXP(-CC(10)*RH)-2.0D0*RH*(4.0D0*CC(6)*RH**3+ &
      3.0D0*CC(5)*RH**2+2.0D0*CC(4)*RH+CC(3))/(CC(6)*RH**4+ &
      CC(5)*RH**3+CC(4)*RH**2+CC(3)*RH+CC(2))**2-RH**2*(12.0D0* &
      CC(6)*RH**2+6.0D0*CC(5)*RH+2.0D0*CC(4))/(CC(6)*RH**4+ &
      CC(5)*RH**3+CC(4)*RH**2+CC(3)*RH+CC(2))**2+2.0D0*RH**2* &
      (4.0D0*CC(6)*RH**3+3.0D0*CC(5)*RH**2+2.0D0*CC(4)*RH+CC(3))* &
      (4.0D0*CC(6)*RH**3+3.0D0*CC(5)*RH**2+2.0D0*CC(4)*RH+CC(3))/ &
      (CC(6)*RH**4+CC(5)*RH**3+CC(4)*RH**2+CC(3)*RH+CC(2))**3+ &
      2.0D0*CC(1)*RH+1.0D0
!=====
!----
      RETURN
      END
!----
!--------------------------------
!--------------------------------
      SUBROUTINE PISTH2ODEF(T)
      IMPLICIT NONE
!--
      REAL(8) CC(10),RT
      COMMON /PISTRE/ CC,RT
      INTEGER(4) I
      REAL(8) T,C(10,6)
!--
      C(1,1)=0.0D0
      C(1,2)=0.0D0
      C(1,3)=0.24657688D+6
      C(1,4)=0.51359951D+2
      C(1,5)=0.0D0
      C(1,6)=0.0D0
!--
      C(2,1)=0.0D0
      C(2,2)=0.0D0
      C(2,3)=0.58638965D+0
      C(2,4)=-0.28646939D-2
      C(2,5)=0.31375577D-4
      C(2,6)=0.0D0
!--
      C(3,1)=0.0D0
      C(3,2)=0.0D0
      C(3,3)=-0.62783840D+1
      C(3,4)=0.14791599D-1
      C(3,5)=0.35779579D-3
      C(3,6)=0.15432925D-7
!--
      C(4,1)=0.0D0
      C(4,2)=0.0D0
      C(4,3)=0.0D0
      C(4,4)=-0.42719875D+0
      C(4,5)=-0.16325155D-4
      C(4,6)=0.0D0
!--
      C(5,1)= 0.0D0
      C(5,2)= 0.0D0
      C(5,3)= 0.56654978D+4
      C(5,4)=-0.16580167D+2
      C(5,5)= 0.76560762D-1
      C(5,6)= 0.0D0
!--
      C(6,1)= 0.0D0
      C(6,2)= 0.0D0
      C(6,3)= 0.0D0
      C(6,4)= 0.10917883D+0
      C(6,5)= 0.0D0
      C(6,6)= 0.0D0
!--
      C(7,1)= 0.38878656D+13
      C(7,2)=-0.13494878D+9
      C(7,3)= 0.30916564D+6
      C(7,4)= 0.75591105D+1
      C(7,5)= 0.0D0
      C(7,6)= 0.0D0
!--
      C(8,1)= 0.0D0
      C(8,2)= 0.0D0
      C(8,3)=-0.65537898D+5
      C(8,4)= 0.18810675D+3
      C(8,5)= 0.0D0
      C(8,6)= 0.0D0
!--
      C(9,1)=-0.14182435D+14
      C(9,2)= 0.18165390D+9
      C(9,3)=-0.19769068D+6
      C(9,4)=-0.23530318D+2
      C(9,5)= 0.0D0
      C(9,6)= 0.0D0
!--
      C(10,1)= 0.0D0
      C(10,2)= 0.0D0
      C(10,3)= 0.92093375D+5
      C(10,4)= 0.12246777D+3
      C(10,5)= 0.0D0
      C(10,6)= 0.0D0
!====
      DO I=1,10
      CC(I)=C(I,1)/(T**4)+C(I,2)/(T**2)+C(I,3)/T+C(I,4)+ &
      C(I,5)*T+C(I,6)*T**2
      END DO
!====
      RETURN
      END
!----
!--------------------------------
!--------------------------------
      SUBROUTINE PISTCO2DEF(T)
      IMPLICIT NONE
!--
      REAL(8) CC(10),RT
      COMMON /PISTRE/ CC,RT
      INTEGER(4) I
      REAL(8) T,C(10,6)
!--
      C(1,1)= 0.0D0
      C(1,2)= 0.0D0
      C(1,3)=+0.18261340D+7
      C(1,4)=+0.79224365D+2
      C(1,5)= 0.0D0
      C(1,6)= 0.0D0
      C(2,1)= 0.0D0
      C(2,2)= 0.0D0
      C(2,3)= 0.0D0
      C(2,4)=+0.66560660D-4
      C(2,5)=+0.57152798D-5
      C(2,6)=+0.30222363D-9
!--
      C(3,1)= 0.0D0
      C(3,2)= 0.0D0
      C(3,3)= 0.0D0
      C(3,4)=+0.59957845D-2
      C(3,5)=+0.71669631D-4
      C(3,6)=+0.62416103D-8
!--
      C(4,1)= 0.0D0
      C(4,2)= 0.0D0
      C(4,3)=-0.13270279D+1
      C(4,4)=-0.15210731D+0
      C(4,5)=+0.53654244D-3
      C(4,6)=-0.71115142D-7
!--
      C(5,1)= 0.0D0
      C(5,2)= 0.0D0
      C(5,3)=+0.12456776D+0
      C(5,4)=+0.49045367D+1
      C(5,5)=+0.98220560D-2
      C(5,6)=+0.55962121D-5
!--
      C(6,1)= 0.0D0
      C(6,2)= 0.0D0
      C(6,3)= 0.0D0
      C(6,4)=+0.75522299D+0
      C(6,5)= 0.0D0
      C(6,6)= 0.0D0
!--
      C(7,1)=-0.39344644D+12
      C(7,2)=+0.90918237D+8
      C(7,3)=+0.42776716D+6
      C(7,4)=-0.22347856D+2
      C(7,5)= 0.0D0
      C(7,6)= 0.0D0
!--
      C(8,1)= 0.0D0
      C(8,2)= 0.0D0
      C(8,3)=+0.40282608D+3
      C(8,4)=+0.11971627D+3
      C(8,5)= 0.0D0
      C(8,6)= 0.0D0
!--
      C(9,1)= 0.0D0
      C(9,2)=+0.22995650D+8
      C(9,3)=-0.78971817D+5
      C(9,4)=-0.63376456D+2
      C(9,5)= 0.0D0
      C(9,6)= 0.0D0
!--
      C(10,1)= 0.0D0
      C(10,2)= 0.0D0
      C(10,3)=+0.95029765D+5
      C(10,4)=+0.18038071D+2
      C(10,5)= 0.0D0
      C(10,6)= 0.0D0
!====
      DO I=1,10
      CC(I)=C(I,1)/(T**4)+C(I,2)/(T**2)+C(I,3)/T+C(I,4)+ &
      C(I,5)*T+C(I,6)*T**2
      END DO
!====
      RETURN
      END
!---
