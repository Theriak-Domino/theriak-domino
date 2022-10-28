!-----Version: 28.05.2022
!               ************
!               * dbread.f *
!               ************
!     Subroutines for reading the databases
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
      SUBROUTINE DBREAD
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
      CHARACTER(3) SECTIO,PROVID
      CHARACTER(16) AINFO1
      COMMON /SECINFO/ SECTIO,PROVID,AINFO1
!-----END OF COMMON VARIABLES
      LOGICAL(4) IDCODE,FILEND
      INTEGER(4) I001,I,I1,II,IS,I2,I3,I4,NNPICK,DURCH,DUTOT,J,IP,IQ,IAA
!-----
!----- keep NPICK and PICK original
!----- use NPICKR and PICKR instead
! following moved to end
!      DO I=1,NPICKR
!       PICKR(I)=' '
!      END DO
!      NPICKR=0
!
      NNPICK=0
      IF (NPICK.GT.0) THEN
       NPICKR=NPICK
       DO I=1,NPICKR
        PICKR(I)=PICK(I)
!        WRITE (UNIT=6,FMT='(''PICK '',I3,2X,A16)') I,PICKR(I)
       END DO
      END IF
!----- double read if NPICKR > 0
      IF (NPICKR.GT.0) THEN
       NNPICK=NPICKR
       DUTOT=2
      ELSE
       DUTOT=1
      END IF
!=====
      DO DURCH=1,DUTOT
      IF (DURCH.EQ.1.AND.DUTOT.EQ.2) THEN
        NPICKR=0
      END IF
!+++
!      WRITE (UNIT=scr,FMT=502) (PICK(I),I=1,NPICK)
!  502 FORMAT (' origi: ',50(A16,1X))
!+++
!----
      IF (DURCH.EQ.2) THEN

      DO I1=1,SOMAX+1
       NSIEL(I1)=0
       DO I2=1,SIELMAX
         XELSI(I1,I2)=0.0D0
         NEMQQ(I1,I2)=0
         DO I3=1,SIELMAX
           EMQQ(I1,I2,I3)=0
         END DO
         DO I3=1,EMAX
           EMXX(I1,I2,I3)=0.0D0 
         END DO
       END DO
       DO I2=1,EMAX+1
         DO I3=1,SITMAX
           DO I4=1,MULMAX
             ELSI(I1,I2,I3,I4)=0
           END DO
         END DO
       END DO
      END DO
!----
      NPICKR=NNPICK
!+++ check for endmembers of solutions to choose
      DO IS=1,NSOL
       IQ=0
       DO I=1,NPICKR
        IF (VERGL(SOLNAM(IS),PICKR(I))) IQ=I
       END DO
       IF (IQ.NE.0) THEN
        DO J=IQ,NPICKR-1
         PICKR(J)=PICKR(J+1)
        END DO
        NPICKR=NPICKR-1
        DO J=1,NEND(IS)
! check that phase is not already picked
      IAA=0
      DO II=1,NPICKR
       IF (VERGL(NAME(EM(IS,J)),PICKR(II))) IAA=II
      END DO
      IF (IAA.EQ.0) THEN
!
         NPICKR=NPICKR+1
         PICKR(NPICKR)=NAME(EM(IS,J))
!
      END IF
!
        END DO
       END IF
      END DO
!+++
!+++ check for phases in COM LINES to choose
      DO IP=NUN+1,NPHA
       CALL DAREST(IP)
       IF (COM) THEN
        IQ=0
        DO I=1,NPICKR
         IF (VERGL(NAME(IP),PICKR(I))) IQ=1
        END DO
        IF (IQ.EQ.1) THEN
         DO J=1,NCOM
! check that phase is not already picked
      IAA=0
      DO II=1,NPICKR
       IF (VERGL(NAME(ICOM(J)),PICKR(II))) IAA=II
      END DO
      IF (IAA.EQ.0) THEN
!
          NPICKR=NPICKR+1
          PICKR(NPICKR)=NAME(ICOM(J))
!
      END IF
!
         END DO
        END IF
       END IF
      END DO
!+++
!      WRITE (UNIT=scr,FMT=501) (PICKR(I),I=1,NPICKR)
!  501 FORMAT (' picked: ',50(A16,1X))
!+++
      END IF
!+++
!=====
      REWIND (UNIT=dbs)
      NUN=0
      DO I=1,NC
!!        IF (CHEM(I).NE.0.0D0.OR.OXYDE(I).EQ.'E') THEN
        IF (DABS(CHEM(I)).GT.0.0D0.OR.OXYDE(I).EQ.'E') THEN
          NUN=NUN+1
          CHMCOD(NUN)=I
        END IF
      END DO
!-----
      IF (NUN.LE.0) THEN
      CALL SHOUTF
      WRITE (UNIT=scr,FMT=2000) NUN
      WRITE (UNIT=out,FMT=2000) NUN
 2000 FORMAT (// &
      ' The number of components for this system is zero')
      STOP
      END IF
!-----
      PHASID(0)='SOL'
      DO I=1,NUN
        I001=INDEX(OXYDE(CHMCOD(I)),' ')
        NAME(I)='"'//OXYDE(CHMCOD(I))(1:I001-1)//'"'
        ABK(I)=OXYDE(CHMCOD(I))(1:8)
        NULL(I)=.FALSE.
        ISOFIX(I)=0
        PHASID(I)='ELE'
        DO II=1,COMAX
          XX(I,II)=0.0D0
        END DO
        XX(I,I)=1.0D0
        EMSOL(I)=0
        EMNR(I)=0
        BULK(I)=CHEM(CHMCOD(I))
        MOLWT(I)=MOLGEW(CHMCOD(I))
        OXNUM(I)=OXANZ(CHMCOD(I))
        CHNAME(I)=OXYDE(CHMCOD(I))(1:8)
      END DO
      NPHA=NUN
      SUGNR=NUN
      SEXCL=0
      PEXCL=0
      NSED=0
!-----
! hier oxide einfuegen fuer chemical potentials
!-----
!-----START READING DATABASE
!-----
      IDCODE=.FALSE.
      FILEND=.FALSE.
      NSOL=0
!---- flag that no solution has NNEGEM assigned
!      WRITE (UNIT=scr,FMT= &
!      '('' read database: NNEGEM not assigned :('')')
      GIBBSFLAG=.FALSE.
      PROVID='XXX'
      GOTO 5011
!-----START END-FILE ACTION
 5010 RECODE='***'
      FILEND=.TRUE.
      REC='** ENDFILE **'
      GOTO 5012
!-----END OF END-FILE ACTION
 5011 DO I=1,8
        GENUG(I)=.FALSE.
      END DO
      SECTIO='BAH'
 1002 RECODE=' '
!-----SKIP BLANK AND COMMENT RECORDS
 1003 IF (RECODE.NE.' '.AND.RECODE(1:1).NE.'!') GOTO 3
      READ (UNIT=dbs,FMT='(A250)',END=5010) REC
      CALL FIBLA(REC,I1)
      IF (I1.NE.0) RECODE=REC(I1:I1+2)
      GOTO 1003
!-----
    3 IDCODE=.FALSE.
      IF (SECTIO.EQ.'MIN') IDCODE=(INDEX(REC,'(').NE.0)
      IF (SECTIO.EQ.'FLU') IDCODE=(INDEX(REC,'(').NE.0)
      IF (SECTIO.EQ.'SOL') IDCODE=(INDEX(REC,'  (').NE.0)
      IF (SECTIO.EQ.'MAR') IDCODE=(INDEX(REC,' - ').NE.0)
      IF (SECTIO.EQ.'SMA') IDCODE=(INDEX(REC,'* ').EQ.1)
      IF (SECTIO.EQ.'FIX') IDCODE=.TRUE.
      IF (SECTIO.EQ.'SED') IDCODE=.TRUE.
 5012 IF (RECODE.EQ.'***'.OR.IDCODE) THEN
      IF (GENUG(1)) THEN
      IF (SECTIO.EQ.'MIN'.OR.SECTIO.EQ.'FLU') THEN
      CALL MINFIN
      IF (GENUG(1).AND.GENUG(2).AND.GENUG(3)) CALL DASAVE(NPHA)
      END IF
      IF (SECTIO.EQ.'SOL') THEN
!!         WRITE (UNIT=6,FMT='(''CALL SOLFIN, DURCH='',I3)') DURCH
         CALL SOLFIN
      END IF
      DO I=1,8
        GENUG(I)=.FALSE.
      END DO
      END IF
      IF (RECODE.EQ.'***') THEN
      IF (PRTLOG(1)) THEN
      CALL LABLA(REC,I2)
      WRITE (UNIT=scr,FMT='(/'' '',132A1)') (REC(I:I),I=1,I2)
      WRITE (UNIT=out,FMT='(/'' '',132A1)') (REC(I:I),I=1,I2)
      END IF
      IF (FILEND.OR.INDEX(REC,'END').NE.0.OR. &
      INDEX(REC,'FINISH').NE.0) GOTO 999
      SECTIO='BAH'
      IF (INDEX(REC,'MINERAL DATA').NE.0) SECTIO='MIN'
      IF (INDEX(REC,'GAS DATA').NE.0) SECTIO='FLU'
      IF (INDEX(REC,'SOLUTION DATA').NE.0) SECTIO='SOL'
      IF (INDEX(REC,'FIXED PHASES').NE.0) SECTIO='FIX'

      IF (SECTIO.EQ.'FIX') WRITE (UNIT=6,FMT='(''JAJAJAJAJAJAJAJAJAJAJA'')')


      IF (INDEX(REC,'SEEDS').NE.0) SECTIO='SED'
      IF (INDEX(REC,'SITEMARG').NE.0) SECTIO='SMA'
      IF (INDEX(REC,'MARGULES').NE.0) THEN
      DO IS=1,NSOL
        IF (MARCOD(IS)) GOTO 5
      END DO
    5 IF (IS.LE.NSOL) THEN
      SECTIO='MAR'
      ELSE
      IF (PRTLOG(1)) THEN
      WRITE (UNIT=scr,FMT=105)
      WRITE (UNIT=out,FMT=105)
  105 FORMAT (1X,'no solution phase is expecting Margules-Parameters')
      END IF
      END IF
      ELSE
      IF (SECTIO.EQ.'BAH'.AND.PRTLOG(1)) THEN
      WRITE (UNIT=scr,FMT=106)
      WRITE (UNIT=out,FMT=106)
  106 FORMAT (1X,'Section not read: data not recognized by THERIAK')
      END IF
      END IF
      ELSE
      IF (SECTIO.EQ.'MIN'.OR.SECTIO.EQ.'FLU') CALL MINNEW
      IF (SECTIO.EQ.'SOL') CALL SOLNEW
      IF (SECTIO.EQ.'MAR') CALL MARNEW
      IF (SECTIO.EQ.'SMA') CALL SMARNEW
      IF (SECTIO.EQ.'FIX') CALL FIXNEW
      IF (SECTIO.EQ.'SED') CALL SEDNEW
      END IF
      ELSE
      IF (GENUG(1)) THEN
      IF (SECTIO.EQ.'MIN'.OR.SECTIO.EQ.'FLU') CALL MINDAT
      IF (SECTIO.EQ.'SOL') CALL SOLDAT
      IF (SECTIO.EQ.'MAR') CALL MARDAT
      IF (SECTIO.EQ.'SMA') CALL SMARDAT
      END IF
      END IF
      GOTO 1002
!-----
  999 CONTINUE
      DO IS=1,NSOL
      IF (SOLINFO(IS).EQ.' ') THEN
       IF (MODELL(IS).EQ.'I') SOLINFO(IS)='ideal'
       IF (MODELL(IS).EQ.'S') SOLINFO(IS)='site mixing'
       IF (MODELL(IS).EQ.'F') SOLINFO(IS)='external'
       IF (NMARG(IS).GT.0.OR.NSMARG(IS).GT.0) THEN
        CALL LABLA(SOLINFO(IS),I1)
        SOLINFO(IS)(I1+1:)='+margules'
       END IF
      END IF
      END DO
!-----END OF READING DATABASE
      END DO
!==== end of double read
!
!----- reset PICKR to zero
      DO I=1,NPICKR
       PICKR(I)=' '
      END DO
      NPICKR=0
!
      RETURN
      END
!-----
!******************************
      SUBROUTINE MINNEW
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
      CHARACTER(3) SECTIO,PROVID
      CHARACTER(16) AINFO1
      COMMON /SECINFO/ SECTIO,PROVID,AINFO1
!-----END OF COMMON VARIABLES
      INTEGER(4) COMAY,I,II,I001
      CHARACTER(60) REJECT,TMPNAM,TMPABK
      CHARACTER(15) USECOD
!-----
      COMAY=COMAX
      CALL TAXI(REC,TMPNAM)
      CALL TAXI(REC,FORMUL)
      CALL TAXI(REC,TMPABK)
      CALL TAXI(REC,USECOD)
      IF (USECOD.EQ.' ') USECOD='N'
      GENUG(1)=.FALSE.
      NULL(NPHA+1)=.FALSE.
!
!-----check here that NAM is not longer than 16 characters
      CALL LABLA(TMPNAM,I001)
      IF (I001.GT.16) THEN
        WRITE (6,1000) TMPNAM(1:I001),TMPNAM(1:16)
 1000 FORMAT (/' WARNING: name too long: ',A,' --> is truncated to: ',A)
!        STOP
      END IF
      NAM=TMPNAM(1:16)
!-----check here that FORM is not longer than 8 characters
      CALL LABLA(TMPABK,I001)
      IF (I001.GT.8) THEN
        WRITE (6,1005) TMPABK(1:I001),TMPABK(1:8)
 1005 FORMAT (/' WARNING: abbreviation too long: ',A,' --> is truncated to: ',A)
!        STOP
      END IF
      FORM=TMPABK(1:16)
!
!---- check first for PICKR
      IF (NPICKR.GT.0) THEN
      I001=0
      DO I=1,NPICKR
      IF (VERGL(NAM,PICKR(I))) I001=1
      END DO
!CDC2020
      DO I=1,NPICKR
      IF (VERGL('-'//NAM,PICKR(I))) I001=0
      END DO
      IF (I001.EQ.0) THEN
      GENUG(1)=.FALSE.
      REJECT='not picked'
      GOTO 11
      END IF
      END IF
!
      CALL CHEMIE(COMAY,NC,OXYDE,OXANZ,FORMUL,CHE)
!---  number of atoms for cod=V11 stored because of phases with CODE = +...
      NAT=0.0D0
      DO I=1,COMAY
        NAT=NAT+CHE(I)
      END DO
      DO I=1,LUSE
        IF (GENUG(1).OR.USE(I:I).EQ.',') GO TO 9
        GENUG(1)=(INDEX(USECOD,USE(I:I)).NE.0.OR.USE(1:1).EQ.'*')
      END DO
    9 IF (USECOD(1:1).EQ.'*') THEN
      USECOD(1:1)=','
      I001=INDEX(USECOD,' ')
      GENUG(1)=(INDEX(USE,USECOD(1:I001-1)).NE.0)
      END IF
      IF (.NOT.GENUG(1)) &
      REJECT='Code not matching : "'//USECOD//'"'
      DO II=1,NPHA
        IF (.NOT.GENUG(1)) GO TO 10
        IF (NAM.EQ.NAME(II)) THEN
          GENUG(1)=.FALSE.
          REJECT='Defined more than once in database'
        END IF
      END DO
!
!      IF (USECOD(1:1).EQ.'-') WRITE (UNIT=6,FMT=110) NAM
!  110 format (A,' uses ''-'' in code: ', &
!      'phase will be excluded from becoming stable')
!!
!      IF (USECOD(1:1).EQ.'+') WRITE (UNIT=6,FMT=112) NAM
!  112 format (A,' uses ''+'' in code: ', &
!      'phase will be included for use in COM line ', &
!      'even if outside definded space')
!
   10 DO 522,II=1,NC
      IF (.NOT.GENUG(1)) GO TO 11
!!      GENUG(1)=(CHE(II).EQ.0.0.OR.CHEM(II).NE.0.0)
      GENUG(1)=(DABS(CHE(II)).LT.1D-20.OR.DABS(CHEM(II)).GT.0.0D0)
      IF (.NOT.GENUG(1).AND.OXYDE(II).EQ.'E') GENUG(1)=.TRUE.
!=====
!---- meaning of '+' is confusing.
!---- in order to be compatible with Doug Tinkham's coding of
!---- Landau Phases, using '+' should exclude the phase from
!---- becoming stable.
!---- because of many old files still in use, this is not yet implemented.
!---- at the moment '-' excludes the phase.
!      IF (.NOT.GENUG(1).AND.USECOD(1:1).EQ.'+') THEN

      IF (USECOD(1:1).EQ.'-') THEN
!      DO 702,I=1,NC
!  702 CHE(I)=0.0D0
      GENUG(1)=.TRUE.
      NULL(NPHA+1)=.TRUE.
      GOTO 11
      END IF

      IF (.NOT.GENUG(1).AND.USECOD(1:1).EQ.'+') THEN
      DO I=1,NC
        CHE(I)=0.0D0
      END DO
      GENUG(1)=.TRUE.
      NULL(NPHA+1)=.TRUE.
      GOTO 11
      END IF

      IF (.NOT.GENUG(1)) REJECT='Composition outside defined space'
  522 CONTINUE
   11 IF (GENUG(1)) THEN
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
      COM=.FALSE.
      DIS=.FALSE.
      VO1=.TRUE.
      VO2=.FALSE.
      VO3=.FALSE.
      AQU=.FALSE.
      FIX=.FALSE.
      TL1=.FALSE.
      ELSE
      IF (PRTLOG(1)) THEN
      WRITE (UNIT=scr,FMT=107) NAM,REJECT
      WRITE (UNIT=out,FMT=107) NAM,REJECT
  107 FORMAT (' --------->',A16,' : ',A60)
      END IF
      END IF
      RETURN
      END
!-----
!******************************
      SUBROUTINE MINDAT
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
      CHARACTER(3) SECTIO,PROVID
      CHARACTER(16) AINFO1
      COMMON /SECINFO/ SECTIO,PROVID,AINFO1
!-----END OF COMMON VARIABLES
      INTEGER(4) I
      REAL(8) FF
!-----
!---- standard state
      IF (RECODE.EQ.'ST ') THEN
      READ (UNIT=REC,FMT='(5X,4D15.0)',ERR=999) G0R,H0R,S0R,V0R
      GENUG(2)=.TRUE.
!     V0R=V0R/10.0D0
      END IF
!---- Redlich-Kwong
      IF (RECODE.EQ.'R-K') THEN
      READ (UNIT=REC,FMT='(5X,4D15.8)',ERR=999) AA0,AAT,BB0,BBT
      RDK=.TRUE.
      VDW=.FALSE.
      VO3=.FALSE.
      VO1=.FALSE.
      VO2=.FALSE.
      END IF
!---- Van der Waals
      IF (RECODE.EQ.'VDW') THEN
      READ (UNIT=REC,FMT='(5X,4D15.8)',ERR=999) AA0,AAT,BB0,BBT
      VDW=.TRUE.
      RDK=.FALSE.
      VO3=.FALSE.
      VO1=.FALSE.
      VO2=.FALSE.
      END IF
!---- CORK programmed by Doug Tinkham
      IF (RECODE.EQ.'CS1') THEN
      READ (UNIT=REC,FMT='(5X,4D15.8)',ERR=999) AA0,BB0
      VOLVO=6
      VDW=.FALSE.
      RDK=.FALSE.
      VO3=.FALSE.
      VO1=.FALSE.
      VO2=.FALSE.
      END IF
!---- CORK with cubic equation
      IF (RECODE.EQ.'CS2') THEN
      READ (UNIT=REC,FMT='(5X,4D15.8)',ERR=999) AA0,BB0
      VOLVO=7
      VDW=.FALSE.
      RDK=.FALSE.
      VO3=.FALSE.
      VO1=.FALSE.
      VO2=.FALSE.
      END IF
!---- heat capacity
      IF (RECODE.EQ.'CP1'.OR.RECODE.EQ.'C1') THEN
      READ (UNIT=REC,FMT='(5X,4D15.0)',ERR=999) K1,K4,K3,K8
      GENUG(3)=.TRUE.
      END IF
      IF (RECODE.EQ.'CP3'.OR.RECODE.EQ.'C3') THEN
      READ (UNIT=REC,FMT='(5X,4D15.0)',ERR=999) K1,K2,K3,K4
      GENUG(3)=.TRUE.
      END IF
      IF (RECODE.EQ.'CP2'.OR.RECODE.EQ.'C2') THEN
      READ (UNIT=REC,FMT='(5X,5D15.0)',ERR=999) K6,K2,K5,K7,K9
      END IF
!---- disorder contributions
      IF (RECODE.EQ.'D1') THEN
      READ (UNIT=REC,FMT='(5X,5D15.0)',ERR=999) D1,D4,D3,D8,D6
      DIS=.TRUE.
      END IF
      IF (RECODE.EQ.'D2') THEN
      READ (UNIT=REC,FMT='(5X,5D15.0)',ERR=999) D2,D5,TD0,TDMAX,VADJ
      END IF
!---- aqueous phases (HKF equation)
      IF (RECODE.EQ.'AQ1') THEN
      READ (UNIT=REC,FMT='(5X,5D15.0)',ERR=999) K1,K2,K8,K9,K7
      AQU=.TRUE.
      PROVID='AQU'
      GENUG(3)=.TRUE.
      END IF
      IF (RECODE.EQ.'AQ2') THEN
      READ (UNIT=REC,FMT='(5X,5D15.0)',ERR=999) D1,D2,D3,D4
      GENUG(3)=.TRUE.
      END IF
!---- aqueous phases (Phreeqc)
      IF (RECODE.EQ.'AQP') THEN
      READ (UNIT=REC,FMT='(5X,5D15.0)',ERR=999) D1,D2
      AQU=.TRUE.
      PROVID='AQP'
      GENUG(3)=.TRUE.
      END IF
!---- aqueous phases (Holland and Powell 2011, not used)
      IF (RECODE.EQ.'HQ1') THEN
      READ (UNIT=REC,FMT='(5X,D15.0)',ERR=999) D1
      AQU=.TRUE.
      PROVID='AQ2'
      GENUG(3)=.TRUE.
      END IF
!---- special cases
      IF (RECODE.EQ.'SPC'.OR.RECODE.EQ.'EXT') THEN
      CALL TAXI(REC,CH)
      CALL TAXI(REC,FALL)
      SPC=.TRUE.
      GENUG(2)=.TRUE.
      GENUG(3)=.TRUE.
      END IF
!---- combination of phases
      IF (RECODE.EQ.'COM') THEN
      CALL TAXI(REC,CH)
      CALL TAXI(REC,FORMUL)
      CALL GELI(REC,FF)
      CALL TAXI(REC,AINFO1)
      COM=.TRUE.
      GENUG(2)=.TRUE.
      GENUG(3)=.TRUE.
      IF (FF.GT.0.0D0) S0R=-R*DLOG(FF)
      END IF
!---- lambda transitions 1 (Berman and Brown 1985)
      IF (RECODE.EQ.'TR1'.OR.RECODE.EQ.'T1') THEN
      TRTYP=0
      NLANDA=NLANDA+1
      IF (NLANDA.GT.4) THEN
      WRITE (scr,1010) NAM
 1010 FORMAT (//' more than 4 transitions for phase ',A16)
      STOP
      END IF
      I=NLANDA
      READ (UNIT=REC,FMT='(5X,5D15.0)',ERR=999) TQ1B(I),TRE(I),ASPK(I), &
      BSPK(I),DHTR(I)
      END IF
      IF (RECODE.EQ.'TR2'.OR.RECODE.EQ.'T2') THEN
      IF (NLANDA.GT.0) THEN
      I=NLANDA
      READ (UNIT=REC,FMT='(5X,4D15.0)',ERR=999) TEQ(I),DVTR(I),DVDT(I), &
      DVDP(I)
      END IF
      END IF
!---- lambda transitions 2 (Helgesson et al. 1978, SUPCRT)
      IF (RECODE.EQ.'CSK') THEN
      TRTYP=1
      GENUG(3)=.TRUE.
      VO3=.FALSE.
      VO1=.FALSE.
      VO2=.FALSE.
      NLANDA=NLANDA+1
      IF (NLANDA.GT.4) THEN
      WRITE (scr,1012) NAM
 1012 FORMAT (//' more than 4 transitions for phase ',A16)
      STOP
      END IF
      I=NLANDA
      READ (UNIT=REC,FMT='(5X,7D15.0)',ERR=999) ASPK(I),BSPK(I),DVDP(I), &
      TQ1B(I),DHTR(I),DVTR(I),TEQ(I)
      END IF
!---- lambda transitions ?
      IF (RECODE.EQ.'TL1') THEN
      READ (UNIT=REC,FMT='(5X,2D15.0)',ERR=999) TKRI,SMA
      TL1=.TRUE.
      END IF
!---- volume function 1 (as in TWQ)
      IF (RECODE.EQ.'V1') THEN
      READ (UNIT=REC,FMT='(5X,4D15.0)',ERR=999) VTA,VTB,VPA,VPB
      VTA=VTA/100000.0D0*V0R
      VTB=VTB/100000.0D0*V0R
      VPA=VPA/100000.0D0*V0R
      VPB=VPB/100000000.0D0*V0R
      VO1=.TRUE.
      VO2=.FALSE.
      VO3=.FALSE.
      END IF
!---- volume function Erik Duesterhoeft
      IF (RECODE.EQ.'VE') THEN
      READ (UNIT=REC,FMT='(5X,4D15.0)',ERR=999) VTA,VTB,VPA,VPB
      VOLVO=11
      VTA=VTA/100000.0D0
      VTB=VTB/100000.0D0
      VPA=VPA/100000.0D0
      VPB=VPB/100000000.0D0
      VO1=.FALSE.
      VO2=.FALSE.
      VO3=.FALSE.
      END IF
!---- volume function 2 (alpha = a + bT, beta = const.)
      IF (RECODE.EQ.'V2') THEN
      READ (UNIT=REC,FMT='(5X,3D15.0)',ERR=999) VAA,VAB,VB
      VO2=.TRUE.
      VO1=.FALSE.
      VO3=.FALSE.
      END IF
!---- volume function 3 (experimental, do not use)
      IF (RECODE.EQ.'V3') THEN
      READ (UNIT=REC,FMT='(5X,4D15.0)',ERR=999) VL0,VLA,VLN,VL2
      VO3=.TRUE.
      VO1=.FALSE.
      VO2=.FALSE.
      END IF
!---- volume function (Holland and Powell 1998)
      IF (RECODE.EQ.'VHP') THEN
      READ (UNIT=REC,FMT='(5X,5D15.0)',ERR=999) VTA,VTB,TKRI,SMA,VPA
      VOLVO=4
      VO3=.FALSE.
      VO1=.FALSE.
      VO2=.FALSE.
      END IF
      IF (RECODE.EQ.'VH2') THEN
      READ (UNIT=REC,FMT='(5X,5D15.0)',ERR=999) D1,D2,D3
      VOLVO=4
      VO3=.FALSE.
      VO1=.FALSE.
      VO2=.FALSE.
      END IF
!---- volume function (Holland and Powell 2011)
      IF (RECODE.EQ.'V11') THEN
      READ (UNIT=REC,FMT='(5X,6D15.0)',ERR=999) VAA,D1,D2,D3,VLN,VL2
      VOLVO=5
      VO3=.FALSE.
      VO1=.FALSE.
      VO2=.FALSE.
      END IF
!---- volume function (SIMPLIFIED Holland and Powell 2011)
      IF (RECODE.EQ.'EX1') THEN
      READ (UNIT=REC,FMT='(5X,4D15.0)',ERR=999) VAA,D1,D2,D3
      VOLVO=10
      VO3=.FALSE.
      VO1=.FALSE.
      VO2=.FALSE.
      END IF
!---- Bragg-Williams (Holland and Powell 2011)
      IF (RECODE.EQ.'BW1') THEN
      READ (UNIT=REC,FMT='(5X,6D15.0)',ERR=999) D4,D5,D6,D7,D8,D9
      VOLVO=5
      VO3=.FALSE.
      VO1=.FALSE.
      VO2=.FALSE.
      END IF
!---- volume function (Gottschalk 1997)
      IF (RECODE.EQ.'VG') THEN
      READ (UNIT=REC,FMT='(5X,2D15.0)',ERR=999) VTA,VTB
      VOLVO=8
      VO3=.FALSE.
      VO1=.FALSE.
      VO2=.FALSE.
      END IF
!---- Landau (Holland and Powell 2011)
      IF (RECODE.EQ.'LA1') THEN
      READ (UNIT=REC,FMT='(5X,3D15.0)',ERR=999) TKRI,SMA,VPA
      VOLVO=5
      VO3=.FALSE.
      VO1=.FALSE.
      VO2=.FALSE.
      END IF
!---- dk0dt for liquids (Holland and Powell 2011)
      IF (RECODE.EQ.'DK1') THEN
      READ (UNIT=REC,FMT='(5X,D15.0)',ERR=999) VL0
      VOLVO=5
      VO3=.FALSE.
      VO1=.FALSE.
      VO2=.FALSE.
      END IF
!----
      RETURN
  999 CALL SHOUTD
      WRITE (UNIT=scr,FMT=1000) RECODE,NAM
      WRITE (UNIT=out,FMT=1000) RECODE,NAM
 1000 FORMAT (//' Troubles reading line: ',A3,' for phase: ',A16// &
      ' check fieldwidths, multiple decimal points, O and 0,'/ &
      ' or similar errors. (all input formats are: (5X,nD15.0)')
      STOP
      END
!-----
!******************************
      SUBROUTINE MINFIN
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
      CHARACTER(3) SECTIO,PROVID
      CHARACTER(16) AINFO1
      COMMON /SECINFO/ SECTIO,PROVID,AINFO1
!-----END OF COMMON VARIABLES
      INTEGER(4) I,JX,I1,I2,II,I3
      REAL(8) FF,YY(COMAX)
      CHARACTER(500) MODEL1
      CHARACTER(16) EMNAM
!-----
      IF (GENUG(1).AND.GENUG(2).AND.GENUG(3)) THEN
      NPHA=NPHA+1
      IF (NPHA.GT.PHMAX) THEN
      CALL SHOUTF
      WRITE (UNIT=scr,FMT=100) PHMAX,NAM
      WRITE (UNIT=out,FMT=100) PHMAX,NAM
  100 FORMAT (// &
      ' The maximum number of phases is set to: ',I5/ &
      ' This limit was exeeded with the phase: ',A16/ &
      ' possible actions:'/ &
      '   a) Increase the value of PHMAX in the parameter statements'/ &
      '   b) Check the stabilities in smaller subsystems and then'/ &
      '      exclude (or delete) the unstable phases.')
      STOP
      END IF
!-----
      IF (COM) THEN
      NCOM=0
      CALL FIBLA(FORMUL,I1)
 2001 IF (I1.EQ.0) GOTO 71
      I2=INDEX(FORMUL,'[')
      JX=INDEX(FORMUL,']')
      IF (I2.LE.I1) I2=I1+1
      IF (JX.LE.I2+1) JX=I2+2
      EMNAM=FORMUL(I1:I2-1)
      DO I=1,(NPHA-1)
        IF (EMNAM.EQ.NAME(I)) GOTO 72
      END DO
   72 IF (I.EQ.NPHA) THEN
      CALL SHOUTD
      WRITE (UNIT=scr,FMT=300) NAM,EMNAM,NAM
      WRITE (UNIT=out,FMT=300) NAM,EMNAM,NAM
  300 FORMAT (//' Troubles reading the "COM" line of phase: ',A16/ &
      ' The phase: ',A16,' was not found in the database'/ &
      ' possible reasons:'/ &
      '   a) The phase is not in the database'/ &
      '   b) The phase is excluded by "USE"-variable or composition'/ &
      '   c) The phase is located in the database after ',A16)
      STOP
      END IF
      MODEL1=FORMUL(I2+1:JX-1)
!     READ (UNIT=MODEL1,FMT='(BN,D16.0)',ERR=999) FF
      CALL GELI(MODEL1,FF)
      NCOM=NCOM+1
!dc
      IF (EMNAM.EQ.AINFO1) NULL(I)=.TRUE.
!dc
      ICOM(NCOM)=I
      FFCOM(NCOM)=FF
      MODEL1=FORMUL
      FORMUL=MODEL1(JX+1:)
      CALL FIBLA(FORMUL,I1)
      GOTO 2001
      CALL SHOUTF
      WRITE (UNIT=scr,FMT=302) NAM
      WRITE (UNIT=out,FMT=302) NAM
  302 FORMAT (//' Troubles reading the "COM" line of phase: ',A16)
      CALL LABLA(FORMUL,II)
      WRITE (UNIT=scr,FMT=303) (FORMUL(I:I),I=1,II)
      WRITE (UNIT=out,FMT=303) (FORMUL(I:I),I=1,II)
  303 FORMAT (/' Remaining record: ',170A1)
      CALL LABLA(MODEL1,II)
      WRITE (UNIT=scr,FMT=304) (MODEL1(I:I),I=1,II)
      WRITE (UNIT=out,FMT=304) (MODEL1(I:I),I=1,II)
  304 FORMAT (' The following string cannot be converted', &
      ' to a real number: ',250A1)
      STOP
      END IF
!-----
   71 CONTINUE
      INDIVI(NPHA,1)=0.0D0
      INDIVI(NPHA,2)=0.0D0
      SUGNR=SUGNR+1
      NAME(NPHA)=NAM
      IF (FORM.NE.' ') THEN
      ABK(NPHA)=FORM(1:8)
      ELSE
      ABK(NPHA)=NAM(1:8)
      END IF
      PHASID(NPHA)=SECTIO
      ISOFIX(NPHA)=0
      IF (AQU) THEN
      NULL(NPHA)=.TRUE.
!      PHASID(NPHA)='AQU'
      PHASID(NPHA)=PROVID
      IF (PROVID.EQ.'AQP') THEN
      INDIVI(NPHA,1)=D1
      INDIVI(NPHA,2)=D2
      END IF
      END IF
!=====
      IF (NAM(1:1).EQ.'$') THEN
       NULL(NPHA)=.TRUE.
       PEXCL=PEXCL+1
      END IF
!=====
      DO I=1,NUN
        XX(NPHA,I)=CHE(CHMCOD(I))
      END DO
!**** check composition of COM phases
      IF (PRTLOG(1)) THEN
      IF (COM) THEN
       DO I=1,NUN
        YY(I)=0.0D0
       END DO
      DO I1=1,NCOM
       DO I2=1,NUN
        YY(I2)=YY(I2)+XX(ICOM(I1),I2)*FFCOM(I1)
       END DO
      END DO
      I3=0
      DO I=1,NUN
!       IF (XX(NPHA,I).NE.YY(I)) I3=I3+1
       IF (DABS(XX(NPHA,I)-YY(I)).GT.1D-10) I3=I3+1
      END DO
      IF (I3.GT.0) THEN
      WRITE (UNIT=scr,FMT=310) NAM
      WRITE (UNIT=out,FMT=310) NAM
  310 FORMAT (1X,A16,' check composition')  
      WRITE (UNIT=scr,FMT=311) (CHNAME(I),I=1,NUN)
      WRITE (UNIT=out,FMT=311) (CHNAME(I),I=1,NUN)
  311 FORMAT (13X,20(A3,2X))  
      WRITE (UNIT=scr,FMT=312) (XX(NPHA,I),I=1,NUN)
      WRITE (UNIT=out,FMT=312) (XX(NPHA,I),I=1,NUN)
  312 FORMAT (1X,'is        ',20F5.1)
      WRITE (UNIT=scr,FMT=313) (YY(I),I=1,NUN)
      WRITE (UNIT=out,FMT=313) (YY(I),I=1,NUN)
  313 FORMAT (1X,'should be ',20F5.1)
      STOP
      END IF
      END IF
      END IF
!****
      EMSOL(NPHA)=0
      EMNR(NPHA)=0
      IF (PRTLOG(1)) THEN
      WRITE (UNIT=scr,FMT='(1X,A16,'' O.K.'')') NAM
      WRITE (UNIT=out,FMT='(1X,A16,'' O.K.'')') NAM
      END IF
      ELSE
      IF (PRTLOG(1)) THEN
      WRITE (UNIT=scr,FMT=101) NAM
      WRITE (UNIT=out,FMT=101) NAM
  101 FORMAT (' --------->',A16,' : not enough data')
      END IF
      END IF
      RETURN
      END
!-----
!******************************
      SUBROUTINE SOLNEW
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
      CHARACTER(3) SECTIO,PROVID
      CHARACTER(16) AINFO1
      COMMON /SECINFO/ SECTIO,PROVID,AINFO1
!-----END OF COMMON VARIABLES
      CHARACTER(250) CH001
      CHARACTER(60) TMPSOL
      CHARACTER(8) CH8
      CHARACTER(1) CH1
      INTEGER(4) I,K,I2,I3,LL,I001
!-----
      TMPSOL=' '
      K=NSOL+1
      NEND(K)=0
      SOLINFO(K)=' '
      EXSOL(K)=.FALSE.
      LAAR(K)=.FALSE.
      CALL TAXI(REC,TMPSOL)
      CALL TAXI(REC,MODCOD)
!
!-----check here that SONAM is not longer than 16 characters
      CALL LABLA(TMPSOL,I001)
      IF (I001.GT.16) THEN
        WRITE (6,1000) TMPSOL(1:I001),TMPSOL(1:16)
 1000 FORMAT (/' WARNING: solutionname too long: ',A,' --> is truncated to: ',A)
!        STOP
      END IF
      SONAM=TMPSOL(1:16)
!
      IF (INDEX(MODCOD,'SKIP').NE.0) THEN
      IF (PRTLOG(1)) THEN
      WRITE (UNIT=scr,FMT=108) SONAM
      WRITE (UNIT=out,FMT=108) SONAM
  108 FORMAT (' --------->',A16,' : explicitly excluded')
      END IF
      GOTO 40
      END IF
!-----
      IF (INDEX(MODCOD,'EXCL').NE.0) THEN
       EXSOL(K)=.TRUE.
       SEXCL=SEXCL+1
      END IF
!-----
      GENUG(1)=.TRUE.
!++++
      NSITE(K)=0
      CH001=' '
      CALL LABLA(REC,I2)
      IF (I2.NE.0) THEN
      LL=1
      DO 550,I=1,I2
      CH1=REC(I:I)
      IF (CH1.EQ.'('.OR.CH1.EQ.')'.OR.CH1.EQ.':'.OR.CH1.EQ.',') THEN
       CH001(LL:LL+1)='  '
       LL=LL+2
      ELSE
        IF (CH1.EQ.'-') THEN
          CH001(LL:LL+4)='  -  '
          LL=LL+5
        ELSE
          CH001(LL:LL)=CH1
          LL=LL+1
        END IF
      END IF
  550 CONTINUE
!+++++
    1 CALL TAXI(CH001,CH8)
      IF (CH8.EQ.' ') GOTO 3
      NSITE(K)=NSITE(K)+1
!-----
      IF (NSITE(K).GT.SITMAX) THEN
      CALL SHOUTD
      WRITE (UNIT=scr,FMT=109) SITMAX,SONAM,NSITE(K)
      WRITE (UNIT=out,FMT=109) SITMAX,SONAM,NSITE(K)
  109 FORMAT (//' The maximum number of sites in a solution phase', &
      ' is set to ',I5/ &
      ' The solution: ',A16,' has ',I5,' sites'/ &
      ' possible actions:'/ &
      '   a) Increase the value of SITMAX in the parameter statements'/ &
      '   b) If not all endmembers are considered, the solution may'/ &
      '      be re-defined with less mixing sites')
      STOP
      END IF
!-----
      CHSITE(K,NSITE(K))=CH8
      CALL GELI(CH001,SITMUL(K,NSITE(K)))
      IF (SITMUL(K,NSITE(K)).LE.0.0D0) SITMUL(K,NSITE(K))=1.0D0
      I3=0
    2 CALL TAXI(CH001,CH8)
      IF (CH8.EQ.'-') GOTO 1
      IF (CH8.EQ.' ') GOTO 3
      I3=I3+1
      NELPS(K,NSITE(K))=I3
      ELONS(K,NSITE(K),I3)=CH8
      GOTO 2
    3 CONTINUE
      END IF
!++++
   40 CONTINUE
      RETURN
      END
!-----
!******************************
      SUBROUTINE SOLDAT
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
      CHARACTER(3) SECTIO,PROVID
      CHARACTER(16) AINFO1
      COMMON /SECINFO/ SECTIO,PROVID,AINFO1
!-----END OF COMMON VARIABLES
      INTEGER(4) I,II,III,K,I3,IL,I2,LL,IM
      CHARACTER(16) EMNAM
      CHARACTER(250) CH001,REC2
      CHARACTER(1) CH1
      REAL(8) FF
!-----
      K=NSOL+1
      II=NEND(K)+1
      VLAA(K,II)=0.0D0
      VLAA0(K,II)=0.0D0
      VLAAT(K,II)=0.0D0
      VLAAP(K,II)=0.0D0
      CALL TAXI(REC,EMNAM)
!-----
      IF (EMNAM.EQ.'REF') THEN
       CALL FIBLA(REC,I2)
       SOLINFO(K)=REC(I2:)
       RETURN
      END IF
!-----
      CALL TAXI(REC,REC2)
      CALL GELI(REC,FF)
      IF (DABS(FF).GT.0.0D0) THEN
      VLAA0(K,II)=FF
      CALL GELI(REC,FF)
      VLAAT(K,II)=FF
      CALL GELI(REC,FF)
      VLAAP(K,II)=FF
      LAAR(K)=.TRUE.
      END IF
!+++++
      CH001=' '
      CALL LABLA(REC2,I2)
      IF (I2.NE.0) THEN
      LL=1
      DO 551,I=1,I2
      CH1=REC2(I:I)
      IF (CH1.EQ.'-'.OR.CH1.EQ.',') THEN
      CH001(LL:LL+1)='  '
      LL=LL+2
      ELSE
      CH001(LL:LL)=CH1
      LL=LL+1
      END IF
  551 CONTINUE
      END IF
!+++++
      DO I=1,NSITE(K)
      DO IM=1,IDINT(SITMUL(K,I))
        CALL TAXI(CH001,CH)
        I3=0
        DO IL=1,NELPS(K,I)
          IF (CH(1:8).EQ.ELONS(K,I,IL)) THEN
            I3=IL
            GOTO 12
          END IF
        END DO
   12 ELSI(K,II,I,IM)=I3
!-----
        IF (I3.EQ.0) THEN
          CALL SHOUTD
          WRITE (UNIT=scr,FMT=109) SONAM,EMNAM,CH(1:8)
          WRITE (UNIT=out,FMT=109) SONAM,EMNAM,CH(1:8)
  109 FORMAT (//' Solution: ',A16,' endmember: ',A16/ &
      ' the element "',A8,'" was not defined for this solution.')
          STOP
        END IF
!-----
      END DO
      END DO
      DO III=1,NPHA
       IF (NAME(III).EQ.EMNAM.AND.(.NOT.NULL(III))) GO TO 15
      END DO
   15 IF (III.LE.NPHA) THEN
      NEND(K)=NEND(K)+1
!-----
      IF (NEND(K).GT.EMAX) THEN
      CALL SHOUTD
      WRITE (UNIT=scr,FMT=110) EMAX,SONAM,NEND(K)
      WRITE (UNIT=out,FMT=110) EMAX,SONAM,NEND(K)
  110 FORMAT (//' The maximum number of endmembers in a solution', &
      ' phase is set to ',I5/ &
      ' The solution: ',A16,' has ',I5,' endmembers'/ &
      ' possible actions:'/ &
      '   a) Increase the value of EMAX in the parameter statements'/ &
      '   b) Very unstable endmembers may be omitted. (carefully)')
      STOP
      END IF
!-----
      IF (EXSOL(K)) NULL(III)=.TRUE.
!-----
      EM(K,NEND(K))=III
      IF (PRTLOG(1)) THEN
      WRITE (UNIT=scr,FMT=111) SONAM,EMNAM
      WRITE (UNIT=out,FMT=111) SONAM,EMNAM
  111 FORMAT (1X,A16,': ',A16,' O.K.')
      END IF
      ELSE
      IF (PRTLOG(1)) THEN
      WRITE (UNIT=scr,FMT=112) SONAM,EMNAM
      WRITE (UNIT=out,FMT=112) SONAM,EMNAM
  112 FORMAT (1X,A16,' --------->',A16,' not a considered phase')
      END IF
      END IF
      RETURN
      END
!-----
!******************************
      SUBROUTINE SOLFIN
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
      CHARACTER(3) SECTIO,PROVID
      CHARACTER(16) AINFO1
      COMMON /SECINFO/ SECTIO,PROVID,AINFO1
!-----END OF COMMON VARIABLES
      INTEGER(4) I,II,IE,IR001(EMAX),I001,I1,K,IP,IM, &
      IP1,II1,JX,IL,LL
      REAL(8) FF
      CHARACTER(16) CH16
      CHARACTER(50) CH50
!====
!---- IS=solution, IE=endmember, II=site, IM=sitmul
!---- from SOLNEW
!----      I4  NELPS(IS,II) number of different elements in site II
!----      C8  CHSITE(IS,II) sitename of site II
!----      C8  ELONS(IS,II,K) elements on site II  (K=1 to NELPS(IS,II))
!----
!---- from SOLDAT
!----      I4  ELSI(IS,IE,II,IM) elements for endmember IE on site II (IM=1 to SITMUL(IS,II))
!----                            ELSI is index from ELONS
!---- from SOLFIN
!----      I4  NSIEL(IS) total number of elements over all sites
!----      C16 SIEL(IS,JX) name of elements (JX=1 to NSIEL(IS))
!----      I4  NEMQQ(IS,JX) number of endmebers with element JX on same site
!----      I4  EMQQ(IS,JX,IK) endmembers with element JX on same site (IK=1 to NEMQQ)
!----      R8  EMXX(IS,JX,IE) numer of elements JX on same site for endmember IE / SITMUL
!====
!----  as used for Margules
!----      I4 RANGE(IS,IP) number of endmembers involved in parameter
!----      I4 POLY(IS,IP) polynomial degree of parameter
!----      I4 INDX(IS,IP,J) index (endmember) of parameter (j=1 to POLY)
!====
!----  as used for site Margules
!----  IP=Marguls parameter, J=index in polynomial (J=1 o SMPOLY(IS,IP))
!----      I4 SMPOLY(IS,IP) polynomial degree for parameter IP
!----      I4 SMSIT(IS,IP) site number for site Margules IP
!---       I4 SMEI(IS,IP,J) index of element on site SMSIT
!----      R8 XEL(IS,II,K) concentraion of element K on site II of phase IS
!----      R8 SIFEL(IS,II,K,IE) as above for endmemmber IE
!----  new:
!----      I4 SINDX(IS,IP,X) index for parameters (1 to NSIEL)
!====
!-----
      K=NSOL+1
      IF (NEND(K).GT.1) THEN
      NSOL=K
      IF (NSOL.GT.SOMAX) THEN
      CALL SHOUTD
      WRITE (UNIT=scr,FMT=110) SOMAX,SONAM
      WRITE (UNIT=out,FMT=110) SOMAX,SONAM
  110 FORMAT (//' The maximum number of solution phases is', &
      ' set to ',I5/ &
      ' This limit was exeeded with the phase: ',A16/ &
      ' possible action:'/ &
      '   a) Increase the value of SOMAX in the parameter statements')
      STOP
      END IF
!-----
      ALPHA(K)=1.0D0
!dC      NONEG(K)=.FALSE.
      NONEG(K)=.TRUE.
      DO I=1,15
       SPAR(K,I)=0.0D0
      END DO
      ALPDIV(K)=' '
      CH50=' '
      I1=INDEX(MODCOD,')')
      IF (I1.NE.0) CH50=MODCOD(I1+1:)
      CALL FIBLA(CH50,I1)
      IF (I1.NE.0) THEN
      CALL GELI(CH50,FF)
      IF (FF.GT.0.0D0) ALPHA(K)=FF
      END IF
!dC      IF (INDEX(MODCOD,'+').NE.0) NONEG(K)=.TRUE.
      IF (INDEX(MODCOD,'-').NE.0) NONEG(K)=.FALSE.
      IF (INDEX(MODCOD,'SITE').NE.0) MODELL(K)='S'
      IF (NSITE(K).EQ.0.AND.INDEX(MODCOD,'SITE').NE.0) MODCOD=' '
      IF (INDEX(MODCOD,'SITE').EQ.0.AND.INDEX(MODCOD,'EXT').EQ.0) &
       MODELL(K)='I'
!-----
!!testcdc
!      IS=K
!      WRITE (UNIT=6,FMT='(''TEST IN SOLFIN'')')
!      WRITE (UNIT=6,FMT='(/,''NSIEL='',I3)') NSIEL
!      DO I=1,20
!        WRITE (UNIT=6,FMT='('' I,SIEL(IS,I)'',I5,2X,A16)') I,SIEL(IS,I)
!        WRITE (UNIT=6,FMT='(''I,NEMQQ(IS,I)'',A16)') I,NEMQQ(IS,I)
!!        DO II=1,5
!!         WRITE (UNIT=6,FMT='(''EMQQ(IS,I,II)(II=1,5)'',20I3)') EMQQ(IS,I,II)
!!        END DO
!!        DO II=1,10
!!         WRITE (UNIT=6,FMT='(''EMXX(IS,I,II)(II=1,10)'',20F20.10)') EMXX(IS,I,II)
!!        END DO
!!
!      END DO
!=====
!-----find effective NSITE,NELPS and ELONS etc.
      II=1
   10 IF (II.GT.NSITE(K)) GOTO 22
      IP=1
   12 IF (IP.GT.NELPS(K,II)) GOTO 20
      I001=0
      DO IE=1,NEND(K)
      DO  IM=1,IDINT(SITMUL(K,II))
        IF (ELSI(K,IE,II,IM).EQ.IP) THEN
          I001=1
          GOTO 13
        END IF
      END DO
      END DO
   13 IF (I001.EQ.0) THEN
        DO IP1=IP,NELPS(K,II)-1
          ELONS(K,II,IP1)=ELONS(K,II,IP1+1)
        END DO
        NELPS(K,II)=NELPS(K,II)-1
        DO IE=1,NEND(K)
        DO IM=1,IDINT(SITMUL(K,II))
         IF (ELSI(K,IE,II,IM).GT.IP) &
          ELSI(K,IE,II,IM)=ELSI(K,IE,II,IM)-1
        END DO
        END DO
      ELSE
        IP=IP+1
      END IF
      GOTO 12
   20 CONTINUE
      IF (NELPS(K,II).LE.1) THEN
      DO II1=II,NSITE(K)-1
        SITMUL(K,II1)=SITMUL(K,II1+1)
        CHSITE(K,II1)=CHSITE(K,II1+1)
        NELPS(K,II1)=NELPS(K,II1+1)
        DO IP1=1,NELPS(K,II1)
          ELONS(K,II1,IP1)=ELONS(K,II1+1,IP1)
        END DO
        DO IE=1,NEND(K)
        DO IM=1,IDINT(SITMUL(K,II1))
          ELSI(K,IE,II1,IM)=ELSI(K,IE,II1+1,IM)
        END DO
        END DO
      END DO
      NSITE(K)=NSITE(K)-1
      ELSE
      II=II+1
      END IF
      GOTO 10
   22 CONTINUE
!DC   IF (NSITE(K).EQ.1.AND.NELPS(K,1).EQ.NEND(K)
!DC  >.AND.MODELL(K).EQ.'I') ALPHA(K)=SITMUL(K,1)
!-----
!-----define EMQQ, EMXX, and SIEL etc.
      JX=0
      DO II=1,NSITE(K)
      DO IL=1,NELPS(K,II)
        JX=JX+1
        CH16=ELONS(K,II,IL)
        CALL LABLA(CH16,LL)
        CH16(LL+1:LL+1)='('
        CH16(LL+2:)=CHSITE(K,II)
        CALL LABLA(CH16,LL)
        CH16(LL+1:LL+1)=')'
        SIEL(K,JX)=CH16
        NEMQQ(K,JX)=0
        DO IE=1,NEND(K)
          FF=0.0D0
          DO IM=1,IDINT(SITMUL(K,II))
            IF (ELSI(K,IE,II,IM).EQ.IL) FF=FF+1.0D0
          END DO
          IF (FF.GT.0.0D0) THEN
            NEMQQ(K,JX)=NEMQQ(K,JX)+1
            EMQQ(K,JX,NEMQQ(K,JX))=IE
            EMXX(K,JX,IE)=FF/SITMUL(K,II)
            SIFEL(K,II,IL,IE)=FF/SITMUL(K,II)
          END IF
        END DO
      END DO
      END DO
      NSIEL(K)=JX
!-----
      IF (INDEX(MODCOD,'EXT').NE.0) THEN
      MODELL(K)='F'
      DO IE=1,NEND(K)
        MANAM(IE)=NAME(EM(K,IE))
      END DO
      CALL SOLINI(SONAM,NEND(K),MANAM,NEMBAS(K),IR001)
      IF (NEMBAS(K).GT.0) THEN
        DO IE=1,NEMBAS(K)
          EMBCOD(K,IE)=IR001(IE)
        END DO
      END IF
      IF (NEMBAS(K).LT.0) THEN
      CALL SHOUTF
      WRITE (UNIT=scr,FMT=102) SONAM,NAME(EM(K,-NEMBAS(K)))
      WRITE (UNIT=out,FMT=102) SONAM,NAME(EM(K,-NEMBAS(K)))
  102 FORMAT (1X,A16,' :there is no data for ',A16, &
      ' in external subroutine')
      STOP
      END IF
      END IF
!-----
      ALPHA0(K)=ALPHA(K)
      IF ((MODELL(K).EQ.'I'.OR.MODELL(K).EQ.'S').AND. &
      ALPDIV(K).NE.' ') THEN
      CALL GELI(ALPDIV(K)(2:),FF)
      IF (DABS(FF).LT.1D-20) FF=1.0D0
      ALPHA(K)=ALPHA(K)/FF
      END IF
      SOLNAM(NSOL)=SONAM
      SOSKIP(NSOL)=0
      DO I=1,NEND(NSOL)
        EMSOL(EM(NSOL,I))=NSOL
        EMNR(EM(NSOL,I))=I
      END DO
      IF (PRTLOG(1)) THEN
      WRITE (UNIT=scr,FMT=103) SONAM,(NAME(EM(K,I)),I=1,NEND(K))
      WRITE (UNIT=out,FMT=103) SONAM,(NAME(EM(K,I)),I=1,NEND(K))
  103 FORMAT (1X,A16,' O.K., endmembers: ',100(5(2X,A16)/35X))
      END IF
      MARCOD(NSOL)=(INDEX(MODCOD,'MARGULES').NE.0)
      NMARG(NSOL)=0
      NSMARG(NSOL)=0
      NSTREM(NSOL)=0
!-----
!=====the following if NEND(K).LE.1
      ELSE
      IF (PRTLOG(1)) THEN
      WRITE (UNIT=scr,FMT=104) SONAM
      WRITE (UNIT=out,FMT=104) SONAM
  104 FORMAT (' --------->',A16,' : less than two endmembers')
      END IF
      END IF
!=====
      IF (PRTLOG(1)) THEN
      WRITE (UNIT=scr,FMT='('' '')')
      WRITE (UNIT=out,FMT='('' '')')
      END IF
!-----
      RETURN
      END
!-----
!******************************
      SUBROUTINE MARNEW
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      CHARACTER(3) SECTIO,PROVID
      CHARACTER(16) AINFO1
      COMMON /SECINFO/ SECTIO,PROVID,AINFO1
!-----END OF COMMON VARIABLES
      INTEGER(4) I,I1
!-----
      CALL LABLA(REC,I1)
      MARSTR=REC(1:I1)
!---- above needed for tsatziki
      DO I=2,249
        IF (REC(I-1:I+1).EQ.' - ') REC(I-1:I+1)='   '
      END DO
      DO I=1,EMAX
      CALL TAXI(REC,MANAM(I))
        IF (MANAM(I).EQ.' ') GO TO 13
      END DO
   13 DIM=I-1
      NPAR=0
      IF (DIM.GT.1) GENUG(1)=.TRUE.
      RETURN
      END
!-----
!******************************
      SUBROUTINE MARDAT
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
      CHARACTER(3) SECTIO,PROVID
      CHARACTER(16) AINFO1,CH16
      COMMON /SECINFO/ SECTIO,PROVID,AINFO1
!-----END OF COMMON VARIABLES
      INTEGER(4) I,IIND(MPOMAX),IND(MPOMAX),POL,IS,I001,K,IM,I1,I2
      REAL(8) WWCP,WWH,WWS,WWV,WWK
      LOGICAL(4) CODE
!-----
      NPAR=NPAR+1
      CALL TAXI(REC,CH)
      POL=INDEX(CH,' ')-1
!
      CALL LABLA(CH,I1)
      CH16=CH(1:I1)
!---- above needed for tsatziki
      IF (POL.GT.MPOMAX) THEN
      CALL SHOUTF
      WRITE (UNIT=scr,FMT=114) MPOMAX,POL,(MANAM(I),I=1,DIM)
      WRITE (UNIT=out,FMT=114) MPOMAX,POL,(MANAM(I),I=1,DIM)
  114 FORMAT (//' The maximum polynomial for the Margules parameters', &
      ' is set to ',I5/ &
      ' A value of ',I5,' is needed for:',100(/10X,A16))
      WRITE (UNIT=scr,FMT=115)
      WRITE (UNIT=out,FMT=115)
  115 FORMAT ( &
      ' possible action:'/ &
      '   a) Increase the value of MPOMAX in the parameter statements')
      STOP
      END IF
      READ (UNIT=CH,FMT='(100I1)',ERR=999) (IIND(I),I=1,POL)
      DO 520,I=1,POL
      IF (IIND(I).LT.1.OR.IIND(I).GT.DIM) THEN
      CALL SHOUTD
      WRITE (UNIT=scr,FMT=116) (IIND(K),K=1,POL)
      WRITE (UNIT=out,FMT=116) (IIND(K),K=1,POL)
  116 FORMAT (//' The index of a Margules Parameter is out of bounds: ', &
      100I1)
      WRITE (UNIT=scr,FMT=117) (MANAM(K),K=1,DIM)
      WRITE (UNIT=out,FMT=117) (MANAM(K),K=1,DIM)
  117 FORMAT (' subsystem:',100(/10X,A16))
      STOP
      END IF
  520 CONTINUE
      CALL GELI(REC,WWH)
      CALL GELI(REC,WWS)
      CALL GELI(REC,WWV)
      CALL GELI(REC,WWCP)
      CALL GELI(REC,WWK)
!-----
      DO 532,IS=1,NSOL
      IF (MARCOD(IS)) THEN
      CODE=.TRUE.
      DO I=1,DIM
        IF (.NOT.CODE) GO TO 17
        DO K=1,NEND(IS)
          IF (MANAM(I).EQ.NAME(EM(IS,K))) GO TO 16
        END DO
   16 IND(I)=K
        CODE=(K.NE.NEND(IS)+1)
      END DO
   17 IF (CODE) THEN
      NMARG(IS)=NMARG(IS)+1
      IF (NMARG(IS).GT.MAMAX) THEN
      CALL SHOUTF
      WRITE (UNIT=scr,FMT=113) MAMAX,SOLNAM(IS)
      WRITE (UNIT=out,FMT=113) MAMAX,SOLNAM(IS)
  113 FORMAT (//' The maximum number of Margules parameters for each', &
      ' solution phase is set to ',I5/ &
      ' The solution ',A16,' needs more Margules parameters'/ &
      ' possible action:'/ &
      '   a) Increase the value of MAMAX in the parameter statements')
      STOP
      END IF
      RANGE(IS,NMARG(IS))=DIM
      POLY(IS,NMARG(IS))=POL
      DO I001=1,EMAX
        DSJDX(IS,NMARG(IS),I001)=0.0D0
        QQ(IS,NMARG(IS),I001)=0
      END DO
      DO I=1,DIM
        SJIND(IS,NMARG(IS),I)=IND(I)
        DSJDX(IS,NMARG(IS),IND(I))=1.0D0
      END DO
      DO I=1,POL
        INDX(IS,NMARG(IS),I)=IND(IIND(I))
        QQ(IS,NMARG(IS),IND(IIND(I)))=QQ(IS,NMARG(IS),IND(IIND(I)))+1
      END DO
      WH(IS,NMARG(IS))=WWH
      WS(IS,NMARG(IS))=WWS
      WV(IS,NMARG(IS))=WWV
      WCP(IS,NMARG(IS))=WWCP
      WK(IS,NMARG(IS))=WWK
!     WG(IS,NMARG(IS))=WWH+WWCP*(T-T0)-(WWS+DLOG(T/T0)*WWCP)*T+WWV*P
!
      CALL LABLA(MARSTR,I1)
      CALL LABLA(CH16,I2)
      MARDEF(IS,NMARG(IS))=MARSTR(1:I1)//' '//CH16(1:I2)
!      CALL PUST(6,MARDEF(IS,NMARG(IS)))
!---- above needed for tsatziki
      IF (PRTLOG(1)) WRITE (scr,1000) SOLNAM(IS),(IIND(I),I=1,POL)
      IF (PRTLOG(1)) WRITE (out,1000) SOLNAM(IS),(IIND(I),I=1,POL)
 1000 FORMAT (1X,A16,': MARGULES-PARAMETER ',100I1)
      IM=NMARG(IS)
      IF (PRTLOG(1)) CALL MARGINFO(IS,IM)
      END IF
      END IF
  532 CONTINUE
      RETURN
!-----
  999 CALL SHOUTF
      WRITE (UNIT=scr,FMT=302)
      WRITE (UNIT=out,FMT=302)
  302 FORMAT (//' Troubles reading Margules parameters')
      CALL LABLA(REC,K)
      WRITE (UNIT=scr,FMT=303) (REC(I:I),I=1,K)
      WRITE (UNIT=out,FMT=303) (REC(I:I),I=1,K)
  303 FORMAT (/' Remaining record: ',250A1)
      CALL LABLA(CH,K)
      WRITE (UNIT=scr,FMT=304) (CH(I:I),I=1,K)
      WRITE (UNIT=out,FMT=304) (CH(I:I),I=1,K)
  304 FORMAT (' The following string cannot be converted', &
      ' to a real number: ',25A1)
      STOP
      END
!-----
!******************************
      SUBROUTINE SMARNEW
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
      CHARACTER(3) SECTIO,PROVID
      CHARACTER(16) AINFO1
      COMMON /SECINFO/ SECTIO,PROVID,AINFO1
!-----END OF COMMON VARIABLES
      INTEGER(4) IS
      CHARACTER(16) CH01
!-----
      CALL TAXI(REC(3:),CH01)
      CSOL=0
      DO IS=1,NSOL
       IF (SOLNAM(IS).EQ.CH01.AND.MARCOD(IS)) CSOL=IS
      END DO
!      NPAR=0
      IF (CSOL.GT.0) THEN
      GENUG(1)=.TRUE.
      ELSE
      GENUG(1)=.FALSE.
      IF (PRTLOG(1)) THEN
      WRITE (UNIT=scr,FMT=104) CH01
      WRITE (UNIT=out,FMT=104) CH01
  104 FORMAT (' --------->',A16,' : phase not found')
      END IF
      END IF
      RETURN
      END
!-----
!******************************
      SUBROUTINE SMARDAT
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
      CHARACTER(3) SECTIO,PROVID
      CHARACTER(16) AINFO1
      COMMON /SECINFO/ SECTIO,PROVID,AINFO1
!-----END OF COMMON VARIABLES
      INTEGER(4) I,I001,K,I1,I2,IM
      REAL(8) WWH,WWS,WWV
!      LOGICAL(4) CODE
      CHARACTER(50) CH50,CH1,CH2,SITNAM,ELNAM
!-----
!      CALL PUST(6,REC)
!      WRITE (6,1000) SOLNAM(CSOL)
! 1000 FORMAT (' site Margules line for ',A)
!      NPAR=NPAR+1
      CALL TAXI(REC,CH50)
!
      CALL LABLA(CH50,I1)
      MARSTR=CH50(1:I1)
!---- above needed for tsatziki
      I=INDEX(CH50,' - ')
      IF (I.EQ.0) THEN
      CALL SHOUTD
      WRITE (scr,110) SOLNAM(CSOL),CH50
      WRITE (out,110) SOLNAM(CSOL),CH50
  110 FORMAT (//' site Margules parameters',/, &
      1X,A16,' missing " - ": ',A)
      STOP
      END IF
!-----
      CH1=CH50(1:I-1)
      CH2=CH50(I+3:)
      CALL FIBLA(CH1,I1)
      CALL LABLA(CH1,I2)
      SITNAM=CH1(I1:I2)
      K=0
      DO I=1,NSITE(CSOL)
       IF (CHSITE(CSOL,I).EQ.SITNAM) K=I
      END DO
      IF (K.EQ.0) THEN
!      WRITE (scr,300) SOLNAM(CSOL),SITNAM
!      WRITE (out,300) SOLNAM(CSOL),SITNAM
!  300 FORMAT (//' site Margules parameters',/, &
!      ' solution phase: ',A16,2X,'no site ',A)
       RETURN
      END IF
      NPAR=NSMARG(CSOL)+1
      SMSIT(CSOL,NPAR)=K
!-----
      I001=0
      SMPOLY(CSOL,NPAR)=0
   10 CALL FIBLA(CH2,I1)
      I2=INDEX(CH2,',')
      IF (I2.EQ.0) THEN
       CALL LABLA(CH2,I2)
       I2=I2+1
       I001=1
      END IF
      ELNAM=CH2(I1:I2-1)
!
      K=0
      DO I=1,NELPS(CSOL,SMSIT(CSOL,NPAR))
       IF (ELNAM.EQ.ELONS(CSOL,SMSIT(CSOL,NPAR),I)) K=I
      END DO
      IF (K.EQ.0) THEN
       RETURN
      END IF
      SMPOLY(CSOL,NPAR)=SMPOLY(CSOL,NPAR)+1
      SMEI(CSOL,NPAR,SMPOLY(CSOL,NPAR))=K
!+++++
      IF (SMSIT(CSOL,NPAR).GT.1)THEN
       DO I=1,SMSIT(CSOL,NPAR)-1
        K=K+NELPS(CSOL,I)
       END DO
      END IF
      SINDX(CSOL,NPAR,SMPOLY(CSOL,NPAR))=K
!+++++
      IF (I001.EQ.1) GOTO 11
      CH2(I1:I2)=' '
      GOTO 10
   11 CONTINUE
!
      IF (SMPOLY(CSOL,NPAR).GT.MPOMAX) THEN
      CALL SHOUTF
      WRITE (UNIT=scr,FMT=114) MPOMAX,CH50
      WRITE (UNIT=out,FMT=114) MPOMAX,CH50
  114 FORMAT (//' site Margules parameters',/, &
      ' The maximum polynomial is ',I5,'  input: ',A)
      STOP
      END IF
!=====
      CALL GELI(REC,WWH)
      CALL GELI(REC,WWS)
      CALL GELI(REC,WWV)
!=====
      NSMARG(CSOL)=NSMARG(CSOL)+1
      IF (NSMARG(CSOL).GT.MAMAX) THEN
      CALL SHOUTF
      WRITE (UNIT=scr,FMT=113) MAMAX,SOLNAM(CSOL)
      WRITE (UNIT=out,FMT=113) MAMAX,SOLNAM(CSOL)
  113 FORMAT (//' site Margules parameters',/, &
      ' The maximum number of Margules parameters for each', &
      ' solution phase is set to ',I5/ &
      ' The solution ',A16,' needs more Margules parameters')
      STOP
      END IF
      SWH(CSOL,NSMARG(CSOL))=WWH
      SWS(CSOL,NSMARG(CSOL))=WWS
      SWV(CSOL,NSMARG(CSOL))=WWV
!
      CALL LABLA(MARSTR,I1)
      SMARDEF(CSOL,NSMARG(CSOL))=MARSTR(1:I1)
!      CALL PUST(6,MARDEF(IS,NMARG(IS)))
!---- above needed for tsatziki
      IF (PRTLOG(1)) WRITE (scr,1000) SOLNAM(CSOL),CH50
      IF (PRTLOG(1)) WRITE (out,1000) SOLNAM(CSOL),CH50
 1000 FORMAT (1X,A16,': site Margules-parameter ',A)
      IM=NSMARG(CSOL)
      IF (PRTLOG(1)) CALL SMARGINFO(CSOL,IM)
!===== hier test of input!
      END
!-----
!******************************
      SUBROUTINE FIXNEW
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
      CHARACTER(3) SECTIO,PROVID
      CHARACTER(16) AINFO1
      COMMON /SECINFO/ SECTIO,PROVID,AINFO1
!-----END OF COMMON VARIABLES
      INTEGER(4) I,II,I1,I2,JX,IS,IE,FIXFORC
      REAL(8) FF,X001(EMAX),FSUM
      CHARACTER(500) MODEL1
      CHARACTER(60) REJECT
      CHARACTER(16) EMNAM
!-----
      CALL GELI(REC,FF)
      FIXFORC=IDINT(FF)
      CALL TAXI(REC,NAM)
      CALL TAXI(REC,SONAM)
      CALL TAXI(REC,FORMUL)
      GENUG(1)=.TRUE.
      NULL(NPHA+1)=.FALSE.
      IF (FIXFORC.EQ.0) NULL(NPHA+1)=.TRUE.
      DO II=1,NPHA
        IF (NAM.EQ.NAME(II)) THEN
          GENUG(1)=.FALSE.
          REJECT='Defined more than once in database'
          GOTO 99
        END IF
      END DO
      IS=0
      DO II=1,NSOL
        IF (SONAM.EQ.SOLNAM(II)) THEN
          IS=II
          GOTO 11
        END IF
      END DO
   11 IF (IS.EQ.0) THEN
      GENUG(1)=.FALSE.
      REJECT='Solution phase not found : '//SONAM
      GOTO 99
      END IF
!-----
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
      COM=.FALSE.
      DIS=.FALSE.
      VO1=.TRUE.
      VO2=.FALSE.
      VO3=.FALSE.
      AQU=.FALSE.
      FIX=.FALSE.
      TL1=.FALSE.
!-----
      NCOM=0
      DO I=1,NEND(IS)
        X001(I)=0.0D0
      END DO
      FSUM=0.0D0
      CALL FIBLA(FORMUL,I1)
 2001 IF (I1.EQ.0) GOTO 71
      I2=INDEX(FORMUL,'(')
      JX=INDEX(FORMUL,')')
      IF (I2.LE.I1) I2=I1+1
      IF (JX.LE.I2+1) JX=I2+2
      EMNAM=FORMUL(I1:I2-1)
      IE=0
      DO I=1,NEND(IS)
        IF (EMNAM.EQ.NAME(EM(IS,I))) THEN
          IE=I
          GOTO 72
        END IF
      END DO
   72 IF (IE.EQ.0) THEN
      CALL SHOUTD
      WRITE (UNIT=scr,FMT=300) NAM,EMNAM,SONAM,SONAM
      WRITE (UNIT=out,FMT=300) NAM,EMNAM,SONAM,SONAM
  300 FORMAT (//' Troubles reading the "fixed" phase: ',A16/ &
      ' The phase: ',A16,' is not an endmember of the solution ',A16/ &
      ' possible reasons:'/ &
      '   a) The phase is not in the database'/ &
      '   b) The phase is excluded by "USE"-variable or composition'/ &
      '   c) The phase is located in the database after ',A16)
      STOP
      END IF
      MODEL1=FORMUL(I2+1:JX-1)
!     READ (UNIT=MODEL1,FMT='(BN,D16.0)',ERR=999) FF
      CALL GELI(MODEL1,FF)
      X001(IE)=FF
      FSUM=FSUM+FF
      MODEL1=FORMUL
      FORMUL=MODEL1(JX+1:)
      CALL FIBLA(FORMUL,I1)
      GOTO 2001
!-----
   71 CONTINUE
      NPHA=NPHA+1
      IF (NPHA.GT.PHMAX) THEN
      CALL SHOUTF
      WRITE (UNIT=scr,FMT=100) PHMAX,NAM
      WRITE (UNIT=out,FMT=100) PHMAX,NAM
  100 FORMAT (// &
      ' The maximum number of phases is set to: ',I5/ &
      ' This limit was exeeded with the phase: ',A16/ &
      ' possible actions:'/ &
      '   a) Increase the value of PHMAX in the parameter statements'/ &
      '   b) Check the stabilities in smaller subsystems and then'/ &
      '      exclude (or delete) the unstable phases.')
      STOP
      END IF
      SUGNR=SUGNR+1
      NAME(NPHA)=NAM
      ABK(NPHA)=NAM(1:8)
      PHASID(NPHA)='MIN'
      DO I=1,NUN
        XX(NPHA,I)=0.0D0
        DO IE=1,NEND(IS)
          XX(NPHA,I)=XX(NPHA,I)+X001(IE)*XX(EM(IS,IE),I)
        END DO
      END DO
      EMSOL(NPHA)=0
      EMNR(NPHA)=0
      FIX=.TRUE.
      ISOFIX(NPHA)=IS
      SOSKIP(IS)=FIXFORC
      DO IE=1,NEND(IS)
       XEMFIX(NPHA,IE)=X001(IE)
      END DO
      IF (PRTLOG(1)) THEN
      WRITE (UNIT=scr,FMT='(1X,A16,'' O.K.'')') NAM
      WRITE (UNIT=out,FMT='(1X,A16,'' O.K.'')') NAM
      END IF
!-----
      CALL DASAVE(NPHA)
      RETURN
!-----
   99 IF (PRTLOG(1)) THEN
      WRITE (UNIT=scr,FMT=107) NAM,REJECT
      WRITE (UNIT=out,FMT=107) NAM,REJECT
  107 FORMAT (' --------->',A16,' : ',A60)
      END IF
      RETURN
!-----
      CALL SHOUTF
      WRITE (UNIT=scr,FMT=302) NAM
      WRITE (UNIT=out,FMT=302) NAM
  302 FORMAT (//' Troubles reading the "fixed" phase: ',A16)
      CALL LABLA(FORMUL,II)
      WRITE (UNIT=scr,FMT=303) (FORMUL(I:I),I=1,II)
      WRITE (UNIT=out,FMT=303) (FORMUL(I:I),I=1,II)
  303 FORMAT (/' Remaining record: ',170A1)
      CALL LABLA(MODEL1,II)
      WRITE (UNIT=scr,FMT=304) (MODEL1(I:I),I=1,II)
      WRITE (UNIT=out,FMT=304) (MODEL1(I:I),I=1,II)
  304 FORMAT (' The following string cannot be converted', &
      ' to a real number: ',250A1)
      STOP
      END
!-----
!******************************
      SUBROUTINE SEDNEW
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
      CHARACTER(3) SECTIO,PROVID
      CHARACTER(16) AINFO1
      COMMON /SECINFO/ SECTIO,PROVID,AINFO1
!-----END OF COMMON VARIABLES
      INTEGER(4) I1,IS,IE
      REAL(8) FF,SUM
      CHARACTER(16) CH16
!-----
      CALL TAXI(REC,CH16)
      I1=0
      DO IS=1,NSOL
        IF (SOLNAM(IS).EQ.CH16) THEN 
          I1=IS
          EXIT
        END IF
      END DO
!+++++
      IF (I1.NE.0) THEN
        NSED=NSED+1
        SIS(NSED)=I1
        DO IE=1,NEND(SIS(NSED))
          SXX(NSED,IE)=0.0D0
        END DO
        IF (PRTLOG(1)) THEN
         WRITE (UNIT=scr,FMT='('' seedling in database: NSED ='' &
       & ,I4,2X,'', solution '',I3,2X,A)') NSED,I1,SOLNAM(SIS(NSED))
         WRITE (UNIT=out,FMT='('' seedling in database: NSED ='' &
       & ,I4,2X,'', solution '',I3,2X,A)') NSED,I1,SOLNAM(SIS(NSED))
        END IF
!===
    2   CONTINUE
        CALL TAXI(REC,CH16)
        CALL GELI(REC,FF)
        IF (CH16.EQ.' ') GOTO 888
        DO IE=1,NEND(SIS(NSED))
          IF (NAME(EM(SIS(NSED),IE)).EQ.CH16) THEN
!!            WRITE (UNIT=scr,FMT=1005) IE,NAME(EM(SIS(NSED),IE)),FF
!! 1005       FORMAT ('       endmemb ',i3,2x,a,F15.2)
            SXX(NSED,IE)=FF
            EXIT
          END IF
        END DO
!
        GOTO 2
!===
  888   CONTINUE
        SUM=0.0D0
        DO IE=1,NEND(SIS(NSED))
          SUM=SUM+SXX(NSED,IE)
        END DO
        IF (DABS(SUM).LT.1D-20) THEN
          NSED=NSED-1
          RETURN
        ELSE
        DO IE=1,NEND(SIS(NSED))
          SXX(NSED,IE)=SXX(NSED,IE)/SUM
        END DO
        END IF
!+++++
      END IF
!====
      RETURN
      END
!-----
!******************************
!-----test for x=0 -> a=0
!-----test for x=1 -> a=1
!-----test if solution is complex
!-----test for consistency with Gibbs-Duhem equation
!-----test only if MODELL not equal 'I'
!-----muss nach GRECAL (wegen T,P,RT,GG,GGK) aufgerufen werden
!-----
      SUBROUTINE GIBBSTEST(E1,E2)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!-----END OF COMMON VARIABLES
      INTEGER(4) I,IGD,IS,CODE,ITEST,I001,II,E1,E2,IE, &
      IC,NROW,NCOL,RANG,IKOMM
      REAL(8) FF,FFX,XX0(EMAX),XX1(EMAX),XX2(EMAX),F1, &
      GGD0,GGD1,GGD2,XGD,AOFG(EMAX),AOFM(EMAX),GDTEST, &
      XXX(EMAX),AAA(EMAX),MAT(500,500),MUE(EMAX)
!-----
      XGD=0.0001D0
      GDTEST=1D-1
      DO 500,IS=E1,E2
      NNEGEM(IS)=0
      NSTREM(IS)=0
      IKOMM=0
      IF (PRTLOG(2)) IKOMM=1
      IF (NEND(IS).LE.1) GOTO 500
!-----test solution model (only ideal part of site-mixing)
!-----
!-----test x=0 -> a=0
      IF (MODELL(IS).NE.'I') THEN
!!      WRITE (UNIT=6,FMT='('' test 1'')')
      FF=1.0D0/DBLE(NEND(IS)-1.0D0)
      DO I=1,NEND(IS)
        XXX(I)=FF
      END DO
      I001=0
      DO I=1,NEND(IS)
        XXX(I)=0.0D0
        CALL ACTIVI(IS,XXX,AAA)
!dC        IF (AAA(I).NE.0.0D0) THEN
        IF (AAA(I).GT.2.0D-10) THEN
          I001=I001+1
          NNEGEM(IS)=I001
          NEGEM(IS,I001)=I
        END IF
        XXX(I)=FF
      END DO
!=====
!     flag that NNEGEM has been set at least for one solution (IS)
!      WRITE (UNIT=scr,FMT='('' Gibbstest: NNEGEM assigned :)'')')
      GIBBSFLAG=.TRUE.
!=====
      IF (I001.GT.0) THEN
      IKOMM=IKOMM+1
!
      IF (.NOT.DRU) RETURN
!
      IF (PRTLOG(4)) THEN
        IF (IKOMM.EQ.1) THEN
          WRITE (UNIT=scr,FMT=1000)
          WRITE (UNIT=out,FMT=1000)
 1000     FORMAT (/,' ----------------')
        END IF
        WRITE (UNIT=scr,FMT=1001) SOLNAM(IS)
        WRITE (UNIT=out,FMT=1001) SOLNAM(IS)
 1001   FORMAT (/ &
        1X,A16,' : ideal part of solution model has ', &
        'non-zero activity for zero concentration')
      END IF
!--
      IF (PRTLOG(2).OR.PRTLOG(1)) THEN
        WRITE (UNIT=scr,FMT='('' '')')
        WRITE (UNIT=out,FMT='('' '')')
        WRITE (UNIT=scr,FMT=1010) (ABK(EM(IS,I)),I=1,NEND(IS))
        WRITE (UNIT=out,FMT=1010) (ABK(EM(IS,I)),I=1,NEND(IS))
 1010   FORMAT (29X,15(2X,A8))
        DO I=1,NEND(IS)
          XXX(I)=0.0D0
          CALL ACTIVI(IS,XXX,AAA)
          WRITE (UNIT=scr,FMT=1020) NAME(EM(IS,I)),(AAA(II),II=1,NEND(IS))
          WRITE (UNIT=out,FMT=1020) NAME(EM(IS,I)),(AAA(II),II=1,NEND(IS))
 1020     FORMAT (1X,A16,' = 0 -> A:',15F10.6)
          XXX(I)=FF
        END DO
      END IF
!--
      END IF
!-----
      ELSE
!!      WRITE (UNIT=6,FMT='('' no test 1'')')
      END IF
      IF (PRTLOG(4)) THEN
      IF (NNEGEM(IS).GT.0) THEN
      WRITE (scr,1022) (NAME(EM(IS,NEGEM(IS,IE))),IE=1,NNEGEM(IS))
      WRITE (out,1022) (NAME(EM(IS,NEGEM(IS,IE))),IE=1,NNEGEM(IS))
 1022 FORMAT (/' the following endmembers may have negative', &
      ' concentrations: ',20(1X,A16))
      END IF
      END IF
!-----end of solution test 1
!-----
!-----test x=1 -> a=1
      IF (MODELL(IS).NE.'I') THEN
!!      WRITE (UNIT=6,FMT='('' test 2'')')
      DO I=1,NEND(IS)
        XXX(I)=0.0D0
      END DO
      I001=0
      DO I=1,NEND(IS)
        XXX(I)=1.0D0
        CALL ACTIVI(IS,XXX,AAA)
        IF (DABS(AAA(I)-1.0D0).GT.1.0D-10) I001=1
        XXX(I)=0.0D0
      END DO
      IF (I001.EQ.1) THEN
      IKOMM=IKOMM+1
      IF (PRTLOG(4)) THEN
      IF (IKOMM.EQ.1) THEN
      WRITE (UNIT=scr,FMT=1030)
      WRITE (UNIT=out,FMT=1030)
 1030 FORMAT (/,' ----------------')
      END IF
      WRITE (UNIT=scr,FMT=1031) SOLNAM(IS)
      WRITE (UNIT=out,FMT=1031) SOLNAM(IS)
 1031 FORMAT (/ &
      1X,A16,' : ', &
      'activity is not one for pure endmember'/' ')
      WRITE (UNIT=scr,FMT=1040) (ABK(EM(IS,I)),I=1,NEND(IS))
      WRITE (UNIT=out,FMT=1040) (ABK(EM(IS,I)),I=1,NEND(IS))
 1040 FORMAT (29X,10(2X,A8))
      DO I=1,NEND(IS)
        XXX(I)=1.0D0
        CALL ACTIVI(IS,XXX,AAA)
        WRITE (UNIT=scr,FMT=1050) NAME(EM(IS,I)),(AAA(II),II=1,NEND(IS))
        WRITE (UNIT=out,FMT=1050) NAME(EM(IS,I)),(AAA(II),II=1,NEND(IS))
 1050 FORMAT (1X,A16,' = 1 -> A:',10F10.6)
        XXX(I)=0.0D0
      END DO
!     STOP
      END IF
      END IF
!-----
      ELSE
!!      WRITE (UNIT=6,FMT='('' no test 2'')')
      END IF
!-----end of solution test 2
!-----
!-----test 3: if solution is complex
      IF (MODELL(IS).NE.'I') THEN
!!      WRITE (UNIT=6,FMT='('' test 3'')')
      NROW=NUN
      NCOL=NEND(IS)
      DO IC=1,NUN
      DO IE=1,NEND(IS)
        MAT(IC,IE)=XX(EM(IS,IE),IC)
      END DO
      END DO
      RANG=0
      CALL KOMPLEX(NROW,NCOL,MAT,RANG)
      IF (RANG.LT.NEND(IS)) THEN
      IKOMM=IKOMM+1
      IF (PRTLOG(4)) THEN
      IF (IKOMM.EQ.1) THEN
      WRITE (UNIT=scr,FMT=1060)
      WRITE (UNIT=out,FMT=1060)
 1060 FORMAT (/,' ----------------')
      END IF
      WRITE (UNIT=scr,FMT=1061) SOLNAM(IS)
      WRITE (UNIT=out,FMT=1061) SOLNAM(IS)
 1061 FORMAT (/ &
      1X,A16,' : ', &
      'solution is complex, do not use "ideal" site mixing.')
      END IF
      END IF
      ELSE
!!      WRITE (UNIT=6,FMT='('' no test 3'')')
      END IF
!-----end of complex-test
!-----
!-----test 4: Gibbs-Duhem relation
      DO 800,ITEST=1,NEND(IS)
!!      WRITE (UNIT=6,FMT='('' test 4 '',A16)') NAME(EM(IS,ITEST))
      CODE=0
      F1=0.0D0
!     FF=1.0D0/DBLE(NEND(IS)+1)
      FF=0.5D0/DBLE(NEND(IS))
      DO I=1,NEND(IS)
        F1=F1+FF
        XX0(I)=FF
      END DO
      XX0(ITEST)=1.0D0-F1+FF
      CALL GNONID(IS,XX0,GGD0)
      CALL MUECAL(IS,XX0,MUE)
      DO 710,IGD=1,NEND(IS)
      DO I=1,NEND(IS)
        XX1(I)=XX0(I)
        XX2(I)=XX0(I)
      END DO
      XX1(IGD)=XX0(IGD)-XGD
      XX2(IGD)=XX0(IGD)+XGD
      DO I=1,NEND(IS)
        XX1(I)=XX1(I)/(1.0D0-XGD)
        XX2(I)=XX2(I)/(1.0D0+XGD)
      END DO
      FFX=XX2(IGD)-XX1(IGD)
      CALL GNONID(IS,XX1,GGD1)
      CALL GNONID(IS,XX2,GGD2)
      AOFG(IGD)=GGD0+(1.0D0-XX0(IGD))*(GGD2-GGD1)/FFX
      AOFG(IGD)=AOFG(IGD)-GG(EM(IS,IGD))
      AOFM(IGD)=MUE(IGD)-GG(EM(IS,IGD))
      IF (DABS(AOFG(IGD)-AOFM(IGD)).GT.GDTEST) CODE=1
  710 CONTINUE
!-----if inconsistent or PRTLOG(1):
      IF (CODE.EQ.1.OR.PRTLOG(1)) THEN
      WRITE (UNIT=scr,FMT='(''test TC,P'',2f20.10)') TC,P
      WRITE (UNIT=out,FMT='(''test TC,P'',2f20.10)') TC,P
      IKOMM=IKOMM+1
      IF (CODE.EQ.1) THEN
      IF (IKOMM.EQ.1) THEN
      WRITE (UNIT=scr,FMT=1070)
      WRITE (UNIT=out,FMT=1070)
 1070 FORMAT (/,' ----------------')
      END IF
      WRITE (UNIT=scr,FMT=1071) SOLNAM(IS)
      WRITE (UNIT=out,FMT=1071) SOLNAM(IS)
 1071 FORMAT (/ &
      1X,A16,' : Gibbs-Duhem test failed: ')
      ELSE
      IF (IKOMM.EQ.1) THEN
      WRITE (UNIT=scr,FMT=1070)
      WRITE (UNIT=out,FMT=1070)
      END IF
      WRITE (UNIT=scr,FMT=2000) SOLNAM(IS)
      WRITE (UNIT=out,FMT=2000) SOLNAM(IS)
 2000 FORMAT (/1X,A16,' : Gibbs-Duhem test: ')
      END IF
!---
      WRITE (UNIT=scr,FMT=2005) (NAME(EM(IS,I)),I=1,NEND(IS))
      WRITE (UNIT=out,FMT=2005) (NAME(EM(IS,I)),I=1,NEND(IS))
 2005 FORMAT (17X,10A16)
      WRITE (UNIT=scr,FMT=2010) (XX0(I),I=1,NEND(IS))
      WRITE (UNIT=out,FMT=2010) (XX0(I),I=1,NEND(IS))
 2010 FORMAT (' X         ',10F16.8)
      WRITE (UNIT=scr,FMT=2015) (AOFM(I),I=1,NEND(IS))
      WRITE (UNIT=out,FMT=2015) (AOFM(I),I=1,NEND(IS))
! 2015 FORMAT (' MUE(calc) ',10F16.4)
 2015 FORMAT (' MUE(calc) ',10(2X,1PE14.7))
      WRITE (UNIT=scr,FMT=2020) (AOFG(I),I=1,NEND(IS))
      WRITE (UNIT=out,FMT=2020) (AOFG(I),I=1,NEND(IS))
! 2020 FORMAT (' MUE(deriv)',10F16.4)
 2020 FORMAT (' MUE(deriv)',10(2X,1PE14.7))
      DO I=1,NEND(IS)
        AOFG(I)=AOFG(I)/RT
        IF (AOFG(I).LT.-150) AOFG(I)=-150
        IF (AOFG(I).GT.150) AOFG(I)=150
        AOFG(I)=DEXP(AOFG(I))
        AOFM(I)=AOFM(I)/RT
        IF (AOFM(I).LT.-150) AOFM(I)=-150
        IF (AOFM(I).GT.150) AOFM(I)=150
        AOFM(I)=DEXP(AOFM(I))
      END DO
      WRITE (UNIT=scr,FMT=2025) (AOFM(I),I=1,NEND(IS))
      WRITE (UNIT=out,FMT=2025) (AOFM(I),I=1,NEND(IS))
! 2025 FORMAT (' A(calc)   ',10F16.8)
 2025 FORMAT (' A(calc)   ',10(2X,1PE14.7))
      WRITE (UNIT=scr,FMT=2030) (AOFG(I),I=1,NEND(IS))
      WRITE (UNIT=out,FMT=2030) (AOFG(I),I=1,NEND(IS))
! 2030 FORMAT (' A(deriv)  ',10F16.8)
 2030 FORMAT (' A(deriv)  ',10(2X,1PE14.7))
      END IF
  800 CONTINUE
!-----end of Gibbs-Duhem-test
!-----analyze Solution model
      I001=IS
!
!     INITIALIZE  MINEM AND MAXEM 0,1 FOR ALL SOLUTIONS
      DO I=1,NEND(IS)
        MINEM(IS,I)=0.0D0
        MAXEM(IS,I)=1.0D0
      END DO
!
!cdc      IF (NNEGEM(IS).GT.0) CALL ANALSOL(I001)
      IF (NSITE(IS).GT.0) CALL ANALSOL(I001)
      IF (NNEGEM(IS).GT.0.AND.NONEG(IS)) THEN
      IF (PRTLOG(4)) THEN
      WRITE (UNIT=scr,FMT=2040) SOLNAM(IS)
      WRITE (UNIT=out,FMT=2040) SOLNAM(IS)
 2040 FORMAT (/ &
      1X,A16,' : ', &
      'all concentrations will be forced to be positive'/' ')
      END IF
!     HIER ALLE MINEM UND MAXEM AUF 0 1 SETZEN
      DO I=1,NEND(IS)
        MINEM(IS,I)=0.0D0
        MAXEM(IS,I)=1.0D0
      END DO
      NNEGEM(IS)=0
      END IF
!-----END analyze Solution model
!     HIER DRUCKEN VON MINEM UND MAXEM
      IF (PRTLOG(4)) THEN
      WRITE (UNIT=6,FMT=2048)
 2048 FORMAT (/,21X,'min.',3X,'max.')
      DO I=1,NEND(IS)
        WRITE (UNIT=6,FMT=2050) NAME(EM(IS,I)),MINEM(IS,I),MAXEM(IS,I)
 2050 FORMAT (1X,A16,2X,F6.3,2X,F5.3)
      END DO
      END IF
!++++++
  500 CONTINUE
      RETURN
      END
!-----
!******************************
      SUBROUTINE ANALSOL(IS)
!-----analyze Solution model
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!-----END OF COMMON VARIABLES
      INTEGER(4) I,IS,II,IE,NSTSIT,IR,IP,I001,I0,I1, &
      IC,NROW,NCOL,RANG,COLNR(500),ROWNR(500),PERMU(SITMAX), &
      I001A
      REAL(8) MAT(500,500),MINSO(SIELMAX), &
      MAXSO(SIELMAX),XXPERM(500,SIELMAX),SUM
      CHARACTER(32) NAMPERM(500),CH16,COLNAME(500)
!=========================================================
!      WRITE (UNIT=6,FMT='('' analyze solution'')')
!      WRITE (6,1000) NSITE(IS),NSIEL(IS)
! 1000 FORMAT (' number of sites:',I3,2X,' NSIEL:',I3)
!      DO 100,I=1,NSITE(IS)
!      WRITE (6,1005) I,NELPS(IS,I),SITMUL(IS,I)
! 1005 FORMAT (' site:',I3,2X,' number of el:',I3,2X,' multipl.:',F5.2)
!      WRITE (6,1010) (ELONS(IS,I,IE),IE=1,NELPS(IS,I))
! 1010 FORMAT ('    ELONS:',20(2X,A8))
!  100 CONTINUE
!=========================================================
!-----make structural endmembers
!-----find maximum and minimum occupancies for each site
!       WRITE (UNIT=scr,FMT='(''ANALSOL '',I3)') IS
      NSTSIT=2
      DO I=1,NSIEL(IS)
        MINSO(I)=1D10
        MAXSO(I)=-1D10
        DO IE=1,NEND(IS)
        IF (EMXX(IS,I,IE).LT.MINSO(I)) MINSO(I)=EMXX(IS,I,IE)
        IF (EMXX(IS,I,IE).GT.MAXSO(I)) MAXSO(I)=EMXX(IS,I,IE)
        END DO
      END DO
!=========================================================
!      WRITE (UNIT=6,FMT='(''NSIEL'',I4)') NSIEL(IS)
!      DO 210,I=1,NSIEL(IS)
!      WRITE (6,1050) SIEL(IS,I),MINSO(I),MAXSO(I)
! 1050 FORMAT (1X,A16,2X,F7.2,2X,F7.2)
!  210 CONTINUE
!=========================================================
!-
!-----write site-elements and their concentrations for each endmember
      IF (PRTLOG(1)) THEN
      WRITE (UNIT=scr,FMT='(/,'' site occupancies:'')')
      WRITE (UNIT=out,FMT='(/,'' site occupancies:'')')
      WRITE (scr,2012) (NAME(EM(IS,IE)),IE=1,NEND(IS))
      WRITE (out,2012) (NAME(EM(IS,IE)),IE=1,NEND(IS))
 2012 FORMAT (27X,20(1X,A16))
      DO I=1,NSIEL(IS)
!=========================================================
!        WRITE (6,2015) I,SIEL(IS,I)
! 2015 FORMAT (' SIEL: (',I3,')  ',A16)
!        DO 255,II=1,NEMQQ(IS,I)
!          WRITE (6,2020) EMQQ(IS,I,II)
! 2020 FORMAT ('    EMQQ:',I5)
!        END DO
!=========================================================
!
        WRITE (scr,2025) I,SIEL(IS,I),(EMXX(IS,I,IE),IE=1,NEND(IS))
        WRITE (out,2025) I,SIEL(IS,I),(EMXX(IS,I,IE),IE=1,NEND(IS))
 2025 FORMAT (1X,I3,2X,A16,20(1X,F16.5))
      END DO
      END IF
!=========================================================
!-----check rank of (site occupancies) x (endmembers)
!      NROW=NEND(IS)
!      NCOL=NSIEL(IS)
!      DO 300,IR=1,NROW
!      ROWNR(IR)=IR
!      DO 300,IC=1,NCOL
!      COLNR(IC)=IC
!  300 MAT(IR,IC)=EMXX(IS,IC,IR)
!      RANG=0
!C-
!      WRITE (6,2030) (SIEL(IS,COLNR(I)),I=1,NCOL)
! 2030 FORMAT (23X,20(2X,A16))
!      DO 305,IR=1,NROW
!  305 WRITE (6,2035) NAME(EM(IS,ROWNR(IR))),(MAT(IR,I),I=1,NCOL)
! 2035 FORMAT (1X,A16,20(F18.5))
!C-
!      I001=NCOL
!      CALL DEPSITES(NROW,NCOL,MAT,RANG,COLNR,ROWNR,I001)
!      WRITE (6,2037) RANG
! 2037 FORMAT (' RANG = ',I5)
!C-
!C      WRITE (6,2040) (NAME(EM(IS,COLNR(IE))),IE=1,NEND(IS))
!C 2040 FORMAT (23X,20(2X,A16))
!C      DO 310,IC=1,NROW
!C  310 WRITE (6,2050) SIEL(IS,ROWNR(IC)),(MAT(IC,IE),IE=1,NCOL)
!C 2050 FORMAT (1X,A16,20(F18.5))
!C-
!      WRITE (6,2040) (SIEL(IS,COLNR(I)),I=1,NCOL)
! 2040 FORMAT (23X,20(2X,A16))
!      DO 310,IR=1,NROW
!  310 WRITE (6,2045) NAME(EM(IS,ROWNR(IR))),(MAT(IR,I),I=1,NCOL)
! 2045 FORMAT (1X,A16,20(F18.5))
!=========================================================
!-----make combinations
      IP=0
      DO I=1,NSITE(IS)
        PERMU(I)=1
      END DO
      PERMU(NSITE(IS))=0
   10 IP=IP+1
      II=NSITE(IS)
   11 IF (II.LT.1) GOTO 88
      PERMU(II)=PERMU(II)+1
      IF (PERMU(II).GT.NELPS(IS,II)) THEN
      PERMU(II)=1
      II=II-1
      GOTO 11
      END IF
!=========================================================
!      WRITE (6,3030) IP,(PERMU(I),I=1,NSITE(IS))
! 3030 FORMAT (' permu: ',I3,4X,20(1X,I3))
!=========================================================
      DO I=1,NSIEL(IS)
        XXPERM(IP,I)=MINSO(I)
      END DO
      II=0
      NAMPERM(IP)=' '
      DO I=1,NSITE(IS)
        XXPERM(IP,II+PERMU(I))=MAXSO(II+PERMU(I))
        IF (I.EQ.1) THEN
          NAMPERM(IP)=ELONS(IS,I,PERMU(I))
        ELSE
          CALL LABLA(NAMPERM(IP),I001)
          CH16=NAMPERM(IP)(1:I001)//'-'//ELONS(IS,I,PERMU(I))
          NAMPERM(IP)=CH16
        END IF
        II=II+NELPS(IS,I)
      END DO
      GOTO 10
!-
   88 CONTINUE
      IP=IP-1
!=========================================================
!      WRITE (6,3040) (NAMPERM(I),I=1,IP)
! 3040 FORMAT (23X,20(2X,A16))
!      DO 710,IR=1,NSIEL(IS)
!  710 WRITE (6,3045) SIEL(IS,IR),(XXPERM(I,IR),I=1,IP)
! 3045 FORMAT (1X,A16,20(F18.5))
!=========================================================
!-----check that each combination can be expressed by endmembers
      NROW=NSIEL(IS)
      NCOL=NEND(IS)+IP
      DO IR=1,NROW
        ROWNR(IR)=IR
        DO IC=1,NEND(IS)
          COLNR(IC)=IC
          MAT(IR,IC)=EMXX(IS,IR,IC)
        END DO
        DO IC=1,IP
          COLNR(NEND(IS)+IC)=NEND(IS)+IC
          MAT(IR,NEND(IS)+IC)=XXPERM(IC,IR)
        END DO
      END DO
      DO IC=1,NEND(IS)
        COLNAME(IC)=NAME(EM(IS,IC))
      END DO
      DO IC=1,IP
        COLNAME(NEND(IS)+IC)=NAMPERM(IC)
      END DO
      RANG=0
!=========================================================
!- write for test
!      WRITE (6,3050) (COLNAME(I),I=1,NCOL)
! 3050 FORMAT (20(2X,A10))
!      DO IR=1,NROW
!        WRITE (6,3055) (MAT(IR,I),I=1,NCOL)
!      END DO
! 3055 FORMAT (20(F12.5))
!=========================================================
      I001=NEND(IS)
      CALL DEPSITES(NROW,NCOL,MAT,RANG,COLNR,ROWNR,I001)
!=========================================================
!- write for test
!      WRITE (6,3057) RANG
! 3057 FORMAT (' RANG = ',I5)
!      WRITE (6,3058) (COLNAME(COLNR(I)),I=1,NCOL)
! 3058 FORMAT (20(2X,A10))
!      DO IR=1,NROW
!        WRITE (6,3059) (MAT(IR,I),I=1,NCOL)
!      END DO
! 3059 FORMAT (20(F12.5))
!=========================================================
!-
      IF (PRTLOG(1)) THEN
      WRITE (UNIT=scr,FMT='(/,'' structural space:'')')
      WRITE (UNIT=out,FMT='(/,'' structural space:'')')
      WRITE (scr,3070) (COLNAME(COLNR(I)),I=1,NEND(IS))
      WRITE (out,3070) (COLNAME(COLNR(I)),I=1,NEND(IS))
 3070 FORMAT (27X,20(1X,A16))
      END IF
!
      DO 800,IC=NEND(IS)+1,NCOL
!=========================================================
!      WRITE (UNIT=6,FMT='(''IC = '',I4 )') IC
!=========================================================
      I001=0
      I001A=0
      DO IR=NEND(IS)+1,NROW
        IF (DABS(MAT(IR,IC)).GT.0.0D0) I001=I001+1
!=========================================================
!        WRITE (UNIT=6,FMT='(''IR,MAT(IR,IC) '',I4,2X,F20.10)') IR,MAT(IR,IC)
!=========================================================
      END DO
!-----
      IF (I001.EQ.0) THEN
!
      IF (PRTLOG(1)) THEN
      WRITE (scr,3072) IC-NEND(IS),COLNAME(COLNR(IC)), &
      (MAT(IR,IC),IR=1,NEND(IS))
      WRITE (out,3072) IC-NEND(IS),COLNAME(COLNR(IC)), &
      (MAT(IR,IC),IR=1,NEND(IS))
 3072 FORMAT (1X,I3,2X,A16,20(1X,F16.5))
      END IF
!-----
!---
!-----make structural endmembers for initial guess in minimization
!-----and for directional search
!-----note that to avoid site occupancies of zero (-> some activities = zero)
!-----the compositions are shifted. (is now commentet out)
      I0=0
      I1=0
      DO IR=1,NEND(IS)
!=========================================================
!        WRITE (UNIT=6,FMT='(''IR,IC,MAT(IR,IC)= '',2I4,2X,F20.10)') IR,IC,MAT(IR,IC)
!=========================================================
        IF (DABS(MAT(IR,IC)).LT.1D-10) I0=I0+1
        IF (DABS(MAT(IR,IC)-1.0D0).LT.1D-10) I1=I1+1
      END DO
!-----
!=========================================================
!      WRITE (UNIT=6,FMT='(''NONEG(IS) '',L4)') NONEG(IS)
!=========================================================
!--- don't bother if NONEG(IS)
      IF (NONEG(IS)) GOTO 815
!-----
!=========================================================
!      WRITE (UNIT=6,FMT='(''IC,I1,IO '',3I4)') IC,I1,I0
!=========================================================
      IF (I1.NE.1.OR.I0.NE.NEND(IS)-1) THEN
      NSTREM(IS)=NSTREM(IS)+1
!=========================================================
!      WRITE (UNIT=6,FMT='(''IS,NSTREM(IS) '',I4,2X,I4)') IS,NSTREM(IS)
!=========================================================
      SUM=0.0D0
      DO IR=1,NEND(IS)
!DC        STREM(IS,NSTREM(IS),IR)=MAT(COLNR(IR),IC)
        STREM(IS,NSTREM(IS),COLNR(IR))=MAT(IR,IC)
!        IF (STREM(IS,NSTREM(IS),IR).GT.1D-1) THEN
!        STREM(IS,NSTREM(IS),IR)=STREM(IS,NSTREM(IS),IR)-1D-4
!        ELSE
!        STREM(IS,NSTREM(IS),IR)=STREM(IS,NSTREM(IS),IR)+1D-3
!        END IF
!DC        SUM=SUM+STREM(IS,NSTREM(IS),IR)
        SUM=SUM+STREM(IS,NSTREM(IS),COLNR(IR))
      END DO
      DO IR=1,NEND(IS)
        STREM(IS,NSTREM(IS),IR)=STREM(IS,NSTREM(IS),IR)/SUM
      END DO
      END IF
!---
  815 CONTINUE
!-----else from: IF (I001.EQ.0)
      ELSE
      IF (PRTLOG(1)) THEN
      WRITE (scr,3074) IC-NEND(IS),COLNAME(COLNR(IC))
      WRITE (out,3074) IC-NEND(IS),COLNAME(COLNR(IC))
 3074 FORMAT (1X,I3,2X,A16,' cannot be reached')
      END IF
!----end from: IF (I001.EQ.0)
      END IF
  800 CONTINUE
!-


      DO I=1,NSTREM(IS)
      DO IR=1,NEND(IS)
      IF (STREM(IS,I,IR).LT.MINEM(IS,IR)) MINEM(IS,IR)=STREM(IS,I,IR)
      IF (STREM(IS,I,IR).GT.MAXEM(IS,IR)) MAXEM(IS,IR)=STREM(IS,I,IR)
      END DO
      END DO

      IF (PRTLOG(4).OR.PRTLOG(1)) THEN
      IF (NSTREM(IS).GT.0) THEN
      WRITE (UNIT=scr,FMT='(/,'' structural endmembers:'')')
      WRITE (UNIT=out,FMT='(/,'' structural endmembers:'')')
      WRITE (scr,3085) (NAME(EM(IS,I)),I=1,NEND(IS))
      WRITE (out,3085) (NAME(EM(IS,I)),I=1,NEND(IS))
 3085 FORMAT (27X,20(1X,A16))
      DO 900,I=1,NSTREM(IS)
      WRITE (scr,3087) I,SOLNAM(IS),(STREM(IS,I,IR),IR=1,NEND(IS))
      WRITE (out,3087) I,SOLNAM(IS),(STREM(IS,I,IR),IR=1,NEND(IS))
 3087 FORMAT (1X,I3,2X,A16,20(1X,1PE16.8))
  900 CONTINUE
      END IF
      END IF
!
!---- make ALLEM(IS,I,II)   I=1 to NEND(IS)+NSTREM(IS) II=1 to NEND(IS)
      NALLEM(IS)=0
      DO I=1,NEND(IS)
       NALLEM(IS)=NALLEM(IS)+1
       DO II=1,NEND(IS)
        ALLEM(IS,NALLEM(IS),II)=0.0D0
       END DO
       ALLEM(IS,NALLEM(IS),I)=1.0D0
      END DO
      DO I=1,NSTREM(IS)
       NALLEM(IS)=NALLEM(IS)+1
       DO II=1,NEND(IS)
        ALLEM(IS,NALLEM(IS),II)=STREM(IS,I,II)
       END DO
      END DO
!      WRITE (UNIT=scr,FMT='('' Gibbstest: NALLEM assigned :)'')')
!-
!      IF (PRTLOG(4).OR.PRTLOG(1)) THEN
!      WRITE (UNIT=scr,FMT='(/,'' all endmembers:'')')
!      WRITE (UNIT=out,FMT='(/,'' all endmembers:'')')
!      WRITE (scr,3090) (NAME(EM(IS,I)),I=1,NEND(IS))
!      WRITE (out,3090) (NAME(EM(IS,I)),I=1,NEND(IS))
! 3090 FORMAT (27X,20(1X,A16))
!      DO I=1,NALLEM(IS)
!      WRITE (scr,3092) I,SOLNAM(IS),(ALLEM(IS,I,IR),IR=1,NEND(IS))
!      WRITE (out,3092) I,SOLNAM(IS),(ALLEM(IS,I,IR),IR=1,NEND(IS))
! 3092 FORMAT (1X,I3,2X,A16,20(1X,F16.5))
!      END DO
!      END IF
!-
      RETURN
      END
!-----
!******************************
      SUBROUTINE KOMPLEX(NROW,NCOL,MAT,RANG)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      REAL(8) MAT(500,500),COLSUM(500),WERT,FF,F1, &
      FFC(500),FFR(500)
      INTEGER(4) NROW,NCOL,RANG,PIV,IR,IC,JX,JY,I,II
!----
      RANG=0
      DO 500,PIV=1,NROW
      DO IC=PIV,NCOL
        COLSUM(IC)=0.0D0
        DO IR=1,NROW
          COLSUM(IC)=COLSUM(IC)+DABS(MAT(IR,IC))
        END DO
      END DO
      JY=0
      JX=0
      WERT=0.0D0
      DO IR=PIV,NROW
        DO IC=PIV,NCOL
          IF (COLSUM(IC).LE.0.0D0) GOTO 419
          FF=DABS(MAT(IR,IC)/COLSUM(IC))
          IF (FF.GT.WERT) THEN
            WERT=FF
            JX=IC
            JY=IR
          END IF
  419     CONTINUE
        END DO
        IF (DABS(WERT).GT.0.0D0) GOTO 411
      END DO
  411 CONTINUE
      IF (JX.EQ.0.OR.JY.EQ.0) RETURN
!-----colchg
      IF (JX.NE.PIV) THEN
      DO I=1,NROW
        FFC(I)=MAT(I,JX)
      END DO
      DO I=1,NROW
        MAT(I,JX)=MAT(I,PIV)
      END DO
      DO I=1,NROW
        MAT(I,PIV)=FFC(I)
      END DO
      END IF
!-----rowchg
      IF (JY.NE.PIV) THEN
      DO I=1,NCOL
        FFR(I)=MAT(JY,I)
      END DO
      DO I=1,NCOL
        MAT(JY,I)=MAT(PIV,I)
      END DO
      DO I=1,NCOL
        MAT(PIV,I)=FFR(I)
      END DO
      END IF
!-----
      FF=MAT(PIV,PIV)
      RANG=PIV
!-----divrow
      DO I=1,NCOL
        MAT(PIV,I)=MAT(PIV,I)/FF
      END DO
!-----
      DO II=1,NROW
        IF (II.NE.PIV) THEN
        F1=MAT(II,PIV)
!-----substr
        DO I=1,NCOL
          MAT(II,I)=MAT(II,I)-MAT(PIV,I)*F1
          IF (DABS(MAT(II,I)).LE.1.0D-10) MAT(II,I)=0.0D0
        END DO
!-----
        END IF
      END DO
  500 CONTINUE
      RETURN
      END
!-----
!******************************
      SUBROUTINE DEPSITES(NROW,NCOL,MAT,RANG,COLNR,ROWNR,CPIMAX)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      REAL(8) MAT(500,500),COLSUM(500),WERT,FF,F1, &
      FFC(500),FFR(500)
      INTEGER(4) NROW,NCOL,RANG,PIV,IR,IC,JX,JY,I,II, &
      COLNR(500),ROWNR(500),I001,CPIMAX
!----
      RANG=0
      DO 500,PIV=1,NROW
!DC      DO 400,IC=PIV,NCOL
      DO IC=PIV,CPIMAX
        COLSUM(IC)=0.0D0
        DO IR=1,NROW
          COLSUM(IC)=COLSUM(IC)+DABS(MAT(IR,IC))
        END DO
      END DO
      JY=0
      JX=0
      WERT=0.0D0
      DO IR=PIV,NROW
!DC        DO 420,IC=PIV,NCOL
        DO IC=PIV,CPIMAX
          IF (COLSUM(IC).LE.0.0D0) GOTO 419
          FF=DABS(MAT(IR,IC)/COLSUM(IC))
          IF (FF.GT.WERT) THEN
          WERT=FF
          JX=IC
          JY=IR
          END IF
  419     CONTINUE
        END DO
        IF (DABS(WERT).GT.0.0D0) GOTO 411
      END DO
  411 CONTINUE
      IF (JX.EQ.0.OR.JY.EQ.0) RETURN
!-----colchg
      IF (JX.NE.PIV) THEN
        I001=COLNR(JX)
        DO I=1,NROW
          FFC(I)=MAT(I,JX)
        END DO
        COLNR(JX)=COLNR(PIV)
        DO I=1,NROW
          MAT(I,JX)=MAT(I,PIV)
        END DO
        COLNR(PIV)=I001
        DO I=1,NROW
          MAT(I,PIV)=FFC(I)
        END DO
      END IF
!-----rowchg
      IF (JY.NE.PIV) THEN
      I001=ROWNR(JY)
      DO I=1,NCOL
        FFR(I)=MAT(JY,I)
      END DO
      ROWNR(JY)=ROWNR(PIV)
      DO I=1,NCOL
        MAT(JY,I)=MAT(PIV,I)
      END DO
      ROWNR(PIV)=I001
      DO I=1,NCOL
        MAT(PIV,I)=FFR(I)
      END DO
      END IF
!-----
      FF=MAT(PIV,PIV)
      RANG=PIV
!-----divrow
      DO I=1,NCOL
        MAT(PIV,I)=MAT(PIV,I)/FF
      END DO
!-----
      DO II=1,NROW
        IF (II.NE.PIV) THEN
        F1=MAT(II,PIV)
!-----substr
        DO I=1,NCOL
          MAT(II,I)=MAT(II,I)-MAT(PIV,I)*F1
         IF (DABS(MAT(II,I)).LE.1.0D-10) MAT(II,I)=0.0D0
        END DO
!-----
        END IF
      END DO
  500 CONTINUE
      RETURN
      END
!-----
!******************************
      SUBROUTINE PROREAD(SYREC)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!-----END OF COMMON VARIABLES
      LOGICAL(4) MORE
      INTEGER(4) I001,I,II,LLL,LPP
      REAL(8) FF
      CHARACTER(500) SYREC
!*****
      LLL=0
      LPP=60
      DRU=.TRUE.
      SECO=.FALSE.
      VCHECK=.FALSE.
      VNEED=.TRUE.
      NPICK=0
      DO I=1,11
        PRTLOG(I)=.FALSE.
      END DO
!-----READ T AND P FROM UNIT dat
      MORE=.TRUE.
   10 READ (UNIT=dat,FMT='(A500)') SYREC
      IF (SYREC(1:1).EQ.'!') GOTO 10
      CALL GELI(SYREC,TC)
      CALL GELI(SYREC,P)
      CALL GELI(SYREC,PRAT)
      IF (PRAT.LE.0.0D0) PRAT=1.0D0
!-----THE FOLLOWING RECORD IS STORED IN SYREC
      READ (UNIT=dat,FMT='(A500)') SYREC
!-----READ NC AND R FROM UNIT dbs
      READ (UNIT=dbs,FMT='(A500)') REC
      CALL GELI(REC,FF)
      NC=IDINT(FF)
!+++++
      IF (NC.GT.COMAX.OR.NC.LE.0) THEN
      CALL SHOUTD
      WRITE (scr,1000) COMAX,NC
      WRITE (out,1000) COMAX,NC
 1000 FORMAT (// &
      ' The maximum number of components is set to: ',I5/ &
      ' The database defines: ',I5,' components'/ &
      ' possible action:'/ &
      '  a) Increase the value of COMAX in the parameter statements'/ &
      '  b) Remove unused components from the database header'/ &
      '  c) Make a smaller database with fewer components')
      STOP
      END IF
      CALL GELI(REC,R)
      IF (DABS(R).LT.1D-20) R=8.3143D0
!
      CALL GELI(REC,FF)
      WSCAN=IDINT(FF)
      WSCAN=WSCAN*2
      IF (WSCAN.LT.1) WSCAN=0
      IF (WSCAN.GT.4) WSCAN=4
!
      T0=298.15D0
      P0=1.0D0
      TT0=T0*T0
      SQT0=DSQRT(T0)
!-----READ ELEMENTS AND MOLGEW FROM UNIT 18
      DO I=1,NC,7
        I001=MIN0(NC,I+6)
        READ (UNIT=dbs,FMT='(7A10)',ERR=509) (OXYDE(II),II=I,I001)
      END DO
      DO I=1,NC,7
        I001=MIN0(NC,I+6)
        READ (UNIT=dbs,FMT='(7F10.2)',ERR=509) (MOLGEW(II),II=I,I001)
      END DO
      DO I=1,NC,7
        I001=MIN0(NC,I+6)
        READ (UNIT=dbs,FMT='(7F10.2)',ERR=509) (OXANZ(II),II=I,I001)
      END DO
      DO I=1,NC
        CALL FIBLA(OXYDE(I),I001)
        FORM=OXYDE(I)(I001:)
        OXYDE(I)=FORM
      END DO
      GOTO 510
  509 CALL SHOUTD
      WRITE (UNIT=scr,FMT=2000)
      WRITE (UNIT=out,FMT=2000)
 2000 FORMAT (//' Troubles reading the header of the database'/ &
      ' check FORMAT = (7F10.2) (MOLGEW(i)) or OXANZ(i))')
      STOP
  510 CONTINUE
      RETURN
      END
!
!***********************************************************************
!
      SUBROUTINE CALCPAR
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!-----END OF COMMON VARIABLES
      INTEGER(4) I001,I002,I,I1,I2,LPP
      REAL(8) FAR001(10)
      CHARACTER(500) CH001,CH002,SYREC
      CHARACTER(8) CAR001(10)
!----
      DATA CAR001/'LO1MAX','EQUALX','TEST','DELXMIN', &
      'DELXSCAN','DELXSTAR','STEPSTAR','STEPMAX','GCMAX','LPP'/
      DATA FAR001/80.0,1D-5,1D-8,1D-7, &
      1.0D0,2D-2,2.0,3.0,100.0,60.0/
!*****
   22 CONTINUE
      read(UNIT=ini,FMT='(A500)',end=22) SYREC
      IF (SYREC(1:4).EQ.'$END') GOTO 2
      DO 501,I2=1,10
      I002=INDEX(CAR001(I2),' ')
      IF (I002.EQ.0) I002=9
      I001=INDEX(SYREC,CAR001(I2)(1:I002-1))
      IF (I001.NE.0) THEN
      CH002=SYREC(I001:)
      I001=INDEX(CH002,'=')
      CH001=CH002(I001+1:)
      CALL FIBLA(CH001,I001)
      CH002=CH001(I001:)
      I001=INDEX(CH002,' ')
      CH001=CH002(1:I001-1)
      READ (UNIT=CH001,FMT='(BN,D16.0)',ERR=999) FAR001(I2)
      END IF
  501 CONTINUE
      GOTO 22
!-----
    2 CONTINUE
      LO1MAX=IDINT(FAR001(1))
      EQUALX=FAR001(2)
      TEST=FAR001(3)
      DXMIN=FAR001(4)
      DXSCAN=FAR001(5)
      DXSTAR=FAR001(6)
      STPSTA=IDINT(FAR001(7))
      STPMAX=IDINT(FAR001(8))
      GCMAX=IDINT(FAR001(9))
      LPP=IDINT(FAR001(10))
      IF (DXMIN.LE.0.0) DXMIN=1D-7
      IF (DXSCAN.LT.0.001) DXSCAN=1.0D-3
      IF (DXSCAN.GT.1.0) DXSCAN=1.0D0
      IF (DXSTAR.LE.0.0) DXSTAR=DXSCAN/10.0D0
      IF (LPP.LT.30) LPP=30
!-----
      RETURN
!
  999 CALL SHOUTF
      WRITE (scr,1010) CAR001(I2)
      WRITE (out,1010) CAR001(I2)
 1010 FORMAT (//' Troubles with reading the calculation parameter ',A8)
      CALL LABLA(SYREC,I1)
      WRITE (scr,1012) (SYREC(I:I),I=1,I1)
      WRITE (out,1012) (SYREC(I:I),I=1,I1)
 1012 FORMAT (/' RECORD: ',500A1)
      WRITE (scr,1014) (CH001(I:I),I=1,I001-1)
      WRITE (out,1014) (CH001(I:I),I=1,I001-1)
 1014 FORMAT (' The following string cannot be converted', &
      ' to a real number: ',500A1)
      STOP
      END
!
!***********************************************************************
!
      SUBROUTINE MARGINFO(IS,IM)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!-----END OF COMMON VARIABLES
      INTEGER(4) IS,IM,IE,II
      WRITE (6,1000) IS
 1000 FORMAT (/,'IS = solution number =',I3, &
              /,'IM = margules parameter of solution IS', &
              /,'IE = endmember of solution IS', &
              /,'II = indices (1-POLY')
      WRITE (6,1002) IS,NMARG(IS)
 1002 FORMAT (' NMARG(IS)',/,' NMARG(',I3,') =',I3)
!!!      DO IM=1,NMARG(IS)
       WRITE (6,1004) IS,IM,RANGE(IS,IM)
 1004  FORMAT ('   RANGE(IS,IM)',/,'   RANGE(',I3,',',I3,') =',I3)
       WRITE (6,1006) IS,IM,POLY(IS,IM)
 1006  FORMAT ('   POLY(IS,IM)',/,'   POLY(',I3,',',I3,') =',I3)
       WRITE (6,2000) IS,IM,WH(IS,IM)
 2000  FORMAT ('   WH(IS,IM)',/,'    WH(',I3,',',I3,') =',F20.10)
       WRITE (6,2002) IS,IM,WS(IS,IM)
 2002  FORMAT ('   WS(IS,IM)',/,'    WS(',I3,',',I3,') =',F20.10)
       WRITE (6,2004) IS,IM,WV(IS,IM)
 2004  FORMAT ('   WV(IS,IM)',/,'    WV(',I3,',',I3,') =',F20.10)
       WRITE (6,2006) IS,IM,WCP(IS,IM)
 2006  FORMAT ('   WCP(IS,IM)',/,'   WCP(',I3,',',I3,') =',F20.10)
       WRITE (6,2008) IS,IM,WK(IS,IM)
 2008  FORMAT ('   WK(IS,IM)',/,'    WK(',I3,',',I3,') =',F20.10)
       DO IE=1,NEND(IS)
        WRITE (6,1008) IS,IM,IE,DSJDX(IS,IM,IE)
 1008   FORMAT ('   DSJDX(IS,IM,IE)', &
              /,'   DSJDX(',I3,',',I3,',',I3,') =',F4.1)
        WRITE (6,1010) IS,IM,IE,QQ(IS,IM,IE)
 1010   FORMAT ('   QQ(IS,IM,IE)', &
              /,'   QQ(',I3,',',I3,',',I3,') =',I3)
        WRITE (6,1012) IS,IM,IE,SJIND(IS,IM,IE)
 1012   FORMAT ('   SJIND(IS,IM,IE)', &
              /,'   SJIND(',I3,',',I3,',',I3,') =',I3)
       END DO
       DO II=1,POLY(IS,IM)
        WRITE (6,1014) IS,IM,II,SJIND(IS,IM,II)
 1014   FORMAT ('     INDX(IS,IM,II)', &
              /,'     INDX(',I3,',',I3,',',I3,') =',I3)
!!!       END DO
      END DO
!---
      RETURN
      END
!
!***********************************************************************
!
      SUBROUTINE SMARGINFO(IS,IM)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!-----END OF COMMON VARIABLES
      INTEGER(4) IS,IM,II
      WRITE (6,1000) IS
 1000 FORMAT (/,'IS = solution number =',I3, &
              /,'IM = margules parameter of solution IS', &
              /,'II = indices (1-POLY')
      WRITE (6,1002) IS,NSMARG(IS)
 1002 FORMAT (' NSMARG(IS)',/,' NSMARG(',I3,') =',I3)
!!!      DO IM=1,NSMARG(IS)
       WRITE (6,1004) IS,IM,SMSIT(IS,IM)
 1004  FORMAT ('   SMSIT(IS,IM)',/,'   SMSIT(',I3,',',I3,') =',I3)
       WRITE (6,1006) IS,IM,SMPOLY(IS,IM)
 1006  FORMAT ('   SMPOLY(IS,IM)',/,'   SMPOLY(',I3,',',I3,') =',I3)
       WRITE (6,2000) IS,IM,SWH(IS,IM)
 2000  FORMAT ('    SWH(IS,IM)',/,'    SWH(',I3,',',I3,') =',F20.10)
       WRITE (6,2002) IS,IM,SWS(IS,IM)
 2002  FORMAT ('    SWS(IS,IM)',/,'    SWS(',I3,',',I3,') =',F20.10)
       WRITE (6,2004) IS,IM,SWV(IS,IM)
 2004  FORMAT ('    SWV(IS,IM)',/,'    SWV(',I3,',',I3,') =',F20.10)
       DO II=1,SMPOLY(IS,IM)
        WRITE (6,1014) IS,IM,II,SMEI(IS,IM,II)
 1014   FORMAT ('     SMEI(IS,IM,II)', &
              /,'     SMEI(',I3,',',I3,',',I3,') =',I3)
        WRITE (6,1016) IS,IM,II,SINDX(IS,IM,II)
 1016   FORMAT ('     SINDX(IS,IM,II)', &
              /,'     SINDX(',I3,',',I3,',',I3,') =',I3)
       END DO
!!!      END DO
!---
      RETURN
      END

