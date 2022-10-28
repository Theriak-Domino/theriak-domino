!-----Version: 28.05.2022
!               ************
!               * hprogs.f *
!               ************
!     Subroutines with (almost) no COMMON-Blocks
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
      SUBROUTINE FIBLA(CH,II)
      IMPLICIT NONE
      CHARACTER*(*) CH
      INTEGER(4) II,I,LAE
      LAE=LEN(CH)
      DO I=1,LAE
        IF (CH(I:I).NE.' ') GOTO 1
      END DO
    1 II=I
      IF (II.EQ.LAE+1) II=0
      RETURN
      END
!-----
!******************************
      SUBROUTINE LABLA(CH,II)
      IMPLICIT NONE
      CHARACTER*(*) CH
      INTEGER(4) II,I,LAE
      LAE=LEN(CH)
      DO I=LAE,1,-1
        IF (CH(I:I).NE.' ') GOTO 1
      END DO
    1 II=I
      RETURN
      END
!-----
!******************************
      SUBROUTINE PUST(I001,CH)
      IMPLICIT NONE
      CHARACTER*(*) CH
      INTEGER(4) I001,II
      CALL LABLA(CH,II)
      IF (II.EQ.0) II=1
      WRITE (UNIT=I001,FMT='(A)') CH(1:II)
      RETURN
      END
!-----
!******************************
      SUBROUTINE PUSTU(I001,CH)
      IMPLICIT NONE
      CHARACTER*(*) CH
      INTEGER(4) I001,II,I
      CALL LABLA(CH,II)
      IF (II.EQ.0) II=1
      WRITE (UNIT=I001,FMT='(130A1)') ('-',I=1,II)
      WRITE (UNIT=I001,FMT='(A)') CH(1:II)
      WRITE (UNIT=I001,FMT='(130A1)') ('-',I=1,II)
      RETURN
      END
!-----
!******************************
      SUBROUTINE PUSTCOL(I001,CH,COL1,COL2)
      IMPLICIT NONE
      CHARACTER*(*) CH
      CHARACTER(16) FORMA
      INTEGER(4) I001,II,COL1,COL2,LAE,IA,IE,IL,I
      LAE=COL2-COL1+1
      IF (COL1.EQ.1) THEN
      WRITE (UNIT=FORMA,FMT='(''(A)'')')
      ELSE
      WRITE (UNIT=FORMA,FMT='(''('',I3,''X,A)'')') COL1-1
      END IF
      CALL FIBLA(FORMA,IA)
      CALL LABLA(FORMA,IE)
      CALL LABLA(CH,IL)
!      IF (IL.EQ.0) IL=1
      IL=IL+1
      DO I=1,IL,LAE
       II=MIN(I+LAE-1,IL)
       WRITE (UNIT=I001,FMT=FORMA(IA:IE)) CH(I:II)
      END DO
      RETURN
      END
!-----
!******************************
      SUBROUTINE GELI(CH,FF)
      IMPLICIT NONE
      include 'files.cmn'
      CHARACTER*(*) CH
      CHARACTER(250) CH001
      CHARACTER(50) CH002
      CHARACTER(16) CH016
      REAL(8) FF,F1,F2
      INTEGER(4) I001,I002,I,II
      CALL FIBLA(CH,I001)
      IF (I001.EQ.0) THEN
      FF=0.0D0
      RETURN
      END IF
      CH001=CH(I001:)
      I001=INDEX(CH001,' ')
      CH002=CH001(1:I001)
      I002=INDEX(CH002,'/')
      IF (I002.EQ.0) THEN
      CH016=CH001(1:I001-1)
      READ (UNIT=CH016,FMT='(BN,D16.0)',ERR=999) FF
      ELSE
      CH016=CH002(1:I002-1)
      READ (UNIT=CH016,FMT='(BN,D16.0)',ERR=999) F1
      CH016=CH002(I002+1:I001-1)
      READ (UNIT=CH016,FMT='(BN,D16.0)',ERR=999) F2
      FF=F1/F2
      END IF
      CH=CH001(I001:)
      RETURN
  999 CALL SHOUTF
      WRITE (UNIT=6,FMT=302)
      WRITE (UNIT=out,FMT=302)
  302 FORMAT (//' Troubles with format-free reading')
      CALL LABLA(CH001,II)
      WRITE (UNIT=6,FMT=303) (CH001(I:I),I=1,II)
      WRITE (UNIT=out,FMT=303) (CH001(I:I),I=1,II)
  303 FORMAT (/' Remaining record: ',250A1)
      WRITE (UNIT=6,FMT=304) CH016
      WRITE (UNIT=out,FMT=304) CH016
  304 FORMAT (' The following string cannot be converted', &
      ' to a real number: ',A16/ &
      ' PRTCOD = -2 may be useful to trace error')
      STOP
      END
!-----
!******************************
      SUBROUTINE TAXI(REC,CHST)
      IMPLICIT NONE
      CHARACTER*(*) REC
      CHARACTER(250) CH001
      CHARACTER*(*) CHST
      INTEGER(4) I1
      CALL FIBLA(REC,I1)
      IF (I1.EQ.0) THEN
      CHST=' '
      RETURN
      END IF
      CH001=REC(I1:)
      I1=INDEX(CH001,'  ')
      CHST=CH001(1:I1-1)
      REC=CH001(I1:)
      RETURN
      END
!-----
!******************************
      SUBROUTINE TAXI1(REC,CHST)
      IMPLICIT NONE
      CHARACTER*(*) REC
      CHARACTER(250) CH001
      CHARACTER*(*) CHST
      INTEGER(4) I1
      CALL FIBLA(REC,I1)
      IF (I1.EQ.0) THEN
      CHST=' '
      RETURN
      END IF
      CH001=REC(I1:)
      I1=INDEX(CH001,' ')
      CHST=CH001(1:I1-1)
      REC=CH001(I1:)
      RETURN
      END
!-----
!******************************
      SUBROUTINE MAKEZAHL(XX,IST,CHXX,IX)
      IMPLICIT NONE
      CHARACTER(80) CHXE,CHXF
      CHARACTER*(*) CHXX
      CHARACTER(25) FORME,FORMF
      REAL(8) XX,FL1
      INTEGER(4) L,I,I1,I2,IST,IFF,IEE,IX
!-----
      IF (XX.GT.0.0D0) FL1=DLOG10(XX)
      IF (XX.LT.0.0D0) FL1=DLOG10(-XX)
      IF (FL1.GT.0.0D0) THEN
        L=IDINT(FL1+1)
      ELSE
        L=IDINT(FL1-1)
      END IF
      WRITE (UNIT=FORME,FMT=3000) IST+6,IST-1
 3000 FORMAT ('(1PE',I2,'.',I3,')')
      CALL COLLAPS(FORME,I)
      WRITE (UNIT=CHXE,FMT=FORME(1:I)) XX
      CALL COLLAPS(CHXE,IEE)
!
      IF (L.GT.0) I1=IST-L
      IF (L.LT.0) I1=IST-1-L
      IF (I1.LE.0) I1=0
      WRITE (UNIT=FORMF,FMT=3005) I1
 3005 FORMAT ('(1F25.',I3,')')
      CALL COLLAPS(FORMF,I)
      WRITE (UNIT=CHXF,FMT=FORMF(1:I)) XX
      CALL COLLAPS(CHXF,I1)
      I2=I1
      DO I=I1,1,-1
        IF (CHXF(I:I).NE.'0') THEN
         I2=I
         GOTO 10
        END IF
      END DO
   10 CONTINUE
      CHXF(I2+1:)=' '
      CALL LABLA(CHXF,IFF)
      IF (CHXF(IFF:IFF).EQ.'.') THEN
        CHXF(IFF:IFF)=' '
        IFF=IFF-1
      END IF
! return shorter
      IF (IFF.GT.IEE) THEN
        CHXX=CHXE
        IX=IEE
      ELSE
        CHXX=CHXF
        IX=IFF
      END IF

!=====
      RETURN
      END
!
!*************************************************************
!*************************************************************
      SUBROUTINE ZAHL(CH20,I1,I2)
      IMPLICIT NONE
      CHARACTER*(*) CH20
      INTEGER(4) I1,I2,J
!---
      I1=0
      I2=0
      DO J=20,1,-1
      IF (CH20(J:J).NE.'0') THEN
      I2=J
      GOTO 5
      END IF
      END DO
    5 CONTINUE
      IF (I2.NE.0) THEN
      IF (CH20(I2:I2).EQ.'.') I2=I2-1
      END IF
      CALL FIBLA(CH20,I1)
!---
      RETURN
      END
!
!-----
!******************************
      SUBROUTINE NUMTEX(X,B,NT)
      IMPLICIT NONE
      CHARACTER(40) A,B
      INTEGER(4) NT,I,I1,I2
      REAL(8) X
!-----
      A=' '
      WRITE (UNIT=A,FMT='(F22.10)') X
      CALL FIBLA(A,I1)
      I2=40
      DO I=40,1,-1
       IF (A(I:I).EQ.'0'.OR.A(I:I).EQ.' ') THEN
       I2=I2-1
       ELSE
       GOTO 1
       END IF
      END DO
    1 CONTINUE
      IF (A(I:I).EQ.'.') I2=I2-1
      NT=I2-I1+1
      B=A(I1:I2)
!-----
      RETURN
      END
!-----
!******************************
      SUBROUTINE CHEMIE(COMAY,NC,OXYDE,OXANZ,FORMUL,CHE)
      IMPLICIT NONE
      include 'files.cmn'
      INTEGER(4) I,I1,I2,I3,NC,COMAY,II,MACHO
      CHARACTER*(*) FORMUL
      CHARACTER(500) CH170
      CHARACTER(10) ELE,OXYDE(COMAY)
      REAL(8) CHE(COMAY),OXANZ(COMAY),FF
!-----
      MACHO=0
      DO I=1,COMAY
        CHE(I)=0.0D0
      END DO
      CALL FIBLA(FORMUL,I1)
 1001 IF (I1.EQ.0) GOTO 2
      I2=INDEX(FORMUL,'(')
      I3=INDEX(FORMUL,')')
      IF (I2.LE.I1) I2=I1+1
      IF (I3.LE.I2+1) I3=I2+2
      ELE=FORMUL(I1:I2-1)
      DO I=1,NC
        IF (ELE.EQ.OXYDE(I)) GOTO 1
      END DO
    1 IF (I.EQ.NC+1) THEN
      CALL SHOUTF
      CALL LABLA(FORMUL,II)
      WRITE (UNIT=6,FMT=302) (FORMUL(I:I),I=1,II)
      WRITE (UNIT=out,FMT=302) (FORMUL(I:I),I=1,II)
!  100 FORMAT (//' Troubles with formula: ',250A1)
      WRITE (UNIT=6,FMT=102) ELE
      WRITE (UNIT=out,FMT=102) ELE
  102 FORMAT (/1X,A10,': is not a known component')
      STOP
      END IF
      CH170=FORMUL(I2+1:I3-1)
!-----
      IF (ELE.EQ.'O'.AND.CH170.EQ.'?') THEN
      MACHO=I
      ELSE
!     READ (UNIT=CH170,FMT='(BN,D16.0)',ERR=999) FF
      CALL GELI(CH170,FF)
      CHE(I)=CHE(I)+FF
      END IF
!-----
      CH170=FORMUL
      FORMUL=CH170(I3+1:)
      CALL FIBLA(FORMUL,I1)
      GOTO 1001
    2 IF (MACHO.GT.0) THEN
      DO 600,I=1,NC
      CHE(MACHO)=CHE(MACHO)+OXANZ(I)*CHE(I)
  600 CONTINUE
      END IF
      RETURN
!=====
      CALL SHOUTF
      CALL LABLA(FORMUL,II)
      WRITE (UNIT=6,FMT=302) (FORMUL(I:I),I=1,II)
      WRITE (UNIT=out,FMT=302) (FORMUL(I:I),I=1,II)
  302 FORMAT (//' Troubles with formula: ',250A1)
      CALL LABLA(CH170,II)
      WRITE (UNIT=6,FMT=304) (CH170(I:I),I=1,II)
      WRITE (UNIT=out,FMT=304) (CH170(I:I),I=1,II)
  304 FORMAT (' The following string cannot be converted', &
      ' to a real number: ',250A1)
      WRITE (UNIT=6,FMT=306)
      WRITE (UNIT=out,FMT=306)
  306 FORMAT (' PRTCOD = -2 may be useful to trace error')
      STOP
      END
!-----
!********************************
      SUBROUTINE TRENNE(IMAX)
      include 'files.cmn'
      INTEGER(4) I,IMAX
      CHARACTER(1) CH1
      CH1='-'
      WRITE (UNIT=6,FMT='(/132A1)') (CH1,I=1,IMAX)
      WRITE (UNIT=out,FMT='(/132A1)') (CH1,I=1,IMAX)
      RETURN
      END
!-----
!******************************
      SUBROUTINE SHOUTI
      IMPLICIT NONE
      WRITE (UNIT=6,FMT=1000)
 1000 FORMAT (// &
      '    #    #    #  #####   #    #   #####'/ &
      '    #    ##   #  #    #  #    #     #'/ &
      '    #    # #  #  #    #  #    #     #'/ &
      '    #    #  # #  #####   #    #     #'/ &
      '    #    #   ##  #       #    #     #'/ &
      '    #    #    #  #        ####      #'// &
      ' ######  #####   #####    ####   #####'/ &
      ' #       #    #  #    #  #    #  #    #'/ &
      ' #####   #    #  #    #  #    #  #    #'/ &
      ' #       #####   #####   #    #  #####'/ &
      ' #       #   #   #   #   #    #  #   #'/ &
      ' ######  #    #  #    #   ####   #    #')
      RETURN
      END
!-----
!******************************
      SUBROUTINE SHOUTW
      IMPLICIT NONE
      WRITE (UNIT=6,FMT=1000)
 1000 FORMAT (// &
      ' #    #    ##    #####   #    #     #    #    #   ####'/ &
      ' #    #   #  #   #    #  ##   #     #    ##   #  #    #'/ &
      ' #    #  #    #  #    #  # #  #     #    # #  #  #'/ &
      ' # ## #  ######  #####   #  # #     #    #  # #  #  ###'/ &
      ' ##  ##  #    #  #   #   #   ##     #    #   ##  #    #'/ &
      ' #    #  #    #  #    #  #    #     #    #    #   ####')
      RETURN
      END
!-----
!******************************
      SUBROUTINE SHOUTF
      IMPLICIT NONE
      WRITE (UNIT=6,FMT=1000)
 1000 FORMAT (// &
      ' ######    ##     #####    ##    #'/ &
      ' #        #  #      #     #  #   #'/ &
      ' #####   #    #     #    #    #  #'/ &
      ' #       ######     #    ######  #'/ &
      ' #       #    #     #    #    #  #'/ &
      ' #       #    #     #    #    #  ######'// &
      ' ######  #####   #####    ####   #####'/ &
      ' #       #    #  #    #  #    #  #    #'/ &
      ' #####   #    #  #    #  #    #  #    #'/ &
      ' #       #####   #####   #    #  #####'/ &
      ' #       #   #   #   #   #    #  #   #'/ &
      ' ######  #    #  #    #   ####   #    #')
      RETURN
      END
!-----
!******************************
      SUBROUTINE SHOUTD
      IMPLICIT NONE
      WRITE (UNIT=6,FMT=1000)
 1000 FORMAT (// &
      ' #####     ##     #####    ##    #####     ##     #### ', &
      '  ######'/ &
      ' #    #   #  #      #     #  #   #    #   #  #   #     ', &
      '  #'/ &
      ' #    #  #    #     #    #    #  #####   #    #   #### ', &
      '  #####'/ &
      ' #    #  ######     #    ######  #    #  ######       #', &
      '  #'/ &
      ' #    #  #    #     #    #    #  #    #  #    #  #    #', &
      '  #'/ &
      ' #####   #    #     #    #    #  #####   #    #   #### ', &
      '  ######'// &
      ' ######  #####   #####    ####   #####'/ &
      ' #       #    #  #    #  #    #  #    #'/ &
      ' #####   #    #  #    #  #    #  #    #'/ &
      ' #       #####   #####   #    #  #####'/ &
      ' #       #   #   #   #   #    #  #   #'/ &
      ' ######  #    #  #    #   ####   #    #')
      RETURN
      END
!-----
!******************************
      SUBROUTINE LOWUP(REC)
      IMPLICIT NONE
! ----
      CHARACTER*(*) REC
      CHARACTER(250) CH001
      CHARACTER(26) UPPER,LOWER
      INTEGER(4) I1,I2,I,J
      DATA UPPER /'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
      DATA LOWER /'abcdefghijklmnopqrstuvwxyz'/
! ----
      CALL LABLA(REC,I1)
      CH001=' '
      I2=1
      DO 500,I=1,I1
      J=INDEX(LOWER,REC(I:I))
      IF (J.EQ.0) THEN
      CH001(I2:I2)=REC(I:I)
      I2=I2+1
      ELSE
      CH001(I2:I2)=UPPER(J:J)
      I2=I2+1
      END IF
  500 CONTINUE
      REC=CH001
      RETURN
      END
!-----
!******************************
      SUBROUTINE UPLOW2(REC)
      IMPLICIT NONE
! ----
      CHARACTER*(*) REC
      CHARACTER(250) CH001
      CHARACTER(26) UPPER,LOWER
      INTEGER(4) I1,I2,I,J
      DATA UPPER /'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
      DATA LOWER /'abcdefghijklmnopqrstuvwxyz'/
! ----
      CALL LABLA(REC,I1)
      CH001=' '
      I2=1
      DO 500,I=1,I1
      J=INDEX(UPPER,REC(I:I))
      IF (J.EQ.0) THEN
      CH001(I2:I2)=REC(I:I)
      I2=I2+1
      ELSE
      CH001(I2:I2)=LOWER(J:J)
      I2=I2+1
      END IF
  500 CONTINUE
      REC=CH001
      RETURN
      END
!-----
!******************************
      SUBROUTINE COLLAPS(CH,J)
      IMPLICIT NONE
      CHARACTER*(*) CH
      CHARACTER(200) CH1
      INTEGER(4) J,I,LAE
      LAE=LEN(CH)
      CH1=' '
      J=0
      DO 500,I=1,LAE
      IF (CH(I:I).NE.' ') THEN
      J=J+1
      CH1(J:J)=CH(I:I)
      END IF
  500 CONTINUE
      CH=CH1(1:J)
      RETURN
      END
!-----
!******************************
      LOGICAL(4) FUNCTION VERGL(A1,A2)
      IMPLICIT NONE
      CHARACTER*(*) A1,A2
      CHARACTER(250) B1,B2
      B1=A1
      B2=A2
      CALL LOWUP(B1)
      CALL LOWUP(B2)
      VERGL=B1.EQ.B2
      RETURN
      END
!-----
!******************************
      SUBROUTINE SORTIER(CH001,N)
      IMPLICIT NONE
      CHARACTER*(*) CH001(*)
      CHARACTER(80) CH1(2,10000)
      INTEGER(4) I,II,N,K,K2,IN,OUT,IS,CHECK,I1,I2,I1MAX,I2MAX
!-----
      DO I=1,N
        CH1(1,I)=CH001(I)
      END DO
!=====
      IN=1
      OUT=2
      K=1
!=====
    1 CONTINUE
      II=0
      K2=2*K
!=====
      DO 510,IS=1,N,K2
      I1=IS
      I1MAX=IS+K-1
      I2=IS+K
      I2MAX=IS+K2-1
    2 CHECK=0
      IF (I1.GT.I1MAX.OR.I1.GT.N) CHECK=CHECK+1
      IF (I2.GT.I2MAX.OR.I2.GT.N) CHECK=CHECK+2
!-- normalfall
      IF (CHECK.EQ.0) THEN
      II=II+1
!C      IF (CH1(IN,I1).LT.CH1(IN,I2)) THEN
      IF (LLT(CH1(IN,I1),CH1(IN,I2))) THEN
      CH1(OUT,II)=CH1(IN,I1)
      I1=I1+1
      ELSE
      CH1(OUT,II)=CH1(IN,I2)
      I2=I2+1
      END IF
      GOTO 2
      END IF
!-- I1 ist fertig
      IF (CHECK.EQ.1) THEN
      DO I=I2,MIN(I2MAX,N)
        II=II+1
        CH1(OUT,II)=CH1(IN,I)
      END DO
      END IF
!-- I2 ist fertig
      IF (CHECK.EQ.2) THEN
      DO I=I1,MIN(I1MAX,N)
        II=II+1
        CH1(OUT,II)=CH1(IN,I)
      END DO
      END IF
!-- I1 und I2 fertig
  510 CONTINUE
!=====
      K=2*K
      IF (K.GT.N) GOTO 99
      IN=3-IN
      OUT=3-OUT
      GOTO 1
!=====
   99 CONTINUE
      DO I=1,N
        CH001(I)=CH1(OUT,I)
      END DO
!=====
      END
!======================================================================
!======================================================================
!-----
!********************************
      SUBROUTINE LINSOFT2(N0,ARR0,CRR0,DX0,ERRC)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!.....solves a linear system of n equations with n unknowns
!.....
!.....          x(1)*A(1,1) + x(2)*A(1,2) + ...   = C(1)
!.....          x(1)*A(2,1) + x(2)*A(2,2) + ...   = C(2)
!.....          ...
!.....          x(1)*A(n,1) + x(2)*A(n,2) + ...   = C(n)
!.....
!.....N0,N: number of equations
!.....ARR0,ARR: coefficients
!.....CRR0,CRR: constants (right hand side)
!.....DX0: solution
!.....ERRC: 0:OK, 1:rank not N0
      REAL(8) ARR0(PHMAX,PHMAX),CRR0(PHMAX),DX0(PHMAX),COLSUM(PHMAX), &
      FF(COMAX),WERT,F1,FC,RTEST
      INTEGER(4) N0,IC,IR,ERRC,IIX,IIY,PIV,II,I
!-----common blocks
      REAL(8) ARR(PHMAX,PHMAX),CRR(PHMAX)
      INTEGER(4) N,COLNR(PHMAX),RANG
!-----common blocks
!-----end of common
      RTEST=0.0D0
      ERRC=0
      N=N0
      DO IR=1,N
        COLNR(IR)=IR
        CRR(IR)=CRR0(IR)
        DO IC=1,N
          ARR(IR,IC)=ARR0(IR,IC)
        END DO
      END DO
!-----
!=====
!===== reduc
      RANG=0
      DO 600,PIV=1,N
!----- mach colsum
      DO IC=PIV,N
        COLSUM(IC)=0.0D0
        DO IR=1,N
          COLSUM(IC)=COLSUM(IC)+DABS(ARR(IR,IC))
        END DO
      END DO
!----- end mach colsum
      IIY=0
      IIX=0
      WERT=0.0D0
!--- suche max
      DO IR=PIV,N
      DO IC=PIV,N
        IF (COLSUM(IC).LE.0.0D0) GOTO 609
        F1=DABS(ARR(IR,IC)/COLSUM(IC))
        IF (F1.GT.WERT) THEN
        WERT=F1
        IIX=IR
        IIY=IC
        END IF
  609 CONTINUE
      END DO
      END DO
!--- end suche max
      IF (IIX.EQ.0.OR.IIY.EQ.0) GOTO 601
      IF (IIX.NE.PIV) THEN
!---- switch rows
      FC=CRR(IIX)
      DO I=1,N
        FF(I)=ARR(IIX,I)
      END DO
      CRR(IIX)=CRR(PIV)
      DO I=1,N
        ARR(IIX,I)=ARR(PIV,I)
      END DO
      CRR(PIV)=FC
      DO I=1,N
        ARR(PIV,I)=FF(I)
      END DO
!---- end switch rows
      END IF
      IF (IIY.NE.PIV) THEN
!---- switch columns
      II=COLNR(IIY)
      DO I=1,N
        FF(I)=ARR(I,IIY)
      END DO
      COLNR(IIY)=COLNR(PIV)
      DO I=1,N
        ARR(I,IIY)=ARR(I,PIV)
      END DO
      COLNR(PIV)=II
      DO I=1,N
        ARR(I,PIV)=FF(I)
      END DO
!---- end switch columns
      END IF
      F1=ARR(PIV,PIV)
      RANG=PIV
!---- lindiv
      CRR(PIV)=CRR(PIV)/F1
      DO I=1,N
        ARR(PIV,I)=ARR(PIV,I)/F1
      END DO
!---- end lindiv
      DO 500,II=1,N
      IF (II.NE.PIV) THEN
      F1=ARR(II,PIV)
!---- linsub
      CRR(II)=CRR(II)-CRR(PIV)*F1
      DO I=1,N
        ARR(II,I)=ARR(II,I)-ARR(PIV,I)*F1
        IF (RTEST.NE.0.0D0) THEN
        IF (DABS(ARR(II,I)).LE.RTEST) ARR(II,I)=0.0D0
        END IF
      END DO
!---- end linsub
      END IF
  500 CONTINUE
  600 CONTINUE
!===== end reduc
  601 CONTINUE
!=====
      IF (RANG.NE.N) THEN
      WRITE (6,1000) RANG,N
      WRITE (out,1000) RANG,N
 1000 FORMAT ('LINSOFT failed: rank =',I4,'  N =',I4)
      ERRC=1
      RETURN
      END IF
!---- write result into DX0
      DO IC=1,N
        DX0(COLNR(IC))=CRR(IC)
      END DO
!=====
!     DO 800,IR=1,N
!     WRITE (6,2000) IR,DX0(IR)
!     WRITE (out,2000) IR,DX0(IR)
!2000 FORMAT ('linsoft: X =',I5,F20.8)
! 800 CONTINUE
!=====
      RETURN
      END
!-----
!********************************
      SUBROUTINE LINSOFT(N0,AA0,CC0,XX,ERRC)
      include 'files.cmn'
      INTEGER(4) PHMAX
      PARAMETER (PHMAX=500)
!.....solves a linear system of n equations with n unknowns
!.....
!.....          x(1)*A(1,1) + x(2)*A(1,2) + ...   = C(1)
!.....          x(1)*A(2,1) + x(2)*A(2,2) + ...   = C(2)
!.....          ...
!.....          x(1)*A(n,1) + x(2)*A(n,2) + ...   = C(n)
!.....
!.....N0,N: number of equations
!.....AA0,AA: coefficients
!.....CC0,CC: constants (right hand side)
!.....XX: solution
      REAL(8) AA0(PHMAX,PHMAX),CC0(*),XX(*)
      INTEGER(4) N0,IC,IR,ERRC
!-----common blocks
      REAL(8) AA(PHMAX,PHMAX),CC(PHMAX)
      INTEGER(4) N,COLNR(PHMAX),ROWNR(PHMAX),RANG
      COMMON /LINRE/ AA,CC
      COMMON /LININ/ N,COLNR,ROWNR,RANG
!-----end of common
      ERRC=0
      N=N0
      DO IR=1,N
        ROWNR(IR)=IR
        COLNR(IR)=IR
        CC(IR)=CC0(IR)
        DO IC=1,N
          AA(IR,IC)=AA0(IR,IC)
        END DO
      END DO
!-----
!     CALL LINPRT
      CALL REDUC
      IF (RANG.NE.N) THEN
      WRITE (6,1000) RANG,N
      WRITE (out,1000) RANG,N
 1000 FORMAT ('LINSOFT failed: rank =',I4,'  N =',I4)
      CALL LINPRT
      ERRC=1
      RETURN
      END IF
!     CALL LINPRT
      DO IC=1,N
        XX(COLNR(IC))=CC(IC)
      END DO
!=====
!     DO 800,IR=1,N
!     WRITE (6,2000) IR,XX(IR)
!     WRITE (out,2000) IR,XX(IR)
!2000 FORMAT ('linsoft: X =',I5,F20.8)
! 800 CONTINUE
!=====
      RETURN
      END
!-----
!********************************
      SUBROUTINE REDUC
      IMPLICIT NONE
      INTEGER(4) PHMAX
      PARAMETER (PHMAX=500)
!-----common blocks
      REAL(8) AA(PHMAX,PHMAX),CC(PHMAX)
      INTEGER(4) N,COLNR(PHMAX),ROWNR(PHMAX),RANG
      COMMON /LINRE/ AA,CC
      COMMON /LININ/ N,COLNR,ROWNR,RANG
!-----end of common
      INTEGER(4) PIV
      REAL(8) COLSUM(PHMAX),WERT
!----
      INTEGER(4) IR,IC,IX,IY,II,I2
      REAL(8) FF,F1
      RANG=0
      DO 600,PIV=1,N
      DO IC=PIV,N
        COLSUM(IC)=0.0D0
        DO IR=1,N
          COLSUM(IC)=COLSUM(IC)+DABS(AA(IR,IC))
        END DO
      END DO
      IY=0
      IX=0
      WERT=0.0D0
      DO IR=PIV,N
      DO IC=PIV,N
        IF (COLSUM(IC).LE.0.0D0) GOTO 609
        FF=DABS(AA(IR,IC)/COLSUM(IC))
        IF (FF.GT.WERT) THEN
        WERT=FF
        IX=IR
        IY=IC
        END IF
  609 CONTINUE
      END DO
!     IF (WERT.NE.0.0D0) GOTO 611
      END DO
!  611 CONTINUE
      IF (IX.EQ.0.OR.IY.EQ.0) RETURN
      IF (IX.NE.PIV) CALL LINROW(IX,PIV)
      IF (IY.NE.PIV) CALL LINCOL(IY,PIV)
      FF=AA(PIV,PIV)
      RANG=PIV
      CALL LINDIV(PIV,FF)
      DO II=1,N
        IF (II.NE.PIV) THEN
        F1=AA(II,PIV)
        I2=II
        CALL LINSUB(PIV,F1,I2)
        END IF
      END DO
  600 CONTINUE
      RETURN
      END
!-----
!********************************
      SUBROUTINE LINPRT
      IMPLICIT NONE
      include 'files.cmn'
      INTEGER(4) PHMAX
      PARAMETER (PHMAX=500)
!-----common blocks
      REAL(8) AA(PHMAX,PHMAX),CC(PHMAX)
      INTEGER(4) N,COLNR(PHMAX),ROWNR(PHMAX),RANG
      COMMON /LINRE/ AA,CC
      COMMON /LININ/ N,COLNR,ROWNR,RANG
!-----end of common
      INTEGER(4) I,II
!----
      WRITE (UNIT=6,FMT='(10(20X,10I10/))') (COLNR(I),I=1,N)
      WRITE (UNIT=out,FMT='(10(20X,10I10/))') (COLNR(I),I=1,N)
      DO 500,II=1,N
      WRITE (UNIT=6,FMT='(/I10,11E10.3,10(/10X,10E10.3))') &
      ROWNR(II),CC(II),(AA(II,I),I=1,N)
      WRITE (UNIT=out,FMT='(/I10,11E10.3,10(/10X,10E10.3))') &
      ROWNR(II),CC(II),(AA(II,I),I=1,N)
  500 CONTINUE
      RETURN
      END
!-----
!********************************
      SUBROUTINE LINDIV(I1,FF)
      IMPLICIT NONE
      INTEGER(4) PHMAX
      PARAMETER (PHMAX=500)
!-----common blocks
      REAL(8) AA(PHMAX,PHMAX),CC(PHMAX)
      INTEGER(4) N,COLNR(PHMAX),ROWNR(PHMAX),RANG
      COMMON /LINRE/ AA,CC
      COMMON /LININ/ N,COLNR,ROWNR,RANG
!-----end of common
      INTEGER(4) I1,I
      REAL(8) FF
      CC(I1)=CC(I1)/FF
      DO I=1,N
        AA(I1,I)=AA(I1,I)/FF
      END DO
      RETURN
      END
!----
!-----
!********************************
      SUBROUTINE LINCOL(I1,I2)
      IMPLICIT NONE
      INTEGER(4) PHMAX
      PARAMETER (PHMAX=500)
!-----common blocks
      REAL(8) AA(PHMAX,PHMAX),CC(PHMAX)
      INTEGER(4) N,COLNR(PHMAX),ROWNR(PHMAX),RANG
      COMMON /LINRE/ AA,CC
      COMMON /LININ/ N,COLNR,ROWNR,RANG
!-----end of common
      INTEGER(4) I,II,I1,I2
      REAL(8) FF(PHMAX)
!----
      II=COLNR(I1)
      DO I=1,N
        FF(I)=AA(I,I1)
      END DO
      COLNR(I1)=COLNR(I2)
      DO I=1,N
        AA(I,I1)=AA(I,I2)
      END DO
      COLNR(I2)=II
      DO I=1,N
        AA(I,I2)=FF(I)
      END DO
      RETURN
      END
!-----
!********************************
      SUBROUTINE LINROW(I1,I2)
      IMPLICIT NONE
      INTEGER(4) PHMAX
      PARAMETER (PHMAX=500)
!-----common blocks
      REAL(8) AA(PHMAX,PHMAX),CC(PHMAX)
      INTEGER(4) N,COLNR(PHMAX),ROWNR(PHMAX),RANG
      COMMON /LINRE/ AA,CC
      COMMON /LININ/ N,COLNR,ROWNR,RANG
!-----end of common
      INTEGER(4) I,II,I1,I2
      REAL(8) FF(PHMAX),FC
!----
      II=ROWNR(I1)
      FC=CC(I1)
      DO I=1,N
        FF(I)=AA(I1,I)
      END DO
      ROWNR(I1)=ROWNR(I2)
      CC(I1)=CC(I2)
      DO I=1,N
        AA(I1,I)=AA(I2,I)
      END DO
      ROWNR(I2)=II
      CC(I2)=FC
      DO I=1,N
        AA(I2,I)=FF(I)
      END DO
      RETURN
      END
!-----
!********************************
      SUBROUTINE LINSUB(I1,F1,I2)
      IMPLICIT NONE
      INTEGER(4) PHMAX
      PARAMETER (PHMAX=500)
!-----common blocks
      REAL(8) AA(PHMAX,PHMAX),CC(PHMAX)
      INTEGER(4) N,COLNR(PHMAX),ROWNR(PHMAX),RANG
      COMMON /LINRE/ AA,CC
      COMMON /LININ/ N,COLNR,ROWNR,RANG
!-----end of common
      INTEGER(4) I,I1,I2
      REAL(8) F1
!----
      CC(I2)=CC(I2)-CC(I1)*F1
      DO I=1,N
        AA(I2,I)=AA(I2,I)-AA(I1,I)*F1
!       IF (DABS(AA(I2,I)).LE.1.0D-10) AA(I2,I)=0.0D0
      END DO
      RETURN
      END
!
!-----
!*GAUSSEL***************************************************************
! Subroutine to find solution of a linear system of N equations in N   *
! unknowns using Gaussian elimination, provided a unique solution      *
! exists.  The coefficients and constants of the linear system are     *
! stored in the matrix MAT, which has ROWMAX rows and COLMAX columns.  *
! If the system is singular, SING is returned as true, and the         *
! solution X is undefined.  Local identifiers used are:                *
!     I,J,K  : subscripts                                              *
!     MULT   : multiplier used to eliminate an unknown                 *
!     VALUE : absolute value of pivot element                          *
!     PROW : row containing pivot element                              *
!     DX  : a small positive real value ("almost zero")                *
!     TEMP   : used to interchange rows of matrix                      *
!                                                                      *
! Accepts: Two-dimensional array MAT, integers ROWMAX, COLMAX, and N   *
! Returns: One-dimensional array X and logical value SING              *
!***********************************************************************
! http://oregonstate.edu/instruct/ch590/lessons/lesson13.htm 
!.....solves a linear system of n equations with n unknowns
!.....
!.....          x(1)*MAT(1,1) + x(2)*MAT(1,2) + ...   = MAT(1,n+1)
!.....          x(1)*MAT(2,1) + x(2)*MAT(2,2) + ...   = MAT(2,n+1)
!.....          ...
!.....          x(1)*MAT(n,1) + x(2)*MAT(n,2) + ...   = MAT(n,n+1)
!.....
!.....N: number of equations
!.....MAT: coefficients + constants (right hand side)
!.....X: solution
!.....SING: true if singularity
 
      SUBROUTINE GAUSSEL(MAT,ROWMAX,COLMAX,N,X,SING)
      IMPLICIT NONE
!
      INTEGER(4) ROWMAX,COLMAX,N,PROW,I,J,K
      REAL(8) MAT(45,45),X(45),TEMP,MULT,VALUE
      LOGICAL(4) SING
!
      SING = .FALSE.
      DO I=1,N
!===  Locate pivot element
        VALUE=DABS(MAT(I,I))
        PROW=I
        DO K=I+1,N
         IF (DABS(MAT(K,I)).GT.VALUE) THEN
          VALUE=DABS(MAT(K,I))
          PROW=K
         END IF
        END DO
!---  Check if matrix is (nearly) singular
        IF (VALUE.LT.1D-13) THEN
         SING=.TRUE.
         RETURN
        END IF
!---  It isn't, so interchange rows PROW and I if necessary
        IF (PROW.NE.I) THEN
         DO J=1,N+1
          TEMP=MAT(I,J)
          MAT(I,J)=MAT(PROW,J)
          MAT(PROW,J)=TEMP
         END DO
        END IF
!---  Eliminate Ith unknown from equations I + 1, ..., N
        DO J=I+1,N
         MULT=-MAT(J,I)/MAT(I,I)
         DO K=I,N+1
          MAT(J,K)=MAT(J,K)+MULT*MAT(I,K)
         END DO
        END DO
!===
      END DO
!---  Find the solutions by back substitution
      X(N)=MAT(N,N+1)/MAT(N,N)
      DO J=N-1,1,-1
       X(J)=MAT(J,N+1)
       DO K=J+1,N
        X(J)=X(J)-MAT(J,K)*X(K)
       END DO
       X(J)=X(J)/MAT(J,J)
      END DO
!
      END
!

