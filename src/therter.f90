!-----Version: 28.05.2022
!               ***********
!               * THERTER *
!               ***********
!
!     Calculation of ternary phase diagram
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
!
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      INCLUDE 'files.cmn'
!-----
!-----VARIABLES FOR TERNARY PHASE DIAGRAM
!     number of triangles: MAXTR
!     total number of tielines: MAXTI
!     number of tieline-groups: MAXLI
!     number of tielines per group: MAXZU
!     number of seeds: MAXSE
      INTEGER(4) MAXTR,MAXTI,MAXLI,MAXZU,MAXSE
      PARAMETER (MAXTR=50,MAXTI=1000,MAXLI=30,MAXZU=400,MAXSE=200)
!=====
      INTEGER(4) CORNER(3),NUNEW,NUNORI,CHORIG(COMAX),NAKKU, &
      IAKKU(PHMAX),IN2,IN3,NTIE,NRL,NDREI,NSEED,EGGE(MAXTR), &
      FIRSTP(MAXLI),LASTP(MAXLI),NPHT,TPHA(COMAX,2),CODE1
      COMMON /JAIN/ CORNER,NUNEW,NUNORI,CHORIG,NAKKU,IAKKU,IN2,IN3, &
      NTIE,NRL,NDREI,NSEED,EGGE,FIRSTP,LASTP,NPHT,TPHA,CODE1
      REAL(8) XXECK(3,COMAX),XAKKU(PHMAX,3),ZUG(MAXZU,3), &
      DREIX(MAXTR,COMAX,3),TIEX(MAXTI,2,3),FFF(3),TOLER, &
      SEED(MAXSE,3),XCORN(COMAX+3,0:COMAX)
      COMMON /JARE/ XXECK,XAKKU,ZUG,DREIX,TIEX,FFF,TOLER,SEED,XCORN
      CHARACTER(20) ECKE(3),AKKU(PHMAX)
      CHARACTER(25) ASNAM(COMAX),TCHA(COMAX)
      COMMON /JACH/ ECKE,AKKU,ASNAM,TCHA
!-----END OF COMMON VARIABLES
      LOGICAL(4) MORE
      INTEGER(4) I001,I002,I,I1,I2,II,COMAY,j,ierr,LARG
      REAL(8) FF,TIN,PIN
      CHARACTER(8) SORTSTRING(PHMAX)
      CHARACTER(80) DATI,KOMMENTAR
      CHARACTER(500) CH001,CH002,SYREC,CHIN(4),ZEITSTRING
!-----
      INTEGER(4) NSCAN,K,IVOR,LPR3,SCMODE
      REAL(8) BSCAN,EMBULK(3),NEU(3),ALT(3),URALT(3),SUDIA1,SUDIA2, &
      FF1,SUDIA,NICHT0
      LOGICAL(4) BOING,STRICT(PHMAX+1)
!-----minimat variables
      REAL(8) AA(50,4),KOEFF(4),F1,F2,F3,F4
      INTEGER(4) NCOL,NROW,RANG,ICOL,IROW,CONR(50),ECKCODE,OUTCODE, &
      COPHASE(3),ICH
!*****
      progname='THERTER'
      vers='28.05.2022'
      task='"Computation of ternary phase diagrams"'
      ierr=0
      CALL initialize('$THERTER-FILES',ierr)
      if(ierr.ne.0) STOP
!-----
      LARG=0
      IERR=0
      DO I=1,5
      CALL GetLineArgs (I,LARGUM(I),IERR)
      IF(IERR.NE.0.OR.LARGUM(I).EQ.' ')THEN
      GOTO 399
      ELSE
      LARG=LARG+1
      END IF
      END DO
  399 CONTINUE
      IFNR=5
      IF (LARG.GT.0) THEN
      INFILE=LARGUM(1)
      CALL LABLA(INFILE,I1)
      IFNR=39
      OPEN (UNIT=IFNR,FILE=INFILE(1:I1),STATUS='UNKNOWN')
      END IF
      IF (LARG.GT.1) THEN
      filename(dat)=LARGUM(2)
      CALL LABLA(filename(dat),fnl(dat))
      END IF
!-----
!+++++ CODE1=1 will print test-information
      CODE1=0
!*****
      DO I=1,3
        COPHASE(I)=0
      END DO
      DO I=1,4
        CHIN(I)=' '
      END DO
      TIN=0.0D0
      PIN=0.0D0
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
      DO I=1,4
        READ (UNIT=log,FMT='(A500)',END=111) CHIN(I)
      END DO
      READ (log,FMT='(F20.8,2X,F20.8)',END=111) TIN,PIN
  111 CONTINUE
      CALL LABLA(CHIN(1),I002)
      IF (I002.EQ.0) I002=1
!-----
      WRITE (scr,112)
  112 FORMAT (/ &
      '-------------------'/ &
      'database definition'/ &
      '-------------------')
      CH002='Enter [ "?" | CR | "files" | database filename ] <'// &
      CHIN(1)(1:I002)//'>?'
!-----
  412 continue
      CALL PUST (scr,CH002)
      READ (IFNR,FMT='(A500)') CH001
      IF (CH001.EQ.' ') THEN
          CH001=CHIN(1)
          I001=I002
      else if (CH001.eq.'?') then
         call helpme('$THT-START')
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
!-----
      REWIND (UNIT=out)
      REWIND (UNIT=dbs)
      REWIND (UNIT=dat)
      REWIND (UNIT=plt)
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
!-----
      CALL TAXI(SYREC,FORMUL)
      CALL TAXI(SYREC,USE)
      CALL CHEMIE(COMAY,NC,OXYDE,OXANZ,FORMUL,CHEM)
      CALL LABLA(USE,LUSE)
      CALL TAXI(SYREC,KOMMENTAR)
!
      IF (PRTCOD.EQ.0) TEST=DABS(TEST)
      CALL DBREAD
      IF (PRTLOG(1)) STOP
      CLOSE(UNIT=dat)
!-----START LOOPING
!     CALL NURVONPT
!     CALL CALSTR
!     IF (PRTLOG(2).OR.PRTLOG(3).OR.PRTLOG(4)) CALL PRININ
!.....
!     PRTLOG(6)=.TRUE.
!.....
!+++++
      DO I=1,NUN
        CHORIG(I)=CHMCOD(I)
      END DO
      NUNORI=NUN
      NAKKU=0
!+++++
      write(scr,113)
  113 FORMAT (/ &
      '----------'/ &
      'endmembers'/ &
      '----------')
      DO II=1,NPHA
        SORTSTRING(II)=ABK(II)
      END DO
      I001=NPHA
      CALL SORTIER(SORTSTRING,I001)
      WRITE (UNIT=scr,FMT='(/,12(A8,1X))') &
      (SORTSTRING(II),II=1,I001)
!C      WRITE (UNIT=scr,FMT='(/,9(A8,1X))') (ABK(II),II=1,NPHA)
!-----
   10 continue
!-----
      DO 570,I=1,3
      CALL LABLA(CHIN(I+1),I002)
      IF (I002.EQ.0) I002=1
   13 CONTINUE
      WRITE (UNIT=scr,FMT=2000) I,I,CHIN(I+1)(1:I002)
 2000 format (/,'Enter [ "?" | "list" | CR | endmember ',i1, &
      ' (formula ',i1,') ] <',a,'>?')
      READ (UNIT=IFNR,FMT='(A500)') CH001
      CALL TAXI(CH001,NAM)
      IF (VERGL(NAM,'list')) THEN
      DO II=1,NPHA
        SORTSTRING(II)=ABK(II)
      END DO
      I001=NPHA
      CALL SORTIER(SORTSTRING,I001)
      WRITE (UNIT=scr,FMT='(/,12(A8,1X))') &
      (SORTSTRING(II),II=1,I001)
      goto 13
      else if (NAM.EQ.'?') THEN
         call helpme('$THT-ENDMEMB')
         goto 13
      else if (NAM.EQ.' ') THEN
         CH001=CHIN(I+1)(1:I002)
         CALL TAXI(CH001,NAM)
      else
         CALL LABLA(NAM,I001)
         CHIN(I+1)=NAM(1:I001)//'   '//CH001(1:231)
      end if
!-----
      ECKE(I)=NAM
      I001=0
      DO II=1,NPHA
        IF (NAM.EQ.NAME(II).OR.NAM.EQ.ABK(II)) THEN
        I001=II
        GOTO 561
        END IF
      END DO
  561 IF (I001.NE.0) THEN
      DO II=1,NC
        XXECK(I,II)=0.0D0
      END DO
      DO II=1,NUN
        XXECK(I,CHMCOD(II))=XX(I001,II)
      END DO
      ELSE
      I1=0
      CALL TAXI(CH001,FORMUL)
      CALL CHEMIE(COMAY,NC,OXYDE,OXANZ,FORMUL,CHE)
      DO II=1,NC
        IF (CHE(II).NE.0.0D0) I1=I1+1
        XXECK(I,II)=CHE(II)
      END DO
      IF (I1.EQ.0) THEN
      CALL SHOUTI
      WRITE (UNIT=6,FMT=2004) NAM
 2004 FORMAT (/A16,' has no composition')
      GOTO 10
      END IF
      END IF
  570 CONTINUE
!-----
      write(scr,114)
  114 FORMAT (/ &
      '----------'/ &
      'conditions'/ &
      '----------')
      WRITE (scr,2010) TIN,PIN
 2010 FORMAT (/,'Enter [ CR | T(C)  P(Bar) ] <',F10.2,2X,F10.2,'> ?')
      READ (UNIT=IFNR,FMT='(A500)') CH001
      IF (CH001.EQ.' ') THEN
      TC=TIN
      P=PIN
      ELSE
      CALL GELI(CH001,TC)
      CALL GELI(CH001,P)
      TIN=TC
      PIN=P
      END IF
      T=TC+273.15D0
      CALL NURVONPT
      CALL CALSTR
!+++++store terminal input
      CLOSE (UNIT=log)
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
      DO I=1,4
        CALL PUST(log,CHIN(I))
      END DO
      WRITE (log,FMT='(F20.8,2X,F20.8)') TIN,PIN
      CLOSE (UNIT=log)
!
!***************************************************************
!******  END OF INPUT UND TESTS. START CALCULATING *************
!***************************************************************
!-----
      CALL LABLA(CHIN(1),I002)
      NCOMIN=3
      COMINS(1)=KOMMENTAR
      COMINS(2)='therter version: '//vers(1:10)
!      COMINS(3)='database: '//CHIN(1)(1:I002)
      CALL LABLA(DBNAME,I002)
      COMINS(3)='database: '//DBNAME(1:I002)
!      DO 690,I=1,NPHA
!      IF (LODATA(3,I)) THEN
!      NCOMIN=NCOMIN+1
!      COMINS(NCOMIN)=ABK(I)
!      I1=INDEX(COMINS(NCOMIN),'  ')
!      COMINS(NCOMIN)(I1:)=': '//CHDATA(I)
!      IF (NCOMIN.GE.50) GOTO 691
!      END IF
!  690 CONTINUE
!      DO II=1,NSOL
!       CALL LABLA(SOLNAM(II),I001)
!       NCOMIN=NCOMIN+1
!       COMINS(NCOMIN)=SOLNAM(II)(1:I001)//': '//SOLINFO(II)
!       IF (NCOMIN.GE.50) GOTO 691
!      END DO
!  691 CONTINUE
!-----
      WRITE (plt,3000)
 3000 FORMAT ('NULLPT      5  5  '/'FAT   0.02 ', &
      /'PUNKTE   97   0.5   16.5  5.25  999  999', &
      /'PUNKTE   98   0.5   16.5  5.25  999  999', &
      /'FAT   0.01 ', &
      /'SEITE      15   ', &
      /'FONT     Helvetica  ')
      WRITE (UNIT=CH,FMT='(F9.2)') TC
      CALL FIBLA(CH,I1)
      CALL LABLA(CH,I2)
      WRITE (plt,3001) 'T = '//CH(I1:I2)//' [C]'
 3001 FORMAT ('PSYM     ',A20,'  16  9  0  0.5  0  0  0  0')
      WRITE (UNIT=CH,FMT='(F9.1)') P
      CALL FIBLA(CH,I1)
      CALL LABLA(CH,I2)
      WRITE (plt,3002) 'P = '//CH(I1:I2)//' [Bar]'
 3002 FORMAT ('PSYM     ',A20,'  16  9  0  0.5  0  0  -2  0')
      WRITE (plt,3005) (ECKE(I),I=1,3)
 3005 FORMAT ('ECKEN   ',3A20,' 0.5  0.5  ')
!-----
   18 CONTINUE
      CH001='0'
      WRITE (scr,2020) CH001(1:1)
 2020 FORMAT (//,'Enter ["?" | CR | number of seeds ] <',a,'>?')
      READ (IFNR,FMT='(A500)') CH001
      if(CH001.eq.'?') then
         call helpme('$THBT-SEED')
         goto 18
      end if
      CALL GELI(CH001,FF)
      NSEED=IDINT(FF)
      IF (NSEED.GT.0) THEN
!=====
      WRITE (scr,2022) (ECKE(I),I=1,3),NSEED
 2022 FORMAT ('Variables are:'/3A20/'Enter [ XYZ for ',i2.2, &
      ' seed points ]')
      DO I=1,NSEED
        write(scr,'(''XYZ for seed point'',i2.2,'' ? '')') I
        READ (IFNR,*,end=579,err=579) (SEED(I,II),II=1,3)
        NSEED=I
      END DO
  579 continue
      if(I.ne.NSEED) NSEED=I-1
      END IF
!-----
      NSEED=NSEED+1
      SEED(NSEED,1)=0.98D0
      SEED(NSEED,2)=0.01D0
      SEED(NSEED,3)=0.01D0
      NSEED=NSEED+1
      SEED(NSEED,1)=0.01D0
      SEED(NSEED,2)=0.98D0
      SEED(NSEED,3)=0.01D0
      NSEED=NSEED+1
      SEED(NSEED,1)=0.01D0
      SEED(NSEED,2)=0.01D0
      SEED(NSEED,3)=0.98D0
!-----
   19 CONTINUE
      CH001="11  0.02"
      WRITE (scr,2030) CH001(1:8)
 2030 FORMAT('Enter [ "?" | CR | scan-density  tolerance ] <', &
      a,'>?')
      READ (IFNR,FMT='(A500)') CH001
      if(CH001.eq.'?') then
         call helpme('$THT-SCAN')
         goto 19
      end if
!=====
      CALL GELI(CH001,FF)
      IF (FF.LE.0.0D0) FF=11.0D0
      NSCAN=IDINT(FF)
      IF (NSCAN.LE.0) THEN
      BSCAN=1.0D0
      ELSE
      BSCAN=1.0D0/FF
      END IF
      CALL GELI(CH001,TOLER)
      IF (TOLER.LE.0.0D0) TOLER=0.02D0
      NDREI=0
      NTIE=0
      LPR3=0
!-----check phases
      NROW=NC
      NCOL=4
      RANG=0
!=============================================
      DO 900,I=NUN+1,NPHA
      DO ICOL=1,3
        DO IROW=1,NC
          AA(IROW,ICOL)=XXECK(ICOL,IROW)
        END DO
      END DO
      DO IROW=1,NC
        AA(IROW,4)=0.0D0
      END DO
      DO IROW=1,NUN
        AA(CHMCOD(IROW),4)=XX(I,IROW)
      END DO
      DO ICOL=1,4
        CONR(ICOL)=ICOL
      END DO
      CALL MINIMAT(AA,NCOL,NROW,CONR,RANG)
!.....
!     WRITE (UNIT=scr,FMT='(A16,2X,I3)') NAME(NUMMER(I)),RANG
!     WRITE (UNIT=out,FMT='(A16,2X,I3)') NAME(NUMMER(I)),RANG
!.....
      IF (RANG.GT.3) THEN
!.....
      WRITE (scr,2050) NAME(NUMMER(I))
      WRITE (out,2050) NAME(NUMMER(I))
 2050 FORMAT (A16,6X, &
      ' -------> phase excluded (outside ternary system)')
!.....
!      NULL(I)=.TRUE.
      STRICT(I)=.FALSE.
      ELSE
      STRICT(I)=.TRUE.
!.....
!      WRITE (scr,2055) NAME(NUMMER(I)),CONR(4),AA(1,4),CONR(1), &
!      AA(2,4),CONR(2),AA(3,4),CONR(3)
!      WRITE (out,2055) NAME(NUMMER(I)),CONR(4),AA(1,4),CONR(1), &
!      AA(2,4),CONR(2),AA(3,4),CONR(3)
! 2055 FORMAT(A16,2X,':  ',I2,'=',3(F10.4,'*',I2))
!.....the following only here, after the first dbread
      DO IROW=1,3
        KOEFF(CONR(IROW))=-AA(IROW,4)
      END DO
      KOEFF(CONR(4))=1.0D0
      OUTCODE=0
      ECKCODE=0
      DO IROW=1,3
        IF (KOEFF(IROW)*KOEFF(4).GT.0.0D0) OUTCODE=1
        IF (KOEFF(IROW).EQ.0.0D0) ECKCODE=ECKCODE+1
      END DO
      IF (OUTCODE.EQ.1) THEN
      WRITE (scr,2057) NAME(NUMMER(I))
      WRITE (out,2057) NAME(NUMMER(I))
 2057 FORMAT (A16,6X,': WARNING - phase outside triangle')
!ccc muss unbeding ausgeschlossen werden. (koeff wird sonst -1d+15 etc...)
      STRICT(I)=.FALSE.
      ELSE
      NSEED=NSEED+1
      SEED(NSEED,CONR(1))=AA(1,4)
      SEED(NSEED,CONR(2))=AA(2,4)
      SEED(NSEED,CONR(3))=AA(3,4)
      NSEED=NSEED+1
      SEED(NSEED,CONR(1))=AA(1,4)+0.01D0
      SEED(NSEED,CONR(2))=AA(2,4)
      SEED(NSEED,CONR(3))=AA(3,4)
      NSEED=NSEED+1
      SEED(NSEED,CONR(1))=AA(1,4)
      SEED(NSEED,CONR(2))=AA(2,4)+0.01D0
      SEED(NSEED,CONR(3))=AA(3,4)
      NSEED=NSEED+1
      SEED(NSEED,CONR(1))=AA(1,4)
      SEED(NSEED,CONR(2))=AA(2,4)
      SEED(NSEED,CONR(3))=AA(3,4)+0.01D0
      END IF
      IF (ECKCODE.EQ.2) THEN
      DO IROW=1,3
        IF (KOEFF(IROW)*KOEFF(4).LT.0.0D0) THEN
        COPHASE(IROW)=COPHASE(IROW)+1
        GOTO 951
        END IF
      END DO
  951 CONTINUE
      END IF
!-----end of else
      END IF
  900 CONTINUE
!=============================================
      DO IROW=1,3
        IF (COPHASE(IROW).EQ.0) THEN
        WRITE (scr,2059) ECKE(IROW)
        WRITE (out,2059) ECKE(IROW)
 2059 FORMAT(A20,2X,': WARNING - no phase in this corner')
        END IF
      END DO
!-----end check phases
      NPICK=0
      WRITE (scr,2071)
      WRITE (out,2071)
 2071 FORMAT (/,'considered phases:')
      DO I=NUN+1,NPHA
        IF (STRICT(I)) THEN
        WRITE (scr,2070) NAME(NUMMER(I))
        WRITE (out,2070) NAME(NUMMER(I))
        NPICK=NPICK+1
        PICK(NPICK)=NAME(NUMMER(I))
 2070 FORMAT (A16)
        END IF
      END DO
!*****
!
      CALL DBREAD
      CALL FLUSH(6)
!
!
!*****
!-----define COMINS
      DO I=1,NPHA
        IF (LODATA(3,I).AND.(.NOT.NULL(I))) THEN
        NCOMIN=NCOMIN+1
        COMINS(NCOMIN)=ABK(I)
        I1=INDEX(COMINS(NCOMIN),'  ')
        COMINS(NCOMIN)(I1:)=': '//CHDATA(I)
        IF (NCOMIN.GE.50) GOTO 691
        END IF
      END DO
      DO II=1,NSOL
       NICHT0=0
       DO I=1,NEND(II)
        IF (.NOT.NULL(EM(II,I))) NICHT0=NICHT0+1
       END DO
       IF (NICHT0.NE.0) THEN
       CALL LABLA(SOLNAM(II),I001)
       NCOMIN=NCOMIN+1
       COMINS(NCOMIN)=SOLNAM(II)(1:I001)//': '//SOLINFO(II)
       IF (NCOMIN.GE.50) GOTO 691
       END IF
      END DO
  691 CONTINUE
!********************
!     start mega-loop
!
      DO 602,I1=0,NSCAN
      DO 600,I2=0,(NSCAN-I1)
  100 IF (NSEED.GT.0) THEN
      SCMODE=0
      EMBULK(1)=DMAX1(SEED(NSEED,1),0.0D0)
      EMBULK(2)=DMAX1(SEED(NSEED,2),0.0D0)
      EMBULK(3)=DMAX1(SEED(NSEED,3),0.0D0)
      NSEED=NSEED-1
      ELSE
      SCMODE=1
      IF (NSCAN.EQ.0) GOTO 666
      EMBULK(1)=DBLE(I1)*BSCAN
      EMBULK(2)=DBLE(I2)*BSCAN
      EMBULK(3)=DBLE(NSCAN-I1-I2)*BSCAN
      END IF
!.....
      IF (CODE1.EQ.1) THEN
      CH001=' '
      IF (SCMODE.EQ.0) THEN
      WRITE (UNIT=CH001,FMT='('' SEED: '')')
      ELSE
      WRITE (UNIT=CH001,FMT='('' SCAN: '')')
      END IF
      WRITE (UNIT=CH001(8:),FMT=4000) (EMBULK(I),I=1,3)
 4000 FORMAT(3F9.5)
      END IF
!.....
      CALL INSIDE(EMBULK)
!.....
      IF (CODE1.EQ.1) THEN
      IF (IN3.NE.0) THEN
      WRITE (UNIT=CH001(40:),FMT=4001) EGGE(IN3),IN3
 4001 FORMAT(' : INSIDE',I3,'-PHASE REGION',I3)
      WRITE (scr,FMT='(A80)') CH001
      WRITE (out,FMT='(A80)') CH001
      END IF
      IF (IN2.NE.0) THEN
      WRITE (UNIT=CH001(40:),FMT=4002) IN2
 4002 FORMAT (' : INSIDE TWO-PHASE REGION',I3)
      WRITE (scr,FMT='(A80)') CH001
      WRITE (out,FMT='(A80)') CH001
      END IF
      END IF
!.....
!-----
!     first if not inside
!
      IF (IN2.EQ.0.AND.IN3.EQ.0) THEN
!.....
      IF (CODE1.EQ.1) THEN
      WRITE (scr,FMT='(A80)') CH001
      WRITE (out,FMT='(A80)') CH001
      END IF
!.....
      CALL JANUS(EMBULK)
!.....
      IF (CODE1.EQ.1) THEN
      DO I=4,I001
        WRITE (scr,4008) (XCORN(I,II),II=1,3)
 4008 FORMAT (' XCORN ',3F12.6)
      END DO
      END IF
!.....
      IF (NUNEW.EQ.3) THEN
      DO I=1,NUNEW
        DO II=1,3
          ZUG(I,II)=XCORN(I+3,II)
        END DO
      END DO
      CALL NEWTRI(NUNEW)
      END IF
!-----
!     first if two phases
!
      IF (NUNEW.EQ.2) THEN
      DO I=1,2
        DO II=1,3
          ZUG(I,II)=XCORN(I+3,II)
        END DO
      END DO
      CALL NEWTIE
!----
      NPHT=NUN2
      DO I=1,NUN2
        TCHA(I)=ASNAM(I)
        TPHA(I,1)=NUMMER(I)
        TPHA(I,2)=EMCODE(I)
      END DO
!-----
!     invert solvus loop
!
      DO 610,IVOR=1,-1,-2
      DO I=1,3
        ALT(I)=(TIEX(NRL,1,I)+TIEX(NRL,2,I))/2.0D0
        URALT(I)=ALT(I)-DBLE(IVOR)*FFF(I)
      END DO
      FF=1.0D0
!-----
!     map solvus loop
!
  700 IF (FF.LT.0.05) GOTO 800
      DO I=1,3
        NEU(I)=ALT(I)+FF*(ALT(I)-URALT(I))
      END DO
      BOING=.FALSE.
      DO I=1,3
        IF (NEU(I).LE.0.0D0) THEN
        NEU(I)=0.0D0
        BOING=.TRUE.
        END IF
      END DO
!.....
      IF (CODE1.EQ.1) THEN
      CH001=' '
      WRITE (UNIT=CH001,FMT=4010) (NEU(I),I=1,3)
 4010 FORMAT(' NEXT: ',3F9.5)
      END IF
!.....
      CALL INSIDE(NEU)
      IF (IN2.NE.0) THEN
!.....
      IF (CODE1.EQ.1) THEN
      WRITE (UNIT=CH001(40:),FMT=4012) IN2
 4012 FORMAT (' : INSIDE TWO-PHASE REGION',I3)
      WRITE (scr,FMT='(A80)') CH001
      WRITE (out,FMT='(A80)') CH001
      END IF
!.....
      IF (IN2.EQ.NTIE) THEN
      FF=0.0D0
      ELSE
      FF=FF/2.0D0
      END IF
      END IF
      IF (IN3.NE.0) THEN
!.....
      IF (CODE1.EQ.1) THEN
      WRITE (UNIT=CH001(40:),FMT=4014) EGGE(IN3),IN3
 4014 FORMAT(' : INSIDE',I3,'-PHASE REGION',I3)
      WRITE (scr,FMT='(A80)') CH001
      WRITE (out,FMT='(A80)') CH001
      END IF
!.....
      DO I=1,EGGE(IN3)
        DO II=1,3
          ZUG(I,II)=DREIX(IN3,I,II)
        END DO
      END DO
      I001=1
      I002=EGGE(IN3)
      CALL CLOSER(I001,I002,SUDIA1)
      I001=2
      CALL CLOSER(I001,I002,SUDIA2)
      SUDIA=TOLER/DMAX1(SUDIA1,SUDIA2,1D-5)
      IF (SUDIA.LT.0.9D0) THEN
!.....
      IF (CODE1.EQ.1) THEN
      WRITE (scr,4016)
      WRITE (out,4016)
 4016 FORMAT (' CLOSEST SIDE TOO FAR AWAY FROM LAST TIE-LINE')
      END IF
!.....
      FF=DMAX1(0.5D0,SUDIA)*FF
      ELSE
!.....
      IF (CODE1.EQ.1) THEN
      WRITE (scr,4018)
      WRITE (out,4018)
 4018 FORMAT (' CLOSEST SIDE TAKEN AS NEW TIE-LINE')
      END IF
!.....
      NRL=NRL+1
      LASTP(NTIE)=NRL
      FF=0.0D0
      END IF
      END IF
!-----
!     second if not inside
!
      IF (IN2.EQ.0.AND.IN3.EQ.0) THEN
!.....
      IF (CODE1.EQ.1) THEN
      WRITE (scr,FMT='(A80)') CH001
      WRITE (out,FMT='(A80)') CH001
      END IF
!.....
      CALL JANUS(NEU)
!-----
!DC   IF (NUNEW.GE.3) THEN
      IF (NUNEW.EQ.3) THEN
      DO I=1,NUNEW
        DO II=1,3
          ZUG(I,II)=XCORN(I+3,II)
        END DO
      END DO
      CALL NEWTRI(NUNEW)
      END IF
!----
      MORE=.FALSE.
      IF (NPHT.NE.NUN2) THEN
      MORE=.TRUE.
      GOTO 30
      END IF
      DO I=1,NUN2
        IF (TCHA(I).NE.ASNAM(I)) MORE=.TRUE.
!       IF (TPHA(I,1).NE.NUMMER(I)) MORE=.TRUE.
!       IF (TPHA(I,2).NE.EMCODE(I)) MORE=.TRUE.
        IF (MORE) GOTO 30
      END DO
   30 CONTINUE
      IF (MORE) THEN
!.....
      IF (CODE1.EQ.1) THEN
      WRITE (UNIT=6,FMT='('' DIFFERENT ASSEMBLAGE'')')
      WRITE (UNIT=out,FMT='('' DIFFERENT ASSEMBLAGE'')')
      END IF
!.....
      NUNEW=0
      FF=FF/2.0D0
      END IF
!-----
!     second if two phases
!
      IF (NUNEW.EQ.2) THEN
!.....
      IF (CODE1.EQ.1) THEN
      WRITE (scr,4020)
      WRITE (out,4020)
 4020 FORMAT(' TWO COEXISTING PHASES')
      END IF
!.....
      DO I=1,2
        DO II=1,3
          ZUG(I,II)=XCORN(I+3,II)
        END DO
      END DO
      I001=1
      I002=2
      CALL CLOSER(I001,I002,SUDIA1)
      I001=2
      CALL CLOSER(I001,I002,SUDIA2)
      SUDIA=TOLER/DMAX1(SUDIA1,SUDIA2,1D-5)
      IF (SUDIA.LT.0.9D0) THEN
!.....
      IF (CODE1.EQ.1) THEN
      WRITE (scr,4022) SUDIA
      WRITE (out,4022) SUDIA
 4022 FORMAT (' TOO FAR AWAY FROM LAST TIE-LINE:',F10.5)
      END IF
!.....
      FF=DMAX1(0.5D0,SUDIA)*FF
      ELSE
!.....
      IF (CODE1.EQ.1) THEN
      WRITE (scr,4024) SUDIA
      WRITE (out,4024) SUDIA
 4024 FORMAT(' NEW TIE-LINE IS O.K.:',F10.5)
      END IF
!.....
      NRL=NRL+1
      LASTP(NTIE)=NRL
      FF1=0.0D0
      DO I=1,3
        FF1=FF1+(TIEX(NRL,1,I)-TIEX(NRL,2,I))**2
        URALT(I)=ALT(I)
        ALT(I)=(TIEX(NRL,1,I)+TIEX(NRL,2,I))/2.0D0
      END DO
      FF=DMIN1(2.0D0,SUDIA)
      IF (BOING) FF=0.0D0
      FF1=DSQRT(FF1)/DSQRT(2.0D0)
      IF (FF1.LT.(TOLER/2.0D0)) FF=0.0D0
      END IF
      END IF
!
!     end second if two phases
!-----
      END IF
!
!     end second if not inside
!-----
      GOTO 700
!
!     end map solvus loop
!-----
  800 CONTINUE
      IF (IVOR.EQ.1) THEN
      DO K=1,2
        I001=0
        DO I=FIRSTP(NTIE),LASTP(NTIE)
          I001=I001+1
          DO II=1,3
            ZUG(I001,II)=TIEX(I,K,II)
          END DO
        END DO
        I002=FIRSTP(NTIE)-1
        DO I=I001,1,-1
          I002=I002+1
          DO II=1,3
            TIEX(I002,K,II)=ZUG(I,II)
          END DO
        END DO
      END DO
!.....
      IF (CODE1.EQ.1) THEN
      WRITE (scr,4030)
      WRITE (out,4030)
 4030 FORMAT(' SOLVUS INVERTED, TRY OTHER SIDE')
      END IF
!.....
      END IF
  610 CONTINUE
!
!     end invert solvus loop
!=====
!     one tieline group is finished
!=====
      I=NTIE
      I001=LASTP(I)-FIRSTP(I)+1
      WRITE (scr,FMT='('' TIELINE '',I4,'' / '',I5)') I,I001
      CALL FLUSH(6)
      WRITE (plt,3010)
 3010 FORMAT('TIELIN      1   ')
      DO II=FIRSTP(I),LASTP(I)
      DO K=1,2
!       WRITE (scr,3012) (TIEX(II,K,I001),I001=1,3)
!       WRITE (out,3012) (TIEX(II,K,I001),I001=1,3)
        WRITE (plt,3012) (TIEX(II,K,I001),I001=1,3)
 3012 FORMAT(3F12.6,'  ')
      END DO
      END DO
      WRITE (plt,3013)
 3013 FORMAT('  999 999 0   ')
!----
      IF (LPR3.LT.NDREI) THEN
      DO 636,I=LPR3+1,NDREI
      WRITE (scr,FMT='('' POLYGON '',I4)') I
      WRITE (plt,3014)
 3014 FORMAT('LINIEN    0  0  0  0  ')
      WRITE (plt,3016) (DREIX(I,EGGE(I),K),K=1,3)
      DO 637,II=1,EGGE(I)
!.....
      IF (CODE1.EQ.1) THEN
      WRITE (scr,4032) (DREIX(I,II,K),K=1,3)
      WRITE (out,4032) (DREIX(I,II,K),K=1,3)
 4032 FORMAT (' POLYGON: ',3F12.6)
      END IF
!.....
      WRITE (plt,3016) (DREIX(I,II,K),K=1,3)
 3016 FORMAT ('  ',3F12.6)
  637 CONTINUE
      WRITE (plt,3018)
 3018 FORMAT ('  999  999  0  ')
  636 CONTINUE
      LPR3=NDREI
      END IF
!++++
      END IF
!
!     end first if two phases
!=====
      END IF
!
!     end first if not inside
!=====
      IF (SCMODE.EQ.0) GOTO 100
  600 CONTINUE
  602 CONTINUE
!
!     end of mega-loop
!*********************
  666 CONTINUE
!-----
      IF (LPR3.LT.NDREI) THEN
      DO 638,I=LPR3+1,NDREI
      WRITE (scr,FMT='('' POLYGON '',I4)') I
      WRITE (plt,3020)
 3020 FORMAT('LINIEN    0  0  0  0  ')
      WRITE (plt,3022) (DREIX(I,EGGE(I),K),K=1,3)
 3022 FORMAT ('  ',3F12.6)
      DO 639,II=1,EGGE(I)
!.....
      IF (CODE1.EQ.1) THEN
      WRITE (scr,4040) (DREIX(I,II,K),K=1,3)
      WRITE (out,4040) (DREIX(I,II,K),K=1,3)
 4040 FORMAT (' POLYGON: ',3F12.6)
      END IF
!.....
      WRITE (plt,3022) (DREIX(I,II,K),K=1,3)
  639 CONTINUE
      WRITE (plt,3024)
 3024 FORMAT ('  999  999  0  ')
  638 CONTINUE
      END IF
!----
      DO 640,I=1,NAKKU
      IF (IAKKU(I).EQ.1) THEN
      WRITE (plt,3026) XAKKU(I,1),XAKKU(I,2),XAKKU(I,3)
 3026 FORMAT ('PUNKTE    -1   0.1  ',3(F10.6,2X),'  999  999  0')
      IF (XAKKU(I,2).LT.1D-3.AND.XAKKU(I,1).GT.1D-3) THEN
      WRITE (plt,3028) AKKU(I),XAKKU(I,1),XAKKU(I,2),XAKKU(I,3)
 3028 FORMAT ('TEXT    ',A20,2X,3F10.6,' 0.20  -0.5  -1  -0.5  0')
      GOTO 641
      END IF
      IF (XAKKU(I,3).LT.1D-3.AND.XAKKU(I,1).GT.1D-3) THEN
      WRITE (plt,3030) AKKU(I),XAKKU(I,1),XAKKU(I,2),XAKKU(I,3)
 3030 FORMAT ('TEXT    ',A20,2X,3F10.6,' 0.20  -0.5  -1  -1.0  30')
      GOTO 641
      END IF
      WRITE (plt,3032) AKKU(I),XAKKU(I,1),XAKKU(I,2),XAKKU(I,3)
 3032 FORMAT ('TEXT    ',A20,2X,3F10.6,' 0.20  0.5  0  -0.5  0')
      END IF
  641 CONTINUE
  640 CONTINUE
!----
!      WRITE (plt,3034)
! 3034 FORMAT ('FERTIG  ')
!-----
      DATI=sdate
      CALL CPUTIME(ZEITSTRING)
      CALL LABLA(DATI,J)
      DATI=DATI(1:J)//'  '//ZEITSTRING
      CALL LABLA(DATI,I001)
      WRITE (plt,3100)
 3100 FORMAT ('ACHSEN   0   10   10   0  10   0  10', &
      /,'NPLOIG'/// &
      '      0.00     10.00       0.0', &
      '      10.0   10.0000   10.0000  0  0')
      WRITE (plt,3102) NCOMIN+1
 3102 FORMAT (I5,'    0    0    0    0    0')
      WRITE (plt,3104) DATI(1:I001)
 3104 FORMAT ('     16.00     4.000 0.2000000    0.0000    0',A)
      F1=16.0D0
      F3=0.2D0
      F4=0.0D0
      ICH=0
      DO II=1,NCOMIN
      F2=15.0D0-(DBLE(II-1))*0.35D0
      WRITE (plt,3106) F1,F2,F3,F4,ICH,COMINS(II)
 3106 FORMAT (F10.2,F10.2,F10.7,F10.4,I5,A)
      END DO
      WRITE (plt,3034)
 3034 FORMAT (/,'FERTIG  ')
      WRITE (scr,3040) DATI(1:I001)
      WRITE (out,3040) DATI(1:I001)
 3040 FORMAT (/,'exit THERTER',/,A)
!-----
      END
!-----
!******************************
      SUBROUTINE NEWTRI(NNT)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      INCLUDE 'files.cmn'
      INTEGER(4) MAXTR,MAXTI,MAXLI,MAXZU,MAXSE
      PARAMETER (MAXTR=50,MAXTI=1000,MAXLI=30,MAXZU=400,MAXSE=200)
!=====
      INTEGER(4) CORNER(3),NUNEW,NUNORI,CHORIG(COMAX),NAKKU, &
      IAKKU(PHMAX),IN2,IN3,NTIE,NRL,NDREI,NSEED,EGGE(MAXTR), &
      FIRSTP(MAXLI),LASTP(MAXLI),NPHT,TPHA(COMAX,2),CODE1
      COMMON /JAIN/ CORNER,NUNEW,NUNORI,CHORIG,NAKKU,IAKKU,IN2,IN3, &
      NTIE,NRL,NDREI,NSEED,EGGE,FIRSTP,LASTP,NPHT,TPHA,CODE1
      REAL(8) XXECK(3,COMAX),XAKKU(PHMAX,3),ZUG(MAXZU,3), &
      DREIX(MAXTR,COMAX,3),TIEX(MAXTI,2,3),FFF(3),TOLER, &
      SEED(MAXSE,3),XCORN(COMAX+3,0:COMAX)
      COMMON /JARE/ XXECK,XAKKU,ZUG,DREIX,TIEX,FFF,TOLER,SEED,XCORN
      CHARACTER(20) ECKE(3),AKKU(PHMAX)
      CHARACTER(25) ASNAM(COMAX),TCHA(COMAX)
      COMMON /JACH/ ECKE,AKKU,ASNAM,TCHA
!-----END OF COMMON VARIABLES
      INTEGER(4) I,II,NNT,KK1,KK3
      REAL(8) A1,B1,C1,A2,B2,C2,A3,B3,C3,DD,FF,XFF(3)
!-----
      NDREI=NDREI+1
      EGGE(NDREI)=NNT
!.....
      IF (CODE1.EQ.1) THEN
      WRITE (scr,2000) NNT,NDREI
      WRITE (out,2000) NNT,NDREI
 2000 FORMAT(' NEW',I3,'-PHASE REGION, NR.:',I3)
      END IF
!.....
      DO 400,KK1=1,NNT-2
      KK3=KK1+2
  100 FF=ZUG(KK1,1)*(ZUG(KK1+1,2)-ZUG(KK3,2)) &
      +ZUG(KK1+1,1)*(ZUG(KK3,2)-ZUG(KK1,2)) &
      +ZUG(KK3,1)*(ZUG(KK1,2)-ZUG(KK1+1,2))
      IF (FF.GT.0.0D0) THEN
      KK3=KK3+1
      ELSE
      IF (FF.EQ.0.0D0) GOTO 101
      DO I=1,3
        XFF(I)=ZUG(KK1+1,I)
      END DO
      DO I=1,3
        ZUG(KK1+1,I)=ZUG(KK3,I)
      END DO
      DO I=1,3
        ZUG(KK3,I)=XFF(I)
      END DO
      KK3=KK1+2
      END IF
      IF (KK3.LE.NNT) GOTO 100
  101 CONTINUE
  400 CONTINUE
      DO I=1,NNT
        DO II=1,3
          DREIX(NDREI,I,II)=ZUG(I,II)
        END DO
      END DO
      DD=-0.001D0
      DO I=1,NNT
        II=I+1
        IF (II.EQ.NNT+1) II=1
        A1=ZUG(I,1)
        B1=ZUG(I,2)
        C1=ZUG(I,3)
        A2=ZUG(II,1)
        B2=ZUG(II,2)
        C2=ZUG(II,3)
        CALL SCHIFTE(A1,B1,C1,A2,B2,C2,DD,A3,B3,C3)
        NSEED=NSEED+1
        SEED(NSEED,1)=A3
        SEED(NSEED,2)=B3
        SEED(NSEED,3)=C3
      END DO
      RETURN
      END
!-----
!******************************
      SUBROUTINE NEWTIE
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      INCLUDE 'files.cmn'
      INTEGER(4) MAXTR,MAXTI,MAXLI,MAXZU,MAXSE
      PARAMETER (MAXTR=50,MAXTI=1000,MAXLI=30,MAXZU=400,MAXSE=200)
!=====
      INTEGER(4) CORNER(3),NUNEW,NUNORI,CHORIG(COMAX),NAKKU, &
      IAKKU(PHMAX),IN2,IN3,NTIE,NRL,NDREI,NSEED,EGGE(MAXTR), &
      FIRSTP(MAXLI),LASTP(MAXLI),NPHT,TPHA(COMAX,2),CODE1
      COMMON /JAIN/ CORNER,NUNEW,NUNORI,CHORIG,NAKKU,IAKKU,IN2,IN3, &
      NTIE,NRL,NDREI,NSEED,EGGE,FIRSTP,LASTP,NPHT,TPHA,CODE1
      REAL(8) XXECK(3,COMAX),XAKKU(PHMAX,3),ZUG(MAXZU,3), &
      DREIX(MAXTR,COMAX,3),TIEX(MAXTI,2,3),FFF(3),TOLER, &
      SEED(MAXSE,3),XCORN(COMAX+3,0:COMAX)
      COMMON /JARE/ XXECK,XAKKU,ZUG,DREIX,TIEX,FFF,TOLER,SEED,XCORN
      CHARACTER(20) ECKE(3),AKKU(PHMAX)
      CHARACTER(25) ASNAM(COMAX),TCHA(COMAX)
      COMMON /JACH/ ECKE,AKKU,ASNAM,TCHA
!-----END OF COMMON VARIABLES
      REAL(8) A1,B1,C1,A2,B2,C2,A3,B3,C3,DD
      INTEGER(4) I,II
!-----
      NTIE=NTIE+1
!.....
      IF (CODE1.EQ.1) THEN
      WRITE (scr,2000) NTIE
      WRITE (out,2000) NTIE
      END IF
 2000 FORMAT(' NEW TWO-PHASE REGION, NR.:',I3)
!.....
      NRL=NRL+1
      FIRSTP(NTIE)=NRL
      LASTP(NTIE)=NRL
      DO I=1,2
        DO II=1,3
          TIEX(NRL,I,II)=ZUG(I,II)
        END DO
      END DO
      A1=ZUG(1,1)
      B1=ZUG(1,2)
      C1=ZUG(1,3)
      A2=ZUG(2,1)
      B2=ZUG(2,2)
      C2=ZUG(2,3)
      DD=TOLER/2.0D0
      CALL SCHIFTE(A1,B1,C1,A2,B2,C2,DD,A3,B3,C3)
!.....
      IF (CODE1.EQ.1) THEN
      WRITE (scr,1005) A3,B3,C3
      WRITE (out,1005) A3,B3,C3
 1005 FORMAT (' POINT: ',3F10.5)
      END IF
!.....
      FFF(1)=(A1+A2)/2.0D0-A3
      FFF(2)=(B1+B2)/2.0D0-B3
      FFF(3)=(C1+C2)/2.0D0-C3
      RETURN
      END
!-----
!******************************
      SUBROUTINE INSIDE(STR)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      INCLUDE 'files.cmn'
      INTEGER(4) MAXTR,MAXTI,MAXLI,MAXZU,MAXSE
      PARAMETER (MAXTR=50,MAXTI=1000,MAXLI=30,MAXZU=400,MAXSE=200)
!=====
      INTEGER(4) CORNER(3),NUNEW,NUNORI,CHORIG(COMAX),NAKKU, &
      IAKKU(PHMAX),IN2,IN3,NTIE,NRL,NDREI,NSEED,EGGE(MAXTR), &
      FIRSTP(MAXLI),LASTP(MAXLI),NPHT,TPHA(COMAX,2),CODE1
      COMMON /JAIN/ CORNER,NUNEW,NUNORI,CHORIG,NAKKU,IAKKU,IN2,IN3, &
      NTIE,NRL,NDREI,NSEED,EGGE,FIRSTP,LASTP,NPHT,TPHA,CODE1
      REAL(8) XXECK(3,COMAX),XAKKU(PHMAX,3),ZUG(MAXZU,3), &
      DREIX(MAXTR,COMAX,3),TIEX(MAXTI,2,3),FFF(3),TOLER, &
      SEED(MAXSE,3),XCORN(COMAX+3,0:COMAX)
      COMMON /JARE/ XXECK,XAKKU,ZUG,DREIX,TIEX,FFF,TOLER,SEED,XCORN
      CHARACTER(20) ECKE(3),AKKU(PHMAX)
      CHARACTER(25) ASNAM(COMAX),TCHA(COMAX)
      COMMON /JACH/ ECKE,AKKU,ASNAM,TCHA
!-----END OF COMMON VARIABLES
!-----
      REAL(8) STR(3),SUM
!.....
      CHARACTER(500) CH001
!.....
      INTEGER(4) I,N,K,KK,COD,I001
      IN2=0
      IN3=0
      COD=0
      SUM=0.0D0
!.....
      CH001=' '
      WRITE (UNIT=CH001,FMT=4010) (STR(I),I=1,3)
 4010 FORMAT(' NEXT: ',3F9.5)
!.....
      DO I=1,3
        SUM=SUM+STR(I)
      END DO
      DO I=1,3
        STR(I)=STR(I)/SUM
      END DO
      DO 500,N=1,NTIE
      KK=0
      DO K=FIRSTP(N),LASTP(N)
        KK=KK+1
        DO I=1,3
          ZUG(KK,I)=TIEX(K,1,I)
        END DO
      END DO
      DO K=LASTP(N),FIRSTP(N),-1
        KK=KK+1
        DO I=1,3
          ZUG(KK,I)=TIEX(K,2,I)
        END DO
      END DO
      I001=KK
      CALL ISIN(I001,STR,COD)
      IF (COD.EQ.1) THEN
      IN2=N
      RETURN
      END IF
  500 CONTINUE
      DO 600,N=1,NDREI
      DO K=1,EGGE(N)
        DO I=1,3
          ZUG(K,I)=DREIX(N,K,I)
        END DO
      END DO
      I001=EGGE(N)
      CALL ISIN(I001,STR,COD)
      IF (COD.EQ.1) THEN
      IN3=N
      RETURN
      END IF
  600 CONTINUE
      RETURN
      END
!-----
!******************************
      SUBROUTINE ISIN(K,STR,COD)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      INCLUDE 'files.cmn'
      INTEGER(4) MAXTR,MAXTI,MAXLI,MAXZU,MAXSE
      PARAMETER (MAXTR=50,MAXTI=1000,MAXLI=30,MAXZU=400,MAXSE=200)
!=====
      INTEGER(4) CORNER(3),NUNEW,NUNORI,CHORIG(COMAX),NAKKU, &
      IAKKU(PHMAX),IN2,IN3,NTIE,NRL,NDREI,NSEED,EGGE(MAXTR), &
      FIRSTP(MAXLI),LASTP(MAXLI),NPHT,TPHA(COMAX,2),CODE1
      COMMON /JAIN/ CORNER,NUNEW,NUNORI,CHORIG,NAKKU,IAKKU,IN2,IN3, &
      NTIE,NRL,NDREI,NSEED,EGGE,FIRSTP,LASTP,NPHT,TPHA,CODE1
      REAL(8) XXECK(3,COMAX),XAKKU(PHMAX,3),ZUG(MAXZU,3), &
      DREIX(MAXTR,COMAX,3),TIEX(MAXTI,2,3),FFF(3),TOLER, &
      SEED(MAXSE,3),XCORN(COMAX+3,0:COMAX)
      COMMON /JARE/ XXECK,XAKKU,ZUG,DREIX,TIEX,FFF,TOLER,SEED,XCORN
      CHARACTER(20) ECKE(3),AKKU(PHMAX)
      CHARACTER(25) ASNAM(COMAX),TCHA(COMAX)
      COMMON /JACH/ ECKE,AKKU,ASNAM,TCHA
!-----END OF COMMON VARIABLES
      REAL(8) STR(3),SQ3,X0,Y0,X1,Y1,X2,Y2,YN,DYN
      INTEGER(4) K,COD,I
!-----
      SQ3=DSQRT(3.0D0)/2.0D0
      X0=(STR(3)+2.0D0*STR(2))/2.0D0
      Y0=SQ3*STR(3)
      X2=(ZUG(K,3)+2.0D0*ZUG(K,2))/2.0D0
      Y2=SQ3*ZUG(K,3)
      COD=0
      DO 500,I=1,K
      X1=X2
      Y1=Y2
      X2=(ZUG(I,3)+2.0D0*ZUG(I,2))/2.0D0
      Y2=SQ3*ZUG(I,3)
      IF ((X1.LE.X0.AND.X2.GT.X0).OR.(X2.LE.X0.AND.X1.GT.X0)) THEN
      YN=(Y2-Y1)*(X0-X1)/(X2-X1)+Y1
      DYN=Y0-YN
      IF (DYN.GT.0.0D0) COD=COD+1
      IF (DABS(DYN).LT.1D-5) THEN
      COD=1
      RETURN
      END IF
      END IF
  500 CONTINUE
      COD=MOD(COD,2)
      RETURN
      END
!-----
!******************************
      SUBROUTINE CLOSER(NPT,NZ,SOVMIN)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      INCLUDE 'files.cmn'
      INTEGER(4) MAXTR,MAXTI,MAXLI,MAXZU,MAXSE
      PARAMETER (MAXTR=50,MAXTI=1000,MAXLI=30,MAXZU=400,MAXSE=200)
!=====
      INTEGER(4) CORNER(3),NUNEW,NUNORI,CHORIG(COMAX),NAKKU, &
      IAKKU(PHMAX),IN2,IN3,NTIE,NRL,NDREI,NSEED,EGGE(MAXTR), &
      FIRSTP(MAXLI),LASTP(MAXLI),NPHT,TPHA(COMAX,2),CODE1
      COMMON /JAIN/ CORNER,NUNEW,NUNORI,CHORIG,NAKKU,IAKKU,IN2,IN3, &
      NTIE,NRL,NDREI,NSEED,EGGE,FIRSTP,LASTP,NPHT,TPHA,CODE1
      REAL(8) XXECK(3,COMAX),XAKKU(PHMAX,3),ZUG(MAXZU,3), &
      DREIX(MAXTR,COMAX,3),TIEX(MAXTI,2,3),FFF(3),TOLER, &
      SEED(MAXSE,3),XCORN(COMAX+3,0:COMAX)
      COMMON /JARE/ XXECK,XAKKU,ZUG,DREIX,TIEX,FFF,TOLER,SEED,XCORN
      CHARACTER(20) ECKE(3),AKKU(PHMAX)
      CHARACTER(25) ASNAM(COMAX),TCHA(COMAX)
      COMMON /JACH/ ECKE,AKKU,ASNAM,TCHA
!-----END OF COMMON VARIABLES
      REAL(8) SOVMIN,SOV(COMAX),XXT(3)
      INTEGER(4) NPT,NZ,I,II,IMIN
!-----
      IMIN=1
      DO II=1,3
        XXT(II)=TIEX(NRL,NPT,II)
      END DO
      DO 500,I=1,NZ
      SOV(I)=0.0D0
      DO II=1,3
        SOV(I)=SOV(I)+(XXT(II)-ZUG(I,II))**2
      END DO
      IF (I.EQ.1) THEN
      SOVMIN=SOV(1)
      IMIN=1
      ELSE
      IF (SOV(I).LT.SOVMIN) THEN
      SOVMIN=SOV(I)
      IMIN=I
      END IF
      END IF
  500 CONTINUE
      DO II=1,3
        TIEX(NRL+1,NPT,II)=ZUG(IMIN,II)
      END DO
      SOVMIN=DSQRT(SOVMIN)
      RETURN
      END
!-----
!******************************
      SUBROUTINE SCHIFTE(A1,B1,C1,A2,B2,C2,DD,A3,B3,C3)
      REAL(8) S,A1,B1,C1,A2,B2,C2,X1,X2,Y1,Y2,X3,Y3, &
      A3,B3,C3,DD,XM,YM,LL,AA1,BB1,CC1,AA2,BB2,CC2,C4
      C4=A1
      C4=A2
      S=2.0D0/DSQRT(3.0D0)
      X1=(S/2.0D0)*(C1+2.0D0*B1)
      Y1=C1
      X2=(S/2.0D0)*(C2+2.0D0*B2)
      Y2=C2
      XM=(X1+X2)/2.0D0
      YM=(Y1+Y2)/2.0D0
      LL=DSQRT((X2-X1)**2+(Y2-Y1)**2)
      AA1=Y1-YM
      BB1=XM-X1
      CC1=LL*DD/2.0D0+XM*Y1-X1*YM
      AA2=X1-X2
      BB2=Y1-Y2
      CC2=XM*(X1-X2)+YM*(Y1-Y2)
      X3=(CC2*BB1-CC1*BB2)/(AA2*BB1-AA1*BB2)
      Y3=(CC2*AA1-CC1*AA2)/(BB2*AA1-BB1*AA2)
      B3=X3/S-Y3/2.0D0
      C3=Y3
      A3=1.0D0-B3-C3
      RETURN
      END
!-----
!******************************
      SUBROUTINE JANUS(EMBULK)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      INCLUDE 'files.cmn'
      INTEGER(4) MAXTR,MAXTI,MAXLI,MAXZU,MAXSE
      PARAMETER (MAXTR=50,MAXTI=1000,MAXLI=30,MAXZU=400,MAXSE=200)
!=====
      INTEGER(4) CORNER(3),NUNEW,NUNORI,CHORIG(COMAX),NAKKU, &
      IAKKU(PHMAX),IN2,IN3,NTIE,NRL,NDREI,NSEED,EGGE(MAXTR), &
      FIRSTP(MAXLI),LASTP(MAXLI),NPHT,TPHA(COMAX,2),CODE1
      COMMON /JAIN/ CORNER,NUNEW,NUNORI,CHORIG,NAKKU,IAKKU,IN2,IN3, &
      NTIE,NRL,NDREI,NSEED,EGGE,FIRSTP,LASTP,NPHT,TPHA,CODE1
      REAL(8) XXECK(3,COMAX),XAKKU(PHMAX,3),ZUG(MAXZU,3), &
      DREIX(MAXTR,COMAX,3),TIEX(MAXTI,2,3),FFF(3),TOLER, &
      SEED(MAXSE,3),XCORN(COMAX+3,0:COMAX)
      COMMON /JARE/ XXECK,XAKKU,ZUG,DREIX,TIEX,FFF,TOLER,SEED,XCORN
      CHARACTER(20) ECKE(3),AKKU(PHMAX)
      CHARACTER(25) ASNAM(COMAX),TCHA(COMAX)
      COMMON /JACH/ ECKE,AKKU,ASNAM,TCHA
!-----END OF COMMON VARIABLES
      CHARACTER(64) U64,STAB(COMAX),STAA
      LOGICAL(4) MORE
      INTEGER(4) K,I,II,I001,IIX,I1,I2
      REAL(8) F,AR(COMAX),SUM,EMBULK(3)
!      REAL(8) TRACK(COMAX,COMAX)
!-----minimat variables
      REAL(8) AA(50,4)
      INTEGER(4) NCOL,NROW,RANG,ICOL,IROW,CONR(50),NPOSNB
!+++++
!.....
      DO I=1,64
        U64(I:I)='-'
      END DO
!.....
      NPOSNB=0
      DO I=1,3
      IF (EMBULK(I).NE.0.0D0) NPOSNB=NPOSNB+1
      END DO
!.....
      DO II=1,NC
        CHE(II)=EMBULK(1)*XXECK(1,II)+EMBULK(2)*XXECK(2,II) &
        +EMBULK(3)*XXECK(3,II)
        IF (DABS(CHE(II)).LT.1.0D-12) CHE(II)=0.0D0
      END DO
      MORE=.FALSE.
      DO I=1,NC
        IF (CHE(I).EQ.0.0D0.NEQV.CHEM(I).EQ.0.0D0) MORE=.TRUE.
        CHEM(I)=CHE(I)
      END DO
      IF (MORE) THEN
!.....
      IF (CODE1.EQ.1) THEN
      WRITE (scr,FMT='('' RE-READ DATABASE'')')
      WRITE (out,FMT='('' RE-READ DATABASE'')')
      END IF
!.....
      CALL DBREAD
!
!
      GOTO 737
! no check of phases necessary because PICK is active
!-----check phases
      NROW=NC
      NCOL=4
      RANG=0
      DO 900,I=NUN+1,NPHA
      DO ICOL=1,3
        DO IROW=1,NC
          AA(IROW,ICOL)=XXECK(ICOL,IROW)
        END DO
      END DO
      DO IROW=1,NC
        AA(IROW,4)=0.0D0
      END DO
      DO IROW=1,NUN
        AA(CHMCOD(IROW),4)=XX(I,IROW)
      END DO
      DO ICOL=1,4
        CONR(ICOL)=ICOL
      END DO
      CALL MINIMAT(AA,NCOL,NROW,CONR,RANG)
!.....
!---- check that i not = 0 !!!!
      IF (CODE1.EQ.1) THEN
      IF (NUMMER(I).GT.0) THEN
      STAA=NAME(NUMMER(I))
      ELSE
      STAA=SOLNAM(EMCODE(I))
      END IF
      WRITE (UNIT=6,FMT='(A16,2X,''rang:'',I3)') STAA,RANG
      WRITE (UNIT=out,FMT='(A16,2X,''rang:'',I3)') STAA,RANG
      END IF
!.....
      IF (RANG.GT.3) THEN
      NULL(I)=.TRUE.
      ELSE
      END IF
  900 CONTINUE
!-----end check phases
!      CALL NURVONPT
  737 CONTINUE
      ELSE
      DO I=1,NUN
        BULK(I)=CHE(CHMCOD(I))
      END DO
      END IF
!.....
      IF (CODE1.EQ.1) THEN
      PRTLOG(2)=.TRUE.
      PRTLOG(3)=.TRUE.
      PRTLOG(4)=.TRUE.
      PRTLOG(5)=.TRUE.
      PRTLOG(6)=.TRUE.
      WRITE (scr,FMT='('' CALL THERIAK'')')
      WRITE (out,FMT='('' CALL THERIAK'')')
      END IF
!.....
      CALL NURVONPT
      CALL CALSTR
!.....
      IF (CODE1.EQ.1) THEN
      CALL PRININ
      END IF
!.....
      CALL THERIA
!+++++
      DO I=1,3
        DO II=1,NUNORI
          XCORN(I,II)=XXECK(I,CHORIG(II))
        END DO
      END DO
      DO 410,K=1,NUN2
      DO I=1,NC
        CHE(I)=0.0D0
      END DO
      DO I=1,NUN
        CHE(CHMCOD(I))=X(K,I)
      END DO
      DO I=1,NUNORI
        XCORN(K+3,I)=CHE(CHORIG(I))
      END DO
  410 CONTINUE
!+++++
      I001=NUN2+3
      DO 600,K=1,3
      IIX=0
!-----
      DO II=K,NUNORI
        IF (XCORN(K,II).NE.0.0D0) IIX=II
      END DO
      IF (IIX.EQ.0.AND.EMBULK(K).NE.0.0D0) THEN
      CALL SHOUTF
      WRITE (scr,3000)
      WRITE (out,3000)
 3000 FORMAT (/' corners not linearly independent')
      STOP
      END IF
      IF (IIX.EQ.K) GOTO 100
      DO I=1,I001
        XCORN(I,0)=XCORN(I,K)
      END DO
      DO I=1,I001
        XCORN(I,K)=XCORN(I,IIX)
      END DO
      DO I=1,I001
        XCORN(I,IIX)=XCORN(I,0)
      END DO
  100 CONTINUE
      F=XCORN(K,K)
      IF (F.EQ.0.0D0) THEN
      DO II=1,I001
        XCORN(II,K)=0.0D0
      END DO
      ELSE
      DO I=1,I001
        IF (XCORN(I,K).NE.0.0D0) XCORN(I,K)=XCORN(I,K)/F
      END DO
      DO I=1,NUNORI
        AR(I)=XCORN(K,I)
      END DO
      DO I=1,NUNORI
        IF (I.NE.K.AND.AR(I).NE.0.0D0) THEN
        DO II=1,I001
          XCORN(II,I)=XCORN(II,I)-XCORN(II,K)*AR(I)
          IF (DABS(XCORN(II,I)).LT.1D-12) XCORN(II,I)=0.0D0
        END DO
        END IF
      END DO
      END IF
      DO I=1,NUNORI
        XCORN(K,I)=0.0D0
      END DO
      XCORN(K,K)=1.0D0
  600 CONTINUE
!+++++
      DO 500,I=1,NUN2
      IF (NUMMER(I).GT.0) THEN
      ASNAM(I)=ABK(NUMMER(I))
!     ASNAM(I)=NAME(NUMMER(I))
      I2=1
      ELSE
      ASNAM(I)=SOLNAM(EMCODE(I))
      I2=0
      END IF
      I1=0
      II=1
   10 IF (II.GT.NAKKU) GOTO 11
      IF (ASNAM(I).EQ.AKKU(II)) THEN
      I1=II
      GOTO 11
      ELSE
      II=II+1
      GOTO 10
      END IF
   11 IF (I1.EQ.0) THEN
      NAKKU=NAKKU+1
      AKKU(NAKKU)=ASNAM(I)(1:20)
      IAKKU(NAKKU)=I2
      XAKKU(NAKKU,1)=XCORN(I+3,1)
      XAKKU(NAKKU,2)=XCORN(I+3,2)
      XAKKU(NAKKU,3)=XCORN(I+3,3)
      END IF
  500 CONTINUE
!.....
!     DO 70,I=1,I001
!     WRITE (scr,5000) (XCORN(I,II),II=1,3),NAKKU,AKKU(NAKKU)
!     WRITE (out,5000) (XCORN(I,II),II=1,3),NAKKU,AKKU(NAKKU)
!5000 FORMAT (' XCORN ',3F12.6,2X,I3,2X,A20)
!  70 CONTINUE
!.....
      DO 660,II=4,I001
      SUM=0.0D0
      DO I=1,NUNORI
        SUM=SUM+XCORN(II,I)
      END DO
      IF (SUM.NE.0.0D0) THEN
      DO I=1,NUNORI
        XCORN(II,I)=XCORN(II,I)/SUM
      END DO
!     DO  I=1,NUN2
!       TRACK(II-3,I)=TRACK(II-3,I)/SUM
!     END DO
      END IF
  660 CONTINUE
      NUNEW=I001-3
!+++++
!.....
      IF (CODE1.EQ.1) THEN
      WRITE (scr,4000) U64,(ECKE(I),I=1,3)
      WRITE (out,4000) U64,(ECKE(I),I=1,3)
 4000 FORMAT ('  ',A64/1X,'|',8X,3(2X,A16),2X,'|'/1X,'|',64X,'|')
      DO I=1,NUNEW
        WRITE (scr,4002) I,(XCORN(I+3,II),II=1,3)
        WRITE (out,4002) I,(XCORN(I+3,II),II=1,3)
 4002 FORMAT (1X,'|   ',I2,')',2X,3(3X,F9.6,6X),2X,'|')
      END DO
      WRITE (scr,FMT='(2X,A64)') U64
      WRITE (out,FMT='(2X,A64)') U64
      DO I=1,NUN2
        IF (NUMMER(I).GT.0) THEN
        STAB(I)=NAME(NUMMER(I))
        ELSE
        STAB(I)=SOLNAM(EMCODE(I))
        END IF
      END DO
      WRITE (scr,4004) (STAB(I),I=1,NUN2)
      WRITE (out,4004) (STAB(I),I=1,NUN2)
 4004 FORMAT (' '/4X,16(1X,A7)/'  ')
      CALL PRTSTR(1,NMAX)
      WRITE (scr,FMT='(''  '')')
      WRITE (out,FMT='(''  '')')
      END IF
!.....
      RETURN
      END
!-----
!******************************
      SUBROUTINE MINIMAT(AA,NCOL,NROW,CONR,RANG)
      IMPLICIT NONE
!-----minimat variables
      REAL(8) AA(50,4)
      INTEGER(4) NCOL,NROW,RANG,CONR(50)
!----
      INTEGER(4) PIV
      REAL(8) COLSUM(4),WERT,DET
!----
      INTEGER(4) IR,IC,IIX,IIY,I,II,I2,IJ
      REAL(8) FF,F1,FFF(50)
      RANG=0
!.....
!     DO 900,IC=1,NCOL
!     WRITE (scr,1010) (AA(IR,IC),IR=1,NROW)
!     WRITE (out,1010) (AA(IR,IC),IR=1,NROW)
!1010 FORMAT(25F10.4)
! 900 CONTINUE
!.....
      DO 600,PIV=1,NROW
      DO IC=PIV,NCOL
        COLSUM(IC)=0.0D0
        DO IR=1,NROW
          COLSUM(IC)=COLSUM(IC)+DABS(AA(IR,IC))
        END DO
      END DO
      IIY=0
      IIX=0
      WERT=0.0D0
      DO IR=PIV,NROW
        DO IC=PIV,NCOL
          IF (COLSUM(IC).LE.0.0D0) GOTO 609
          FF=DABS(AA(IR,IC)/COLSUM(IC))
          IF (FF.GT.WERT) THEN
          WERT=FF
          IIX=IC
          IIY=IR
          END IF
  609     CONTINUE
        END DO
        IF (WERT.NE.0.0D0) GOTO 611
      END DO
  611 CONTINUE
      IF (IIX.EQ.0.OR.IIY.EQ.0) RETURN
      IF (IIX.NE.PIV) THEN
!----colchg
!     CALL COLCHG(IIX,PIV)
      IJ=CONR(IIX)
      DO I=1,NROW
        FFF(I)=AA(I,IIX)
      END DO
      CONR(IIX)=CONR(PIV)
      DO I=1,NROW
        AA(I,IIX)=AA(I,PIV)
      END DO
      CONR(PIV)=IJ
      DO I=1,NROW
        AA(I,PIV)=FFF(I)
      END DO
!----
      END IF
      IF (IIY.NE.PIV) THEN
!----rowchg
!     CALL ROWCHG(IIY,PIV)
      DO I=1,NCOL
        FFF(I)=AA(IIY,I)
      END DO
      DO I=1,NCOL
        AA(IIY,I)=AA(PIV,I)
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
      RETURN
      END
!-----

