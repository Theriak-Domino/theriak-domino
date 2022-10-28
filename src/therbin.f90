!-----Version: 28.05.2022
!               ***********
!               * THERBIN *
!               ***********
!
!     Calculates a binary phase diagram
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
!
!=====VARIABLES FOR BINARY PHASE DIAGRAM
!     total number of tielines: MAXTI
!     number of tieline-groups: MAXLI
!     number of tielines per group: MAXZU
!     number of seeds: MAXSE
      INTEGER(4) MAXTI,MAXLI,MAXZU,MAXSE
      PARAMETER (MAXTI=500,MAXLI=50,MAXZU=200,MAXSE=400)
!=====
      INTEGER(4) CORNER(3),NUNEW,NUNORI,CHORIG(COMAX),NAKKU,IAKKU(PHMAX), &
      IN2,NTIE,NRL,NSEED,FIRSTP(MAXLI),LASTP(MAXLI),NPHT(MAXLI),CODE1
      COMMON /JAIN/ CORNER,NUNEW,NUNORI,CHORIG,NAKKU,IAKKU,IN2,NTIE, &
      NRL,NSEED,FIRSTP,LASTP,NPHT,CODE1
      REAL(8) XXECK(3,COMAX),ZUG(MAXZU,3),TIEX(MAXTI,2,3),FFF(3),TOLER, &
      SEED(MAXSE,3),XCORN(0:COMAX+3,0:COMAX),YYMIN,YYMAX
      COMMON /JARE/ XXECK,ZUG,TIEX,FFF,TOLER,SEED,XCORN,YYMIN,YYMAX
      CHARACTER(20) ECKE(3),AKKU(PHMAX)
      CHARACTER(25) ASNAM(COMAX)
      COMMON /JACH/ ECKE,AKKU,ASNAM
!=====END OF COMMON VARIABLES
      LOGICAL(4) MORE
      INTEGER(4) I001,I002,I,I1,I2,II,COMAY,ierr,j,LARG
      REAL(8) FF,TPIN
      CHARACTER(8) SORTSTRING(PHMAX)
      CHARACTER(80) DATI,KOMMENTAR
      CHARACTER(500) CH001,CH002,SYREC,CHIN(4),ZEITSTRING
!-----
      INTEGER(4) NXSCAN,NYSCAN,K,IVOR,SCMODE,LL,NICHT0
      REAL(8) BSCAN,EMBULK(3),NEU(3),ALT(3),URALT(3),SUDIA1,SUDIA2, &
      FX,FF1,SUDIA,SUDIA3,SUDIA4,HSCAN,Y0999
      CHARACTER(25) XTXT,YTXT,CHPHT(MAXLI,COMAX)
      LOGICAL(4) BOING,STRICT(PHMAX+1)
!-----minimat variables
      REAL(8) AA(50,4),KOEFF(4),F1,F2,F3,F4
      INTEGER(4) NCOL,NROW,RANG,ICOL,IROW,CONR(50),ECKCODE,OUTCODE, &
      COPHASE(3),ICH
!*****
!CCCC      CALL CPUTIME(ZEITSTRING)
      progname='THERBIN'
      vers='28.05.2022'
      task='"Computation of binary phase diagrams"'
      ierr=0
      call initialize('$THERBIN-FILES',ierr)
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
      TPIN=0.0D0
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
      READ (UNIT=log,FMT='(F20.8)',END=111) TPIN
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
      CALL PUST (6,CH002)
      READ (IFNR,FMT='(A500)') CH001
      IF (CH001.EQ.' ') THEN
          CH001=CHIN(1)
          I001=I002
      else if (CH001.eq.'?') then
         call helpme('$THB-START')
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
!-----SET UP FIRSTP NUN COLUMNS OF MATRIX
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
!.....
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
      WRITE (scr,113)
  113 FORMAT (/ &
      '-----------'/ &
      'endmembers:'/ &
      '-----------')
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
      DO 570,I=1,2
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
         call helpme('$THB-ENDMEMB')
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
      WRITE (UNIT=scr,FMT=2004) NAM
 2004 FORMAT (/A16,' has no composition')
      GOTO 10
      END IF
      END IF
  570 CONTINUE
!-----
      WRITE (scr,114)
  114 FORMAT (/ &
      '-----------------'/ &
      'define Y-variable'/ &
      '-----------------')
   11 continue
      CALL LABLA(CHIN(4),I002)
      IF (I002.EQ.0) I002=1
      WRITE (scr,2001) CHIN(4)(1:I002)
2001  format(/'Enter [ "?" | CR | Y-variable  ymin  ymax ] <',a,'>?')
      READ (UNIT=IFNR,FMT='(A500)') CH001
      if (CH001.EQ.'?') THEN
         call helpme('$THB-YVAR')
         goto 11
      else if (CH001.EQ.' ') THEN
         CH001=CHIN(4)
      else
         CHIN(4)=CH001
      end if
!-----
      CALL TAXI(CH001,NAM)
      CALL LOWUP(NAM)
      IF (NAM.NE.'TC'.AND.NAM.NE.'TK'.AND.NAM.NE.'P') THEN
      CALL SHOUTI
      WRITE (scr,2010) NAM
 2010 FORMAT (/A16,': Y-variable not recognized')
      GOTO 11
      END IF
!-----
      ECKE(3)=NAM
      CALL GELI(CH001,YYMIN)
      CALL GELI(CH001,YYMAX)
      IF (YYMIN.EQ.YYMAX) THEN
      CALL SHOUTI
      WRITE (scr,2012)
 2012 FORMAT (/' ymin and ymax are the same')
      GOTO 11
      END IF
!-----
      IF (NAM.EQ.'TC'.OR.NAM.EQ.'TK') THEN
15    continue
      WRITE (scr,2014) 'pressure', TPIN
      READ (UNIT=IFNR,FMT='(A500)') CH001
      if (CH001.EQ.'?') THEN
         call helpme('$THB-PT')
         goto 15
      else if (CH001.EQ.' ') THEN
         P=TPIN
      else
         CALL GELI(CH001,P)
         TPIN=P
      end if
!-----
      IF (NAM.EQ.'TC') YTXT='Temperature [C]'
      IF (NAM.EQ.'TK') YTXT='Temperature [K]'
      WRITE (UNIT=CH001,FMT='(''P = '',F16.5,'' Bar'')') P
      END IF
      IF (NAM.EQ.'P') THEN
   17 CONTINUE
      WRITE (scr,2014) 'temperature', TC
 2014 FORMAT ('Enter [ "?" | CR | ',a,' ] <',F10.2,'>?')
      READ (UNIT=IFNR,FMT='(A500)') CH001
      if (CH001.EQ.'?') THEN
         call helpme('$THB-PT')
         goto 17
      else if (CH001.EQ.' ') THEN
         TC=TPIN
      else
         CALL GELI(CH001,TC)
         TPIN=TC
      end if
!-----
      T=TC+273.15D0
      YTXT='Pressure [Bar]'
      WRITE (UNIT=CH001,FMT='(''TC = '',F16.5,'' C'')') TC
      END IF
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
      WRITE (log,FMT='(F20.8)') TPIN
      CLOSE (UNIT=log)
!-----
      IF (YYMIN.GT.YYMAX) THEN
      FF=YYMIN
      YYMIN=YYMAX
      YYMAX=FF
      END IF
!
!***************************************************************
!******  END OF INPUT UND TESTS. START CALCULATING *************
!***************************************************************
!-----
      CALL LABLA(CHIN(1),I002)
      NCOMIN=3
      COMINS(1)=KOMMENTAR
      COMINS(2)='therbin version: '//vers(1:10)
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
      Y0999=(YYMAX-YYMIN)/1000.0D0
      CALL LABLA(ECKE(2),I1)
      XTXT='X('//ECKE(2)(1:I1)//')'
      WRITE (plt,3000) XTXT,YTXT,YYMIN,YYMAX
 3000 FORMAT ('NULLPT      5  3  '/'FAT   0.02 ', &
      /'PUNKTE   97   0.5   16.0  0.25  999  999', &
      /'PUNKTE   98   0.5   16.0  0.25  999  999', &
      /'PUNKTE   99   0.25   -0.2  15.2  999  999', &
      /'FAT   0.01 ', &
      'FONT     Helvetica  '/ &
      'AXIS     -4  -3   '/ &
      A25,'  15   0   1'/ &
      A25,'  15  ',F10.3,2X,F10.3)
      DO 572,I=1,2
      WRITE (plt,3004) ECKE(I),I-1,YYMAX
 3004 FORMAT ('TEXT   ',A20,2X,I3,2X,F12.3,'  0.5  0  -0.5  1.5  0')
  572 CONTINUE
      II=INDEX(CH001,'.')
      DO I=II+5,II,-1
        IF (CH001(I:I).EQ.'0') THEN
          CH001(I:I)=' '
        ELSE
          IF (CH001(I:I).EQ.'.') CH001(I:I)=' '
          GOTO 574
        END IF
      END DO
  574 CONTINUE
      CALL LABLA(CH001,II)
      LL=0
      DO I=1,II
        IF (CH001(I:I).NE.' '.OR.CH001(I+1:I+1).NE.' ') THEN
          LL=LL+1
          CH002(LL:LL)=CH001(I:I)
        END IF
      END DO
      WRITE (plt,3005) CH002(1:LL),YYMAX
 3005 FORMAT ('TEXT   ',A25,'  0.5  ',F12.3,'  0.5  0  -0.5  1.5  0')
!-----
   18 CONTINUE
      CH001='0'
      WRITE (scr,2020) CH001(1:1)
 2020 FORMAT (/,'Enter ["?" | CR | number of seeds ] <',a,'>?')
      READ (IFNR,FMT='(A500)') CH001
      if(CH001.eq.'?') then
         call helpme('$THB-SEED')
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
  579 CONTINUE
      if(I.ne.NSEED) NSEED=I-1
      END IF
!     WRITE (scr,2022) (ECKE(I),I=1,3)
! 2022 FORMAT (' enter the seed points. the variables are:'/3A20)
!      DO 578,I=1,NSEED
!      READ (IFNR,*) (SEED(I,II),II=1,3)
!  578 CONTINUE
!     END IF
!-----
!     WRITE (scr,2030)
!2030 FORMAT(' enter nx-, ny-scan and toler (if scan=0 then no scan)',
!    >'  default: 10   10   0.02')
!     READ (IFNR,FMT='(A500)') CH001
!
   19 CONTINUE
      CH001="10  10  0.02"
      WRITE (scr,2030) CH001(1:12)
 2030 FORMAT(/'Enter [ "?" | CR | X-scan-density', &
      ' Y-scan-density  tolerance ] <',a,'>?')
      READ (IFNR,FMT='(A500)') CH001
      if(CH001.eq.'?') then
         call helpme('$THB-SCAN')
         goto 19
      end if
!=====
      CALL GELI(CH001,FF)
      IF (FF.LE.0.0D0) FF=10.0D0
      NXSCAN=IDINT(FF)
      IF (NXSCAN.LE.0) THEN
      BSCAN=1.0D0
      ELSE
      BSCAN=1.0D0/FF
      END IF
      CALL GELI(CH001,FF)
      IF (FF.LE.0.0D0) FF=10.0D0
      NYSCAN=IDINT(FF)
      HSCAN=(YYMAX-YYMIN)/FF
      CALL GELI(CH001,TOLER)
      IF (TOLER.LE.0.0D0) TOLER=0.02D0
      NTIE=0
!-----check phases
      NROW=NC
      NCOL=3
      RANG=0
      DO 900,I=NUN+1,NPHA
      DO ICOL=1,2
        DO IROW=1,NC
          AA(IROW,ICOL)=XXECK(ICOL,IROW)
        END DO
      END DO
      DO IROW=1,NC
        AA(IROW,3)=0.0D0
      END DO
      DO IROW=1,NUN
        AA(CHMCOD(IROW),3)=XX(I,IROW)
      END DO
      DO ICOL=1,3
        CONR(ICOL)=ICOL
      END DO
      CALL MINIMAT(AA,NCOL,NROW,CONR,RANG)
!.....
      IF (CODE1.EQ.1) THEN
      WRITE (UNIT=scr,FMT='(A16,2X,I3)') NAME(NUMMER(I)),RANG
      WRITE (UNIT=out,FMT='(A16,2X,I3)') NAME(NUMMER(I)),RANG
      END IF
!.....
      IF (RANG.GT.2) THEN
!.....
      WRITE (scr,2050) NAME(NUMMER(I))
      WRITE (out,2050) NAME(NUMMER(I))
 2050 FORMAT (A16,6X, &
      ' -------> phase excluded (outside binary system)')
!.....
!      NULL(I)=.TRUE.
      STRICT(I)=.FALSE.
      ELSE
      STRICT(I)=.TRUE.
!.....
      IF (CODE1.EQ.1) THEN
      WRITE (scr,2055) NAME(NUMMER(I)),CONR(3),AA(1,3),CONR(1), &
      AA(2,3),CONR(2),AA(3,3),CONR(3)
      WRITE (out,2055) NAME(NUMMER(I)),CONR(3),AA(1,3),CONR(1), &
      AA(2,3),CONR(2),AA(3,3),CONR(3)
 2055 FORMAT(A16,2X,':  ',I2,'=',3(F10.4,'*',I2))
      END IF
!.....the following only after the first dbread
      DO IROW=1,2
        KOEFF(CONR(IROW))=-AA(IROW,3)
      END DO
      KOEFF(CONR(3))=1.0D0
      OUTCODE=0
      ECKCODE=0
      DO IROW=1,2
        IF (KOEFF(IROW)*KOEFF(3).GT.0.0D0) OUTCODE=1
        IF (KOEFF(IROW).EQ.0.0D0) ECKCODE=ECKCODE+1
      END DO
      IF (OUTCODE.EQ.1) THEN
      WRITE (scr,2057) NAME(NUMMER(I))
      WRITE (out,2057) NAME(NUMMER(I))
 2057 FORMAT (A16,6X,': WARNING - phase outside binary section')
!ccc muss unbeding ausgeschlossen werden. (koeff wird sonst -1d+15 etc...)
      STRICT(I)=.FALSE.
      END IF
      IF (ECKCODE.EQ.1) THEN
      DO IROW=1,2
        IF (KOEFF(IROW)*KOEFF(3).LT.0.0D0) THEN
        COPHASE(IROW)=COPHASE(IROW)+1
        GOTO 951
        END IF
      END DO
  951 CONTINUE
      END IF
!-----end of else
      END IF
  900 CONTINUE
      DO IROW=1,2
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
!*****
!     start mega-loop
!
      DO 602,I1=0,NXSCAN
      DO 600,I2=0,NYSCAN
  100 IF (NSEED.GT.0) THEN
      SCMODE=0
      EMBULK(1)=DMAX1(SEED(NSEED,1),0.0D0)
      EMBULK(2)=DMAX1(SEED(NSEED,2),0.0D0)
      EMBULK(3)=SEED(NSEED,3)
      IF (EMBULK(3).LT.YYMIN) EMBULK(3)=YYMIN
      IF (EMBULK(3).GT.YYMAX) EMBULK(3)=YYMAX
      NSEED=NSEED-1
      ELSE
      SCMODE=1
      EMBULK(1)=DBLE(I1)*BSCAN
      EMBULK(2)=DBLE(NXSCAN-I1)*BSCAN
      EMBULK(3)=YYMIN+DBLE(I2)*HSCAN
      END IF
!-----
!.....
      IF (CODE1.EQ.1) THEN
      CH001=' '
      IF (SCMODE.EQ.0) THEN
      WRITE (UNIT=CH001,FMT='('' SEED: '')')
      ELSE
      WRITE (UNIT=CH001,FMT='('' SCAN: '')')
      END IF
      WRITE (UNIT=CH001(8:),FMT=4000) (EMBULK(I),I=1,3)
 4000 FORMAT(3F12.5)
      END IF
!.....
      CALL INSIDE(EMBULK)
!.....
      IF (CODE1.EQ.1) THEN
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
      IF (IN2.EQ.0) THEN
!.....
      IF (CODE1.EQ.1) THEN
      WRITE (scr,FMT='(A80)') CH001
      WRITE (out,FMT='(A80)') CH001
      END IF
!.....
      CALL JANUS(EMBULK)
!+++++
      IF (CODE1.EQ.1) THEN
      DO I=4,I001
        WRITE (scr,4008) (XCORN(I,II),II=1,3)
 4008 FORMAT (' XCORN ',3F12.6)
      END DO
      END IF
!+++++
!-----
!     first if two phases
!
      IF (NUNEW.EQ.2) THEN
      DO I=1,2
        ZUG(I,3)=EMBULK(3)
        DO II=1,2
          ZUG(I,II)=XCORN(I+2,II)
        END DO
      END DO
      CALL NEWTIE
      NPHT(NTIE)=NUN2
      DO I=1,NUN2
        CHPHT(NTIE,I)=ASNAM(I)
      END DO
!=====
!     invert solvus loop
!
      DO 610,IVOR=1,-1,-2
      DO I=1,3
      ALT(I)=(TIEX(NRL,1,I)+TIEX(NRL,2,I))/2.0D0
        URALT(I)=ALT(I)-DBLE(IVOR)*FFF(I)
      END DO
      FF=1.0D0
!=====
!     map solvus loop
!
  700 IF (FF.LT.0.001) GOTO 800
      DO I=1,3
        NEU(I)=ALT(I)+FF*(ALT(I)-URALT(I))
      END DO
!+++++
      IF (CODE1.EQ.1) THEN
      WRITE (scr,FMT='(''URALT '',3F12.5)') (URALT(I),I=1,3)
      WRITE (scr,FMT='(''ALT '',3F12.5)') (ALT(I),I=1,3)
      WRITE (scr,FMT='(''NEU '',3F12.5)') (NEU(I),I=1,3)
      WRITE (scr,FMT='(''FF='',F12.5)') FF
      END IF
!+++++
      BOING=.FALSE.
      DO I=1,2
        IF (NEU(I).LT.0.0D0) THEN
        NEU(I)=0.0D0
        END IF
      END DO
      IF (NEU(3).LT.YYMIN) THEN
      NEU(3)=YYMIN
      BOING=.TRUE.
      END IF
      IF (NEU(3).GT.YYMAX) THEN
      NEU(3)=YYMAX
      BOING=.TRUE.
      END IF
!.....
      IF (CODE1.EQ.1) THEN
      CH001=' '
      WRITE (UNIT=CH001,FMT=4010) (NEU(I),I=1,3)
 4010 FORMAT(' NEXT: ',3F12.5)
      END IF
!.....
      CALL INSIDE(NEU)
      IF (IN2.NE.0) THEN
!.....
      IF (CODE1.EQ.1) THEN
      WRITE (UNIT=CH001(45:),FMT=4012) IN2
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
!-----
!     second if not inside
!
      IF (IN2.EQ.0) THEN
!.....
      IF (CODE1.EQ.1) THEN
      WRITE (scr,FMT='(A80)') CH001
      WRITE (out,FMT='(A80)') CH001
      END IF
!.....
      CALL JANUS(NEU)
      MORE=.FALSE.
      IF (NPHT(NTIE).NE.NUN2) THEN
      MORE=.TRUE.
      GOTO 30
      END IF
      MORE=.TRUE.
      IF (ASNAM(1).EQ.CHPHT(NTIE,1).AND. &
      ASNAM(2).EQ.CHPHT(NTIE,2)) MORE=.FALSE.
      IF (ASNAM(1).EQ.CHPHT(NTIE,2).AND. &
      ASNAM(2).EQ.CHPHT(NTIE,1)) MORE=.FALSE.
   30 CONTINUE
      IF (MORE) THEN
!.....
      IF (CODE1.EQ.1) THEN
      WRITE (UNIT=scr,FMT='('' DIFFERENT ASSEMBLAGE'')')
      WRITE (UNIT=out,FMT='('' DIFFERENT ASSEMBLAGE'')')
      END IF
!.....
      NUNEW=0
      FF=FF*0.3D0
      IF (DABS(NEU(3)-ALT(3)).LT.(Y0999/2.0D0)) FF=0.0D0
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
        ZUG(I,3)=NEU(3)
        DO II=1,2
          ZUG(I,II)=XCORN(I+2,II)
        END DO
      END DO
      CALL CLOSER(SUDIA1,SUDIA2,SUDIA3,SUDIA4)
!-----
      IF (SUDIA4.NE.0.0D0) THEN
      IF (SUDIA3.LT.Y0999) THEN
      SUDIA=4.0D0
      ELSE
      SUDIA=TOLER/SUDIA4
      END IF
      ELSE
      SUDIA=3.0D0
      END IF
!-----
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
      DO I=1,3
        URALT(I)=ALT(I)
        ALT(I)=(TIEX(NRL,1,I)+TIEX(NRL,2,I))/2.0D0
      END DO
      FF=DMIN1(2.0D0,SUDIA)
      IF (BOING) FF=0.0D0
!     IF (DABS(URALT(3)-ALT(3)).LT.Y0999) FF=0.0D0
      FF1=DABS(TIEX(NRL,1,1)-TIEX(NRL,2,1))
      IF (FF1.LT.0.001D0) FF=0.0D0
      END IF
      END IF
!----------- end second if two phases
      END IF
!----------- end second if not inside
      GOTO 700
!
!     end map solvus loop
!=====
  800 CONTINUE
      IF (IVOR.EQ.1) THEN
      DO 634,K=1,2
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
  634 CONTINUE
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
      WRITE (plt,3010)
 3010 FORMAT('TIELIN      0   ')
      I001=LASTP(I)-FIRSTP(I)+1
      WRITE (scr,3012) I,I001
 3012 FORMAT (' TIELINE ',I4,' :',I5,' LINES')
      CALL FLUSH(6)
!.....
!      DO 636,II=FIRSTP(I),LASTP(I)
!      DO 636,K=1,2
!.....
!      WRITE (scr,3016) (TIEX(II,K,I001),I001=2,3)
!      WRITE (out,3016) (TIEX(II,K,I001),I001=2,3)
!!.....
!      WRITE (plt,3016) (TIEX(II,K,I001),I001=2,3)
! 3016 FORMAT(3(F12.6,2X))
!  636 CONTINUE
!.....
      WRITE (plt,3016) (((TIEX(II,K,I001),I001=2,3),K=1,2), &
      II=FIRSTP(I),LASTP(I))
 3016 FORMAT(5(F12.6,2X,F12.6))
      WRITE (plt,3018)
 3018 FORMAT('  999  999  0   ')
      CALL SEEDER
!=====
      END IF
!----------- end first if two phases
      END IF
!----------- end first if not inside
      IF (SCMODE.EQ.0) GOTO 100
  600 CONTINUE
  602 CONTINUE
!
!     end of mega-loop
!*****
      DO 640,I=1,NAKKU
      IF (IAKKU(I).EQ.1) THEN
      FF=2.0D0*YYMAX
      DO 642,II=1,NTIE
      DO 644,I2=1,NPHT(II)
      IF (CHPHT(II,I2).EQ.AKKU(I)) THEN
      IF (TIEX(FIRSTP(II),1,3).LT.FF) THEN
      FF=TIEX(FIRSTP(II),1,3)
      IF (I2.EQ.1) THEN
      FX=TIEX(FIRSTP(II),1,2)
      ELSE
      FX=TIEX(FIRSTP(II),2,2)
      END IF
      END IF
      IF (TIEX(LASTP(II),1,3).LT.FF) THEN
      FF=TIEX(LASTP(II),1,3)
      IF (I2.EQ.1) THEN
      FX=TIEX(LASTP(II),1,2)
      ELSE
      FX=TIEX(LASTP(II),2,2)
      END IF
      END IF
      END IF
  644 CONTINUE
  642 CONTINUE
      IF (FF.LE.YYMAX) THEN
      WRITE (plt,3020) FX,FF
 3020 FORMAT ('PUNKTE    -1   0.1  ',F8.5,1X,F8.2,'  999  999  ')
      IF (FX.LT.0.999D0) THEN
      WRITE (plt,3022) AKKU(I)(1:8),FX,FF
 3022 FORMAT ('TEXT    ',A8,2X,F8.5,1X,F8.2,' 0.20  0.5  0  0.5  0')
      ELSE
      WRITE (plt,3024) AKKU(I)(1:8),FX,FF
 3024 FORMAT ('TEXT    ',A8,2X,F8.5,1X,F8.2,' 0.20  -0.5  -1  0.5  0')
      END IF
      END IF
      END IF
  640 CONTINUE
!*****
!       WRITE (plt,3026)
!  3026 FORMAT ('FERTIG  ')
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
 3104 FORMAT ('     15.00    -2.000 0.2000000    0.0000    0',A)
      F1=16.5D0
      F3=0.2D0
      F4=0.0D0
      ICH=0
      DO II=1,NCOMIN
      F2=15.0D0-(DBLE(II-1))*0.35D0
      WRITE (plt,3106) F1,F2,F3,F4,ICH,COMINS(II)
 3106 FORMAT (F10.2,F10.2,F10.7,F10.4,I5,A)
      END DO
      WRITE (plt,3026)
 3026 FORMAT (/'FERTIG  ')
      WRITE (scr,3030) DATI(1:I001)
      WRITE (out,3030) DATI(1:I001)
 3030 FORMAT (/,'exit THERBIN',/,A)
!-----
      END
!-----
!******************************
      SUBROUTINE NEWTIE
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!=====VARIABLES FOR BINARY PHASE DIAGRAM
      INTEGER(4) MAXTI,MAXLI,MAXZU,MAXSE
      PARAMETER (MAXTI=500,MAXLI=50,MAXZU=200,MAXSE=400)
!=====
      INTEGER(4) CORNER(3),NUNEW,NUNORI,CHORIG(COMAX),NAKKU,IAKKU(PHMAX), &
      IN2,NTIE,NRL,NSEED,FIRSTP(MAXLI),LASTP(MAXLI),NPHT(MAXLI),CODE1
      COMMON /JAIN/ CORNER,NUNEW,NUNORI,CHORIG,NAKKU,IAKKU,IN2,NTIE, &
      NRL,NSEED,FIRSTP,LASTP,NPHT,CODE1
      REAL(8) XXECK(3,COMAX),ZUG(MAXZU,3),TIEX(MAXTI,2,3),FFF(3),TOLER, &
      SEED(MAXSE,3),XCORN(0:COMAX+3,0:COMAX),YYMIN,YYMAX
      COMMON /JARE/ XXECK,ZUG,TIEX,FFF,TOLER,SEED,XCORN,YYMIN,YYMAX
      CHARACTER(20) ECKE(3),AKKU(PHMAX)
      CHARACTER(25) ASNAM(COMAX)
      COMMON /JACH/ ECKE,AKKU,ASNAM
!=====END OF COMMON VARIABLES
!-----
      INTEGER(4) I,II
!-----
      NTIE=NTIE+1
!.....
      IF (CODE1.EQ.1) THEN
      WRITE (scr,2000) NTIE
      WRITE (out,2000) NTIE
 2000 FORMAT(' NEW TWO-PHASE REGION, NR.:',I3)
      END IF
!.....
      NRL=NRL+1
      FIRSTP(NTIE)=NRL
      LASTP(NTIE)=NRL
      IF (ZUG(1,2).GT.ZUG(2,2)) THEN
      ASNAM(3)=ASNAM(1)
      DO II=1,3
        ZUG(3,II)=ZUG(1,II)
      END DO
      ASNAM(1)=ASNAM(2)
      DO II=1,3
        ZUG(1,II)=ZUG(2,II)
      END DO
      ASNAM(2)=ASNAM(3)
      DO II=1,3
        ZUG(2,II)=ZUG(3,II)
      END DO
      END IF
      DO I=1,2
        DO II=1,3
          TIEX(NRL,I,II)=ZUG(I,II)
        END DO
      END DO
      FFF(1)=0.0D0
      FFF(2)=0.0D0
      FFF(3)=TOLER/4.0D0*(YYMAX-YYMIN)
      RETURN
      END
!-----
!******************************
      SUBROUTINE SEEDER
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!=====VARIABLES FOR BINARY PHASE DIAGRAM
      INTEGER(4) MAXTI,MAXLI,MAXZU,MAXSE
      PARAMETER (MAXTI=500,MAXLI=50,MAXZU=200,MAXSE=400)
!=====
      INTEGER(4) CORNER(3),NUNEW,NUNORI,CHORIG(COMAX),NAKKU,IAKKU(PHMAX), &
      IN2,NTIE,NRL,NSEED,FIRSTP(MAXLI),LASTP(MAXLI),NPHT(MAXLI),CODE1
      COMMON /JAIN/ CORNER,NUNEW,NUNORI,CHORIG,NAKKU,IAKKU,IN2,NTIE, &
      NRL,NSEED,FIRSTP,LASTP,NPHT,CODE1
      REAL(8) XXECK(3,COMAX),ZUG(MAXZU,3),TIEX(MAXTI,2,3),FFF(3),TOLER, &
      SEED(MAXSE,3),XCORN(0:COMAX+3,0:COMAX),YYMIN,YYMAX
      COMMON /JARE/ XXECK,ZUG,TIEX,FFF,TOLER,SEED,XCORN,YYMIN,YYMAX
      CHARACTER(20) ECKE(3),AKKU(PHMAX)
      CHARACTER(25) ASNAM(COMAX)
      COMMON /JACH/ ECKE,AKKU,ASNAM
!=====END OF COMMON VARIABLES
!-----
      INTEGER(4) K,NP,I,II,NNP,I001
      REAL(8) SCHAU(3)
!-----
      NNP=LASTP(NTIE)-FIRSTP(NTIE)
      IF (NNP.LE.0) NNP=1
      DO 500,K=FIRSTP(NTIE),LASTP(NTIE),NNP
      IF (DABS(TIEX(K,1,1)-TIEX(K,2,1)).GT.0.001D0) THEN
      DO 514,NP=1,2
      DO 512,I=1,-1,-2
      DO 510,II=1,-1,-2
      SEED(NSEED+1,1)=TIEX(K,NP,1)+DBLE(I)*0.001D0
      IF (SEED(NSEED+1,1).LT.0.0D0) SEED(NSEED+1,1)=0.0D0
      SEED(NSEED+1,2)=TIEX(K,NP,2)-DBLE(I)*0.001D0
      IF (SEED(NSEED+1,2).LT.0.0D0) SEED(NSEED+1,2)=0.0D0
      SEED(NSEED+1,3)=TIEX(K,NP,3)+DBLE(II)*(YYMAX-YYMIN)/400.0D0
      IF (SEED(NSEED+1,3).LT.YYMIN) SEED(NSEED+1,3)=YYMIN
      IF (SEED(NSEED+1,3).GT.YYMAX) SEED(NSEED+1,3)=YYMAX
      DO I001=1,3
        SCHAU(I001)=SEED(NSEED+1,I001)
      END DO
      CALL INSIDE(SCHAU)
      IF (IN2.EQ.0) THEN
      NSEED=NSEED+1
!.....
      IF (CODE1.EQ.1) THEN
      WRITE (scr,FMT='('' NSEED='',I5)') NSEED
      WRITE (out,FMT='('' NSEED='',I5)') NSEED
      END IF
!.....
      END IF
  510 CONTINUE
  512 CONTINUE
  514 CONTINUE
      END IF
  500 CONTINUE
      RETURN
      END
!-----
!******************************
      SUBROUTINE INSIDE(STR)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
!=====VARIABLES FOR BINARY PHASE DIAGRAM
      INTEGER(4) MAXTI,MAXLI,MAXZU,MAXSE
      PARAMETER (MAXTI=500,MAXLI=50,MAXZU=200,MAXSE=400)
!=====
      INTEGER(4) CORNER(3),NUNEW,NUNORI,CHORIG(COMAX),NAKKU,IAKKU(PHMAX), &
      IN2,NTIE,NRL,NSEED,FIRSTP(MAXLI),LASTP(MAXLI),NPHT(MAXLI),CODE1
      COMMON /JAIN/ CORNER,NUNEW,NUNORI,CHORIG,NAKKU,IAKKU,IN2,NTIE, &
      NRL,NSEED,FIRSTP,LASTP,NPHT,CODE1
      REAL(8) XXECK(3,COMAX),ZUG(MAXZU,3),TIEX(MAXTI,2,3),FFF(3),TOLER, &
      SEED(MAXSE,3),XCORN(0:COMAX+3,0:COMAX),YYMIN,YYMAX
      COMMON /JARE/ XXECK,ZUG,TIEX,FFF,TOLER,SEED,XCORN,YYMIN,YYMAX
      CHARACTER(20) ECKE(3),AKKU(PHMAX)
      CHARACTER(25) ASNAM(COMAX)
      COMMON /JACH/ ECKE,AKKU,ASNAM
!=====END OF COMMON VARIABLES
!-----
      REAL(8) STR(3),SUM,DIFF1,DIFF2,DIFF3,DIFF4,DIFF5,DIFF6, &
      PROD1,PROD2,PROD3
      INTEGER(4) I,N,K
!-----
      IN2=0
      SUM=0.0D0
      DO I=1,2
        SUM=SUM+STR(I)
      END DO
      DO I=1,2
        STR(I)=STR(I)/SUM
      END DO
      DO 500,N=1,NTIE
      DIFF1=STR(1)-TIEX(FIRSTP(N),1,1)
      DIFF2=STR(1)-TIEX(FIRSTP(N),2,1)
      DIFF5=DABS(STR(3)-TIEX(FIRSTP(N),1,3))
      PROD1=DIFF1*DIFF2
      IF (PROD1.LT.1D-5.AND.DIFF5.LE.1D-2) THEN
      IN2=N
      RETURN
      END IF
!-----
      DO 502,K=FIRSTP(N)+1,LASTP(N)
      DIFF3=STR(1)-TIEX(K,1,1)
      DIFF4=STR(1)-TIEX(K,2,1)
      PROD2=DIFF3*DIFF4
      DIFF5=STR(3)-TIEX(K-1,1,3)
      DIFF6=STR(3)-TIEX(K,1,3)
      PROD3=DIFF5*DIFF6
      IF ((PROD1.LE.1D-5.OR.PROD2.LE.1D-5).AND.PROD3.LE.1D-2) THEN
      IN2=N
      RETURN
      END IF
      PROD1=PROD2
  502 CONTINUE
  500 CONTINUE
      RETURN
      END
!-----
!******************************
      SUBROUTINE CLOSER(SOV1,SOV2,SOV3,SOV4)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
!=====VARIABLES FOR BINARY PHASE DIAGRAM
      INTEGER(4) MAXTI,MAXLI,MAXZU,MAXSE
      PARAMETER (MAXTI=500,MAXLI=50,MAXZU=200,MAXSE=400)
!=====
      INTEGER(4) CORNER(3),NUNEW,NUNORI,CHORIG(COMAX),NAKKU,IAKKU(PHMAX), &
      IN2,NTIE,NRL,NSEED,FIRSTP(MAXLI),LASTP(MAXLI),NPHT(MAXLI),CODE1
      COMMON /JAIN/ CORNER,NUNEW,NUNORI,CHORIG,NAKKU,IAKKU,IN2,NTIE, &
      NRL,NSEED,FIRSTP,LASTP,NPHT,CODE1
      REAL(8) XXECK(3,COMAX),ZUG(MAXZU,3),TIEX(MAXTI,2,3),FFF(3),TOLER, &
      SEED(MAXSE,3),XCORN(0:COMAX+3,0:COMAX),YYMIN,YYMAX
      COMMON /JARE/ XXECK,ZUG,TIEX,FFF,TOLER,SEED,XCORN,YYMIN,YYMAX
      CHARACTER(20) ECKE(3),AKKU(PHMAX)
      CHARACTER(25) ASNAM(COMAX)
      COMMON /JACH/ ECKE,AKKU,ASNAM
!=====END OF COMMON VARIABLES
!-----
      REAL(8) SOV1,SOV2,SOV3,SOV4
      INTEGER(4) II
!-----
      IF (ZUG(1,2).GT.ZUG(2,2)) THEN
      DO II=1,3
        ZUG(3,II)=ZUG(1,II)
      END DO
      DO II=1,3
        ZUG(1,II)=ZUG(2,II)
      END DO
      DO II=1,3
        ZUG(2,II)=ZUG(3,II)
      END DO
      END IF
!-----
      SOV1=DABS(TIEX(NRL,1,1)-ZUG(1,1))
      SOV2=DABS(TIEX(NRL,2,1)-ZUG(2,1))
      SOV3=DABS(TIEX(NRL,1,3)-ZUG(1,3))
      SOV4=DABS(TIEX(NRL,1,1)-TIEX(NRL,2,1)-ZUG(1,1)+ZUG(2,1))
      DO II=1,3
        TIEX(NRL+1,1,II)=ZUG(1,II)
        TIEX(NRL+1,2,II)=ZUG(2,II)
      END DO
      RETURN
      END
!-----
!******************************
      SUBROUTINE JANUS(EMBULK)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      INCLUDE 'files.cmn'
!=====VARIABLES FOR BINARY PHASE DIAGRAM
      INTEGER(4) MAXTI,MAXLI,MAXZU,MAXSE
      PARAMETER (MAXTI=500,MAXLI=50,MAXZU=200,MAXSE=400)
!=====
      INTEGER(4) CORNER(3),NUNEW,NUNORI,CHORIG(COMAX),NAKKU,IAKKU(PHMAX), &
      IN2,NTIE,NRL,NSEED,FIRSTP(MAXLI),LASTP(MAXLI),NPHT(MAXLI),CODE1
      COMMON /JAIN/ CORNER,NUNEW,NUNORI,CHORIG,NAKKU,IAKKU,IN2,NTIE, &
      NRL,NSEED,FIRSTP,LASTP,NPHT,CODE1
      REAL(8) XXECK(3,COMAX),ZUG(MAXZU,3),TIEX(MAXTI,2,3),FFF(3),TOLER, &
      SEED(MAXSE,3),XCORN(0:COMAX+3,0:COMAX),YYMIN,YYMAX
      COMMON /JARE/ XXECK,ZUG,TIEX,FFF,TOLER,SEED,XCORN,YYMIN,YYMAX
      CHARACTER(20) ECKE(3),AKKU(PHMAX)
      CHARACTER(25) ASNAM(COMAX)
      CHARACTER(64) U64
      COMMON /JACH/ ECKE,AKKU,ASNAM
!=====END OF COMMON VARIABLES
!-----
      CHARACTER(16) STAB(COMAX)
      LOGICAL(4) MORE
      INTEGER(4) K,I,II,I001,JX,I1,I2,MAXRAN,RANK
      REAL(8) F,AR(COMAX),SUM,EMBULK(3)
!-----minimat variables
      REAL(8) AA(50,4)
      INTEGER(4) NCOL,NROW,RANG,ICOL,IROW,CONR(50)
!-----
!.....
      DO I=1,64
        U64(I:I)='-'
      END DO
!.....
      DO II=1,NC
        CHE(II)=EMBULK(1)*XXECK(1,II)+EMBULK(2)*XXECK(2,II)
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
      GOTO 737
! no check of phases necessary because PICK is active
!-----check phases
      NROW=NC
      NCOL=3
      RANG=0
      DO 900,I=NUN+1,NPHA
      DO ICOL=1,2
        DO IROW=1,NC
          AA(IROW,ICOL)=XXECK(ICOL,IROW)
        END DO
      END DO
      DO IROW=1,NC
        AA(IROW,3)=0.0D0
      END DO
      DO IROW=1,NUN
        AA(CHMCOD(IROW),3)=XX(I,IROW)
      END DO
      DO ICOL=1,3
        CONR(ICOL)=ICOL
      END DO
      CALL MINIMAT(AA,NCOL,NROW,CONR,RANG)
!.....
!     WRITE (UNIT=scr,FMT='(A16,2X,I3)') NAME(NUMMER(I)),RANG
!     WRITE (UNIT=out,FMT='(A16,2X,I3)') NAME(NUMMER(I)),RANG
!.....
      IF (RANG.GT.2) NULL(I)=.TRUE.
  900 CONTINUE
!-----end check phases
  737 CONTINUE
      ELSE
      DO I=1,NUN
        BULK(I)=CHE(CHMCOD(I))
      END DO
      END IF
      IF (ECKE(3)(1:2).EQ.'TC') TC=EMBULK(3)
      IF (ECKE(3)(1:2).EQ.'TK') TC=EMBULK(3)-273.15D0
      IF (ECKE(3)(1:1).EQ.'P') P=EMBULK(3)
      CALL NURVONPT
      CALL CALSTR
      CALL THERIA
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
      END IF
  500 CONTINUE
!-----
      DO I=1,2
        DO II=1,NUNORI
          XCORN(I,II)=XXECK(I,CHORIG(II))
        END DO
      END DO
      DO K=1,NUN2
        DO I=1,NC
          CHE(I)=0.0D0
        END DO
        DO I=1,NUN
          CHE(CHMCOD(I))=X(K,I)
        END DO
        DO I=1,NUNORI
          XCORN(K+2,I)=CHE(CHORIG(I))
        END DO
      END DO
!-----
      I001=NUN2+2
      RANK=0
      MAXRAN=MIN0(I001,NUNORI)
      DO 600,K=1,MAXRAN
      JX=0
!-----
      IF (K.LE.2) THEN
      DO II=K,NUNORI
        IF (XCORN(K,II).NE.0.0D0) THEN
        JX=II
        GOTO 421
        END IF
      END DO
  421 IF (JX.EQ.0.AND.EMBULK(K).NE.0.0D0) THEN
      CALL SHOUTF
      WRITE (scr,3000)
      WRITE (out,3000)
 3000 FORMAT (/' corners not linearly independent')
      GOTO 999
      END IF
      IF (JX.EQ.K) GOTO 100
      DO I=1,I001
        XCORN(I,0)=XCORN(I,K)
      END DO
      DO I=1,I001
        XCORN(I,K)=XCORN(I,JX)
      END DO
      DO I=1,I001
        XCORN(I,JX)=XCORN(I,0)
      END DO
  100 CONTINUE
!------
      ELSE
      DO 450,II=K,I001
      IF (XCORN(II,K).NE.0.0D0) THEN
      JX=II
      CALL SHOUTF
      WRITE (scr,3002)
 3002 FORMAT (/' The system is not binary')
      GOTO 999
      END IF
  450 CONTINUE
      END IF
!-----
      F=XCORN(K,K)
      IF (F.EQ.0.0D0) THEN
      DO II=1,I001
        XCORN(II,K)=0.0D0
      END DO
      ELSE
      RANK=RANK+1
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
      IF (XCORN(K,K).NE.0.0D0) THEN
      DO I=1,NUNORI
        XCORN(K,I)=0.0D0
      END DO
      XCORN(K,K)=1.0D0
      END IF
  600 CONTINUE
      IF (RANK.GT.2) THEN
      CALL SHOUTF
      WRITE (scr,3004) RANK
 3004 FORMAT (/I5,': The rank is larger than two')
      GOTO 999
      END IF
!+++++
      IF (CODE1.EQ.1) THEN
      WRITE (scr,FMT='(''RANK='',I5)') RANK
      DO 70,II=1,NUNORI
      WRITE (scr,5000) (XCORN(I,II),I=1,I001)
 5000 FORMAT (' XCORN ',10F12.6)
   70 CONTINUE
      END IF
!+++++
!-----
      DO 660,II=3,I001
      SUM=0.0D0
      DO I=1,NUNORI
        SUM=SUM+XCORN(II,I)
      END DO
      IF (SUM.NE.0.0D0) THEN
      DO I=1,NUNORI
        XCORN(II,I)=XCORN(II,I)/SUM
      END DO
      END IF
  660 CONTINUE
      NUNEW=I001-2
!.....
      IF (CODE1.EQ.1) THEN
      WRITE (scr,4000) U64,(ECKE(I),I=1,3)
      WRITE (out,4000) U64,(ECKE(I),I=1,3)
 4000 FORMAT ('  ',A64/1X,'|',8X,3(2X,A16),2X,'|'/1X,'|',64X,'|')
      DO 560,I=1,NUNEW
        WRITE (scr,4002) I,(XCORN(I+2,II),II=1,2)
        WRITE (out,4002) I,(XCORN(I+2,II),II=1,2)
 4002 FORMAT (1X,'|   ',I2,')',2X,2(3X,1PE14.7,6X),20X,'|')
  560 CONTINUE
      WRITE (scr,FMT='(2X,A64)') U64
      WRITE (out,FMT='(2X,A64)') U64
      DO 562,I=1,NUN2
        IF (NUMMER(I).GT.0) THEN
        STAB(I)=NAME(NUMMER(I))
        ELSE
        STAB(I)=SOLNAM(EMCODE(I))
        END IF
  562 CONTINUE
      WRITE (scr,4004) (STAB(I),I=1,NUN2)
      WRITE (out,4004) (STAB(I),I=1,NUN2)
 4004 FORMAT (' '/4X,16(1X,A7)/'  ')
      WRITE (scr,FMT='(''  '')')
      WRITE (out,FMT='(''  '')')
      END IF
!.....
      RETURN
  999 WRITE (scr,5502) TC,P
      WRITE (out,5502) TC,P
 5502 FORMAT (/'T[C] = ',F9.3,5X,'P = ',F10.2/' ')
      WRITE (scr,5504) (ASNAM(I),I=1,NUN2)
 5504 FORMAT (4A25)
      DO 564,I=1,NUN
      WRITE (scr,5500) NAME(I),BULK(I)
      WRITE (out,5500) NAME(I),BULK(I)
 5500 FORMAT (A16,2X,F20.12)
  564 CONTINUE
      STOP
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
      INTEGER(4) IR,IC,JX,JY,I,II,I2,IJ
      REAL(8) FF,F1,FFF(50)
      RANG=0
!.....
!      DO 900,IC=1,NCOL
!      WRITE (scr,1010) (AA(IR,IC),IR=1,NROW)
!      WRITE (out,1010) (AA(IR,IC),IR=1,NROW)
! 1010 FORMAT(25F10.4)
!  900 CONTINUE
!.....
      DO 600,PIV=1,NROW
      DO IC=PIV,NCOL
        COLSUM(IC)=0.0D0
        DO IR=1,NROW
          COLSUM(IC)=COLSUM(IC)+DABS(AA(IR,IC))
        END DO
      END DO
      JY=0
      JX=0
      WERT=0.0D0
      DO IR=PIV,NROW
        DO IC=PIV,NCOL
          IF (COLSUM(IC).LE.0.0D0) GOTO 609
          FF=DABS(AA(IR,IC)/COLSUM(IC))
          IF (FF.GT.WERT) THEN
          WERT=FF
          JX=IC
          JY=IR
          END IF
  609   CONTINUE
        END DO
        IF (WERT.NE.0.0D0) GOTO 611
      END DO
  611 CONTINUE
      IF (JX.EQ.0.OR.JY.EQ.0) RETURN
      IF (JX.NE.PIV) THEN
!----colchg
!     CALL COLCHG(JX,PIV)
      IJ=CONR(JX)
      DO I=1,NROW
        FFF(I)=AA(I,JX)
      END DO
      CONR(JX)=CONR(PIV)
      DO I=1,NROW
        AA(I,JX)=AA(I,PIV)
      END DO
      CONR(PIV)=IJ
      DO I=1,NROW
        AA(I,PIV)=FFF(I)
      END DO
!----
      END IF
      IF (JY.NE.PIV) THEN
!----rowchg
!     CALL ROWCHG(JY,PIV)
      DO I=1,NCOL
        FFF(I)=AA(JY,I)
      END DO
      DO I=1,NCOL
        AA(JY,I)=AA(PIV,I)
      END DO
      DO I=1,NCOL
        AA(PIV,I)=FFF(I)
      END DO
!----
      END IF
!----
      FF=AA(PIV,PIV)
!     WRITE (UNIT=scr,FMT='(''FF= '',F10.4)') FF
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

