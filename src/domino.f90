!-----Version: 28.05.2022
!               **********
!               * DOMINO *
!               **********
!
!     Calculation of equilibrium assemblage diagrams
!
!     Program written by Christian de Capitani
!     at the Department of Geology
!            Stanford University
!            Stanford, CA., 94305   (1989-1991)
!     and at Mineralogisch-Petrographisches Institut
!            Universitaet Basel     (since 1992)
!
!     major revision: July 1993
!     revisions: October 2002, July 2004, February 2005, May 2006, March 2007,
!                December 2007, May 2008, August 2009, June 2014
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
!          christian.decapitani@unibas.ch
!
!
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      INCLUDE 'files.cmn'
!--
!-----END OF COMMON VARIABLES
      LOGICAL(4) MORE,SIMU,MULTI,TITP,TMODE
      INTEGER(4) I001,I002,I,I1,I2,I0,II,COMAY,EINS,ALLES
      REAL(8) FF,TIN,PIN,TSIM,PSIM
      CHARACTER(500) CH001,CH002,SYREC,CHIN(5)
      CHARACTER(16) KEYWORD,CH16
      CHARACTER(80) KOMMENTAR
!----
      REAL(8) WPREC,CPREC,CWINK,F1,F2,F3,F4,FX1,FY1,FX2,FY2,FX3,FY3
      INTEGER(4) NL1,NL2,IRE,IOB,IL,IP,LEVMAX,LEVCUR,IDELX,IDELY, &
      LEV,ICASE,LREF,IXREF,IYREF,AGIT(50,50),I3,I4,IA,ierr, j, jj, &
      isimu,LARG
      CHARACTER(40) HITEXT
      CHARACTER(26) SORTSTRING(PHMAX)
      CHARACTER(50) CHCOM
      CHARACTER(250) SCRFILE,DBFILE,COPLOT
      CHARACTER(500) CHLINE(6),XLINE,YLINE,WASLINE,LABLINE,ZEITSTRING
      CHARACTER(80) PIXFNAME,ISODIVCH
      CHARACTER(8) CH8
!--
      call clearscreen
      progname='DOMINO'
      vers='28.05.2022'
      task='"Computation of phase diagrams in complex systems"'
      isimu=0
      sdate=' '
!-----
      LARG=0
      IERR=0
      DO I=1,5
      CALL GetLineArgs (I,LARGUM(I),IERR)
      IF(IERR.NE.0.OR.LARGUM(I).EQ.' ')THEN
      GOTO 200
      ELSE
      LARG=LARG+1
      END IF
      END DO
  200 CONTINUE
      IFNR=5
      IF (LARG.GT.0) THEN
      INFILE=LARGUM(1)
      CALL LABLA(INFILE,I1)
      IFNR=39
      OPEN (UNIT=IFNR,FILE=INFILE(1:I1),STATUS='UNKNOWN')
      END IF
!-----
!      WRITE (UNIT=6,FMT='(''larg: '',i2)') larg
!      WRITE (UNIT=6,FMT='(''arg: '',i2,1x,a)') &
!      ((I,largum(I)),I=1,larg)
!-----
!    5 CONTINUE
!*****
      EINS=1
!*****
      SIMU=.FALSE.
      ierr=0
      call initialize('$DOMINO-FILES',ierr)
      if(ierr.ne.0) STOP
    1 CONTINUE
!*****
      DO I=1,5
        CHIN(I)=' '
      END DO
      DO I=1,6
        CHLINE(I)=' '
      END DO
      XLINE=' '
      YLINE=' '
      WASLINE=' '
      LABLINE=' '
      TIN=0.0D0
      PIN=0.0D0
      TSIM=0.0D0
      PSIM=0.0D0
      PIXFNAME=' '
      MULTI=.FALSE.
      TMODE=.FALSE.
!------------------
!     Open UNIT=log
!------------------
      j=log
      line=filename(j)(1:fnl(j))//ext(j)
      path=wpath
      akzess=' '
      state=' '
      call openfile(j,ierr)
      if(ierr.ne.0) STOP
!
      DO I=1,5
        READ (UNIT=log,FMT='(A500)',END=210) CHIN(I)
      END DO
      READ (log,FMT='(F10.4,1X,F10.2)',END=210) TIN,PIN
  210 CONTINUE
      CALL LABLA(CHIN(1),I002)
      IF (I002.EQ.0) I002=1
      WRITE (6,1000)
 1000 FORMAT (/, &
      ' -------------------',/, &
      ' database definition',/, &
      ' -------------------')
      IF (.NOT.SIMU) THEN
    3 CONTINUE
      WRITE(UNIT=scr, &
      FMT='(A)') ' Enter [ "?" | CR | "files" | "script" '// &
      '| database filename ] <'//CHIN(1)(1:I002)//'>? '
      READ (IFNR,FMT='(A500)') CH001
!---
      IF (CH001.EQ.'?') THEN
       call helpme('$DOM-SCRIPT')
       GOTO 3
      END IF
      IF (VERGL(CH001,'files')) then
       call listfiles
       goto 3
      END IF
!---
      END IF
!=====
      IF (VERGL(CH001,'script').OR.SIMU) THEN
      SIMU=.TRUE.
      CALL SCRDEF(CH001,isimu,CHIN(1),COPLOT,SCRFILE)
      END IF
!======
      COPLOT=filename(plt)(1:fnl(plt))//ext(plt)
!------
      IF (CH001(1:7).EQ.'script.') THEN
      MULTI=.TRUE.
      SCRFILE=CH001(8:)
      CALL LABLA(SCRFILE,I)
      READ(UNIT=IFNR,FMT='(A)') COPLOT
      READ (UNIT=IFNR,FMT='(A)') DBFILE
      CH001=DBFILE
      END IF
!=====
      CALL LABLA(CH001,I001)
      IF (I001.EQ.0) THEN
      CH001=CHIN(1)
      I001=I002
      ELSE
      CHIN(1)=CH001
      END IF
!=====
      CH002=CH001
      CALL TAXI(CH002,DBNAME)
      filename(dbs)=DBNAME
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
!c------------------
!c     open UNIT=plt
!c------------------
!      IF(.NOT.SIMU) THEN
!      j=plt
!      line=COPLOT
!      path=wpath
!      akzess=' '
!      state=' '
!      call openfile(j,ierr)
!      if(ierr.ne.0) STOP
!      END IF
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
!
      IF (SIMU) THEN
      DBFILE=CH001
      CALL PUST(spt,DBFILE)
      END IF
!*****
      REWIND (UNIT=out)
      REWIND (UNIT=dbs)
      REWIND (UNIT=dat)
!      REWIND (UNIT=plt)
!*****
      COMAY=COMAX
      CALL PROREAD(SYREC)
!-- pick nach proread!
      NPICK=0
      DO I=1,COMAX
       CALL TAXI(CH002,CH16)
       IF (CH16.NE.' ') THEN
       NPICK=NPICK+1
       PICK(NPICK)=CH16
       ELSE
       GOTO 215
       END IF
      END DO
  215 CONTINUE
!--
!=====
      IF (SIMU) THEN
      WRITE (spt,1010) PRAT,LO1MAX,EQUALX,TEST,DXMIN,DXSCAN, &
      DXSTAR,STPSTA,STPMAX,GCMAX
 1010 FORMAT (F10.4,2X,I5,2X,F10.6,2X,E14.7,2X,E14.7,2X,E14.7, &
      E14.7,2X,I5,2X,I6,2X,I6)
      CHLINE(1)=SYREC
      CALL PUST(spt,CHLINE(1))
      END IF
!=
      IF (MULTI) THEN
      READ (IFNR,1012) PRAT,LO1MAX,EQUALX,TEST,DXMIN,DXSCAN, &
      DXSTAR,STPSTA,STPMAX,GCMAX
 1012 FORMAT (F10.4,2X,I5,2X,F10.6,2X,E14.7,2X,E14.7,2X,E14.7, &
      E14.7,2X,I5,2X,I6,2X,I6)
!cdcmar08: hier hat intel muehe wenn leere Zeilen folgen: (muss ein blank haben)
!          forrtl: severe (174): SIGSEGV, segmentation fault occurred
      DO I=1,6
        READ (IFNR,FMT='(A500)') CHLINE(I)
!      CALL PUST(6,CHLINE(I))
      END DO
      READ (IFNR,FMT='(A500)') XLINE
      READ (IFNR,FMT='(A500)') YLINE
      READ (IFNR,FMT='(A500)') WASLINE
      READ (IFNR,FMT='(A500)') LABLINE
      READ (UNIT=IFNR,FMT='(E14.7,2X,E14.7)') TSIM,PSIM
      READ (IFNR,FMT='(A80)') PIXFNAME
      SYREC=CHLINE(1)
      END IF
!-----
!-----READ  PRTCOD, FORMUL AND USE FROM SYREC
!-----SET UP FIRST NUN COLUMNS OF MATRIX
      CALL GELI(SYREC,FF)
      PRTCOD=IDINT(FF)
      DO I=1,11
        PRTLOG(I)=.FALSE.
      END DO
!-----
      CALL TAXI(SYREC,FORMUL)
      NBUL=1
      BULINE(1)=FORMUL
      CALL LABLA(FORMUL,I1)
      WRITE (UNIT=out,FMT='(170A1)') (FORMUL(I:I),I=1,I1)
      CALL TAXI(SYREC,USE)
      CALL CHEMIE(COMAY,NC,OXYDE,OXANZ,FORMUL,CHEM)
      CALL LABLA(USE,LUSE)
      CALL TAXI(SYREC,KOMMENTAR)
!*****
      TEST=DABS(TEST)
      CALL DBREAD
      CALL NURVONPT
      CALL CALSTR
      ALLES=NSOL
      CALL GIBBSTEST(EINS,ALLES)
!-----START LOOPING
      WRITE (out,1020) CHNAME(1),CHEM(CHMCOD(1))
 1020 FORMAT (' Bulk composition: ',A8,1X,F11.6)
      DO I=2,NUN
        WRITE (out,1022) CHNAME(I),CHEM(CHMCOD(I))
 1022 FORMAT (19X,A8,1X,F11.6)
      END DO
!----
      NCTT=0
      NCTTOT=0
      NASS=0
      NREAC=0
      NPUN=0
      NLIN=0
      AFIX1=0
      AFIX2=0
      DO I=1,4
      DO II=1,NC
        CHEX(I,II)=0.0D0
      END DO
      END DO
      XVAR=' '
      YVAR=' '
      NPA(-1)=0
      TEXTA(-1)=' '
!-----
!----- define SORTSTRING with names and abbreviations
      DO I=NUN+1,NPHA
        CALL LABLA(NAME(I),j)
        CALL LABLA(ABK(I),jj)
        SORTSTRING(I-NUN)=NAME(I)(1:j)//' ('//ABK(I)(1:jj)//')'
      END DO
      I001=NPHA-NUN
      CALL SORTIER(SORTSTRING,I001)
!-----
      TITP=.TRUE.
   10 SYREC=' '
      IF (MULTI) THEN
      SYREC=XLINE
      ELSE
      IF (TITP) WRITE (6,1024)
 1024 FORMAT (/ &
      ' --------------------'/ &
      ' definition of X-axis'/ &
      ' --------------------')
      CALL helpme('$DOM-XAXES*')
      TITP=.TRUE.
      CALL LABLA(CHIN(2),I002)
      IF (I002.EQ.0) I002=1
      WRITE (scr,1026) CHIN(2)(1:I002)
 1026 FORMAT (' Enter [ "?" | "list" | CR | X-variable  X-min  ', &
      'X-max  (X-Grid)  (width)  (ptdist)] <',a,'>? ')
      READ (UNIT=IFNR,FMT='(A500)') SYREC
      END IF
!>
      IF (SYREC.eq.'?') THEN
        CALL helpme('$DOM-XAXES')
        GOTO 10
      END IF
      IF (VERGL(SYREC,'list')) THEN
      I001=NPHA-NUN
      WRITE (scr,1028) ('-',I=1,35)
 1028 FORMAT (/,'List of phase names (abbreviations)',/,80A1)
      WRITE (UNIT=scr,FMT='(5(2X,A22))') (SORTSTRING(II),II=1,I001)
      WRITE (UNIT=scr,FMT='(120A1)') ('-',I=1,120)
      TITP=.FALSE.
      GOTO 10
      END IF
!>
      IF (SYREC.EQ.' ') THEN
      SYREC=CHIN(2)
      ELSE
      CHIN(2)=SYREC
      END IF
      IF (SIMU) XLINE=SYREC
      CALL TAXI(SYREC,XVAR)
      CALL GELI(SYREC,XMIN)
      CALL GELI(SYREC,XMAX)
      CALL GELI(SYREC,FF)
      IDELX=IDINT(FF)
      CALL GELI(SYREC,BREITE)
      CALL GELI(SYREC,PTDIST)
      IF (PTDIST.LT.0.01D0) PTDIST=100.0D0
      IF (XMAX.LT.XMIN) THEN
      FF=XMIN
      XMIN=XMAX
      XMAX=FF
      END IF
      CALL LOWUP(XVAR)
      IF (XVAR.NE.'TC'.AND.XVAR.NE.'TK'.AND.XVAR.NE.'P' &
      .AND.XVAR(1:2).NE.'A('.AND.XVAR(1:4).NE.'LNA(' &
      .AND.XVAR.NE.'BIN'.AND.XVAR(1:5).NE.'LOGA(' &
      .AND.XVAR(1:3).NE.'MA('.AND.XVAR.NE.'TER') THEN
      IF (MULTI) STOP
      CALL SHOUTI
      WRITE (6,1030)
 1030 FORMAT (//' X-variable not recognized by DOMINO')
      GOTO 10
      END IF
      IF (XMIN.EQ.XMAX.AND.XVAR.NE.'TER') THEN
      IF (MULTI) STOP
      CALL SHOUTI
      WRITE (6,1032)
 1032 FORMAT (//' X-min is equal to X-max')
      GOTO 10
      END IF
!---
      IF (XVAR.EQ.'P'.AND.XMIN.EQ.0.0D0) XMIN=0.0001
      IF (XVAR.EQ.'P'.AND.XMAX.EQ.0.0D0) XMAX=0.0001
!---
      IF (XVAR.EQ.'BIN'.OR.XVAR.EQ.'TER') THEN
      I1=2
      NBUL=0
      IF (XVAR.EQ.'TER') I1=3
      DO 500,I=1,I1
      IF (MULTI) THEN
      SYREC=CHLINE(I+1)
      ELSE
      READ (UNIT=dat,FMT='(A500)') SYREC
      IF (SIMU) CHLINE(I+1)=SYREC
      END IF
      CALL GELI(SYREC,FF)
      CALL TAXI(SYREC,FORMUL)
      NBUL=NBUL+1
      BULINE(NBUL)=FORMUL
      CALL LABLA(FORMUL,I2)
      WRITE (UNIT=out,FMT='(170A1)') (FORMUL(I3:I3),I3=1,I2)
      CALL CHEMIE(COMAY,NC,OXYDE,OXANZ,FORMUL,CHE)
      DO II=1,NC
        CHEX(I,II)=CHE(II)
      END DO
  500 CONTINUE
      IF (XVAR.EQ.'TER') THEN
      XMIN=0.0D0
      XMAX=1.0D0
      YMIN=0.0D0
      YMAX=1.0D0
      BREITE=15.0D0
      HOEHE=15.0D0
      YVAR='TER'
      GOTO 12
      END IF
      IF (DMAX1(XMIN,XMAX).GT.1.0D0 &
      .OR.DMIN1(XMIN,XMAX).LT.0.0D0) THEN
      XMIN=0.0D0
      XMAX=1.0D0
      END IF
      END IF
!-----
      TITP=.TRUE.
   11 SYREC=' '
      IF (MULTI) THEN
      SYREC=YLINE
      ELSE
      IF (TITP) WRITE (6,1034)
 1034 FORMAT (/ &
      ' --------------------'/ &
      ' definition of Y-axis'/ &
      ' --------------------')
      CALL helpme('$DOM-YAXES*')
      TITP=.TRUE.
      CALL LABLA(CHIN(3),I002)
      IF (I002.EQ.0) I002=1
      WRITE (scr,1036) CHIN(3)(1:I002)
 1036 FORMAT (' Enter [ "?" | CR | "list" | Y-variable  Y-min  ', &
      'Y-max  (Y-Grid)  (height)] <',a,'>? ')
      READ (UNIT=IFNR,FMT='(A500)') SYREC
      END IF
!>
      IF (SYREC.eq.'?') THEN
        CALL helpme('$DOM-YAXES')
        GOTO 11
      END IF
      IF (VERGL(SYREC,'list')) THEN
      I001=NPHA-NUN
      WRITE (scr,1038) ('-',I=1,35)
 1038 FORMAT (/,'List of phase names (abbreviations)',/,80A1)
      WRITE (UNIT=scr,FMT='(5(2X,A22))') (SORTSTRING(II),II=1,I001)
      WRITE (UNIT=scr,FMT='(120A1)') ('-',I=1,120)
      TITP=.FALSE.
      GOTO 11
      END IF
!>
      IF (SYREC.EQ.' ') THEN
      SYREC=CHIN(3)
      ELSE
      CHIN(3)=SYREC
      END IF
      IF (SIMU) YLINE=SYREC
      CALL TAXI(SYREC,YVAR)
      IF (YVAR.EQ.'THG') CALL GELI(SYREC,THGR)
      CALL GELI(SYREC,YMIN)
      CALL GELI(SYREC,YMAX)
      CALL GELI(SYREC,FF)
      IDELY=IDINT(FF)
      CALL GELI(SYREC,HOEHE)
      IF (YMAX.LT.YMIN) THEN
      FF=YMIN
      YMIN=YMAX
      YMAX=FF
      END IF
      CALL LOWUP(YVAR)
      IF (YVAR.NE.'TC'.AND.YVAR.NE.'TK'.AND.YVAR.NE.'P' &
      .AND.YVAR(1:2).NE.'A('.AND.YVAR(1:4).NE.'LNA(' &
      .AND.YVAR.NE.'BIN'.AND.YVAR(1:5).NE.'LOGA(' &
      .AND.YVAR(1:3).NE.'MA('.AND.YVAR.NE.'PT'.AND.YVAR.NE.'THG') &
      THEN
      IF (MULTI) STOP
      CALL SHOUTI
      WRITE (6,1040)
 1040 FORMAT (//' Y-variable not recognized by DOMINO')
      GOTO 11
      END IF
      IF (YMIN.EQ.YMAX) THEN
      IF (MULTI) STOP
      CALL SHOUTI
      WRITE (6,1042)
 1042 FORMAT (//' Y-min is equal to Y-max')
      GOTO 11
      END IF
!---
      IF (YVAR.EQ.'P'.AND.YMIN.EQ.0.0D0) YMIN=0.0001
      IF (YVAR.EQ.'P'.AND.YMAX.EQ.0.0D0) YMAX=0.0001
!-----
   12 IF (YVAR.EQ.'BIN') THEN
      DO 520,I=3,4
      IF (MULTI) THEN
          SYREC=CHLINE(I+2)
      ELSE
          READ (UNIT=dat,FMT='(A500)') SYREC
          IF (SIMU) CHLINE(I+2)=SYREC
      END IF
      CALL GELI(SYREC,FF)
      CALL TAXI(SYREC,FORMUL)
      NBUL=NBUL+1
      BULINE(NBUL)=FORMUL
      CALL LABLA(FORMUL,I1)
      WRITE (UNIT=out,FMT='(170A1)') (FORMUL(I3:I3),I3=1,I1)
      CALL CHEMIE(COMAY,NC,OXYDE,OXANZ,FORMUL,CHE)
      DO II=1,NC
        CHEX(I,II)=CHE(II)
      END DO
  520 CONTINUE
      IF (DMAX1(YMIN,YMAX).GT.1.0D0 &
      .OR.DMIN1(YMIN,YMAX).LT.0.0D0) THEN
      YMIN=0.0D0
      YMAX=1.0D0
      END IF
      END IF
!-----
      IF ((XVAR.EQ.'TC'.OR.XVAR.EQ.'TK') &
      .AND.(YVAR.EQ.'TC'.OR.YVAR.EQ.'TK'.OR.YVAR.EQ.'PT' &
      .OR.YVAR.EQ.'THG')) THEN
      IF (MULTI) STOP
      CALL SHOUTI
      WRITE (6,1044)
 1044 FORMAT (//,' X- and Y-variable are both Temperature!')
      GOTO 10
      END IF
      IF (XVAR.EQ.'P'.AND.YVAR.EQ.'P') THEN
      IF (MULTI) STOP
      CALL SHOUTI
      WRITE (6,1046)
 1046 FORMAT (//' X- and Y-variable are both Pressure!')
      GOTO 10
      END IF
!-----
      IF (XVAR.EQ.'BIN'.OR.XVAR.EQ.'TER'.OR.YVAR.EQ.'BIN') &
       THEN
      DO II=1,NC
        CHEM(II)=0.0D0
        DO  I=1,4
          CHEM(II)=CHEM(II)+CHEX(I,II)
        END DO
      END DO
      CALL DBREAD
!     WRITE (6,2100) CHNAME(1),CHEM(CHMCOD(1))
      WRITE (out,1048) CHNAME(1),CHEM(CHMCOD(1))
 1048 FORMAT (' Bulk composition: ',A8,1X,F11.6)
      DO I=2,NUN
!       WRITE (6,2102) CHNAME(I),CHEM(CHMCOD(I))
        WRITE (out,1050) CHNAME(I),CHEM(CHMCOD(I))
 1050 FORMAT (19X,A8,1X,F11.6)
      END DO
      CALL NURVONPT
      CALL CALSTR
      ALLES=NSOL
      CALL GIBBSTEST(EINS,NSOL)
      END IF
!-----
!     this test is here because it can only be made after
!     the DBREAD statement.
      IF (XVAR(1:2).EQ.'A('.OR.XVAR(1:4).EQ.'LNA(' &
      .OR.XVAR(1:5).EQ.'LOGA('.OR.XVAR(1:3).EQ.'MA(') THEN
      I2=INDEX(XVAR,'(')+1
      I1=INDEX(XVAR,')')-1
      IF (I1.LT.I2) THEN
      IF (MULTI) STOP
      CALL SHOUTI
      WRITE (6,1052) I2,I1,XVAR
 1052 FORMAT (//' Expression ..A(xxx) is incorrect ',2I3,2X,A) 
      GOTO 10
      END IF
      CH8=XVAR(I2:I1)
      CALL FIXA(CH8,AFIX1,OFIX1)
      XVAR(I2:I1)=CH8
      IF (AFIX1.EQ.0) THEN
      IF (MULTI) STOP
      CALL SHOUTI
      WRITE (scr,1054) CH8
 1054 FORMAT (// &
      ' The phase ',A8,' was not found in the database'/ &
      ' Possible  reasons:'/ &
      '    a) Phase is not in database'/ &
      '    b) Wrong abbreviation was used'/ &
      '    c) Phase is not within defined chemical system'/ &
      '    c) Phase is excluded by "USE"-variable')
      IF (YVAR.EQ.'BIN') STOP
      GOTO 10
      END IF
      END IF
!-----
      IF (YVAR(1:2).EQ.'A('.OR.YVAR(1:4).EQ.'LNA(' &
      .OR.YVAR(1:5).EQ.'LOGA('.OR.YVAR(1:3).EQ.'MA(') THEN
      I2=INDEX(YVAR,'(')+1
      I1=INDEX(YVAR,')')-1
      IF (I1.LT.I2) THEN
      IF (MULTI) STOP
      CALL SHOUTI
      WRITE (scr,1052) I2,I1,YVAR
      GOTO 11
      END IF
      CH8=YVAR(I2:I1)
      CALL FIXA(CH8,AFIX2,OFIX2)
      YVAR(I2:I1)=CH8
      IF (AFIX2.EQ.0) THEN
      IF (MULTI) STOP
      CALL SHOUTI
      WRITE (scr,1054) CH8
      GOTO 11
      END IF
      END IF
!-----
      XTEXT=XVAR
      IF (XVAR.EQ.'TC') XTEXT='Temperature [C]'
      IF (XVAR.EQ.'TK') XTEXT='Temperature [K]'
      IF (XVAR.EQ.'P') XTEXT='Pressure [Bar]'
      IF (XVAR.EQ.'BIN') XTEXT='binary'
      IF (XVAR(1:2).EQ.'A(') XTEXT(1:1)='a'
      IF (XVAR(1:4).EQ.'LNA(') THEN
      CH8=XVAR(5:)
      XTEXT='Ln a('//CH8
      END IF
      IF (XVAR(1:5).EQ.'LOGA(') THEN
      CH8=XVAR(6:)
      XTEXT='Log a('//CH8
      END IF
      IF (XVAR(1:3).EQ.'MA(') THEN
      CH8=XVAR(4:)
!      XTEXT='mu('//CH8
      XTEXT='µ('//CH8
      END IF
      YTEXT=YVAR
!
      IF (YVAR.EQ.'THG') THEN
      F1=THGR
      CALL NUMTEX(F1,HITEXT,I1)
      YTEXT='Temp.[C],P='//HITEXT(1:I1)//'*(TC-25)+1'
      END IF
!
      IF (YVAR.EQ.'TC') YTEXT='Temperature [C]'
      IF (YVAR.EQ.'PT') YTEXT='Temp.[C],P=f(T)'
      IF (YVAR.EQ.'TK') YTEXT='Temperature [K]'
      IF (YVAR.EQ.'P') YTEXT='Pressure [Bar]'
      IF (YVAR.EQ.'BIN') YTEXT='binary'
      IF (YVAR(1:2).EQ.'A(') YTEXT(1:1)='a'
      IF (YVAR(1:4).EQ.'LNA(') THEN
      CH8=YVAR(5:)
      YTEXT='Ln a('//CH8
      END IF
      IF (YVAR(1:5).EQ.'LOGA(') THEN
      CH8=YVAR(6:)
      YTEXT='Log a('//CH8
      END IF
      IF (YVAR(1:3).EQ.'MA(') THEN
      CH8=YVAR(4:)
!      YTEXT='mu('//CH8
      YTEXT='µ('//CH8
      END IF
      CALL LABLA(XTEXT,I1)
      I1=I1+2
!-----
      CH=' '
      IF (XVAR.NE.'TC'.AND.XVAR.NE.'TK' &
      .AND.YVAR.NE.'TC'.AND.YVAR.NE.'TK'.AND.YVAR.NE.'PT' &
      .AND.YVAR.NE.'THG') THEN
!
      IF (MULTI) THEN
      TC=TSIM
      TIN=TSIM
      ELSE
      WRITE (scr,1056) TIN
 1056 FORMAT (/,' Enter Temperature in deg. C (',F10.4,')')
      READ (UNIT=IFNR,FMT='(A500)') SYREC
      IF (SYREC.EQ.' ') THEN
      TC=TIN
      ELSE
      CALL GELI(SYREC,TC)
      TIN=TC
      END IF
      IF (SIMU) TSIM=TC
      END IF
!
      T=TC+273.15D0
      WRITE (UNIT=CH,FMT='(''T='',F8.2,''[C]'')') TC
      DO 550,I=1,13
      IF (CH(I:I).NE.' ') THEN
      XTEXT(I1:I1)=CH(I:I)
      I1=I1+1
      END IF
  550 CONTINUE
      I1=I1+1
      END IF
!-----
      CH=' '
      IF (XVAR.NE.'P'.AND.YVAR.NE.'P'.AND.YVAR.NE.'PT' &
      .AND.YVAR.NE.'THG') THEN
      IF (MULTI) THEN
      P=PSIM
      PIN=PSIM
      ELSE
      WRITE (6,1058) PIN
 1058 FORMAT (/,' Enter Pressure in Bar (',F10.2,')')
      READ (UNIT=IFNR,FMT='(A500)') SYREC
      IF (SYREC.EQ.' ') THEN
      P=PIN
      ELSE
      CALL GELI(SYREC,P)
      PIN=P
      END IF
      IF (SIMU) PSIM=P
      END IF
!
      WRITE (UNIT=CH,FMT='(''P='',F8.1,''[Bar]'')') P
      DO 600,I=1,16
      IF (CH(I:I).NE.' ') THEN
      XTEXT(I1:I1)=CH(I:I)
      I1=I1+1
      END IF
  600 CONTINUE
      END IF
      IF (XVAR.EQ.'TER') THEN
      YTEXT=XTEXT(5:)
      XTEXT='ternary'
      END IF
!=====
   15 MAP=.FALSE.
      I0=0
      I1=0
      I2=0
!*****
      IF (MULTI) THEN
          SYREC=WASLINE
      ELSE
!*****
      WRITE (6,1060)
 1060 FORMAT (/ &
      ' ------------------------------'/ &
      ' definition of calculation type'/ &
      ' ------------------------------')
   13 CONTINUE
      CALL helpme('$DOM-ISO*')
      CALL LABLA(CHIN(4),I002)
      IF (I002.EQ.0) I002=1
      CH001=' Enter [  "?"  | CR | "point" | "list" | "i" | "." |'
      CH002='       | "pix" ix iy | (Phase)  Key  (Nr)  '// &
      'min  max  step ] <'//CHIN(4)(1:I002)//'>? '
      CALL PUST(scr,CH001)
      CALL PUST(scr,CH002)
      READ (UNIT=IFNR,FMT='(A500)') SYREC
!-----
      IF (VERGL(SYREC,'list')) THEN
!      DO 610,I=NUN+1,NPHA
!  610 SORTSTRING(I-NUN)=NAME(I)
!      I001=NPHA-NUN
!      CALL SORTIER(SORTSTRING,I001)
!      WRITE (UNIT=scr,FMT='(/,''List of phases'')')
!      WRITE (UNIT=scr,FMT='(80A1)') ('-',I=1,14)
!      WRITE (6,FMT='(6(4X,A16))') (SORTSTRING(II),II=1,I001)
      I001=NPHA-NUN
      WRITE (scr,1062) ('-',I=1,35)
  1062 FORMAT (/,'List of phase names (abbreviations)',/,80A1)
      WRITE (UNIT=scr,FMT='(5(2X,A22))') (SORTSTRING(II),II=1,I001)
!--
      IF (NSOL.GT.0) THEN
      WRITE (UNIT=scr,FMT='(/A)') &
      'List of solutions, endmembers and site occupancies'
      WRITE (UNIT=scr,FMT='(80A1)') ('-',I=1,50)
      DO 650,I=1,NSOL
      WRITE (6,1064) SOLNAM(I),(ABK(EM(I,II)),II=1,NEND(I))
 1064 FORMAT (3X,A16,' : ',5(A8,2X)/20(22X,5(A8,2X)/))
      IF (NSIEL(I).GT.0) THEN
      WRITE (6,1066) (SIEL(I,II),II=1,NSIEL(I))
 1066 FORMAT (20(22X,5A10/))
      END IF
  650 CONTINUE
      END IF
      WRITE (UNIT=scr,FMT='('' '')')
      GOTO 13
      END IF
!-----
      IF (SYREC.eq.'?') THEN
        CALL helpme('$DOM-ISO')
        GOTO 13
      END IF
!-----
      IF (VERGL(SYREC,'point')) THEN
       IF (PRTCOD.GE.1) THEN
        DO I=2,8
         PRTLOG(I)=.TRUE.
        END DO
       END IF
       TMODE=.TRUE.
       IF (CHIN(4).EQ.' ') THEN
        CHIN(4)='.'
        SYREC=' '
       ELSE
        SYREC=CHIN(4)
       END IF
      END IF
!-----
!+++++
!+++++ added by kostas, 05.06.09
      if(VERGL(syrec,'i'))  then
         I001=NPHA-NUN
         call isohelp (syrec)
         if(syrec.eq.' ') goto 13
      end if
!+++++        
      END IF
!*****
      IF (SYREC.EQ.' ') THEN
        SYREC=CHIN(4)
      ELSE
        CHIN(4)=SYREC
      END IF
      IF (SIMU) WASLINE=SYREC
      KEYWORD=' '
!
! action            input                        coding
! ------            -----                        ------
!                                                 (I0)  (I1)  (I2)
!                   phase     keyword   number   MOLPH MAPPH MAPEM MAPNR
!                   -----     -------   ------   ----- ----- ----- -----
! REACTIONS         '.'         -         -        0     0     0
! moles of phase    name      'mol'       -       IP     0    -1     1
! vol of phase      name      'vol'       -       IP     0    -2     1
! mol.vol of phase  name      'mvol'      -       IP     0    -3     1
! wt of phase       name      'wt'        -       IP     0    -4     1
! mol.wt of phase   name      'mwt'       -       IP     0    -5     1
! rho of phase      name      'rho'       -       IP     0    -6     1
! vol% of phase     name      'vol%'      -       IP     0    -8     1
! G_ (if present)   name      'g'         -       IP     0    -9     1
!
! isopleths         sol.name  endmember   NR       0    IS   emNr    NR
! isocomp.(on site) sol.name  El(site)    NR       0    IS   EM+site NR
! moles of phase    sol.name  'mol'       NR       0    IS    -1     NR
! vol of phase      sol.name  'vol'       NR       0    IS    -2     NR
! mol.vol of phase  sol.name  'mvol'      NR       0    IS    -3     NR
! wt of phase       sol.name  'wt'        NR       0    IS    -4     NR
! mol.wt of phase   sol.name  'mwt'       NR       0    IS    -5     NR
! rho of phase      sol.name  'rho'       NR       0    IS    -6     NR
! Mg/(Mg+Fe)        sol.name  'Mg#'       NR       0    IS    -7     NR
! vol% of phase     sol.name  'vol%'      NR       0    IS    -8     NR
! G_ (if present)   sol.name  'g'         NR       0    IS    -9     NR
! Si(pfu)           sol.name  'sipfu'     NR       0    IS   -10     NR
! Al(pfu)           sol.name  'alpfu'     NR       0    IS   -11     NR
!
! vol of solids     'volsol'    -         -      -1000   0    -1     1
! wt of solids      'wtsol'     -         -      -1000   0    -2     1
! rho of solids     'rhosol'    -         -      -1000   0    -3     1
! total G           'gtot'      -         -      -1000   0    -4     1
! wt% H2O in solids '%h2o.sol'  -         -      -1000   0    -5     1
!
!----- here FUNTAXI
!      CALL TAXI(SYREC,SONAM)
      ISODIV=1.0D0
      CALL FUNTAXI(SYREC,SONAM,ISODIV,ISODIVCH)
!!!!      WRITE (scr,FMT='(''1.ISODIV '',F10.5,2X,A)') ISODIV,ISODIVCH
!-----
      IF (TMODE) THEN
       IF (SIMU) CLOSE(UNIT=job)
       GOTO 88
      END IF
!-----
      IF (VERGL(SONAM,'pix')) THEN
      CALL GELI(SYREC,FF)
      IDELX=IDINT(FF)
      CALL GELI(SYREC,FF)
      IDELY=IDINT(FF)
      IF (SIMU) CLOSE(UNIT=job)
      GOTO 88
      END IF
      IF (SONAM.EQ.'.') THEN
!      VNEED=.FALSE.
      GOTO 40
      END IF
      IF (VERGL(SONAM,'volsol')) THEN
      I0=-1000
      I2=-1
      GOTO 40
      END IF
      IF (VERGL(SONAM,'wtsol')) THEN
      I0=-1000
      I2=-2
      GOTO 40
      END IF
      IF (VERGL(SONAM,'rhosol')) THEN
      I0=-1000
      I2=-3
      GOTO 40
      END IF
      IF (VERGL(SONAM,'gtot')) THEN
      I0=-1000
      I2=-4
      GOTO 40
      END IF
      IF (VERGL(SONAM,'%h2o.sol')) THEN
      I0=-1000
      I2=-5
      GOTO 40
      END IF
!-----
      DO I=1,NPHA
!        write(*,*) name(i), sonam
!        IF (NAME(I).EQ.SONAM) THEN
        IF (VERGL(NAME(I),SONAM)) THEN
        SONAM=NAME(I)
        I0=I
        GOTO 250
        END IF
      END DO
      DO I=1,NSOL
!        write(*,*) solnam(i), sonam
!        IF (SOLNAM(I).EQ.SONAM) THEN
        IF (VERGL(SOLNAM(I),SONAM)) THEN
        SONAM=SOLNAM(I)
        I1=I
        GOTO 220
        END IF
      END DO
  220 CONTINUE
      IF (I1.EQ.0.AND.I0.EQ.0) THEN
      IF (MULTI) STOP
      CALL SHOUTI
      WRITE (6,2000) SONAM
 2000 FORMAT (// &
      ' Phase ',A16,' was not found in the database!')
      GOTO 15
      END IF
!-----
  250 CONTINUE
!----- here FUNTAXI
      IF (ISODIV.NE.1.0D0) THEN
      CALL TAXI(SYREC,KEYWORD)
      ELSE
      CALL FUNTAXI(SYREC,KEYWORD,ISODIV,ISODIVCH)
!!!!      WRITE (scr,FMT='(''2.ISODIV '',F10.5,2X,A)') ISODIV,ISODIVCH
      END IF
      IF (VERGL(KEYWORD,'mol')) THEN
      I2=-1
      GOTO 40
      END IF
      IF (VERGL(KEYWORD,'vol')) THEN
      I2=-2
      GOTO 40
      END IF
      IF (VERGL(KEYWORD,'mvol')) THEN
      I2=-3
      GOTO 40
      END IF
      IF (VERGL(KEYWORD,'wt')) THEN
      I2=-4
      GOTO 40
      END IF
      IF (VERGL(KEYWORD,'mwt')) THEN
      I2=-5
      GOTO 40
      END IF
      IF (VERGL(KEYWORD,'rho')) THEN
      I2=-6
      GOTO 40
      END IF
      IF (VERGL(KEYWORD,'Mg#')) THEN
      I2=-7
      GOTO 40
      END IF
      IF (VERGL(KEYWORD,'vol%')) THEN
      I2=-8
      GOTO 40
      END IF
      IF (VERGL(KEYWORD,'g')) THEN
      I2=-9
      GOTO 40
      END IF
      IF (VERGL(KEYWORD,'sipfu')) THEN
      I2=-10
      GOTO 40
      END IF
      IF (VERGL(KEYWORD,'alpfu')) THEN
      I2=-11
      GOTO 40
      END IF
      IF (I1.EQ.0) GOTO 40
!-----
      DO 680,I=1,NEND(I1)
!      IF (ABK(EM(I1,I)).EQ.KEYWORD) THEN
      IF (VERGL(ABK(EM(I1,I)),KEYWORD)) THEN
      KEYWORD=ABK(EM(I1,I))
      I2=I
      GOTO 40
      END IF
  680 CONTINUE
      DO I=1,NSIEL(I1)
!        IF (SIEL(I1,I).EQ.KEYWORD) THEN
        IF (VERGL(SIEL(I1,I),KEYWORD)) THEN
        KEYWORD=SIEL(I1,I)
        I2=I+NEND(I1)
        GOTO 40
        END IF
      END DO
!=====
   40 IF (I1.NE.0.AND.I2.EQ.0) THEN
      IF (MULTI) STOP
      CALL SHOUTI
      WRITE (6,2002) KEYWORD
 2002 FORMAT (// &
      ' The endmember ',A8,' was not found in the database')
      GOTO 15
      END IF
      IF ((I1.NE.0.AND.I2.NE.0).OR.I0.NE.0) THEN
      MAP=.TRUE.
      MOLPH=I0
      MAPPH=I1
      MAPEM=I2
      IF (I0.NE.0) THEN
      MAPNR=1
      ELSE
      CALL GELI(SYREC,FF)
      MAPNR=IDINT(FF)
      END IF
      CALL GELI(SYREC,F1)
      CALL GELI(SYREC,F2)
      CALL GELI(SYREC,F3)
      IF (F2.LT.F1.AND.F3.LT.0.0D0) THEN
      FF=F1
      F1=F2
      F2=FF
      F3=-F3
      END IF
      IF (F3.EQ.0.0D0) THEN
      IF (MULTI) STOP
      CALL SHOUTI
      WRITE (6,2004)
 2004 FORMAT (// &
      ' step is zero')
      GOTO 15
      END IF
      NGRENZ=0
!--
      FF=F1
      DO WHILE(FF.LE.F2)
        IF (NGRENZ.GE.MAXMAP) THEN
        CALL SHOUTW
        WRITE (6,2006) MAXMAP
 2006 FORMAT (//' only',I4,' isopleths will be calculated')
        GOTO 255
        END IF
        NGRENZ=NGRENZ+1
        GRENZ(NGRENZ)=FF
        FF=FF+F3
      END DO
  255 CONTINUE
!-----
      MAPTEX(1)='NNN'
      MAPTEX(2)=' '
      DO 700,I=3,NGRENZ+2
      CHCOM=' '
      WRITE (UNIT=CHCOM,FMT='(F16.7)') GRENZ(I-2)
      CALL FIBLA(CHCOM,I1)
      DO II=50,1,-1
        IF (CHCOM(II:II).NE.'0'.AND.CHCOM(II:II).NE.' ') THEN
        I2=II
        GOTO 260
        END IF
      END DO
  260 CONTINUE
      IF (CHCOM(I2:I2).EQ.'.') I2=I2-1
      IF (I2.LT.I1) I2=I1
      MAPTEX(I)=CHCOM(I1:I2)
!dC
!dC      WRITE (*,*) MAPTEX(I)
  700 CONTINUE
!-----
      END IF
!=====
!     SET THE DEFAULT VALUES, IF NOT INPUT
   14 CONTINUE
      IF (MULTI) THEN
      SYREC=LABLINE
      ELSE
      WRITE (6,2010)
 2010 FORMAT (/ &
      ' -----------------------------------------------'/ &
      ' labeling of reactions, precision and smoothness'/ &
      ' -----------------------------------------------')
      call helpme('$DOM-LABEL*')
      CALL LABLA(CHIN(5),I002)
      IF (I002.EQ.0) I002=1
      CH002=' Enter [ "?" | CR | Label (prec  smooth) ] <'// &
      CHIN(5)(1:I002)//'>? '
      CALL PUST (6,CH002)
      READ (UNIT=IFNR,FMT='(A500)') SYREC
      END IF
      IF (SYREC.eq.'?') THEN
        CALL helpme('$DOM-LABEL')
        GOTO 14
      END IF
      IF (SYREC.EQ.' ') THEN
        SYREC=CHIN(5)
      ELSE
        CHIN(5)=SYREC
      END IF
      IF (SIMU) THEN
      LABLINE=SYREC
      CALL LABLA(SCRFILE,I)
!-----batch defined in THERIAK.INI
      if(batch.eq.1) then
         line='guzzler '//SCRFILE(1:I)//ext(plt)(1:4)//'  '// &
         SCRFILE(1:I)//ext(cln)(1:4)//'  '//SCRFILE(1:I)//ext(rxn)
         call PUST(job,line)
         line='explot  '//SCRFILE(1:I)//ext(cln)(1:4)//'  '// &
         SCRFILE(1:I)//ext(pst)
         CALL PUST(job,line)
      end if
      CLOSE (UNIT=job)
      END IF
!-----
      CALL GELI(SYREC,FF)
      LABCOD=IDINT(FF)
      STRAT=0
      IF (LABCOD.EQ.-1) STRAT=1
      IF (LABCOD.LT.1.OR.LABCOD.GT.3) LABCOD=1
      CALL GELI(SYREC,PREC)
      CALL GELI(SYREC,WPREC)
      CALL GELI(SYREC,FF)
      WSCAN=IDINT(FF)
      WSCAN=WSCAN*2
      IF (WSCAN.LT.1) WSCAN=0
      IF (WSCAN.GT.4) WSCAN=4
!-----jump here for pix (achtung bei pix war UNIT=job nicht geschlossen 
!-----(korrigiert))
   88 CONTINUE
      IF (SIMU) THEN
      CALL LABLA(SCRFILE,I)
!cdcdez09      PIXFNAME='_'//SCRFILE(1:I)//'_pix'//dir
      PIXFNAME='_'//SCRFILE(1:I)//'_pix'
      END IF
      LABLINE=CHIN(5)
!-----store terminal input
      CLOSE (UNIT=log)
!------------------
!     open UNIT=log
!------------------
      j=log
      line=filename(j)(1:fnl(j))//ext(j)
      path=wpath
      akzess=' '
      state='old'
      call openfile(j,ierr)
      if(ierr.ne.0) STOP
!-----

      DO I=1,5
        CALL PUST(log,CHIN(I))
      END DO
      WRITE (log,FMT='(F10.4,1X,F10.2)') TIN,PIN
      CLOSE (UNIT=log)
!=====
      IF (SIMU) THEN
      DO I=2,6
        CALL PUST(spt,CHLINE(I))
      END DO
      CALL PUST(spt,XLINE)
      CALL PUST(spt,YLINE)
      CALL PUST(spt,WASLINE)
      CALL PUST(spt,LABLINE)
      WRITE (UNIT=spt,FMT='(E14.7,2X,E14.7)') TSIM,PSIM
      CALL PUST(spt,PIXFNAME)
      CLOSE (UNIT=spt)
      GOTO 1
      END IF
!
!***************************************************************
!******  END OF INPUT UND TESTS. START CALCULATING *************
!***************************************************************
!-----
      IF (BREITE.LE.0.0D0) BREITE=15.0D0
      IF (HOEHE.LE.0.0D0) HOEHE=15.0D0
!---
      CALL LABLA(CHIN(1),I002)
      NCOMIN=3
      COMINS(1)=KOMMENTAR
      COMINS(2)='domino version: '//vers(1:10)
!      COMINS(3)='database: '//CHIN(1)(1:I002)
      CALL LABLA(DBNAME,I002)
      COMINS(3)='database: '//DBNAME(1:I002)
      IF (PEXCL.GT.0) THEN
      DO I=1,NPHA
       IF (NAME(I)(1:1).EQ.'$') THEN
       NCOMIN=NCOMIN+1
       COMINS(NCOMIN)='excl.: '//NAME(I)(2:)
       IF (NCOMIN.GE.50) GOTO 300
       END IF
      END DO
      END IF
      IF (SEXCL.GT.0) THEN
      DO I=1,NSOL
       IF (EXSOL(I)) THEN
       NCOMIN=NCOMIN+1
       COMINS(NCOMIN)='excl.: '//SOLNAM(I)
       IF (NCOMIN.GE.60) GOTO 300
       END IF
      END DO
      END IF
      DO I=1,NPHA
        IF (LODATA(3,I)) THEN
        NCOMIN=NCOMIN+1
        COMINS(NCOMIN)=ABK(I)
        I1=INDEX(COMINS(NCOMIN),'  ')
        COMINS(NCOMIN)(I1:)=': '//CHDATA(I)
        IF (NCOMIN.GE.60) GOTO 300
        END IF
      END DO
  300 CONTINUE
!---
      IF (TMODE) THEN
       IF (MULTI) GOTO 801
       CALL TESTXY
       GOTO 801
      END IF
!---
      IF (VERGL(SONAM,'pix')) THEN
      CALL DOPIX(IDELX,IDELY,PIXFNAME,MULTI)
      GOTO 801
      END IF
!---
      IF (IDELX.LE.0) IDELX=10
      IF (IDELX.GT.39) IDELX=40
      II=IDINT(DBLE(IDELX)*HOEHE/BREITE)
      IF ((2*II).LT.IDELY.OR.II.GT.(2*IDELY)) IDELY=II
      IF (PREC.LE.0.0D0) PREC=0.005D0
      IF (WPREC.LE.0.0D0) WPREC=0.04D0
      F1=BREITE/(PREC*DBLE(IDELX*3))
      II=IDINT(DLOG(F1)/DLOG(2.0D0))
      LEVMAX=II
      WRITE (6,2020) XTEXT,YTEXT
      WRITE (out,2020) XTEXT,YTEXT
 2020 FORMAT (' X: ',A80/' Y: ',A80)
      WRITE (6,2022) LEVMAX,PREC,WPREC
      WRITE (out,2022) LEVMAX,PREC,WPREC
 2022 FORMAT (' MAXIMUM LEVEL:',I4,'  PREC =',F7.4, &
      '  WPREC =',F7.4)
      XWIDE=XMAX-XMIN
      YHIGH=YMAX-YMIN
      DELX=XWIDE/DBLE(IDELX)
      DELY=YHIGH/DBLE(IDELY)
      YPOSA=(YHIGH/HOEHE)*0.8D0+YMAX
      XPOSB=(XWIDE/BREITE)*1.5D0+XMAX
      XPOSC=XMIN+(XWIDE/BREITE)*15.0D0
      YPOSC=YMIN-(YHIGH/HOEHE)*2.0D0
!---
      IF (MAP.AND.(NCOMIN.LT.60)) THEN
      NCOMIN=NCOMIN+1
!     IF (MOLPH.NE.0) THEN
!     COMINS(NCOMIN)='Isolines: '//NAME(MOLPH)
!     ELSE
!     COMINS(NCOMIN)='Isopleths: '//SOLNAM(MAPPH)
!     IF (MAPEM.EQ.-1) CH='mol'
!     IF (MAPEM.GT.0.AND.MAPEM.LE.NEND(MAPPH)) CH=ABK(EM(MAPPH,MAPEM))
!     IF (MAPEM.GT.NEND(MAPPH)) CH=SIEL(MAPPH,MAPEM-NEND(MAPPH))
!     I1=INDEX(COMINS(NCOMIN),'  ')
!     COMINS(NCOMIN)(I1+1:)=CH
!     END IF
!
      COMINS(NCOMIN)='Isolines: '//SONAM
      I1=INDEX(COMINS(NCOMIN),'  ')
      COMINS(NCOMIN)(I1+1:)=KEYWORD
      CALL LABLA(COMINS(NCOMIN),I1)
      COMINS(NCOMIN)(I1+2:)=ISODIVCH
      END IF
!-----
      X1000=0.005D0*DABS(XMAX-XMIN)/BREITE
      F1=-DLOG10(X1000)+0.5D0
      I1=IDNINT(F1)+1
      IF (I1.LT.1) I1=1
      IF (I1.GT.7) I1=7
      WRITE (UNIT=FORX,FMT='(''F10.'',I1)') I1
      Y1000=0.005D0*DABS(YMAX-YMIN)/HOEHE
      F1=-DLOG10(Y1000)+0.5D0
      I1=IDNINT(F1)+1
      IF (I1.LT.1) I1=1
      IF (I1.GT.7) I1=7
      WRITE (UNIT=FORY,FMT='(''F10.'',I1)') I1
      FORXY=FORX//','//FORY
!-----
!===== LEVEL 0
!-----
      LEV=0
      ICASE=0
      NZU=IDELX+1
      DO 750,I2=1,IDELY+1
      F2=YMIN+DBLE(I2-1)*DELY
      DO 760,I1=1,IDELX+1
      F1=XMIN+DBLE(I1-1)*DELX
      CALL SQUEAL(F1,F2,I001)
      XZU(I1)=F1
      YZU(I1)=F2
      ASZU(I1)=I001
      AGIT(I1,I2)=I001
  760 CONTINUE
      CALL COOL(LEV,ICASE)
  750 CONTINUE
      ICASE=1
      NZU=IDELY+1
      DO 770,I1=1,IDELX+1
      F1=XMIN+DBLE(I1-1)*DELX
      DO 780,I2=1,IDELY+1
      F2=YMIN+DBLE(I2-1)*DELY
      XZU(I2)=F1
      YZU(I2)=F2
      ASZU(I2)=AGIT(I1,I2)
  780 CONTINUE
      CALL COOL(LEV,ICASE)
  770 CONTINUE
      CALL DOMINO
!------------------
!     open UNIT=plt
!------------------
      j=plt
      line=COPLOT
      path=wpath
      akzess=' '
      state=' '
      call openfile(j,ierr)
      if(ierr.ne.0) STOP
!----
      CALL PPLOIG
      CLOSE (UNIT=plt)
!=====
!----- HIGHER LEVELS
!=====
      DO 800,LEVCUR=1,LEVMAX
   80 MORE=.FALSE.
      CHCOM=' '
      DO 850,IL=1,NLIN
!-----
!----- CHECK OPEN ENDS
      NL1=LEVEL(1,FIRST(IL))
      NL2=LEVEL(2,LAST(IL))
      I1=IX(1,FIRST(IL))
      I2=IY(1,FIRST(IL))
      I3=IX(2,LAST(IL))
      I4=IY(2,LAST(IL))
      IF (NL1.EQ.NL2.AND.I1.EQ.I3.AND.I2.EQ.I4) GOTO 360
      IF (NL1.LT.LEVCUR) THEN
      IRE=(2**NL1)*IDELX+1
      IOB=(2**NL1)*IDELY+1
      IF (I1.NE.0.AND.I1.NE.IRE.AND.I2.NE.0.AND.I2.NE.IOB) THEN
      LREF=NL1
      IXREF=I1
      IYREF=I2
      MORE=.TRUE.
      WRITE (UNIT=CHCOM,FMT=4000) IL,REANR(FIRST(IL))
 4000 FORMAT('LINE ',I5,'   REACTION',I5,': OPEN END')
      GOTO 851
      END IF
      END IF
      IF (NL2.LT.LEVCUR) THEN
      IRE=(2**NL2)*IDELX+1
      IOB=(2**NL2)*IDELY+1
      IF (I3.NE.0.AND.I3.NE.IRE.AND.I4.NE.0.AND.I4.NE.IOB) THEN
      LREF=NL2
      IXREF=I3
      IYREF=I4
      MORE=.TRUE.
      WRITE (UNIT=CHCOM,FMT=4002) IL,REANR(FIRST(IL))
 4002 FORMAT('LINE ',I5,'   REACTION',I5,': OPEN END')
      GOTO 851
      END IF
      END IF
  360 CONTINUE
!-----
!----- CHECK LINES WITH ONLY TWO POINTS
      I1=LAST(IL)-FIRST(IL)
      IF (I1.EQ.1.AND.LEVEL(2,FIRST(IL)).LT.LEVCUR) THEN
      IRE=(2**NL2)*IDELX+1
      IOB=(2**NL2)*IDELY+1
      LREF=LEVEL(2,FIRST(IL))
      IXREF=IX(2,FIRST(IL))
      IYREF=IY(2,FIRST(IL))
      IF (IXREF.GT.0.AND.IXREF.LT.IRE.AND.IYREF.GT.0 &
      .AND.IYREF.LT.IOB) THEN
      MORE=.TRUE.
      WRITE (UNIT=CHCOM,FMT=4004) IL,REANR(FIRST(IL))
 4004 FORMAT('LINE ',I5,'   REACTION',I5,': TWO POINTS')
      GOTO 851
      END IF
      END IF
!-----
!----- CHECK FOR BUMPS
      DO 820,IP=FIRST(IL),LAST(IL)-2
      FX1=(XPUN(IP)-XMIN)*BREITE/XWIDE
      FY1=(YPUN(IP)-YMIN)*HOEHE/YHIGH
      FX2=(XPUN(IP+1)-XMIN)*BREITE/XWIDE
      FY2=(YPUN(IP+1)-YMIN)*HOEHE/YHIGH
      FX3=(XPUN(IP+2)-XMIN)*BREITE/XWIDE
      FY3=(YPUN(IP+2)-YMIN)*HOEHE/YHIGH
      F1=(FX1-FX2)**2+(FY1-FY2)**2
      F2=(FX2-FX3)**2+(FY2-FY3)**2
      IF (F1.NE.0.0D0.AND.F2.NE.0.0D0) THEN
      F3=(FX1-FX3)**2+(FY1-FY3)**2
      F4=4.0D0*(WPREC**2)
      CPREC=(F4-F3)/(F4+F3)
      CWINK=(F1+F2-F3)/(2.0D0*DSQRT(F1)*DSQRT(F2))
      ELSE
      CPREC=1.0D0
      CWINK=-1.0D0
      END IF
      IF (CWINK.GT.CPREC) THEN
      I1=1
      IF (F2.GT.F1) I1=2
      IF (LEVEL(I1,IP+1).LT.LEVCUR) THEN
      LREF=LEVEL(I1,IP+1)
      IXREF=IX(I1,IP+1)
      IYREF=IY(I1,IP+1)
      IRE=(2**LREF)*IDELX+1
      IOB=(2**LREF)*IDELY+1
      IF (IXREF.GT.0.AND.IXREF.LT.IRE.AND.IYREF.GT.0 &
      .AND.IYREF.LT.IOB) THEN
      MORE=.TRUE.
      WRITE (UNIT=CHCOM,FMT=4006) IL,REANR(FIRST(IL))
 4006 FORMAT('LINE ',I5,'   REACTION',I5,': BUMP')
      GOTO 851
      END IF
      END IF
      END IF
  820 CONTINUE
!-----
  850 CONTINUE
!=====
!----- REFINE THE CALCULATION
  851 CONTINUE
      IF (MORE) THEN
      WRITE (6,4008) LREF,IXREF,IYREF,CHCOM
      WRITE (out,4008) LREF,IXREF,IYREF,CHCOM
 4008 FORMAT (' LEVEL:',I3,'   SQUARE: (',I3,' ,',I3,' ) ',A50)
      LEV=LREF+1
      DO IP=1,NPUN
        DO II=1,2
          IF (LEVEL(II,IP).EQ.LREF.AND.IX(II,IP).EQ.IXREF &
          .AND.IY(II,IP).EQ.IYREF) THEN
            LEVEL(II,IP)=LEV
            I1=IX(II,IP)
            F1=XMIN+(DBLE(I1)-0.5D0)*DELX/DBLE(2**LREF)
            IX(II,IP)=2*I1
            IF (XPUN(IP).LT.F1) IX(II,IP)=IX(II,IP)-1
            I1=IY(II,IP)
            F1=YMIN+(DBLE(I1)-0.5D0)*DELY/DBLE(2**LREF)
            IY(II,IP)=2*I1
            IF (YPUN(IP).LT.F1) IY(II,IP)=IY(II,IP)-1
          END IF
        END DO
      END DO
!-----
      FF=DBLE(2**LREF)
      F1=XMIN+(DBLE(IXREF)-0.5D0)*DELX/FF
      F2=YMIN+(DBLE(IYREF)-0.5D0)*DELY/FF
!cdcJun2011
      IF (F1.LT.XMIN.OR.F1.GT.XMAX.OR.F2.LT.YMIN.OR.F2.GT.YMAX) THEN
      WRITE (6,4010)
      WRITE (out,4010)
 4010 FORMAT (' ---> point outside, skip')
      GOTO 80
      END IF
!
      CALL SQUEAL(F1,F2,IA)
      NZU=3
      ASZU(1)=0
      ASZU(2)=IA
      ASZU(3)=0
      ICASE=0
      XZU(1)=XMIN+DBLE(IXREF-1)*DELX/FF
      YZU(1)=F2
      XZU(2)=F1
      YZU(2)=F2
      XZU(3)=XMIN+DBLE(IXREF)*DELX/FF
      YZU(3)=F2
      CALL COOL(LEV,ICASE)
!-----
      NZU=3
      ASZU(1)=0
      ASZU(2)=IA
      ASZU(3)=0
      ICASE=1
      XZU(1)=F1
      YZU(1)=YMIN+DBLE(IYREF-1)*DELY/FF
      XZU(2)=F1
      YZU(2)=F2
      XZU(3)=F1
      YZU(3)=YMIN+DBLE(IYREF)*DELY/FF
      CALL COOL(LEV,ICASE)
!-----
      CALL DOMINO
      GOTO 80
      END IF
!=====
!------------------
!     open UNIT=plt
!------------------
      j=plt
      line=COPLOT
      path=wpath
      akzess=' '
      state=' '
      call openfile(j,ierr)
      CALL PPLOIG
      CLOSE (UNIT=plt)
!=====
  800 CONTINUE
!=====
!----- END HIGHER LEVELS
!=====
  801 CONTINUE
      CALL CPUTIME(ZEITSTRING)
      CALL LABLA(ZEITSTRING,I001)
      WRITE (scr,4020) ZEITSTRING(1:I001)
      WRITE (out,4020) ZEITSTRING(1:I001)
 4020 FORMAT (' exit DOMINO',/,1X,A)
!-----
      END
!-----
!******************************
      SUBROUTINE COOL(LEV,ICASE)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!
!-----END OF COMMON VARIABLES
!----
      LOGICAL(4) MORE
      REAL(8) F1,F2,F3,F4,DIST,DELTA,FF
      INTEGER(4) I,I1,IC,IR,II,LEV,ICASE,NCOL,IRF,NOR, &
      IIX,IIY
!----
      IIX=-ICASE
      IIY=ICASE-1
      DO 500,I=1,NZU
        IF (ASZU(I).EQ.0) THEN
        F1=XZU(I)
        F2=YZU(I)
        I1=0
        CALL SQUEAL(F1,F2,I1)
        ASZU(I)=I1
        END IF
  500 CONTINUE
      NCOL=NZU
!-----
!cdcFeb2011
      DIST=10.0D0
      IC=1
   10 IF (IC.GE.NCOL) GOTO 888
      IF (NCOL.GE.MAXZUG) THEN
      WRITE (6,FMT='('' ZUG TOO SMALL'')')
      WRITE (out,FMT='('' ZUG TOO SMALL'')')
      STOP
!     GOTO 888
      END IF
!cdcFeb2011
!      IF (ASZU(IC).NE.ASZU(IC+1)) THEN
!cdcJun2011 ist zwar schneller, aber ueberspringt ein paar Zicks und Zacks
!           TEXTA ist nur die Isoplethenbeschriftung
!      IF &
!      (TEXTA(ASZU(IC)).NE.TEXTA(ASZU(IC+1)).OR.DIST.GT.(PTDIST)) &
!      THEN
      IF &
      (ASZU(IC).NE.ASZU(IC+1).OR.DIST.GT.(PTDIST)) &
      THEN
      F1=(XZU(IC)-XMIN)*BREITE/XWIDE
      F2=(XZU(IC+1)-XMIN)*BREITE/XWIDE
      F3=(YZU(IC)-YMIN)*HOEHE/YHIGH
      F4=(YZU(IC+1)-YMIN)*HOEHE/YHIGH
      DIST=DSQRT((F1-F2)**2+(F3-F4)**2)
      IF (DIST.GT.PREC) THEN
!+++++
      F1=(XZU(IC+1)+XZU(IC))/2.0D0
      F2=(YZU(IC+1)+YZU(IC))/2.0D0
      CALL SQUEAL(F1,F2,I1)
      DO I=NCOL,IC+1,-1
        XZU(I+1)=XZU(I)
        YZU(I+1)=YZU(I)
        ASZU(I+1)=ASZU(I)
      END DO
      XZU(IC+1)=F1
      YZU(IC+1)=F2
      ASZU(IC+1)=I1
      NCOL=NCOL+1
!+++++
      ELSE
      IC=IC+1
      DIST=10.0D0
      END IF
      ELSE
      IC=IC+1
      DIST=10.0D0
      END IF
      GOTO 10
  888 CONTINUE
!=====
!=====
      IRF=0
      DO 700,I=1,NCOL-1
!+++++
!      IF (I.LE.NCOL-2) THEN
!      IF (ASZU(I).NE.ASZU(I+1).AND.ASZU(I+1).NE.ASZU(I+2)) THEN
!      ASZU(I+1)=ASZU(I)
!      END IF
!      END IF
!+++++
!cdcmar08 eher nicht
!      IF (ASZU(I).NE.ASZU(I+1)) THEN
      IF (TEXTA(ASZU(I)).NE.TEXTA(ASZU(I+1))) THEN
      MORE=.TRUE.
      DO 750,IR=1,NREAC
      IF ((ASZU(I).EQ.ASS1(IR).AND.ASZU(I+1).EQ.ASS2(IR)) &
      .OR.(ASZU(I).EQ.ASS2(IR).AND.ASZU(I+1).EQ.ASS1(IR))) THEN
      MORE=.FALSE.
      NOR=IR
      GOTO 200
      END IF
  750 CONTINUE
  200 CONTINUE
!-----
      IF (MORE) THEN
      IF (NREAC.GE.MAXREA) THEN
      WRITE (UNIT=6,FMT='('' TOO MANY REACTIONS'')')
      WRITE (UNIT=out,FMT='('' TOO MANY REACTIONS'')')
      STOP
!     GOTO 698
      END IF
      NREAC=NREAC+1
      ASS1(NREAC)=ASZU(I)
      ASS2(NREAC)=ASZU(I+1)
      XA1(NREAC)=XZU(I)
      YA1(NREAC)=YZU(I)
      XA2(NREAC)=XZU(I+1)
      YA2(NREAC)=YZU(I+1)
      NOR=NREAC
      WRITE (6,1000) NREAC,ASS1(NREAC),ASS2(NREAC)
      WRITE (out,1000) NREAC,ASS1(NREAC),ASS2(NREAC)
 1000 FORMAT (' REACTION:',I4,I4,'  =',I4)
      WRITE (6,1002) XA1(NREAC),YA1(NREAC),XA2(NREAC),YA2(NREAC)
      WRITE (out,1002) XA1(NREAC),YA1(NREAC),XA2(NREAC),YA2(NREAC)
 1002 FORMAT (' position:',2(5X,1PE12.5,2X,1PE12.5))
      IR=NREAC
      CALL DECREAC(IR)
      END IF
!-----
      IRF=IRF+1
      IF (NPUN.GE.MAXPUN) THEN
      WRITE (UNIT=6,FMT='('' TOO MANY POINTS'')')
      WRITE (UNIT=out,FMT='('' TOO MANY POINTS'')')
      STOP
      END IF
      NPUN=NPUN+1
      XPUN(NPUN)=(XZU(I)+XZU(I+1))/2.0D0
      YPUN(NPUN)=(YZU(I)+YZU(I+1))/2.0D0
      REANR(NPUN)=NOR
      IF (ASZU(I).EQ.ASS1(NOR)) THEN
      REASENS(NPUN)=1
      ELSE
      REASENS(NPUN)=2
      END IF
      IF (ICASE.NE.0) REASENS(NPUN)=-REASENS(NPUN)
      LEVEL(1,NPUN)=LEV
      LEVEL(2,NPUN)=LEV
      FF=DBLE(2**LEV)
      DELTA=(PREC*XWIDE/BREITE)/100.0D0
      IX(2,NPUN)=IDINT((XPUN(NPUN)+DELTA-XMIN)*FF/DELX)+1
      DELTA=(PREC*YHIGH/HOEHE)/100.0D0
      IY(2,NPUN)=IDINT((YPUN(NPUN)+DELTA-YMIN)*FF/DELY)+1
      IX(1,NPUN)=IX(2,NPUN)+IIX
      IY(1,NPUN)=IY(2,NPUN)+IIY
      END IF
!  698 CONTINUE
  700 CONTINUE
!-----
      IF (ICASE.EQ.0) THEN
      WRITE (6,1012) NCTT,NCTTOT,YZU(1), &
      IRF,(REANR(NPUN-IRF+II),II=1,IRF)
      WRITE (out,1012) NCTT,NCTTOT,YZU(1), &
      IRF,(REANR(NPUN-IRF+II),II=1,IRF)
 1012 FORMAT (1X,2I6,1X,' Y =',F20.4,' ,',3X,I4,' REACTIONS:',500I5)
      ELSE
      WRITE (6,1014) NCTT,NCTTOT,XZU(1), &
      IRF,(REANR(NPUN-IRF+II),II=1,IRF)
      WRITE (out,1014) NCTT,NCTTOT,XZU(1), &
      IRF,(REANR(NPUN-IRF+II),II=1,IRF)
 1014 FORMAT (1X,2I6,1X,' X =',F20.4,' ,',3X,I4,' REACTIONS:',500I5)
      END IF
      NCTT=0
      CALL FLUSH(6)
      RETURN
      END
!-----
!******************************
      SUBROUTINE DECREAC(IR)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!
      REAL(8) F1,F2,FF,XREA(2,COMAX)
      INTEGER(4) IR,I,II,I1,I2,I12,LT,NNR(2),PRIM(10),PZ, &
      K,JK,IP,IK,MULI(2*COMAX),KGV,II1,NNA(2),NA(2), &
      ACODE(2,COMAX),PFACS(2*COMAX,10),I0
      CHARACTER(32) CHREA(2,COMAX),CH1
      CHARACTER(80) PHIN(2)
      DATA PRIM/2,3,5,7,11,13,17,19,23,29/
!=====
      TEXRE(1,IR)=' '
      TEXRE(2,IR)=' '
      IF (LABCOD.EQ.1) GOTO 999
      IF (LABCOD.EQ.3) GOTO 777
      IF (ASS1(IR).EQ.0.OR.ASS2(IR).EQ.0) GOTO 999
!=====
!     FIND PHASE-IN / PHASE-OUT
!
      NA(1)=ASS1(IR)
      NA(2)=ASS2(IR)
      NNA(1)=NPA(NA(1))
      NNA(2)=NPA(NA(2))
      DO I12=1,2
        DO I=1,NNA(I12)
          ACODE(I12,I)=1
        END DO
      END DO
!-
      DO I=1,NNA(1)
        DO II=1,NNA(2)
          IF (ACODE(2,II).EQ.1.AND.NUMA(NA(1),I).EQ. &
          NUMA(NA(2),II)) THEN
          ACODE(1,I)=0
          ACODE(2,II)=0
          GOTO 121
          END IF
        END DO
  121   CONTINUE
      END DO
      DO 130,I12=1,2
      LT=2
      PHIN(I12)='('
      DO 140,I=1,NNA(I12)
!+++
      IF (ACODE(I12,I).EQ.1) THEN
      II=NUMA(NA(I12),I)
      IF (II.LE.NPHA) THEN
      CH1=ABK(II)
      ELSE
      IF (II.LE.(NPHA+NSOL)) THEN
      CH1=SOLNAM(II-NPHA)
      ELSE
      CH1=MAPTEX(II-NPHA-NSOL)(1:32)
      END IF
      END IF
!+++
      I1=INDEX(CH1,'  ')
      PHIN(I12)(LT:LT+I1-1)=CH1(1:I1-1)
      LT=LT+I1
      END IF
  140 CONTINUE
      IF (LT.EQ.2) LT=3
      PHIN(I12)(LT-1:LT-1)=')'
  130 CONTINUE
      I1=INDEX(PHIN(1),'  ')
      FORMUL=PHIN(1)(1:I1)//'= '//PHIN(2)
      CALL LABLA(FORMUL,I0)
      WRITE (6,2050) FORMUL(1:I0)
      WRITE (out,2050) FORMUL(1:I0)
 2050 FORMAT (' REACTION: ',A)
      TEXRE(1,IR)=PHIN(1)
      TEXRE(2,IR)=PHIN(2)
      GOTO 999
!=====
!     ENTWURSCHTEL REACTION
!
  777 DO 200,I12=1,2
      IF (I12.EQ.1) THEN
      F1=XA1(IR)
      F2=YA1(IR)
      CALL SQUEAL(F1,F2,I)
!     CALL PRTSTR(1,NUN2)
      IF (I.NE.ASS1(IR)) THEN
      WRITE (UNIT=6,FMT='('' HUCH'')')
      WRITE (UNIT=out,FMT='('' HUCH'')')
      GOTO 999
      END IF
      ELSE
      F1=XA2(IR)
      F2=YA2(IR)
      CALL SQUEAL(F1,F2,I)
!     CALL PRTSTR(1,NUN2)
      IF (I.NE.ASS2(IR)) THEN
      WRITE (UNIT=6,FMT='('' HUCH'')')
      WRITE (UNIT=out,FMT='('' HUCH'')')
      GOTO 999
      END IF
      END IF
!-----
      IF (MAP) NUN2=NUN2-1
      K=0
      NNR(I12)=0
      DO 250,IP=1,NUN2
      II=NUMMER(IP)
      CHREA(I12,K+1)=' '
!-
      IF (AFIX1.NE.0) THEN
      IF (II.EQ.OFIX1) II=AFIX1
      IF (II.EQ.(AFIX1+1)) THEN
      II=AFIX1
      NN(IP)=-NN(IP)
      END IF
      END IF
      IF (AFIX2.NE.0) THEN
      IF (II.EQ.OFIX2) II=AFIX2
      IF (II.EQ.(AFIX2+1)) THEN
      II=AFIX2
      NN(IP)=-NN(IP)
      END IF
      END IF
!+++
      IF (II.NE.0) THEN
      CH1=ABK(II)
      I1=0
      DO 260,JK=1,K
      IF (CHREA(I12,JK).EQ.CH1) THEN
      I1=JK
      GOTO 261
      END IF
  260 CONTINUE
  261 IF (I1.EQ.0) THEN
      K=K+1
      XREA(I12,K)=NN(IP)
      CHREA(I12,K)=CH1
      ELSE
      XREA(I12,I1)=XREA(I12,I1)+NN(IP)
      END IF
!+++
      ELSE
      DO 270,IK=1,NEND(EMCODE(IP))
      I1=0
      CH1=ABK(EM(EMCODE(IP),IK))
      DO 280,JK=1,K
      IF (CHREA(I12,JK).EQ.CH1) THEN
      I1=JK
      GOTO 281
      END IF
  280 CONTINUE
  281 IF (I1.EQ.0) THEN
      K=K+1
      XREA(I12,K)=NN(IP)*XEM(IP,IK)
      CHREA(I12,K)=CH1
      ELSE
      XREA(I12,I1)=XREA(I12,I1)+NN(IP)*XEM(IP,IK)
      END IF
  270 CONTINUE
      END IF
!+++
      NNR(I12)=K
!-----
  250 CONTINUE
!-----
  200 CONTINUE
!=====
!     SUBSTRACT AND DIVIDE BY MINIMUM
!
      DO 300,I=1,NNR(1)
      DO 305,II=1,NNR(2)
      IF (CHREA(1,I).EQ.CHREA(2,II)) THEN
      IF (XREA(1,I).GT.XREA(2,II)) THEN
      XREA(1,I)=XREA(1,I)-XREA(2,II)
      IF (XREA(1,I).LE.1D-8) XREA(1,I)=0.0D0
      XREA(2,II)=0.0D0
      ELSE
      XREA(2,II)=XREA(2,II)-XREA(1,I)
      IF (XREA(2,II).LE.1D-8) XREA(2,II)=0.0D0
      XREA(1,I)=0.0D0
      END IF
      END IF
  305 CONTINUE
  300 CONTINUE
!-----
      F1=1D20
      DO I12=1,2
        DO I=1,NNR(I12)
          IF (XREA(I12,I).GT.0.0D0.AND.XREA(I12,I).LT.F1) &
           F1=XREA(I12,I)
        END DO
      END DO
      IF (F1.LT.1D20) THEN
      DO I12=1,2
        DO I=1,NNR(I12)
          XREA(I12,I)=XREA(I12,I)/F1
        END DO
      END DO
      END IF
!=====
!     TRY SIMPLE REACTION
!
      IK=0
      DO 400,I12=1,2
      DO 450,I=1,NNR(I12)
      IK=IK+1
      MULI(IK)=0
      DO 460,I1=1,100
      F1=XREA(I12,I)*DBLE(I1)
      II=IDINT(F1+0.5D0)
      FF=DBLE(II)
      IF (DABS(FF-F1).LT.1D-8) THEN
      MULI(IK)=I1
      GOTO 461
      END IF
  460 CONTINUE
  461 CONTINUE
      IF (MULI(IK).EQ.0) GOTO 888
  450 CONTINUE
  400 CONTINUE
!-----
!     FIND KGV = I1*I2 / GGT
!
      DO 500,I=1,(NNR(1)+NNR(2))
      DO IP=1,10
        PFACS(I,IP)=0
      END DO
      IP=1
      I1=MULI(I)
  510 IF (IP.GT.10) GOTO 511
      PZ=PRIM(IP)
      IF (PZ.GT.I1) GOTO 511
      F1=DBLE(I1)/DBLE(PZ)
      II1=IDINT(F1)*PZ
      IF (II1.EQ.I1) THEN
      PFACS(I,IP)=PFACS(I,IP)+1
      I1=I1/PZ
      ELSE
      IP=IP+1
      END IF
      GOTO 510
  511 CONTINUE
  500 CONTINUE
      KGV=1
      DO 502,IP=1,10
      I1=PFACS(1,IP)
      DO 503,I=2,(NNR(1)+NNR(2))
      IF (PFACS(I,IP).GT.I1) I1=PFACS(I,IP)
  503 CONTINUE
      IF (I1.GT.0) KGV=KGV*PRIM(IP)**I1
  502 CONTINUE
!-----
!     WRITE THE LABELS
!
      DO 600,I12=1,2
      LT=0
      DO 650,I=1,NNR(I12)
      IF (XREA(I12,I).LT.1D-8) GOTO 649
      XREA(I12,I)=XREA(I12,I)*DBLE(KGV)
      IK=IDINT(XREA(I12,I)+0.5D0)
      IF (IK.NE.1) THEN
      WRITE (UNIT=CH1,FMT='(I8)') IK
      CALL FIBLA(CH1,I1)
      TEXRE(I12,IR)(LT+1:LT+9-I1)=CH1(I1:8)
      LT=LT+10-I1
      END IF
      I1=INDEX(CHREA(I12,I),'  ')
      TEXRE(I12,IR)(LT+1:LT+I1+3)=CHREA(I12,I)(1:I1-1)//' + '
      LT=LT+I1+2
  649 CONTINUE
  650 CONTINUE
      IF (LT.GT.1) TEXRE(I12,IR)(LT-1:LT-1)=' '
  600 CONTINUE
      GOTO 999
!-----
  888 WRITE (6,1010)
      WRITE (out,1010)
 1010 FORMAT (' MORE THAN ONE REACTION')
      DO 700,I12=1,2
      TEXRE(I12,IR)=' '
      LT=1
      DO 750,I=1,NNR(I12)
      IF (XREA(I12,I).GT.1D-10) THEN
      I1=INDEX(CHREA(I12,I),'  ')
      I2=LT+I1-1
      TEXRE(I12,IR)(LT:I2)=CHREA(I12,I)(1:I1-1)
      LT=LT+I1
      F1=DBLE(3-2*I12)*XREA(I12,I)
      WRITE (6,1000) F1,CHREA(I12,I)
      WRITE (out,1000) F1,CHREA(I12,I)
 1000 FORMAT ('   ',F20.8,3X,A8)
      END IF
  750 CONTINUE
  700 CONTINUE
!-----
  999 IF (TEXRE(1,IR).EQ.' '.OR.TEXRE(2,IR).EQ.' ') &
       THEN
      WRITE (6,1020)
      WRITE (out,1020)
 1020 FORMAT (' LABELED WITH ASSEMBLAGES')
      TEXRE(1,IR)=TEXTA(ASS1(IR))
      TEXRE(2,IR)=TEXTA(ASS2(IR))
      IF (ASS1(IR).EQ.0.OR.ASS2(IR).EQ.0) THEN
      TEXRE(1,IR)=' '
      TEXRE(2,IR)=' '
      END IF
      END IF
      I1=INDEX(TEXRE(1,IR),'  ')
      FORMUL=TEXRE(1,IR)(1:I1)//'= '//TEXRE(2,IR)
      CALL LABLA(FORMUL,I0)
      WRITE (6,2000) FORMUL(1:I0)
      WRITE (out,2000) FORMUL(1:I0)
 2000 FORMAT (' REACTION: ',A)
      RETURN
      END
!-----
!******************************
      SUBROUTINE DOMINO
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!
!-----END OF COMMON VARIABLES
!----
      REAL(8) F1,F2
      INTEGER(4) I,I1,I2,LL,LA,IHIT,INSERT,ITOC, &
      I001,I002,I003,I004,I005,I006,I007
      LOGICAL(4) TAUSCH
!----
      TAUSCH=.FALSE.
      IF (NPUN.EQ.0) THEN
      NLIN=0
      RETURN
      END IF
      NLIN=1
      FIRST(1)=1
      LAST(1)=1
      LL=1
      LA=1
   10 IF (LL.GE.NPUN) GOTO 888
      IHIT=0
      DO 500,I=LL+1,NPUN
      IF (REANR(I).EQ.REANR(LL)) THEN
      TAUSCH=.FALSE.
      DO 600,I1=1,2
      IF (LEVEL(I1,I).EQ.LEVEL(2,LL).AND.IX(I1,I).EQ.IX(2,LL) &
      .AND.IY(I1,I).EQ.IY(2,LL)) THEN
      IHIT=I
      ITOC=LL
      INSERT=LL+1
      IF (I1.EQ.2) TAUSCH=.TRUE.
      GOTO 499
      END IF
  600 CONTINUE
      DO 650,I1=1,2
      IF (LEVEL(I1,I).EQ.LEVEL(1,LA).AND.IX(I1,I).EQ.IX(1,LA) &
      .AND.IY(I1,I).EQ.IY(1,LA)) THEN
      IHIT=I
      ITOC=LA
      INSERT=LA
      IF (I1.EQ.1) TAUSCH=.TRUE.
      GOTO 499
      END IF
  650 CONTINUE
      END IF
  499 IF (IHIT.NE.0) THEN
      IF (REASENS(IHIT).EQ.REASENS(ITOC)) GOTO 100
      F1=(XPUN(IHIT)-XPUN(ITOC))
      F2=(YPUN(IHIT)-YPUN(ITOC))
      IF (F1.NE.0.0D0.AND.F2.NE.0.0D0) THEN
      F1=F1/F2
      IF (F1.GT.0.0D0.AND.REASENS(IHIT).NE.-REASENS(ITOC)) GOTO 100
      IF (F1.LT.0.0D0.AND.REASENS(IHIT).EQ.-REASENS(ITOC)) GOTO 100
      END IF
      END IF
  500 CONTINUE
!=====
  100 IF (IHIT.NE.0) THEN
      I1=1
      I2=2
      IF (TAUSCH) THEN
      I1=2
      I2=1
      END IF
      IF (TAUSCH.OR.INSERT.NE.IHIT) THEN
      F1=XPUN(IHIT)
      F2=YPUN(IHIT)
      I007=REASENS(IHIT)
      I001=LEVEL(I1,IHIT)
      I002=LEVEL(I2,IHIT)
      I003=IX(I1,IHIT)
      I004=IX(I2,IHIT)
      I005=IY(I1,IHIT)
      I006=IY(I2,IHIT)
!-----
      DO I=IHIT,INSERT+1,-1
        XPUN(I)=XPUN(I-1)
        YPUN(I)=YPUN(I-1)
        REASENS(I)=REASENS(I-1)
        REANR(I)=REANR(I-1)
        LEVEL(1,I)=LEVEL(1,I-1)
        LEVEL(2,I)=LEVEL(2,I-1)
        IX(1,I)=IX(1,I-1)
        IX(2,I)=IX(2,I-1)
        IY(1,I)=IY(1,I-1)
        IY(2,I)=IY(2,I-1)
      END DO
!-----
      XPUN(INSERT)=F1
      YPUN(INSERT)=F2
      REASENS(INSERT)=I007
      REANR(INSERT)=REANR(LL)
      LEVEL(1,INSERT)=I001
      LEVEL(2,INSERT)=I002
      IX(1,INSERT)=I003
      IX(2,INSERT)=I004
      IY(1,INSERT)=I005
      IY(2,INSERT)=I006
      END IF
!=====
      ELSE
!=====
      NLIN=NLIN+1
      IF (NLIN.GT.MAXLIN) THEN
      WRITE (UNIT=6,FMT='('' TOO MANY LINES'')')
      WRITE (UNIT=out,FMT='('' TOO MANY LINES'')')
      STOP
      END IF
      FIRST(NLIN)=LL+1
      LA=LL+1
      END IF
!=====
      LAST(NLIN)=LL+1
      LL=LL+1
      GOTO 10
  888 CONTINUE
      WRITE (6,1000) NLIN,NPUN,NMAXMAX
      WRITE (out,1000) NLIN,NPUN,NMAXMAX
 1000 FORMAT (' NUMBER OF LINES:',I5,4X,'NUMBER OF POINTS:',I6, &
      4X,'max.ph.:',I5)
      CALL FLUSH(6)
!=====
      RETURN
      END
!-----
!******************************
      SUBROUTINE SQUEAL(XZ,YZ,NOA)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!
!-----END OF COMMON VARIABLES
!----
      LOGICAL(4) MORE,STRATCOD
      REAL(8) XZ,YZ,FF,XMAP(EMAX),FA,FB,FC,F001
      INTEGER(4) I,II,I1,I2,IA,IP,IQ,IK,IE,NOA,LT,NPRO,MAPANZ,IS
      CHARACTER(8) KNAME
      CHARACTER(32) SHORTY
      CHARACTER(500) CH001
!----
      IF (YVAR.EQ.'BIN') THEN
      DO I=1,NUN
        BULK(I)=0.0D0
      END DO
      END IF
      IF (XVAR.EQ.'BIN') THEN
      FB=XZ+(X1000*1.437D0)
      IF (FB.GE.1.0D0) FB=XZ-X1000*1.437D0
      FA=1.0D0-FB
      DO I=1,NUN
        II=CHMCOD(I)
        BULK(I)=FA*CHEX(1,II)+FB*CHEX(2,II)
      END DO
      END IF
      IF (YVAR.EQ.'BIN') THEN
      FB=YZ+(Y1000*1.437D0)
      IF (FB.GE.1.0D0) FB=YZ-Y1000*1.437D0
      FA=1.0D0-FB
      DO I=1,NUN
        II=CHMCOD(I)
        BULK(I)=BULK(I)+FA*CHEX(3,II)+FB*CHEX(4,II)
      END DO
      END IF
!----
      IF (XVAR.EQ.'TER') THEN
      IF ((XZ+YZ).GT.1.0D0) THEN
      NOA=-0
      NUN2=0
      IF (PRTLOG(10)) NROWTBL=NROWTBL+1
      RETURN
      END IF
      FA=1.0D0-XZ-YZ
      FB=XZ+X1000
      FC=YZ+(X1000*1.437D0)
      IF (FA.LT.X1000) FA=X1000
!     IF (FB.LT.X1000) FB=X1000
!     IF (FC.LT.X1000) FC=X1000
      DO I=1,NUN
        II=CHMCOD(I)
        BULK(I)=FA*CHEX(1,II)+FB*CHEX(2,II)+FC*CHEX(3,II)
      END DO
      END IF
!----
!----
      IF (XVAR.EQ.'TC') THEN
      TC=XZ
      T=TC+273.15D0
      END IF
      IF (XVAR.EQ.'TK') THEN
      T=XZ
      TC=T-273.15D0
      END IF
      IF (XVAR.EQ.'P') P=XZ
      IF (XVAR(1:2).EQ.'A(') THEN
      IF (XZ.LE.0.0D0) THEN
!     FF=-500.0D0
      FF=R*DLOG(X1000)
      ELSE
      FF=R*DLOG(XZ)
      END IF
      REDATA(3,AFIX1)=-FF
      REDATA(3,AFIX1+1)=FF
      END IF
      IF (XVAR(1:4).EQ.'LNA(') THEN
      FF=R*XZ
      REDATA(3,AFIX1)=-FF
      REDATA(3,AFIX1+1)=FF
      END IF
      IF (XVAR(1:5).EQ.'LOGA(') THEN
      FF=R*XZ*2.302585093D0
      REDATA(3,AFIX1)=-FF
      REDATA(3,AFIX1+1)=FF
      END IF
      IF (XVAR(1:3).EQ.'MA(') THEN
      FF=XZ/T
      REDATA(3,AFIX1)=-FF
      REDATA(3,AFIX1+1)=FF
      END IF
!-----
      IF (YVAR.EQ.'TC'.OR.YVAR.EQ.'PT'.OR.YVAR.EQ.'THG') THEN
      TC=YZ
      T=TC+273.15D0
      IF (YVAR.EQ.'PT') P=1062.091685D0+2.567123D0*TC &
      +0.01085D0*TC**2
      IF (YVAR.EQ.'THG') P=(TC-25.0D0)*THGR+1.0D0
      END IF
      IF (YVAR.EQ.'TK') THEN
      T=YZ
      TC=T-273.15D0
      END IF
      IF (YVAR.EQ.'P') P=YZ
      IF (YVAR(1:2).EQ.'A(') THEN
      IF (YZ.LE.0.0D0) THEN
      FF=-500.0D0
      ELSE
      FF=R*DLOG(YZ)
      END IF
      REDATA(3,AFIX2)=-FF
      REDATA(3,AFIX2+1)=FF
      END IF
      IF (YVAR(1:4).EQ.'LNA(') THEN
      FF=R*YZ
      REDATA(3,AFIX2)=-FF
      REDATA(3,AFIX2+1)=FF
      END IF
      IF (YVAR(1:5).EQ.'LOGA(') THEN
      FF=R*YZ*2.302585093D0
      REDATA(3,AFIX2)=-FF
      REDATA(3,AFIX2+1)=FF
      END IF
      IF (YVAR(1:3).EQ.'MA(') THEN
      FF=YZ/T
      REDATA(3,AFIX2)=-FF
      REDATA(3,AFIX2+1)=FF
      END IF
!-----
!dC
!     WRITE (6,8450) TC,P
!8450 FORMAT ('TC = ',F15.7,'  P = ',F15.5)
      CALL NURVONPT
      CALL CALSTR
      CALL THERIA
      NCTT=NCTT+1
      NCTTOT=NCTTOT+1
      IF (LOO1.GE.LO1MAX) THEN
      WRITE (6,2000) LOO1,XZ,YZ,GTOT,DISTAMAX
      WRITE (out,2000) LOO1,XZ,YZ,GTOT,DISTAMAX
 2000 FORMAT (/'WARNING: loops reached maximum: ',I6, &
      4X,'X,Y = ',F13.6,2X,F15.6, &
      4X,'G(-) = ',1PE12.5,0P, &
      4X,'dist = ',1PE12.5,0P)
      END IF
!*****
      IF (MAP) THEN
      MAPANZ=0
      IF (MOLPH.EQ.-1000) THEN
      MAPANZ=1
      IF (MAPEM.EQ.-1) XMAP(MAPANZ)=VOLSOL
      IF (MAPEM.EQ.-2) XMAP(MAPANZ)=WTSOL
      F001=VOLSOL
      IF (VOLSOL.EQ.0.0D0) F001=1.0D0
      IF (MAPEM.EQ.-3) XMAP(MAPANZ)=WTSOL/F001
      IF (MAPEM.EQ.-4) XMAP(MAPANZ)=GGTOT
      F001=WTSOL
      IF (WTSOL.EQ.0.0D0) F001=1.0D0
      IF (MAPEM.EQ.-5) XMAP(MAPANZ)=WH2OSOL/F001*100.0D0
      GOTO 150
      END IF
!-----
      DO 590,IP=1,NUN2
      IF (NUMMER(IP).EQ.0.AND.EMCODE(IP).EQ.MAPPH) THEN
      MAPANZ=MAPANZ+1
!-----
      IF (MAPEM.GT.NEND(MAPPH)) THEN
      IQ=MAPEM-NEND(MAPPH)
      XMAP(MAPANZ)=0.0D0
      DO IK=1,NEMQQ(MAPPH,IQ)
        IE=EMQQ(MAPPH,IQ,IK)
        XMAP(MAPANZ)=XMAP(MAPANZ)+EMXX(MAPPH,IQ,IE)*XEM(IP,IE)
      END DO
      ELSE
      IF (MAPEM.GT.0) XMAP(MAPANZ)=XEM(IP,MAPEM)
      IF (MAPEM.EQ.-1) XMAP(MAPANZ)=NN(IP)
      IF (MAPEM.EQ.-2) XMAP(MAPANZ)=VOLPH(IP)
      IF (MAPEM.EQ.-3) XMAP(MAPANZ)=VOLM(IP)
      IF (MAPEM.EQ.-4) XMAP(MAPANZ)=WTPH(IP)
      IF (MAPEM.EQ.-5) XMAP(MAPANZ)=WTM(IP)
      F001=VOLPH(IP)
      IF (F001.EQ.0.0D0) F001=1.0D0
      IF (MAPEM.EQ.-6) XMAP(MAPANZ)=WTPH(IP)/F001
      IF (MAPEM.EQ.-7) XMAP(MAPANZ)=MGFE(IP)
      IF (MAPEM.EQ.-8) XMAP(MAPANZ)=VOLPH(IP)/VOLSOL*100.0D0
      IF (MAPEM.EQ.-10) XMAP(MAPANZ)=SIPFU(IP)
      IF (MAPEM.EQ.-11) XMAP(MAPANZ)=ALPFU(IP)
      END IF
!-----das Folgende gilt nur binaer, muss noch korrigiert werden
!----oder ganz weglassen (ordnung nach prtcal verwenden)
!      IF (MAPANZ.GE.2) THEN
!      DO 591,I=2,MAPANZ
!      IF (XMAP(I).LT.XMAP(I-1)) THEN
!      FF=XMAP(I)
!      XMAP(I)=XMAP(I-1)
!      XMAP(I-1)=FF
!      END IF
!  591 CONTINUE
!      END IF
      END IF
!-----
      IF (NUMMER(IP).EQ.MOLPH.AND.NUMMER(IP).NE.0) THEN
      MAPANZ=1
      IF (MAPEM.EQ.-1) XMAP(MAPANZ)=NN(IP)
      IF (MAPEM.EQ.-2) XMAP(MAPANZ)=VOLPH(IP)
      IF (MAPEM.EQ.-3) XMAP(MAPANZ)=VOLM(IP)
      IF (MAPEM.EQ.-4) XMAP(MAPANZ)=WTPH(IP)
      IF (MAPEM.EQ.-5) XMAP(MAPANZ)=WTM(IP)
      F001=VOLPH(IP)
      IF (F001.EQ.0.0D0) F001=1.0D0
      IF (MAPEM.EQ.-6) XMAP(MAPANZ)=WTPH(IP)/F001
      IF (MAPEM.EQ.-7) XMAP(MAPANZ)=MGFE(IP)
      IF (MAPEM.EQ.-8) XMAP(MAPANZ)=VOLPH(IP)/VOLSOL*100.0D0
      END IF
  590 CONTINUE
!-----
!-----Following loop to find possible G_'s
      IF (MAPPH.GT.0.AND.MAPEM.EQ.-9) THEN
       DO IP=NUN2+1,NMAX
        IF (NUMMER(IP).EQ.0) THEN
         IS=EMCODE(IP)
         IF (IS.EQ.MAPPH.AND.EXSOL(IS).AND.SUGG(IP).EQ.MINISUG(IS)) &
          THEN
          MAPANZ=1
          XMAP(MAPANZ)=G(IP)
         END IF
        END IF
       END DO
      END IF
!-----
      IF (MOLPH.GT.0.AND.MAPEM.EQ.-9) THEN
       DO IP=NUN2+1,NMAX
        IF (NUMMER(IP).NE.0) THEN
         IF (NUMMER(IP).EQ.MOLPH.AND.NAME(NUMMER(IP))(1:1).EQ.'$') &
          THEN
          MAPANZ=1
          XMAP(MAPANZ)=G(IP)
         END IF 
        END IF
       END DO
      END IF
!===  das Folgende ist ein wenig unsauber
  150 CONTINUE
!----- hier ISODIV
      IF (ISODIV.NE.1.0D0.AND.ISODIV.NE.0.0D0) &
          XMAP(MAPNR)=XMAP(MAPNR)/ISODIV
      IF (MAPANZ.LT.MAPNR) THEN
      NPRO=1
      ELSE
      NPRO=2
      DO I=1,NGRENZ
        FF=-GRENZ(I)*1D-8
        IF ((XMAP(MAPNR)-GRENZ(I)).GT.FF) NPRO=NPRO+1
      END DO
      END IF
!-----
      NUN2=NUN2+1
      NUMMER(NUN2)=NPHA+NSOL+NPRO
      EMCODE(NUN2)=0
!cdcmar08
      STPHNAM(NUN2)=MAPTEX(NPRO)
      SHORTNAM(NUN2)=MAPTEX(NPRO)
      END IF
!*****
      MORE=.TRUE.
      DO 500,IA=1,NASS
      IF (NUN2.EQ.NPA(IA)) THEN
      MORE=.FALSE.
      DO 550,IP=1,NUN2
      II=NUMMER(IP)
!-
      IF (AFIX1.NE.0) THEN
      IF (II.EQ.OFIX1) II=AFIX1
      IF (II.EQ.(AFIX1+1)) II=AFIX1
      END IF
      IF (AFIX2.NE.0) THEN
      IF (II.EQ.OFIX2) II=AFIX2
      IF (II.EQ.(AFIX2+1)) II=AFIX2
      END IF
      IF (II.EQ.0) II=NPHA+EMCODE(IP)
!cdcmar08
      IF (II.LE.NPHA) THEN
      SHORTY=ABK(II)
      ELSE
      SHORTY=STPHNAM(IP)
      END IF
!-----
!cdcmar08
!+++++ neue Beschriftung: unterscheidet dominante Endglieder (STRAT=1)
!+++++ alte Beschriftung: (STRAT=0)
!!      IF (NUMA(IA,IP).NE.II.OR.NUMATXT(IA,IP).NE.SHORTY) THEN
!!      IF (NUMA(IA,IP).NE.II) THEN
      IF (STRAT.EQ.1) THEN
      STRATCOD=(NUMA(IA,IP).NE.II.OR.NUMATXT(IA,IP).NE.SHORTY)
      ELSE
      STRATCOD=(NUMA(IA,IP).NE.II)
      END IF
      IF (STRATCOD) THEN
!+++++
      MORE=.TRUE.
      GOTO 100
      END IF
  550 CONTINUE
  100 CONTINUE
      IF (.NOT.MORE) THEN
      NOA=IA
      RETURN
      END IF
      END IF
  500 CONTINUE
!=====
      IF (NASS.GE.MAXASS) THEN
      WRITE (UNIT=6,FMT='('' TOO MANY ASSEMBLAGES'')')
      RETURN
      END IF
      NASS=NASS+1
!#####
!     CALL PRTSTR(1,NUN2)
!#####
!=====
!===== versuch Jul 2011
!      DO IP=1,NUN2
!      IF (STPHNAM(IP)(1:5).EQ.'ClAMP') THEN
!      IS=EMCODE(IP)
!      OPEN (UNIT=60,FILE='seco',STATUS='UNKNOWN',ACCESS='APPEND')
!      WRITE (UNIT=60,FMT='(A16,100(3X,1PE12.5))') &
!      STPHNAM(IP),(XEM(IP,II),II=1,NEND(IS))
!      CLOSE (UNIT=60)
!      END IF
!      END DO
!=====
!=====
      NPA(NASS)=NUN2
      TEXTA(NASS)=' '
      LT=0
      DO 600,IP=1,NUN2
      II=NUMMER(IP)
      IF (AFIX1.NE.0) THEN
      IF (II.EQ.OFIX1) II=AFIX1
      IF (II.EQ.(AFIX1+1)) II=AFIX1
      END IF
      IF (AFIX2.NE.0) THEN
      IF (II.EQ.OFIX2) II=AFIX2
      IF (II.EQ.(AFIX2+1)) II=AFIX2
      END IF
!=====
      IF (II.EQ.0) II=NPHA+EMCODE(IP)
      NUMA(NASS,IP)=II
!=====
      IF (II.LE.NPHA) THEN
      CH=ABK(II)
!cdcmar08
!-----
      NUMATXT(NASS,IP)=ABK(II)
      IF (AFIX1.NE.0) THEN
      IF (II.EQ.AFIX1) CH=' '
      END IF
      IF (AFIX2.NE.0) THEN
      IF (II.EQ.AFIX2) CH=' '
      END IF
!=====
      ELSE
!=====
!cdcmar08
      NUMATXT(NASS,IP)=STPHNAM(IP)
!+++++ neue Beschriftung: unterscheidet dominante Endglieder
      IF (STRAT.EQ.1) THEN
      CH=SHORTNAM(IP)
!+++++ alte Beschriftung: 
      ELSE
      IF (IP.NE.1.AND.EMCODE(IP).EQ.EMCODE(IP-1) &
      .OR.EMCODE(IP).EQ.0) GOTO 599
      I2=1
      DO IK=IP+1,NUN2
        IF (EMCODE(IP).EQ.EMCODE(IK)) I2=I2+1
      END DO
      IF (I2.EQ.1) THEN
      CH=SOLNAM(EMCODE(IP))
      ELSE
      WRITE (UNIT=KNAME,FMT='(I8)') I2
      CALL FIBLA(KNAME,I1)
      CH='('//KNAME(I1:)//')'//SOLNAM(EMCODE(IP))
      END IF
      END IF
!+++++
      END IF
!=====
      I1=INDEX(CH,'  ')
!cdcmar08
      IF (I1.EQ.1) GOTO 599
      TEXTA(NASS)(LT+1:LT+I1)=CH(1:I1-1)
      LT=LT+I1
!-----
  599 CONTINUE
  600 CONTINUE
      IF (MAP) THEN
      WRITE (6,1000) NASS,XZ,YZ,TEXTA(NASS)
      WRITE (out,1000) NASS,XZ,YZ,TEXTA(NASS)
      TEXTA(NASS)=MAPTEX(NUMMER(NUN2)-NPHA-NSOL)
      END IF
!-----
      NOA=NASS
      WRITE (6,1000) NASS,XZ,YZ,TEXTA(NASS)
      WRITE (out,1000) NASS,XZ,YZ,TEXTA(NASS)
 1000 FORMAT (' ASSEMBLAGE:',I4,': x,y = ',1PE15.8,2X,1PE15.8,2X,A80)
!-----
      CH001=' '
      CH001(6:18)=' assemblage: '
      I1=19
      DO I=1,NUN2
      CALL LABLA(NUMATXT(NASS,I),I2)
      CH001(I1:)=NUMATXT(NASS,I)
      I1=I1+I2+1
      END DO
      CALL PUST(6,CH001)
      CALL PUST(out,CH001)
!-----
      RETURN
      END
!-----
!******************************
      SUBROUTINE PPLOIG
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      include 'files.cmn'
!
!----
      CHARACTER(80) CCH,DATI
      CHARACTER(250) CH98
!      CHARACTER(50) FORMA
      REAL(8) F1,F2,F3,F4,FF
      INTEGER(4) II,I1,I2,IL,I001,I002,ICH,NL1,NL2,IX1,IY1, &
      IX2,IY2,ITH,j,I0
!      INTEGER(4) ILF
!----
!     CALL DATE(DADA)
!     CALL TIME(TITI)
!      DADA='00.00.00'
!      TITI='00:00:00'
!      DATI=DADA//' '//TITI
       DATI=sdate
       CALL CPUTIME(line)
       CALL LABLA(DATI,j)
       DATI=DATI(1:j)//'  '//line
!----
!      REWIND (UNIT=plt)
!----
!12. Feb. 2018: make long version of coplot
      WRITE (plt,3000) 'L:'//XTEXT,YTEXT
 3000 FORMAT (A80/A80)
!      FORMA='(2'//FORX//',2'//FORY//',2F10.4,''   0  0'')'
!      CALL LABLA(FORMA,ILF)
!      WRITE (plt,FMT=FORMA(1:ILF)) XMIN,XMAX,YMIN,YMAX,BREITE,HOEHE
      WRITE (plt,2000) XMIN,XMAX,YMIN,YMAX,BREITE,HOEHE
 2000 FORMAT(4(1PE20.12),0P,2F10.2)
!-----
      WRITE (plt,3001) NBUL+NCOMIN+3+NSOL
 3001 FORMAT (I5,'    0    0    0    0    0')
!      FORMA='('//FORXY//',F10.7,F10.4,I5,''Bulk('',I1,'')= '',A)'
      F1=XMIN
      F3=0.2D0
      F4=0.0D0
      ICH=0
!      CALL LABLA(FORMA,ILF)
      DO II=1,NBUL
        F2=YPOSA+(DBLE(II-1)*YHIGH/HOEHE)*0.35D0
        CALL LABLA(BULINE(II),I0)
!        WRITE (plt,FMT=FORMA(1:ILF)) F1,F2,F3,F4,ICH,II,BULINE(II)(1:I0)
        WRITE (plt,2024) F1,F2,F3,F4,ICH,II,BULINE(II)(1:I0)
      END DO
 2024 FORMAT (2(1PE20.12),0PF10.7,F10.4,I5,'Bulk(',I1,')= ',A)
!      FORMA='('//FORXY//',F10.7,F10.4,I5,A)'
!      CALL LABLA(FORMA,ILF)
      CALL LABLA(DATI,I0)
!      WRITE (plt,FMT=FORMA(1:ILF)) XPOSC,YPOSC,F3,F4,ICH,DATI(1:I0)
      WRITE (plt,2025) XPOSC,YPOSC,F3,F4,ICH,DATI(1:I0)
 2025 FORMAT (2(1PE20.12),0PF10.7,F10.4,I5,A)
      F1=XPOSB
      DO II=1,NCOMIN
        F2=YMAX-(DBLE(II-1)*YHIGH/HOEHE)*0.35D0
        CALL LABLA(COMINS(II),I0)
!        WRITE (plt,FMT=FORMA(1:ILF)) F1,F2,F3,F4,ICH,COMINS(II)(1:I0)
        WRITE (plt,2026) F1,F2,F3,F4,ICH,COMINS(II)(1:I0)
      END DO
 2026 FORMAT (2(1PE20.12),0PF10.7,F10.4,I5,A)
      DO II=1,NSOL
       CALL LABLA(SOLNAM(II),I001)
       CCH=SOLNAM(II)(1:I001)//': '//SOLINFO(II)
       F2=F2-(YHIGH/HOEHE)*0.35D0
       CALL LABLA(CCH,I0)
!       WRITE (plt,FMT=FORMA(1:ILF)) F1,F2,F3,F4,ICH,CCH(1:I0)
       WRITE (plt,2027) F1,F2,F3,F4,ICH,CCH(1:I0)
 2027  FORMAT (2(1PE20.12),0PF10.7,F10.4,I5,A)
      END DO
      CH98='97'
      I0=2
!      WRITE (plt,FMT=FORMA(1:ILF)) 0.0,0.0,0.2,0.0,0,CH98(1:I0)
      WRITE (plt,2028) 0.0,0.0,0.2,0.0,0,CH98(1:I0)
 2028 FORMAT (2(1PE20.12),0PF10.7,F10.4,I5,A)
      CH98='98'
!      WRITE (plt,FMT=FORMA(1:ILF)) 0.0,0.0,0.2,0.0,0,CH98(1:I0)
      WRITE (plt,2029) 0.0,0.0,0.2,0.0,0,CH98(1:I0)
 2029 FORMAT (2(1PE20.12),0PF10.7,F10.4,I5,A)
!-----
      IF (XVAR.EQ.'TER') THEN
      DO II=1,NPUN
        XPUN(II)=XPUN(II)+YPUN(II)/2.0D0
        YPUN(II)=YPUN(II)*0.8660254D0
      END DO
      I1=2
      I2=2
      WRITE (plt,3010) I1,I2
      F1=0.0D0
      F2=0.0D0
      I1=3
      F3=0.5D0
      F4=0.8660254D0
      I2=2
      WRITE (plt,3002) F1,F2,I1,F3,F4,I2
! 3002 FORMAT (2F10.7,I2,2F10.7,I2)
 3002 FORMAT (2(1PE20.12),I2,2(1PE20.12),I2)
      F3=0.25D0
      F4=0.5330127D0
      I1=0
      WRITE (plt,3004) F3,F4,F1,F2,I1
      F4=0.3330127D0
      WRITE (plt,3004) F3,F4,F1,F2,I1
! 3004 FORMAT (2F10.7,F10.7,F10.4,I5)
 3004 FORMAT (2(1PE20.12),0PF10.7,F10.4,I5)
      END IF
!-----
      ITH=2
      DO 900,IL=1,NLIN
      I1=FIRST(IL)
      I2=LAST(IL)
      I001=2
      I002=I2-I1+1
      NL1=LEVEL(1,FIRST(IL))
      NL2=LEVEL(2,LAST(IL))
      IX1=IX(1,FIRST(IL))
      IX2=IX(2,LAST(IL))
      IY1=IY(1,FIRST(IL))
      IY2=IY(2,LAST(IL))
!      FORMA='(7('//FORXY//',I2))'
!      CALL LABLA(FORMA,ILF)
      IF (NL1.EQ.NL2.AND.IX1.EQ.IX2.AND.IY1.EQ.IY2) THEN
      I002=I002+1
      WRITE (plt,3010) I001,I002
!      WRITE (plt,FMT=FORMA(1:ILF)) (XPUN(II),YPUN(II), &
!      3-MIN0(II-I1,1),II=I1,I2),XPUN(I1),YPUN(I1),ITH
      WRITE (plt,2030) (XPUN(II),YPUN(II), &
      3-MIN0(II-I1,1),II=I1,I2),XPUN(I1),YPUN(I1),ITH
 2030 FORMAT(7(2(1PE20.12),I2))
      ELSE
      WRITE (plt,3010) I001,I002
!      WRITE (plt,FMT=FORMA(1:ILF)) (XPUN(II),YPUN(II), &
!      3-MIN0(II-I1,1),II=I1,I2)
      WRITE (plt,2031) (XPUN(II),YPUN(II), &
      3-MIN0(II-I1,1),II=I1,I2)
 2031 FORMAT(7(2(1PE20.12),I2))
      END IF
 3010 FORMAT (2I5,'    0    0    0    0')
      I001=ASS1(REANR(I1))
      I002=ASS2(REANR(I1))
      IF (MAP.AND.LABCOD.EQ.1) THEN
      IF (NUMA(I001,NPA(I001)).LT.NUMA(I002,NPA(I002))) THEN
      CCH=TEXTA(I002)
      ELSE
      CCH=TEXTA(I001)
      END IF
!---- label grenze der stabilitaet mit N
      IF (TEXTA(I001).EQ.'NNN'.OR.TEXTA(I002).EQ.'NNN') &
          CCH='N'
      ELSE
      CCH=TEXRE(1,REANR(I1))
      END IF
!     F1=XA1(REANR(I1))
!     F2=YA1(REANR(I1))
      FF=DBLE(2*IABS(REASENS(I1))-3)
      IF (REASENS(I1).GT.0) THEN
      F1=XPUN(I1)+FF*X1000
      F2=YPUN(I1)
      ELSE
      F1=XPUN(I1)
      F2=YPUN(I1)+FF*Y1000
      END IF
      F3=0.0D0
      F4=0.0D0
      ICH=0
!      FORMA='('//FORXY//',F10.7,F10.4,I5,A)'
!      CALL LABLA(FORMA,ILF)
      CALL LABLA(CCH,I0)
!      WRITE (plt,FMT=FORMA(1:ILF)) F1,F2,F3,F4,ICH,CCH(1:I0)
      WRITE (plt,FMT=2032) F1,F2,F3,F4,ICH,CCH(1:I0)
 2032 FORMAT (2(1PE20.12),0PF10.7,F10.4,I5,A)
      IF (MAP.AND.LABCOD.EQ.1) THEN
      CCH=' '
      ELSE
      CCH=TEXRE(2,REANR(I1))
      END IF
!     F1=XA2(REANR(I1))
!     F2=YA2(REANR(I1))
      IF (REASENS(I1).GT.0) THEN
      F1=XPUN(I1)-FF*X1000
      ELSE
      F2=YPUN(I1)-FF*Y1000
      END IF
      CALL LABLA(CCH,I0)
!      WRITE (plt,FMT=FORMA(1:ILF)) F1,F2,F3,F4,ICH,CCH(1:I0)
      WRITE (plt,2033) F1,F2,F3,F4,ICH,CCH(1:I0)
 2033 FORMAT (2(1PE20.12),0PF10.7,F10.4,I5,A)
  900 CONTINUE
      IF (XVAR.EQ.'TER') THEN
      DO II=1,NPUN
        YPUN(II)=YPUN(II)/0.8660254D0
        XPUN(II)=XPUN(II)-YPUN(II)/2.0D0
      END DO
      END IF
!-----
      RETURN
      END
!-----
!******************************
      SUBROUTINE FIXA(FPHA,IFIX,IORIG)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
!
!-----END OF COMMON VARIABLES
      LOGICAL(4) MORE
      INTEGER(4) IFIX,IORIG,I,IP
      CHARACTER(8) FPHA
!-----
      IP=0
      IFIX=0
      IORIG=0
      MORE=.FALSE.
      DO 500,I=1,NPHA
!      IF (ABK(I).EQ.FPHA) THEN
      IF (VERGL(ABK(I),FPHA)) THEN
      FPHA=ABK(I)
      MORE=.TRUE.
      IP=I
      GOTO 100
      END IF
  500 CONTINUE
  100 IORIG=IP
!-----
      IF (.NOT.MORE) RETURN
!-----
      NPHA=NPHA+1
      SUGNR=SUGNR+1
      NAME(NPHA)='F.'//FPHA
      ABK(NPHA)='f.'//FPHA(1:6)
      PHASID(NPHA)='MIN'
      DO I=1,NUN
        XX(NPHA,I)=XX(IP,I)
      END DO
      EMSOL(NPHA)=0
      EMNR(NPHA)=0
      NULL(NPHA)=.FALSE.
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
      NLANDA=0
      VOLVO=0
      TRTYP=0
      NCOM=1
      ICOM(1)=IP
      FFCOM(1)=1.0D0
      DO I=2,10
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
      COM=.TRUE.
      DIS=.FALSE.
      VO1=.TRUE.
      VO2=.FALSE.
      VO3=.FALSE.
      AQU=.FALSE.
      FIX=.FALSE.
      TL1=.FALSE.
      CALL DASAVE(NPHA)
      IFIX=NPHA
!-----
      NPHA=NPHA+1
      SUGNR=SUGNR+1
      NAME(NPHA)='-F.'//FPHA
      ABK(NPHA)='-'//FPHA(1:7)
      DO I=1,NUN
        XX(NPHA,I)=-XX(IP,I)
      END DO
      EMSOL(NPHA)=0
      EMNR(NPHA)=0
      NULL(NPHA)=.FALSE.
      FFCOM(1)=-1.0D0
      CALL DASAVE(NPHA)
!-----
      NULL(IP)=.TRUE.
      RETURN
      END
!-----
!******************************
      SUBROUTINE SCRDEF(CH001,isimu,CHIN1,COPLOT,SCRFILE)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      INCLUDE 'files.cmn'
!-----
      INTEGER isimu,i,j,ierr,I1,ILMAX
      CHARACTER *(*) CH001,CHIN1,COPLOT,SCRFILE
!-----
      CHARACTER(8) DUDU
      DUDU=COPLOT(1:8)
      isimu=isimu+1
      ILMAX=LEN(filename(fig))-10
!++++++++
      call clearscreen
!---------------------
!        open UNIT=job (commandline-file, closed before LABEL 88 in main)
!---------------------
      IF (isimu.eq.1) THEN
      j=job
      WRITE (scr,201) ('-',I=1,16)
  201 FORMAT (/,' DOMINO SCRIPTING',/,1X,80A1)
    5 CONTINUE
      state=' '
      akzess='append'
      line=filename(j)(1:fnl(j))//ext(j)
      CALL LABLA(line,I1)
      WRITE (scr,210) line(1:I1)
  210 FORMAT (' JOB filename: ',A)
      write(scr,203) line(1:I1)
  203 FORMAT (/,' Enter [ "?" | CR | new JOB filename ] <',A,'>?')
      READ (unit=IFNR,FMT='(A)') line
        if (line.eq.'?') then
          call helpme('$DOM-SCR_JOB')
          GOTO 5
        END IF
        if (line.ne.' ') then
          CALL CHECKNAME(line,ILMAX)
          call LastChar(line,i)
          fnl(j)=i
          filename(j)=line(1:i)
          fnl(spt)=i
          filename(spt)=line(1:i)
        end if
      ELSE
        state='old'
        akzess='append'
      END IF
      j=job
      line=filename(j)(1:fnl(j))//ext(j)
      path=wpath
      call openfile(j,ierr)
      if(ierr.ne.0) STOP
!++++++++
      ext(spt)='.txt'
      write(SCRFILE,200) filename(spt)(1:fnl(spt)),isimu
  200 format(a,'_',i3.3)
      CALL LABLA(SCRFILE,i)
   10 CONTINUE
      line=SCRFILE(1:i)//ext(spt)
      CALL LABLA(line,I1)
      WRITE (scr,202) line(1:I1)
  202 FORMAT (/,' SCRIPT filename: ',A)
      WRITE (scr,204) line(1:I1)
  204 FORMAT (/,' Enter [ "?" | CR | "end" | new SCRIPT filename ] <', &
      A,'>?')
      line=' '
      ext(plt)='.'//filetype(plt)
      ext(cln)='.'//filetype(cln)
      ext(pst)='.'//filetype(pst)(1:2)
      ext(rxn)='.'//filetype(rxn)
!=====
      read(UNIT=IFNR,FMT='(A)') line
      if(line.eq.'?') then
         call helpme('$DOM-SCR_SCR')
         goto 10
      else if (VERGL(line,'end')) then
         stop
      else if(line.ne.' ') then
         CALL CHECKNAME(line,ILMAX)
         SCRFILE=line
         call LABLA(line,i)
      end if
!-----
!      line='domino < '//SCRFILE(1:i)//ext(spt)
      line='domino  '//SCRFILE(1:i)//ext(spt)
      call PUST(job,line)
!----- following moved to main some lines above label 88 (because of pix)
!c-----batch defined in THERIAK.INI
!      if(batch.eq.1) then
!         line='guzzler '//SCRFILE(1:i)//ext(plt)(1:4)//'  '//
!     &   SCRFILE(1:i)//ext(cln)(1:4)//'  '//SCRFILE(1:i)//ext(rxn)
!         call PUST(job,line)
!         line='explot  '//SCRFILE(1:i)//ext(cln)(1:4)//'  '//
!     &   SCRFILE(1:i)//ext(pst)
!         CALL PUST(job,line)
!      end if
!---------------------
!        open UNIT=spt (script-file, closed before "GOTO 1" in main)
!---------------------
      j=spt
      line=SCRFILE(1:i)//ext(spt)
      path=wpath
      akzess=' '
      state=' '
      CALL openfile(j,ierr)
      IF(ierr.ne.0) STOP
!
      line='script.'//SCRFILE(1:i)//ext(spt)
      call PUST(spt,line)
      line= SCRFILE(1:i)//ext(plt)
      call PUST(spt,line)
!
      call LABLA(CHIN1,i)
      line='Enter [ CR | database filename ] <'//CHIN1(1:i)//'>:'
      call LABLA(line,i)
      write(UNIT=scr,FMT='(A)') line(1:i)
      CH001=' '
      READ (UNIT=IFNR,FMT='(A500)') CH001
!
!----- close moved to main some lines above label 88
!      CLOSE (UNIT=job)
!
      RETURN
      END
!-----
!******************************
      SUBROUTINE TESTXY
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      INCLUDE 'files.cmn'
!-----
      REAL(8) XZ,YZ
      INTEGER(4) IOA,I,I1,I2
      CHARACTER(500) SYREC,CH001
!-----
!      PRTLOG(2)=.TRUE.
      PRTLOG(6)=.TRUE.
    1 CONTINUE
      WRITE (scr,1000)
      WRITE (out,1000)
 1000 FORMAT (/ &
      ' -----------------------'/ &
      ' define point in diagram'/ &
      ' -----------------------')
      CALL PUST(scr,' X-variable = '//XTEXT)
      CALL PUST(scr,' Y-variable = '//YTEXT)
      CALL PUST(out,' X-variable = '//XTEXT)
      CALL PUST(out,' Y-variable = '//YTEXT)
      WRITE (UNIT=scr,FMT='('' '')')
      WRITE (UNIT=out,FMT='('' '')')
      DO I=1,NBUL
       CALL LABLA(BULINE(I),I1)
       WRITE (scr,1002) I,BULINE(I)(1:I1)
       WRITE (out,1002) I,BULINE(I)(1:I1)
 1002  FORMAT(' bulk ',I2,' : ',A)
      END DO
      WRITE (scr,1010)
      WRITE (out,1010)
 1010 FORMAT(/,' enter x and y (or CR to quit)')
      READ (UNIT=kbd,FMT='(A500)',END=99) SYREC
      IF (SYREC.EQ.' ') GOTO 99
      CALL GELI(SYREC,XZ)
      CALL GELI(SYREC,YZ)
      CALL SQUEAL(XZ,YZ,IOA)
      CALL LABLA(TEXTA(IOA),I1)
      WRITE (UNIT=scr,FMT='(/,A100)') REPEAT('-',100)
      WRITE (UNIT=out,FMT='(/,A100)') REPEAT('-',100)
      WRITE (scr,1012) XZ,YZ
      WRITE (out,1012) XZ,YZ
 1012 FORMAT(/,' x = ',1PE12.5,/,' y = ',1PE12.5)
!      WRITE (scr,1014) IOA,TEXTA(IOA)(1:I1)
!      WRITE (out,1014) IOA,TEXTA(IOA)(1:I1)
! 1014 FORMAT(/,' assemblage = ',I4,' : ',A)
!-----
!----
      DO I=1,NUN
       WRITE (scr,1020) OXYDE(CHMCOD(I))(1:8),BULK(I)
       WRITE (out,1020) OXYDE(CHMCOD(I))(1:8),BULK(I)
 1020  FORMAT(A8,2X,F10.4)
      END DO
      WRITE (UNIT=scr,FMT='('' '')')
      WRITE (UNIT=out,FMT='('' '')')
!----
!----
      CH001=' '
      CH001(1:13)=' assemblage: '
      I1=14
      DO I=1,NUN2
      CALL LABLA(STPHNAM(I),I2)
      CH001(I1:)=STPHNAM(I)
      I1=I1+I2+1
      END DO
      WRITE (UNIT=scr,FMT='('' '')')
      WRITE (UNIT=out,FMT='('' '')')
      CALL PUST(scr,CH001)
      CALL PUST(out,CH001)
!-----
      GOTO 1
!-----
   99 CONTINUE
      END
!-----
!******************************
      SUBROUTINE DOPIX(IDELX,IDELY,PIXFNAME,MULTI)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      INCLUDE 'files.cmn'
!-----
      LOGICAL(4) MULTI
      INTEGER I,II,I1,IDELX,IDELY,IXDIM,IYDIM,NVOLD,IOA,NAOLD, &
      ILMAX,I2,IL,I001
      REAL(8) XDIM,YDIM,DY,DX,XZ,YZ,X16
      CHARACTER*(*) PIXFNAME
      CHARACTER(32) CH16
      CHARACTER(80) DATI,DIRNAME,CCH
      CHARACTER(200) CH200
      INTEGER j,ierr
!-----
      IF (IDELX.LE.0) IDELX=10
      IF (IDELY.LE.0) IDELY=10
      XDIM=DBLE(IDELX)
      YDIM=DBLE(IDELY)
      IXDIM=IDELX-1
      IYDIM=IDELY-1
!===== falls ein script verwendet wird: kein dialog
      IF (MULTI) THEN
      ILMAX=LEN(filename(fig))-1
      DIRNAME=PIXFNAME
      CALL FIBLA(DIRNAME,I1)
      CALL LABLA(DIRNAME,I2)
      IF (DIRNAME(I1:I1).EQ.'_') I1=I1+1
      IF (DIRNAME(I2:I2).EQ.dir) I2=I2-1
      CH200='_'//DIRNAME(I1:I2)
      CALL CHECKNAME(CH200,ILMAX)
      CALL LABLA(CH200,IL)
      IL=IL+1
      CH200(IL:IL)=dir
      filename(fig)=CH200(1:IL)
      fnl(fig)=IL
!===== im Normalfall:
      ELSE
      ILMAX=LEN(filename(fig))-10
      DIRNAME=filename(fig)
      IF (IFNR.EQ.5) THEN
        CALL PREMAKEF(DIRNAME,ILMAX,IL)
       ELSE
        DIRNAME='_pixelmaps/'
        IL=11
       END IF
      filename(fig)=DIRNAME(1:IL)
      fnl(fig)=IL
!---
      END IF
!=====
      IF (dir.EQ.':'.AND.os(1:6).EQ.'MacOs9') THEN
      filename(fig)=':_pixelmaps:'
      fnl(fig)=12
      END IF
      WRITE (6,1000) filename(fig)(1:fnl(fig))
 1000 FORMAT (/,' using folder:',2X,'"',A,'"',/)
      CALL MAKEFOLDER(filename(fig))
!---------------------
!        open UNIT=fig
!---------------------
      j=fig
      line=filename(fig)(1:fnl(fig))//'pixinfo'
      path=wpath
      akzess=' '
      state=' '
      CALL openfile(j,ierr)
      IF(ierr.ne.0) STOP
!-----write picture info
      I1=2
      IF (XVAR.EQ.'TER') I1=3
      CALL PUST(fig,XTEXT)
      CALL PUST(fig,YTEXT)
      WRITE (fig,3000) XMIN,XMAX,YMIN,YMAX
 3000 FORMAT (4(1PE20.12,2X))
      WRITE (fig,3010) BREITE,HOEHE,IXDIM+1,IYDIM+1,I1
 3010 FORMAT (F10.4,2X,F10.4/I5,2X,I5,2X,I5)
      WRITE (UNIT=fig,FMT='(I4)') NCOMIN+NSOL
      DO I=1,NCOMIN
        CALL PUST(fig,COMINS(I))
      END DO
      DO I=1,NSOL
       CALL LABLA(SOLNAM(I),I001)
       CCH=SOLNAM(I)(1:I001)//': '//SOLINFO(I)
       CALL PUST(fig,CCH)
      END DO
      WRITE (UNIT=fig,FMT='(I4)') NBUL
      DO I=1,NBUL
        CALL PUST(fig,BULINE(I))
      END DO
      CLOSE (UNIT=fig)
!=====
      NROWTBL=0
      NVARTBL=0
      NVOLD=0
      NAOLD=0
      PRTLOG(10)=.TRUE.
      PRTLOG(2)=.TRUE.
      PRTLOG(6)=.TRUE.
      DX=(XMAX-XMIN)/XDIM
      DY=(YMAX-YMIN)/YDIM
      DO 600,I=0,IYDIM
      YZ=YMIN+(DBLE(I)+0.5D0)*DY
      DO 601,II=0,IXDIM
      XZ=XMIN+(DBLE(II)+0.5D0)*DX
      WRITE (6,3141) NROWTBL+1,XZ,YZ
 3141 FORMAT(I8,2X,'X=',F12.5,2X,'Y=',F12.5)
      CALL SQUEAL(XZ,YZ,IOA)
!+++++
      IF (IOA.NE.-1) THEN
      CH16='assemblage'
      X16=DBLE(IOA)
      CALL SETMAP(CH16,X16)
      END IF
!+++++
      IF (NVARTBL.GT.NVOLD) THEN
!---------------------
!        open UNIT=fig
!---------------------
      j=fig
      line=filename(fig)(1:fnl(fig))//'pixinfo'
      path=wpath
      akzess='APPEND'
      state=' '
      CALL openfile(j,ierr)
      IF(ierr.ne.0) STOP
      DO I1=NVOLD+1,NVARTBL
        WRITE (UNIT=fig,FMT='(A32)') VARTBL(I1)
      END DO
      NVOLD=NVARTBL
      CLOSE (UNIT=fig)
      END IF
!+++++
!+++++
      IF (NASS.GT.NAOLD) THEN
!---------------------
!        open UNIT=fig
!---------------------
      j=fig
      line=filename(fig)(1:fnl(fig))//'pixa'
      path=wpath
      IF (NAOLD.EQ.0) THEN
      akzess=' '
      ELSE
      akzess='APPEND'
      END IF
      state=' '
      CALL openfile(j,ierr)
      IF(ierr.ne.0) STOP
      DO I1=NAOLD+1,NASS
        WRITE (UNIT=fig,FMT='(I4,2X,A)') I1,TEXTA(I1)
      END DO
      NAOLD=NASS
      CLOSE (UNIT=fig)
      END IF
!+++++
  601 CONTINUE
  600 CONTINUE

!=====
       DATI=sdate
       CALL CPUTIME(line)
       CALL LABLA(DATI,j)
       DATI=DATI(1:j)//'  '//line
!---------------------
!        open UNIT=fig
!---------------------
      j=fig
      line=filename(fig)(1:fnl(fig))//'datetime'
      path=wpath
      akzess=' '
      state=' '
      CALL openfile(j,ierr)
      IF(ierr.ne.0) STOP
      WRITE (UNIT=fig,FMT='(A)') DATI
      CLOSE (UNIT=fig)
!=====
      RETURN
      END
!-----
!******************************
      SUBROUTINE CHECKNAME1(DIRNAME)
      IMPLICIT NONE
!-----
      CHARACTER*(*) DIRNAME
      CHARACTER(13) VERBOTEN
      INTEGER(4) I,I1,J,K
      DATA VERBOTEN /'*,!@%$^&|~<>/'/
!-----
      CALL LABLA(DIRNAME,I1)
      DO I=1,I1
        J=INDEX(VERBOTEN,DIRNAME(I:I))
        K=ICHAR(DIRNAME(I:I))
        IF (J.NE.0.OR.K.LE.32.OR.K.EQ.92) DIRNAME(I:I)='0'
      END DO
!=====
      RETURN
      END
!-----
!******************************
      SUBROUTINE CHECKNAME(DIRNAME,ILMAX)
      IMPLICIT NONE
!-----
!---  this is the very restricive version
!-----
      CHARACTER*(*) DIRNAME
      CHARACTER(64) ERLAUBT
      INTEGER(4) I,I1,J,ILMAX
      ERLAUBT='0123456789'// &
      'ABCDEFGHIJKLMNOPQRSTUVWXYZ'// &
      'abcdefghijklmnopqrstuvwxyz'// &
      '_#'
!-----
      ILMAX=50

      CALL LABLA(DIRNAME,I1)
      IF (I1.GT.ILMAX) THEN
      DO I=ILMAX+1,I1
        DIRNAME(I:I)=' '
      END DO
      I1=ILMAX
      END IF
      DO I=1,I1
        J=INDEX(ERLAUBT,DIRNAME(I:I))
        IF (J.EQ.0) DIRNAME(I:I)='a'
      END DO
!=====
      RETURN
      END
!-----
!******************************
      SUBROUTINE PREMAKEF(DIRNAME,ILMAX,IL)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      INCLUDE 'files.cmn'
!-----
      CHARACTER*(*) DIRNAME
      CHARACTER(200) CH200,CH001
      INTEGER(4) ILMAX,I1,I2,IL
!----- 1: stringlength maximal ILMAX including "_" and dir
!----- (length is now set to 50 in CHECKNAME)
    1 CALL FIBLA(DIRNAME,I1)
      CALL LABLA(DIRNAME,I2)
      IF (DIRNAME(I1:I1).EQ.'_') I1=I1+1
      IF (DIRNAME(I2:I2).EQ.dir) I2=I2-1
      CH200='_'//DIRNAME(I1:I2)
!----- 2: remove any forbidden characters
      CALL CHECKNAME(CH200,ILMAX)
      CALL LABLA(CH200,IL)
      IL=IL+1
      CH200(IL:IL)=dir
      WRITE (scr,1000) CH200(1:IL)
 1000 FORMAT (/,' proposed folder name: ',A)
!----- 3: check if folder exists
      CALL DIRLIST
    5 READ (UNIT=99,FMT='(A)',END=99) line
      CALL LABLA(line,I1)
      IF (line(I1:I1).NE.dir) THEN
      I1=I1+1
      line(I1:I1)=dir
      END IF
!
!      WRITE (6,1004) line(1:I1),CH200(1:IL)
! 1004 FORMAT ('line : "',A,'"',/,'ch200 : "',A,'"')
      IF (line.EQ.CH200) THEN
      WRITE(scr,1002) line(1:I1)
 1002 FORMAT(/,' folder "',A,'" exists.', &
      /,' enter CR to overwrite, or new folder name')
      READ (UNIT=IFNR,FMT='(A)') CH001
      IF (CH001.EQ.' ') THEN
      CALL REMOFOLDER(line)
      GOTO 99
      ELSE
      DIRNAME=CH001
      CALL FILEDELETE
      GOTO 1
      END IF
!
      ELSE
      GOTO 5
      END IF
!=====
   99 CALL FILEDELETE
      DIRNAME=CH200(1:IL)
      RETURN
      END
!-----
!******************************
      subroutine isohelp (syrec)
!
!****  GLOBAL VARIABLES
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      INCLUDE 'files.cmn'
!
!**** LOCAL VARIABLES                               
      integer i, j, i001, ivar, erst(3), letzt(3), pn, I1, nrprop, &
      I2,I3,I4,I5,IEXCL,NEXCL(10),IS,IP,I6
      parameter(nrprop=26)
      CHARACTER(16) min, max, step,DIVIS
      character *(*) syrec
      CHARACTER(26) sortstring(phmax),KEY,phase
      character property(nrprop)*30, var(nrprop)*8, isotype(3)*30, &
      answ*80
      CHARACTER(8) PNC
      CHARACTER(80) MIMA,KEYPLUS,CH80
!
!**** LOCAL INIT
      data isotype /'FOR SOLUTION PHASES:', &
        'FOR NON_SOLUTION PHASES:', &
        'FOR BULK ROCK:'/
!bulk rock 1-5      erst - letzt
!non-solution 6-12  erst - letzt
!solution  13-22    erst - letzt
!      
      data erst   /  1, 13, 21 /
      data letzt  / 12, 20, 25 /   
      data property / &
        'Isopleths [mol fractions]', &
        'Site occupancies', &
        'Amount [mol]', &
        'Volume [ccm]', &
        'Molar volume [g/mol]', &
        'Weight [g]', &
        'Molar weight [g/mol]', &
        'Density [g/ccm]', &
        'Ratio Mg/(Mg+Fe)', &
        'Si (per formula unit)', &
        'Al (per formula unit)', &
        'Volume% (of solids)', &
        'G(excluded phase)', &
        'Amount [mol]', &
        'Volume [ccm]', &
        'Molar volume [g/mol]', &
        'Weight [g]', &
        'Molar weight [g/mol]', &
        'Density [g/ccm]', &
        'Volume% (of solids)', &
        'G(excluded phase)', &
        'Volume of solids [ccm]', &
        'Weight of solids [g]', &
        'Density of solids [g/ccm]', &
        'Total Gibbs Free Energy [J]', &
        'H2O in solids [wt.%]' &
        /
      data var / &
        'species', &
        'El(site)', &
        'mol', &
        'vol', &
        'mvol', &
        'wt', &
        'mwt', &
        'rho', &
        'Mg#', &
        'sipfu', &
        'alpfu', &
        'vol%', &
        'g', &
        'mol', &
        'vol', &
        'mvol', &
        'wt', &
        'mwt', &
        'rho', &
        'vol%', &
        'g', &
        'volsol', &
        'wtsol', &
        'rhosol', &
        'gtot', &
        '%h2o.sol' &
        /
!----
      syrec=' '
      ivar=0
      pn=1
      min='0'
      max='0'
      step='0'
      answ=' '
      IEXCL=0
!
!**** CODE START 
10    format(/,80a)
      write(scr,10) ' List of variables'
      write(scr,FMT='(1X,80a)') ('-',I=1,17)
      do i=1,3
        write(scr,12) isotype(i)
12      format(/,2x,a) 
        write(scr,14) (j, property(j), j=erst(i), letzt(i))
14      format(3(4x,i2.0,':',1x,a))
      end do
!=====
11    write(scr,FMT='(/,A,/,A)') ' b: back to calculation type', &
                    ' Enter [ CR | "b" | number ] <b>? '
      read (IFNR,'(a)') answ
      if(answ.eq.' '.OR.VERGL(answ,'b')) then
         return
      else
         read(answ,*) ivar
      end if  
      if(ivar.lt.1.or.ivar.gt.nrprop)  then
        write(scr,FMT='('' Number must be between 1 and '',i2)') &
        nrprop
        goto 11
      end if 
!+++++
!+++++ check for G_'s
      IF (var(ivar).EQ.'g') THEN
!===== G_'s in solutions
       IF (ivar.LE.letzt(1)) THEN
        IF (SEXCL.EQ.0) THEN
         WRITE (scr,FMT='('' no excluded solution phase present'')')
         GOTO 11
        ELSE
         WRITE (scr,FMT='(/,A)') ' excluded solutions:'
         WRITE(scr,FMT='(1X,19A)') ('-',I=1,19)
         DO IS=1,NSOL
           IF (EXSOL(IS)) THEN
            IEXCL=IEXCL+1
            NEXCL(IEXCL)=IS
            WRITE (scr,FMT='(4X,I2,'':'',1X,A)') IEXCL,SOLNAM(IS)
           END IF
         END DO
        WRITE (scr,FMT='(/,A)') 'Enter [ CR | "b" | number ] <1>? '
        READ (IFNR,'(A)') answ
        IF (VERGL(answ,'b')) RETURN
        IF (answ.EQ.' ') answ='1'
        READ (answ,*) j
        IF (j.LT.1.OR.j.GT.IEXCL) j=1
        phase=SOLNAM(NEXCL(j))
        KEY=var(ivar)
        pn=1
        GOTO 888
        END IF
       END IF
!===== G_'s in non-solutions
        IF (PEXCL.EQ.0) THEN
         WRITE (scr,FMT='('' no excluded phase present'')')
         GOTO 11
        ELSE
         WRITE (scr,FMT='(/,A)') ' excluded phases:'
         WRITE(scr,FMT='(1X,16A)') ('-',I=1,16)
         DO IP=NUN+1,NPHA
           IF (NAME(IP)(1:1).EQ.'$') THEN
            IEXCL=IEXCL+1
            NEXCL(IEXCL)=IP
            WRITE (scr,FMT='(4X,I2,'':'',1X,A)') IEXCL,NAME(IP)
           END IF
         END DO
        WRITE (scr,FMT='(/,A)') ' Enter [ CR | "b" | number ] <1>? '
        READ (IFNR,'(A)') answ
        IF (VERGL(answ,'b')) RETURN
        IF (answ.EQ.' ') answ='1'
        READ (answ,*) j
        IF (j.LT.1.OR.j.GT.IEXCL) j=1
        phase=NAME(NEXCL(j))
        KEY=var(ivar)
        pn=0
        GOTO 888
        END IF
      END IF
!+++++
!+++++ case1: solution
!+++++
      if(ivar.le.letzt(1)) then
        write(scr,10) ' List of solution phases'
        write(scr,FMT='(1X,80a)') ('-',I=1,23)
        WRITE (scr,15) &
        (I,SOLNAM(I), I=1,NSOL)
15      format(5(4x,i2.0,':',1x,a))
        answ=' '
127   write(scr,FMT='(/,A)') ' Enter [ CR | "b" | number ] <b>? '
      read (IFNR,'(a)') answ
      if(answ.eq.' '.OR.VERGL(answ,'b')) then
         return
      else
         read(answ,*) j
      end if  
        if(j.lt.1.or.j.gt.NSOL) then
         write(scr,FMT='('' Number must be between 1 and '',i3)') NSOL
         goto  127
        end if 
        phase=SOLNAM(J)
        KEY=var(ivar)
!+++++ case1: solution endmember or site occupancy
        call labla(phase,i)
        if(ivar.gt.erst(1)+1) goto  32
        write(scr,10) ' List of endmembers and site occupancies ', &
        'for '//phase 
        write(scr,FMT='(1X,80a)') ('-',I=1,44+i)
!-----  define SORTSTRING with EM and SITEL
        I1=0
        DO I=1,NEND(J)
          I1=I1+1
          SORTSTRING(I1)=ABK(EM(J,I))
        END DO
        IF (NSIEL(J).GT.0) THEN
          DO I=1,NSIEL(J)
          I1=I1+1
          SORTSTRING(I1)=SIEL(J,I)
          END DO
        END IF
!-----
        write (scr,16) (i, SORTSTRING(i),i=1,I1)
16      format(5(4x,i2.0,':',1x,a16))
        answ=' '
      write(scr,FMT='(/,A)') ' Enter [ CR | "b" | number ] <b>? '
      read (IFNR,'(a)') answ
      if(answ.eq.' '.OR.VERGL(answ,'b')) then
         return
      else
         read(answ,*) i
      end if  
        KEY=SORTSTRING(I)
!+++++ case1: all solutions
32      continue
        CALL SOLORDER(phase)
        call labla(phase,j)
        write(scr,FMT='(2A)') ' Enter phase number to use if more ', &
        'than one phase of the same solution is stable'
        write (scr,24)
24      format(' Enter [ ? | CR | "b" | number ] <1> ? ')
        read(IFNR,'(a)') answ
        IF (answ.eq.'?' ) then
           call  helpme('$DOM-ISO1')
           goto 32
        END IF
        IF (VERGL(answ,'b')) RETURN
        if(answ.eq.' ') then
           pn=1
        else
          read(answ,*) pn
        end if   
      end if
!+++++
!+++++ case2: non-solution 
!+++++
      if (ivar.ge.erst(2).and.ivar.le.letzt(2)) then
!-----  define SORTSTRING with NAME
        DO I=NUN+1,NPHA
          SORTSTRING(I-NUN)=NAME(I)
        END DO
        I001=NPHA-NUN
        CALL SORTIER(SORTSTRING,I001)
!-----
        write(scr,10) ' List of phases'
        WRITE (scr,FMT='(1X,80A1)') ('-',I=1,15)
        write (scr,20) (i, SORTSTRING(i),i=1,I001)
20      format(5(1x, i3.0,':',1X,A16))
        answ=' '
17    write(scr,FMT='(/,A)') ' Enter [ CR | "b" | number ] <b>? '
      read (IFNR,'(a)') answ
      if(answ.eq.' '.OR.VERGL(answ,'b')) then
         return
      else
         read(answ,*) j
      end if  
        if(j.lt.1.or.j.gt.i001) then
         write(scr,FMT='('' Number must be between 1 and '',i3)') i001
         goto  17
        end if 
        phase = sortstring(j)
        KEY=var(ivar)
        pn=0
      end if
!+++++
!+++++ case3: bulk rock 
!+++++
      if(ivar.ge.erst(3)) then
         phase=' '
         KEY=var(ivar)
         pn=0
      end if
!+++++
!+++++ minmax and write syrec 
!+++++
  888 CONTINUE
      call labla(phase,I1)
      call labla(KEY,I2)
      KEYPLUS=phase(1:I1+2)//KEY(1:I2)
      CH80='0  1  0.1'
      WRITE (scr,18) KEYPLUS(1:I1+I2+2),CH80(1:9)
18    format(' Enter isopleths range, step [and a denominator]', &
      ' for ',A, &
      /,' Enter [ CR | "b" | min  max  step (denominator)] <',a,'> ?')
      read(IFNR,FMT='(A80)') answ
      if(answ.eq.' ') answ=CH80
      IF(VERGL(answ,'b')) RETURN
      CALL TAXI1(answ,min)
      CALL TAXI1(answ,max)
      CALL TAXI1(answ,step)
      CALL TAXI1(answ,DIVIS)
!
      call labla(phase,I1)
      call labla(KEY,I2)
      CALL LABLA(DIVIS,I6)
      IF (DIVIS.NE.' ') THEN
       KEYPLUS=KEY(1:I2)//'/'//DIVIS(1:I6)
       KEY=KEYPLUS
       CALL LABLA(KEY,I2)
      END IF
!
      CALL LABLA(min,I3)
      CALL LABLA(max,I4)
      CALL LABLA(step,I5)
      WRITE (PNC,FMT='(I4)') pn
      CALL FIBLA(PNC,I)
      CALL LABLA(PNC,J)
      MIMA=min(1:I3+2)//max(1:I4+2)//step(1:I5)
!--bulk things
      IF (phase.EQ.' '.AND.pn.EQ.0) syrec=KEY(1:I2+2)//MIMA
!--non-solutions
      IF (phase.NE.' '.AND.pn.EQ.0) &
          syrec=phase(1:I1+2)//KEY(1:I2+2)//MIMA
!--solutions
      IF (phase.NE.' '.AND.pn.NE.0) &
          syrec=phase(1:I1+2)//KEY(1:I2+2)//PNC(I:J+2)//MIMA
      CALL PUST(scr,' input: '//SYREC)
!  100 continue
      return
      end
!-----
!******************************
      SUBROUTINE SOLORDER(PHASE)
      IMPLICIT NONE
      INCLUDE 'theriak.cmn'
      INCLUDE 'files.cmn'
!
      CHARACTER*(*) PHASE
      CHARACTER(120) CH001
      CHARACTER(16) CH16
      INTEGER(4) IS,IE,I1,I2
!-----
      DO IS=1,NSOL
       IF (SOLNAM(IS).EQ.PHASE) THEN
        CALL LABLA(PHASE,I1)
        CH001=' Sorting order for '//PHASE(1:I1)//':'
        DO IE=1,NEND(IS)
         CALL LABLA(CH001,I1)
         CH16=ABK(EM(IS,IE))
         CALL LABLA(CH16,I2)
         CH001(I1+1:)=' ['//CH16(1:I2)//']'
        END DO
       CALL PUST(scr,' ')
       CALL PUST(OUT,' ')
       CALL PUST(scr,CH001)
       CALL PUST(OUT,CH001)
       RETURN
       END IF
      END DO
!-----
      RETURN
      END
!******************************
      SUBROUTINE FUNTAXI(CH,CH1,F1,F1CH)
      implicit none         
      CHARACTER*(*) CH,CH1,F1CH
      CHARACTER(32) CH001,CH002
      REAL(8) F1,FF
      INTEGER(4) I1
!----
      CALL TAXI(CH,CH001)
      I1=INDEX(CH001,'/')
      IF (I1.EQ.0) THEN
       CH1=CH001
       F1=1.0D0
       F1CH=' '
      ELSE
       CH1=CH001(1:I1-1)
       CH002=CH001(I1+1:)
       F1CH=CH001(I1:)
       CALL GELI(CH002,FF)
       F1=FF
      END IF
!----
      RETURN
      END
!-----
