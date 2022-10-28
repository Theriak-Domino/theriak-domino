!-----Version: 28.05.2022
!               ***********
!               * MAKEMAP *
!               ***********
!
      IMPLICIT NONE
      INCLUDE 'files.cmn'
      INCLUDE 'map.cmn'
!----
      INTEGER(4) dirnr
      PARAMETER (dirnr=100)
!====
      LOGICAL(4) VERGL
      EXTERNAL VERGL
      REAL(8) F001,NULL,XZAHL,YZAHL,SIDE,GRS,GRZ,X1,KUR,LAN,XPOS,DXX, &
      YPOS,DXH,XMUL,YMUL,XT,YT,XTIK,YTIK,XLANG,X,Y,THETA,SYM,XCORR, &
      YCORR,MAPDIV,FXPOS
      INTEGER(4) I,J,JJ,I001,NX,NY,NMAX,I1,I2,I1X,I2X,I3,C1X,C2X, &
      IVOR,INACH,I0,EXPO,NVAR,ICMAX,NDIRS,LARG,IFNR,IXPOS,J1,J2
      CHARACTER(500) CH001,CH002,CHIN(3),ZEITSTRING
      CHARACTER(132) VARNAME(500)
      CHARACTER(80) DATI,INFILE
      CHARACTER(16) BTEXT,CH16,KEY,MAPDIVCH
!-----
      integer ij, diridx, ierr
      character dirname(dirnr)*30
!-----
      IXPOS=0
      FXPOS=0.0D0
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
!-----
!-----
      PSFONT='Helvetica'
      XZAHL=2.0D0
      YZAHL=2.0D0
!
      ABC='ABC'
      S=20.0D0
      X0=4.0D0
      Y0=18.0D0
      H=10.0D0
      B=10.0D0
      L=0.0D0
      R=10.0D0
      U=0.0D0
      O=10.0D0
      SYSTEMA=2
      PSFONT='Helvetica'
      SVGFONT='Arial'
      LINCOL=0
      FILCOL=0
      LGRAY=0.0D0
      FGRAY=0.0D0
      LCOLR=0.0D0
      LCOLG=0.0D0
      LCOLB=0.0D0
      FCOLR=0.0D0
      FCOLG=0.0D0
      FCOLB=0.0D0
      FAT=0.02D0
!!      CMPX=35.43307D0
      CMPX=72.0D0/2.54D0
      CALL SVGCOLS
!-----
      MISSING=1.0D20
      NULL=0.0D0
      CMIN=220.0
      CMAX=20.0
      MAKESVG=.TRUE.
      MAKEPS=.TRUE.
      LINDASH=.FALSE.
!-----
!!!      CALL clearscreen
      progname='MAKEMAP'
      vers='28.05.2022'
      task='draw greymap images of phase diagrams'
      sdate=' '
      ierr=0
      call initialize('$MAKEMAP-FILES',ierr)
      if(ierr.ne.0) STOP
!*****
      DO I=1,3
        CHIN(I)=' '
      END DO
!------------------
!     Open UNIT=log
!------------------
      J=log
      line=filename(J)(1:fnl(J))//ext(J)
      path=wpath
      akzess=' '
      state=' '
      call openfile(J,ierr)
      if(ierr.ne.0) STOP
!-----
      DO I=1,2
        READ (UNIT=log,FMT='(A500)',END=411) CHIN(I)
      END DO
  411 CONTINUE
!------------------
!     Open UNIT=out
!------------------
      J=out
      line=filename(J)(1:fnl(J))//ext(J)
      path=wpath
      akzess=' '
      state=' '
      call openfile(J,ierr)
      if(ierr.ne.0) STOP
!-----
      CALL DIRLIST
!-----
      NDIRS=0
      DO 300,I=1,dirnr
      read (99,'(a)',end=301) line
      if(line(1:1).eq.'_') then
        NDIRS=NDIRS+1
        dirname(NDIRS)=line
      end if
  300 CONTINUE
  301 CALL FILEDELETE
!-----
      IF (NDIRS.EQ.0) THEN
      write(UNIT=scr,FMT=116)
  116 FORMAT (/, &
      ' no folder with pixel map information found', &
      ' (note that names of pixel map folders begin with "_")')
      stop
!-----
      ELSE
    5 write(scr,101)
  101 format(' folders containing pixel map information:',/)
!     >' (all files and folders beginning with "_")',/)
      DO 310,I=1,NDIRS
      write (scr,103) I, dirname(I)
  103 format (3x,' folder ',i2,':', 1x, a)
  310 CONTINUE
!
      IF (NDIRS.GE.dirnr) THEN
      write(scr,102) dirnr
  102 format(/,' only first ',i2,' folders listed.',/)
      END IF
!
  421 WRITE (UNIT=scr,FMT='('' '')')
      CALL LABLA(CHIN(1),I001)
      IF (I001.EQ.0) I001=1
      CH002=' Enter [ "?" | CR | "files" | "list" | folderNr ] <'// &
      CHIN(1)(1:I001)//'>?'
  422 CONTINUE
      CALL PUST (scr,CH002)
      READ (IFNR,FMT='(A500)') CH001
      IF (CH001.EQ.'?') THEN
        CALL helpme('$MAP-INF')
        GOTO 422
      END IF
      IF (VERGL(CH001,'files')) THEN
        CALL listfiles
        GOTO 422
      END IF
      IF (VERGL(CH001,'list')) GOTO 5
      IF (CH001.EQ.' ') THEN
         CH001=CHIN(1)
      ELSE
          CHIN(1)=CH001
      END IF
      CALL GELI(CH001,F001)
      diridx=IDINT(F001)
      IF (diridx.GT.NDIRS.OR.diridx.LT.1) THEN
       WRITE (scr,FMT='('' number must be 1 -'',I3)') NDIRS
       GOTO 421
      END IF
      call LASTCHAR(dirname(diridx),ij)
      if (dirname(diridx)(ij:ij).ne.dir) then
      ij=ij+1
      dirname(diridx)(ij:ij)=dir
      end if
!-----
      END IF
      IF (dir.EQ.':'.AND.os(1:6).EQ.'MacOs9') THEN
      dirname(diridx)=':_pixelmaps:'
      ij=12
      END IF
!------------------
!     Open UNIT=pgm (unit=24, file=.../pixels.pgm)
!------------------
      J=pgm
      line=dirname(diridx)(1:ij)//filename(J)(1:fnl(J))//ext(J)
!      CALL PUST(scr,'line: '//line)
      path=wpath
      akzess=' '
      state='unknown'
      call openfile(J,ierr)
      if(ierr.ne.0) stop
!------------------
!     Open UNIT=pst (unit=19, file=.../pixel.ps)
!------------------
      IF (MAKEPS) THEN
      J=pst
      line=dirname(diridx)(1:ij)//filename(J)(1:fnl(J))//ext(J)
      path=wpath
      akzess=' '
      state='unknown'
      call openfile(J,ierr)
      if(ierr.ne.0) stop
      END IF
!------------------
!     Open UNIT=svg (unit=40, file=.../pixel.svg)
!------------------
      IF (MAKESVG) THEN
      svg=40
      J=40
      line=dirname(diridx)(1:ij)//'pixel.svg'
      path=wpath
      akzess=' '
      state='unknown'
      call openfile(J,ierr)
      if(ierr.ne.0) stop
      END IF
!------------------
!     Open  UNIT=30
!------------------
      J=30
      line=dirname(diridx)(1:ij)//'pixinfo'
      path=wpath
      akzess=' '
      state='old'
      call openfile(J,ierr)
      if(ierr.ne.0) stop
!------------------
!     Open  UNIT=32
!------------------
      J=32
      line=dirname(diridx)(1:ij)//'datetime'
      path=wpath
      akzess=' '
      state='old'
      call openfile(J,ierr)
      if(ierr.ne.0) stop
!
      READ (UNIT=32,FMT='(A80)') DATI
      CLOSE (UNIT=32)
!----
      READ (UNIT=30,FMT='(A132)') XTEXT
      READ (UNIT=30,FMT='(A132)') YTEXT
      READ (30,2000) XMIN,XMAX,YMIN,YMAX
 2000 FORMAT (4(1PE20.12,2X))
      READ (30,2010) BREITE,HOEHE,IXDIM,IYDIM,SYSTEMA
 2010 FORMAT (F10.4,2X,F10.4/I5,2X,I5,2X,I5)
      READ (UNIT=30,FMT='(I4)') NCOMIN
      DO I=1,NCOMIN
        READ (UNIT=30,FMT='(A132)') COMINS(I)
      END DO
      READ (UNIT=30,FMT='(I4)') NBUL
      DO I=1,NBUL
        READ (UNIT=30,FMT='(A132)') BULINE(I)
      END DO
      NVAR=0
      DO I=1,500
        READ (UNIT=30,FMT='(A132)',END=711) VARNAME(I)
        NVAR=NVAR+1
      END DO
  711 CONTINUE
      IF (SYSTEMA.EQ.3) THEN
      HOEHE=HOEHE*0.8660254D0
      END IF
      B=BREITE
      H=HOEHE
      L=XMIN
      R=XMAX
      U=YMIN
      O=YMAX
      CLOSE (UNIT=30)
!+++++
      CALL SORTIER(VARNAME,NVAR)
!+++++
      WRITE (UNIT=CH16,FMT='(I5,''.x.'',I5)') IXDIM,IYDIM
      CALL COLLAPS(CH16,I1)
      DO I=1,I1
       IF (CH16(I:I).EQ.'.') CH16(I:I)=' '
      END DO
    7 CONTINUE
      WRITE (UNIT=scr,FMT='('' '')')
      CH002=' diagram: '//DATI(1:21)//'     ('//CH16(1:I1)//')'
      CALL PUST (scr,CH002)
!    7 WRITE (scr,1000) DATI(1:21),CH16(1:I1)
! 1000 FORMAT (/,' diagram: ',A,5X,'(',A,')')
      IF (NCOMIN.LT.50) THEN
       NCOMIN=NCOMIN+1
       COMINS(NCOMIN)='pix: '//CH16(1:I1)
      END IF
      DO I=1,NBUL
       CALL LABLA(BULINE(I),I1)
       WRITE (scr,1002) I,BULINE(I)(1:I1)
 1002  FORMAT (' bulk ',I2,': ',A)
      END DO
      CALL LABLA(XTEXT,I1)
      CALL LABLA(YTEXT,I2)
      IF (I2.GT.I1) I1=I2
      WRITE (scr,1004) XTEXT(1:I1),L,R
 1004 FORMAT (' X-axis : ',A,3X,F12.5,3X,F12.5)
      WRITE (scr,1006) YTEXT(1:I1),U,O
 1006 FORMAT (' Y-axis : ',A,3X,F12.5,3X,F12.5)
!+++++
      WRITE (scr,1100)
 1100 FORMAT (/,' functions available for mapping:')
      call helpme('$PIX-ABBREV*')
      WRITE (scr,1105) (I,VARNAME(I),I=1,NVAR)
! 1105 FORMAT (5(I5,1X,A20))
 1105 FORMAT (5(I5,':',1X,A18))
!dc 1105 FORMAT (5(I3.3,1X,A16))
!
!      WRITE (scr,1110)
! 1110 FORMAT (
!     >/,' key (optional):',
!     >/,'     "x1"      : map (df/dX)',
!     >/,'     "y1"      : map (df/dY)',
!     >/,'     "alpha"   : map (1/f)*(df/dX)',
!     >/,'     "beta"    : map -(1/f)*(df/dY)',/)
!-----
      CALL LABLA(CHIN(2),I001)
      IF (I001.EQ.0) I001=1
      CH002=' Enter [ "?" | CR | "list" | functionNr  (key) ] <'// &
      CHIN(2)(1:I001)//'>?'
!-----
  432 CONTINUE
      CALL PUST(scr,' ')
      CALL PUST (scr,CH002)
      READ (IFNR,FMT='(A500)') CH001
      IF (CH001.EQ.'?') THEN
        CALL helpme('$MAP-KEY')
        GOTO 7
      END IF
      IF (VERGL(CH001,'list')) GOTO 7
      IF (CH001.EQ.' ') THEN
         CH001=CHIN(2)
      ELSE
          CHIN(2)=CH001
      END IF
!---- hier FUNGELI
!      CALL GELI(CH001,F001)
!      I1=IDINT(F001)
      CALL FUNGELI(CH001,I1,MAPDIV,MAPDIVCH)
      IF (MAPDIV.EQ.0.0D0) MAPDIV=1.0D0
!
      IF (I1.GT.NVAR) THEN
      WRITE (scr,1115) NVAR
 1115 FORMAT (' maximum variableNr is:',I4)
      GOTO 432
      END IF
!
      CALL TAXI(CH001,KEY)
      TITLE=VARNAME(I1)
      CALL LABLA(TITLE,I001)
!------------------
!     Open  UNIT=33  (file containing values)
!------------------
      J=33
      line=dirname(diridx)(1:ij)//TITLE(1:I001)
      path=wpath
      akzess=' '
      state='old'
      call openfile(J,ierr)
      if(ierr.ne.0) stop
!-----store terminal input
      CLOSE (UNIT=log)
!------------------
!     Open UNIT=log
!------------------
      J=log
      line=filename(J)(1:fnl(J))//ext(J)
      path=wpath
      akzess=' '
      state='old '
      call openfile(J,ierr)
      if(ierr.ne.0) STOP
!-----
      DO I=1,2
        CALL PUST(log,CHIN(I))
      END DO
      CLOSE (UNIT=log)
!
!---- hier division
      IF (MAPDIV.NE.1.0D0) THEN
       CH002=TITLE(1:I001)//MAPDIVCH
       CALL LABLA(CH002,I001)
       TITLE=CH002(1:I001)
      END IF
!+++++
!     WRITE (6,1005)
!1005 FORMAT ('DIMENSIONS?')
!     READ (IFNR,*) NX,NY
      NX=IXDIM
      NY=IYDIM
      NMAX=(NX)*(NY)
      DO I=1,NMAX
        FF0(I)=MISSING
      END DO
      XMUL=DBLE(NX)/BREITE
      YMUL=DBLE(NY)/HOEHE-0.1
      DXH=HOEHE/DBLE(NY)
!---------------
!--- read image
!---------------
      FMIN=1D40
      FMAX=-1D40
      DO 600,I=1,2*NMAX
      READ (UNIT=33,FMT=3000,END=599) JJ,F001
 3000 FORMAT (I6,2X,D20.12)
!---- hier division
      F001=F001/MAPDIV
      IF (FF0(JJ).EQ.MISSING) THEN
      IF (F001.LT.FMIN) FMIN=F001
      IF (F001.GT.FMAX) FMAX=F001
      FF0(JJ)=F001
      ELSE
      FF0(JJ)=-999.0D0
      END IF
  600 CONTINUE
  599 CONTINUE
!+
      IF (FMIN.EQ.FMAX) THEN
       FMIN=FMIN*0.9D0
       FMAX=FMAX*1.1D0
       IF (FMIN.EQ.0.0D0) THEN
        FMIN=-1.0D0
        FMAX=1.0D0
       END IF
      END IF
!+
      DO I=1,NMAX
        IF (FF0(I).EQ.-999.0D0) FF0(I)=MISSING
      END DO
      CLOSE (UNIT=33)
!---
!      WRITE (scr,3010) FMIN,FMAX
! 3010 FORMAT ('FMIN=',1PE20.12,2X,'FMAX=',1PE20.12)
!      IF (TITLE(1:1).EQ.'x') THEN
!      FMAX=1.0D0
!      FMIN=0.0D0
!      END IF
!===
      XCORR=0.0D0
      YCORR=0.0D0
!---------------------------------------
!---- verschiedene Manipulationen
!---------------------------------------
      IF (KEY.NE.' ') CALL DERIVX1(KEY,XCORR,YCORR)
      XPOS=X0+XCORR
      YPOS=Y0+YCORR
      FA=(CMAX-CMIN)/(FMAX-FMIN)
      CALL VPINIT
      IF (MAKEPS) THEN
      WRITE (UNIT=pst,FMT='(''(Helvetica)   nf'')')
      END IF
!-----------------------------------------
!---- image und PostScript + svg erstellen
!-----------------------------------------
      DXX=0.0D0
      IF (SYSTEMA.EQ.3) THEN
        DXX=BREITE/DBLE(NX)/2.0D0
        XPOS=X0+BREITE/2.0D0
        IXPOS=NX/2
        FXPOS=DBLE(NX)/2.0D0
      END IF
      WRITE (UNIT=CH16,FMT='(I5,''.x.'',I5)') NX,NY
      CALL COLLAPS(CH16,I1)
      DO 450,I=1,I1
      IF (CH16(I:I).EQ.'.') CH16(I:I)=' '
  450 CONTINUE
      CALL LABLA(TITLE,I001)
      CH002=' making a grey map ('//CH16(1:I1)//') of "'// &
      TITLE(1:I001)//'" in folder "'//dirname(diridx)(1:ij)//'"'
      CALL PUST (scr,CH002)
!----- .ppm (rgb) files have the keyword "P3"
!----- .pgm (grey) files have the keyword "P2"
!----- ICMAX is the maximum value
!      ICMAX=IDINT(MAX(CMIN,CMAX))
      ICMAX=255
      WRITE (pgm,3055) NX,NY,ICMAX
 3055 FORMAT ('P2',/,'#.',/,I5,2X,I5,/,I5,/)

      DO 610,I=NMAX-NX+1,1,-NX
      IF (SYSTEMA.EQ.3) THEN
        XPOS=XPOS-DXX
        FXPOS=FXPOS+0.5D0
        IXPOS=IDINT(FXPOS)+1
      END IF
      YPOS=YPOS-DXH
      I1=0
      DO 620,J=I,I+NX-1
      I1=I1+1
      I2=J
      CALL INCALC(I1,I2)
  620 CONTINUE
!----- next line for .ppm file (inverted)
!dc      WRITE (pgm,3050) (255-FLINE(J),255-FLINE(J),255-FLINE(J),J=1,NX)
!----- next line for .ppm file (normal)
!dc      WRITE (pgm,3050) (FLINE(J),FLINE(J),FLINE(J),J=1,NX)
!----- next line for .pgm file


      IF (SYSTEMA.EQ.3) THEN
!      DO J=IXPOS,(NX+IXPOS-1)
!      WRITE (UNIT=6,FMT='(''J,MOD(J,NX)+1 '',2I10)') J,MOD(J,NX)+1
!      END DO
      IF (IXPOS.LT.1) IXPOS=1
!      WRITE (pgm,3049) (FLINE(MOD(J,NX)+1),J=IXPOS,NX+IXPOS-1)
      WRITE (pgm,3049) (FLINE(J),J=IXPOS,NX)
      WRITE (pgm,3049) (FLINE(J),J=1,IXPOS-1)
 3049 FORMAT (35I5)
      END IF

      IF (SYSTEMA.EQ.2) THEN
      WRITE (pgm,3050) (FLINE(J),J=1,NX)
 3050 FORMAT (35I5)
      END IF
!----
      WRITE (UNIT=CH001,FMT=3051) (FLINE(J),J=1,NX)
 3051 FORMAT (1X,250Z2.2)
      CALL LABLA(CH001,I001)
      CH001(I001+1:I001+1)='>'
      CH001(1:1)='<'
      IF (MAKEPS) THEN
      WRITE (pst,3060) CH001(1:I001+1)
 3060 FORMAT ('/linie'/A/'def')
      WRITE (pst,3065) XPOS,YPOS,NX,XMUL,YMUL
 3065 FORMAT ('gsave',2X,2F10.6,2X,'translate',I4,'  1  8  ','[', &
      F10.6,'  0  0  ',F10.6,'  0  0]  {linie} image grestore')
      END IF
  610 CONTINUE

      IF (MAKESVG) THEN
      WRITE (svg,3100) (X0+XCORR)*CMPX,595.0D0-(Y0+YCORR)*CMPX, &
      (B)*CMPX,(H)*CMPX
 3100 FORMAT (/,'<image x="',F10.4,'" y="',F10.4,'" width="',F10.4, &
      '" height="',F10.4,'"',/ &
      ' image-rendering="pixelated"',/ &
      ' preserveAspectRatio="none"',/ &
      ' xlink:href="pixels.pgm" > </image>',/)
!<image x="  113.3858" y="   84.7638" width="  425.1969" height="  368.2313" 
!image-rendering="pixelated"
!preserveAspectRatio="none" 
!xlink:href="pixels.pgm" > </image>
      END IF

!----------------------------
!---- ab hier: EXPLOT-things
!----------------------------
      Y0=Y0-H
      I0=0
      BTEXT=' '
      GRS=0.5D0
      GRZ=0.3D0
      I1X=2
      I2X=2
      I3=-1
      KUR=0.2D0
      LAN=0.4D0
      XTIK=(XMAX-XMIN)/10.0D0
      IF (ABS(XMAX).GT.ABS(XMIN)) THEN
      XLANG=XMAX
      ELSE
      XLANG=XMIN
      END IF
      EXPO=INT(LOG10(DABS(XTIK)))
      WRITE (UNIT=CH16,FMT='(F16.7)') XLANG
      CALL FIBLA(CH16,I1)
      I2=INDEX(CH16,'.')
      IVOR=I2-I1
      INACH=MAX(-EXPO+1,-1)
      IF (INACH.EQ.0) INACH=-1
      C1X=IVOR+INACH+1
      C2X=INACH
      X1=U
      SIDE=-1.0D0
      CALL XAXIS(XTEXT,X1,SIDE,XTIK,GRS,GRZ,I1X,I2X,I3,KUR,LAN,C1X,C2X)
      X1=O
      SIDE=1.0D0
      CALL XAXIS(BTEXT,X1,SIDE,XTIK,GRS,GRZ,I1X,I0,I3,KUR,LAN,C1X,C2X)
!----
      YTIK=(YMAX-YMIN)/10.0D0
      IF (ABS(YMAX).GT.ABS(YMIN)) THEN
      XLANG=YMAX
      ELSE
      XLANG=YMIN
      END IF
      EXPO=INT(LOG10(DABS(YTIK)))
      WRITE (UNIT=CH16,FMT='(F16.7)') XLANG
      CALL FIBLA(CH16,I1)
      I2=INDEX(CH16,'.')
      IVOR=I2-I1
      INACH=MAX(-EXPO+1,-1)
      IF (INACH.EQ.0) INACH=-1
      C1X=IVOR+INACH+1
      C2X=INACH
      X1=L
      SIDE=-1.0D0
      CALL YAXIS(YTEXT,X1,SIDE,YTIK,GRS,GRZ,I1X,I2X,I3,KUR,LAN,C1X,C2X)
      X1=R
      SIDE=1.0D0
      CALL YAXIS(BTEXT,X1,SIDE,YTIK,GRS,GRZ,I1X,I0,I3,KUR,LAN,C1X,C2X)
!----
      GRS=0.5D0
      X=B+2.0D0
      Y=H-GRS
      THETA=0.0D0
      CALL VPTEXT(X,Y,GRS,THETA,TITLE)
!----
      Y=Y-1.5D0*GRS
      X=B+6.0D0
      GRS=0.20D0
      Y=Y+1.5D0*GRS
      DO 810,I=1,NCOMIN
      CH001=COMINS(I)
      Y=Y-1.5D0*GRS
      CALL VPTEXT(X,Y,GRS,THETA,CH001)
  810 CONTINUE
!----
      GRS=0.25D0
      X=0.0D0
      Y=H+1.0D0-1.5D0*GRS
      DO 820,I=1,NBUL
      CH001=BULINE(I)
      Y=Y+1.5D0*GRS
      CALL VPTEXT(X,Y,GRS,THETA,CH001)
  820 CONTINUE
!----
      GRS=0.2D0
      X=B-0.0D0
      Y=-2.0D0
      CALL VPTEXT(X,Y,GRS,THETA,DATI)
!----
      X=B+1.0D0
      Y=0.25D0
      GRS=0.5D0
      SYM=97.0D0
      CALL SYMBOL(X,Y,SYM,GRS)
      SYM=98.0D0
      CALL SYMBOL(X,Y,SYM,GRS)
      X=-0.20D0
      Y=H+0.20D0
      GRS=0.25D0
      SYM=99.0D0
      CALL SYMBOL(X,Y,SYM,GRS)
!----
      IF (SYSTEMA.EQ.3) THEN
      XT=X0+BREITE/2.0D0
      YT=Y0+HOEHE
      X=X0+BREITE
      Y=Y0
      IF (MAKEPS) THEN
      WRITE (pst,3080) X0,Y0,FAT,XT,YT,X,Y
 3080 FORMAT (2(F8.3,1X),'moveto',/ &
      F6.3,2X,'setlinewidth',/ &
      2(F8.3,1X),'lineto',/ &
      2(F8.3,1X),'lineto  stroke')
      END IF
      IF (MAKESVG) THEN
      J1=1
      J2=0
      CALL SVGPATH(J1,J2)
      WRITE (svg,3085) X0*CMPX,595.0D0-Y0*CMPX,XT*CMPX, &
      595.0D0-YT*CMPX,X*CMPX,595.0D0-Y*CMPX
 3085 FORMAT (' M ',F10.4,1X,F10.4,' L ',F10.4,1X,F10.4, &
      ' L ',F10.4,1X,F10.4,'"/>')
      END IF
      END IF
!----
      FRANGE=FMAX-FMIN
      CALL XLEG
      IF (MAKEPS) THEN
      WRITE (pst,3070)
 3070 FORMAT ('showpage')
      END IF

      IF (MAKESVG) THEN
      WRITE (svg,1005)
 1005 FORMAT (/'</g>',/,'</svg>')
      CLOSE (UNIT=svg)
      END IF

!----
      CALL CPUTIME(ZEITSTRING)
      CALL LABLA(ZEITSTRING,I001)
      WRITE (scr,150) ZEITSTRING(1:I001)
  150 FORMAT (/,' exit MAKEMAP',/,1X,A)
!----
      END
!-----
!******************************
      SUBROUTINE INCALC(I1,I2)
      IMPLICIT NONE
      INCLUDE 'map.cmn'
!====
      INTEGER(4)  I1,I2
!---
      IF (FF0(I2).EQ.MISSING) THEN
      FLINE(I1)=255
      ELSE
      FLINE(I1)=IDINT(FF0(I2)*FA+CMAX-FA*FMAX)
      END IF
      IF (FLINE(I1).GT.255) FLINE(I1)=255
      IF (FLINE(I1).LT.0) FLINE(I1)=0
!----
      RETURN
      END
!-----
!******************************
      SUBROUTINE XLEG
      IMPLICIT NONE
      INCLUDE 'files.cmn'
      INCLUDE 'map.cmn'
!====
      INTEGER(4)  I,I1,I2,INACH,DELI,EXPO,J1
      REAL(8) X,Y,GRAU,STR,CRANGE,XLANG,DELY,DICKE
      CHARACTER(20) CH16
!---
      CRANGE=CMAX-CMIN
      DELI=-1
      IF (CRANGE.LT.0) DELI=1
      DELY=DABS(10.0D0/CRANGE)
      DICKE=1.5D0*DELY
      X=X0+BREITE+2.0D0
!      Y=Y0-HOEHE+10.0D0
      Y=Y0+10.0D0
      IF (MAKEPS) THEN
      WRITE (pst,1000) DICKE
 1000 FORMAT (F8.4,' setlinewidth')
      END IF

      DO 500,I=IDINT(CMAX),IDINT(CMIN),DELI

      GRAU=DBLE(I)/255.0D0
      IF (MAKEPS) THEN
      WRITE (pst,1005) GRAU,X,Y
 1005 FORMAT (F6.3,'  setgray  ', &
      F6.2,2X,F6.2,'  moveto', &
      '  1  0  rlineto  stroke')
      END IF

      IF (MAKESVG) THEN
      LINCOL=0
      FILCOL=0
      LGRAY=GRAU
      CALL SVGCOLS
      CALL LABLA(SVGSTRO,J1)
      WRITE (svg,2000) DICKE*CMPX,SVGSTRO(1:J1), &
      (X)*CMPX,595.0D0-Y*CMPX, &
      (X+1.0D0)*CMPX,595.0D0-Y*CMPX
 2000 FORMAT ('<path stroke-width="',F10.4,'"', &
      ' stroke="',A,'" fill="none" d=" M',2F10.4,' L',2F10.4, &
      '" />')
      LINCOL=0
      FILCOL=0
      LGRAY=0.0D0
      CALL SVGCOLS
      END IF
      FAT=0.02

      Y=Y-DELY
  500 CONTINUE
!----
      IF (MAKEPS) THEN
      WRITE (pst,1010)
 1010 FORMAT ('/Helvetica findfont 0.4 scalefont setfont'/ &
      '0  setgray  0.02  setlinewidth')
      END IF
      X=X0+BREITE+3.5D0
      Y=Y0+10.0D0-0.1D0
!====
      IF (ABS(FMAX).GT.ABS(FMIN)) THEN
      XLANG=FMAX
      ELSE
      XLANG=FMIN
      END IF
      EXPO=INT(LOG10(DABS(FRANGE/10.0D0)))
      CH16=' '
      WRITE (UNIT=CH16,FMT='(F20.7)') XLANG
      CALL FIBLA(CH16,I1)
      I2=INDEX(CH16,'.')
      INACH=MAX(-EXPO+2,-1)
      IF (INACH.EQ.0) INACH=-1
      IF (INACH.GT.7) INACH=7

      DO 600,I=0,10
!      STR=1.0D0-DBLE(I)*0.1D0
      STR=FMAX-DBLE(I)*0.1D0*FRANGE
      CH16=' '
      WRITE (UNIT=CH16,FMT='(F20.7)') STR
      CALL FIBLA(CH16,I1)
      I2=INDEX(CH16,'.')
      IF (MAKEPS) THEN
      WRITE (pst,1014) X-0.5D0,Y+0.1D0,X-0.3D0,Y+0.1D0
 1014 FORMAT (F6.2,2X,F6.2,'  moveto  ', &
      F6.2,2X,F6.2,'  lineto  stroke')
      WRITE (pst,1015) X,Y,CH16(I1:I2+INACH)
 1015 FORMAT (F6.2,2X,F6.2,'  moveto', &
      '  (',A,')  show')
      END IF

      IF (MAKESVG) THEN
      WRITE (svg,2010) FAT*CMPX,SVGSTRO(1:J1), &
      (X-0.5D0)*CMPX,595.0D0-(Y+0.1D0)*CMPX, &
      (X-0.3D0)*CMPX,595.0D0-(Y+0.1D0)*CMPX
 2010 FORMAT ('<path stroke-width="',F10.4,'"', &
      ' stroke="',A,'" fill="none" d=" M',2F10.4,' L',2F10.4, &
      '" />')

      WRITE (svg,2020) (X)*CMPX,595.0D0-(Y)*CMPX,0.4D0*CMPX, &
      CH16(I1:I2+INACH)
 2020 FORMAT ('<text x="',F10.4,'" y="',F10.4, &
      '" font-family="Arial" font-size="',F10.4, &
      '" style="fill: #000000; stroke: none">',A,'</text>')
      END IF

      Y=Y-1.0D0
  600 CONTINUE
!----
      RETURN
      END
!-----
!******************************
      SUBROUTINE DERIVX1(KEY,XCORR,YCORR)
      IMPLICIT NONE
      INCLUDE 'files.cmn'
      INCLUDE 'map.cmn'
!====
      INTEGER(4) I,II,NX,NY,NMAX,JJ
      REAL(8) DX,DY,XCORR,YCORR,FF1(MAXPIX)
      CHARACTER(16) KEY
      CHARACTER(132) CH001
!----
      NX=IXDIM
      NY=IYDIM
      DX=(XMAX-XMIN)/DBLE(NX)
      DY=(YMAX-YMIN)/DBLE(NY)
      NMAX=(NX)*(NY)
      IF (KEY.EQ.'x1') GOTO 1
      IF (KEY.EQ.'y1') GOTO 2
      IF (KEY.EQ.'alpha') GOTO 3
      IF (KEY.EQ.'beta') GOTO 4
      IF (KEY.EQ.'xy') GOTO 5
      RETURN
!=====================
!++++ Fall 1: (df/dx)
!=====================
    1 XCORR=BREITE/(2.0D0*DBLE(NX))
      CH001='(d/dx)'//TITLE(1:120)
      TITLE=CH001
      DO I=1,NMAX,NX
      DO II=0,NX-1
        JJ=I+II
        IF (II.LT.(NX-1).AND.FF0(JJ).NE.MISSING &
        .AND.FF0(JJ+1).NE.MISSING) THEN
        FF1(JJ)=(FF0(JJ+1)-FF0(JJ))/DX
        ELSE
        FF1(JJ)=MISSING
        END IF
      END DO
      END DO
      GOTO 999
!=====================
!++++ Fall 2: (df/dy)
!=====================
    2 YCORR=HOEHE/(2.0D0*DBLE(NY))
      CH001='(d/dy)'//TITLE(1:120)
      TITLE=CH001
      DO I=1,NMAX,NX
      DO II=0,NX-1
        JJ=I+II
        IF (I.LT.(NMAX-NX+1).AND.FF0(JJ).NE.MISSING &
        .AND.FF0(JJ+NX).NE.MISSING) THEN
        FF1(JJ)=(FF0(JJ+NX)-FF0(JJ))/DY
        ELSE
        FF1(JJ)=MISSING
        END IF
      END DO
      END DO
      GOTO 999
!===========================
!++++ Fall 3: (1/f)*(df/dx)
!===========================
    3 XCORR=BREITE/(2.0D0*DBLE(NX))
      CH001='(alpha)'//TITLE(1:120)
      TITLE=CH001
      DO I=1,NMAX,NX
      DO II=0,NX-1
        JJ=I+II
        IF (II.LT.(NX-1).AND.FF0(JJ).NE.MISSING &
        .AND.FF0(JJ+1).NE.MISSING) THEN
        FF1(JJ)=(FF0(JJ+1)-FF0(JJ))/DX/((FF0(JJ)+FF0(JJ+1))/2.0D0)
        ELSE
        FF1(JJ)=MISSING
        END IF
      END DO
      END DO
      GOTO 999
!============================
!++++ Fall 4: -(1/f)*(df/dy)
!============================
    4 YCORR=HOEHE/(2.0D0*DBLE(NY))
      CH001='(beta)'//TITLE(1:120)
      TITLE=CH001
      DO I=1,NMAX,NX
      DO II=0,NX-1
        JJ=I+II
        IF (I.LT.(NMAX-NX+1).AND.FF0(JJ).NE.MISSING &
        .AND.FF0(JJ+NX).NE.MISSING) THEN
        FF1(JJ)=-(FF0(JJ+NX)-FF0(JJ))/DY/((FF0(JJ)+FF0(JJ+NX))/2.0D0)
        ELSE
        FF1(JJ)=MISSING
        END IF
      END DO
      END DO
      GOTO 999
!=============================
!++++ Fall 5: (df/dx)+(df/dy)
!=============================
    5 YCORR=HOEHE/(2.0D0*DBLE(NY))
      XCORR=BREITE/(2.0D0*DBLE(NX))
      CH001='(df/dxy)'//TITLE(1:120)
      TITLE=CH001
      DO I=1,NMAX,NX
      DO II=0,NX-1
        JJ=I+II
        IF (II.LT.(NX-1).AND.I.LT.(NMAX-NX+1).AND.FF0(JJ).NE.MISSING &
        .AND.FF0(JJ+NX).NE.MISSING.AND.FF0(JJ+1).NE.MISSING) THEN
        FF1(JJ)=(FF0(JJ+1)-FF0(JJ))/DX+(FF0(JJ+NX)-FF0(JJ))/DY
        ELSE
        FF1(JJ)=MISSING
        END IF
      END DO
      END DO
      GOTO 999
!====
  999 CONTINUE
!      WRITE (scr,3000) (FF0(I),I=1,NX)
!      WRITE (scr,3000) (FF1(I),I=1,NX)
! 3000 FORMAT (200E10.3)
      FMIN=1D40
      FMAX=-1D40
      DO 600,I=1,NMAX
      IF (FF1(I).NE.MISSING) THEN
      IF (FF1(I).GT.FMAX) FMAX=FF1(I)
      IF (FF1(I).LT.FMIN) FMIN=FF1(I)
      END IF
      FF0(I)=FF1(I)
  600 CONTINUE
!---
!      WRITE (scr,3010) FMIN,FMAX
! 3010 FORMAT ('FMIN=',1PE20.12,2X,'FMAX=',1PE20.12)
!====
      RETURN
      END
!
!************************************************************
!******************* explot things **************************
!************************************************************
!
!***************
      SUBROUTINE XAXIS(TEXT,X1,SIDE,TIK,GRS,GRZ,I1,I2,I3,KUR,LAN,C1,C2)
      IMPLICIT NONE
      INCLUDE 'map.cmn'
!----- end of common variables
      CHARACTER*(*) TEXT
      CHARACTER(32) CH16,NUMMER
      REAL(8) X1,SIDE,TIK,GRS,GRZ,KUR,LAN,SIZE,SID,XX,YY,FF,THETA, &
      XKOR,YKOR,ZERO,X,FTIK,F3,F4
      INTEGER(4) I,II,I1,I2,I3,LAE,C1,C2,J1,J2,C4
!==== C1 is only used in YAXIS to define distance for label
      C4=C1
      THETA=0.0D0
      ZERO=0.0D0
      I=I3
      FTIK=ABS(TIK/100000.0D0)
      YY=(X1-U)*H/(O-U)
      SID=SIDE/ABS(SIDE)
!--
      J1=1
      J2=0
      CALL SVGPATH(J1,J2)
!-F90      DO 500,X=L,R+TIK/10.0D0,TIK
      X=L
      DO WHILE(X.LE.(R+TIK/10.0D0))
      XX=(X-L)*B/(R-L)
      I=I+1
      IF (MOD(I,I1).EQ.0) THEN
      F3=LAN
      ELSE
      F3=KUR
      END IF
      IF (ABS(SIDE).GT.1) THEN
      F4=F3
      ELSE
      F4=0.0D0
      END IF
      FF=YY-SID*F4
      CALL VPMOVE(XX,FF)
      FF=YY+SID*F3
      CALL VPDRAW(XX,FF)
      CALL VPSTROKE
      IF (I2.NE.0) THEN
      IF (MOD(I,I2).EQ.0) THEN
!     FF=L+DBLE(I)*TIK
      IF (X.LT.-FTIK) THEN
      FF=X-FTIK
      ELSE
      FF=X+FTIK
      END IF
      WRITE (UNIT=CH16,FMT='(F32.7)') FF
      CALL FIBLA(CH16,II)
      NUMMER=CH16(II:)
      LAE=INDEX(NUMMER,'.')+C2
      DO II=LAE+1,32
        NUMMER(II:II)=' '
      END DO
      DO II=1,LAE
        IF (NUMMER(II:II).EQ.' ') NUMMER(II:II)='0'
      END DO
      YKOR=YY-GRZ/2.0D0+SID*(F3+GRZ)
      CALL PSMLEN(NUMMER,LAE,GRZ,SIZE)
      XKOR=XX-SIZE/2.0D0
      CALL SVGPATHEND
        CALL VPTEXT(XKOR,YKOR,GRZ,THETA,NUMMER)
      CALL SVGPATH(J1,J2)
      END IF
      END IF
      X=X+TIK
      END DO
!-F90  500 CONTINUE
      CALL VPMOVE(B,YY)
      CALL VPDRAW(ZERO,YY)
      CALL VPSTROKE
      CALL SVGPATHEND

      LAE=INDEX(TEXT,'  ')-1
      CALL PSMLEN(TEXT,LAE,GRS,SIZE)
      XKOR=B/2.0D0-SIZE/2.0D0
      FF=DMAX1(LAN,KUR,ZERO)
!     YKOR=YY-GRS/2.0D0+SID*(LAN+GRZ*1.5D0+GRS)
      YKOR=YY-GRS/2.0D0+SID*(FF+GRZ*1.5D0+GRS)
      CALL VPTEXT(XKOR,YKOR,GRS,THETA,TEXT)
      RETURN
      END
!
!***************
      SUBROUTINE YAXIS(TEXT,X1,SIDE,TIK,GRS,GRZ,I1,I2,I3,KUR,LAN,C1,C2)
      IMPLICIT NONE
      INCLUDE 'map.cmn'
!----- end of common variables
      CHARACTER*(*) TEXT
      CHARACTER(32) CH16,NUMMER
      REAL(8) X1,SIDE,TIK,GRS,GRZ,KUR,LAN,SIZE,SID,XX,YY,FF,THETA, &
      XKOR,YKOR,ZERO,FFF,Y,FTIK,F3,F4
      INTEGER(4) I,II,I1,I2,I3,LAE,C1,C2,J1,J2
!==== C1 is only used in YAXIS to define distance for label
      THETA=0.0D0
      ZERO=0.0D0
      CH16='00000000000000000000000000000000'
      CALL PSMLEN(CH16,C1,GRZ,FFF)
      I=I3
      FTIK=ABS(TIK/100000.0D0)
      XX=(X1-L)*B/(R-L)
      SID=SIDE/ABS(SIDE)
!--
      J1=1
      J2=0
      F3=0.0D0
      CALL SVGPATH(J1,J2)
!-F90      DO 500,Y=U,O+TIK/10.0D0,TIK
      Y=U
      DO WHILE(Y.LE.(O+TIK/10.0D0))
      YY=(Y-U)*H/(O-U)
      I=I+1
      IF (MOD(I,I1).EQ.0) THEN
      F3=LAN
      ELSE
      F3=KUR
      END IF
      IF (ABS(SIDE).GT.1) THEN
      F4=F3
      ELSE
      F4=0.0D0
      END IF
      FF=XX-SID*F4
      CALL VPMOVE(FF,YY)
      FF=XX+SID*F3
      CALL VPDRAW(FF,YY)
      CALL VPSTROKE
      IF (I2.NE.0) THEN
      IF (MOD(I,I2).EQ.0) THEN
!     FF=U+DBLE(I)*TIK
      IF (Y.LT.-FTIK) THEN
      FF=Y-FTIK
      ELSE
      FF=Y+FTIK
      END IF
      WRITE (UNIT=CH16,FMT='(F32.7)') FF
      CALL FIBLA(CH16,II)
      NUMMER=CH16(II:)
      LAE=INDEX(NUMMER,'.')+C2
      DO II=LAE+1,32
        NUMMER(II:II)=' '
      END DO
      DO II=1,LAE
        IF (NUMMER(II:II).EQ.' ') NUMMER(II:II)='0'
      END DO
      CALL PSMLEN(NUMMER,LAE,GRZ,SIZE)
      IF (SID.EQ.-1.0D0) THEN
      XKOR=XX-SIZE-F3-GRZ/2.0D0
      ELSE
      XKOR=XX+F3+GRZ/2.0D0+FFF-SIZE
      END IF
      YKOR=YY-GRZ/2.0D0
      CALL SVGPATHEND
        CALL VPTEXT(XKOR,YKOR,GRZ,THETA,NUMMER)
      CALL SVGPATH(J1,J2)
      END IF
      END IF
      Y=Y+TIK
      END DO
!-F90  500 CONTINUE
      CALL VPMOVE(XX,H)
      CALL VPDRAW(XX,ZERO)
      CALL VPSTROKE
      CALL SVGPATHEND

      LAE=INDEX(TEXT,'  ')-1
      XKOR=XX+GRS/2.0D0+SID*(F3+FFF+GRZ/2.0D0+GRS*1.5D0)
      CALL PSMLEN(TEXT,LAE,GRS,SIZE)
      YKOR=H/2.0D0-SIZE/2.0D0
      THETA=90.0D0
      CALL VPTEXT(XKOR,YKOR,GRS,THETA,TEXT)
      RETURN
      END
!
!***************
      SUBROUTINE PSMLEN(CH,LL,GRS,SIZE)
      INCLUDE 'map.cmn'
!
      CHARACTER*(*) CH
      CHARACTER(1) A1
      REAL(8) VOR(0:126),NACH(0:126),BREI(0:126)
      REAL(8) GRS,SIZE,WID,KOR
      INTEGER(4) LL,I1,I,CODE
!====
      DATA VOR/32*0.0D0,2.7828D0,1.2438D0,0.5190D0,0.1417D0, &
      0.3366D0,0.2799D0,0.5228D0,0.6364D0,0.7375D0, &
      0.3755D0,0.4072D0,0.5177D0,0.8752D0,0.4570D0, &
      0.8751D0,-0.0814D0,0.4412D0,1.0085D0,0.3372D0, &
      0.3228D0,0.2820D0,0.3568D0,0.4256D0,0.4595D0, &
      0.3815D0,0.3768D0,1.1000D0,1.1096D0,0.4532D0, &
      0.5025D0,0.5021D0,0.7817D0,0.3652D0,0.1675D0, &
      0.8002D0,0.4745D0,0.8849D0,0.9048D0,0.9030D0, &
      0.4320D0,0.8278D0,0.9982D0,0.1723D0,0.7968D0, &
      0.7988D0,0.7480D0,0.7636D0,0.3844D0,0.9193D0, &
      0.3844D0,0.9332D0,0.4695D0,0.2010D0,0.8623D0, &
      0.2936D0,0.2032D0,0.2216D0,0.1036D0,0.2810D0, &
      0.6478D0,-0.0814D0,0.2372D0,0.4427D0,-0.2169D0, &
      0.6461D0,0.4287D0,0.5482D0,0.3008D0,0.2646D0, &
      0.4061D0,0.1684D0,0.2996D0,0.7096D0,0.6669D0, &
      -0.1672D0,0.5795D0,0.6865D0,0.7084D0,0.7044D0, &
      0.3618D0,0.5482D0,0.2646D0,0.7002D0,0.3518D0, &
      0.1443D0,0.6555D0,0.0993D0,0.0773D0,0.1770D0, &
      0.2027D0,0.3075D0,0.4379D0,1.0015D0,0.2913D0, &
      0.7567D0/
      DATA NACH/32*0.0D0,15*0.0D0,-0.0778D0, &
      44*0.0D0,-0.0778D0,2*0.0D0,-0.2294D0, &
      11*0.0D0,-0.0432D0,19*0.0D0/
      DATA BREI/32*0.0D0,2.78D0,2.78D0,3.55D0,5.56D0, &
      5.56D0,8.89D0,6.67D0,2.22D0,3.33D0,3.33D0,3.89D0,5.84D0,2.78D0, &
      3.33D0, &
      2.78D0,2.78D0,5.56D0,5.56D0,5.56D0,5.56D0,5.56D0,5.56D0,5.56D0, &
      5.56D0, &
      5.56D0,5.56D0,2.78D0,2.78D0,5.84D0,5.84D0,5.84D0,5.56D0,10.15D0, &
      6.67D0, &
      6.67D0,7.22D0,7.22D0,6.67D0,6.11D0,7.78D0,7.22D0,2.78D0,5.00D0, &
      6.67D0, &
      5.56D0,8.33D0,7.22D0,7.78D0,6.67D0,7.78D0,7.22D0,6.67D0,6.11D0, &
      7.22D0, &
      6.67D0,9.44D0,6.67D0,6.67D0,6.11D0,2.78D0,2.78D0,2.78D0,4.69D0, &
      5.56D0, &
      2.22D0,5.56D0,5.56D0,5.00D0,5.56D0,5.56D0,2.78D0,5.56D0,5.56D0, &
      2.22D0, &
      2.22D0,5.00D0,2.22D0,8.33D0,5.56D0,5.56D0,5.56D0,5.56D0,3.33D0, &
      5.00D0, &
      2.78D0,5.56D0,5.00D0,7.22D0,5.00D0,5.00D0,5.00D0,3.34D0,2.60D0, &
      3.34D0, &
      5.84D0/
      VOR(32)=0.6364D0
      BREI(32)=3.33D0
      WID=0.0D0
      CODE=0
      IF (LL.EQ.0) LL=1
      DO 500,I=1,LL
      A1=CH(I:I)
      I1=ICHAR(A1)
      IF (CODE.EQ.1) THEN
      CODE=0
      ELSE
      IF (I1.EQ.92) THEN
      CODE=1
      ELSE
      WID=WID+BREI(I1)
      END IF
      END IF
  500 CONTINUE
      KOR=VOR(ICHAR(CH(1:1)))+NACH(ICHAR(CH(LL:LL)))
      SIZE=(WID-KOR)*GRS*0.13715D0
      RETURN
      END
!
!***************
      SUBROUTINE SYMBOL(X,Y,SYM,GRS)
      IMPLICIT NONE
      INCLUDE 'map.cmn'
!----- end of common variables
      REAL(8) X,Y,SYM,GRS,ASYM,FSYM,THETA,GRS2,SQ2, &
      XP,XM,YP,YM,XPD,XMD,YPD,YMD,X1,Y1,XCDCH(21),YCDCH(21)
      INTEGER(4) ISYM,I,I1,I2
!====
      DATA XCDCH/0.0D0,-0.25D0,-0.125D0,-3.25D0,-2.75D0,-5.5D0,-4.5D0, &
      -5.0D0,-3.75D0,-3.25D0,-1.5D0,-2.0D0,-1.0D0,0.0D0, &
      1.8D0,1.8D0,6.0D0,6.0D0,1.8D0,1.8D0,0.0D0/
      DATA YCDCH/-6.0D0,-6.0D0,-3.5D0,-4.0D0,-3.0D0,-0.5D0,0.5D0, &
      2.0D0,1.75D0,3.0D0,1.0D0,4.5D0,4.0D0,6.0D0, &
      6.0D0,1.8D0,1.8D0,-1.8D0,-1.8D0,-6.0D0,-6.0D0/
      ASYM=ABS(SYM)
      ISYM=INT(SYM)
      FSYM=DBLE(ISYM)
      GRS2=GRS/2.0D0
      THETA=0.0D0

      IF (ASYM.GT.2.9D0.AND.ASYM.LT.10.6D0) THEN
      IF (FSYM.NE.SYM) THEN
      THETA=180.0D0/FSYM
      END IF
      CALL POLY(X,Y,ISYM,GRS2,THETA)
      RETURN
      END IF

      IF (SYM.EQ.1.0D0) CALL VPPOINT(X,Y,GRS,1)
      IF (SYM.EQ.-1.0D0) CALL VPPOINT(X,Y,GRS,-1)
      IF (SYM.EQ.1.0D0) CALL SVGCIRCLE(X,Y,GRS,1)
      IF (SYM.EQ.-1.0D0) CALL SVGCIRCLE(X,Y,GRS,-1)

      XP=X+GRS2
      XM=X-GRS2
      YP=Y+GRS2
      YM=Y-GRS2

      I1=1
      I2=0

      IF (SYM.EQ.0.0D0) THEN
       CALL SVGPATH(I1,I2)
       CALL VPMOVE(X,YM)
       CALL VPDRAW(X,YP)
       CALL VPSTROKE
       CALL SVGPATHEND
      END IF
       IF (SYM.EQ.2.0D0) THEN
       CALL SVGPATH(I1,I2)
       CALL VPMOVE(X,YP)
       CALL VPDRAW(X,YM)
       CALL VPMOVE(XM,Y)
       CALL VPDRAW(XP,Y)
       CALL VPSTROKE
       CALL SVGPATHEND
      END IF
      SQ2=GRS2/SQRT(2.0D0)
      XPD=X+SQ2
      XMD=X-SQ2
      YPD=Y+SQ2
      YMD=Y-SQ2
      IF (SYM.EQ.-2.0D0) THEN
       CALL SVGPATH(I1,I2)
       CALL VPMOVE(XMD,YMD)
       CALL VPDRAW(XPD,YPD)
       CALL VPMOVE(XMD,YPD)
       CALL VPDRAW(XPD,YMD)
       CALL VPSTROKE
       CALL SVGPATHEND
      END IF
      IF (SYM.EQ.11.0D0) THEN
       CALL SVGPATH(I1,I2)
       CALL VPMOVE(XMD,YMD)
       CALL VPDRAW(XPD,YPD)
       CALL VPMOVE(X,YP)
       CALL VPDRAW(X,YM)
       CALL VPMOVE(XPD,YMD)
       CALL VPDRAW(XMD,YPD)
       CALL VPMOVE(XM,Y)
       CALL VPDRAW(XP,Y)
       CALL VPSTROKE
       CALL SVGPATHEND
      END IF
      IF (SYM.EQ.12.0D0) THEN
       CALL VPPOINT(X,Y,GRS,1)
       CALL SVGCIRCLE(X,Y,GRS,1)
       CALL SVGPATH(I1,I2)
       CALL VPMOVE(X,YP)
       CALL VPDRAW(X,YM)
       CALL VPMOVE(XPD,YMD)
       CALL VPDRAW(X,Y)
       CALL VPDRAW(XMD,YMD)
       CALL VPSTROKE
       CALL SVGPATHEND
      END IF
      IF (SYM.EQ.13.0D0) THEN
       CALL SVGPATH(I1,I2)
       CALL VPMOVE(X,YM)
       CALL VPDRAW(X,YP)
       CALL VPMOVE(XMD,Y)
       CALL VPDRAW(X,YP)
       CALL VPDRAW(XPD,Y)
       CALL VPSTROKE
       CALL SVGPATHEND
      END IF
      IF (SYM.EQ.97.0D0) THEN
       CALL SVGPATH(I1,I2)
       CALL VPMOVE(X,Y)
       CALL SVGPATHEND
       CALL GREYCDCH(X,Y,GRS)
      END IF
      IF (SYM.EQ.99.0D0.OR.SYM.EQ.98.0D0) THEN
        CALL SVGPATH(I1,I2)
        CALL VPMOVE(X,YM)
        DO I=2,21
          X1=X+(XCDCH(I)/6.0D0)*GRS2
          Y1=Y+(YCDCH(I)/6.0D0)*GRS2
          CALL VPDRAW(X1,Y1)
        END DO
        CALL VPSTROKE
        CALL SVGPATHEND
        IF (SYM.EQ.98.0D0) THEN
         CALL VPMOVE (X,Y)
         CALL VPTHERDOM(GRS)
         CALL SVGTHERDOM(X,Y,GRS)
        END IF
      END IF
      CALL VPMOVE (X,Y)

      RETURN
      END
!
!***************
      SUBROUTINE POLY(X,Y,N,R2,TH)
      IMPLICIT NONE
      INCLUDE 'map.cmn'
!----- end of common variables
      REAL(8) X,Y,R2,TH,PI,DELAL,AL1,AL,XX,YY
      INTEGER(4) I,N,NN,I1,I2
!====
      DATA PI/3.141592654D0/
      IF (IABS(N).LE.1) THEN
      R2=R2*2.0D0
      TH=DBLE(N)
!!??
      CALL SYMBOL(X,Y,TH,R2)
!!??
      RETURN
      END IF

      I1=1
      I2=0
      IF (N.LT.0) I2=1
      CALL SVGPATH(I1,I2)

      TH=TH/57.29577951D0
      NN=IABS(N)
      DELAL=2*PI/NN
      AL1=TH-PI/2.0D0-DELAL/2.0D0
      AL=AL1
      XX=X+R2*COS(AL)
      YY=Y+R2*SIN(AL)
      CALL VPMOVE(XX,YY)
      DO I=1,NN-1
        AL=AL+DELAL
        XX=X+R2*COS(AL)
        YY=Y+R2*SIN(AL)
        CALL VPDRAW(XX,YY)
      END DO
      CALL VPCLOSE
      IF (N.GT.0) THEN
      CALL VPSTROKE
      ELSE
      CALL VPFILL
      END IF
      CALL VPMOVE(X,Y)

      CALL SVGPATHEND

      RETURN
      END
!
!***************
      SUBROUTINE VPTHERDOM(GRS)
      IMPLICIT NONE
      INCLUDE 'files.cmn'
      INCLUDE 'map.cmn'
!
      REAL(8) GRS,GRSS,GRSR,GRSM
      IF (MAKEPS) THEN
      GRSS=GRS*0.25D0
      GRSR=GRS*0.625D0
      GRSM=GRS/25.0D0
      WRITE(pst,1000) GRSM,GRSS,GRSR,GRSR
 1000 FORMAT ('gsave ',F7.4,' 0  rmoveto ', &
      /,'currentpoint translate ',F7.4,' ff ',/'125 rotate', &
      /,'0 ',F7.4,'  moveto',/,'{pop pop  ', &
      /'/theta currentpoint atan def ', &
      /'0 0 moveto theta neg rotate 0 ',f7.4,' moveto} ', &
      /'(T h e r i a k  -  D o m i n o) kshow ', &
      'grestore ')
      RETURN
      END IF
      END
!
!***************
      SUBROUTINE SVGTHERDOM(X,Y,GRS)
      IMPLICIT NONE
      INCLUDE 'map.cmn'
!----- end of common variables
      REAL(8) X,Y,GRS,RX,RY,FSIZ,DELX,DELY
      IF (MAKESVG) THEN
      FSIZ=GRS*0.25D0
      DELX=GRS*0.442D0
      DELY=GRS*0.442D0
!      RX=GRS*0.7071D0
!      RY=GRS*0.7071D0
      RX=DSQRT(DELX**2+DELY**2)
      RY=DSQRT(DELX**2+DELY**2)

      WRITE(svg,1000) (X+X0-DELX)*CMPX, &
      595.0D0-(Y+Y0-DELY)*CMPX, &
      RX*CMPX,RY*CMPX, &
      (X+X0+DELX)*CMPX, &
      595.0D0-(Y+Y0-DELY)*CMPX
 1000 FORMAT ('<defs> ', &
      ' <path id="kreis" d="M ',F10.4,',',F10.4,' A', &
      F10.4,',',F10.4,' 0 1 1 ', &
      F10.4,',',F10.4,'"/> </defs>')
      WRITE (svg,1010) FSIZ*CMPX
 1010 FORMAT ('<text font-size="',F10.4,'" ', &
      ' font-family="Arial" ', &
      'stroke="none">',/,'<textPath xlink:href="#kreis" >', &
      'T h e r i a k  -  D o m i n o', &
      '</textPath> </text>')
!      WRITE (svg,1020)
! 1020 FORMAT ('<use xlink:href="#kreis" stroke ="gray" fill="none"/>')
      END IF
      RETURN
      END
!
!***************
      SUBROUTINE GREYCDCH(X,Y,GRS)
      IMPLICIT NONE
      INCLUDE 'files.cmn'
      INCLUDE 'map.cmn'
!----- end of common variables
      REAL(8) GRS,X,Y,XC,YC,XMUL,YMUL,FF,XCOR,RX,RY
      INTEGER(4) NX,NY,J,IX,IY
      CHARACTER(2) HXCOL
      CHARACTER(60) CDCH(31)
      DATA CDCH / &
      'FFFFFFFFFFFFFFFFFFFFFFFFFFFFCEBDB59C8CEFFFFFFFFFFFFFFFFFFFFF', &
      'FFFFFFFFFFFFFFFFFFFFFFFFFFE7C6BDAD947BEFFFFFFFFFFFFFFFFFFFFF', &
      'FFFFFFFFFFFFFFFFFFFFFFFFFFD6C6B5A58C6BEFFFFFFFFFFFFFFFFFFFFF', &
      'FFFFFFFFFFFFFFFFFFFFFFFFEFCEC6B59C7373EFFFFFFFFFFFFFFFFFFFFF', &
      'FFFFFFFFFFFFFFFFFFF7F7F7DECEBDAD8C6B84F7FFFFFFFFFFFFFFFFFFFF', &
      'FFFFFFFFFFFFFFFFFFF7E7E7D6C6BD9C737394F7FFFFFFFFFFFFFFFFFFFF', &
      'FFFFFFFFFFFFFFFFFFF7E7DECEBDAD8C6B7B9CF7FFFFFFFFFFFFFFFFFFFF', &
      'FFFFFFFFFFFFFFFFFFF7E7D6C6B59C73738CA5F7FFFFFFFFFFFFFFFFFFFF', &
      'FFFFFFFFFFF7FFFFFFF7D6CEBDA584637B94B5F7FFFFFFFFFFFFFFFFFFFF', &
      'FFFFFFFFFFF7EFF7FFF7D6BDAD8C636B7BA5ADF7FFFFFFFFFFFFFFFFFFFF', &
      'FFF7F7FFF7EFEFE7F7FFCEB594635A638494A5E7EFEFEFEFEFEFEFE7EFE7', &
      'FFF7EFEFEFEFE7DED6EFC6946B636B737B8494A5ADADB5ADADA59C9C8C94', &
      'FFFFE7E7E7E7DED6C6BDAD63637B7B7B7B848494A5A5ADADADA59C948C8C', &
      'FFFFEFDEDED6CEBDAD845A6384847B7B7B84848494A5A5ADA59C948C8484', &
      'FFFFE7DED6CEBD9C7363637B73737B847B7B84848C8C847B73736B635A63', &
      'FFE7D6C6B59C84737B8C8C7B7373737B848C9CA5ADA5A594846B6B636B7B', &
      'CEB5ADA59CA5ADB5ADA594847B7B7B848C9CA5B5B5BDB5B5A59C84847384', &
      'E7D6CED6D6D6C6C6C6BDA59C7B7B8C8C8CA5ADB5C6BDBDB5B5A59C8C8484', &
      'FFF7F7EFE7E7D6CECEC6B5A58C7384949C9CADB5C6C6C6BDB5B5AD9C948C', &
      'FFFFFFF7F7F7EFE7D6CEC6A5947B7B8CA5B5BDBDBDC6C6BDBDB5ADA59C94', &
      'FFFFFFFFF7F7F7EFE7CEC6BDA5848C9CADB5C6EFF7EFF7F7EFEFEFEFEFE7', &
      'FFFFFFFFFFFFF7F7E7DECEBDAD947BA5ADC6CEFFFFFFFFFFFFFFFFFFFFFF', &
      'FFFFFFFFFFFFFFEFEFE7D6C6AD948494BDC6C6F7FFFFFFFFFFFFFFFFFFFF', &
      'FFFFFFFFFFFFFFF7EFDED6CEBDA5848CADBDC6F7FFFFFFFFFFFFFFFFFFFF', &
      'FFFFFFFFFFFFFFF7EFE7D6D6CEC694739CB5C6FFFFFFFFFFFFFFFFFFFFFF', &
      'FFFFFFFFFFFFFFFFFFFFFFFFFFF79C7B94ADC6F7FFFFFFFFFFFFFFFFFFFF', &
      'FFFFFFFFFFFFFFFFFFFFFFFFFFF7A58484A5B5F7FFFFFFFFFFFFFFFFFFFF', &
      'FFFFFFFFFFFFFFFFFFFFFFFFFFEF9C847B9CADF7FFFFFFFFFFFFFFFFFFFF', &
      'FFFFFFFFFFFFFFFFFFFFFFFFFFEFA5847B8CADF7FFFFFFFFFFFFFFFFFFFF', &
      'FFFFFFFFFFFFFFFFFFFFFFFFFFEF9C8C848C9CF7FFFFFFFFFFFFFFFFFFFF', &
      'FFFFFFFFFFFFFFFFFFFFFFFFFFE7A5948C8CA5F7FFFFFFFFFFFFFFFFFFFF' /
!----
!CdC Mar 2015: flip horizontal, different correction
      NX=30
      NY=31
      XMUL=DBLE(NX)/GRS
      YMUL=DBLE(NY)/GRS
      FF=30.0D0/31.0D0
      XCOR=GRS/30.0D0
      XC=X0+X-GRS*FF/2.0D0+XCOR*0.0D0
      YC=Y0+Y-GRS/2.0D0
!
      IF (MAKEPS) THEN
      WRITE (pst,FMT='(''/cdch <'')')
      WRITE (pst,FMT='(A60)') (CDCH(J),J=NY,1,-1)
      WRITE (pst,FMT='(''> def'')')
      WRITE (pst,1000) XC,YC,NX,NY,XMUL,YMUL
 1000 FORMAT ('gsave',2X,2F10.6,2X,'translate',I4,2X,I4,'  8  ','[', &
      F10.6,'  0  0  ',F10.6,'  0  0]  {cdch} image grestore')
      END IF
!
      RX=GRS/DBLE(NX)*CMPX*1.03D0
      RY=GRS/DBLE(NY)*CMPX*1.05D0

      IF (MAKESVG) THEN
      DO IY=1,NY
        YC=595.0D0-(Y+Y0+GRS/2.0D0-DBLE(IY-1)*GRS/DBLE(NY))*CMPX
        DO IX=1,NX
          HXCOL=CDCH(IY)(2*(IX-1)+1:2*(IX-1)+2)
          IF (HXCOL.NE.'FF') THEN
!!          XC=(X+X0+DBLE(IX-1)*GRS/DBLE(NX))*CMPX
!!          XC=(X+X0-GRS*FF/2.0D0+XCOR*1.0D0+DBLE(IX-1)*GRS/DBLE(NX))*CMPX
          XC=(X+X0-GRS*FF/2.0D0+XCOR*0.0D0+DBLE(IX-1)*GRS/DBLE(NX))*CMPX
          WRITE (svg,2000) XC,YC,RY,RY,HXCOL,HXCOL,HXCOL
 2000     FORMAT ('<rect x="',F10.4,'" y="',F10.4, &
          '" height="',F10.4,'" width="',F10.4, &
          '" fill="#',A2,A2,A2,'"/>')
          END IF
        END DO
      END DO
      END IF
!
      RETURN
      END
!
!***************
      SUBROUTINE GREYCDCH1(X,Y,GRS)
      IMPLICIT NONE
      INCLUDE 'files.cmn'
      INCLUDE 'map.cmn'
!
      REAL(8) GRS,X,Y,XC,YC,XMUL,YMUL,F1,XCOR
      INTEGER(4) NX,NY,J
      CHARACTER(60) CDCH(31)
      DATA CDCH / &
      'FFFFFFFFFFFFFFFFFFFFFFFFC72D2A28272AFAFFFFFFFFFFFFFFFFFFFFFF', &
      'FFFFFFFFFFFFFFFFFFFFFFFFD22A2A292522FAFFFFFFFFFFFFFFFFFFFFFF', &
      'FFFFFFFFFFFFFFFFFFFFFFFFDC2828262321FAFFFFFFFFFFFFFFFFFFFFFF', &
      'FFFFFFFFFFFFFFFFFFFFFFFFE92F28252422FAFFFFFFFFFFFFFFFFFFFFFF', &
      'FFFFFFFFFFFFFFFFFFFFFFFFF32E28252322FAFFFFFFFFFFFFFFFFFFFFFF', &
      'FFFFFFFFFF8792ACCCF1FFFFFE2E29252322FAFFFFFFFFFFFFFFFFFFFFFF', &
      'FFFFFFFFFFAE42322B374662802D29282623FAFFFFFFFFFFFFFFFFFFFFFF', &
      'FFFFFFFFFFFC4F35333837322F2D2A282827FAFFFFFFFFFFFFFFFFFFFFFF', &
      'FFFFFFFFFF8F4736363735312F2D2C2B2A29FAFFFFFFFFFFFFFFFFFFFFFF', &
      'FFFFFFFC7942443C3A373532302E2D2B2C2BFAFFFFFFFFFFFFFFFFFFFFFF', &
      'FFFFF46F4542403F3D3937353433322E2C2CDAE3E3E2E2E4EDF1F1F1F0FF', &
      'FFE8624F464541413D383635363535322F2C2D2C2C2B293E778788857DFF', &
      'D94D5054494444413D39383736383937312E2C2D2B2C2D67848C88857CFF', &
      '394E635F504A47423E3A393A3A3B3A3935302D2C2B2D517D898D8C857CFF', &
      '4566786C5B534A453D3738393B3B3A3A35312F2C2B3C75858C8D8A827AC7', &
      '90788477645A5345413637383938383937322F2E2D6684898D8B877F7770', &
      'FFAA8C7F6B60594D433A3234302B2B2F32302F303F818C8D8C88847A71FF', &
      'FFB08D8574665D53473B2C292424242223272A2C5B888E8F8D8682776CFF', &
      'FF7885867A6B5F565B692A242525232222222428728C90918D867D7569FF', &
      'C95D727A7A6F635EDB6B2F24282622242726253B7B8B90908983786E63FF', &
      'BBB2DEDB75726CD0FF4B36262D2A27262827D9E8F0F2F3F3F1F1EFEDEBFF', &
      'FFFFFFFF8D79C7FFF73837313A352B2A2F2DFAFFFFFFFFFFFFFFFFFFFFFF', &
      'FFFFFFFFCABEFFFFDC3736435042302C3B33FBFFFFFFFFFFFFFFFFFFFFFF', &
      'FFFFFFFFFFFFFFFFC33935475A493D3B4433FBFFFFFFFFFFFFFFFFFFFFFF', &
      'FFFFFFFFFFFFFFFFAC3D364A5C4C3B3E4B36FBFFFFFFFFFFFFFFFFFFFFFF', &
      'FFFFFFFFFFFFFFFF98453F465954373F513BFBFFFFFFFFFFFFFFFFFFFFFF', &
      'FFFFFFFFFFFFFFFF92B1F55955553F475446FBFFFFFFFFFFFFFFFFFFFFFF', &
      'FFFFFFFFFFFFFFFFFFFFFFB851513D475D55FCFFFFFFFFFFFFFFFFFFFFFF', &
      'FFFFFFFFFFFFFFFFFFFFFFFF61473A466068FEFFFFFFFFFFFFFFFFFFFFFF', &
      'FFFFFFFFFFFFFFFFFFFFFFFFB7433544657CFEFFFFFFFFFFFFFFFFFFFFFF', &
      'FFFFFFFFFFFFFFFFFFFFFFFFFF4F33496489FFFFFFFFFFFFFFFFFFFFFFFF' /
!----
      NX=30
      NY=31
      XMUL=DBLE(NX)/GRS
      YMUL=DBLE(NY)/GRS
      F1=30.0D0/31.0D0
      XCOR=GRS/30.0D0
      XC=X0+X-GRS*F1/2.0D0+XCOR*1.0D0
      YC=Y0+Y-GRS/2.0D0
      WRITE (pst,FMT='(''/cdch <'')')
      WRITE (pst,FMT='(A60)') (CDCH(J),J=1,NY)
      WRITE (pst,FMT='(''> def'')')
     WRITE (pst,1000) XC,YC,NX,NY,XMUL,YMUL
 1000 FORMAT ('gsave',2X,2F10.6,2X,'translate',I4,2X,I4,'  8  ','[', &
      F10.6,'  0  0  ',F10.6,'  0  0]  {cdch} image grestore')
      RETURN
      END
!
!***************
      SUBROUTINE VPMOVE(X1,Y1)
      IMPLICIT NONE
      INCLUDE 'files.cmn'
      INCLUDE 'map.cmn'
!----- end of common variables
      REAL(8) X1,Y1
!====
      IF (MAKEPS) THEN
      WRITE(pst,1000) X1+X0,Y1+Y0
 1000 FORMAT (F10.4,1X,F10.4,' moveto ')
      END IF
!
      IF (MAKESVG) THEN
      WRITE(svg,1005) (X1+X0)*CMPX,595.0D0-(Y1+Y0)*CMPX
 1005 FORMAT ('M ',F10.4,1X,F10.4)
      END IF
!
      RETURN
      END
!
!***************
      SUBROUTINE VPDRAW(X1,Y1)
      IMPLICIT NONE
      INCLUDE 'files.cmn'
      INCLUDE 'map.cmn'
!----- end of common variables
      REAL(8) X1,Y1
!====
      IF (MAKEPS) THEN
      WRITE(pst,1000) X1+X0,Y1+Y0
 1000 FORMAT (F10.4,1X,F10.4,' lineto ')
      END IF
!
      IF (MAKESVG) THEN
      WRITE(svg,1005) (X1+X0)*CMPX,595.0D0-(Y1+Y0)*CMPX
 1005 FORMAT ('L ',F10.4,1X,F10.4)
      END IF
!
      RETURN
      END
!
!***************
      SUBROUTINE VPCLOSE
      IMPLICIT NONE
      INCLUDE 'files.cmn'
      INCLUDE 'map.cmn'
!----- end of common variables
      IF (MAKEPS) THEN
      WRITE(pst,1000)
 1000 FORMAT (' closepath ')
      END IF
!
      IF (MAKESVG) THEN
      WRITE(svg,1005)
 1005 FORMAT ('Z ')
      END IF

      RETURN
      END
!
!***************
      SUBROUTINE VPSTROKE
      IMPLICIT NONE
      INCLUDE 'files.cmn'
      INCLUDE 'map.cmn'
!
      IF (MAKEPS) THEN
      IF (LINCOL.EQ.0) THEN
      WRITE (pst,1000) LGRAY
 1000 FORMAT (F7.4,' setgray  stroke ')
      ELSE
      WRITE (pst,1002) LCOLR,LCOLG,LCOLB
 1002 FORMAT (3(1X,F7.4),' setrgbcolor  stroke ')
      END IF
      END IF
      RETURN
      END
!
!***************
      SUBROUTINE VPTEXT(X1,Y1,GRS,THETA,TEXT)
      IMPLICIT NONE
!
      INCLUDE 'files.cmn'
      INCLUDE 'map.cmn'
!----- end of common variables
      CHARACTER*(*) TEXT
      CHARACTER(1) A1
      REAL(8) X1,Y1,GRS,THETA,XI
      INTEGER(4) I,I1,CODE,I001,I002,LL,J1,J2,J3
!====
!!--- for ps
      IF (MAKEPS) THEN
!
      IF (GRS.EQ.0.0D0) RETURN
      IF (TEXT.EQ.' ') RETURN
      XI=GRS*1.3715D0
      CALL VPSTROKE
      WRITE(pst,1000) XI
 1000 FORMAT (F7.4,' ff ')
      CALL VPMOVE(X1,Y1)
      IF (THETA.NE.0.0D0) THEN
      WRITE (pst,1010) THETA
 1010 FORMAT (F9.3,' rotate')
      END IF
!=====
      CODE=0
      I001=1
      I002=0
      CALL LAENGE(TEXT,LL)
      DO 500,I=1,LL
      A1=TEXT(I:I)
      I1=ICHAR(A1)
      IF (CODE.EQ.1) THEN
      IF (I002.GE.I001) CALL SHOWTEX(TEXT,I001,I002)
      I001=I+1
      IF (A1.EQ.'_') THEN
      XI=-0.3D0*GRS
      WRITE (pst,1013) XI
 1013 FORMAT ('0 ',F7.3,' rmoveto')
      END IF
      IF (A1.EQ.'^') THEN
      XI=0.3D0*GRS
      WRITE (pst,1014) XI
 1014 FORMAT ('0 ',F7.3,' rmoveto')
      END IF
      IF (A1.EQ.'>') THEN
      XI=GRS*1.3715D0
      WRITE(pst,1015) XI
 1015 FORMAT ('/Symbol    nf ',F7.4,' ff ')
      END IF
      IF (A1.EQ.'<') THEN
      XI=GRS*1.3715D0
      CALL VPNEWFONT(PSFONT)
      WRITE(pst,1016) XI
 1016 FORMAT (F7.4,' ff ')
      END IF
      CODE=0
      ELSE
      IF (I1.EQ.92) THEN
      CODE=1
      ELSE
      I002=I
      END IF
      END IF
  500 CONTINUE
!=====
      IF (I002.GE.I001) CALL SHOWTEX(TEXT,I001,I002)
      IF (THETA.NE.0.0D0) THEN
      WRITE (pst,1020) -THETA
 1020 FORMAT (F9.3,' rotate')
      END IF
!
      END IF
!!--- end for ps
!
!!--- for svg
      IF (MAKESVG) THEN
!!      SVGFONT='Arial'
      CALL LABLA(SVGSTRO,J1)
      CALL LABLA(TEXT,J2)
      CALL LABLA(SVGFONT,J3)
      WRITE (svg,2000) (X1+X0)*CMPX,595.0D0-(Y1+Y0)*CMPX, &
      -THETA,(X1+X0)*CMPX,595.0D0-(Y1+Y0)*CMPX, &
      SVGFONT(1:J3),GRS*CMPX*1.3715D0,SVGSTRO(1:J1),TEXT(1:J2)
 2000 FORMAT ('<text x="',F10.4,'" y="',F10.4,'"', &
      /,' transform="rotate(',F10.4,',',F10.4,',',F10.4,')"', &
      /,' font-family="''',A,'''" font-size="',F10.4,'"', &
      /,' style="fill: ',A,'; stroke: none">',A,'</text>')
      END IF
!!--- end for svg
!
      RETURN
      END
!
!***************
      SUBROUTINE VPPOINT(X,Y,GRS,II)
      IMPLICIT NONE
      INCLUDE 'files.cmn'
      INCLUDE 'map.cmn'
!
      REAL(8) X,Y,GRS
      INTEGER(4) II
!====
      WRITE (pst,1000) X+X0,Y+Y0,GRS
 1000 FORMAT (F10.4,1X,F10.4,1X,F10.4,'  pt ')
      IF (II.EQ.1) THEN
      CALL VPSTROKE
      ELSE
      CALL VPFILL
      END IF
      RETURN
      END
!
!***************
      SUBROUTINE VPFILL
      IMPLICIT NONE
      INCLUDE 'files.cmn'
      INCLUDE 'map.cmn'
!!
      IF (FILCOL.EQ.0) THEN
      WRITE (pst,1000) FGRAY
 1000 FORMAT (F7.4,' setgray  fill ')
      ELSE
      WRITE (pst,1002) FCOLR,FCOLG,FCOLB
 1002 FORMAT (3(1X,F7.4),' setrgbcolor  fill ')
      END IF
      RETURN
      END
!
!***************
      SUBROUTINE SHOWTEX(TEXT,I1,I2)
      IMPLICIT NONE
      INCLUDE 'files.cmn'
!
      CHARACTER*(*) TEXT
      CHARACTER(500) TEXT2
      CHARACTER(1) A1
      INTEGER(4) I1,I2,I92,I,I001
!====
      I92=92
      A1=CHAR(I92)
!     A1='?'
      I001=0
      TEXT2=' '
      DO 500,I=I1,I2
      I001=I001+1
      IF (TEXT(I:I).EQ.'('.OR.TEXT(I:I).EQ.')') THEN
      TEXT2(I001:I001)=A1
      I001=I001+1
      END IF
      TEXT2(I001:I001)=TEXT(I:I)
  500 CONTINUE
      WRITE (pst,1000) TEXT2(1:I001)
 1000 FORMAT ('(',A,') show')
      RETURN
      END
!
!***************
      SUBROUTINE LAENGE(CH,LAE)
      IMPLICIT NONE
      CHARACTER*(*) CH
      INTEGER(4) I,LAE,IL
!====
      IL=LEN(CH)
      LAE=0
      DO 500,I=IL,1,-1
      LAE=I
      IF (CH(I:I).NE.' ') GOTO 5
  500 CONTINUE
    5 RETURN
      END
!
!***************
      SUBROUTINE VPNEWFONT(AA)
      IMPLICIT NONE
      INCLUDE 'files.cmn'
!
      CHARACTER(32) AA,BB
      INTEGER(4) I1
!====
      I1=INDEX(AA,'  ')-1
      BB='('//AA(1:I1)//')'
      WRITE (pst,1000) BB
 1000 FORMAT (A32,' nf')
      RETURN
      END
!
!***************
      SUBROUTINE VPINIT
      IMPLICIT NONE
      INCLUDE 'files.cmn'
      INCLUDE 'map.cmn'
!----- end of common variables
!====
      IF (MAKEPS) THEN
      WRITE (pst,1000)
 1000 FORMAT('%!'/'%%BoundingBox: 0 0 842 595' &
      /'/slw    { stroke setlinewidth } def' &
      /'/dash   { stroke setdash } def' &
      /'/ff     { /s exch def lefont findfont s ', &
      'scalefont setfont } def' &
      /'/nf     { /lefont exch def} def' &
      /'/pt     { newpath 2.0 div 0.0 360.0 arc } def' &
      /'%%End of procedures' &
      /'/lefont 100 string def' &
      /'72 2.54 div dup scale' &
      /'1 setlinecap' &
      /'1 setlinejoin' &
      /'0.02 setlinewidth' &
      /'%%End of defaults')
      END IF

!====
      IF (MAKESVG) THEN
      WRITE (svg,1005)
 1005 FORMAT( &
       '<?xml version="1.0" encoding="utf-8"?>' &
      /'<svg version="1.1"' &
      /'xmlns="http://www.w3.org/2000/svg"' &
      /'xmlns:xlink="http://www.w3.org/1999/xlink" ' &
      /'width="29.7cm" height="21cm"' &
      /'viewBox="0 0 842 595">' &
      /'<g xml:space="preserve" ', &
      'stroke-linecap="round" ' , &
      'stroke-linejoin="round" stroke-miterlimit="10">')
      END IF

      RETURN
      END
!
!***************
      SUBROUTINE SVGPATH(I1,I2)
      IMPLICIT NONE
      INCLUDE 'map.cmn'
!----- end of common variables
      INTEGER(4) I1,I2,J1,J2
      REAL(8) DICKE
      CHARACTER(7) VOLL,STRO
!
      IF (MAKESVG) THEN
      IF (I1.EQ.0) THEN
        DICKE=0.0D0
      ELSE
        DICKE=FAT*CMPX
      END IF

      IF (I1.EQ.0) THEN 
        STRO='none'
      ELSE
        STRO=SVGSTRO
      END IF

      IF (I2.EQ.0) THEN 
        VOLL='none'
      ELSE
        VOLL=SVGFILL
      END IF

      CALL LABLA(STRO,J1)
      CALL LABLA(VOLL,J2)
      IF (LINDASH) THEN
      WRITE (svg,1000) DICKE,STRO(1:J1),VOLL(1:J2), &
      DASH1*CMPX,GAP1*CMPX,DASH2*CMPX,GAP2*CMPX
 1000 FORMAT ('<path stroke-width="',F10.4,'"', &
      ' stroke="',A,'" fill="',A, &
      '" stroke-dasharray="',4F10.4, &
      '" d=" ')
      ELSE
      WRITE (svg,1010) DICKE,STRO(1:J1),VOLL(1:J2)
 1010 FORMAT ('<path stroke-width="',F10.4,'"', &
      ' stroke="',A,'" fill="',A,'" d=" ')
      END IF

      END IF
      RETURN
      END
!
!***************
      SUBROUTINE SVGCIRCLE(X,Y,GRS,II)
      IMPLICIT NONE
      INCLUDE 'map.cmn'
!----- end of common variables
      INTEGER(4) II,J1,J2
      REAL(8) X,Y,GRS,DICKE
      CHARACTER(7) VOLL,STRO
!
      IF (MAKESVG) THEN
      DICKE=FAT*CMPX
      STRO=SVGSTRO
      IF (II.GT.0) THEN 
        VOLL='none'
      ELSE
        VOLL=SVGFILL
      END IF

      CALL LABLA(STRO,J1)
      CALL LABLA(VOLL,J2)
      WRITE (svg,1000) (X+X0)*CMPX,595.0D0-(Y+Y0)*CMPX, &
      GRS*CMPX/2.0D0,DICKE,STRO(1:J1),VOLL(1:J2)
 1000 FORMAT ('<circle cx="',F10.4,'" cy="',F10.4, &
      '" r="',F10.4, &
      '" stroke-width="',F10.4,'"', &
      ' stroke="',A,'" fill="',A,'" /> ')
      END IF
      RETURN
      END
!
!***************
      SUBROUTINE SVGPATHEND
      IMPLICIT NONE
      INCLUDE 'map.cmn'
!----- end of common variables
      IF (MAKESVG) THEN
      WRITE (svg,1000)
 1000 FORMAT ('"/>')
      END IF
      RETURN
      END
!
!***************
      SUBROUTINE SVGCOLS
      IMPLICIT NONE
      INCLUDE 'map.cmn'
!----- end of common variables
      IF (LINCOL.EQ.0) THEN
       WRITE (UNIT=SVGSTRO,FMT=1000) IDINT(LGRAY*255.0D0), &
       IDINT(LGRAY*255.0D0),IDINT(LGRAY*255.0D0)
      ELSE
       WRITE (UNIT=SVGSTRO,FMT=1000) IDINT(LCOLR*255.0D0), &
       IDINT(LCOLG*255.0D0),IDINT(LCOLB*255.0D0)
      END IF
 1000 FORMAT ('#',Z2.2,Z2.2,Z2.2)
!!      WRITE (unit=6,fmt='(''SVGSTRO '',A)') SVGSTRO
!
      IF (FILCOL.EQ.0) THEN
       WRITE (UNIT=SVGFILL,FMT=1010) IDINT(FGRAY*255.0D0), &
       IDINT(FGRAY*255.0D0),IDINT(FGRAY*255.0D0)
      ELSE
       WRITE (UNIT=SVGFILL,FMT=1010) IDINT(FCOLR*255.0D0), &
       IDINT(FCOLR*255.0D0),IDINT(FCOLR*255.0D0)
      END IF
 1010 FORMAT ('#',Z2.2,Z2.2,Z2.2)
!!      WRITE (unit=6,fmt='(''SVGFILL '',A)') SVGFILL
!
      RETURN
      END
!
!*******************************
      SUBROUTINE FUNGELI(CH,I1,F1,F1CH)
      CHARACTER*(*) CH,F1CH
      CHARACTER(32) CH001,CH002
      INTEGER(4) I1,I
      REAL(8) F1,FF
!-
      CALL TAXI(CH,CH002)
      I=INDEX(CH002,'/')
      IF (I.EQ.0) THEN
       CALL GELI(CH002,FF)
       I1=IDINT(FF)
       F1=1.0D0
       F1CH=' '
      ELSE
       CH001=CH002(1:I-1)
       CALL GELI(CH001,FF)
       I1=IDINT(FF)
       CH001=CH002(I+1:)
       F1CH='/'//CH001
       CALL GELI(CH001,FF)
       F1=FF
      END IF
!-
      RETURN
      END
!
!*******************************
      subroutine CALCPAR
      return
      end
!-----
