!-----Version: 28.05.2022
!               **********
!               * EXPLOT *
!               **********
!     Converts Output of Domino, Thalia etc. to a plot
!     May also be used to create plots by editing the input file
!     The output is a PostScript file and/or a svg file.
!
!     Program written by Christian de Capitani
!     at Mineralogisch-Petrographisches Institut
!        Universitaet Bern, Switzerland     (1978 - 1983)
!     at the Department of Geological Sciences,
!        University of British Columbia, Vancouver, B.C.   (1983 - 1987)
!     at the Department of Geology
!        Stanford University, Stanford, CA., 94305   (1987-1991)
!     at Mineralogisch-Petrographisches Institut
!        Universitaet Basel     (since 1992)
!
!     revisions: continuously
!     last major revisions: December 1991
!                           April 1994
!     revision: October 2002
!     added svg: April 2015
!
!     for details of algorithm see: (not documented)
!
!     Any suggestions, complaints or comments are greatly appreciated
!     by the author and should be sent to:
!          Christian de Capitani
!          Mineralogisch-Petrographisches Institut
!          Universitaet Basel
!          Bernoullistrasse 30
!          CH-4056 BASEL
!
!23456789.123456789.123456789.123456789.123456789.123456789.123456789.12
      IMPLICIT NONE
      INCLUDE 'expl.cmn'
!----- end of common variables
      CHARACTER(6) IDENT
      CHARACTER(200) TEXT,XTEXT,YTEXT
      CHARACTER(200) CH001,CH002,CHIN(2)
      REAL(8) XX,YY,LANG,KURZ,XMIN,XMAX,YMIN,YMAX, &
      SIDE,F1,F2,F3,F4,GRS,GRZ,X,Y,Z,X1,TIK,ZERO, &
      X99,Y99,SYM99,GR99
      INTEGER(4) COD,II,I001,C1,C2,I1,I2,I3,NID,I,I002,LONGCOD,NPEND, &
      J1,J2
!=====
      CHARACTER(500) wpath,tpath,CPLNAME,FNAME,LOGFNAME
      INTEGER(4) iwpath,itpath
      character filetype(10)*3
      CHARACTER(80) filelog,filecln,fileps,filesvg
      character largum(5)*80,sdate*21
      integer ierr,j,larg
      character dir*1, ext*10, os*20
!-----
      data dir, ext, os /'/',2*' '/
      data largum /5*' '/
      data filetype &
      /'hlp','kbd','scr','log','cln','plt','rxn','grd','pst','svg'/
!=====
!-----
      CALL GetEnvVar(tpath,itpath,ierr, dir, ext, os)
      CALL getwork(wpath,iwpath,ierr,dir)
!      WRITE (*,FMT='(''program directory: '',A)') tpath(1:itpath)
!      WRITE (*,FMT='(''working directory: '',A)') wpath(1:iwpath)
!-----
      sdate=' '
      filelog='explot.last'
      filecln='clean'
      fileps='plot.ps'
      filesvg='plot.svg'
      MAKESVG=.TRUE.
      MAKEPS=.TRUE.
      LINDASH=.FALSE.
!*****
      j=scr
      call writetit (j,os)
      call getdate(sdate)
      write(scr,'(''Run: '',a/)') sdate
!-----largum(1) = graphics file input      ---> unit= cln
!     largum(2) = PostScript file output   ---> units= pst+svg
!
!-----
      larg=0
      do 400 I=1,5
        call GetLineArgs (I,largum(I),ierr)
        if(ierr.ne.0.or.largum(I).eq.' ')then
          goto 401
        else
          larg=larg+1
        end if
  400 continue
  401 continue
!------
      if(largum(1).eq.'-h') then
         call helpme('$EXP-CALL',tpath,itpath)
         stop
      end if
!*****
      CHIN(1)=' '
      CHIN(2)='no'
      FNAME=wpath(1:iwpath)//filelog
      LOGFNAME=FNAME
      CALL LABLA(FNAME,j)
!      WRITE (*,FMT='(''try working directory: '',A)') FNAME(1:j)
      OPEN(UNIT=log,FILE=FNAME(1:j),STATUS='OLD',ERR=300)
      GOTO 301
  300 CONTINUE
      FNAME=tpath(1:itpath)//filelog
      LOGFNAME=FNAME
      CALL LABLA(FNAME,j)
!      WRITE (*,FMT='(''try program directory: '',A)') FNAME(1:j)
      OPEN(UNIT=log,FILE=FNAME(1:j),STATUS='UNKNOWN')
  301 CONTINUE
!-----
      CALL LABLA(LOGFNAME,j)
      WRITE (UNIT=scr,FMT='(/,''log-file used: '',A)') LOGFNAME(1:j)
!-----
      DO I=1,2
        READ (UNIT=log,FMT='(A500)',END=411) CHIN(I)
      END DO
  411 CONTINUE
      CALL LABLA(CHIN(1),I002)
      IF (I002.EQ.0) I002=1
      CH002='Enter [ "?" | CR | graphics file name ] <'// &
      CHIN(1)(1:I002)//'>? '
  412 CALL LABLA(CH002,j)
!-----check if command line arguments are present.
      if(larg.eq.0) then
        WRITE (UNIT=6,FMT='(/,A)') CH002(1:j)
        READ (UNIT=5,FMT='(A500)') CH001
      else
         CH001=largum(1)
      end if
      if(CH001.eq.'?') then
         call helpme('$EXP-START',tpath,itpath)
         goto 412
      end if
!*****
      CALL LABLA(CH001,I001)
      IF (I001.EQ.0) THEN
      CH001=CHIN(1)
      I001=I002
      ELSE
      CHIN(1)=CH001
      END IF
!*****
      CPLNAME=wpath(1:iwpath)//CH001
      I001=INDEX(CPLNAME,'   ')-1
!      WRITE (*,FMT='(''try working directory: '',A)') CPLNAME(1:I001)
!-----
      OPEN(UNIT=cln,FILE=CPLNAME(1:I001),STATUS='OLD',ERR=310)
      GOTO 312
  310 CONTINUE
      CPLNAME=tpath(1:itpath)//CH001
      I001=INDEX(CPLNAME,'   ')-1
!      WRITE (*,FMT='(''try program directory: '',A)') CPLNAME(1:I001)
      OPEN(UNIT=cln,FILE=CPLNAME(1:I001),STATUS='OLD',ERR=311)
      wpath=tpath
      iwpath=itpath
      GOTO 312
  311 WRITE (*,*) 'input not found'
      STOP
  312 CONTINUE
!=====
      WRITE (*,FMT='(/,''working directory: '',A)') wpath(1:iwpath)
!=====
!+++++store terminal input
      CLOSE (UNIT=log)
      FNAME=wpath(1:iwpath)//filelog
      CALL LABLA(FNAME,j)
      OPEN(UNIT=log,FILE=FNAME(1:j),STATUS='UNKNOWN')
      DO I=1,2
        CALL PUST(log,CHIN(I))
      END DO
      CLOSE (UNIT=log)
!*****
      ZERO=0.0D0
      ABC='ABC'
      S=20.0D0
      X0=5.0D0
      Y0=3.0D0
      H=10.0D0
      B=10.0D0
      L=0.0D0
      R=10.0D0
      U=0.0D0
      O=10.0D0
      SYSTEM=2
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
      NID=0
      NPEND=0
      FAT=0.02D0
      SVGFAT=FAT
!!      CMPX=35.43307D0
      CMPX=72.0D0/2.54D0
      IMAN=0
      CALL SVGCOLS
!=====
      IF (largum(2).ne.' ') THEN
        fileps=largum(2)
        CALL LABLA(fileps,I1)
        I2=INDEX(fileps,'.ps')
        IF (I2.NE.I1-2) fileps(I1+1:)='.ps'
!
!!        MAKESVG=.FALSE.
        filesvg=largum(2)
        CALL LABLA(filesvg,I1)
        I2=INDEX(filesvg,'.ps')
        IF (I2.NE.I1-2) THEN
          filesvg(I1+1:)='.svg'
        ELSE
          filesvg(I2:)='.svg'
        END IF
!
      END IF
!
      IF (MAKEPS) THEN
        FNAME=wpath(1:iwpath)//fileps
        CALL LABLA(FNAME,j)
        OPEN(UNIT=pst,FILE=FNAME(1:j),STATUS='UNKNOWN')
      END IF
      IF (MAKESVG) THEN
        FNAME=wpath(1:iwpath)//filesvg
        CALL LABLA(FNAME,j)
        OPEN(UNIT=svg,FILE=FNAME(1:j),STATUS='UNKNOWN')
      END IF

      CALL VPINIT
!-----
      CALL VPNEWFONT(PSFONT)
      X99=-4.0D0
      Y99=-2.0D0
      SYM99=99.0D0
      GR99=0.25D0
!     CALL SYMBOL(X99,Y99,SYM99,GR99)
!====
   10 CONTINUE
      IF (NPEND.EQ.1) GOTO 999
      READ (UNIT=cln,FMT='(A500)',END=999) CH001
      IDENT=CH001(1:6)
      REC=CH001(7:)
      NID=NID+1
!.....
      IF (IDENT.EQ.'FERTIG') GOTO 999
      IF (IDENT.EQ.'SHOWP') THEN
      CALL VPSHOWP
      GOTO 10
      END IF
      IF (IDENT.EQ.'LAND') THEN
      CALL VPLAND
      GOTO 10
      END IF
!.....
      IF (IDENT.EQ.'FAT') THEN
      CALL GELI(REC,FAT)
      CALL VPFAT(FAT)
      GOTO 10
      END IF
!.....
      IF (IDENT.EQ.'STYLE') THEN
      CALL GELI(REC,DASH1)
      CALL GELI(REC,GAP1)
      CALL GELI(REC,DASH2)
      CALL GELI(REC,GAP2)
      CALL VPDASH
      IF (DASH1.NE.0.0D0.AND.GAP1.NE.0.0D0.AND.DASH2.NE.0.0D0 &
      .AND.GAP2.NE.0.0D0) THEN
        LINDASH=.TRUE.
      ELSE
        LINDASH=.FALSE.
      END IF
      GOTO 10
      END IF
!.....
      IF (IDENT.EQ.'ABC') THEN
      CALL TAXI(REC,ABC)
      GOTO 10
      END IF
!.....
      IF (IDENT.EQ.'ACHSEN') THEN
      SYSTEM=2
      CALL GELI(REC,F1)
      COD=INT(F1)
      CALL GELI(REC,B)
      IF (B.LE.0.0D0) B=15.0D0
      CALL GELI(REC,H)
      IF (H.LE.0.0D0) H=15.0D0
      CALL GELI(REC,L)
      CALL GELI(REC,R)
      IF (L.EQ.R) THEN
       L=L*0.9D0
       R=R*1.1D0
       IF (L.EQ.0.0D0) THEN
        L=-1.0D0
        R=1.0D0
       END IF
      END IF
      CALL GELI(REC,U)
      CALL GELI(REC,O)
      IF (U.EQ.O) THEN
       U=U*0.9D0
       O=O*1.1D0
       IF (U.EQ.0.0D0) THEN
        U=-1.0D0
        O=1.0D0
       END IF
      END IF
      IF (COD.EQ.1) THEN
      J1=1
      J2=0
      CALL SVGPATH(J1,J2)
      CALL VPMOVE(B,H)
      CALL VPDRAW(B,ZERO)
      CALL VPDRAW(ZERO,ZERO)
      CALL VPDRAW(ZERO,H)
      CALL VPCLOSE
      CALL VPSTROKE
      CALL SVGPATHEND
      END IF
      GOTO 10
      END IF
!.....
      IF (IDENT.EQ.'IMAGE') THEN
      CALL TAXI(REC,IMACH)
      CALL GELI(REC,IMAX)
      CALL GELI(REC,IMAY)
      CALL GELI(REC,IMAB)
      CALL GELI(REC,IMAH)
      IMAN=1
      GOTO 10
      END IF
!.....
      IF (IDENT.EQ.'CLIP') THEN
      IF (SYSTEM.EQ.2) THEN
      CALL VPMOVE(B,H)
      CALL VPDRAW(B,ZERO)
      CALL VPDRAW(ZERO,ZERO)
      CALL VPDRAW(ZERO,H)
      END IF
      IF (SYSTEM.EQ.3) THEN
      CALL VPMOVE(ZERO,ZERO)
      CALL VPDRAW(S,ZERO)
      F1=S/2.0D0
      CALL VPDRAW(F1,H)
      END IF
      CALL VPCLOSE
      CALL VPCLIP
      CALL VPNEW
      GOTO 10
      END IF
!.....
      IF ((IDENT.EQ.'XAXIS'.OR.IDENT.EQ.'YAXIS').AND.SYSTEM.EQ.2) THEN
        CALL TAXI(REC,TEXT)
        IF (TEXT(1:1).EQ.'''') THEN
          I1=INDEX(TEXT,'  ')-2
          CH001=TEXT(2:I1)
          TEXT=CH001(1:200)
        END IF
        CALL GELI(REC,X1)
        CALL GELI(REC,SIDE)
        CALL GELI(REC,TIK)
        CALL GELI(REC,GRS)
        CALL GELI(REC,GRZ)
        CALL GELI(REC,F1)
        CALL GELI(REC,F2)
        I1=INT(F1)
        I2=INT(F2)
        CALL GELI(REC,KURZ)
        CALL GELI(REC,LANG)
        CALL GELI(REC,F1)
        CALL GELI(REC,F2)
        C1=INT(F1)
        C2=INT(F2)
        IF (I2.LT.0) THEN
        I3=0
        I2=ABS(I2)
      ELSE
        I3=-1
      END IF
      IF (IDENT.EQ.'XAXIS') THEN
      CALL XAXIS(TEXT,X1,SIDE,TIK,GRS,GRZ,I1,I2,I3,KURZ,LANG,C1,C2)
      ELSE
      CALL YAXIS(TEXT,X1,SIDE,TIK,GRS,GRZ,I1,I2,I3,KURZ,LANG,C1,C2)
      END IF
      GOTO 10
      END IF
!.....
      IF (IDENT.EQ.'AXIS') THEN
      SYSTEM=2
      CALL GELI(REC,F1)
      CALL GELI(REC,F2)
      CALL TAXI(REC,XTEXT)
      CALL GELI(REC,B)
      CALL GELI(REC,XMIN)
      CALL GELI(REC,XMAX)
      CALL TAXI(REC,YTEXT)
      CALL GELI(REC,H)
      CALL GELI(REC,YMIN)
      CALL GELI(REC,YMAX)
      CALL AXIS(XTEXT,YTEXT,XMIN,XMAX,YMIN,YMAX,F1,F2)
      GOTO 10
      END IF
!.....
      IF (IDENT.EQ.'GITTER') THEN
      CALL GITTER
      GOTO 10
      END IF
!.....
      IF (IDENT.EQ.'SEITE') THEN
      SYSTEM=3
      ABC='ABC'
      CALL GELI(REC,S)
      H=S*SQRT(3.0D0)/2.0D0
      F1=S/2.0D0
      F2=H/3.0D0
      II=3
!      X99=0.0D0
!      Y99=F2
!      CALL SYMBOL(X99,Y99,SYM99,GR99)
      F3=2.0D0*F2
      F4=0.0D0
      CALL POLY(F1,F2,II,F3,F4)
      GOTO 10
      END IF
!.....
      IF (IDENT.EQ.'ECKEN'.AND.SYSTEM.EQ.3) THEN
      CALL ECKEN
      GOTO 10
      END IF
!.....
      IF (IDENT.EQ.'NULLPT') THEN
      CALL GELI(REC,X0)
      CALL GELI(REC,Y0)
      GOTO 10
      END IF
!.....
      IF (IDENT.EQ.'RELNUL') THEN
      CALL GELI(REC,F1)
      CALL GELI(REC,F2)
      X0=X0+F1
      Y0=Y0+F2
      GOTO 10
      END IF
!.....
      IF (IDENT.EQ.'LINIEN') THEN
      CALL LINIEN
      GOTO 10
      END IF
!.....
      IF (IDENT.EQ.'ARC') THEN
      CALL ARC1
      GOTO 10
      END IF
!.....
      IF (IDENT.EQ.'BEZ3') THEN
      CALL BEZ3
      GOTO 10
      END IF
!.....
      IF (IDENT.EQ.'HISTO') THEN
      CALL HISTO
      GOTO 10
      END IF
!.....
      IF (IDENT.EQ.'YBARS') THEN
      CALL YBARS
      GOTO 10
      END IF
!.....
      IF (IDENT.EQ.'XHIST') THEN
      CALL XHIST
      GOTO 10
      END IF
!.....
      IF (IDENT.EQ.'BINEX') THEN
      CALL BINEX
      GOTO 10
      END IF
!.....
      IF (IDENT.EQ.'PUNKTE') THEN
      CALL PUNKTE
      GOTO 10
      END IF
!.....
      IF (IDENT.EQ.'TIELIN') THEN
      CALL TIELIN
      GOTO 10
      END IF
!.....
      IF (IDENT.EQ.'FONT  ') THEN
      CALL TAXI(REC,PSFONT)
      IF (PSFONT.EQ.'2') THEN
        PSFONT='Helvetica'
        SVGFONT='Arial'
      ELSE
        IF (PSFONT.EQ.'Helvetica') THEN
          SVGFONT='Arial'
        ELSE
          SVGFONT=PSFONT
        END IF
      END IF
      CALL VPNEWFONT(PSFONT)
      GOTO 10
      END IF
!.....
      IF (IDENT.EQ.'LGRAY ') THEN
      LINCOL=0
      CALL GELI(REC,LGRAY)
      CALL SVGCOLS
      GOTO 10
      END IF
!.....
      IF (IDENT.EQ.'FGRAY ') THEN
      FILCOL=0
      CALL GELI(REC,FGRAY)
      CALL SVGCOLS
      GOTO 10
      END IF
!.....
      IF (IDENT.EQ.'LCOLOR ') THEN
      LINCOL=1
      CALL GELI(REC,LCOLR)
      CALL GELI(REC,LCOLG)
      CALL GELI(REC,LCOLB)
      CALL SVGCOLS
      GOTO 10
      END IF
!.....
      IF (IDENT.EQ.'FCOLOR ') THEN
      FILCOL=1
      CALL GELI(REC,FCOLR)
      CALL GELI(REC,FCOLG)
      CALL GELI(REC,FCOLB)
      CALL SVGCOLS
      GOTO 10
      END IF
!.....
      IF (IDENT.EQ.'PSYM  '.OR.IDENT.EQ.'INTEXT'.OR.IDENT.EQ.'TEXT' &
      .OR.IDENT.EQ.'TEXTB') THEN
      IF (IDENT.EQ.'TEXTB') THEN
      TEXT=REC
      READ (UNIT=cln,FMT='(A500)',END=999) CH001
      REC=CH001
      IDENT='TEXT'
      ELSE
      CALL TAXI(REC,TEXT)
      END IF
      CALL INTEXT(IDENT,TEXT)
      GOTO 10
      END IF
!.....
      IF (IDENT.EQ.'POLY') THEN
      CALL KOOLI(X,Y,Z)
      CALL XXYY(X,Y,Z,XX,YY)
      CALL GELI(REC,F1)
      II=INT(F1)
      CALL GELI(REC,F2)
      CALL GELI(REC,F3)
      CALL POLY(XX,YY,II,F2,F3)
      GOTO 10
      END IF
!.....
      IF (IDENT.EQ.'NPLOIG'.OR.NID.EQ.1) THEN
      REC=CH001
      LONGCOD=1
      CALL NPLOIG(NID,LONGCOD,NPEND)
      END IF
      IF (IDENT.EQ.'NPLOG2') THEN
      REC=CH001
      LONGCOD=2
      CALL NPLOIG(NID,LONGCOD,NPEND)
      END IF
!-----
      GOTO 10
  999 CLOSE (UNIT=cln)
      CALL VPENDPLOT
!-----
      WRITE (scr,150)
  150 FORMAT (/,'exit EXPLOT')
      END
!
!***************
      SUBROUTINE XAXIS(TEXT,X1,SIDE,TIK,GRS,GRZ,I1,I2,I3,KUR,LAN,C1,C2)
      IMPLICIT NONE
      INCLUDE 'expl.cmn'
!----- end of common variables
      CHARACTER*(*) TEXT
      CHARACTER(32) CH16,NUMMER
      REAL(8) X1,SIDE,TIK,GRS,GRZ,KUR,LAN,SIZE,SID,XX,YY,FF,THETA, &
      XKOR,YKOR,ZERO,X,FTIK,F3,F4
      INTEGER(4) I,II,I1,I2,I3,LAE,C1,C2,J1,J2,C4
!==== (C1 is only used in YAXIS to define distance of label to numbers)
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
      INCLUDE 'expl.cmn'
!----- end of common variables
      CHARACTER*(*) TEXT
      CHARACTER(32) CH16,NUMMER
      REAL(8) X1,SIDE,TIK,GRS,GRZ,KUR,LAN,SIZE,SID,XX,YY,FF,THETA, &
      XKOR,YKOR,ZERO,FFF,Y,FTIK,F3,F4
      INTEGER(4) I,II,I1,I2,I3,LAE,C1,C2,J1,J2
!==== (C1 is only used in YAXIS to define distance of label to numbers)
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
      SUBROUTINE AXIS(XTEXT,YTEXT,XMIN,XMAX,YMIN,YMAX,XZAHL,YZAHL)
      IMPLICIT NONE
      INCLUDE 'expl.cmn'
!----- end of common variables
      CHARACTER*(*) XTEXT,YTEXT
      CHARACTER(10) BTEXT
      REAL(8) XMIN,XMAX,YMIN,YMAX,GRS,GRZ,KUR,LAN,X1, &
      XTIK,YTIK,XGRID,YGRID,SIDE,XZAHL,YZAHL
      INTEGER(4) I0,I3,C1X,C2X,C1Y,C2Y,I1X,I2X,I1Y,I2Y,IKOD
!====
      DATA I0/0/
      BTEXT=' '
      IKOD=0
      IF (XZAHL.LT.0.0D0.OR.YZAHL.LT.0.0D0) IKOD=1
      XZAHL=B/ABS(XZAHL)
      YZAHL=H/ABS(YZAHL)
      GRS=0.5D0
      GRZ=0.35D0
      I3=-1
      KUR=0.2D0
      LAN=0.4D0
      L=XMIN
      R=XMAX
      U=YMIN
      O=YMAX
      CALL SCALA(XMIN,XMAX,XZAHL,XGRID,I1X,I2X,C1X,C2X)
      IF (IKOD.EQ.0) THEN
      L=XMIN
      R=XMAX
      END IF
      XTIK=XGRID/DBLE(I2X)
      CALL SCALA(YMIN,YMAX,YZAHL,YGRID,I1Y,I2Y,C1Y,C2Y)
      IF (IKOD.EQ.0) THEN
      U=YMIN
      O=YMAX
      END IF
      YTIK=YGRID/DBLE(I2Y)
      X1=U
      SIDE=-1.0D0
      CALL XAXIS(XTEXT,X1,SIDE,XTIK,GRS,GRZ,I1X,I2X,I3,KUR,LAN,C1X,C2X)
      X1=O
      SIDE=1.0D0
      CALL XAXIS(BTEXT,X1,SIDE,XTIK,GRS,GRZ,I1X,I0,I3,KUR,LAN,C1X,C2X)
      X1=L
      SIDE=-1.0D0
      CALL YAXIS(YTEXT,X1,SIDE,YTIK,GRS,GRZ,I1Y,I2Y,I3,KUR,LAN,C1Y,C2Y)
      X1=R
      SIDE=1.0D0
      CALL YAXIS(BTEXT,X1,SIDE,YTIK,GRS,GRZ,I1Y,I0,I3,KUR,LAN,C1Y,C2Y)
      RETURN
      END
!
!***************
      SUBROUTINE SCALA(XMIN,XMAX,ZAHL,TIK,II1,II2,C1,C2)
      IMPLICIT NONE
      INCLUDE 'expl.cmn'
!----- end of common variables
      CHARACTER(32) CH16
      REAL(8) XMIN,XMAX,TIK,DIFF,DIFF6,ZAHL,GRID,GRIDS(5),FX,FY, &
      OB,UNT,XLANG,FF
      INTEGER(4) I,C1,C2,EXPO,PLUS,I1,I2,IVOR,INACH,II1,II2
!====
      DATA GRIDS/1.0D0,2.0D0,2.5D0,5.0D0,10.0D0/
      PLUS=1
      IF (XMAX.LT.XMIN) THEN
      PLUS=-1
      FF=XMAX
      XMAX=XMIN
      XMIN=FF
      END IF
      DIFF=XMAX-XMIN
      DIFF6=DIFF/ZAHL
      EXPO=INT(LOG10(DIFF6))
      IF (DIFF6.LT.1.0D0) EXPO=EXPO-1
      DIFF6=DIFF6/(10.0D0**DBLE(EXPO))
      FY=ABS(DIFF6-GRIDS(1))
      GRID=GRIDS(1)
      DO I=2,5
        FX=ABS(DIFF6-GRIDS(I))
        IF (FX.LT.FY) THEN
        FY=FX
        GRID=GRIDS(I)
        END IF
      END DO
      TIK=GRID*10.0D0**DBLE(EXPO)
      IF (GRID.EQ.1.0D0.OR.GRID.EQ.10.0D0) THEN
      II1=2
      II2=2
      END IF
      IF (GRID.EQ.2.0D0) THEN
      II1=2
      II2=4
      END IF
      IF (GRID.EQ.2.5D0.OR.GRID.EQ.5.0D0) THEN
      II1=5
      II2=5
      END IF
      IF (XMIN.GE.0.0D0) THEN
      FF=XMIN/TIK+0.001D0
      ELSE
      FF=XMIN/TIK-0.999D0
      END IF
      UNT=INT(FF)*TIK
      IF (XMAX.GE.0.0D0) THEN
      FF=XMAX/TIK+0.999D0
      ELSE
      FF=XMAX/TIK-0.001D0
      END IF
      OB=INT(FF)*TIK
      IF (PLUS.GT.0D0) THEN
      XMAX=OB
      XMIN=UNT
      ELSE
      XMAX=UNT
      XMIN=OB
      TIK=-TIK
      END IF
      IF (ABS(XMAX).GT.ABS(XMIN)) THEN
      XLANG=XMAX
      ELSE
      XLANG=XMIN
      END IF
      WRITE (UNIT=CH16,FMT='(F32.7)') XLANG
      CALL FIBLA(CH16,I1)
      I2=INDEX(CH16,'.')
      IVOR=I2-I1
      INACH=MAX(-EXPO+1,-1)
      IF (INACH.EQ.0) INACH=-1
      C1=IVOR+INACH+1
      C2=INACH
      RETURN
      END
!
!***************
      SUBROUTINE GITTER
      IMPLICIT NONE
      INCLUDE 'expl.cmn'
!----- end of common variables
      REAL(8) F1,FF1,FF,F2,XX,YY,X,Y,Z,ZERO
      INTEGER(4) ILOOP,I,J1,J2
!====
      DATA ZERO/0.0D0/
      J1=1
      J2=0
      CALL SVGPATH(J1,J2)
      CALL GELI(REC,F1)
!=
      IF (SYSTEM.EQ.2) THEN
      CALL GELI(REC,F2)
      Z=0.0D0
      Y=U
!--
!-F90      DO 500,X=(L+F1-DMOD(L,F1)),R,F1
      X=L+F1-DMOD(L,F1)
      DO WHILE(X.LE.R)
      CALL XXYY(X,Y,Z,XX,YY)
      CALL VPMOVE(XX,YY)
      IF (Y.EQ.U) THEN
      Y=O
      ELSE
      Y=U
      END IF
      CALL XXYY(X,Y,Z,XX,YY)
      CALL VPDRAW(XX,YY)
      X=X+F1
      END DO
!-F90
      X=L
!--
!-F90      DO 550,Y=(U+F2-DMOD(U,F2)),O,F2
      Y=U+F2-DMOD(U,F2)
      DO WHILE(Y.LE.O)
      CALL XXYY(X,Y,Z,XX,YY)
      CALL VPMOVE(XX,YY)
      IF (X.EQ.L) THEN
      X=R
      ELSE
      X=L
      END IF
      CALL XXYY(X,Y,Z,XX,YY)
      CALL VPDRAW(XX,YY)
      Y=Y+F2
      END DO
!-F90
      CALL VPSTROKE
      END IF
!==
      IF (SYSTEM.EQ.3) THEN
      ILOOP=INT(F1/2.0D0+0.1D0)
      F2=1.0D0/F1
      DO 600,I=1,ILOOP
      FF=DBLE(I)*F2
      FF1=1.0D0-FF
      CALL XXYY(FF,ZERO,FF1,XX,YY)
      CALL VPMOVE(XX,YY)
      CALL XXYY(ZERO,FF,FF1,XX,YY)
      CALL VPDRAW(XX,YY)
      CALL XXYY(FF1,FF,ZERO,XX,YY)
      CALL VPDRAW(XX,YY)
      CALL XXYY(FF1,ZERO,FF,XX,YY)
      CALL VPDRAW(XX,YY)
      IF ((0.5D0-FF).LT.1D-5) RETURN
      CALL XXYY(ZERO,FF1,FF,XX,YY)
      CALL VPDRAW(XX,YY)
      CALL XXYY(FF,FF1,ZERO,XX,YY)
      CALL VPDRAW(XX,YY)
      CALL XXYY(FF,ZERO,FF1,XX,YY)
      CALL VPDRAW(XX,YY)
  600 CONTINUE
      CALL VPSTROKE
      END IF
!=
      CALL SVGPATHEND
      RETURN
      END
!
!***************
      SUBROUTINE PUNKTE
      IMPLICIT NONE
      INCLUDE 'expl.cmn'
!----- end of common variables
      REAL(8) XX,YY,SYM,X,Y,Z,GRS
!====
      CALL GELI(REC,SYM)
      CALL GELI(REC,GRS)
      CALL KOOLI(X,Y,Z)
   10 IF (X.EQ.999.0D0.AND.Y.EQ.999.0D0) GOTO 999
      CALL XXYY(X,Y,Z,XX,YY)
      CALL SYMBOL(XX,YY,SYM,GRS)
      CALL KOOLI(X,Y,Z)
      GOTO 10
  999 RETURN
      END
!
!***************
      SUBROUTINE SYMBOL(X,Y,SYM,GRS)
      IMPLICIT NONE
      INCLUDE 'expl.cmn'
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
      SUBROUTINE LINIEN
      IMPLICIT NONE
      INCLUDE 'expl.cmn'
!----- end of common variables
      REAL(8) F1,F2,F3,F4,X,Y,Z,XX,YY,ZERO
      INTEGER(4) I1,I2
!====
      ZERO=0.0D0
      CALL GELI(REC,F1)
      CALL GELI(REC,F2)
      CALL GELI(REC,F3)
      CALL GELI(REC,F4)
      I1=1
      I2=0
      IF (F1.EQ.1) I1=0
      IF (F1.GT.0) I2=1
      CALL SVGPATH(I1,I2)

      CALL KOOLI(X,Y,Z)
      CALL XXYY(X,Y,Z,XX,YY)

      IF (F3.NE.0.0D0) THEN
      CALL VPMOVE(XX,ZERO)
      CALL VPDRAW(XX,YY)
      ELSE
      CALL VPMOVE(XX,YY)
      END IF

      CALL KOOLI(X,Y,Z)
   10 IF (X.EQ.999.0D0.AND.Y.EQ.999.0D0) GOTO 999
      CALL XXYY(X,Y,Z,XX,YY)

      CALL VPDRAW(XX,YY)
      CALL KOOLI(X,Y,Z)
      GOTO 10
  999 CONTINUE
      IF (F3.NE.0.0D0) CALL VPDRAW(XX,ZERO)
      IF (F2.NE.0.0D0) CALL VPCLOSE
      IF (F1.EQ.0.0D0) CALL VPSTROKE
      IF (F1.EQ.1.0D0) CALL VPFILL
      IF (F1.EQ.2.0D0) CALL VPFLLSTR
      CALL SVGPATHEND
      RETURN
      END
!
!***************
      SUBROUTINE ARC1
      IMPLICIT NONE
      INCLUDE 'expl.cmn'
!----- end of common variables
      REAL(8) RX,RY,ROT,F1,F2,X1,Y1,X2,Y2,X,Y,Z
      INTEGER(4) I1,I2
!====
      CALL GELI(REC,RX)
      CALL GELI(REC,RY)
      CALL GELI(REC,ROT)
      CALL GELI(REC,F1)
      CALL GELI(REC,F2)

      I1=1
      I2=0
      CALL SVGPATH(I1,I2)

      CALL KOOLI(X,Y,Z)
      CALL XXYY(X,Y,Z,X1,Y1)
      CALL KOOLI(X,Y,Z)
      CALL XXYY(X,Y,Z,X2,Y2)

      CALL VPMOVE(X1,Y1)
      CALL VPARC1(RX,RY,ROT,F1,F2,X2,Y2)
!
      CALL SVGPATHEND
      RETURN
      END
!
!***************
      SUBROUTINE BEZ3
      IMPLICIT NONE
      INCLUDE 'expl.cmn'
!----- end of common variables
      REAL(8) X0X,X0Y,X1X,X1Y,X2X,X2Y,XX,YY,X,Y,Z
      INTEGER(4) I1,I2
!====
      CALL KOOLI(X,Y,Z)
      CALL XXYY(X,Y,Z,X0X,X0Y)
      CALL GELI(REC,X1X)
      CALL GELI(REC,X1Y)
      CALL GELI(REC,X2X)
      CALL GELI(REC,X2Y)
      CALL KOOLI(X,Y,Z)
      CALL XXYY(X,Y,Z,XX,YY)

      I1=1
      I2=0
      CALL SVGPATH(I1,I2)

      CALL VPMOVE(X0X,X0Y)
      CALL VPBEZ3(X1X,X1Y,X2X,X2Y,XX,YY)
!
      CALL SVGPATHEND
      RETURN
      END
!
!***************
      SUBROUTINE HISTO
      IMPLICIT NONE
      INCLUDE 'expl.cmn'
!----- end of common variables
      REAL(8) AN,DELTA,ZERO,X,Y,Z,XX,YY,F1
      INTEGER(4) KOD,NUM,I,J1,J2
!====
      ZERO=0.0D0
      CALL GELI(REC,F1)
      KOD=INT(F1)
      CALL GELI(REC,AN)
      CALL GELI(REC,DELTA)
      CALL GELI(REC,F1)
      NUM=INT(F1)
      Z=0.0D0
!-----
      IF (KOD.NE.0) THEN
      Y=U
      X=AN
      CALL XXYY(X,Y,Z,XX,YY)
      CALL VPMOVE(XX,YY)
      ELSE
      X=AN-DELTA/2.0D0
      END IF
!-----
      J1=1
      J2=0
      CALL SVGPATH(J1,J2)
      DO 500,I=1,NUM
      CALL GELI(REC,Y)
      IF (KOD.EQ.0) THEN
      X=X+DELTA
      CALL XXYY(X,Y,Z,XX,YY)
      IF (I.EQ.1) THEN
      CALL VPMOVE(XX,YY)
      ELSE
      END IF
      CALL VPDRAW(XX,YY)
      ELSE
      CALL XXYY(X,Y,Z,XX,YY)
      CALL VPDRAW(XX,YY)
      X=X+DELTA
      CALL XXYY(X,Y,Z,XX,YY)
      CALL VPDRAW(XX,YY)
      IF (KOD.EQ.2.OR.I.EQ.NUM) THEN
      CALL VPDRAW(XX,ZERO)
      END IF
      END IF
  500 CONTINUE
      CALL VPSTROKE
      CALL SVGPATHEND
      END
!
!***************
      SUBROUTINE XHIST
      IMPLICIT NONE
      INCLUDE 'expl.cmn'
!----- end of common variables
      REAL(8) F1,F2,F3,F4,X,Y,Z,XX,YY,XXOLD
      INTEGER(4) J1,J2
!====
      CALL GELI(REC,F1)
      CALL GELI(REC,F2)
      CALL GELI(REC,F3)
      CALL GELI(REC,F4)
      J1=1
      J2=0
      CALL SVGPATH(J1,J2)
      CALL KOOLI(X,Y,Z)
      CALL XXYY(X,Y,Z,XX,YY)
      CALL VPMOVE(XX,YY)
      XXOLD=XX
      CALL KOOLI(X,Y,Z)
   10 IF (X.EQ.999.0D0.AND.Y.EQ.999.0D0) GOTO 999
      CALL XXYY(X,Y,Z,XX,YY)
      CALL VPDRAW(XXOLD,YY)
      CALL VPDRAW(XX,YY)
      XXOLD=XX
      CALL KOOLI(X,Y,Z)
      GOTO 10
  999 CALL VPSTROKE
      CALL SVGPATHEND
      RETURN
      END
!
!***************
      SUBROUTINE YBARS
      IMPLICIT NONE
      INCLUDE 'expl.cmn'
!----- end of common variables
      REAL(8) F1,F2,X,Y,Z,XX,YY,XX1,YY1,XX2,YY2,GRS,SYM, &
      FX1,FX2,FY1,FY2
      INTEGER(4) J1,J2
!====
      Z=0.0D0
      CALL GELI(REC,SYM)
      CALL GELI(REC,GRS)
   10 CALL GELI(REC,X)
      CALL GELI(REC,Y)
      IF (X.EQ.999.0D0.AND.Y.EQ.999.0D0) GOTO 999
      CALL GELI(REC,F1)
      CALL GELI(REC,F2)
      CALL XXYY(X,Y,Z,XX,YY)
      CALL SYMBOL(XX,YY,SYM,GRS)
      CALL XXYY(X,F1,Z,XX1,YY1)
      CALL XXYY(X,F2,Z,XX2,YY2)
      FX1=XX-GRS/2.0D0
      FX2=XX+GRS/2.0D0
      FY1=YY-GRS/2.0D0
      FY2=YY+GRS/2.0D0
      IF (YY1.GT.YY) FY1=YY+GRS/2.0D0
      IF (YY2.LT.YY) FY2=YY-GRS/2.0D0
      J1=1
      J2=0
      CALL SVGPATH(J1,J2)
      CALL VPMOVE(FX1,YY1)
      CALL VPDRAW(FX2,YY1)
      CALL VPMOVE(XX,YY1)
      CALL VPDRAW(XX,FY1)
      CALL VPMOVE(XX,FY2)
      CALL VPDRAW(XX,YY2)
      CALL VPMOVE(FX1,YY2)
      CALL VPDRAW(FX2,YY2)
      CALL SVGPATHEND
      GOTO 10
  999 CALL VPSTROKE
      RETURN
      END
!
!***************
      SUBROUTINE BINEX
      IMPLICIT NONE
      INCLUDE 'expl.cmn'
!----- end of common variables
      CHARACTER(8) TEXT
      CHARACTER(3) XKODE
      REAL(8) X1,X2,Y1,Y2,GRS,GRZ,SYM,SSYM,FILL,X,Y,Z
      INTEGER(4) J1,J2
!====
      IF (SYSTEM.EQ.3) RETURN
      CALL GELI(REC,SYM)
      CALL GELI(REC,GRS)
    1 CALL TAXI(REC,TEXT)
      IF (TEXT.EQ.'ENDEX') GOTO 999
      CALL TAXI(REC,XKODE)
      CALL GELI(REC,FILL)
      CALL KOOLI(X,Y,Z)
      CALL XXYY(X,Y,Z,X1,Y1)
      CALL KOOLI(X,Y,Z)
      CALL XXYY(X,Y,Z,X2,Y2)
      IF (XKODE.EQ.'BOX') CALL BOX(X1,Y1,X2,Y2,FILL)
      IF (XKODE.EQ.'DIS') THEN
      SSYM=SYM
      IF (FILL.EQ.1.0D0) SSYM=-SYM
      GRZ=GRS/5.0D0
      CALL SYMBOL(X1,Y1,SSYM,GRZ)
      J1=1
      J2=0
      CALL SVGPATH(J1,J2)
      CALL VPMOVE(X1,Y1)
      CALL VPDRAW(X2,Y2)
      CALL VPSTROKE
      CALL SVGPATHEND
      CALL SYMBOL(X2,Y2,SSYM,GRS)
      END IF
      GOTO 1
  999 CONTINUE
      RETURN
      END
!
!***************
      SUBROUTINE BOX(X1,Y1,X2,Y2,FILL)
      IMPLICIT NONE
      INCLUDE 'expl.cmn'
!----- end of common variables
      REAL(8) X1,Y1,X2,Y2,FILL
      INTEGER(4) J1,J2
!====
      J1=1
      J2=0
      IF (FILL.NE.0.0D0) THEN
        J1=0
        J2=1
      END IF
      CALL SVGPATH(J1,J2)
      CALL VPMOVE(X1,Y1)
      CALL VPDRAW(X2,Y1)
      CALL VPDRAW(X2,Y2)
      CALL VPDRAW(X1,Y2)
      CALL VPCLOSE
      CALL SVGPATHEND
!
      IF (FILL.EQ.0.0D0) THEN
      CALL VPSTROKE
      ELSE
      CALL VPFILL
      END IF
      RETURN
      END
!
!***************
      SUBROUTINE TIELIN
      IMPLICIT NONE
      INCLUDE 'expl.cmn'
!----- end of common variables
      REAL(8) X,Y,Z,XZUG(500),YZUG(500),F1,XX,YY
      INTEGER(4) I1,I,NN,IN,J1,J2
!====
      NN=0
      CALL GELI(REC,F1)
      I1=INT(F1)
      CALL KOOLI(X,Y,Z)
      DO 500,I=1,500
      IF (X.EQ.999.0D0.AND.Y.EQ.999.0D0) GOTO 10
      CALL XXYY(X,Y,Z,XX,YY)
      XZUG(I)=XX
      YZUG(I)=YY
      CALL KOOLI(X,Y,Z)
      NN=I
  500 CONTINUE
   10 CONTINUE
      IN=NN/2
!     IF (IN.GE.500) THEN
      IF (IN.GE.0) THEN
      WRITE (6,1001) IN
 1001 FORMAT(1X,I4,' TIELINES READ.')
      END IF
!
      J1=1
      J2=0
      IF (I1.EQ.2) THEN
        J1=0
        J2=1
      END IF
      CALL SVGPATH(J1,J2)
      CALL VPMOVE(XZUG(1),YZUG(1))
      DO I=3,NN-1,2
        CALL VPDRAW(XZUG(I),YZUG(I))
      END DO
      DO I=NN,2,-2
        CALL VPDRAW(XZUG(I),YZUG(I))
      END DO
      CALL VPCLOSE
      CALL SVGPATHEND
!
      IF (I1.EQ.1) THEN
      J1=1
      J2=0
      CALL SVGPATH(J1,J2)
      DO I=1,NN-1,2
        CALL VPMOVE(XZUG(I),YZUG(I))
        CALL VPDRAW(XZUG(I+1),YZUG(I+1))
      END DO
      END IF
      CALL SVGPATHEND
!
      IF (I1.EQ.2) THEN
      CALL VPFILL
      ELSE
      CALL VPSTROKE
      END IF
      RETURN
      END
!
!***************
      SUBROUTINE INTEXT(IDENT,TEXT)
      IMPLICIT NONE
      INCLUDE 'expl.cmn'
!----- end of common variables
      CHARACTER(200) TEXT
      CHARACTER(6) IDENT
      REAL(8) X,Y,Z,GRS,XKOR,SKOR,YKOR,THETA,XX,YY,THDEG,SIZE
      INTEGER(4) LAE
!====
!      CALL TAXI(REC,TEXT)
      SKOR=0.0D0
      CALL KOOLI(X,Y,Z)
      CALL GELI(REC,GRS)
      CALL GELI(REC,XKOR)
      IF (IDENT.EQ.'TEXT'.OR.IDENT.EQ.'PSYM') CALL GELI(REC,SKOR)
      CALL GELI(REC,YKOR)
      CALL GELI(REC,THETA)
      CALL LAENGE(TEXT,LAE)
      IF (IDENT.EQ.'INTEXT'.OR.IDENT.EQ.'TEXT') THEN
      CALL XXYY(X,Y,Z,XX,YY)
      ELSE
      XX=X
      YY=Y
      END IF
      CALL PSMLEN(TEXT,LAE,GRS,SIZE)
      XKOR=XKOR*GRS+SKOR*SIZE
      YKOR=YKOR*GRS
      THDEG=THETA/57.29577951D0
      XX=XX+COS(THDEG)*XKOR-SIN(THDEG)*YKOR
      YY=YY+SIN(THDEG)*XKOR+COS(THDEG)*YKOR
      CALL VPTEXT(XX,YY,GRS,THETA,TEXT)
      RETURN
      END
!
!***************
      SUBROUTINE ECKEN
      IMPLICIT NONE
      INCLUDE 'expl.cmn'
!----- end of common variables
      CHARACTER(50) AA,BB,CC
      REAL(8) GRS,DA,RL,FK,SIZE,RR,RO,F1,F2,F3
      INTEGER(4) LA,LB,LC
!====
      FK=SQRT(3.0D0)/3.0D0
      CALL TAXI(REC,AA)
      CALL TAXI(REC,BB)
      CALL TAXI(REC,CC)
      CALL GELI(REC,GRS)
      CALL GELI(REC,DA)
      CALL LAENGE(AA,LA)
      CALL LAENGE(BB,LB)
      CALL LAENGE(CC,LC)
      CALL PSMLEN(AA,LA,GRS,SIZE)
      RL=DA*FK+SIZE/2.0D0
      F1=-RL
      F2=-DA-GRS
      F3=0.0D0
      CALL VPTEXT(F1,F2,GRS,F3,AA)
      CALL PSMLEN(BB,LB,GRS,SIZE)
      RR=SIZE/2.0D0-DA*FK
      F1=S-RR
      CALL VPTEXT(F1,F2,GRS,F3,BB)
      CALL PSMLEN(CC,LC,GRS,SIZE)
      RO=SIZE/2.0D0
      F1=S/2.0D0-RO
      F2=H+DA
      CALL VPTEXT(F1,F2,GRS,F3,CC)
      RETURN
      END
!
!***************
      SUBROUTINE KOOLI(X,Y,Z)
      IMPLICIT NONE
      INCLUDE 'expl.cmn'
!----- end of common variables
      REAL(8) X,Y,Z
!====
      CALL GELI(REC,X)
      CALL GELI(REC,Y)
      IF (SYSTEM.EQ.3) THEN
      CALL GELI(REC,Z)
      ELSE
      Z=0.0D0
      END IF
      RETURN
      END
!
!***************
      SUBROUTINE XXYY(X,Y,Z,XX,YY)
      IMPLICIT NONE
      INCLUDE 'expl.cmn'
!----- end of common variables
      REAL(8) X,Y,Z,CA,CB,CC,SUMME,XX,YY
!====
      IF (SYSTEM.EQ.3) THEN
      SUMME=X+Y+Z
      IF (SUMME.EQ.0.0D0) SUMME=1.0D0
!      IF (ABC.EQ.'ABC') THEN
      CA=X/SUMME
      CB=Y/SUMME
      CC=Z/SUMME
!      END IF
      IF (ABC.EQ.'BCA') THEN
      CB=X/SUMME
      CC=Y/SUMME
      CA=Z/SUMME
      END IF
      IF (ABC.EQ.'CAB') THEN
      CC=X/SUMME
      CA=Y/SUMME
      CB=Z/SUMME
      END IF
      IF (ABC.EQ.'CBA') THEN
      CC=X/SUMME
      CB=Y/SUMME
      CA=Z/SUMME
      END IF
      IF (ABC.EQ.'BAC') THEN
      CB=X/SUMME
      CA=Y/SUMME
      CC=Z/SUMME
      END IF
      IF (ABC.EQ.'ACB') THEN
      CA=X/SUMME
      CC=Y/SUMME
      CB=Z/SUMME
      END IF
      XX=(S/2.0D0)*(CC+2.0D0*CB)
      YY=H*CC
      ELSE
      XX=(X-L)*B/(R-L)
      YY=(Y-U)*H/(O-U)
      END IF
      RETURN
      END
!
!***************
      SUBROUTINE PSMLEN(CH,LL,GRS,SIZE)
      IMPLICIT NONE
      INCLUDE 'expl.cmn'
!----- end of common variables
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
      SUBROUTINE POLY(X,Y,N,R2,TH)
      IMPLICIT NONE
      INCLUDE 'expl.cmn'
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
      SUBROUTINE NPLOIG(NID,LONGCOD,NPEND)
      IMPLICIT NONE
      INCLUDE 'expl.cmn'
!----- end of common variables
      CHARACTER(80) XTEXT,YTEXT,GTEXT
      CHARACTER(500) LABEL
      REAL(8) GRS,XMIN,XMAX,YMIN,YMAX,XPOS,YPOS,SIZLAB,ANGE, &
      X(1000),Y(1000),Z,XX,YY,XZAHL,YZAHL,F1,F2,SYM,FAT3, &
      X99,Y99,SYM99,GR99,X98,Y98,SYM98,GR98,SYM97
      INTEGER(4) I,ISPEED,IAXCOL,NUMLBL,NPOINT,NPL,ISYMB, &
      ICOLPT,ICOLLB,IP(1000),NCHAR,NID,LONGCOD,NPEND,I1,I2
!====
      SYM99=99.0D0
      GR99=0.25D0
      SYM97=97.0D0
      SYM98=98.0D0
      GR98=0.5D0
      FAT3=0.009D0
      GRS=0.30D0
      XZAHL=-4.0D0
      YZAHL=-3.0D0
      IF (NID.EQ.1) THEN
      XTEXT=REC(1:80)
      READ (cln,FMT='(A80)') YTEXT
      ELSE
      READ (cln,FMT='(A80/A80)') XTEXT,YTEXT
      END IF
      IF (XTEXT(1:2).EQ.'L:') THEN
      LONGCOD=2
      GTEXT=XTEXT(3:)
      XTEXT=GTEXT
      END IF
!-----
      IF (LONGCOD.EQ.1) THEN
      READ (cln,FMT='(F10.4,5F10.4,2I3)') XMIN,XMAX,YMIN,YMAX, &
      B,H,ISPEED,IAXCOL
      ELSE
      READ (cln,FMT='(1PE20.12,3(0PE20.12),2F10.4,2I3)') &
      XMIN,XMAX,YMIN,YMAX,B,H,ISPEED,IAXCOL
      END IF
      IF (B.LE.0.0D0) B=15.0D0
      IF (H.LE.0.0D0) H=15.0D0
      IF (XMIN.EQ.XMAX) THEN
       XMIN=XMIN*0.9D0
       XMAX=XMAX*1.1D0
       IF (XMIN.EQ.0.0D0) THEN
        XMIN=-1.0D0
        XMAX=1.0D0
       END IF
      END IF
      IF (YMIN.EQ.YMAX) THEN
       YMIN=YMIN*0.9D0
       YMAX=YMAX*1.1D0
       IF (YMIN.EQ.0.0D0) THEN
        YMIN=-1.0D0
        YMAX=1.0D0
       END IF
      END IF
!-----
      IF (XTEXT.NE.' '.OR.YTEXT.NE.' ') THEN
      X99=-0.20D0
      Y99=H+0.20D0
      CALL SYMBOL(X99,Y99,SYM99,GR99)
      CALL AXIS(XTEXT,YTEXT,XMIN,XMAX,YMIN,YMAX,XZAHL,YZAHL)
      END IF
   10 READ (cln,1000,END=998) NUMLBL,NPOINT,NPL,ISYMB,ICOLPT,ICOLLB
!     WRITE (6,1000) NUMLBL,NPOINT,NPL,ISYMB,ICOLPT,ICOLLB
 1000 FORMAT (6I5)
      IF (NUMLBL.EQ.0.AND.NPOINT.EQ.0) GOTO 999
!!      IF (NPL.EQ.0) NPL=1
      IF (NPOINT.GT.1000) THEN
      WRITE (6,FMT='('' TOO MANY POINTS IN NPLOIG'')')
      RETURN
      END IF
      IF (NPOINT.GT.0) THEN
!-----
      IF (LONGCOD.EQ.1) THEN
      READ (cln,FMT='(7(2F10.4,I2))') (X(I),Y(I),IP(I),I=1,NPOINT)
      ELSE
      READ (cln,FMT='(7(2(0PE20.12),I2))') (X(I),Y(I),IP(I),I=1,NPOINT)
!     WRITE (6,FMT='(7(2(0PE20.12),I2))') (X(I),Y(I),IP(I),I=1,NPOINT)
      END IF
!-----
      END IF
!===  IF (NPOINT.GT.0)
!=== > READ (cln,*) (X(I),Y(I),IP(I),I=1,NPOINT)
      IF (ICOLPT.EQ.3) CALL VPFAT(FAT3)
      IP(1)=3
      I1=1
      I2=0
      CALL SVGPATH(I1,I2)
      DO 500,I=1,NPOINT
      F1=X(I)
      F2=Y(I)
      Z=0.0D0
      CALL XXYY(F1,F2,Z,XX,YY)
      IF (IP(I).EQ.3) THEN
      CALL VPMOVE(XX,YY)
      ELSE
      CALL VPDRAW(XX,YY)
      END IF
!!      SYM=DBLE(ISYMB)
!!      IF (ISYMB.NE.0.AND.MOD(I-1,NPL).EQ.0) CALL SYMBOL(XX,YY,SYM,GRS)
  500 CONTINUE
      CALL SVGPATHEND
      CALL VPSTROKE


      DO I=1,NPOINT
      F1=X(I)
      F2=Y(I)
      Z=0.0D0
      CALL XXYY(F1,F2,Z,XX,YY)
      SYM=DBLE(ISYMB)
      IF (NPL.NE.0) THEN
       IF (MOD(I-1,NPL).EQ.0) CALL SYMBOL(XX,YY,SYM,GRS)
      END IF
      END DO



      IF (ICOLPT.EQ.3) CALL VPFAT(FAT)
      DO 600,I=1,NUMLBL
!-----
      IF (LONGCOD.EQ.1) THEN
      READ (cln,1010) XPOS,YPOS,SIZLAB,ANGE,NCHAR,LABEL
 1010 FORMAT (2F10.4,F10.7,F10.4,I5,A500)
      ELSE
      READ (cln,1011) XPOS,YPOS,SIZLAB,ANGE,NCHAR,LABEL
!     WRITE (6,1011) XPOS,YPOS,SIZLAB,ANGE,NCHAR,LABEL
 1011 FORMAT (2(0PE20.12),F10.7,F10.4,I5,A500)
      END IF
!-----
      IF (XPOS.EQ.0.0D0.AND.YPOS.EQ.0.0D0.AND.LABEL.EQ.'97') THEN
      X98=B+1.0D0
      Y98=0.25D0
      CALL SYMBOL(X98,Y98,SYM97,GR98)
      GOTO 600
      END IF
!-----
      IF (XPOS.EQ.0.0D0.AND.YPOS.EQ.0.0D0.AND.LABEL.EQ.'98') THEN
      X98=B+1.0D0
      Y98=0.25D0
      CALL SYMBOL(X98,Y98,SYM98,GR98)
      GOTO 600
      END IF
!-----
!      NCHAR=INDEX(LABEL,'  ')-1
      CALL LABLA(LABEL,NCHAR)
      CALL XXYY(XPOS,YPOS,Z,XX,YY)
      ANGE=DMOD(ANGE,360.0D0)
      IF (ANGE.GT.180.0D0) ANGE=ANGE-360.0D0
      F1=ANGE
      IF (ABS(ANGE).NE.90.0D0) THEN
      ANGE=DATAN(DTAN(ANGE/57.29577951D0)*H*(R-L)/B/(O-U)) &
      *57.29577951D0
      IF (F1.GT.0D0.AND.ANGE.LT.0D0) ANGE=ANGE+180.0D0
      IF (F1.LT.0D0.AND.ANGE.GT.0D0) ANGE=ANGE-180.0D0
      END IF
      CALL VPTEXT(XX,YY,SIZLAB,ANGE,LABEL)
  600 CONTINUE
      GOTO 10
  998 NPEND=1
  999 RETURN
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
      DO I=IL,1,-1
        LAE=I
        IF (CH(I:I).NE.' ') GOTO 5
      END DO
    5 RETURN
      END
!
!***************
      SUBROUTINE FIBLA(CH,II)
      IMPLICIT NONE
      CHARACTER*(*) CH
      INTEGER(4) II,I,LAE
!====
      LAE=LEN(CH)
      DO I=1,LAE
        IF (CH(I:I).NE.' ') GOTO 1
      END DO
    1 II=I
      IF (II.EQ.LAE+1) II=0
      RETURN
      END
!
!***************
      SUBROUTINE GELI(RECO,FF)
      IMPLICIT NONE
      INCLUDE 'expl.cmn'
!----- end of common variables
      CHARACTER*(*) RECO
      CHARACTER(500) CH001
      CHARACTER(500) CH016
      REAL(8) FF
      INTEGER(4) I001,ZAHL
!====
      ZAHL=9
    1 CALL FIBLA(RECO,I001)
      IF (I001.EQ.0) THEN
      READ (UNIT=cln,FMT='(A500)',END=999) CH001
      RECO=CH001
      ELSE
      GOTO 2
      END IF
      GOTO 1
    2 CONTINUE
      CH001=RECO(I001:)
      I001=INDEX(CH001,' ')
      CH016=CH001(1:I001-1)
!      WRITE (UNIT=6,FMT='(''CH016 = '',A)') CH016
      READ (UNIT=CH016,FMT='(BN,F16.0)') FF
      RECO=CH001(I001:)
      RETURN
  999 CALL ENDEX(ZAHL)
      RETURN
      END
!
!***************
      SUBROUTINE TAXI(RECO,CHST)
      IMPLICIT NONE
      INCLUDE 'expl.cmn'
!----- end of common variables
      CHARACTER*(*) RECO
      CHARACTER(500) CH001
      CHARACTER*(*) CHST
      INTEGER(4) I1,ZAHL
!====
      ZAHL=9
    1 CALL FIBLA(RECO,I1)
      IF (I1.EQ.0) THEN
      READ (UNIT=cln,FMT='(A500)',END=999) CH001
      RECO=CH001
      ELSE
      GOTO 2
      END IF
      GOTO 1
    2 CONTINUE
      CHST=' '
      CH001=RECO(I1:)
      I1=INDEX(CH001,'  ')
      CHST=CH001(1:I1-1)
      RECO=CH001(I1:)
      RETURN
  999 CALL ENDEX(ZAHL)
      RETURN
      END
!
!***************
      SUBROUTINE ENDEX(ZAHL)
      IMPLICIT NONE
      INCLUDE 'expl.cmn'
!----- end of common variables
      INTEGER(4) ZAHL
!====
      WRITE (scr,1000) ZAHL
 1000 FORMAT (' END-OF-FILE, UNIT=',I2)
      STOP
      END
!
!***************
      SUBROUTINE VPINIT
      IMPLICIT NONE
      INCLUDE 'expl.cmn'
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
      SUBROUTINE VPLAND
      IMPLICIT NONE
      INCLUDE 'expl.cmn'
!----- end of common variables
!====
      IF (MAKEPS) THEN
      WRITE (pst,1000)
 1000 FORMAT('%!' &
      /'21 0 translate' &
      /'90 rotate')
      END IF
      RETURN
      END
!
!***************
      SUBROUTINE VPENDPLOT
      IMPLICIT NONE
      INCLUDE 'expl.cmn'
!----- end of common variables
      INTEGER(4) I1
      REAL(8) X1,Y1,B1,H1
      IF (MAKEPS) THEN
      WRITE (pst,1000)
 1000 FORMAT ('showpage')
      CLOSE (UNIT=pst)
      END IF

      IF (MAKESVG) THEN
      WRITE (svg,1005)
 1005 FORMAT (/'</g>')

!<image x="141.7" y="297.36" width="283.46" height="212.6" xlink:href="Base(5)_export.bmp">

      IF (IMAN.GT.0) THEN
      B1=IMAB*CMPX
      H1=IMAH*CMPX
      X1=(IMAX+X0)*CMPX
      Y1=595.0D0-(IMAY+Y0+IMAH)*CMPX
      CALL LABLA(IMACH,I1)
      WRITE (svg,1007) X1,Y1,B1,H1,IMACH(1:I1)
 1007 FORMAT (/'<image x="',F7.2,'" y="',F7.2,'" width="',F7.2, &
      '" height="',F7.2,'" xlink:href="',A,'"')
      END IF

      WRITE (svg,1010)
 1010 FORMAT (/'</svg>')
      CLOSE (UNIT=svg)
      END IF

      RETURN
      END
!
!***************
      SUBROUTINE VPSHOWP
      IMPLICIT NONE
      INCLUDE 'expl.cmn'
!----- end of common variables
      IF (MAKEPS) THEN
      WRITE (pst,1000)
 1000 FORMAT ('showpage')
      WRITE (pst,1005)
 1005 FORMAT('%!' &
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
      RETURN
      END
!
!***************
      SUBROUTINE VPNEWFONT(AA)
      IMPLICIT NONE
      INCLUDE 'expl.cmn'
!----- end of common variables
      CHARACTER(32) AA,BB
      INTEGER(4) I1
!====
      IF (MAKEPS) THEN
      I1=INDEX(AA,'  ')-1
      BB='('//AA(1:I1)//')'
      WRITE (pst,1000) BB
 1000 FORMAT (A32,' nf')
      END IF
      RETURN
      END
!
!***************
      SUBROUTINE VPFAT(FATO)
      IMPLICIT NONE
      INCLUDE 'expl.cmn'
!----- end of common variables
      REAL(8) FATO
!====
      IF (MAKEPS) THEN
      WRITE (pst,1000) FATO
 1000 FORMAT (F7.3,' slw ')
      END IF
!
      IF (MAKESVG) THEN
       SVGFAT=FATO
      END IF
      RETURN
      END
!
!***************
      SUBROUTINE VPDASH
      IMPLICIT NONE
      INCLUDE 'expl.cmn'
!----- end of common variables
!====
      IF (MAKEPS) THEN
      IF (DASH1.EQ.0.0D0.AND.GAP1.EQ.0.0D0.AND.DASH2.EQ.0.0D0 &
      .AND.GAP2.EQ.0.0D0) THEN
      WRITE (pst,1000)
 1000 FORMAT ('[] 0 dash')
      ELSE
      WRITE (pst,1010) DASH1,GAP1,DASH2,GAP2
 1010 FORMAT ('[',4F7.4,'] 0 dash')
      END IF
      END IF
      RETURN
      END
!
!***************
      SUBROUTINE VPMOVE(X1,Y1)
      IMPLICIT NONE
      INCLUDE 'expl.cmn'
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
      INCLUDE 'expl.cmn'
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
      SUBROUTINE VPARC1(RX,RY,ROT,F1,F2,X2,Y2)
      IMPLICIT NONE
      INCLUDE 'expl.cmn'
!----- end of common variables
      REAL(8) RX,RY,ROT,F1,F2,X2,Y2
      INTEGER(4) I1,I2
!====
      IF (MAKESVG) THEN
      I1=0
      I2=0
      IF (F1.NE.0.0D0) I1=1
      IF (F2.NE.0.0D0) I2=1
      WRITE(svg,1005) RX*CMPX,RY*CMPX,ROT,I1,I2, &
      (X2+X0)*CMPX,595.0D0-(Y2+Y0)*CMPX
 1005 FORMAT ('A ',F10.4,1X,F10.4,1X,F10.4,I4,I4,1X,F10.4,1X,F10.4)
      END IF
!
      RETURN
      END
!
!***************
      SUBROUTINE VPBEZ3(X1X,X1Y,X2X,X2Y,XX,YY)
      IMPLICIT NONE
      INCLUDE 'expl.cmn'
!----- end of common variables
      REAL(8) X0X,X0Y,X1X,X1Y,X2X,X2Y,XX,YY,Q1,Q2,Q3,Q4,Q5,Q6
!====
      IF (MAKESVG) THEN

      Q1=(X1X-X0)*CMPX
      Q2=595.0D0-(X1Y+Y0)*CMPX
      Q3=(X2X-X0)*CMPX
      Q4=595.0D0-(X2Y+Y0)*CMPX
      Q5=(XX-X0)*CMPX
      Q6=595.0D0-(YY+Y0)*CMPX

      WRITE(svg,1005) (X1X+X0)*CMPX,595.0D0-(X1Y+Y0)*CMPX, &
      (X2X+X0)*CMPX,595.0D0-(X2Y+Y0)*CMPX, &
      (XX+X0)*CMPX,595.0D0-(YY+Y0)*CMPX
 1005 FORMAT ('C ',F10.4,1X,F10.4,1X,F10.4,1X,F10.4,1X,F10.4,1X,F10.4)

      WRITE (6,2000) X1X,X1Y,X2X,X2Y,XX,YY
 2000 FORMAT ('C ',F10.4,1X,F10.4,1X,F10.4,1X,F10.4,1X,F10.4,1X,F10.4)
      WRITE (6,2005) X0,Y0,CMPX
 2005 FORMAT ('X0,Y0,CMPX ',F10.4,1X,F10.4,1X,F10.4,1X,F10.4,1X,F10.4,1X,F10.4)


      END IF
!
      RETURN
      END
!
!***************
      SUBROUTINE VPCLOSE
      IMPLICIT NONE
      INCLUDE 'expl.cmn'
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
      SUBROUTINE VPCLIP
      IMPLICIT NONE
      INCLUDE 'expl.cmn'
!----- end of common variables
      IF (MAKEPS) THEN
      WRITE(pst,1000)
 1000 FORMAT (' clip ')
      END IF
      RETURN
      END
!
!***************
      SUBROUTINE VPNEW
      IMPLICIT NONE
      INCLUDE 'expl.cmn'
!----- end of common variables
      IF (MAKEPS) THEN
      WRITE(pst,1000)
 1000 FORMAT (' newpath ')
      END IF
      RETURN
      END
!
!***************
      SUBROUTINE VPSTROKE
      IMPLICIT NONE
      INCLUDE 'expl.cmn'
!----- end of common variables
      IF (MAKEPS) THEN
      IF (LINCOL.EQ.0) THEN
      WRITE(pst,1000) LGRAY
 1000 FORMAT (F10.4,' setgray  stroke ')
      ELSE
      WRITE(pst,1002) LCOLR,LCOLG,LCOLB
 1002 FORMAT (3(1X,F10.4),' setrgbcolor  stroke ')
      END IF
      END IF
      RETURN
      END
!
!***************
      SUBROUTINE VPTHERDOM(GRS)
      IMPLICIT NONE
      INCLUDE 'expl.cmn'
!----- end of common variables
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
      END IF
      RETURN
      END
!
!***************
      SUBROUTINE SVGTHERDOM(X,Y,GRS)
      IMPLICIT NONE
      INCLUDE 'expl.cmn'
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
      INCLUDE 'expl.cmn'
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
      INCLUDE 'expl.cmn'
!----- end of common variables
      REAL(8) GRS,X,Y,XC,YC,XMUL,YMUL,FF,XCOR
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
      FF=30.0D0/31.0D0
      XCOR=GRS/30.0D0
      XC=X0+X-GRS*FF/2.0D0+XCOR*1.0D0
      YC=Y0+Y-GRS/2.0D0
      IF (MAKEPS) THEN
      WRITE (pst,FMT='(''/cdch <'')')
      WRITE (pst,FMT='(A60)') (CDCH(J),J=1,NY)
      WRITE (pst,FMT='(''> def'')')
      WRITE (pst,1000) XC,YC,NX,NY,XMUL,YMUL
 1000 FORMAT ('gsave',2X,2F10.6,2X,'translate',I4,2X,I4,'  8  ','[', &
      F10.6,'  0  0  ',F10.6,'  0  0]  {cdch} image grestore')
      END IF
      RETURN
      END
!
!***************
      SUBROUTINE VPFILL
      IMPLICIT NONE
      INCLUDE 'expl.cmn'
!----- end of common variables
      IF (MAKEPS) THEN
      IF (FILCOL.EQ.0) THEN
      WRITE(pst,1000) FGRAY
 1000 FORMAT (F10.4,' setgray  fill ')
      ELSE
      WRITE(pst,1002) FCOLR,FCOLG,FCOLB
 1002 FORMAT (3(1X,F10.4),' setrgbcolor  fill ')
      END IF
      END IF
      RETURN
      END
!
!***************
      SUBROUTINE VPFLLSTR
      IMPLICIT NONE
      INCLUDE 'expl.cmn'
!----- end of common variables
      IF (MAKEPS) THEN
      WRITE(pst,1000)
 1000 FORMAT (' gsave ')
      CALL VPFILL
      WRITE(pst,1002)
 1002 FORMAT (' grestore ')
      CALL VPSTROKE
      END IF
      RETURN
      END
!
!***************
      SUBROUTINE VPPOINT(X,Y,GRS,II)
      IMPLICIT NONE
      INCLUDE 'expl.cmn'
!----- end of common variables
      REAL(8) X,Y,GRS
      INTEGER(4) II
!====
      IF (MAKEPS) THEN
      WRITE(pst,1000) X+X0,Y+Y0,GRS
 1000 FORMAT (F10.4,1X,F10.4,1X,F10.4,'  pt ')
      IF (II.EQ.1) THEN
      CALL VPSTROKE
      ELSE
      CALL VPFILL
      END IF
      END IF
      RETURN
      END
!
!***************
      SUBROUTINE VPTEXT(X1,Y1,GRS,THETA,TEXT)
      IMPLICIT NONE
!
      INCLUDE 'expl.cmn'
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

      DO I=1,J2
       IF (TEXT(I:I).EQ.'&') TEXT(I:I)=' '
      END DO

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
      SUBROUTINE SHOWTEX(TEXT,I1,I2)
      IMPLICIT NONE
      INCLUDE 'expl.cmn'
!----- end of common variables
      CHARACTER*(*) TEXT
      CHARACTER(500) TEXT2
      CHARACTER(1) A1
      INTEGER(4) I1,I2,I92,I,I001
!====
      IF (MAKEPS) THEN
      IF (TEXT.EQ.' ') RETURN
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
      WRITE (pst,1000) TEXT2(1:I001)//' '
 1000 FORMAT ('(',A,') show')
      END IF
      RETURN
      END
!
!***************
      SUBROUTINE SVGPATH(I1,I2)
      IMPLICIT NONE
      INCLUDE 'expl.cmn'
!----- end of common variables
      INTEGER(4) I1,I2,J1,J2
      REAL(8) DICKE
      CHARACTER(7) VOLL,STRO
!
      IF (MAKESVG) THEN
      IF (I1.EQ.0) THEN
        DICKE=0.0D0
      ELSE
        DICKE=SVGFAT*CMPX
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
      INCLUDE 'expl.cmn'
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
      INCLUDE 'expl.cmn'
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
      INCLUDE 'expl.cmn'
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
      SUBROUTINE LASTCHAR(CH,II)
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
      INTEGER(4) I001,II,J
      CALL LABLA(CH,II)
      WRITE (UNIT=I001,FMT='(500A1)') (CH(J:J),J=1,II)
      RETURN
      END
!-----
!********************************
      SUBROUTINE helpme(tag,tpath,itpath)
      IMPLICIT NONE
      CHARACTER*(*) tag,tpath
      INTEGER(4)  hlp,kbd,scr,log,cln,plt,rxn,grd,pst,svg
      common /iounits/ hlp,kbd,scr,log,cln,plt,rxn,grd,pst,svg
      CHARACTER line*130
      CHARACTER(120) FNAME
      INTEGER(4) itpath,j
!-----
      FNAME=tpath(1:itpath)//'thhelp.txt'
      CALL LABLA(FNAME,j)
      OPEN(UNIT=hlp,FILE=FNAME(1:j),STATUS='OLD',ERR=201)
   30 READ(UNIT=hlp,FMT='(A)',END=150) line
      IF (line.NE.tag) GOTO 30
!-----
   50 READ(UNIT=hlp,FMT='(A)',END=150) line
      IF (line(1:1).EQ.'§') THEN
        WRITE (UNIT=scr,FMT='(/A)') 'press CR to continue!'
        READ (kbd,*)
        GOTO 50
      END IF
      IF (line(1:4).EQ.'$END') GOTO 200
      WRITE (UNIT=scr,FMT='(A80)') line(1:80)
      GOTO 50
  150 CONTINUE
      WRITE(scr,155) tag
  155 FORMAT(' Help for',1x,a,1x,'does not exist!')
  200 CLOSE (hlp)
      RETURN
  201 WRITE(scr,160)
  160 FORMAT(' Help-file not found.')
      RETURN
      END
!-----
!******************************
      SUBROUTINE writetit(iu,os)
      implicit none
      INCLUDE 'expl.cmn'
      character progname*15,vers*20,task*80
      character *(*) os
      integer i, j, k, iu
      progname='EXPLOT'
      vers='28.05.2022'
      task='"Create a PostScritp(TM) file from graphics input"'
      call LABLA(progname,i)
      call LABLA(vers,j)
      call LABLA(os,k)
!
      if(iu.eq.scr) call clearscreen
      WRITE (iu,1000) progname(1:i), vers(1:j), os(1:k)
1000  FORMAT (/, &
      'Program',1x,a,', Version (dd.mm.yy)',1x,a,1x,'(',a,')')
      j=i+j+k
      write(UNIT=iu,FMT='(132A1)') ('=',i=1,32+j)
      call LABLA(task,i)
      write(iu,1001) task(1:i)
 1001 format(/,a,//, &
      'Written by:', &
      /,10x,'Christian de Capitani (Basel, Switzerland)', &
      /,10x,'E-mail: christian.decapitani@unibas.ch', &
      //,'Input dialogue and help by:', &
      /,10x,'Konstantin Petrakakis (Vienna, Austria)', &
      /,10x,'E-mail: konstantin.petrakakis@univie.ac.at'/)
      write(UNIT=iu,FMT='(132A1)') ('=',j=1,80)
      RETURN
      END
!-----
!******************************
      block data guzzini
      INCLUDE 'expl.cmn'
      data  hlp,kbd,scr,log,cln,plt,rxn,grd,pst,svg &
            / 2,  5,  6, 12, 13, 14, 15, 16, 17, 40/
      end
