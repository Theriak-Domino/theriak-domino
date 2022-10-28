!-----Version: 28.05.2022
!               ***********
!               * GUZZLER *
!               ***********
!
!     Program written by Christian de Capitani
!     at the Department of Geological Sciences,
!     University of British Columbia, Vancouver, B.C. Canada
!     (May 1984 - Sept 1987)
!
!     revision: September 1993
!     revision: October 2002
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
!     This program reads a NPLOIG-input as produced by PTAX etc.
!     and re-labels the curves.
!     WARNING: Due to some lovable errors the program may develop
!     a certain creativity.
!
!23456789.123456789.123456789.123456789.123456789.123456789.123456789.12
      IMPLICIT NONE
!
      INTEGER(4) MAXPT,MAXGRD,MAXLIN,MATE
      PARAMETER (MAXPT=50000,MAXGRD=100,MAXLIN=1000,MATE=2)
!-----
      INTEGER(4) GITTER(0:1,-1:MAXGRD,-1:MAXGRD), &
      TEMPLE(-MATE:MATE,-MATE:MATE),NXGRID,NYGRID
      COMMON /GITT/ GITTER,TEMPLE,NXGRID,NYGRID
!-----
      REAL(8) XX(MAXPT),YY(MAXPT),LAE(MAXPT)
      INTEGER(4) IPEN(MAXPT),IXX(MAXPT),IYY(MAXPT),DANGER(MAXPT)
      LOGICAL(4) ORIG(MAXPT)
      COMMON /PTRE/ XX,YY,LAE
      COMMON /PTIN/ IPEN,IXX,IYY,DANGER
      COMMON /PTLO/ ORIG
!-----
      REAL(8) XPOS(MAXLIN,2),YPOS(MAXLIN,2),ANGE(MAXLIN,2), &
      SIZLAB(MAXPT,2),SIGL(MAXLIN,2),CENTRE(MAXLIN)
      INTEGER(4) CODE(MAXLIN),FIRST(MAXLIN),LAST(0:MAXLIN), &
      NCHAR(MAXLIN,2),REANR(0:MAXLIN),NCHN(MAXLIN),LINKS(MAXLIN)
      CHARACTER(100) LABEL(MAXLIN,2),REACH(MAXLIN)
      COMMON /LIRE/ XPOS,YPOS,ANGE,SIZLAB,SIGL,CENTRE
      COMMON /LIIN/ CODE,FIRST,LAST,NCHAR,REANR,NCHN,LINKS
      COMMON /LICH/ LABEL,REACH
!-----
      INTEGER(4) NRE,NL,NP
      REAL(8) GRS,GRZ,DEGRAD,DMIN,DELX,DELY,XFAC,YFAC,SFAC
      COMMON /COUNT/ NRE,NL,NP
      COMMON /USEF/ GRS,GRZ,DEGRAD,DMIN,DELX,DELY,XFAC,YFAC,SFAC
!-----
      REAL(8) X(1000),Y(1000),XMIN,XMAX,YMIN,YMAX,BREITE,HOEHE, &
      FF,FF1,FF2,F1,F2,F4,XOFF,GAP,PI2,PI,ZERO,DIST,XKOR,YKOR,SIZE, &
      D0,THE,THEP,THKOR,X1,Y1,X2,Y2,X11,X12,Y11,Y12,XL,ADDFAC, &
      XZU(100),YZU(100),SIZU(100),ANZU(100),MINILAE, &
      OLDL,OLDR,OLDB,OLDXF,OLDU,OLDO,OLDH,OLDYF,XCM,XUS,YCM,YUS
      INTEGER(4) BLANK(30),IFORCE(MAXLIN),FONT,IPLTR,IP(1000), &
      ISPEED,IAXCOL,I,II,I0,I1,I2,N1,LL,I001,I002,LOC,LASTRE,NBLA, &
      NAIMNR,NAIM,NAIMAX,NGLOB,NFORCE,NFCODE,ICH,ILF, &
      NUMLBL,NPOINT,NPL,ISYMB,ICOLPT,ICOLLB,NZU,NCHZU(100)
      CHARACTER(80) XTEXT,YTEXT
      CHARACTER(5) HILCH,FORX,FORY
      CHARACTER(15) FORXY
      CHARACTER(100) FORM,LABZU(100)
      CHARACTER(200) ARCH
      CHARACTER(500) CH001,CH002,CHIN(3)
      LOGICAL(4) PRINP,LONGCOD,PRT98,PRT97,OLDX,OLDY
!-----
!...... FILE 13: OLD NPLOIG-INPUT   ---> unit= plt
!...... FILE  6: COMMENTS
!...... FILE  7: NEW NPLOIG-INPUT   ---> unit= cln
!...... FILE  9: GITTER(0,...)      ---> unit= grd
!...... FILE 10: TABLE OF REACTIONS ---> unit= rxn
!
!---------------------------------------------------------------------
!
      CHARACTER(500) wpath,tpath,CPLNAME,FNAME,LOGFNAME
      INTEGER(4) iwpath,itpath
      character filetype(8)*3
      CHARACTER(30) filelog, filecln,fileplt,filerxn,filegrd
      character largum(5)*80,sdate*21
      integer ierr,j,larg
      character dir*1, ext*10, os*20
      integer  hlp,kbd,scr,log,cln,plt,rxn,grd
      common /iounits/ hlp,kbd,scr,log,cln,plt,rxn,grd
!-----
      data dir, ext, os /'/',2*' '/
      data largum /5*' '/
      data filetype /'hlp','kbd','scr','log','cln','plt','rxn','grd'/
!-----


      DO I0=0,1
        DO I1=-1,MAXGRD
          DO I2=-1,MAXGRD
            GITTER(I0,I1,I2)=0
          END DO
        END DO
      END DO
      DO I1=1,MAXPT
        XX(I1)=0.0D0
        YY(I1)=0.0D0
        LAE(I1)=0.0D0
        IXX(I1)=0
        IYY(I1)=0
        IPEN(I1)=0
        DANGER(I1)=0
      END DO
      DO I1=1,1000
        X=0.0D0
        Y=0.0D0
        IP=0
      END DO


!      DATA GITTER/20808*0/
!      DATA XX/50000*0.0D0/
!      DATA YY/50000*0.0D0/
!      DATA LAE/50000*0.0D0/
!      DATA IXX/50000*0/
!      DATA IYY/50000*0/
!      DATA IPEN/50000*0/
!      DATA DANGER/50000*0/
!      DATA X/1000*0.0D0/
!      DATA Y/1000*0.0D0/
!      DATA IP/1000*0/
!=====
      CALL GetEnvVar(tpath,itpath,ierr, dir, ext, os)
      CALL getwork(wpath,iwpath,ierr,dir)
!      WRITE (*,FMT='(/,''program directory: '',A)') tpath(1:itpath)
!      WRITE (*,FMT='(''working directory: '',A)') wpath(1:iwpath)
!-----
      LONGCOD=.FALSE.
      sdate=' '
      filelog='guzzler.last'
      filecln='clean'
      fileplt='coplot'
      filerxn='table'
      filegrd='gitter'
!*****
      j=scr
      call writetit (j,os)
      call getdate(sdate)
      write(scr,'(''RUN: '',a/)') sdate
!-----
!     largum(1) = OLD NPLOIG-INPUT   ---> unit= plt
!     largum(2) = NEW NPLOIG-INPUT   ---> unit= cln
!     largum(3) = TABLE OF REACTIONS ---> unit= rxn
!-----
      larg=0
      ierr=0
      do 400 I=1,5
          call GetLineArgs (I,largum(I),ierr)
          if(ierr.ne.0.or.largum(I).eq.' ')then
             goto 401
          else
             larg=larg+1
          end if
  400 continue
  401 continue
!-----
!      WRITE (UNIT=6,FMT='(''larg: '',i2)') larg
!      DO I=1,larg
!        WRITE (UNIT=6,FMT='(''arg: '',i2,1x,a)') I,largum(I)
!      END DO
!-----
      if(largum(1).eq.'-h') then
         call helpme('$GUZ-CALL',tpath,itpath)
         stop
      end if
!*****
      CHIN(1)=' '
      CHIN(2)='0.2'
      CHIN(3)='-3'
      FNAME=wpath(1:iwpath)//filelog
      LOGFNAME=FNAME
      CALL LABLA(FNAME,j)
      WRITE (*,FMT='(''try working directory: '',A)') FNAME(1:j)
      OPEN(UNIT=log,FILE=FNAME(1:j),STATUS='OLD',ERR=300)
      GOTO 301
  300 CONTINUE
      FNAME=tpath(1:itpath)//filelog
      LOGFNAME=FNAME
      CALL LABLA(FNAME,j)
      WRITE (*,FMT='(''try program directory: '',A)') FNAME(1:j)
      OPEN (UNIT=log,FILE=FNAME(1:j),STATUS='UNKNOWN')
  301 CONTINUE
!-----
      CALL LABLA(LOGFNAME,j)
      WRITE (UNIT=scr,FMT='(/,''log-file used: '',A)') LOGFNAME(1:j)
!-----
      DO I=1,3
        READ (UNIT=log,FMT='(A500)',END=411) CHIN(I)
      END DO
  411 CALL LABLA(CHIN(1),I002)
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
         call helpme('$GUZ-START',tpath,itpath)
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
      CALL LABLA(CPLNAME,I001)
!      WRITE (*,FMT='(''try working directory: '',A)') CPLNAME(1:I001)
!-----
      OPEN(UNIT=plt,FILE=CPLNAME(1:I001),STATUS='OLD',ERR=310)
      GOTO 312
  310 CONTINUE
      CPLNAME=tpath(1:itpath)//CH001
      I001=INDEX(CPLNAME,'   ')-1
!      WRITE (*,FMT='(''try program directory: '',A)') CPLNAME(1:I001)
      OPEN(UNIT=plt,FILE=CPLNAME(1:I001),STATUS='OLD',ERR=311)
      wpath=tpath
      iwpath=itpath
      GOTO 312
  311 WRITE (*,*) 'Input-file not found.'
      STOP
  312 CONTINUE
!=====
      WRITE (*,FMT='(/,''working directory: '',A)') wpath(1:iwpath)
!=====
      if(larg.ge.2) then
         CH001=largum(2)
      else
         CH001=filecln
      end if
      FNAME=wpath(1:iwpath)//CH001
      CALL LABLA(FNAME,I001)
      OPEN(UNIT=cln,FILE=FNAME(1:I001),STATUS='UNKNOWN')
!=====
      FNAME=wpath(1:iwpath)//filegrd
      CALL LABLA(FNAME,I001)
      OPEN(UNIT=grd,FILE=FNAME(1:I001),STATUS='UNKNOWN')
!=====
      if(larg.ge.3) then
         CH001=largum(3)
      else
         CH001=filerxn
      end if
      FNAME=wpath(1:iwpath)//CH001
      CALL LABLA(FNAME,I001)
      OPEN(UNIT=rxn,FILE=FNAME(1:I001),STATUS='UNKNOWN')
!=====
      REWIND plt
      REWIND cln
      REWIND grd
      REWIND rxn
!*****
      I001=INDEX(CPLNAME,'   ')-1
      PRINP=.FALSE.
      IF (CPLNAME(I001+1:).NE.' ') PRINP=.TRUE.
!......
!...... ONE UNIT OF POINTS PLUS LABELS IS CALLED ONE REACTION.
!...... EACH TIME IPEN=3 IS ENCOUNTERED, A NEW LINE SEGMENT IS STARTED.
!...... IF A REACTION CONSISTS OF N LINE SEGMENTS, IT IS ASSUMED THAT
!...... 2*N LABELS FOLLOW.
!...... ALL LENGTHS ARE TRANSFORMED TO CM, ALL ANGLES TO RAD.
!......
!...... BOOK-KEEPING:
!...... FOR EACH POINT: XX,YY: COORDINATES, LAE: DISTANCE FROM LAST POINT
!......                 WITH IPEN=3, IXX,IYY: GRID-COORDINATES,
!......                 DANGER: VALUE FROM 0 TO 3
!...... FOR EACH LINE SEGMENT: SIGL: =+1 IF LABEL TO THE LEFT
!......                        CODE: = 3 IF CURVE CAN BE LABELED,
!......                              = 2 IF CURVE IS NUMBERED
!......                              = 1 IF CURVE IS NUMBERED OFFSET
!......                              = 0 IF CURVE IS NOT LABELED
!......                  FIRST,LAST: FIRST AND LAST POINT OF LINE

!----- READ PRELUDE
!
      READ (plt,FMT='(A80/A80)') XTEXT,YTEXT
      IF (PRINP) WRITE (6,FMT='(A80/A80)') XTEXT,YTEXT
      IF (XTEXT(1:2).EQ.'L:') LONGCOD=.TRUE.
!     IF (XTEXT.EQ.' ') XTEXT='TEMPERATURE (deg. C)'
!     IF (YTEXT.EQ.' ') YTEXT='PRESSURE (kBar)'
!-----
      IF (LONGCOD) THEN
      READ (plt,FMT='(0PE20.12,3(0PE20.12),2F10.4,2I3)') &
      XMIN,XMAX,YMIN,YMAX,BREITE,HOEHE,ISPEED,IAXCOL
      IF (PRINP) WRITE (6,FMT='(1PE20.11,5(1PE20.12),2I3)') &
      XMIN,XMAX,YMIN,YMAX,BREITE,HOEHE,ISPEED,IAXCOL
      ELSE
!      READ (plt,FMT='(I1,F9.4,5F10.4,2I3)') IPLTR,XMIN,XMAX,YMIN,YMAX, &
!      BREITE,HOEHE,ISPEED,IAXCOL
      READ (plt,FMT='(F10.4,5F10.4,2I3)') XMIN,XMAX,YMIN,YMAX, &
      BREITE,HOEHE,ISPEED,IAXCOL
      IPLTR=0
      IF (PRINP) WRITE (6,FMT='(I1,1PE9.2,5(1PE10.3),2I3)') &
      IPLTR,XMIN,XMAX,YMIN,YMAX,BREITE,HOEHE,ISPEED,IAXCOL
      END IF
!=====
!----- used to recalculate new xy for labels
!=====
      OLDL=XMIN
      OLDR=XMAX
      OLDB=BREITE
      OLDXF=OLDB/(OLDR-OLDL)
      OLDU=YMIN
      OLDO=YMAX
      OLDH=HOEHE
      OLDYF=OLDH/(OLDO-OLDU)
      OLDX=.TRUE.
      OLDY=.TRUE.
!=====
      IF(larg.EQ.0.AND.LONGCOD) THEN
      WRITE(scr,4000) XMIN,XMAX,BREITE
 4000 FORMAT (/,' Enter [ CR | X-min  X-max  width ]  <' &
      ,2F20.8,2X,F8.3,'>? ')
      READ(5,FMT='(A500)') CH001
      IF (CH001.NE.' ') THEN
      CALL GELI(CH001,XMIN)
      CALL GELI(CH001,XMAX)
      CALL GELI(CH001,FF)
      FF=DABS(FF)
      IF (FF.GT.0.1) BREITE=FF
      ELSE
       OLDX=.FALSE.
      END IF
      WRITE(scr,4005) YMIN,YMAX,HOEHE
 4005 FORMAT (/,' Enter [ CR | Y-min  Y-max  height ] <' &
      ,2F20.8,2X,F8.3,'>? ')
      READ(5,FMT='(A500)') CH001
      IF (CH001.NE.' ') THEN
      CALL GELI(CH001,YMIN)
      CALL GELI(CH001,YMAX)
      CALL GELI(CH001,FF)
      FF=DABS(FF)
      IF (FF.GT.0.1) HOEHE=FF
      ELSE
       OLDY=.FALSE.
      END IF
      END IF
!=====
!=====
!
!----- INITIALIZE USEFUL VARIABLES
!
!...... GRS: SIZE OF LABELS         GRZ: SIZE OF NUMBERS
!...... XOFF: DISTANCE BETWEEN CURVE AND LABEL = XOFF*GRS
!...... SFAC: MYSTIC FACTOR         GAP: NOT IMPORTANT
!...... NRE: NUMBER OF REACTIONS
!...... NL: NUMBER OF LINE SEGMENTS
!...... NP: NUMBER OF POINTS
!
!----
      FF=0.005D0*DABS(XMAX-XMIN)/BREITE
      F1=-DLOG10(FF)+0.5D0
      I1=IDNINT(F1)+1
      IF (I1.LT.1) I1=1
      IF (I1.GT.7) I1=7
      WRITE (UNIT=FORX,FMT='(''F10.'',I1)') I1
      F4=0.005D0*DABS(YMAX-YMIN)/HOEHE
      F1=-DLOG10(F4)+0.5D0
      I1=IDNINT(F1)+1
      IF (I1.LT.1) I1=1
      IF (I1.GT.7) I1=7
      WRITE (UNIT=FORY,FMT='(''F10.'',I1)') I1
      FORXY=FORX//','//FORY
!#####
!     FORXY='1PE10.3,1PE10.3'
!#####
!     GRS=0.25D0
!     GRZ=0.31D0
      FONT=2
      CALL LABLA(CHIN(2),I002)
      IF (I002.EQ.0) I002=1
  415 CONTINUE
!-----Check if command line arguments are present
      CH002='Enter [ "?" | CR | size of labels ] <'// &
      CHIN(2)(1:I002)//'>?'
      CALL LABLA(CH002,j)
      if(larg.eq.0) then
        WRITE (UNIT=6,FMT='(/,A)') CH002(1:j)
        READ (5,FMT='(A500)') CH001
      else
        CH001=CHIN(2)(1:I002)
      end if
!-----
      if(CH001.eq.'?') then
         call helpme('$GUZ-SIZE',tpath,itpath)
         goto 415
      end if
!-----
      CALL LABLA(CH001,I001)
      IF (I001.EQ.0) THEN
      CH001=CHIN(2)
      I001=I002
      ELSE
      CHIN(2)=CH001
      END IF
      CALL GELI(CH001,GRS)
!+++++store terminal input
      CLOSE (UNIT=log)
      FNAME=wpath(1:iwpath)//filelog
      CALL LABLA(FNAME,j)
      OPEN(UNIT=log,FILE=FNAME(1:j),STATUS='UNKNOWN')
!-----
      DO I=1,3
        CALL PUST(log,CHIN(I))
      END DO
      CLOSE (UNIT=log)
!*****
      GRZ=1.2D0*GRS
      XOFF=0.4D0
!     SFAC=1.0D0/0.75D0
      SFAC=1.0D0
      GAP=0.05D0
      PI2=DATAN(1.0D0)*2.0D0
      PI=2.0D0*PI2
      DEGRAD=90.0D0/PI2
      ZERO=0.0D0
      NRE=0
      NL=0
      NP=0
      REANR(0)=0
      LAST(0)=0
      DMIN=(1.1D0+XOFF)*GRS*2.0D0/DBLE(MATE+1)
      NAIMNR=MATE+1-IDINT(1.1D0*GRZ/DMIN)
      IF (NAIMNR.GT.MATE) NAIMNR=MATE
      IF (NAIMNR.LT.1) NAIMNR=1
      NXGRID=MIN0(IDINT(BREITE/DMIN),MAXGRD)
      NYGRID=MIN0(IDINT(HOEHE/DMIN),MAXGRD)
      DELX=BREITE/DBLE(NXGRID)
      DELY=HOEHE/DBLE(NYGRID)
      XFAC=BREITE/(XMAX-XMIN)
      YFAC=HOEHE/(YMAX-YMIN)
      NZU=0
      PRT97=.FALSE.
      PRT98=.FALSE.
      MINILAE=0.02D0
!-----
      DO II=-1,NYGRID
      DO I=-1,NXGRID
        IF (I.LE.0.OR.I.GE.NXGRID-1.OR.II.LE.0.OR.II.GE.NYGRID-1) THEN
          GITTER(0,I,II)=MATE
        ELSE
          GITTER(0,I,II)=0
        END IF
        GITTER(1,I,II)=0
      END DO
      END DO
!-----
      DO II=-MATE,MATE
      DO I=-MATE,MATE
        F1=DSQRT(DBLE(I*I+II*II))
        I1=MATE+1-MIN0(IDINT(F1),MATE+1)
        TEMPLE(I,II)=I1
      END DO
      END DO
!-----
!----- READ THE REACTIONS
!
 2222 READ (plt,1005,END=9999) NUMLBL,NPOINT,NPL,ISYMB,ICOLPT,ICOLLB
      IF (PRINP) WRITE (6,1005) NUMLBL,NPOINT,NPL,ISYMB,ICOLPT,ICOLLB
 1005 FORMAT (6I5)
      IF (NPOINT.EQ.0.AND.NUMLBL.EQ.0) GOTO 2222
      IF (NPOINT.EQ.0.AND.NUMLBL.GT.0) THEN
!      DO 505,I=NZU+1,NZU+NUMLBL
      I=NZU
      DO 505,II=1,NUMLBL
      I=I+1
!-----
      IF (LONGCOD) THEN
      READ (plt,1002) XZU(I),YZU(I),SIZU(I),ANZU(I),NCHZU(I),LABZU(I)
      IF (PRINP) WRITE (6,1001) XZU(I),YZU(I),SIZU(I),ANZU(I), &
      NCHZU(I),LABZU(I)
 1002 FORMAT (2(0PE20.12),0PE10.3,0PE10.3,I5,A100)
      ELSE
      READ (plt,1001) XZU(I),YZU(I),SIZU(I),ANZU(I),NCHZU(I),LABZU(I)
      IF (PRINP) WRITE (6,1001) XZU(I),YZU(I),SIZU(I),ANZU(I), &
      NCHZU(I),LABZU(I)
 1001 FORMAT (2(0PE10.3),0PE10.3,0PE10.3,I5,A100)
      END IF
!-----
      IF (XZU(I).EQ.0.0D0.AND.YZU(I).EQ.0.0D0.AND.LABZU(I).EQ.'97') &
       THEN
      PRT97=.TRUE.
      I=I-1
      GOTO 505
      END IF
!-----
      IF (XZU(I).EQ.0.0D0.AND.YZU(I).EQ.0.0D0.AND.LABZU(I).EQ.'98') &
       THEN
      PRT98=.TRUE.
      I=I-1
      END IF
!-----
  505 CONTINUE
!      NZU=NZU+NUMLBL
      NZU=I
      GOTO 2222
      END IF
      NRE=NRE+1
      LASTRE=LAST(NL)
!----- READ THE POINTS AND ADD MORE IF NECESSARY
      IF (LONGCOD) THEN
      READ (plt,1008) (X(I),Y(I),IP(I),I=1,NPOINT)
      IF (PRINP) WRITE (6,1008) (X(I),Y(I),IP(I),I=1,NPOINT)
 1008 FORMAT (7(2(0PE20.12),I2))
      ELSE
      READ (plt,1006) (X(I),Y(I),IP(I),I=1,NPOINT)
 1006 FORMAT (7(2D10.0,I2))
      IF (PRINP) WRITE (6,1007) (X(I),Y(I),IP(I),I=1,NPOINT)
 1007 FORMAT (7(2(1PE10.3),I2))
      END IF
!-----
      IF (NPOINT.EQ.1) THEN
      NPOINT=2
      X(2)=X(1)
      Y(2)=Y(1)+0.001D0/YFAC
      IP(2)=2
      END IF
      DO 504,I1=1,NPOINT
      X(I1)=(X(I1)-XMIN)*XFAC
      Y(I1)=(Y(I1)-YMIN)*YFAC
!     IF (IP(I1).EQ.3) THEN
      IF (IP(I1).EQ.3.OR.X(I1).LE.0.0D0.OR.X(I1).GE.BREITE &
      .OR.Y(I1).LE.0.0D0.OR.Y(I1).GE.HOEHE) THEN
      CALL ADDPT(X(I1),Y(I1),IP(I1),ZERO)
      ELSE
      DIST=DSQRT((X(I1)-XX(NP))**2+(Y(I1)-YY(NP))**2)
      IF (DIST.GT.DMIN/2.0D0) THEN
      FF=DINT(DIST*2.0D0/DMIN)+1.0D0
      DIST=DIST/FF
      N1=NP
!--
!-F90      DO 506,F1=1.0D0,FF
      F1=1.0D0
      DO WHILE(F1.LE.FF)
      X1=((FF-F1)*XX(N1)+F1*X(I1))/FF
      Y1=((FF-F1)*YY(N1)+F1*Y(I1))/FF
      CALL ADDPT(X1,Y1,IP(I1),DIST)
      F1=F1+1.0D0
      END DO
!-F90
      ELSE
      IF (DIST.GT.0.0D0) CALL ADDPT(X(I1),Y(I1),IP(I1),DIST)
      END IF
      END IF
      ORIG(NP)=.TRUE.
  504 CONTINUE
!----- READ THE LABELS AND TRANSFORM XPOS AND YPOS TO BE NEAR THE
!----- CENTRE, BUT STILL OFF THE CURVE.
      IF (NUMLBL.LE.0) NUMLBL=2*NUMLBL+2
      DO 508,I1=1,NUMLBL,2
      N1=NL-NUMLBL/2+(I1-1)/2+1
      DO 510,I=1,2
!-----
      IF (LONGCOD) THEN
!      READ (13,1011) XPOS(N1,I),YPOS(N1,I),SIZLAB(N1,I),
      READ (plt,1011) XPOS(N1,I),YPOS(N1,I),SIZLAB(N1,I), &
      ANGE(N1,I),NCHAR(N1,I),LABEL(N1,I)
      IF (PRINP) WRITE (6,1011) XPOS(N1,I),YPOS(N1,I),SIZLAB(N1,I), &
      ANGE(N1,I),NCHAR(N1,I),LABEL(N1,I)
 1011 FORMAT (2(0PE20.12),0PE10.3,0PE10.3,I5,A100)
      ELSE
!      READ (13,1010) XPOS(N1,I),YPOS(N1,I),SIZLAB(N1,I),
      READ (plt,1010) XPOS(N1,I),YPOS(N1,I),SIZLAB(N1,I), &
      ANGE(N1,I),NCHAR(N1,I),LABEL(N1,I)
      IF (PRINP) WRITE (6,1010) XPOS(N1,I),YPOS(N1,I),SIZLAB(N1,I), &
      ANGE(N1,I),NCHAR(N1,I),LABEL(N1,I)
 1010 FORMAT (2(0PE10.3),0PE10.3,0PE10.3,I5,A100)
      END IF
!-----
      NCHAR(N1,I)=INDEX(LABEL(N1,I),'   ')-1
!apr2010 Achtung: NCHAR wird im SIZECM auf minimum 1 gesetzt
      XPOS(N1,I)=(XPOS(N1,I)-XMIN)*XFAC
      YPOS(N1,I)=(YPOS(N1,I)-YMIN)*YFAC
  510 CONTINUE
      ANGE(N1,1)=DMOD(ANGE(N1,1),360.0D0)
      IF (ANGE(N1,1).GT.180.0D0) ANGE(N1,1)=ANGE(N1,1)-360.0D0
      IF (DABS(ANGE(N1,1)).EQ.90.0D0) THEN
      ANGE(N1,1)=ANGE(N1,1)/DEGRAD
      ELSE
      ANGE(N1,1)=DATAN(DTAN(ANGE(N1,1)/DEGRAD)*YFAC/XFAC)
      END IF
!cdc      SIZLAB(N1,1)=SIZLAB(N1,1)*SFAC
!cdc      CALL SIZECM(LABEL(N1,1),NCHAR(N1,1),SIZLAB(N1,1),FONT,SIZE)
      SIZLAB(N1,1)=0.0D0
      SIZE=0.0D0
      ANGE(N1,1)=0.0D0
      ANGE(N1,2)=0.0D0
!cdc
      XKOR=SIZE/2.0D0
      YKOR=SIZLAB(N1,1)/2.0D0
      F1=DCOS(ANGE(N1,1))
      F2=DSIN(ANGE(N1,1))
      XPOS(N1,1)=XPOS(N1,1)+F1*XKOR-F2*YKOR
      YPOS(N1,1)=YPOS(N1,1)+F2*XKOR+F1*YKOR
      LINKS(N1)=1
      IF (DABS(ANGE(N1,1)).EQ.90.0) THEN
      IF (XPOS(N1,1).GT.XPOS(N1,2)) LINKS(N1)=2
      ELSE
      FF=YPOS(N1,2)-DTAN(ANGE(N1,1))*(XPOS(N1,2)-XPOS(N1,1))
      IF (FF.GT.YPOS(N1,1)) LINKS(N1)=2
      END IF
  508 CONTINUE
!----- PRINT REACTION
      ARCH=LABEL(NL,LINKS(NL))
      I1=INDEX(ARCH,'   ')
      IF (I1.EQ.0) I1=51
      I2=3-LINKS(NL)
      ARCH(I1+1:I1+1)='='
      ARCH(I1+3:)=LABEL(NL,I2)
      CALL LABLA(ARCH,I0)
      WRITE (6,FMT='(1X,I3,''):'',2X,A)') REANR(NL),ARCH(1:I0)
      WRITE (rxn,FMT='(1X,I3,''):'',2X,A)') REANR(NL),ARCH(1:I0)
!----- UPDATE DANGER
      LOC=1
      CALL OCCUPY(LOC,IXX(NP),IYY(NP))
      DO I1=1,LASTRE
        DANGER(I1)=MAX0(DANGER(I1),GITTER(1,IXX(I1),IYY(I1)))
      END DO
!----- UPDATE GITTER(0,..) AND RESET GITTER(1,..)=0
      I1=LASTRE+1
      LOC=0
      CALL OCCUPY(LOC,IXX(I1),IYY(I1))
      DO 514,I1=LASTRE+2,NP-1
      IF (IXX(I1).NE.IXX(I1-1).OR.IYY(I1).NE.IYY(I1-1)) THEN
      CALL CROSS(LOC,IXX(I1),IYY(I1))
      END IF
  514 CONTINUE
      CALL OCCUPY(LOC,IXX(NP),IYY(NP))
!-----
      GOTO 2222
!-----
 9999 CONTINUE
      WRITE (UNIT=6,FMT='(/,1X,132A1)') ('-',I=1,80)
!=====
!-----Check if command line arguments are present
   40 CONTINUE
      if(larg.eq.0) then
!----- for simple guzzler don't read, but set ARCH='0'
      ARCH='0'
!        call helpme ('$GUZ-LABELS',tpath,itpath)
!        WRITE (6,1080)
! 1080   format(' Enter [ "?" | CR | "0" | r1, r2, ... ]')
!        READ (5,FMT='(A100)') ARCH
      else
        ARCH=' '
!following added sept2009
        CALL GELI(CHIN(3),FF)
        NGLOB=IDINT(FF)
        NFORCE=0
        GOTO 42
      end if
!-----
      if(ARCH.eq.'?') then
        call helpme ('$GUZ-LABEL',tpath,itpath)
        goto 40
      end if
!*****
      NFORCE=0
      I2=0
   50 DO I1=I2+1,100
        IF (ARCH(I1:I1).NE.' '.AND.ARCH(I1:I1).NE.',') GOTO 51
      END DO
   51 IF (I1.GT.100) GOTO 53
      DO I2=I1+1,100
        IF (ARCH(I2:I2).EQ.' '.OR.ARCH(I2:I2).EQ.',') GOTO 52
      END DO
   52 NFORCE=NFORCE+1
      HILCH=' '
      HILCH(6+I1-I2:5)=ARCH(I1:I2-1)
      READ (UNIT=HILCH,FMT='(I5)') IFORCE(NFORCE)
      GOTO 50
   53 CONTINUE
      NGLOB=-3
!     IF (NFORCE.EQ.1) THEN
      IF (NFORCE.GT.0) THEN
      IF (IFORCE(1).EQ.0) THEN
   41 CONTINUE
      CALL helpme ('$GUZ-GLOBS',tpath,itpath)
      WRITE (scr,1082) NGLOB, MINILAE
 1082 FORMAT(/,' Enter [ "?" | CR | option  (min_length)] <', &
      I2,2X,F5.3,'>? ')
      READ (5,FMT='(A100)') ARCH
      IF(ARCH.EQ.'?') THEN
         CALL helpme ('$GUZ-GLOB',tpath,itpath)
         GOTO 41
      END IF
      IF(ARCH.EQ.' ') GOTO 42
      CALL GELI(ARCH,FF)
      NGLOB=IDINT(FF)
      IF (ARCH.NE.' ') CALL GELI(ARCH,MINILAE)
      END IF
      END IF
   42 CONTINUE
!
      DO 520,II=NYGRID,-1,-1
      WRITE (grd,1090) (GITTER(0,I,II),I=-1,NXGRID)
 1090 FORMAT(1X,102I1)
  520 CONTINUE
!----- GO THROUGH ALL LINES AND FIND APPROPRIATE LABEL
!-----
      DO 550,LL=1,NL
      CODE(LL)=0
      IF (NCHAR(LL,1).EQ.0.AND.NCHAR(LL,2).EQ.0) GOTO 800
!apr2010 Achtung: NCHAR wird im SIZECM auf minimum 1 gesetzt
      CALL SIZECM(LABEL(LL,1),NCHAR(LL,1),GRS,FONT,F1)
      CALL SIZECM(LABEL(LL,2),NCHAR(LL,2),GRS,FONT,F2)
      FF=DMAX1(F1,F2)
      NFCODE=NGLOB
      DO 552,I=1,NFORCE
      IF (IFORCE(I).EQ.REANR(LL)) THEN
      WRITE (6,1091) REANR(LL),LAE(LAST(LL)),FF
 1091 FORMAT (/,' REACTION:',I3,'   length of line =',F9.4, &
      '   length of label =',F9.4/' choose one option :'/ &
      ' 3 = text-label,  2 = in-line number,  1 = offset number, ', &
      '0 = no label')
      READ (5,*) NFCODE
      GOTO 100
      END IF
  552 CONTINUE
  100 IF (NFCODE.EQ.0) GOTO 800
      IF (MINILAE.GT.LAE(LAST(LL))) THEN
      NFCODE=0
      GOTO 800
      END IF
      IF (IABS(NFCODE).EQ.1.OR.IABS(NFCODE).EQ.2) GOTO 802
      IF (NCHAR(LL,2).EQ.0) GOTO 802
!apr2010 Achtung: NCHAR wird im SIZECM auf minimum 1 gesetzt
      IF (LABEL(LL,2).EQ.' ') GOTO 802
!
!----- TRY LABELS
!
      NAIMAX=1
      IF (NFCODE.EQ.3) NAIMAX=MATE+1
      DO NAIM=1,NAIMAX
        CALL SEARCH(LL,NAIM,I1,I2,XL)
        IF (XL.GE.FF) GOTO 102
      END DO
      IF (NFCODE.EQ.3) THEN
      I1=FIRST(LL)
      I2=LAST(LL)
      FF=0.0D0
      XL=LAE(I2)
      END IF
  102 IF (XL.GE.FF) THEN
      CENTRE(LL)=LAE(I1)+XL/2.0D0
      CALL POSIT(I1,I2,CENTRE(LL),X1,Y1,ANGE(LL,1))
      CODE(LL)=3
!----- FIND A POINT CLOSE TO XPOS/YPOS
!----- AND CALCULATE SIGL
      I0=FIRST(LL)
      CALL DISTAN(XPOS(LL,1),YPOS(LL,1),XX(I0),YY(I0),D0)
      DO I=FIRST(LL)+1,LAST(LL)
        CALL DISTAN(XPOS(LL,1),YPOS(LL,1),XX(I),YY(I),DIST)
        IF (DIST.LT.D0) THEN
        I0=I
        D0=DIST
        END IF
      END DO
      IF (I0.EQ.LAST(LL)) I0=I0-1
      CALL FLACH(XX(I0),YY(I0),XX(I0+1),YY(I0+1), &
      XPOS(LL,1),YPOS(LL,1),FF)
      SIGL(LL,1)=DSIGN(1.0D0,FF)
      SIGL(LL,2)=-SIGL(LL,1)
      END IF
!
!----- TRY NUMBERS
!
  802 IF (CODE(LL).EQ.0) THEN
      IF (NFCODE.EQ.4) NFCODE=2
!apr2010
!      IF (NCHAR(LL,2).EQ.0) THEN
      IF (LABEL(LL,2).EQ.' ') THEN
      REACH(LL)=LABEL(LL,1)
      NCHN(LL)=NCHAR(LL,1)
      ELSE
      F1=DLOG10(DBLE(REANR(LL)))+1.0D0
      NCHN(LL)=IDINT(F1)
      WRITE (UNIT=HILCH,FMT='(I5)') REANR(LL)
      DO I=1,5
        I001=I
        IF (HILCH(I:I).NE.' ') GOTO 104
      END DO
  104 REACH(LL)=HILCH(I001:)
      END IF
      IF (IABS(NFCODE).EQ.1) GOTO 801
!-----
      CALL SIZECM(REACH(LL),NCHN(LL),GRZ,FONT,F1)
      NAIMAX=NAIMNR
      IF (NFCODE.EQ.2) NAIMAX=MATE+1
      DO NAIM=NAIMNR,NAIMAX
        CALL SEARCH(LL,NAIM,I1,I2,XL)
        IF (XL.GE.F1) GOTO 106
      END DO
      IF (NFCODE.EQ.2) THEN
      I1=FIRST(LL)
      I2=LAST(LL)
      XL=LAE(I2)
      END IF
  106 IF (XL.GE.F1.OR.NFCODE.EQ.2) THEN
      CODE(LL)=2
      FF=LAE(I1)+XL/2.0D0
      FF1=FF-(F1+GRZ/2.0D0)/2.0D0
      FF2=FF+(F1+GRZ/2.0D0)/2.0D0
      CALL POSIT(I1,I2,FF,X1,Y1,ANGE(LL,1))
      CALL POSIT(I1,I2,FF1,X11,Y11,ANGE(LL,2))
      CALL POSIT(I1,I2,FF2,X12,Y12,ANGE(LL,2))
      DO I001=I1+1,I2
        IF (LAE(I001).GT.FF1) GOTO 108
      END DO
  108 XX(I001-1)=X11
      YY(I001-1)=Y11
      ORIG(I001-1)=.TRUE.
      DO I002=I001,I2
        IPEN(I002)=3
        IF (LAE(I002).GT.FF2) GOTO 110
      END DO
  110 CONTINUE
      IF (I002.GT.I2) I002=I2
      XX(I002)=X12
      YY(I002)=Y12
      IPEN(I002)=3
      ORIG(I002)=.TRUE.
      IF (ANGE(LL,1).GT.PI2) ANGE(LL,1)=ANGE(LL,1)-PI
      IF (ANGE(LL,1).LT.-PI2) ANGE(LL,1)=ANGE(LL,1)+PI
      XKOR=-F1/2.0D0
      YKOR=-GRZ/2.0D0
      F1=DCOS(ANGE(LL,1))
      F2=DSIN(ANGE(LL,1))
      XPOS(LL,1)=X1+F1*XKOR-F2*YKOR
      YPOS(LL,1)=Y1+F2*XKOR+F1*YKOR
      END IF
      END IF
!
!----- TRY OFFSET NUMBERS
!
  801 IF (CODE(LL).EQ.0) THEN
      NAIMAX=NAIMNR+1
      IF (NFCODE.EQ.5) GOTO 800
      IF (NFCODE.EQ.1) NAIMAX=1
      DO NAIM=NAIMAX,MATE+1
        CALL SEARCH(LL,NAIM,I1,I2,XL)
        IF (XL.GE.DMIN) GOTO 112
      END DO
  112 IF (XL.LT.DMIN) THEN
      F1=LAE(LAST(LL))/2.0D0
      ELSE
      F1=LAE(I1)+XL/2.0D0
      END IF
      CALL POSIT(FIRST(LL),LAST(LL),F1,X1,Y1,ANGE(LL,1))
      IF (ANGE(LL,1).GT.PI2) ANGE(LL,1)=ANGE(LL,1)-PI
      IF (ANGE(LL,1).LT.-PI2) ANGE(LL,1)=ANGE(LL,1)+PI
      IF (NFCODE.EQ.1) THEN
      WRITE (6,1093) REANR(LL),(X1/XFAC)+XMIN,(Y1/YFAC)+YMIN
 1093 FORMAT (' REACTION:',I3,'   POSITION ON CURVE:',2F12.4,/ &
      ' WHERE DO YOU WANT THE LABEL ?. ENTER X Y')
      READ (5,*) XPOS(LL,1),YPOS(LL,1)
      I1=IDINT((XPOS(LL,1)-XMIN)*XFAC/DELX)
      I2=IDINT((YPOS(LL,1)-YMIN)*YFAC/DELY)
      ELSE
      CALL STAR(X1,Y1,ANGE(LL,1),I1,I2)
      END IF
      IF (I1.EQ.0.AND.I2.EQ.0) THEN
      CODE(LL)=0
      ELSE
      CODE(LL)=1
      LOC=0
      CALL OCCUPY(LOC,I1,I2)
      XPOS(LL,1)=(DBLE(I1)+0.5D0)*DELX
      YPOS(LL,1)=(DBLE(I2)+0.5D0)*DELY
      XPOS(LL,2)=X1
      YPOS(LL,2)=Y1
      ANGE(LL,1)=0.0D0
      IF (XPOS(LL,1).EQ.X1) THEN
      ANGE(LL,2)=PI2
      ELSE
      ANGE(LL,2)=DATAN((YPOS(LL,1)-Y1)/(XPOS(LL,1)-X1))
      END IF
      END IF
      END IF
!-----
  800 IF (CODE(LL).EQ.0) THEN
      WRITE (6,FMT='(1X,I3,''): NO LABEL'')') REANR(LL)
      END IF
  550 CONTINUE
!-----
!----- PRINT NEW IMPROVED INPUT FOR NPLOIG
!-----
      WRITE (cln,1995)
 1995 FORMAT ('NULLPT    5   3'/'FONT      Helvetica ')
      IF (PRT97) THEN
      WRITE (cln,1999) BREITE+1.0D0
 1999 FORMAT ( &
      'FAT   0.02'/'PUNKTE   97  0.5   ',F7.4,'  0.25  999  999 ')
      END IF
      IF (PRT98) THEN
      WRITE (cln,1996) BREITE+1.0D0
 1996 FORMAT ( &
      'FAT   0.02'/'PUNKTE   98  0.5   ',F7.4,'  0.25  999  999 ')
      END IF
      IF (LONGCOD) THEN
      WRITE (cln,1997)
 1997 FORMAT ('FAT   0.03'/'NPLOG2  ')
      ELSE
      WRITE (cln,1998)
 1998 FORMAT ('FAT   0.03'/'NPLOIG  ')
      END IF
      WRITE(cln,2000) XTEXT,YTEXT
 2000 FORMAT (A80/A80)
      IF (LONGCOD) THEN
      FORM='(4(1PE20.12),2(0PF10.4),2I3)'
      ELSE
      FORM='(2'//FORX//',2'//FORY//',2F10.4,2I3)'
      END IF
      CALL LABLA(FORM,ILF)
      WRITE (cln,FMT=FORM(1:ILF)) XMIN,XMAX,YMIN,YMAX,BREITE,HOEHE, &
      ISPEED,IAXCOL
      IF (NZU.GT.0) THEN
      NPOINT=0
      WRITE (cln,2001) NZU,NPOINT,NPL,ISYMB,ICOLPT,ICOLLB
 2001 FORMAT (6I5)
!-----
      IF (LONGCOD) THEN
      FORM='(2(1PE20.12),0PF10.7,F10.4,I5,A100)'
      ELSE
      FORM='('//FORXY//',0PF10.7,F10.4,I5,A100)'
      END IF
      CALL LABLA(FORM,ILF)
!-----
      DO 590,I=1,NZU
!+++++
!----- recalculate new xy for labels
!+++++
      XCM=(XZU(I)-OLDL)*OLDXF
      YCM=(YZU(I)-OLDU)*OLDYF
      XCM=XCM/OLDB*BREITE
      YCM=YCM/OLDH*HOEHE
      XUS=XCM/XFAC+XMIN
      YUS=YCM/YFAC+YMIN
      WRITE (cln,FMT=FORM(1:ILF)) XUS,YUS,SIZU(I),ANZU(I), &
      NCHZU(I),LABZU(I)
!      WRITE (cln,FMT=FORM(1:ILF)) XZU(I),YZU(I),SIZU(I),ANZU(I), &
!      NCHZU(I),LABZU(I)
  590 CONTINUE
      END IF
      DO 600,LL=1,NL
!-----
      IF (CODE(LL).EQ.3) THEN
      IF (DABS(ANGE(LL,1)).EQ.PI2) THEN
      FF1=1.0D0
      ELSE
      FF1=DSIGN(1.0D0,ANGE(LL,1))*DSIGN(1.0D0,DTAN(ANGE(LL,1)))
      END IF
      THKOR=0.0D0
      IF (ANGE(LL,1).GT.PI2) THKOR=-PI
      IF (ANGE(LL,1).LT.-PI2) THKOR=PI
      DO 602,I=1,2
      NBLA=0
      DO I1=1,NCHAR(LL,I)+1
        IF (LABEL(LL,I)(I1:I1).EQ.' ') THEN
        NBLA=NBLA+1
        BLANK(NBLA)=I1
        END IF
      END DO
      NPOINT=0
      NUMLBL=NBLA
!+++++if each character a label
!     NUMLBL=NCHAR(LL,I)
!+++++
      WRITE (cln,2007) NUMLBL,NPOINT,NPL,ISYMB,ICOLPT,ICOLLB
 2007 FORMAT (6I5)
!-----
      IF (SIGL(LL,I)*FF1.LE.0.0D0) THEN
      YKOR=-(1.0D0+XOFF)*GRS
      ADDFAC=-SIGL(LL,I)
      ELSE
      YKOR=XOFF*GRS
      ADDFAC=SIGL(LL,I)
      END IF
      CALL SIZECM(LABEL(LL,I),NCHAR(LL,I),GRS,FONT,SIZE)
      FF=CENTRE(LL)-ADDFAC*SIZE/2.0D0
      I001=1
      DO 606,I1=1,NUMLBL
      I002=BLANK(I1)
      ARCH=LABEL(LL,I)(I001:I002)
      ICH=I002-I001+1
!+++++if each character a label
!     ARCH=LABEL(LL,I)(I1:I1)
!     ICH=1
!+++++
      CALL SIZECM(ARCH,ICH,GRS,FONT,SIZE)
      FF=FF+ADDFAC*SIZE/2.0D0
      XKOR=-SIZE/2.0D0
      CALL POSIT(FIRST(LL),LAST(LL),FF,X1,Y1,THE)
      THE=THE+THKOR
      IF (THE.GT.PI) THE=THE-PI*2.0D0
      IF (THE.LT.-PI) THE=THE+PI*2.0D0
      F1=DCOS(THE)
      F2=DSIN(THE)
      X1=((X1+F1*XKOR-F2*YKOR)/XFAC)+XMIN
      Y1=((Y1+F2*XKOR+F1*YKOR)/YFAC)+YMIN
      IF (DABS(THE).EQ.PI2) THEN
      THEP=THE*DEGRAD
      ELSE
      THEP=DATAN(DTAN(THE)*XFAC/YFAC)*DEGRAD
      END IF
      IF (THE.GT.PI2.AND.THEP.LT.0.0D0) THEP=THEP+180.0D0
      IF (THE.LT.-PI2.AND.THEP.GT.0.0D0) THEP=THEP-180.0D0
      SIZLAB(LL,I)=GRS/SFAC
      IF (LONGCOD) THEN
      FORM='(2(1PE20.12),0PF10.7,F10.4,I5,A100)'
      ELSE
      FORM='('//FORXY//',0PF10.7,F10.4,I5,A100)'
      END IF
      CALL LABLA(FORM,ILF)
      WRITE (cln,FMT=FORM(1:ILF)) X1,Y1,SIZLAB(LL,I),THEP,ICH-1,ARCH
      I001=I002+1
      FF=FF+ADDFAC*SIZE/2.0D0
  606 CONTINUE
  602 CONTINUE
      END IF
!-----
      IF (CODE(LL).EQ.2) THEN
      NPOINT=0
      NUMLBL=1
      WRITE (cln,2008) NUMLBL,NPOINT,NPL,ISYMB,ICOLPT,ICOLLB
 2008 FORMAT (6I5)
      X1=(XPOS(LL,1)/XFAC)+XMIN
      Y1=(YPOS(LL,1)/YFAC)+YMIN
      IF (DABS(ANGE(LL,1)).EQ.PI2) THEN
      THE=ANGE(LL,1)*DEGRAD
      ELSE
      THE=DATAN(DTAN(ANGE(LL,1))*XFAC/YFAC)*DEGRAD
      END IF
      SIZLAB(LL,1)=GRZ/SFAC
      IF (LONGCOD) THEN
      FORM='(2(1PE20.12),0PF10.7,F10.4,I5,A100)'
      ELSE
      FORM='('//FORXY//',0PF10.7,F10.4,I5,A100)'
      END IF
      CALL LABLA(FORM,ILF)
      WRITE (cln,FMT=FORM(1:ILF)) X1,Y1,SIZLAB(LL,1),THE,NCHN(LL), &
      REACH(LL)
      END IF
!-----
      IF (CODE(LL).EQ.1) THEN
      CALL SIZECM(REACH(LL),NCHN(LL),GRZ,FONT,SIZE)
      IF (ANGE(LL,2).EQ.0.0D0) THEN
      F1=1D50
      ELSE
      F1=GRZ*1.5D0/(2.0D0*DSIN(ANGE(LL,2)))
      END IF
      IF (DABS(ANGE(LL,2)).EQ.PI2) THEN
      F2=1D50
      ELSE
      F2=(SIZE+GRZ*0.5D0)/(2.0D0*DCOS(ANGE(LL,2)))
      END IF
      FF=DMIN1(DABS(F1),DABS(F2))
      CALL DISTAN(XPOS(LL,1),YPOS(LL,1),XPOS(LL,2),YPOS(LL,2),DIST)
      X1=(XPOS(LL,1)-XPOS(LL,2))*(DIST-FF)/DIST+XPOS(LL,2)
      Y1=(YPOS(LL,1)-YPOS(LL,2))*(DIST-FF)/DIST+YPOS(LL,2)
      X2=(XPOS(LL,2)-XPOS(LL,1))*(DIST-GAP)/DIST+XPOS(LL,1)
      Y2=(YPOS(LL,2)-YPOS(LL,1))*(DIST-GAP)/DIST+YPOS(LL,1)
      XPOS(LL,1)=XPOS(LL,1)-SIZE/2.0D0
      YPOS(LL,1)=YPOS(LL,1)-GRZ/2.0D0
      X1=(X1/XFAC)+XMIN
      Y1=(Y1/YFAC)+YMIN
      X2=(X2/XFAC)+XMIN
      Y2=(Y2/YFAC)+YMIN
      XPOS(LL,1)=(XPOS(LL,1)/XFAC)+XMIN
      YPOS(LL,1)=(YPOS(LL,1)/YFAC)+YMIN
      ICOLPT=3
      NUMLBL=1
      NPOINT=2
      WRITE (cln,2015) NUMLBL,NPOINT,NPL,ISYMB,ICOLPT,ICOLLB
 2015 FORMAT (6I5)
      IF (LONGCOD) THEN
      FORM='(2(1PE20.12),'' 3'',2(1PE20.12),'' 2'')'
      ELSE
      FORM='('//FORXY//','' 3'','//FORXY//','' 2'')'
      END IF
      CALL LABLA(FORM,ILF)
      WRITE (cln,FMT=FORM(1:ILF)) X1,Y1,X2,Y2
      SIZLAB(LL,1)=GRZ/SFAC
      IF (LONGCOD) THEN
      FORM='(2(1PE20.12),0PF10.7,F10.4,I5,A100)'
      ELSE
      FORM='('//FORXY//',0PF10.7,F10.4,I5,A100)'
      END IF
      CALL LABLA(FORM,ILF)
      WRITE (cln,FMT=FORM(1:ILF)) XPOS(LL,1),YPOS(LL,1),SIZLAB(LL,1), &
      ANGE(LL,1),NCHN(LL),REACH(LL)
      END IF
!-----
      NPOINT=0
      DO 608,I=FIRST(LL),LAST(LL)
        IF (ORIG(I)) THEN
          NPOINT=NPOINT+1
          X(NPOINT)=(XX(I)/XFAC)+XMIN
          Y(NPOINT)=(YY(I)/YFAC)+YMIN
          IP(NPOINT)=IPEN(I)
        END IF
  608 CONTINUE
      ICOLPT=0
      NUMLBL=-1
      WRITE (cln,2005) NUMLBL,NPOINT,NPL,ISYMB,ICOLPT,ICOLLB
 2005 FORMAT (6I5)
      IF (LONGCOD) THEN
      FORM='(7(2(1PE20.12),I2))'
      ELSE
      FORM='(7('//FORXY//',I2))'
      END IF
      CALL LABLA(FORM,ILF)
      WRITE (cln,FMT=FORM(1:ILF)) (X(I),Y(I),IP(I),I=1,NPOINT)
!-----
  600 CONTINUE
!-----
      WRITE (scr,150)
  150 FORMAT (/,'exit GUZZLER')
      END
!
!***********************************************************
      SUBROUTINE ADDPT(X,Y,IP,DIST)
      IMPLICIT NONE
      INTEGER(4) MAXPT,MAXGRD,MAXLIN,MATE
      PARAMETER (MAXPT=50000,MAXGRD=100,MAXLIN=1000,MATE=2)
!-----
      INTEGER(4) GITTER(0:1,-1:MAXGRD,-1:MAXGRD), &
      TEMPLE(-MATE:MATE,-MATE:MATE),NXGRID,NYGRID
      COMMON /GITT/ GITTER,TEMPLE,NXGRID,NYGRID
!-----
      REAL(8) XX(MAXPT),YY(MAXPT),LAE(MAXPT)
      INTEGER(4) IPEN(MAXPT),IXX(MAXPT),IYY(MAXPT),DANGER(MAXPT)
      LOGICAL(4) ORIG(MAXPT)
      COMMON /PTRE/ XX,YY,LAE
      COMMON /PTIN/ IPEN,IXX,IYY,DANGER
      COMMON /PTLO/ ORIG
!-----
      REAL(8) XPOS(MAXLIN,2),YPOS(MAXLIN,2),ANGE(MAXLIN,2), &
      SIZLAB(MAXPT,2),SIGL(MAXLIN,2),CENTRE(MAXLIN)
      INTEGER(4) CODE(MAXLIN),FIRST(MAXLIN),LAST(0:MAXLIN), &
      NCHAR(MAXLIN,2),REANR(0:MAXLIN),NCHN(MAXLIN),LINKS(MAXLIN)
      CHARACTER(100) LABEL(MAXLIN,2),REACH(MAXLIN)
      COMMON /LIRE/ XPOS,YPOS,ANGE,SIZLAB,SIGL,CENTRE
      COMMON /LIIN/ CODE,FIRST,LAST,NCHAR,REANR,NCHN,LINKS
      COMMON /LICH/ LABEL,REACH
!-----
      INTEGER(4) NRE,NL,NP
      REAL(8) GRS,GRZ,DEGRAD,DMIN,DELX,DELY,XFAC,YFAC,SFAC
      COMMON /COUNT/ NRE,NL,NP
      COMMON /USEF/ GRS,GRZ,DEGRAD,DMIN,DELX,DELY,XFAC,YFAC,SFAC
!-----
      REAL(8) X,Y,DIST
      INTEGER(4) IP,IX,IY,LOC,I1
!-----
      NP=NP+1
      ORIG(NP)=.FALSE.
      IF (NP.GT.MAXPT) THEN
      WRITE (6,1000) MAXPT
 1000 FORMAT (' MAXIMUM NUMBER OF POINTS EXEEDED:',I5)
      STOP
      END IF
      XX(NP)=X
      YY(NP)=Y
      IPEN(NP)=IP
      IF (IP.EQ.3) THEN
      LAE(NP)=0.0D0
      LAST(NL)=NP-1
      NL=NL+1
      IF (NL.GT.MAXLIN) THEN
      WRITE (6,1001) MAXLIN
 1001 FORMAT (' MAXIMUM NUMBER OF LINES EXEEDED:',I5)
      STOP
      END IF
      FIRST(NL)=NP
      REANR(NL)=NRE
      ELSE
      LAE(NP)=LAE(NP-1)+DIST
      END IF
      LAST(NL)=NP
      IX=IDINT(X/DELX)
      IY=IDINT(Y/DELY)
      IF (IX.LT.-1) IX=-1
      IF (IX.GT.NXGRID) IX=NXGRID
      IF (IY.LT.-1) IY=-1
      IF (IY.GT.NYGRID) IY=NYGRID
      IXX(NP)=IX
      IYY(NP)=IY
      DANGER(NP)=GITTER(0,IX,IY)
      LOC=1
      I1=NP-1
      IF (I1.EQ.0) I1=1
      IF (NP.EQ.FIRST(NL)) THEN
      CALL OCCUPY(LOC,IX,IY)
      ELSE
      IF (IX.NE.IXX(I1).OR.IY.NE.IYY(I1)) &
        CALL CROSS(LOC,IX,IY)
      END IF
      RETURN
      END
!
!***********************************************************
      SUBROUTINE OCCUPY(LOC,IX,IY)
      IMPLICIT NONE
      INTEGER(4) MAXGRD,MATE
      PARAMETER (MAXGRD=100,MATE=2)
!-----
      INTEGER(4) GITTER(0:1,-1:MAXGRD,-1:MAXGRD), &
      TEMPLE(-MATE:MATE,-MATE:MATE),NXGRID,NYGRID
      COMMON /GITT/ GITTER,TEMPLE,NXGRID,NYGRID
!-----
      INTEGER(4) NRE,NL,NP
      COMMON /COUNT/ NRE,NL,NP
!-----
      INTEGER(4) LOC,IX,IY,I001,I002,I003,I004,I,II,I1
!-----
      I001=MAX0(IY-MATE,0)
      I002=MIN0(IY+MATE,NYGRID-1)
      I003=MAX0(IX-MATE,0)
      I004=MIN0(IX+MATE,NXGRID-1)
      DO II=I001,I002
      DO I=I003,I004
        IF (LOC.EQ.0) GITTER(1,I,II)=0
        I1=MAX0(GITTER(LOC,I,II),TEMPLE(I-IX,II-IY))
        GITTER(LOC,I,II)=I1
      END DO
      END DO
      RETURN
      END
!
!***********************************************************
      SUBROUTINE CROSS(LOC,IX,IY)
      IMPLICIT NONE
      INTEGER(4) MAXGRD,MATE
      PARAMETER (MAXGRD=100,MATE=2)
!-----
      INTEGER(4) GITTER(0:1,-1:MAXGRD,-1:MAXGRD), &
      TEMPLE(-MATE:MATE,-MATE:MATE),NXGRID,NYGRID
      COMMON /GITT/ GITTER,TEMPLE,NXGRID,NYGRID
!-----
      INTEGER(4) NRE,NL,NP
      COMMON /COUNT/ NRE,NL,NP
!-----
      INTEGER(4) LOC,IX,IY,I001,I002,I003,I004,II
!-----
      I001=MAX0(IY-MATE,0)
      I002=MIN0(IY+MATE,NYGRID-1)
      I003=MAX0(IX-MATE,0)
      I004=MIN0(IX+MATE,NXGRID-1)
      DO II=I001,I002
        IF (LOC.EQ.0) GITTER(1,IX,II)=0
        GITTER(LOC,IX,II)=MAX0(GITTER(LOC,IX,II),TEMPLE(0,II-IY))
      END DO
      DO II=I003,I004
        IF (LOC.EQ.0) GITTER(1,II,IY)=0
        GITTER(LOC,II,IY)=MAX0(GITTER(LOC,II,IY),TEMPLE(II-IX,0))
      END DO
      RETURN
      END
!
!***********************************************************
      SUBROUTINE SEARCH(L,N,IO1,IO2,OLD)
      IMPLICIT NONE
      INTEGER(4) MAXPT,MAXGRD,MAXLIN,MATE
      PARAMETER (MAXPT=50000,MAXGRD=100,MAXLIN=1000,MATE=2)
!-----
      INTEGER(4) GITTER(0:1,-1:MAXGRD,-1:MAXGRD), &
      TEMPLE(-MATE:MATE,-MATE:MATE),NXGRID,NYGRID
      COMMON /GITT/ GITTER,TEMPLE,NXGRID,NYGRID
!-----
      REAL(8) XX(MAXPT),YY(MAXPT),LAE(MAXPT)
      INTEGER(4) IPEN(MAXPT),IXX(MAXPT),IYY(MAXPT),DANGER(MAXPT)
      LOGICAL(4) ORIG(MAXPT)
      COMMON /PTRE/ XX,YY,LAE
      COMMON /PTIN/ IPEN,IXX,IYY,DANGER
      COMMON /PTLO/ ORIG
!-----
      REAL(8) XPOS(MAXLIN,2),YPOS(MAXLIN,2),ANGE(MAXLIN,2), &
      SIZLAB(MAXPT,2),SIGL(MAXLIN,2),CENTRE(MAXLIN)
      INTEGER(4) CODE(MAXLIN),FIRST(MAXLIN),LAST(0:MAXLIN), &
      NCHAR(MAXLIN,2),REANR(0:MAXLIN),NCHN(MAXLIN),LINKS(MAXLIN)
      CHARACTER(100) LABEL(MAXLIN,2),REACH(MAXLIN)
      COMMON /LIRE/ XPOS,YPOS,ANGE,SIZLAB,SIGL,CENTRE
      COMMON /LIIN/ CODE,FIRST,LAST,NCHAR,REANR,NCHN,LINKS
      COMMON /LICH/ LABEL,REACH
!-----
      INTEGER(4) NRE,NL,NP
      REAL(8) GRS,GRZ,DEGRAD,DMIN,DELX,DELY,XFAC,YFAC,SFAC
      COMMON /COUNT/ NRE,NL,NP
      COMMON /USEF/ GRS,GRZ,DEGRAD,DMIN,DELX,DELY,XFAC,YFAC,SFAC
!-----
      REAL(8) OLD,F1,F2,XEW
      INTEGER(4) L,N,IO1,IO2,I1,IN1,IN2
!-----
      IO1=FIRST(L)
      IO2=FIRST(L)
      OLD=0.0D0
      I1=FIRST(L)
  222 IF (I1.GT.LAST(L)) GOTO 999
      DO IN1=I1,LAST(L)
        IF (DANGER(IN1).LT.N) GOTO 333
      END DO
  333 IF (IN1.GT.LAST(L)) GOTO 999
      DO IN2=IN1+1,LAST(L)
        IF (DANGER(IN2).GE.N) GOTO 444
      END DO
  444 IN2=IN2-1
      IF (IN1.EQ.FIRST(L)) THEN
      F1=0.0D0
      ELSE
      F1=(LAE(IN1)+LAE(IN1-1))/2.0D0
      END IF
      IF (IN2.EQ.LAST(L)) THEN
      F2=LAE(IN2)
      ELSE
      F2=(LAE(IN2)+LAE(IN2+1))/2.0D0
      END IF
      XEW=F2-F1
      IF (XEW.GT.OLD) THEN
      IO1=IN1
      IO2=IN2
      OLD=XEW
      END IF
      I1=IN2+2
      GOTO 222
  999 RETURN
      END
!
!***********************************************************
      SUBROUTINE POSIT(I1,I2,F1,X1,Y1,THE)
      IMPLICIT NONE
      INTEGER(4) MAXPT
      PARAMETER (MAXPT=50000)
!-----
      REAL(8) XX(MAXPT),YY(MAXPT),LAE(MAXPT)
      INTEGER(4) IPEN(MAXPT),IXX(MAXPT),IYY(MAXPT),DANGER(MAXPT)
      LOGICAL(4) ORIG(MAXPT)
      COMMON /PTRE/ XX,YY,LAE
      COMMON /PTIN/ IPEN,IXX,IYY,DANGER
      COMMON /PTLO/ ORIG
!-----
      REAL(8) F1,X1,Y1,THE
      INTEGER(4) I1,I2,I
      DO I=I1+1,I2-1
        IF (LAE(I).GT.F1) GOTO 222
      END DO
  222 IF (LAE(I)-LAE(I-1).EQ.0.0D0) THEN
      X1=XX(I)
      Y1=YY(I)
      THE=0.0D0
      ELSE
      X1=((XX(I)-XX(I-1))*(F1-LAE(I-1)))/(LAE(I)-LAE(I-1)) &
      +XX(I-1)
      Y1=((YY(I)-YY(I-1))*(F1-LAE(I-1)))/(LAE(I)-LAE(I-1)) &
      +YY(I-1)
      THE=DATAN2((YY(I)-YY(I-1)),(XX(I)-XX(I-1)))
      END IF
      RETURN
      END
!
!***********************************************************
      SUBROUTINE STAR(X,Y,THE,I1,I2)
      IMPLICIT NONE
      INTEGER(4) MAXGRD,MATE
      PARAMETER (MAXGRD=100,MATE=2)
!-----
      INTEGER(4) GITTER(0:1,-1:MAXGRD,-1:MAXGRD), &
      TEMPLE(-MATE:MATE,-MATE:MATE),NXGRID,NYGRID
      COMMON /GITT/ GITTER,TEMPLE,NXGRID,NYGRID
!-----
      REAL(8) GRS,GRZ,DEGRAD,DMIN,DELX,DELY,XFAC,YFAC,SFAC
      COMMON /USEF/ GRS,GRZ,DEGRAD,DMIN,DELX,DELY,XFAC,YFAC,SFAC
!-----
      REAL(8) X,Y,THE,F1,F2,FF
      INTEGER(4) I1,I2,IX,IY,ID,I001,I002,I003,I004,I,II,IST
!-----
      I1=0
      I2=0
      IX=IDINT(X/DELX)
      IY=IDINT(Y/DELY)
      DO 502,ID=1,20
      I001=MAX0(IY-ID,0)
      I002=MIN0(IY+ID,NYGRID-1)
      I003=MAX0(IX-ID,0)
      I004=MIN0(IX+ID,NXGRID-1)
      DO 501,II=I001,I002
      IF (II.EQ.IY-ID.OR.II.EQ.IY+ID) THEN
      IST=1
      ELSE
      IST=MAX0(1,(I004-I003))
      END IF
      DO 500,I=I003,I004,IST
      IF (GITTER(0,I,II).LT.1) THEN
      F1=(DBLE(I)+0.5D0)*DELX
      F2=(DBLE(II)+0.5D0)*DELY
      IF ((F1-X).EQ.0.0D0) THEN
      FF=1.571D0
      ELSE
      FF=DATAN2((F2-Y),(F1-X))
      END IF
      IF (DABS(FF-THE).GE.0.349D0) THEN
      I1=I
      I2=II
      RETURN
      END IF
      END IF
  500 CONTINUE
  501 CONTINUE
  502 CONTINUE
      RETURN
      END
!
!***********************************************************
      SUBROUTINE DISTAN(X1,Y1,X2,Y2,DIST)
      IMPLICIT NONE
      REAL(8) X1,Y1,X2,Y2,DIST
      DIST=DSQRT((X2-X1)**2+(Y2-Y1)**2)
      RETURN
      END
!
!***********************************************************
      SUBROUTINE FLACH(X1,Y1,X2,Y2,X3,Y3,FF)
      IMPLICIT NONE
      REAL(8) X1,Y1,X2,Y2,X3,Y3,FF
      FF=X1*(Y2-Y3)+X2*(Y3-Y1)+X3*(Y1-Y2)
      RETURN
      END
!
!***********************************************************
!     SUBROUTINE SIZECM(CSTR,NCH,GR,SIZE)
!     IMPLICIT REAL(8)(A-H,O-Z)
!     CHARACTER*(*) CSTR
!     BH=0.857142857D0
!     CHEFF=DBLE(NCH)
!     DO 500,I=1,NCH
!     IF (CSTR(I:I).EQ.'.') CHEFF=CHEFF-0.5D0
!     IF (CSTR(I:I).EQ.'1') CHEFF=CHEFF-0.3333333333D0
! 500 CONTINUE
!     COR=0.285714286D0
!     IF (CSTR(NCH:NCH).EQ.' ') COR=0.0D0
!     SIZE=(CHEFF-COR)*GR*BH
!     RETURN
!     END
!
!***********************************************************
      SUBROUTINE SIZECM(CH,LL,GRS,FONT,SIZE)
      IMPLICIT NONE
      CHARACTER*(*) CH
      CHARACTER(1) A1
      INTEGER(4) LL,I1,CODE,I,FONT
      REAL(8) VOR(0:126),NACH(0:126),BREI(0:126)
      REAL(8) GRS,SIZE,WID,KOR
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
      5.56D0,8.89D0,6.67D0,2.22D0,3.33D0,3.33D0,3.89D0, &
      5.84D0,2.78D0,3.33D0, &
      2.78D0,2.78D0,5.56D0,5.56D0,5.56D0,5.56D0,5.56D0, &
      5.56D0,5.56D0,5.56D0, &
      5.56D0,5.56D0,2.78D0,2.78D0,5.84D0,5.84D0,5.84D0, &
      5.56D0,10.15D0,6.67D0, &
      6.67D0,7.22D0,7.22D0,6.67D0,6.11D0,7.78D0,7.22D0, &
      2.78D0,5.00D0,6.67D0, &
      5.56D0,8.33D0,7.22D0,7.78D0,6.67D0,7.78D0,7.22D0, &
      6.67D0,6.11D0,7.22D0, &
      6.67D0,9.44D0,6.67D0,6.67D0,6.11D0,2.78D0,2.78D0, &
      2.78D0,4.69D0,5.56D0, &
      2.22D0,5.56D0,5.56D0,5.00D0,5.56D0,5.56D0,2.78D0, &
      5.56D0,5.56D0,2.22D0, &
      2.22D0,5.00D0,2.22D0,8.33D0,5.56D0,5.56D0,5.56D0, &
      5.56D0,3.33D0,5.00D0, &
      2.78D0,5.56D0,5.00D0,7.22D0,5.00D0,5.00D0,5.00D0, &
      3.34D0,2.60D0,3.34D0,5.84D0/
      IF (LL.EQ.0) LL=1
      VOR(32)=0.6364D0
      BREI(32)=3.33D0
      IF (FONT.EQ.100) THEN
      CALL FANLEN(CH,LL,GRS,SIZE)
      RETURN
      END IF
      WID=0.0D0
      CODE=0
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
!***********************************************************
      SUBROUTINE FANLEN(CH,LL,GRS,SIZE)
      IMPLICIT NONE
      CHARACTER*(*) CH
      CHARACTER(1) A1
      INTEGER(4) LL,I1,BREI(0:127),KOR,CODE,WID,I
      REAL(8) GRS,SIZE
      DATA BREI/32*0, &
      10,4,6,4*12,6,8,8,12,12,6,12,6,12, &
      12,8,8*12,6,6,4*12, &
      9*12,4,3*12,14,2*12, &
      7*12,14,3*12,8,12,8,2*12, &
      6,8*12,4,8,12,4,14,2*12, &
      2*12,10,12,10,2*12,14,3*12,8,4,8,12,6/
      WID=0
      CODE=0
      DO 500,I=1,LL
      A1=CH(I:I)
      I1=ICHAR(A1)
      IF (CODE.EQ.1) THEN
      IF (A1.EQ.' '.OR.A1.EQ.'>'.OR.A1.EQ.'<'.OR.A1.EQ.'^' &
      .OR.A1.EQ.'_'.OR.A1.EQ.'G'.OR.A1.EQ.'g'.OR.A1.EQ.'n' &
      .OR.A1.EQ.'h'.OR.A1.EQ.'-'.OR.I1.EQ.92) &
       CODE=0
      ELSE
      IF (I1.EQ.92) THEN
      CODE=1
      ELSE
      WID=WID+BREI(I1)
      END IF
      END IF
  500 CONTINUE
      KOR=4
      SIZE=DBLE(WID-KOR)*GRS/12.0D0
      RETURN
      END
!******************************
      SUBROUTINE GELI(CH,FF)
      IMPLICIT NONE
      CHARACTER*(*) CH
      CHARACTER(500) CH001
      CHARACTER(16) CH016
      REAL(8) FF
      INTEGER(4) I001,LAE,I,II
      LAE=LEN(CH)
      CALL FIBLA(CH,I001)
      IF (I001.EQ.0) THEN
      FF=0.0D0
      RETURN
      END IF
      CH001=CH(I001:)
      I001=INDEX(CH001,' ')
      CH016=CH001(1:I001-1)
      READ (UNIT=CH016,FMT='(BN,D16.0)',ERR=999) FF
      CH=CH001(I001:)
      RETURN
  999 WRITE (UNIT=6,FMT=302)
  302 FORMAT (//' Troubles with format-free reading')
      CALL LABLA(CH001,II)
      WRITE (UNIT=6,FMT=303) (CH001(I:I),I=1,II)
  303 FORMAT (/' Remaining record: ',250A1)
      WRITE (UNIT=6,FMT=304) CH016
  304 FORMAT (' The following string cannot be converted', &
      ' to a real number: ',A16)
      STOP
      END
!-----
!******************************
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
      WRITE (UNIT=I001,FMT='(250A1)') (CH(J:J),J=1,II)
      RETURN
      END
!-----
!********************************
      SUBROUTINE helpme(tag,tpath,itpath)
      IMPLICIT NONE
      CHARACTER*(*) tag,tpath
      INTEGER(4)  hlp,kbd,scr,log,cln,plt,rxn,grd
      COMMON /iounits/ hlp,kbd,scr,log,cln,plt,rxn,grd
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
        WRITE (UNIT=scr,FMT='(/A)') ' press CR to continue!'
        READ (kbd,*)
        GOTO 50
      END IF
      IF (line(1:4).EQ.'$END') GOTO 200
      WRITE (UNIT=scr,FMT='(1X,A80)') line(1:80)
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
!********************************
      SUBROUTINE writetit(iu,os)
      implicit none
      integer  hlp,kbd,scr,log,cln,plt,rxn,grd
      common /iounits/ hlp,kbd,scr,log,cln,plt,rxn,grd
      character progname*15,vers*20,task*80
      character *(*) os
      integer i, j, k, iu
      progname='GUZZLER'
      vers='28.05.2022'
      task='"Labeling reactions in graphics files"'
      call LABLA(progname,i)
      call LABLA(vers,j)
      call LABLA(os,k)
!-----
      if(iu.eq.scr) call clearscreen
      WRITE (iu,1000) progname(1:i), vers(1:j), os(1:k)
 1000 FORMAT (/, &
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
!********************************
      block data guzzini
      integer  hlp,kbd,scr,log,cln,plt,rxn,grd
      common /iounits/ hlp,kbd,scr,log,cln,plt,rxn,grd
      data  hlp,kbd,scr,log,cln,plt,rxn,grd &
            / 2,  5,  6, 12, 13, 14, 15, 16/
      end
