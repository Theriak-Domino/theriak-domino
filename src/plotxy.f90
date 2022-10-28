!-----Version: 28.05.2022
!               **********
!               * PLOTXY * 
!               **********
!
      IMPLICIT NONE
      include 'files.cmn'
      !
!     Dateien
!     log   <=== unit 20
!     dbs   <=== unit 30
!     plt   <=== unit 35

!----
      INTEGER(4) MAXICOL,MAXIROW
      PARAMETER (MAXICOL=5000,MAXIROW=500)
      CHARACTER(64) COLNAM(MAXICOL),COLNAMO(MAXICOL)
      INTEGER(4) NCOL,NROW,COLNR(MAXICOL),NLIN,TST
      REAL(8) TABELLE(MAXIROW,MAXICOL),XLIN(MAXIROW,20), &
      YLIN(MAXIROW,20)
      LOGICAL(4) INFODA
      COMMON /GLOIN/ NCOL,NROW,COLNR,NLIN,TST
      COMMON /GLOCH/ COLNAM,COLNAMO
      COMMON /GLORE/ TABELLE,XLIN,YLIN
      COMMON /GLOLO/ INFODA
!----- END OF COMMON VARIABLES
!-----
!      LOGICAL(4) VERGL
      CHARACTER(100000) SYSIN
      CHARACTER(500) CHIN(3),CH001,CH002
      CHARACTER(64) CH64,TRUNK
      CHARACTER(80) INFILE
      INTEGER(4) I,I001,I002,I1,I2,IU,LARG,IFNR
      INTEGER(4) j, ierr
!*****
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
!-----
!----- TST=1 for extended printing
      TST=0
!*****
      CHIN(1)=' '
      CHIN(2)=' '
      CHIN(3)=' '
      CH001=' '
      CH002=' '
      INFODA=.FALSE.
!***** 
      progname='PLOTXY'
      vers='28.05.2022'
      task='"XY-plots from tables of variables"'
!      EINS=1
      ierr=0
      call initialize('$PLOTXY-FILES',ierr)
      if(ierr.ne.0) STOP
      descr(dbs)= 'Table of themodynamic variables'
!      REFAS=.FALSE.
!------------------
!     Open UNIT=log
!------------------
!      OPEN(log,FILE='plotxy.last',STATUS='UNKNOWN')
      j=log
      line=filename(j)(1:fnl(j))//ext(j)
      path=wpath
      akzess=' '
      state=' '
      call openfile(j,ierr)
      if(ierr.ne.0) STOP
!-----
      DO I=1,3
       READ (log,FMT='(A500)',END=411) CHIN(I)
      END DO
  411 CONTINUE
      CLOSE (log)
!----
      WRITE (scr,100)
  100 FORMAT (/ &
      ' ----------'/ &
      ' input file'/ &
      ' ----------')
      CALL LABLA(CHIN(1),I002)
      IF (I002.EQ.0) I002=1
      CH002=' Enter [ "?" | CR | "files" | filename ] <'// &
      CHIN(1)(1:I002)//'>?'
!-----
  101 CALL PUST (scr,CH002)
      READ (IFNR,FMT='(A500)') CH001
      IF (CH001.EQ.'?') THEN
        CALL helpme('$XY-START')
        GOTO 101
      END IF
      IF (CH001.EQ.'files') THEN
        CALL listfiles
        GOTO 101
      END IF
      IF (CH001.EQ.' ') THEN
         CH001=CHIN(1)
         I001=I002
      ELSE
          CHIN(1)=CH001
      END IF
      CALL LABLA(CH001,I001)
!-----
!      IF (VERGL(CH001,'loop_table')) INFODA=.TRUE.
      IU=INDEX(CH001,'_table')
      IF (IU.NE.0) THEN
        TRUNK=CH001(1:IU-1)
        filename(inf)=TRUNK(1:IU-1)//'_info'
        fnl(inf)=IU+4
        INFODA=.TRUE.
      END IF
      
!------------------
!     Open UNIT=dbs
!------------------
      j=dbs
      line=CH001(1:I001)
      path=wpath
      akzess=' '
      state='old'
      call openfile(j,ierr)
      if(ierr.ne.0) STOP
!-----
      READ (dbs,FMT='(A)') SYSIN
      NCOL=0
      I1=1
      DO I=1,MAXICOL
       I2=INDEX(SYSIN,',')
       IF (I2.NE.0) THEN
        NCOL=NCOL+1
        READ (UNIT=SYSIN(I1:I2-1),FMT='(A)') COLNAM(NCOL)
        SYSIN(I2:I2)=' '
        I1=I2+1
       ELSE
        NCOL=NCOL+1
        READ (UNIT=SYSIN(I1:),FMT='(A)') COLNAM(NCOL)
        GOTO 422
       END IF
      END DO
!-----
  422 CONTINUE
      DO I=1,NCOL
       COLNAMO(I)=COLNAM(I)
      END DO
      CALL SORTIER(COLNAMO,NCOL)
!-----
      DO I=1,NCOL
       IF (COLNAM(I)(1:1).EQ.':') THEN
        CH64=COLNAM(I)(2:)
        COLNAM(I)=CH64
       END IF
       IF (COLNAMO(I)(1:1).EQ.':') THEN
        CH64=COLNAMO(I)(2:)
        COLNAMO(I)=CH64
       END IF
      END DO
!-----
      DO I1=1,NCOL
       DO I2=1,NCOL
        IF (COLNAMO(I1).EQ.COLNAM(I2)) THEN
         COLNR(I1)=I2
         GOTO 433
        END IF
       END DO
       write(scr,FMT='(''impossible error'')')
  433  CONTINUE
      END DO
!+++++
!     used to be:
!       READ (dbs,*,end=888) (TABELLE(I,I1),I1=1,NCOL)
!       no idea why its not working anymore
      NROW=0
      DO I=1,MAXIROW
       READ (dbs,FMT='(A)',END=888) SYSIN
       READ (SYSIN,*,END=887) (TABELLE(I,I1),I1=1,NCOL)
  887 CONTINUE
       NROW=I
      END DO
  888 CONTINUE
!+++++
      IF (TST.EQ.1) THEN
       write(scr,1000)
       write(scr,1002) (I,COLNAM(I),I=1,NCOL)
      END IF
!+
    7 write(scr,1000)
 1000 FORMAT (/,' Variables available:')
      call helpme('$XY-ABBREV*')
      write(scr,1002) (I,COLNAMO(I),I=1,NCOL)
 1002 FORMAT (5(I5,':',1X,A16))
!-----
      CALL LABLA(CHIN(2),I002)
      IF (I002.EQ.0) I002=1
      CH002=' Enter X-axis: [ "?" | CR | number(s) ] <'// &
      CHIN(2)(1:I002)//'>?'
!-----
      WRITE (UNIT=scr,FMT='('' '')')
      call helpme('$XY-AXES*')
      CALL PUST (6,CH002)
      READ (IFNR,FMT='(A500)') CH001
      IF (CH001.eq.'?') THEN
        CALL helpme('$XY-AXES')
        GOTO 7
      END IF
      IF (CH001.EQ.' ') THEN
         CH001=CHIN(2)
      ELSE
          CHIN(2)=CH001
      END IF
!-----
      CALL LABLA(CHIN(3),I002)
      IF (I002.EQ.0) I002=1
      CH002=' Enter Y-axis: [ "?" | CR | number(s) ] <'// & 
      CHIN(3)(1:I002)//'>?'
!-----
      WRITE (UNIT=scr,FMT='('' '')')
      CALL PUST (6,CH002)
      READ (IFNR,FMT='(A500)') CH002
      IF (CH002.eq.'?') THEN
        CALL helpme('$XY-AXES')
        GOTO 7
      END IF
      IF (CH002.EQ.' ') THEN
         CH002=CHIN(3)
      ELSE
          CHIN(3)=CH002
      END IF
!------------------
!     Open UNIT=log
!------------------
!      OPEN(log,FILE='plotxy.last',STATUS='OLD')
      j=log
      line=filename(j)(1:fnl(j))//ext(j)
      path=wpath
      akzess=' '
      state='old'
      call openfile(j,ierr)
      if(ierr.ne.0) STOP
      DO I=1,3
       CALL PUST(log,CHIN(I))
      END DO
      CLOSE (UNIT=log)
!
      CALL MACHLIN(CH001,CH002,IFNR)
!=====
      END
!-----
!******************************
      SUBROUTINE MACHLIN(CH001,CH002,IFNR)
      IMPLICIT NONE
      include 'files.cmn'
!----
      INTEGER(4) MAXICOL,MAXIROW
      PARAMETER (MAXICOL=5000,MAXIROW=500)
      CHARACTER(64) COLNAM(MAXICOL),COLNAMO(MAXICOL)
      INTEGER(4) NCOL,NROW,COLNR(MAXICOL),NLIN,TST
      REAL(8) TABELLE(MAXIROW,MAXICOL),XLIN(MAXIROW,20), &
      YLIN(MAXIROW,20)
      LOGICAL(4) INFODA
      COMMON /GLOIN/ NCOL,NROW,COLNR,NLIN,TST
      COMMON /GLOCH/ COLNAM,COLNAMO
      COMMON /GLORE/ TABELLE,XLIN,YLIN
      COMMON /GLOLO/ INFODA
!----- END OF COMMON VARIABLES
!-----
      CHARACTER(500) CH001,CH002,BULINE(3)
      CHARACTER(1) C1
      CHARACTER(80) COMINS(50),DATI
      CHARACTER(64) LINTEX(20),XTXT,YTXT,BLANK,CH64
      CHARACTER(32) XVAR(20),YVAR(20),CH8,CHPASS,CH98,CH97
      CHARACTER(16) XMAPDIVCH(20,20),YMAPDIVCH(20,20),CH16
      INTEGER(4) I,I1,I2,MULTI,IM,IX,IY,NL,NXVAR,NYVAR, &
      IXX,IYY,IR,I0,NXSUM(20),NYSUM(20), &
      XXSUM(20,10),YYSUM(20,10),NBUL,NCOMIN,II,ICH,IFNR
      REAL(8) XMINI,XMAXI,YMINI,YMAXI,BREIT,HOCH,SIZLAB,ANGE,FF, &
      XMAPDIV(20,20),YMAPDIV(20,20),X98,Y98,SIZ98,ANG98,F1,F2,F3,F4, &
      XWIDE,YHIGH,YPOSA,XPOSB,XPOSC,YPOSC
      integer j, ierr,LA98
!-----
      DO I1=1,20
       DO I2=1,20
        XMAPDIV(I1,I2)=1.0D0
        YMAPDIV(I1,I2)=1.0D0
       END DO
      END DO
!-----
       C1=','
       NXVAR=0
       DO I=1,20
        NXVAR=NXVAR+1
        CALL FINK(I2,CH001,XVAR(NXVAR),C1)
        IF (XVAR(NXVAR).EQ.' ') NXVAR=NXVAR-1
        IF (I2.EQ.0) GOTO 40
       END DO
   40  CONTINUE
       NYVAR=0
       DO I=1,20
        NYVAR=NYVAR+1
        CALL FINK(I2,CH002,YVAR(NYVAR),C1)
        IF (YVAR(NYVAR).EQ.' ') NYVAR=NYVAR-1
        IF (I2.EQ.0) GOTO 41
       END DO
   41  CONTINUE
!-----
      IF (NXVAR.EQ.0) THEN
       WRITE(scr,FMT='('' X-axis has no value'')')
       RETURN
      END IF
      IF (NYVAR.EQ.0) THEN
       WRITE(scr,FMT='('' Y-axis has no value'')')
       RETURN
      END IF
      IF (NXVAR.NE.1.AND.NYVAR.NE.1) THEN
       WRITE(scr,FMT= &
       '('' only one axis may have multiple variables'')')
       RETURN
      END IF
      IF (NXVAR.EQ.NYVAR) MULTI=0
      IF (NXVAR.GT.NYVAR) MULTI=1
      IF (NXVAR.LT.NYVAR) MULTI=2
!-----
      C1='+'
      DO IXX=1,NXVAR
       NXSUM(IXX)=0
       CHPASS=XVAR(IXX)
       DO I=1,10
        NXSUM(IXX)=NXSUM(IXX)+1
        CALL FINK(I2,CHPASS,CH8,C1)
!        CALL GELI(CH8,FF)
!        XXSUM(IXX,NXSUM(IXX))=IDINT(FF)
        CALL FUNGELI(CH8,I1,FF,CH16)
        XXSUM(IXX,NXSUM(IXX))=I1
        XMAPDIV(IXX,I)=FF
        XMAPDIVCH(IXX,I)=CH16
        IF (FF.EQ.0.0D0) NXSUM(IXX)=NXSUM(IXX)-1
        IF (I2.EQ.0) GOTO 50
       END DO
   50 CONTINUE
      END DO
!-----
      DO IYY=1,NYVAR
       NYSUM(IYY)=0
       CHPASS=YVAR(IYY)
       DO I=1,10
        NYSUM(IYY)=NYSUM(IYY)+1
        CALL FINK(I2,CHPASS,CH8,C1)
!        CALL GELI(CH8,FF)
!        YYSUM(IYY,NYSUM(IYY))=IDINT(FF)
        CALL FUNGELI(CH8,I1,FF,CH16)
        YYSUM(IYY,NYSUM(IYY))=I1
        YMAPDIV(IYY,I)=FF
        YMAPDIVCH(IYY,I)=CH16
        IF (FF.EQ.0.0D0) NYSUM(IYY)=NYSUM(IYY)-1
        IF (I2.EQ.0) GOTO 51
       END DO
   51 CONTINUE
      END DO
!-----
      DO IXX=1,NXVAR
       DO I=1,NXSUM(IXX)
        I1=XXSUM(IXX,I)
!+
      IF (TST.EQ.1) THEN
      WRITE (scr,3010) IXX,I,I1,COLNR(I1),COLNAM(COLNR(I1))
 3010 FORMAT(' XVAR:',I3,' NXSUM:',I3,' XXSUM:',I3,' COLNR:',I3,1X,A)
      END IF
!+
        IF (I1.LE.0.OR.I1.GT.NCOL) THEN
         WRITE (scr,FMT='(''number must be  0  -'',I4)') NCOL
         RETURN
        END IF
       END DO
      END DO
      DO IYY=1,NYVAR
       DO I=1,NYSUM(IYY)
        I1=YYSUM(IYY,I)
!+
      IF (TST.EQ.1) THEN
      WRITE (scr,3008) IYY,I,I1,COLNR(I1),COLNAM(COLNR(I1))
 3008 FORMAT(' YVAR:',I3,' NYSUM:',I3,' YYSUM:',I3,' COLNR:',I3,1X,A)
      END IF
!+
        IF (I1.LE.0.OR.I1.GT.NCOL) THEN
         WRITE (scr,FMT='(''number must be  0  -'',I4)') NCOL
         RETURN
        END IF
       END DO
      END DO
!-----
!+
      IF (TST.EQ.1) THEN
      write(scr,3050) NXVAR,NYVAR
 3050 FORMAT (' NXVAR,NYAVR ',2I5)
      WRITE (scr,3052) (I,XVAR(I),I=1,NXVAR)
 3052 FORMAT (' XVAR: ',20(I2,':',A))
      WRITE (scr,3054) (I,YVAR(I),I=1,NYVAR)
 3054 FORMAT (' YVAR: ',20(I2,':',A))
      END IF
!+
!+++
      BLANK=' '
!      XTXT=COLNAM(COLNR(XXSUM(1,1)))
      CH64=COLNAM(COLNR(XXSUM(1,1)))
      CALL LABLA(CH64,I1)
      CH64(I1+1:)=XMAPDIVCH(1,1)
      XTXT=CH64
!      YTXT=COLNAM(COLNR(YYSUM(1,1)))
      CH64=COLNAM(COLNR(YYSUM(1,1)))
      CALL LABLA(CH64,I1)
      CH64(I1+1:)=YMAPDIVCH(1,1)
      YTXT=CH64
      IF (MULTI.EQ.1) XTXT=COLNAM(COLNR(XXSUM(1,1)))(1:1)
      IF (MULTI.EQ.2) YTXT=COLNAM(COLNR(YYSUM(1,1)))(1:1)
      XMINI=1D20
      XMAXI=-1D20
      YMINI=1D20
      YMAXI=-1D20
      NLIN=0
      DO IXX=1,NXVAR
       DO IYY=1,NYVAR
        NLIN=NLIN+1
        IF (MULTI.EQ.0) LINTEX(NLIN)=' '
        IF (MULTI.EQ.1) THEN
         CH64=COLNAM(COLNR(XXSUM(IXX,1)))
         CALL LABLA(CH64,I1)
         CH64(I1+1:)=XMAPDIVCH(IXX,1)
         LINTEX(NLIN)=CH64
!         LINTEX(NLIN)=COLNAM(COLNR(XXSUM(IXX,1)))
         DO IX=2,NXSUM(IXX)
          CH64=COLNAM(COLNR(XXSUM(IXX,IX)))
          CALL LABLA(CH64,I1)
          CH64(I1+1:)=XMAPDIVCH(IXX,IX)
          CALL LABLA(LINTEX(NLIN),I)
          LINTEX(NLIN)(I+1:)='+'//CH64
!          LINTEX(NLIN)(I+1:)='+'//COLNAM(COLNR(XXSUM(IXX,IX)))
         END DO
        END IF
        IF (MULTI.EQ.2.OR.MULTI.EQ.0) THEN
         CH64=COLNAM(COLNR(YYSUM(IYY,1)))
         CALL LABLA(CH64,I1)
         CH64(I1+1:)=YMAPDIVCH(IYY,1)
         LINTEX(NLIN)=CH64
!         LINTEX(NLIN)=COLNAM(COLNR(YYSUM(IYY,1)))
         DO IY=2,NYSUM(IYY)
          CH64=COLNAM(COLNR(YYSUM(IYY,IY)))
          CALL LABLA(CH64,I1)
          CH64(I1+1:)=YMAPDIVCH(IYY,IY)
          CALL LABLA(LINTEX(NLIN),I)
          LINTEX(NLIN)(I+1:)='+'//CH64
!          LINTEX(NLIN)(I+1:)='+'//COLNAM(COLNR(YYSUM(IYY,IY)))
         END DO
        END IF
!+
      IF (TST.EQ.1) THEN
      write(scr,1000) IXX,IYY,NLIN,NXSUM(IXX),NYSUM(IYY)
 1000 FORMAT (' IXX,IYY:',2I3,' NLIN,NXSUM(IXX,NYSUM(IYY) ',3I5)
      write(scr,1002) (COLNR(XXSUM(IXX,I)),I=1,NXSUM(IXX))
 1002 FORMAT (' COLNR(XXSUM(IXX,I)) ',10I5)
      write(scr,2002) (COLNR(YYSUM(IYY,I)),I=1,NYSUM(IYY))
 2002 FORMAT (' COLNR(YYSUM(IYY,I)) ',10I5)
      END IF
!+
        DO IR=1,NROW
!=====
         FF=0.0D0
         DO IX=1,NXSUM(IXX)
          FF=FF+TABELLE(IR,COLNR(XXSUM(IXX,IX)))/XMAPDIV(IXX,IX)
         END DO
         XLIN(IR,NLIN)=FF
         IF (XLIN(IR,NLIN).GT.XMAXI) XMAXI=XLIN(IR,NLIN)
         IF (XLIN(IR,NLIN).LT.XMINI) XMINI=XLIN(IR,NLIN)
         FF=0.0D0
         DO IY=1,NYSUM(IYY)
          FF=FF+TABELLE(IR,COLNR(YYSUM(IYY,IY)))/YMAPDIV(IYY,IY)
         END DO
         YLIN(IR,NLIN)=FF
         IF (YLIN(IR,NLIN).GT.YMAXI) YMAXI=YLIN(IR,NLIN)
         IF (YLIN(IR,NLIN).LT.YMINI) YMINI=YLIN(IR,NLIN)
!=====
        END DO
       END DO
      END DO
!+
      IF (TST.EQ.1) THEN
      write(scr,3000) MULTI
 3000 FORMAT (' MULTI ',I5)
      END IF
!+
      IF (XMINI.EQ.XMAXI) THEN
       XMINI=XMINI*0.9D0
       XMAXI=XMAXI*1.1D0
       IF (XMINI.EQ.0.0D0) THEN
        XMINI=-1.0D0
        XMAXI=1.0D0
       END IF
      END IF
      IF (YMINI.EQ.YMAXI) THEN
       YMINI=YMINI*0.9D0
       YMAXI=YMAXI*1.1D0
       IF (YMINI.EQ.0.0D0) THEN
        YMINI=-1.0D0
        YMAXI=1.0D0
       END IF
      END IF
!=====
      BREIT=15.0D0
      HOCH=15.0D0
      write(scr,4000) XMINI,XMAXI,BREIT
 4000 FORMAT (/,' Enter [ CR | X-min  X-max  width ]  <' &
      ,2F20.8,2X,F8.3,'>? ')
      read(IFNR,FMT='(A500)') CH001
      IF (CH001.NE.' ') THEN
      CALL GELI(CH001,XMINI)
      CALL GELI(CH001,XMAXI)
      CALL GELI(CH001,FF)
      FF=DABS(FF)
      IF (FF.GT.0.1) BREIT=FF
      END IF
      write(scr,4005) YMINI,YMAXI,HOCH
 4005 FORMAT (/,' Enter [ CR | Y-min  Y-max  height ] <' &
      ,2F20.8,2X,F8.3,'>? ')
      read(IFNR,FMT='(A500)') CH001
      IF (CH001.NE.' ') THEN
      CALL GELI(CH001,YMINI)
      CALL GELI(CH001,YMAXI)
      CALL GELI(CH001,FF)
      FF=DABS(FF)
      IF (FF.GT.0.1) HOCH=FF
      END IF
      NBUL=0
      NCOMIN=0
      DATI='00.00.0000 - 00:00:00  CPU time:    0h 00m 00.00s'
      IF (INFODA) THEN
!------------------
!     Open UNIT=inf
!------------------
      j=inf
      line=filename(j)(1:fnl(j))//ext(j)
      path=wpath
      akzess=' '
      state='unknown'
      call openfile(j,ierr)
      if(ierr.ne.0) STOP
!-----
      READ(inf,FMT='(A)',END=888) DATI
      READ(inf,FMT='(I4)',END=888) NBUL
      DO II=1,NBUL
       READ(inf,FMT='(A)',END=888) BULINE(II)
      END DO
      READ(inf,FMT='(I4)',END=888) NCOMIN
      DO II=1,NCOMIN
       READ(inf,FMT='(A)',END=888) COMINS(II)
      END DO
  888 CONTINUE
      CLOSE (UNIT=inf)
      END IF
!------------------
!     Open UNIT=plt
!------------------
!      OPEN (UNIT=35,FILE='plot',STATUS='UNKNOWN')
      j=plt
      line=filename(j)(1:fnl(j))//ext(j)
      path=wpath
      akzess=' '
      state=' '
      call openfile(j,ierr)
      if(ierr.ne.0) STOP
!-----
!      BREIT=15.0D0
!      HOCH=15.0D0
      I0=0
      WRITE(scr,FMT='(/,'' X-axis  : '',A)') 'L:'//XTXT
      WRITE(scr,FMT='('' Y-axis  : '',A)') YTXT
      write(plt,FMT='(A25/A25)') 'L:'//XTXT,YTXT
      write(plt,2010) XMINI,XMAXI,YMINI,YMAXI,BREIT,HOCH,I0,I0
 2010 FORMAT(4(1PE20.12),0P,2F10.2,2I3)
!+
      XWIDE=XMAXI-XMINI
      YHIGH=YMAXI-YMINI
      YPOSA=(YHIGH/HOCH)*0.8D0+YMAXI
      XPOSB=(XWIDE/BREIT)*1.5D0+XMAXI
!      XPOSC=XMINI+(XWIDE/BREIT)*15.0D0
      XPOSC=XMINI+(XWIDE/BREIT)*BREIT
      YPOSC=YMINI-(YHIGH/HOCH)*2.0D0
      I1=2
      IF (INFODA) I1=NBUL+NCOMIN+3
      WRITE (plt,2012) I1
 2012 FORMAT (I5,'    0    0    0    0    0')
      IF (INFODA) THEN
      F1=XMINI
      F3=0.2D0
      F4=0.0D0
      ICH=0
      CALL LABLA(DATI,I1)
      WRITE (plt,2013) XPOSC,YPOSC,F3,F4,ICH,DATI(1:I1)
 2013 FORMAT (2(1PE20.12),0PF10.7,F10.4,I5,A)
      DO II=1,NBUL
      F2=YPOSA+(DBLE(II-1)*YHIGH/HOCH)*0.35D0
      CALL LABLA(BULINE(II),I1)
      WRITE (plt,2014) F1,F2,F3,F4,ICH,II,BULINE(II)(1:I1)
 2014 FORMAT (2(1PE20.12),0PF10.7,F10.4,I5,'Bulk(',I1,')= ',A)
      END DO
      F1=XPOSB
      DO II=1,NCOMIN
      F2=YMAXI-(DBLE(II-1)*YHIGH/HOCH)*0.35D0
      CALL LABLA(COMINS(II),I1)
      WRITE (plt,2015) F1,F2,F3,F4,ICH,COMINS(II)(1:I1)
 2015 FORMAT (2(1PE20.12),0PF10.7,F10.4,I5,A)
      IF (COMINS(II)(1:3).EQ.'drv') F1=(XWIDE/BREIT)*2.0D0+XMAXI
      END DO
      END IF
      X98=0.0D0
      Y98=0.0D0
      SIZ98=0.0D0
      ANG98=0.0D0
      LA98=0
      CH97='97'
      CH98='98'
      WRITE (plt,2024) X98,Y98,SIZ98,ANG98,LA98,CH97
      WRITE (plt,2024) X98,Y98,SIZ98,ANG98,LA98,CH98
 2024 FORMAT (2(1PE20.12),0PF10.7,F10.4,I5,A2)
!+
      DO NL=1,NLIN
!=====
      DO I=1,NROW
       IF (XLIN(I,NL).LT.XMINI) XLIN(I,NL)=XMINI
       IF (XLIN(I,NL).GT.XMAXI) XLIN(I,NL)=XMAXI
       IF (YLIN(I,NL).LT.YMINI) YLIN(I,NL)=YMINI
       IF (YLIN(I,NL).GT.YMAXI) YLIN(I,NL)=YMAXI
      END DO
      write(plt,FMT='(''    2'',I5,''    0    0    0    0'')') NROW
      write(plt,2020) (XLIN(I,NL),YLIN(I,NL),3-MIN0(1,I-1),I=1,NROW)
 2020 FORMAT (7(2(1PE20.12),I2))
!-----
      SIZLAB=0.35D0
      ANGE=0.0D0
      I1=0
      IM=NROW/2
!-----
      write(plt,2025) XLIN(IM,NL),YMINI,SIZLAB,ANGE,I1,BLANK
      write(plt,2025) XLIN(IM,NL),YMAXI,SIZLAB,ANGE,I1,LINTEX(NL)
 2025 FORMAT (2(1PE20.12),0PF10.7,F10.4,I5,A64)
      WRITE(scr,FMT='('' line '',I3,'': '',A)') NL,LINTEX(NL)
      END DO
!=====
      CLOSE (plt)
!-----
      RETURN
      END
!-----
!******************************
      SUBROUTINE FINK(I2,CH001,CH,C1)
      IMPLICIT NONE
      include 'files.cmn'
!----
      CHARACTER*(*) CH001,CH
      CHARACTER(1) C1
      CHARACTER(500) CH002
      INTEGER(4) I1,I2
!-----
      I2=0
      CH002=CH001
      CALL FIBLA(CH002,I1)
      IF (I1.EQ.0) THEN
       CH=' '
       RETURN
      END IF
      I2=INDEX(CH002,C1)
      IF (I2.NE.0) THEN
       CH002(I2:I2)=' '
       CH=CH002(I1:I2)
       CH001=CH002(I2+1:)
      ELSE
       CH=CH002
       CH001=' '
      END IF
!-----
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
      

