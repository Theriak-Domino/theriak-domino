!-----Version: 28.05.2022
!               ***********
!               * jphases *
!               ***********
!
!     Program written by Christian de Capitani
!     at the Department of Geology
!            Stanford University
!            Stanford, CA., 94305   (1989-1991)
!     and at Mineralogisch-Petrographisches Institut
!            Universitaet Basel     (since 1992)
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
      INTEGER(4) I001,I,I1,I2,II,COMAY,EINS
      REAL(8) FF
      CHARACTER(500) CH001,SYREC
      CHARACTER(80) KOMMENTAR
!----
      INTEGER(4)  &
      ierr, j, jj, &
      LARG
      CHARACTER(26) SORTSTRING(PHMAX)
      CHARACTER(500) XLINE
!--
      progname='jphases'
      vers='28.05.2022'
      task='"read/write files in javathings/..."'
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
!-----
!      WRITE (UNIT=6,FMT='(''larg: '',i2)') larg
!      WRITE (UNIT=6,FMT='(''arg: '',i2,1x,a)') &
!      ((I,largum(I)),I=1,larg)
!-----
!    5 CONTINUE
!*****
      IFNR=5
      EINS=1
!*****
      ierr=0
      call initialize('$DOMINO-FILES',ierr)
      if(ierr.ne.0) STOP
!    1 CONTINUE
!*****
!-----------------------------------
!     write javathings/dominofiles
!-----------------------------------
!      OPEN (UNIT=40,FILE='javathings/dominofiles')
!!
!      j=dat
!      line=filename(j)(1:fnl(j))//ext(j)
!      path=wpath
!      CALL LABLA(line,I1)
!      WRITE (UNIT=40,FMT='(A)') line(1:i1)
!!
!      j=log
!      line=filename(j)(1:fnl(j))//ext(j)
!      CALL LABLA(line,I1)
!      WRITE (UNIT=40,FMT='(A)') line(1:i1)
!!
!      j=out
!      line=filename(j)(1:fnl(j))//ext(j)
!      CALL LABLA(line,I1)
!      WRITE (UNIT=40,FMT='(A)') line(1:i1)
!!
!      j=plt
!      line=filename(j)(1:fnl(j))//ext(j)
!      CALL LABLA(line,I1)
!      WRITE (UNIT=40,FMT='(A)') line(1:i1)
!!
!      j=cln
!      line=filename(j)(1:fnl(j))//ext(j)
!      CALL LABLA(line,I1)
!      WRITE (UNIT=40,FMT='(A)') line(1:i1)
!!
!      j=pst
!      line=filename(j)(1:fnl(j))//ext(j)
!      CALL LABLA(line,I1)
!      WRITE (UNIT=40,FMT='(A)') line(1:i1)
!!
!      j=fig
!      line=filename(j)(1:fnl(j))//ext(j)
!      CALL LABLA(line,I1)
!      WRITE (UNIT=40,FMT='(A)') line(1:i1)
!!
!      CLOSE (UNIT=40)
      
      
      
      
      
      
      
      
!---------------------------
!     read javathings/lastdb
!---------------------------
      OPEN (UNIT=40,FILE='javathings'//dir//'lastdb',STATUS='OLD')
      READ (UNIT=40,FMT='(A)') CH001
      filename(dbs)=CH001
      CLOSE (UNIT=40)
!------------------
!     open UNIT=dbs
!------------------
      j=dbs
      line=CH001
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
      REWIND (UNIT=dbs)
      REWIND (UNIT=dat)
!*****
      COMAY=COMAX
      CALL PROREAD(SYREC)
!!-----------------------------
!!     write javathings/scrline4
!!-----------------------------
!      OPEN (UNIT=40,FILE='javathings/scrline4')
!      WRITE (40,1010) PRAT,LO1MAX,EQUALX,TEST,DXMIN,DXSCAN, &
!      DXSTAR,STPSTA,STPMAX,GCMAX
! 1010 FORMAT (F10.4,2X,I5,2X,F10.6,2X,E14.7,2X,E14.7,2X,E14.7, &
!      E14.7,2X,I5,2X,I6,2X,I6)
!      CLOSE (UNIT=40)
!---------------------------
!     read javathings/cline0
!---------------------------
      OPEN (UNIT=40,FILE='javathings'//dir//'cline0',STATUS='OLD')
      READ (UNIT=40,FMT='(A)') CH001
      SYREC=CH001
      CLOSE (UNIT=40)
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
      CALL LABLA(FORMUL,I1)
      CALL TAXI(SYREC,USE)
      CALL CHEMIE(COMAY,NC,OXYDE,OXANZ,FORMUL,CHEM)
      CALL LABLA(USE,LUSE)
      CALL TAXI(SYREC,KOMMENTAR)
!*****
      TEST=DABS(TEST)
      CALL DBREAD
      CALL NURVONPT
      CALL CALSTR
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
      I001=NPHA-NUN
      WRITE (scr,506) ('-',I=1,35)
  506 FORMAT (/,'List of phase names (abbreviations)',/,80A1)
      WRITE (UNIT=scr,FMT='(5(2X,A22))') (SORTSTRING(II),II=1,I001)
      WRITE (UNIT=scr,FMT='(120A1)') ('-',I=1,120)
!-----------------------------
!     write javathings/nameabk
!-----------------------------
      OPEN (UNIT=40,FILE='javathings'//dir//'nameabk')
      WRITE (UNIT=40,FMT='(A26)') (SORTSTRING(II),II=1,I001)
      CLOSE (UNIT=40)
!
!--
      IF (NSOL.GT.0) THEN
       WRITE (UNIT=scr,FMT='(/A)') &
       'List of solutions, endmembers and site occupancies'
       WRITE (UNIT=scr,FMT='(80A1)') ('-',I=1,50)
       DO 580,I=1,NSOL
        WRITE (6,2072) SOLNAM(I),(ABK(EM(I,II)),II=1,NEND(I))
 2072   FORMAT (3X,A16,' : ',5(A8,2X)/20(22X,5(A8,2X)/))
        IF (NSIEL(I).GT.0) THEN
         WRITE (6,2073) (SIEL(I,II),II=1,NSIEL(I))
 2073    FORMAT (20(22X,5A10/))
        END IF
  580  CONTINUE
      END IF
!
!-----------------------------
!     write javathings/solnames
!-----------------------------
      OPEN (UNIT=40,FILE='javathings'//dir//'solnames')
!
      XLINE=' '
      IF (NSOL.GT.0) THEN
       DO I=1,NSOL
      WRITE (40,2082) SOLNAM(I),(ABK(EM(I,II)),II=1,NEND(I)), &
      '$',(SIEL(I,I2),I2=1,NSIEL(I))
 2082 FORMAT (A16,'  ',200(A10,2X))
      END DO
      ELSE
      WRITE (UNIT=40,FMT='(''    '')')
      END IF
!
!
!
      CLOSE (UNIT=40)
!-----
!
!***************************************************************
!******  END OF INPUT UND TESTS. START CALCULATING *************
!***************************************************************
      END
