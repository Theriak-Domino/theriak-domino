!-----Version: 28.05.2022
!               **********
!               * JFILES *
!               **********
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
      INTEGER(4) I1,J,ierr
!--
      progname='JFILES'
      vers='28.05.2022'
      task='"read/write files in javathings/..."'
!*****
      ierr=0
      call initialize('$DOMINO-FILES',ierr)
      if(ierr.ne.0) STOP
!*****
!*****
!-----------------------------
!     write javathings/scrline4
!-----------------------------
      PRAT=1.0D0;
      OPEN (UNIT=40,FILE='javathings'//dir//'scrline4')
      WRITE (40,1010) PRAT,LO1MAX,EQUALX,TEST,DXMIN,DXSCAN, &
      DXSTAR,STPSTA,STPMAX,GCMAX
 1010 FORMAT (F10.4,2X,I5,2X,F10.6,2X,E14.7,2X,E14.7,2X,E14.7, &
      E14.7,2X,I5,2X,I6,2X,I6)
      CLOSE (UNIT=40)
!-----------------------------------
!     write javathings/dominofiles
!-----------------------------------
      OPEN (UNIT=40,FILE='javathings'//dir//'dominofiles')
!
      j=dat
      line=filename(j)(1:fnl(j))//ext(j)
      path=wpath
      CALL LABLA(line,I1)
      WRITE (UNIT=40,FMT='(A)') line(1:i1)
!
      j=log
      line=filename(j)(1:fnl(j))//ext(j)
      CALL LABLA(line,I1)
      WRITE (UNIT=40,FMT='(A)') line(1:i1)
!
      j=out
      line=filename(j)(1:fnl(j))//ext(j)
      CALL LABLA(line,I1)
      WRITE (UNIT=40,FMT='(A)') line(1:i1)
!
      j=plt
      line=filename(j)(1:fnl(j))//ext(j)
      CALL LABLA(line,I1)
      WRITE (UNIT=40,FMT='(A)') line(1:i1)
!
      j=cln
      line=filename(j)(1:fnl(j))//ext(j)
      CALL LABLA(line,I1)
      WRITE (UNIT=40,FMT='(A)') line(1:i1)
!
      j=pst
      line=filename(j)(1:fnl(j))//ext(j)
      CALL LABLA(line,I1)
      WRITE (UNIT=40,FMT='(A)') line(1:i1)
!
      j=fig
      line=filename(j)(1:fnl(j))//ext(j)
      CALL LABLA(line,I1)
      WRITE (UNIT=40,FMT='(A)') line(1:i1)
!
      j=job
      line=filename(j)(1:fnl(j))//ext(j)
      CALL LABLA(line,I1)
      WRITE (UNIT=40,FMT='(A)') line(1:i1)
!
      CLOSE (UNIT=40)
!*****
      ierr=0
      call initialize('$THERIAK-FILES',ierr)
      if(ierr.ne.0) STOP
!*****
!-----------------------------------
!     write javathings/theriakfiles
!-----------------------------------
      OPEN (UNIT=40,FILE='javathings'//dir//'theriakfiles')
!
      j=dat
      line=filename(j)(1:fnl(j))//ext(j)
      path=wpath
      CALL LABLA(line,I1)
      WRITE (UNIT=40,FMT='(A)') line(1:i1)
!
      j=log
      line=filename(j)(1:fnl(j))//ext(j)
      CALL LABLA(line,I1)
      WRITE (UNIT=40,FMT='(A)') line(1:i1)
!
      j=out
      line=filename(j)(1:fnl(j))//ext(j)
      CALL LABLA(line,I1)
      WRITE (UNIT=40,FMT='(A)') line(1:i1)
!
      j=bin
      line=filename(j)(1:fnl(j))//ext(j)
      CALL LABLA(line,I1)
      WRITE (UNIT=40,FMT='(A)') line(1:i1)
!
      j=tab
      line=filename(j)(1:fnl(j))//ext(j)
      CALL LABLA(line,I1)
      WRITE (UNIT=40,FMT='(A)') line(1:i1)
!
      j=inf
      line=filename(j)(1:fnl(j))//ext(j)
      CALL LABLA(line,I1)
      WRITE (UNIT=40,FMT='(A)') line(1:i1)
!
      CLOSE (UNIT=40)
!*****
      ierr=0
      call initialize('$MAKEMAP-FILES',ierr)
      if(ierr.ne.0) STOP
!*****
!-----------------------------------
!     write javathings/makemapfiles
!-----------------------------------
      OPEN (UNIT=40,FILE='javathings'//dir//'makemapfiles')
!
      j=log
      line=filename(j)(1:fnl(j))//ext(j)
      path=wpath
      CALL LABLA(line,I1)
      WRITE (UNIT=40,FMT='(A)') line(1:i1)
!
      j=out
      line=filename(j)(1:fnl(j))//ext(j)
      path=wpath
      CALL LABLA(line,I1)
      WRITE (UNIT=40,FMT='(A)') line(1:i1)
!
      j=pst
      line=filename(j)(1:fnl(j))//ext(j)
      path=wpath
      CALL LABLA(line,I1)
      WRITE (UNIT=40,FMT='(A)') line(1:i1)
!
      j=pgm
      line=filename(j)(1:fnl(j))//ext(j)
      path=wpath
      CALL LABLA(line,I1)
      WRITE (UNIT=40,FMT='(A)') line(1:i1)
!
      CLOSE (UNIT=40)
!*****
      ierr=0
      call initialize('$PLOTXY-FILES',ierr)
      if(ierr.ne.0) STOP
!*****
!-----------------------------------
!     write javathings/plotxyfiles
!-----------------------------------
      OPEN (UNIT=40,FILE='javathings'//dir//'plotxyfiles')
!
      j=log
      line=filename(j)(1:fnl(j))//ext(j)
      path=wpath
      CALL LABLA(line,I1)
      WRITE (UNIT=40,FMT='(A)') line(1:i1)
!
      j=out
      line=filename(j)(1:fnl(j))//ext(j)
      path=wpath
      CALL LABLA(line,I1)
      WRITE (UNIT=40,FMT='(A)') line(1:i1)
!
      j=plt
      line=filename(j)(1:fnl(j))//ext(j)
      path=wpath
      CALL LABLA(line,I1)
      WRITE (UNIT=40,FMT='(A)') line(1:i1)
!
      j=inf
      line=filename(j)(1:fnl(j))//ext(j)
      path=wpath
      CALL LABLA(line,I1)
      WRITE (UNIT=40,FMT='(A)') line(1:i1)
!
      CLOSE (UNIT=40)
!*****
      ierr=0
      call initialize('$THERBIN-FILES',ierr)
      if(ierr.ne.0) STOP
!*****
!-----------------------------------
!     write javathings/therbinfiles
!-----------------------------------
      OPEN (UNIT=40,FILE='javathings'//dir//'therbinfiles')
!
      j=dat
      line=filename(j)(1:fnl(j))//ext(j)
      path=wpath
      CALL LABLA(line,I1)
      WRITE (UNIT=40,FMT='(A)') line(1:i1)
!
      j=log
      line=filename(j)(1:fnl(j))//ext(j)
      path=wpath
      CALL LABLA(line,I1)
      WRITE (UNIT=40,FMT='(A)') line(1:i1)
!
      j=out
      line=filename(j)(1:fnl(j))//ext(j)
      path=wpath
      CALL LABLA(line,I1)
      WRITE (UNIT=40,FMT='(A)') line(1:i1)
!
      j=plt
      line=filename(j)(1:fnl(j))//ext(j)
      path=wpath
      CALL LABLA(line,I1)
      WRITE (UNIT=40,FMT='(A)') line(1:i1)
!
      CLOSE (UNIT=40)
!*****
      ierr=0
      call initialize('$THERTER-FILES',ierr)
      if(ierr.ne.0) STOP
!*****
!-----------------------------------
!     write javathings/therterfiles
!-----------------------------------
      OPEN (UNIT=40,FILE='javathings'//dir//'therterfiles')
!
      j=dat
      line=filename(j)(1:fnl(j))//ext(j)
      path=wpath
      CALL LABLA(line,I1)
      WRITE (UNIT=40,FMT='(A)') line(1:i1)
!
      j=log
      line=filename(j)(1:fnl(j))//ext(j)
      path=wpath
      CALL LABLA(line,I1)
      WRITE (UNIT=40,FMT='(A)') line(1:i1)
!
      j=out
      line=filename(j)(1:fnl(j))//ext(j)
      path=wpath
      CALL LABLA(line,I1)
      WRITE (UNIT=40,FMT='(A)') line(1:i1)
!
      j=plt
      line=filename(j)(1:fnl(j))//ext(j)
      path=wpath
      CALL LABLA(line,I1)
      WRITE (UNIT=40,FMT='(A)') line(1:i1)
!
      CLOSE (UNIT=40)
!*****
      ierr=0
      call initialize('$GUZZLER-FILES',ierr)
      if(ierr.ne.0) STOP
!*****
!-----------------------------------
!     write javathings/guzzlerfiles
!-----------------------------------
      OPEN (UNIT=40,FILE='javathings'//dir//'guzzlerfiles')
!
      j=log
      line=filename(j)(1:fnl(j))//ext(j)
      path=wpath
      CALL LABLA(line,I1)
      WRITE (UNIT=40,FMT='(A)') line(1:i1)
!
      j=cln
      line=filename(j)(1:fnl(j))//ext(j)
      path=wpath
      CALL LABLA(line,I1)
      WRITE (UNIT=40,FMT='(A)') line(1:i1)
!
      j=rxn
      line=filename(j)(1:fnl(j))//ext(j)
      path=wpath
      CALL LABLA(line,I1)
      WRITE (UNIT=40,FMT='(A)') line(1:i1)
!
      j=grd
      line=filename(j)(1:fnl(j))//ext(j)
      path=wpath
      CALL LABLA(line,I1)
      WRITE (UNIT=40,FMT='(A)') line(1:i1)
!
      CLOSE (UNIT=40)
!*****
      ierr=0
      call initialize('$EXPLOT-FILES',ierr)
      if(ierr.ne.0) STOP
!*****
!-----------------------------------
!     write javathings/explotfiles
!-----------------------------------
      OPEN (UNIT=40,FILE='javathings'//dir//'explotfiles')
!
      j=log
      line=filename(j)(1:fnl(j))//ext(j)
      path=wpath
      CALL LABLA(line,I1)
      WRITE (UNIT=40,FMT='(A)') line(1:i1)
!
      j=pst
      line=filename(j)(1:fnl(j))//ext(j)
      path=wpath
      CALL LABLA(line,I1)
      WRITE (UNIT=40,FMT='(A)') line(1:i1)
!
      CLOSE (UNIT=40)
!
!
      END
!-----
