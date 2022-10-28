!-----Version: 28.05.2022
!
!**********************************************************************
!-----
      subroutine helpme (tag)
      implicit none
      include 'files.cmn'
      character*(*) tag
      integer ierr, j, i
!
      call LastChar(tag,i)
      if(clear.eq.1.and.tag(i:i).ne.'*') call clearscreen
!10    continue
!
      j=hlp
      line=filename(j)(1:fnl(j))//ext(j)
      path=tpath
      akzess=' '
      state='old'
      ierr=0
      call openfile(j,ierr)
      if(ierr.ne.0) return
!
30      continue
        read(hlp,35,end=150) line
35      format(a)
        if(line.ne.tag) goto 30
        write(*,35)
50      continue
        read(hlp,35,end=150) line
        if(line(1:1).eq.'§')then
            write(*,35)
            write(*,35) ' Press the RETURN-key to continue!'
            read(*,*)
            if(clear.eq.1.and.tag(i:i).ne.'*') call clearscreen
        else if(line(1:4).eq.'$END') then
            write(*,35)
            goto 200
        else
            if (tag(i:I).eq.'*') then
            write(*,35) ' '//line(1:90)
            else
            write(*,35) ' (i) '//line(1:90)
            end if
        end if
        goto 50
150     continue
        write(*,155) tag
155     format(' Help for',1x,a,1x,'does not exist!')
200     continue
        close (hlp)
        return
        end
!
!***************************************************************
!
      subroutine writetit (iu)
      implicit none
      include "files.cmn"
!
      integer i, j, k, iu
!
      call LASTCHAR(progname,i)
      call LASTCHAR(vers,j)
      call LASTCHAR(os,k)
!
      if(iu.eq.scr) call clearscreen
      WRITE (iu,1000) progname(1:i),vers(1:j),os(1:k)
      j=i+j+k
1000  FORMAT (/, &
      ' Program',1x,a,', Version (dd.mm.yy)',1x,a,1x,'(',a,')')
      write(iu,2) ('=',i=1,32+j)
2     format(1x,130a1)
      call LASTCHAR(task,i)
      write(iu,101) task(1:i)
101   format(/,1x,a,///, &
      ' Written by:', &
      /,11x,'Christian de Capitani (Basel, Switzerland)', &
      /,11x,'E-mail: christian.decapitani@unibas.ch', &
      //,' Input dialogue and help by:', &
      /,11x,'Konstantin Petrakakis (Vienna, Austria)', &
      /,11x,'E-mail: Konstantin.Petrakakis@univie.ac.at'/)
      write(iu,2) ('=',j=1,80)
      if(iu.eq.6.and.ghelp.eq.1) then
          i=clear
          if(clear.eq.1) clear=0
          call helpme('$GENERAL')
          clear=i
      end if
      RETURN
      END
!
!**********************************************************************
!
        SUBROUTINE initialize(tag, ierr)
        IMPLICIT NONE
        INCLUDE "files.cmn"
!-----  Local variables
        character*(*) tag
!*****
        CHARACTER(500) inipath
        LOGICAL(4) IPR
        integer  i, j, k, ierr, isoname,KPR,CHECKDAT
!-----  End of local variables
        IPR=.TRUE.
        KPR=ierr
        IF (ierr.EQ.9) IPR=.FALSE.
!-----  first find working- and program-directory
!
        CALL GetEnvVar(tpath, itpath, ierr, dir, ext(job), os)
        ierr=KPR
        CALL getwork(wpath, iwpath, ierr, dir)
!-----
!###    Capi: the following statement moved towards the end of the subroutine
!       call writetit(scr)
!------------------
!       Open UNIT=ini (= theriak.ini)
!       first in wpath. if not found in tpath
!------------------
        j=ini
        line=filename(j)(1:fnl(j))//ext(j)
        path=wpath
        akzess=' '
        state='old'
        CALL openfile(j,ierr)
        IF (ierr.ne.0) then
           path=tpath
           CALL openfile(j,ierr)
           IF (ierr.ne.0) then
              WRITE(*,9) filename(j)(1:fnl(j))//ext(j)
9             FORMAT(/,' Initialization file (',a,') not found.')
              RETURN
           END IF
        END IF
        inipath=path
!
!****  Start reading theriak.ini
!
10      CONTINUE
        READ (ini,12,END=50) line
12      format(a)
        if(clear.eq.-1.and.line(1:7).eq.'$Clear=') &
        read(line(8:8),*) clear
        if(ghelp.eq.-1.and.line(1:7).eq.'$GHELP=') &
        read(line(8:8),*) ghelp
        if(line(1:7).eq.'$BATCH=') &
        read(line(8:8),*) batch
        IF (line(1:12).EQ.'$CALC-PARAMS') CALL CALCPAR
!
!---- changes by Capi: check if dat-file is mentioned in theriak.ini
      CHECKDAT=0
!
        if(line.eq.tag) then
           isoname=0
15         continue
           read(ini,12,end=50) line
           if(line(1:1).eq.'!'.or.line.eq.' ') goto 15
           if(line.eq.'$END') goto 50
!
           do 20 i=ifix+1,nrunits
              if(line(1:3).eq.filetype(i)) then
      IF (filetype(i).EQ.'dat') CHECKDAT=1
                 k=len(line)
                 j=index(line,'#')-1
                 if(j.le.0) j=k
                 read(line(5:j),12) filename(i)
                 if(i.eq.dat.and.filename(i)(1:1).eq.'?'.or. &
                 filename(i).eq.' ') then
                    isoname=1
                    write(*,17) filetype(i), &
                    filename(ini)(1:fnl(ini))//ext(ini)
17                  format(/,' Filename for file type ',a, &
                    ' not defined in ',a,/,'Enter file name: ')
                    read(*,12) filename(i)
                    if (filename(i).eq.' ') then
                       write(*,*) ' Not accepted!'
                       stop
                    else
                       j=index(filename(i),'.')
                       if(j.gt.0) then
                          ext(i)=filename(i)(j:)
                          filename(i)=filename(i)(1:j-1)
                          fnl(i)=j-1
                       else
                          call LASTCHAR(filename(i),fnl(i))
                       end if
                    end if
                    goto 15
                 else if (i.eq.dbs.or.i.eq.drv) then
                    filename(i)='Run-time defined'
                    fnl(i)=16
                    goto 15
                 else
!
!***             In case isoname is not 0,
!                see if the filename is given as Filename.Ext, i.e., if a period is given.
!                In this case separate Filename from Ext. Ext will anyway be truncated to 4
!                characters including the period.
!
                    if(isoname.eq.0) then
                      j=0
                      j=index(filename(i),'.')
                      if(j.gt.0) then
                         ext(i)=filename(i)(j:)
                         filename(i)=filename(i)(1:j-1)
                         fnl(i)=j-1
                      else
                         call LASTCHAR(filename(i),fnl(i))
                         ext(i)=' '
                      end if
                    else
                      filename(i)=filename(dat)(1:fnl(dat))//'-'
                      fnl(i)=fnl(dat)+1
                      ext(i)=filetype(i)//'.txt'
                    end if
                    goto 15
                 end if
              end if
20         continue
        else
           goto 10
        end if
!
50      continue
!
!****   code for testing
!        write(*,12) tag
!        write(*,*) ' isoname= ',isoname
!        do i=1,nrunits
!           write(*,52)
!     &     i, filetype(i),  filename(i)(1:fnl(i))//ext(i), descr(i)
!52         format(1x,i2,2x,a,2x,a,2x,a)
!        end do
!        pause
!****   end of testing code
!
!-----  if "(dat)" is in working directory -> everything OK
!-----  else set wpath=tpath (working-dir = program-dir)
!****** This should take place when progname is not guzzler or explot
!******
        j=index(progname,'GUZZ')
        if(j.eq.0) j=index(progname,'EXPL')
!        if(j.eq.0) j=index(progname,'MAP')
        if(j.ne.0) goto 70
!------------------
!       Open UNIT=dat (= THERIN)
!------------------
!---- changes by Capi
!---- if dat-file not in theriak.ini, goto 70
      IF (CHECKDAT.EQ.0) GOTO 70
        j=dat
        line=filename(j)(1:fnl(j))//ext(j)
        path=wpath
        akzess=' '
        state='old'
        CALL openfile(j,ierr)
        IF (ierr.ne.0) then
           IF (IPR) WRITE (*,28) tpath
28         FORMAT (/,' Working-directory is set to:',1x,a)
           path=tpath
           wpath=tpath
           iwpath=itpath
           CALL openfile(j,ierr)
           IF (ierr.ne.0) then
              WRITE(*,29) filename(j)
29            FORMAT(/,' Can''t find',1X,A)
              RETURN
           ELSE
              CLOSE (UNIT=dat)
           END IF
        ELSE
        CLOSE (UNIT=dat)
        END IF
!=====
70      continue
      IF (IPR) THEN
        call writetit(scr)
        CALL LASTCHAR(inipath,j)
        write(*,*)
        write(*,2) ('-',i=1,14)
        write(*,12) ' initialization'
        write(*,2) ('-',i=1,14)
2       format(1x,80a1,:)
3       format(1x,a,1x,a)
        write(*,3) 'Initialization-file:  ',  inipath(1:j)
        write(*,3) 'Program''s directory:  ', tpath(1:itpath)
        write(*,3) 'Working directory:    ',  wpath(1:iwpath)
        write(*,*)
        call getdate(sdate)
        write(*,12) ' Run: '//sdate
        write(*,*)
      END IF
!=====
!****   code for testing
!        write(*,12) tag
!        write(*,*) ' isoname= ', isoname
!        do i=1,nrunits
!           write(*,53)
!     &     i, filetype(i),  filename(i)(1:fnl(i))//ext(i), descr(i)
!53         format(1x,i2,2x,a,2x,a,2x,a)
!        end do
!        pause
!****   end of code testing
!
        close (ini)
        return
        end
!c
!*******************************************************************
!
        subroutine listfiles
!
!       This subroutine is called when the keyword "files" is input.
!       It lists the names, types and descriptions of files associated
!       with the running program.
!
        IMPLICIT NONE
!
        include "files.cmn"
!
        integer i, j
!
        call clearscreen
        call LASTCHAR(progname,i)
        write(*,12) progname(1:i)
12      format(/,1x,a,' file names defined in "theriak.ini"')
        write(*,13)  ('-',j=1,36+i)
13      format(1x,120a1,:)
        write(*,14)
14      format(/,' I/O', 3x,'Type',2x, 'File name',/,20x, &
        'Description',/,80('-'))
!        j=index(progname,'GUZZ')
!        if(j.eq.0) j=index(progname,'EXPL')
!        if(j.eq.0) j=index(progname,'MAKE')
!        if(j.ne.0) then
!           j=ifix+1
!        else
!           j=1
!        end if
!
!---- changed by Capi: always write 1,2,5,6
!----                  3,4 only for domino
!----                  others only if defined in theriak.ini
!
!        do 20 i=j,nrunits
!
      j=INDEX(progname,'DOMINO')
        do 20 i=1,nrunits
        IF ((i.EQ.3.OR.i.EQ.4).AND.j.EQ.0) GOTO 20
           line=filename(i)(1:fnl(i))//ext(i)
!dc           if(filename(i).ne.' '.and.i.le.scr) then
           if(filename(i).ne.' '.and. &
      (i.eq.1.or.i.eq.2.or.i.eq.5.or.i.eq.6)) then
               write(*,52) i, '*', filetype(i),  line(1:fnl(i)+10), &
               descr(i)
           else
!dc               if(i.eq.scr+1) write(*,*)
               if(filename(i).ne.' ') &
               write(*,52) i, ' ', filetype(i),  line(1:fnl(i)+10), &
               descr(i)
           end if
52         format(2x,i2.2,        2x,a,a,         3x,a,    /,20x,a)
20      continue
        write(*,*)
        if(j.lt.ifix+1) write(*,*) &
        ' *: Hard-coded file names (cannot be changed)!'
        write(*,'(1x,80(''-''))')
        write(*,*)
        return
        end
!
!******************************************************************
!
        subroutine openfile (iu,ierr)
        implicit none
        include 'files.cmn'
        integer iu,ipath,ierr
!
!5       continue
        ierr=0
        if(iu.lt.1.or.iu.gt.99) then
            ierr=1
            goto 100
        end if
        if(akzess.eq.' ') akzess='sequential'
        if(line.eq.' ') then
            write(*,20) filetype(iu)
20          format(' Enter filename of type ',a,' ?')
            read(*,22) line
22          format(a)
!            if(line.eq.' ') ierr=1
        end if
        if(ierr.ne.0) goto 100
        if(path.ne.' ') then
            call LASTCHAR(path,ipath)
            path=path(1:ipath)//line
        else
            path=line
        end if
        call LASTCHAR(path,ipath)
        if(state.eq.' ') state='unknown'


      IF (akzess.EQ.'append') THEN
        open(unit=iu,file=path(1:ipath),status=state,err=50, &
        position=akzess,iostat=ierr)
      ELSE
        open(unit=iu,file=path(1:ipath),status=state,err=50, &
        access=akzess,iostat=ierr)
      END IF



50      continue
        if(ierr.ne.0) goto 100
        return
100     continue
        IF (iu.EQ.ini) RETURN
        write(scr,105) iu, filetype(iu), path(1:ipath), descr(iu),ierr
105     format(/,' Unable to open unit ',i2,', type',1x,a,', file', &
        1x,a,/,' containing:',1x,a,2x,/,' iostat:',I6)
        return
        end
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
!
!*********************************************************************
!
        BLOCK DATA INITHER
!
        include "files.cmn"
        include "thblock.cmn"
!
        end
