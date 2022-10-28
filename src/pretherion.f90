!-----Version: 28.05.2022
!               **************
!               * PRETHERION *
!               **************
!
!      vers='28.05.2022'
!     ------------------------------------------------------------------
!     ------------------------------------------------------------------
      IMPLICIT NONE
      CHARACTER(200) CH001,CHIN(2)
      CHARACTER(50) DB(20),MACHS(20),MAKES(20)
      INTEGER(4) NDB,I1,I
!!      REAL(8) X
!
!-----
      DO I=1,2
        CHIN(I)=' '
      END DO
      OPEN (UNIT=10,FILE='lastpret',STATUS='UNKNOWN')
      DO I=1,2
        READ (UNIT=10,FMT='(A200)',END=15) CHIN(I)
      END DO
   15 CONTINUE
      CLOSE (UNIT=10)

!
      NDB=0
      CALL DIRLIST
!      CALL SYSTEM ('ls > zzz')
!      OPEN (UNIT=10,FILE='zzz',STATUS='UNKNOWN')
    1 READ (UNIT=99,FMT='(A)',END=888) CH001
      IF (CH001(1:6).EQ.'trans_') THEN
        CALL LABLA(CH001,I1)
        NDB=NDB+1
        DB(NDB)=CH001(7:I1)

        write (unit=6,fmt='(''DB= '',I4,2X,A)') NDB,CH001(7:I1)

      END IF
      GOTO 1
  888 CONTINUE
      CLOSE (UNIT=99)

      DO I=1,NDB
        CALL LABLA(DB(I),I1)
        WRITE (UNIT=6,FMT='(I3,2X,A)') I,DB(I)(1:I1)
        MACHS(I)='mach'//DB(I)(1:I1)
        MAKES(I)='make'//DB(I)(1:I1)
      END DO

      CALL LABLA(CHIN(1),I1)
      IF (I1.EQ.0) I1=1
      CH001=' experimental database: <'//CHIN(1)(1:I1)//'>?'
      CALL PUST(6,CH001)
      READ (5,FMT='(A200)') CH001
      IF (CH001.EQ.' ') THEN
        CH001=CHIN(1)
      ELSE
        CHIN(1)=CH001
      END IF

      CALL LABLA(CHIN(2),I1)
      IF (I1.EQ.0) I1=1
      CH001=' Enter [ CR | all | sys | pha | exp ] <'//CHIN(2)(1:I1)//'>?'
      CALL PUST (6,CH001)
      READ (5,FMT='(A200)') CH001
!---
      IF (CH001.EQ.' ') THEN
        CH001=CHIN(2)
      ELSE
        CHIN(2)=CH001
      END IF
!------------------
!-----store terminal input
      OPEN (UNIT=10,FILE='lastpret',STATUS='UNKNOWN')
      DO I=1,2
        WRITE (UNIT=10,FMT='(A200)') CHIN(I)
      END DO
      CLOSE (UNIT=10)
!     ------------------------------------------------------------------
      DO I=1,NDB
        CALL LABLA(MAKES(I),I1)
        OPEN (UNIT=10,FILE=MAKES(I)(1:I1),STATUS='UNKNOWN')
        WRITE (UNIT=6,FMT='(A)') MAKES(I)(1:I1)
        CALL PUST(10,DB(I))
        CALL PUST(10,CHIN(1))
        CALL PUST(10,CHIN(2))
        CALL PUST(6,'   '//DB(I))
        CALL PUST(6,'   '//CHIN(1))
        CALL PUST(6,'   '//CHIN(2))
        CLOSE(UNIT=10)
      END DO
!!
      OPEN (UNIT=12,FILE='machall',STATUS='UNKNOWN')
      DO I=1,NDB
        CALL LABLA(MACHS(I),I1)
        OPEN (UNIT=10,FILE=MACHS(I)(1:I1),STATUS='UNKNOWN')
        WRITE (UNIT=6,FMT='(A)') MACHS(I)(1:I1)
        CALL PUST(10,'therion   '//MAKES(I))
        CALL PUST(12,'therion   '//MAKES(I))
        CALL PUST(6,'   therion   '//MAKES(I))
        CLOSE(UNIT=10)
      END DO
      CLOSE(UNIT=12)
      WRITE (UNIT=6,FMT='(''machall'')')
      DO I=1,NDB
        CALL PUST(6,'   therion   '//MAKES(I))
      END DO

      DO I=1,NDB
        CALL LABLA(MACHS(I),I1)
        CALL MAKEEXEC(MACHS(I))
!!        CALL SYSTEM('chmod  a+x  '//MACHS(I)(1:I1))
      END DO
      CALL MAKEEXEC('machall')
!!      CALL SYSTEM('chmod  a+x  machall')



!
      END
!-----
!******************************
!
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
!-----
!******************************
      SUBROUTINE PUST(I001,CH)
      IMPLICIT NONE
      CHARACTER*(*) CH
      INTEGER(4) I001,II
      CALL LABLA(CH,II)
      IF (II.EQ.0) II=1
      WRITE (UNIT=I001,FMT='(A)') CH(1:II)
      RETURN
      END
!-----
!******************************
