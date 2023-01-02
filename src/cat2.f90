!cat2.f90  Version: 2023.01.02
! -----------------------------------------------------------------------
! Copyright (C) 1984-2022  Christian de Capitani
!
! This program is free software: you can redistribute it and/or modify
! it under the terms of the GNU General Public License as published by
! the Free Software Foundation, either version 3 of the License, or
! (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program.  If not, see <https://www.gnu.org/licenses/>.
! -----------------------------------------------------------------------
!
!               ********
!               * CAT2 *
!               ********
!
!      vers='28.05.2022'
      IMPLICIT NONE
!*****
!
!*****
      INTEGER(4) I,LARG,IERR,I1
      CHARACTER(500) CH001
!-----
      CHARACTER(200) LARGUM(3)
!-----
      WRITE (UNIT=6,FMT='(''running cat2'')')
      LARG=0
      IERR=0
      DO I=1,3
      CALL GetLineArgs (I,LARGUM(I),IERR)
      IF(IERR.NE.0.OR.LARGUM(I).EQ.' ')THEN
      GOTO 399
      ELSE
      LARG=LARG+1
      END IF
      END DO
  399 CONTINUE

      IF (LARG.NE.3) THEN
       WRITE (UNIT=6,FMT='(''cat2: < 3 arguments'')')
       GOTO 999
      END IF
!
       DO I=1,LARG
        CALL PUST(6,LARGUM(I))
       END DO
!
!----- open output file
       CALL LABLA(LARGUM(3),I1)
       OPEN (UNIT=23,FILE=LARGUM(3)(1:I1),STATUS='UNKNOWN')
!
!----- open first file
       CALL LABLA(LARGUM(1),I1)
       OPEN (UNIT=21,FILE=LARGUM(1)(1:I1),STATUS='OLD')
    1 READ (UNIT=21,FMT='(A500)',END=10) CH001
       CALL PUST(23,CH001)
       GOTO 1
    10 CLOSE (UNIT=21)
!
!----- open second file
       CALL LABLA(LARGUM(2),I1)
       OPEN (UNIT=22,FILE=LARGUM(2)(1:I1),STATUS='OLD')
    2 READ (UNIT=22,FMT='(A500)',END=20) CH001
       CALL PUST(23,CH001)
       GOTO 2
    20 CLOSE (UNIT=22)
!
       CLOSE (UNIT=23)
!-----
!*****
  999 CONTINUE
      END

