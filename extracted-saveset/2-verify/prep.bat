!
!PREP.BAT	4/18/76
!
!BATCH STREAM TO PREPARE FORTRAN.EXE FILES
! FOR THE CUSP VERIFICATION PACKAGE.
!
!
!
!THE PACKAGE RUNS A FORTRAN OBJECT PROGRAM, BUT IT CANNOT ASSUME
! THE EXISTENCE OF FORTRAN ON THE SYSTEM.  HENCE,
! THE FORTRAN PROGRAM MUST BE LINKED WITH THE OBJECT TIME
! SYSTEM ALREADY IN IT.
@DELETE VERCMP.REL,VERCMP.EXE
@COMPILE VERCMP.FOR
@LINK
*VERCMP/OTS:NONSHAR/G
@SAVE VERCMP

 