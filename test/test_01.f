      program test_01
!
!     Test to check the iso_fortran_env 
!     Preparation of modules
!
!
        use, intrinsic :: iso_fortran_env, only : real64
        implicit none
!
        real(real64) :: pi
!
        pi = 3.141592653589793_real64
!
        print *, pi
		pause
      end program test_01
