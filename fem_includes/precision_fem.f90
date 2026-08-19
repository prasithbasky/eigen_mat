!
!
      module precision_fem
!
!........1.........2.........3.........4.........5.........6.........7.........8.........9........10
!
!        Definition of 
!        
!        - iso_fortran_env
!        - define double precision for all real variables
!        - set "implicit none" for all variables
!       
!
!        use, intrinsic :: iso_fortran_env, only : real64
!
        use, intrinsic :: iso_fortran_env, only : real32, real64, int32, int64
!
        implicit none
!
           integer, parameter :: sp = real32
           integer, parameter :: dp = real64
!
!........1.........2.........3.........4.........5.........6.........7.........8.........9........10
!
!
      end module precision_fem 
