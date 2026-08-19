!
!
      module matrix_fem
!
!........1.........2.........3.........4.........5.........6.........7.........8.........9........10
!
!        Definition of 
!        
!        - storage for matrix DMAT (system matrix)
!        - storage for matrix M (mass matrix)
!        - storage for matrix K (stiffness matrix)
!       
!
        use precision_fem
!
        implicit none
!
           real (dp), allocatable :: DMAT(:,:)
           real (dp), allocatable :: M(:,:)
		   real (dp), allocatable :: K(:,:)
!
!........1.........2.........3.........4.........5.........6.........7.........8.........9........10
!
!
      end module matrix_fem 
