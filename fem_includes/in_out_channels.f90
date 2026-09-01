!
!
      module in_out_channels
!
!........1.........2.........3.........4.........5.........6.........7.........8.........9........10
!
!        Definition of 
!        
!        - Input Channels (will be added)
!        - Output Channels will replace the common block in the helproutines
!        - iunit_out_1 = 101, output of eigenvalues and eigenvectors to file
!                             for mistst.f, name = mistst.out
!          iunit_out_2 = 102, output of eigenvalues and eigenvectors to file
!                             for eigtst.f, name = eigtst.out
!          iunit_out_3 = 103, output of eigenvalues and eigenvectors to file
!                             for realeig_2.f, name = realeig_2.out
!          iunit_out_4 = 104, output of eigenvalues and eigenvectors to file
!                             for realeig_3.f, name = realeig_3.out
!          iunit_out_5 = 105, output of eigenvalues and eigenvectors to file
!                             for realeig_4.f, name = realeig_4.out
!          iunit_out_6 = 106, output of eigenvalues and eigenvectors to file
!                             for realeig_5.f, name = realeig_5.out
!       
!
        use precision_fem
!
        implicit none
!
!          real (dp), allocatable :: DMAT(:,:)
           integer                :: iunit_out_1 = 101
           integer                :: iunit_out_2 = 102		
           integer                :: iunit_out_3 = 103
           integer                :: iunit_out_4 = 104		
           integer                :: iunit_out_5 = 105
           integer                :: iunit_out_6 = 106				   
!
!
! 
!........1.........2.........3.........4.........5.........6.........7.........8.........9........10
!
!
      end module in_out_channels
