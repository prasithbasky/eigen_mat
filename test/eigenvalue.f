!
!
!
!                                              
!........1.........2.........3.........4.........5.........6.........7.........8.........9........10
!*****************************************************************************
!                                                                            *
! EIGENVALUE                                                     2026/05 /02 *
! ==========                                                                 *
!                                                                            *
!     Test (Main) Program for all methods                                    *
!           - MISTST    von Mises method, details see in SUBROUTINE          *
!                       header description                                   *
!                                                                            *
!           - EIGTST    MARTIN,PARLETT,PETERS,REINSCH UND WILKINS            *
!                       details see in SUBROUTINE                            *
!                                                                            *
!           - REALEIG_2 LAPACK Subroutine  DGEEV                             *
!                       details see in SUBROUTINE header description         *
!                                                                            *
!           - REALEIG_3 ARPACK Subroutines DNAUPD and DNEUPD                 *
!                       details see in SUBROUTINE header description         *
!                                                                            *
!           - REALEIG_4 ARPACK Subroutines DSAUPD and DSEUPD                 *
!                       details see in SUBROUTINE header description         *
!                                                                            *
!           - REALEIG_x ... open for further methods                         *
!                                                                            *
!           The program is allocating the storage for all matrixes and       *
!           reads the input data for the eigenvalue and Eigenvector          *
!           calculation.                                                     *
!                                                                            *
!           The date type and precision declaration is handled by            *
!           the modules                                                      *
!                                                                            *     
!           - precision.mod                                                  *
!           - dimension.mod                                                  *     
!           -                                                                *
!                                                                            *                
!                                                      Axel Hänschke         *
!                                                                            *
!                                              Update: July, 16. 2026        *
!                                                                            *
!*****************************************************************************
!
      PROGRAM EIGENVALUE
!
! 
!     ISO Fortran Standard and precision definition
! 
      use precision_fem
!
      use matrix_fem
!
      implicit none
!
! *** INTEGER
!
      INTEGER                          I 
	  INTEGER                          J 
	  INTEGER                          IERR
	  INTEGER                          IERRALLOC
	  INTEGER                          IERRIN
      INTEGER                          KTR	
!
      INTEGER                          IUNIT
      INTEGER                          iline
      INTEGER                          icolumn	  
!
! *** CHARACTER
!
	  CHARACTER                        CHSELECT*1
	  CHARACTER                        CHFILE*1
!
      IERR        = 0
	  IERRALLOC   = 0
	  IERRIN      = 0
!
!........1.........2.........3.........4.........5.........6.........7.........8.........9........10
! 
! *** HELP Initialisierung 
! 
      CALL HEDEF ( IERR )	  
!
! ... Initialisierung der Help-Umgebung
!
      CALL HEINIT ( IERR )
      IF ( IERR .EQ. 1 ) THEN 
         WRITE (*, 5000)
         GOTO 9998
      ENDIF
!
! *** Selection of method.
!
 1000 CONTINUE
!
      CALL HECLEAR ( IERR )
!
!........1.........2.........3.........4.........5.........6.........7.........8.........9........10
!
	  WRITE (*,*) ' '
	  WRITE (*,*) ' '
	  WRITE (*,*) '+----------------------------------------------------------+'
	  WRITE (*,*) '|                                                          |'
	  WRITE (*,*) '|   Program: EIGENVALUE                                    |'
	  WRITE (*,*) '|   ===================                                    |'
	  WRITE (*,*) '|                                                          |'
	  WRITE (*,*) '|   Test (Main) Program for several methods                |'
      WRITE (*,*) '|                                                          |'
      WRITE (*,*) '|                                                          |'
      WRITE (*,*) '|                                                          |'
	  WRITE (*,*) '|                    Dr.-Ing. Axel Haenschke               |'
      WRITE (*,*) '|                    CPS-Consulting                        |'
      WRITE (*,*) '|                    50321 Bruehl / Germany                |'
      WRITE (*,*) '|                                                          |'
      WRITE (*,*) '|                    Prasith Basky Murugaperumal           |'
      WRITE (*,*) '|                    Msc. COMMAS                           |'
      WRITE (*,*) '|                    University Stuttgart                  |'
      WRITE (*,*) '|                                                          |'
      WRITE (*,*) '|                                                          |'
      WRITE (*,*) '+----------------------------------------------------------+'	  
!
	  WRITE (*,*) ' '
	  WRITE (*,*) ' '
!
      call heenter
!
! *** Selection of method.
!
 2201 CONTINUE
!
      CALL HECLEAR ( IERR )
!
!	  WRITE (*,*) ' '
!	  WRITE (*,*) ' '
	  WRITE (*,*) '+-----------------------------------------------------------+'	  
!	  WRITE (*,*) '                                                            '
	  WRITE (*,*) '                                                            '	  
      WRITE (*,*) '  Program to select the Eigenvalue and Eigenvector          '
	  WRITE (*,*) '  calculation method.                                       '
	  WRITE (*,*) '                                                            '
      WRITE (*,*) '  Please select:                                            '
      WRITE (*,*) '   a) Program mistst      Von Mises method                  '
	  WRITE (*,*) '                          one eigenvalue/vector - general   '
      WRITE (*,*) '                          quadratic non symmetric matrix    '
	  WRITE (*,*) '                                                            '
      WRITE (*,*) '   b) Program eigtst      Martin, Parlett, Peters, Reinsch  '
	  WRITE (*,*) '                          and Wilkinson method              '    
	  WRITE (*,*) '                          all eigenvalues/vectors - general '
	  WRITE (*,*) '                          quadratic non symmetric matrix    '
	  WRITE (*,*) '                                                            '
      WRITE (*,*) '   c) Program realeig_2   LAPACK DGEEV                      '
	  WRITE (*,*) '                          all eigenvalues/vectors - general '
	  WRITE (*,*) '                          quadratic non symmetric matrix    '
	  WRITE (*,*) '                                                            '
      WRITE (*,*) '   d) Program realeig_3   ARPACK DNAUPD / DNEUPD            '
	  WRITE (*,*) '                          Implicitly Restarted Arnoldi      '	  
	  WRITE (*,*) '                          method (IRAM) for general         '
	  WRITE (*,*) '                          non symmetric matrix              '
	  WRITE (*,*) '                          LAPACK DGEEV for Y = A * X        '
	  WRITE (*,*) '                          (matrix-vector product)           '
	  WRITE (*,*) '                                                            '
      WRITE (*,*) '   e) Program realeig_4   ARPACK DSAUPD / DSEUPD            '
	  WRITE (*,*) '                          Implicitly Restarted Lanczos      '
	  WRITE (*,*) '                          method (IRLM) for real symmetric  '
	  WRITE (*,*) '                          LAPACK DGEEV for Y = A * X        '
	  WRITE (*,*) '                          (matrix-vector product)           '
	  WRITE (*,*) '                                                            '
      WRITE (*,*) '   f) Program realeig_5   ARPACK DSAUPD / DSEUPD            '
	  WRITE (*,*) '                          Implicitly Restarted Lanczos      '
	  WRITE (*,*) '                          method (IRLM) for real symmetric  '
	  WRITE (*,*) '                          LAPACK DGEsV for                  '
	  WRITE (*,*) '                          the OP*x = (K-σM)^(-1) M x        '
      WRITE (*,*) '                          (operator calculation)            '
	  WRITE (*,*) '                          M and K matrices required         '     
	  WRITE (*,*) '                                                            '
      WRITE (*,*) '   g) Program realeig_6   ARPACK for sparse matrix          '
	  WRITE (*,*) '                          not installed !                   '	  
      WRITE (*,*) '                                                            '
      WRITE (*,*) '                                                            '
      WRITE (*,*) '   s) Program STOP                                          '
	  WRITE (*,*) '                                                            '
	  WRITE (*,*) '+-----------------------------------------------------------+'	  
!	  WRITE (*,*) ' '
!	  WRITE (*,*) ' '	
!
      WRITE (*, 323)

  323 FORMAT ( /
!    +,/,         ' '
     +,/,         '    Select Input please   : ',$ )
!
      KTR = 5
!
      READ ( KTR, 1111 ) CHSELECT
!
! 
 1111 FORMAT (A1)
!
 1112 CONTINUE
!
      WRITE (*,*) ' '
	  WRITE (*,*) ' CHSELECT = ', CHSELECT
	  WRITE (*,*) ' '
	  call heenter
!
! *** Selection of methods
!
!........1.........2.........3.........4.........5.........6.........7.........8.........9........10 
!
	 IF ( CHSELECT(1:1) .EQ. 'A' .OR. CHSELECT(1:1) .EQ. 'a' ) THEN
         GOTO  1001
      ELSEIF ( CHSELECT(1:1) .EQ.'B' .OR. CHSELECT(1:1) .EQ. 'b' ) THEN
         GOTO  1002
      ELSEIF ( CHSELECT(1:1) .EQ.'C' .OR. CHSELECT(1:1) .EQ. 'c' ) THEN
         GOTO  1003
      ELSEIF ( CHSELECT(1:1) .EQ.'D' .OR. CHSELECT(1:1) .EQ. 'd' ) THEN
         GOTO  1004
      ELSEIF ( CHSELECT(1:1) .EQ.'E' .OR. CHSELECT(1:1) .EQ. 'e' ) THEN
         GOTO  1005
!
      ELSEIF ( CHSELECT(1:1) .EQ.'F' .OR. CHSELECT(1:1) .EQ. 'f' ) THEN
         GOTO  1006
!
      ELSEIF ( CHSELECT(1:1) .EQ.'G' .OR. CHSELECT(1:1) .EQ. 'g' ) THEN
         GOTO  1007
!
      ELSEIF ( CHSELECT(1:1) .EQ.'S' .OR. CHSELECT(1:1) .EQ. 's' ) THEN
         GOTO 9999
      ELSE
         WRITE (*,*) ' '
         WRITE (*,*) ' Wrong Input parameter! '
		 WRITE (*,*) ' Please select suitable input! '
         WRITE (*,*) ' '
         CALL HEENTER
         GOTO 2201
      ENDIF
!
! *** Conduct Selection of method a)
!
 1001 CONTINUE
!
! *** Von Mises method - "non-symmetric" matrix
!
      CHFILE    =  'N' 
!
      call hdmat  ( CHFILE, ILINE, ICOLUMN, IERRALLOC, IERR )
	  If ( IERR .EQ. 10 ) then
	     goto 9999
	  endif
!
! *** Double Precision Matrix DMAT - input for calculation
!
      CALL SYSTEM ( 'cls' )
!
         write (*,*) ' '
         write (*,*) ' -------------------------------------- '
		 write (*,*) '                                        '
         write (*,*) ' Input File in Main Program EIGENVALUE  '
		 write (*,*) ' for Von Mises Method                   '
		 write (*,*) '                                        '
         write (*,*) ' -------------------------------------- '
         write (*,*) ' '
		 write (*,*) ' '
!
         WRITE(*,2001)
            DO 11 I=1,ILINE
   11          WRITE(*,2011)(DMAT(I,J),J=1,ICOLUMN)
         call heenter
!
      CALL MISTST ( DMAT, ILINE, ICOLUMN, IERRIN )
!
      If ( IERRALLOC /= 0 ) Then 
	    goto 9998
	  endif
!
      deallocate   (dmat) 
!
      GOTO 2201
!
! *** Conduct Selection of method b)
!
 1002 CONTINUE
!
! *** MARTIN,PARLETT, PETERS,REINSCH UND WILKINSON
!     calculates all Eigenvalues - "non-symmetric" matrix
!
      CHFILE    =  'N' 
!
      call hdmat  ( CHFILE, ILINE, ICOLUMN, IERRALLOC, IERR )
	  If ( IERR .EQ. 10 ) then
	     goto 9999
	  endif
!
! *** Double Precision Matrix DMAT - input for calculation
!
      CALL SYSTEM ( 'cls' )
!
         write (*,*) ' '
         write (*,*) ' -------------------------------------- '
		 write (*,*) '                                        '
         write (*,*) ' Input File in Main Program EIGENVALUE  '
		 write (*,*) ' for MARTIN,PARLETT, PETERS,REINSCH and ' 
		 write (*,*) ' WILKINSON method                       '
		 write (*,*) '                                        '
         write (*,*) ' -------------------------------------- '
         write (*,*) ' '
		 write (*,*) ' '
!
         WRITE(*,2001)
            DO 12 I=1,ILINE
   12          WRITE(*,2011)(DMAT(I,J),J=1,ICOLUMN)
         call heenter
!
      CALL EIGTST ( DMAT, ILINE, ICOLUMN, IERRIN )
!
      If ( IERRALLOC /= 0 ) Then 
	    goto 9998
	  endif
!
      deallocate   (dmat) 
!
      GOTO 2201
!
! *** Conduct Selection of method c)
!
 1003 CONTINUE
!
! *** LAPACK routine DGEEV computes for an N-by-N real nonsymmetric
!     matrix A, the eigenvalues and, optionally, the left and/or right 
!     eigenvectors.
!
      CHFILE    =  'N' 
!
      call hdmat  ( CHFILE, ILINE, ICOLUMN, IERRALLOC, IERR )
	  If ( IERR .EQ. 10 ) then
	     goto 9999
	  endif
!
! *** Double Precision Matrix DMAT - input for calculation
!
      CALL SYSTEM ( 'cls' )
!
         write (*,*) ' '
         write (*,*) ' -------------------------------------- '
		 write (*,*) '                                        '
         write (*,*) ' Input File in Main Program EIGENVALUE  '
		 write (*,*) ' for LAPACK routine DGEEV computes for  '
		 write (*,*) ' an N-by-N real nonsymmetric matrix A   '
		 write (*,*) ' the eigenvalues and, optionally, the   '
		 write (*,*) ' left and/or right eigenvectors.        '
		 write (*,*) '                                        '
         write (*,*) ' -------------------------------------- '
         write (*,*) ' '
		 write (*,*) ' '
!
         WRITE(*,2001)
            DO 13 I=1,ILINE
   13          WRITE(*,2011)(DMAT(I,J),J=1,ICOLUMN)
         call heenter!
!
      CALL REALEIG_2 ( DMAT, ILINE, ICOLUMN, IERRIN )
!
!
      If ( IERRALLOC /= 0 ) Then 
	    goto 9998
	  endif
!
      deallocate   (dmat) 
!
      GOTO 2201
!
! *** Conduct Selection of method d)
!
 1004 CONTINUE
!
!
! *** ARPACK routine DNAUPD and DNEUPD to calculates the eigenvalues 
!     and eigenvectors of a general real non-symmetric matrix 
!     using the Implicitly Restarted Arnoldi Method (IRAM) from 
!     ARPACK.                                      
!
      CHFILE    =  'N' 
!
      call hdmat  ( CHFILE, ILINE, ICOLUMN, IERRALLOC, IERR )
	  If ( IERR .EQ. 10 ) then
	     goto 9999
	  endif
!
! *** Double Precision Matrix DMAT - input for calculation
!
      CALL SYSTEM ( 'cls' )
!
         write (*,*) ' '
         write (*,*) ' --------------------------------------------- '
		 write (*,*) '                                               '
         write (*,*) ' Input File in Main Program EIGENVALUE         '
		 write (*,*) ' for ARPACK routine DNAUPD and DNEUPD to       '
		 write (*,*) ' calculates the eigenvalues and eigenvectors   '
		 write (*,*) ' of a general real non-symmetric matrix using  '
		 write (*,*) ' the Implicitly Restarted Arnoldi Method (IRAM '
         write (*,*) ' from ARPACK.                                  '
		 write (*,*) '                                               '
         write (*,*) ' --------------------------------------------- '
         write (*,*) ' '
		 write (*,*) ' '
!
         WRITE(*,2001)
            DO 14 I=1,ILINE
   14          WRITE(*,2011)(DMAT(I,J),J=1,ICOLUMN)
         call heenter!
!
      CALL REALEIG_3 ( DMAT, ILINE, ICOLUMN, IERRIN )
!
!
      If ( IERRALLOC /= 0 ) Then 
	    goto 9998
	  endif
!
      deallocate   (dmat) 
!
      GOTO 2201
!
! *** Conduct Selection of method e)
!
 1005 CONTINUE
!
!
! *** ARPACK routine DSAUPD and DSEUPD to calculates the eigenvalues 
!     and eigenvectors of a real symmetric matrix 
!     using the Implicitly Restarted Lanczos Method (IRLM) from 
!     ARPACK. This is the standard method for large sparse symmetric
!     eigenvalue problems in FEA.                                
!
      CHFILE    =  'S' 
!
      call hdmat  ( CHFILE, ILINE, ICOLUMN, IERRALLOC, IERR )
	  If ( IERR .EQ. 10 ) then
	     goto 9999
	  endif
!
! *** Double Precision Matrix DMAT - input for calculation
!
      CALL SYSTEM ( 'cls' )
!
         write (*,*) ' '
         write (*,*) ' --------------------------------------------- '
		 write (*,*) '                                               '
         write (*,*) ' Input File in Main Program EIGENVALUE         '
		 write (*,*) ' for ARPACK routine DSAUPD and DSEUPD to       '
		 write (*,*) ' calculates the eigenvalues and eigenvectors   '
		 write (*,*) ' of a real symmetric matrix using the          '
		 write (*,*) ' Implicitly Restarted Lanczos Method (IRLM     '
         write (*,*) ' from ARPACK.                                  '
		 write (*,*) ' This is the standard method for large sparse  '
		 write (*,*) ' symmetric eigenvalue problems in FEA.         '
		 write (*,*) '                                               '
         write (*,*) ' --------------------------------------------- '
         write (*,*) ' '
		 write (*,*) ' '
!
         WRITE(*,2001)
            DO 15 I=1,ILINE
   15          WRITE(*,2011)(DMAT(I,J),J=1,ICOLUMN)
         call heenter!
!
      CALL REALEIG_4 ( DMAT, ILINE, ICOLUMN, IERRIN )
!
!
      If ( IERRALLOC /= 0 ) Then 
	    goto 9998
	  endif
!
      deallocate   (dmat) 
!
      GOTO 2201
!
 1006 CONTINUE
!
!
! *** ARPACK routine DSAUPD and DSEUPD to calculates the eigenvalues 
!     and eigenvectors of a real symmetric matrix 
!     using the Implicitly Restarted Lanczos Method (IRLM) from 
!     ARPACK. This is the standard method for large symmetric
!     eigenvalue problems in FEA.  
!     LAPACK DGESV for for the OP*x = (K-σM)^(-1) M x (operator 
!     calculation)
!
!
      CALL SYSTEM ( 'cls' )
!
         write (*,*) ' '
         write (*,*) ' --------------------------------------------- '
		 write (*,*) '                                               ' 
         write (*,*) ' Selection = F, REALEIG_5                      '
         write (*,*) ' ARPACK DNAUPD / DNEUPD routines, LAPACK DGESV '
         write (*,*) ' for full dense M and K matrices               '
         write (*,*) '                                               '
		 write (*,*) ' Not implemented at the moment.                '
         write (*,*) ' Back to the selection menue!         		 '  
		 write (*,*) '                                               '
         write (*,*) ' --------------------------------------------- '		 
!
         call heenter		 
!
      GOTO 2201
!
 1007 CONTINUE
!
!
! *** ARPACK routine ????? and ????? to calculates the eigenvalues 
!     and eigenvectors of a real symmetric matrix 
!     using the Implicitly Restarted Lanczos Method (IRLM) from 
!     ARPACK. This is the standard method for large sparse symmetric
!     eigenvalue problems in FEA.  
!
       CALL SYSTEM ( 'cls' )
!
         write (*,*) ' '
         write (*,*) ' --------------------------------------------- '
		 write (*,*) '                                               ' 
         write (*,*) ' Selection = G, REALEIG_6                      '
         write (*,*) ' ARPACK ????? / ????? routines, LAPACK DGESV   '
         write (*,*) ' for sparse matrices M and K                   '
         write (*,*) '                                               '
		 write (*,*) ' Not implemented at the moment.                '
         write (*,*) ' Back to the selection menue!         		 '  
		 write (*,*) '                                               '
         write (*,*) ' --------------------------------------------- '		 
!
         call heenter		 
!                             
!
      GOTO 2201
!
!     Format Instructions
!
!........1.........2.........3.........4.........5.........6.........7.........8.........9........10

 2000 FORMAT(14H TESTBEISPIEL:,/,24(1H=),/,  
     &       1X,'ZU BERECHNENDE MATRIX RMAT:',/)
 2010 FORMAT(10(1X,F12.4))
!
 2001 FORMAT(14H TESTBEISPIEL:,/,24(1H=),/,  
     &       1X,'ZU BERECHNENDE MATRIX DMAT:',/)
 2011 FORMAT(10(1X,D12.4))
!
 2020 FORMAT(/,1X,'-BERECHNETE EIGENWERTE:    REALTEIL    I  ',
     +        'IMAGINAERTEIL',/,25X,31(1H-))
 2030 FORMAT(24X,D15.5,' I',D15.5)
!
! 
! 
 2040 FORMAT(/,1X,'-MATRIX DER NORMALISIERTEN EIGENVEKTOREN: - VL -',/)
 2041 FORMAT(10(1X,D12.4))
!
 2050 FORMAT(/,1X,'-MATRIX DER NORMALISIERTEN EIGENVEKTOREN: - VR -',/)
 2051 FORMAT(10(1X,D12.4))
!
 5000 FORMAT ( ' ', //
     + ' ******************************************************************',/,
     + ' *                                                                *',/,
     + ' *      Progranm Stop Neustart                                    *',/,
     + ' *                                                                *',/,
     + ' *                                                                *',/,
     + ' ******************************************************************',/,
     + '                                                                   ',/,                            
     + '                                                                   ' )
!
 9998 CONTINUE 
!      call heenter
 9999 CONTINUE
!
      CALL HEINOU ( 'eigenvalue Main Program', 'AUS', IERR )
!	  
      CALL HEENTER
!
! *** Ende von eigenvalue
!
      STOP
!
      END