!
! REALEIG_4  TEST Routine for the ARPACK Subroutines DSAUPD          2026/05/26
!                                                    DSEUPD
!
!
!                                              (Axel Hänschke)
!                                              (Prasith Basky Murugaperumal)
!........1.........2.........3.........4.........5.........6.........7.........8.........9........10
!*****************************************************************************
!                                                                            *
!     Test Program for the ARPACK Subroutines                                *
!                                              DSAUPD                        *
!                                              DSEUPD                        *
!                                                                            *
!     The routine calculates eigenvalues and eigenvectors of a               *
!     real symmetric matrix using the Implicitly Restarted Lanczos           *
!     Method (IRLM) from ARPACK.  This is the standard method for            *
!     large sparse symmetric eigenvalue problems in FEA.                     *
!                                                                            *
!     TESTBEISPIEL:                                                          *
!     =============                                                          *
!     ZU BERECHNENDE MATRIX (symmetrische Tridiagonalmatrix):                *
!                                                                            *
!        .2000D+01   -.1000D+01    .0000D+00    .0000D+00                    *
!       -.1000D+01    .2000D+01   -.1000D+01    .0000D+00                    *
!        .0000D+00   -.1000D+01    .2000D+01   -.1000D+01                    *
!        .0000D+00    .0000D+00   -.1000D+01    .2000D+01                    *
!                                                                            *
!     -BERECHNETE EIGENWERTE (NEV=2 groesste):  REALTEIL                     *
!                             ----------------                               *
!                                 .36180D+01                                 *
!                                 .26180D+01                                 *
!                                                                            *
!     Analytische Eigenwerte:                                                *
!       lambda_k = 2 - 2*cos(k*pi/5),  k=1..4                                *
!       ~= 0.3820,  1.3820,  2.6180,  3.6180                                 *
!                                                                            *
!*****************************************************************************
!
      SUBROUTINE REALEIG_4 ( DMAT, ILINE, ICOLUMN, IERRIN )
!
!........1.........2.........3.........4.........5.........6.........7.........8.........9........10
!
!
! ======================================================================
!IT*  I N I T I A L I S I E R U N G S T E I L
! ======================================================================
!
!     ISO Fortran Standard and precision definition
! 
      use precision_fem
!
!     use matrix_fem
!
!
      IMPLICIT                         NONE
!
! *** INTEGER
!
      INTEGER                          I
      INTEGER                          J
      INTEGER                          N
      INTEGER                          LDA
      INTEGER                          IERR
      INTEGER                          IUNIT
      INTEGER                          IDIML
      INTEGER                          IDIMC
!
      integer,         intent(in)  ::  iline
      integer,         intent(in)  ::  icolumn
      integer,         intent(out) ::  ierrin
!
! *** DOUBLE PRECISION - DMAT
!
      real(real64),     intent(in)  :: dmat(iline, icolumn)
!
!
! *** Double Precision in new Fortran
!
      real(real64),     allocatable :: A(:,:)
!
!........1.........2.........3.........4.........5.........6.........7.........8.........9........10
!
!
! *** ARPACK parameters (symmetric)
!
      INTEGER                          NEV
      INTEGER                          NCV
      INTEGER                          LDV
      INTEGER                          LWORKL
      INTEGER                          IDO
      INTEGER                          INFO
	  INTEGER                          NPARAM
	  INTEGER                          NPNTR
!      INTEGER                          IPARAM(11) -- NPARAM = 11
!      INTEGER                          IPNTR(14)  -- NPNTR  = 11
      integer,        allocatable ::   IPARAM(:)
	  integer,        allocatable ::   IPNTR(:) 
!
!     PARAMETER                        (NEV    = 2)
!     PARAMETER                        (NCV    = N)
!     PARAMETER                        (LDV    = N)
!     PARAMETER                        (LWORKL = NCV*NCV + 8*NCV)
!
      LOGICAL                          RVEC
!     LOGICAL                          SELECT(NCV)
      logical,        allocatable ::   SELECT(:)
!
      real(real64)                     TOL
      real(real64)                     SIGMA
!
!     DOUBLE PRECISION                 RESID(N)
!     DOUBLE PRECISION                 V(LDV, NCV)
!     DOUBLE PRECISION                 WORKD(3*N)
!     DOUBLE PRECISION                 WORKL(LWORKL)
!     DOUBLE PRECISION                 D(NEV)
!     DOUBLE PRECISION                 Z(N, NEV)
!
      real(real64),     allocatable :: RESID(:)
	  real(real64),     allocatable :: V(:,:)
      real(real64),     allocatable :: WORKD(:)
      real(real64),     allocatable :: WORKL(:)
	  real(real64),     allocatable :: D(:)
	  real(real64),     allocatable :: Z(:,:)
!
!
! *** Field dimensions
!
      N          = ILINE
	  NEV        = 2
      NCV        = N
	  LDA        = N
      LDV        = N 
      LWORKL     = NCV*NCV + 8*NCV	
!
	  NPARAM     = 11
	  NPNTR      = 11
!
      ierrin     = 0
!
! *** Storage allocation of internal fields
!
      allocate (A(n,n))
!
      allocate (IPARAM(nparam))
	  allocate (IPNTR(npntr))
!
      allocate (SELECT(ncv))
!
      allocate (RESID(n))
	  allocate (V(ldv,ncv))	
	  allocate (WORKD(3*n))
	  allocate (WORKL(lworkl))	
      allocate (D(nev))
      allocate (Z(n,nev))	  
!
! ======================================================================
!VT*  V E R A R B E I T U N G S T E I L
! ======================================================================
!
!
!........1.........2.........3.........4.........5.........6.........7.........8.........9........10
!
!
! *** HELP Initialisierung
!
      CALL HEINOU ( 'realeig_4', 'EIN', IERR )
!
!
! *** Test, if iline .eg. icolumn (quadratic matrix is necessary)
! 
      if (iline /= icolumn) then
         ierrin = 1
         return
      end if	
!
! *** Copy DMAT to internal Matrix a
!
      A = DMAT
!
! *** Testausgabe
!
      CALL SYSTEM ( 'cls' )
!
         write (*,*) ' '
		 write (*,*) ' '
         write (*,*) ' Test if Input Data in REALEIG_4    '
         write (*,*) ' Input Data Double Precision Format '
         write (*,*) ' ---------------------------------- '
         write (*,*) ' '
         WRITE(*,2001)
            DO 11 I=1,ILINE
   11          WRITE(*,2011)(DMAT(I,J),J=1,ICOLUMN)
         call heenter
!
! *** Systembefehl absetzen
!
      CALL SYSTEM ( 'cls' )
!
      WRITE (*,*) ' '
      WRITE (*,*) ' '
      WRITE (*,*) '---------------------------------------------------'
      WRITE (*,*) ' '
      WRITE (*,*) ' Program: REALEIG_4'
      WRITE (*,*) ' =================='
      WRITE (*,*) '    '
      WRITE (*,*) ' ARPACK DSAUPD/DSEUPD: Implicitly Restarted Lanczos'
      WRITE (*,*) ' Method (IRLM) for real symmetric matrix.'
      WRITE (*,*) ' '
      WRITE (*,*) ' Computes NEV = 2 eigenvalues of largest algebraic'
      WRITE (*,*) ' value (WHICH = LA) and corresponding eigenvectors.'
      WRITE (*,*) ' '
      WRITE (*,*) ' Mode 1: A*x = lambda*x  (standard symmetric)'
      WRITE (*,*) ' '
      WRITE (*,*) ' Source: ARPACK-NG 3.9.1'
      WRITE (*,*) '         https://github.com/opencollab/arpack-ng'
      WRITE (*,*) ' '
      WRITE (*,*) ' Test matrix: 4x4 symmetric tridiagonal (FEA-like)'
      WRITE (*,*) '   Analytical eigenvalues ~= 0.382, 1.382, 2.618,'
      WRITE (*,*) '   3.618  (lambda_k = 2 - 2*cos(k*pi/5))'
      WRITE (*,*) ' '
      WRITE (*,*) ' Achtung: Eingabematrix nur im Double Precision '
      WRITE (*,*) '          Format zulaessig!'
      WRITE (*,*) ' '
      WRITE (*,*) '---------------------------------------------------'
      WRITE (*,*) ' '
      WRITE (*,*) ' '
!
      call heenter
!
!
!........1.........2.........3.........4.........5.........6.........7.........8.........9........10
!
! *** ARPACK DSAUPD/DSEUPD - Implicitly Restarted Lanczos Method
!
! *** Initialisierung
!
      IDO    = 0
      INFO   = 0
      TOL    = 0.0D0
      SIGMA  = 0.0D0
      RVEC   = .TRUE.
!
      IPARAM(1)  = 1
      IPARAM(2)  = 0
      IPARAM(3)  = 300
      IPARAM(4)  = 1
      IPARAM(5)  = 0
      IPARAM(6)  = 0
      IPARAM(7)  = 1
      IPARAM(8)  = 0
      IPARAM(9)  = 0
      IPARAM(10) = 0
      IPARAM(11) = 0
!
! *** Reverse Communication Interface (RCI) Loop
!
   90 CONTINUE
      CALL DSAUPD ( IDO, 'I', N, 'LA', NEV, TOL, RESID,
     &              NCV, V, LDV, IPARAM, IPNTR, WORKD,
     &              WORKL, LWORKL, INFO )
!
      IF ( IDO .EQ. -1 .OR. IDO .EQ. 1 ) THEN
!
!        Y = A * X  (matrix-vector product)
!
         CALL DGEMV ( 'N', N, N, 1.0D0, A, LDA,
     &                WORKD(IPNTR(1)), 1,
     &                0.0D0, WORKD(IPNTR(2)), 1 )
         GOTO 90
!
      END IF
!
! *** Check for DSAUPD errors
!
      WRITE(*,*) ' DSAUPD: INFO = ', INFO
      WRITE(*,*) ' DSAUPD: IPARAM(5) converged Ritz values = ',
     &             IPARAM(5)
!
      IF ( INFO .LT. 0 ) THEN
         WRITE (*,*) ' DSAUPD error: INFO = ', INFO
         GOTO 9999
      END IF
!
! *** Post-process with DSEUPD to extract eigenpairs
!
      CALL DSEUPD ( RVEC, 'All', SELECT, D, Z, N, SIGMA,
     &              'I', N, 'LA', NEV, TOL, RESID, NCV,
     &              V, LDV, IPARAM, IPNTR, WORKD,
     &              WORKL, LWORKL, IERR )
!
!     AUSGABE DER LOESUNG
!
      WRITE(*,*) ' DSEUPD: IERR = ', IERR
      WRITE(*,*) ' '
!
      IF ( IERR .EQ. 0 ) THEN
      WRITE (*,*) ' '
      WRITE (*,*) ' '
      WRITE (*,*) '---------------------------------------------------'
      WRITE (*,*) ' '
      WRITE (*,*) ' Ergebnisse:'
      WRITE (*,*) ' ==========='
      WRITE (*,*) ' '
         WRITE(*,2020)
         WRITE(*,2030)(D(I),I=1,NEV)
!
!         call heenter
      ENDIF
!
      WRITE(*,2040)
      DO 70 I=1,NEV
   70 WRITE(*,2041)(Z(J,I), J=1,N)
!
      WRITE (*,*) ' '
      WRITE (*,*) ' '
      WRITE (*,*) '---------------------------------------------------'
      WRITE (*,*) ' '
      WRITE (*,*) ' '
!
      write (*,*) 'BERECHNUNG PROGRAMMGEMAESS BEENDET!'
	  call heenter
         goto 9998
!
!
! ======================================================================
!AT*  A U S G A B E T E I L
! ======================================================================
!
!
!     Format Instructions
!
!........1.........2.........3.........4.........5.........6.........7.........8.........9........10

 2001 FORMAT(14H TESTBEISPIEL:,/,24(1H=),/,
     &       1X,'ZU BERECHNENDE MATRIX DMAT (symmetrisch):',/)
 2011 FORMAT(10(1X,D12.4))
!
 2020 FORMAT(/,1X,'-BERECHNETE EIGENWERTE (groesste algebraische):',
     +        /,16X,16(1H-))
 2030 FORMAT(16X,D15.5)
!
 2040 FORMAT(/,1X,'-MATRIX DER NORMALISIERTEN EIGENVEKTOREN: - Z -',/)
 2041 FORMAT(10(1X,D12.4))
!
 9998 CONTINUE
! 
! *** De-Allocation of internal fields
!
      deallocate   (A)
!
      deallocate (IPARAM)
	  deallocate (IPNTR)
!
      deallocate (SELECT)
!
      deallocate (RESID)
	  deallocate (V)
	  deallocate (WORKD)
	  deallocate (WORKL)
	  deallocate (D)
	  deallocate (Z)
!
 9999 CONTINUE
!
      CALL HEINOU ( 'realeig_4', 'AUS', IERR )
!	  call heenter 
!
! *** Ende von realeig_4
!
      RETURN
!
      END
