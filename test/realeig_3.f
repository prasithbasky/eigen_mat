!
! REALEI_3  TEST Routine for the ARPACK Subroutines DNAUPD          2026/05/26
!                                                   DNEUPD
!
!                                              (Axel Hänschke)
!                                              (Prasith Basky Murugaperumal)
!........1.........2.........3.........4.........5.........6.........7.........8.........9........10
!*****************************************************************************
!                                                                            *
!     Test Program for the ARPACK Subroutines                                *
!                                              DNAUPD                        *
!                                              DNEUPD                        *
!                                                                            *
!     The routine calculates eigenvalues and eigenvectors of a general       *
!     real non-symmetric matrix using the Implicitly Restarted Arnoldi       *
!     Method (IRAM) from ARPACK.                                             *
!                                                                            *
!     MIT DEN VORGEGEBENEN DATEN ERHALTEN SIE AUF dem Laptop HP PROBOOK      *
!     FOLGENDES TESTERGEBNIS:                                                *
!                                                                            *
!     TESTBEISPIEL:                                                          *
!     =============                                                          *
!     ZU BERECHNENDE MATRIX:                                                 *
!                                                                            *
!       -.2000D+01    .2000D+01    .2000D+01    .2000D+01                    *
!       -.3000D+01    .3000D+01    .2000D+01    .2000D+01                    *
!       -.2000D+01    .0000D+00    .4000D+01    .2000D+01                    *
!       -.1000D+01    .0000D+00    .0000D+00    .5000D+01                    *
!                                                                            *
!     -BERECHNETE EIGENWERTE (NEV=2 groesste):  REALTEIL  I  IMAGINAERTEIL   *
!                             -------------------------------                *
!                                 .40000D+01 I     .00000D+00                *
!                                 .30000D+01 I     .00000D+00                *
!                                                                            *
!     WEITERE TESTLAEUFE SIND DURCH MODIFIZIERUNG DER WERTE IN DEN           *
!     DATA-ANWEISUNGEN JEDERZEIT MOEGLICH.                                   *
!                                                                            *
!                                                                            *
!*****************************************************************************
!
      SUBROUTINE REALEIG_3 ( DMAT, ILINE, ICOLUMN, IERRIN )
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
!
      integer,         intent(in)  ::  iline
      integer,         intent(in)  ::  icolumn
      integer,         intent(out) ::  ierrin
!
! *** PARAMETER
!
!     PARAMETER                        (N     = 4)
!     PARAMETER                        (IDIML = N)
!     PARAMETER                        (IDIMC = N)
!     PARAMETER                        (LDA   = N)
!
!
!........1.........2.........3.........4.........5.........6.........7.........8.........9........10
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
! *** ARPACK parameters
!
      INTEGER                          NEV
      INTEGER                          NCV
      INTEGER                          LDV
      INTEGER                          LWORKL
      INTEGER                          IDO
      INTEGER                          INFO
	  INTEGER                          NPARAM
	  INTEGER                          NPNTR
!
!      INTEGER                          IPARAM(11) -- NPARAM = 11
!      INTEGER                          IPNTR(14)  -- NPNTR  = 14
      integer,        allocatable ::   IPARAM(:)
	  integer,        allocatable ::   IPNTR(:) 
!
!     PARAMETER                        (NEV    = 2)
!     PARAMETER                        (NCV    = N)
!     PARAMETER                        (LDV    = N)
!     PARAMETER                        (LWORKL = 3*NCV*NCV + 6*NCV)
!
      LOGICAL                          RVEC
!     LOGICAL                          SELECT(NCV)
      logical,        allocatable ::   SELECT(:)
!
      real(real64)                     TOL
      real(real64)                     SIGMAR
      real(real64)                     SIGMAI
!
!     DOUBLE PRECISION                 RESID(N)
!     DOUBLE PRECISION                 V(LDV, NCV)
!     DOUBLE PRECISION                 WORKD(3*N)
!     DOUBLE PRECISION                 WORKL(LWORKL)
!     DOUBLE PRECISION                 DR(NEV+1)
!     DOUBLE PRECISION                 DI(NEV+1)
!     DOUBLE PRECISION                 Z(N, NEV+1)
!     DOUBLE PRECISION                 WORKEV(3*NCV)
!
      real(real64),     allocatable :: RESID(:)
	  real(real64),     allocatable :: V(:,:)
      real(real64),     allocatable :: WORKD(:)
      real(real64),     allocatable :: WORKL(:)
      real(real64),     allocatable :: DR(:)
      real(real64),     allocatable :: DI(:)
      real(real64),     allocatable :: Z(:,:)
      real(real64),     allocatable :: WORKEV(:)	  
!
      CHARACTER                        BMAT*1
      CHARACTER                        WHICH*2
      CHARACTER                        HOWMNY*1
!
!
! *** Field dimensions
!
      N          = ILINE
	  NEV        = 2
	  NCV        = N
	  LDA        = N
	  LDV        = N 
	  LWORKL     = 3*NCV*NCV + 6*NCV
!
	  NPARAM     = 11
	  NPNTR      = 14
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
	  allocate (DR(nev+1))
	  allocate (DI(nev+1))
	  allocate (Z(n,nev+1))
	  allocate (WORKEV(3*ncv))
!

!
! ======================================================================
!VT*  V E R A R B E I T U N G S T E I L
! ======================================================================
!
!........1.........2.........3.........4.........5.........6.........7.........8.........9........10
!
!
! *** HELP Initialisierung
!
      CALL HEINOU ( 'realeig_3', 'EIN', IERR )
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
         write (*,*) ' Test if Input Data in REALEIG_3    '
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
      WRITE (*,*) ' Program: REALEIG_3'
      WRITE (*,*) ' =================='
      WRITE (*,*) '    '
      WRITE (*,*) ' ARPACK DNAUPD/DNEUPD: Implicitly Restarted Arnoldi'
      WRITE (*,*) ' Method (IRAM) for general real non-symmetric '
	  WRITE (*,*) '         matrix.'
      WRITE (*,*) ' '
      WRITE (*,*) ' Computes NEV = 2 eigenvalues of largest magnitude'
      WRITE (*,*) ' (WHICH = LM) and corresponding eigenvectors.'
      WRITE (*,*) ' '
      WRITE (*,*) ' Mode 1: A*x = lambda*x  (standard eigenvalue)'
      WRITE (*,*) ' '
      WRITE (*,*) ' Source: ARPACK-NG 3.9.1'
      WRITE (*,*) '         https://github.com/opencollab/arpack-ng'
      WRITE (*,*) ' '
      WRITE (*,*) ' Suitable for large sparse matrices - standard '
	  WRITE (*,*) '          FEA.'
      WRITE (*,*) ' '
      WRITE (*,*) ' Achtung: Eingabematrix nur im Double Precision '
      WRITE (*,*) '          Format zulaessig!'
      WRITE (*,*) '          Eingabe im standard REAL Format folgt'
      WRITE (*,*) '          Programm Abbruch!!! '
      WRITE (*,*) ' '
      WRITE (*,*) '---------------------------------------------------'
      WRITE (*,*) ' '
      WRITE (*,*) ' '
!
      call heenter
!
!........1.........2.........3.........4.........5.........6.........7.........8.........9........10
!
! *** ARPACK DNAUPD/DNEUPD - Implicitly Restarted Arnoldi Method
!
! *** Initialisierung
!
      BMAT   = 'I'
      WHICH  = 'LM'
      TOL    = 0.0D0
      IDO    = 0
      INFO   = 0
      RVEC   = .TRUE.
      HOWMNY = 'A'
      SIGMAR = 0.0D0
      SIGMAI = 0.0D0
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
      CALL DNAUPD ( IDO, BMAT, N, WHICH, NEV, TOL, RESID,
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
! *** Check for DNAUPD errors
!
      WRITE(*,*) ' DNAUPD: INFO = ', INFO
      WRITE(*,*) ' DNAUPD: IPARAM(5) converged Ritz values = ',
     &             IPARAM(5)
!
      IF ( INFO .LT. 0 ) THEN
         WRITE (*,*) ' DNAUPD error: INFO = ', INFO
         GOTO 9999
      END IF
!
! *** Post-process with DNEUPD to extract eigenpairs
!
      CALL DNEUPD ( RVEC, HOWMNY, SELECT, DR, DI, Z, N,
     &              SIGMAR, SIGMAI, WORKEV, BMAT, N, WHICH,
     &              NEV, TOL, RESID, NCV, V, LDV,
     &              IPARAM, IPNTR, WORKD, WORKL, LWORKL, IERR )
!
!     AUSGABE DER LOESUNG
!
      WRITE(*,*) ' DNEUPD: IERR = ', IERR
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
         WRITE(*,2030)(DR(I),DI(I),I=1,NEV)
!
!        call heenter
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
     &       1X,'ZU BERECHNENDE MATRIX DMAT:',/)
 2011 FORMAT(10(1X,D12.4))
!
 2020 FORMAT(/,1X,'-BERECHNETE EIGENWERTE:    REALTEIL    I  ',
     +        'IMAGINAERTEIL',/,25X,31(1H-))
 2030 FORMAT(24X,D15.5,' I',D15.5)
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
	  deallocate (DR)
	  deallocate (DI)
	  deallocate (Z)
	  deallocate (WORKEV)
!
 9999 CONTINUE
!
      CALL HEINOU ( 'realeig_3', 'AUS', IERR )
!	  call heenter
!
! *** Ende von realeig_3
!
      RETURN
!
      END
