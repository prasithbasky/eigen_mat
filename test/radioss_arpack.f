      SUBROUTINE RADIOSS_ARPACK(CHSELECT, IERRIN)
      IMPLICIT NONE
      CHARACTER*1 CHSELECT
      INTEGER IERRIN

      ! Dimensions
      INTEGER NDDL, NNZK, NDDLI, NNZI, NUMNOD
      INTEGER, ALLOCATABLE :: IADK(:), JDIK(:), IDDL(:), NDOF(:)
      DOUBLE PRECISION, ALLOCATABLE :: DIAG_K(:), LT_K(:), MS(:), IN(:)
      DOUBLE PRECISION, ALLOCATABLE :: LB(:)
      
      ! Dense Matrices
      DOUBLE PRECISION, ALLOCATABLE :: A(:,:), B(:,:), C(:,:)
      
      ! ARPACK variables
      INTEGER NEV, NCV, LDV, LWORKL, INFO_ARPACK, IDO
      INTEGER IPARAM(11), IPNTR(11)
      LOGICAL RVEC, SELECT_ARR(100)
      DOUBLE PRECISION SIGMA, TOL
      DOUBLE PRECISION, ALLOCATABLE :: RESID(:), V(:,:), WORKD(:), WORKL(:)
      DOUBLE PRECISION, ALLOCATABLE :: D(:,:), Z(:,:), WORK_EV(:)
      
      ! Local variables
      INTEGER UNIT_NO, IERR, I, J, P, INFO
      INTEGER NODE, START_DOF, NUM_DOF
      CHARACTER(LEN=100) FNAME, FNAME_RAD
      DOUBLE PRECISION FREQ, PI, EPS

      FNAME = 'implicit_matrices.bin'
      FNAME_RAD = '/home/wsl/Documents/OpenRadioss/example/test/Platte_200x200_mm_Optistruct_0000.rad'
      UNIT_NO = 10
      PI = 3.14159265358979323846D0
      EPS = 1.0D-12

      WRITE(*,*) '========================================='
      WRITE(*,*) '   GENERALIZED EIGENVALUE TEST (ARPACK)  '
      WRITE(*,*) '========================================='
      WRITE(*,*) 'Reading binary data from: ', TRIM(FNAME)

      OPEN(UNIT=UNIT_NO, FILE=FNAME, STATUS='OLD',
     .     FORM='UNFORMATTED', ACCESS='STREAM', IOSTAT=IERR)
      
      IF (IERR /= 0) THEN
         WRITE(*,*) 'Error opening file!'
         IERRIN = 1
         RETURN
      END IF

      READ(UNIT_NO) NDDL, NNZK, NDDLI, NNZI, NUMNOD

      WRITE(*,*) 'Dimensions read:'
      WRITE(*,*) '  NDDL   = ', NDDL
      WRITE(*,*) '  NNZK   = ', NNZK
      WRITE(*,*) '  NUMNOD = ', NUMNOD

      ALLOCATE(IADK(NDDL+1), JDIK(NNZK), DIAG_K(NDDL), LT_K(NNZK))
      ALLOCATE(LB(NDDLI))
      ALLOCATE(IDDL(NUMNOD), NDOF(NUMNOD), MS(NUMNOD), IN(NUMNOD))

      READ(UNIT_NO) IADK, JDIK, DIAG_K, LT_K
      READ(UNIT_NO) LB
      READ(UNIT_NO) IDDL, NDOF, MS, IN
      CLOSE(UNIT_NO)
      
      WRITE(*,*) 'File read completed successfully.'

      ! Allocate Dense Matrices
      ALLOCATE(A(NDDL, NDDL), B(NDDL, NDDL), C(NDDL, NDDL))
      
      WRITE(*,*) 'Assembling dense stiffness matrix K...'
      A = 0.0D0
      ! Fill diagonal of K
      DO I = 1, NDDL
         A(I, I) = DIAG_K(I)
      END DO
      
      ! Fill off-diagonals of K (Upper triangle)
      DO I = 1, NDDL
        DO P = IADK(I), IADK(I+1)-1
          J = JDIK(P)
          A(I, J) = LT_K(P)
          A(J, I) = LT_K(P) ! symmetric
        END DO
      END DO

      WRITE(*,*) 'Assembling Consistent mass matrix M from RAD file...'
      CALL BUILD_CONSISTENT_MASS(FNAME_RAD, NDDL, B)
      
      ! Ensure matrix B is strictly positive definite
      DO I = 1, NDDL
         IF (B(I, I) < EPS) B(I, I) = EPS
      END DO

      WRITE(*,*) 'Preparing Shift-Invert Operator C = K - SIGMA*M'
      SIGMA = -1.0D0
      DO J = 1, NDDL
         DO I = 1, NDDL
            C(I, J) = A(I, J) - SIGMA * B(I, J)
         END DO
      END DO

      ! Factorize C using Cholesky (since K+M is symmetric positive definite)
      CALL DPOTRF('U', NDDL, C, NDDL, INFO)
      IF (INFO /= 0) THEN
         WRITE(*,*) 'Error in DPOTRF: INFO = ', INFO
         RETURN
      END IF
      
      WRITE(*,*) 'Starting ARPACK DSAUPD...'
      NEV = 15
      NCV = MIN(NDDL, 40)
      LDV = NDDL
      LWORKL = NCV*(NCV + 8)
      
      ALLOCATE(RESID(NDDL), V(LDV, NCV), WORKD(3*NDDL), WORKL(LWORKL))
      ALLOCATE(D(NEV, 2), Z(NDDL, NEV), WORK_EV(NCV))
      
      IDO = 0
      INFO_ARPACK = 0
      IPARAM(1) = 1       ! ISHIFT
      IPARAM(3) = 300     ! MAXITR
      IPARAM(7) = 3       ! MODE 3: Shift-Invert for Generalized problem
      TOL = 0.0D0         ! Machine precision
      
1000  CONTINUE
      CALL DSAUPD ( IDO, 'G', NDDL, 'LM', NEV, TOL, RESID, 
     &              NCV, V, LDV, IPARAM, IPNTR, WORKD, WORKL,
     &              LWORKL, INFO_ARPACK )
     
      IF (IDO == -1 .OR. IDO == 1) THEN
         ! Perform Y = (K - SIGMA*M)^{-1} * M * X
         ! For IDO=1, M*X is already in WORKD(IPNTR(3))
         IF (IDO == 1) THEN
            DO I = 1, NDDL
               WORKD(IPNTR(2) + I - 1) = WORKD(IPNTR(3) + I - 1)
            END DO
         ELSE
            ! For IDO=-1, we must compute M*X first
            CALL DSYMV('U', NDDL, 1.0D0, B, NDDL, WORKD(IPNTR(1)), 1,
     &                 0.0D0, WORKD(IPNTR(2)), 1)
         END IF
         ! Solve C * Y = M * X
         CALL DPOTRS('U', NDDL, 1, C, NDDL, WORKD(IPNTR(2)), NDDL, INFO)
         GO TO 1000
         
      ELSE IF (IDO == 2) THEN
         ! Perform Y = M * X
         CALL DSYMV('U', NDDL, 1.0D0, B, NDDL, WORKD(IPNTR(1)), 1,
     &              0.0D0, WORKD(IPNTR(2)), 1)
         GO TO 1000
      END IF
      
      IF (INFO_ARPACK < 0) THEN
         WRITE(*,*) 'Error with DSAUPD, INFO = ', INFO_ARPACK
         RETURN
      END IF
      
      ! Post-process to extract eigenvalues
      RVEC = .TRUE.
      CALL DSEUPD ( RVEC, 'A', SELECT_ARR, D(1,1), Z, NDDL, SIGMA,
     &              'G', NDDL, 'LM', NEV, TOL, RESID, NCV, V, LDV,
     &              IPARAM, IPNTR, WORKD, WORKL, LWORKL, INFO_ARPACK )
     
      IF (INFO_ARPACK /= 0) THEN
         WRITE(*,*) 'Error with DSEUPD, INFO = ', INFO_ARPACK
         RETURN
      END IF
      
      WRITE(*,*) 'Eigenvalue solution successful!'
      WRITE(*,*) ' '
      WRITE(*,*) 'Mode       Eigenvalue       Frequency (Hz)'
      WRITE(*,*) '-----------------------------------------'
      DO I = 1, NEV
         IF (D(I,1) < 0.0D0) THEN
            FREQ = 0.0D0
         ELSE
            FREQ = SQRT(D(I,1)) / (2.0D0 * PI)
         END IF
         WRITE(*, '(I4,2X,E15.6,2X,E15.6)') I, D(I,1), FREQ
      END DO
      
      ! Cleanup
      DEALLOCATE(A, B, C, RESID, V, WORKD, WORKL, D, Z, WORK_EV)
      DEALLOCATE(IADK, JDIK, DIAG_K, LT_K, LB, IDDL, NDOF, MS, IN)

      END SUBROUTINE RADIOSS_ARPACK
