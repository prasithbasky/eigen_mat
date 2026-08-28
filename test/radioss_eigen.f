      SUBROUTINE RADIOSS_EIGEN(CHSELECT, IERRIN)
      IMPLICIT NONE
      CHARACTER*1 CHSELECT
      INTEGER IERRIN

      ! Dimensions
      INTEGER NDDL, NNZK, NDDLI, NNZI, NUMNOD

      ! Sparse stiffness matrix (K_eff)
      INTEGER, ALLOCATABLE :: IADK(:), JDIK(:)
      DOUBLE PRECISION, ALLOCATABLE :: DIAG_K(:), LT_K(:)

      ! Interface matrix
      INTEGER, ALLOCATABLE :: IADI(:), JDII(:), ITOK(:)
      DOUBLE PRECISION, ALLOCATABLE :: DIAG_I(:), LT_I(:)

      ! RHS vector
      DOUBLE PRECISION, ALLOCATABLE :: LB(:)

      ! Nodal maps and mass arrays
      INTEGER, ALLOCATABLE :: IDDL(:), NDOF(:)
      DOUBLE PRECISION, ALLOCATABLE :: MS(:), IN(:)

      ! Dense matrices for LAPACK
      DOUBLE PRECISION, ALLOCATABLE :: A(:,:)
      DOUBLE PRECISION, ALLOCATABLE :: B(:,:)
      DOUBLE PRECISION, ALLOCATABLE :: W(:)
      DOUBLE PRECISION, ALLOCATABLE :: WORK(:)

      ! Local variables
      INTEGER UNIT_NO, IERR, I, J, P, INFO, LWORK
      INTEGER NODE, START_DOF, NUM_DOF
      CHARACTER(LEN=100) FNAME, FNAME_RAD
      DOUBLE PRECISION FREQ, PI, EPS

      FNAME = 'implicit_matrices.bin'
      FNAME_RAD = '/home/wsl/Documents/OpenRadioss/example/test/Platte_200x200_mm_Optistruct_0000.rad'
      UNIT_NO = 10
      PI = 3.14159265358979323846D0
      EPS = 1.0D-12

      WRITE(*,*) '========================================='
      WRITE(*,*) '   GENERALIZED EIGENVALUE TEST ENV'
      WRITE(*,*) '========================================='
      WRITE(*,*) 'Reading binary data from: ', FNAME

      OPEN(UNIT=UNIT_NO, FILE=FNAME, STATUS='OLD',
     .     FORM='UNFORMATTED', ACCESS='STREAM', IOSTAT=IERR)
      IF (IERR /= 0) THEN
        WRITE(*,*) 'Error: Unable to open file ', FNAME
        STOP
      END IF

      ! Read dimensions
      READ(UNIT_NO) NDDL
      READ(UNIT_NO) NNZK
      READ(UNIT_NO) NDDLI
      READ(UNIT_NO) NNZI
      READ(UNIT_NO) NUMNOD

      WRITE(*,*) 'Dimensions read:'
      WRITE(*,*) '  NDDL   = ', NDDL
      WRITE(*,*) '  NNZK   = ', NNZK
      WRITE(*,*) '  NUMNOD = ', NUMNOD

      ! Allocate arrays
      ALLOCATE(IADK(NDDL+1))
      ALLOCATE(JDIK(NNZK))
      ALLOCATE(DIAG_K(NDDL))
      ALLOCATE(LT_K(NNZK))

      IF (NDDLI > 0) THEN
        ALLOCATE(IADI(NDDLI+1))
        ALLOCATE(JDII(NNZI))
        ALLOCATE(ITOK(NDDLI))
        ALLOCATE(DIAG_I(NDDLI))
        ALLOCATE(LT_I(NNZI))
      END IF

      ALLOCATE(LB(NDDL))
      ALLOCATE(IDDL(NUMNOD))
      ALLOCATE(NDOF(NUMNOD))
      ALLOCATE(MS(NUMNOD))
      ALLOCATE(IN(NUMNOD))

      ALLOCATE(A(NDDL, NDDL))
      ALLOCATE(B(NDDL, NDDL))
      ALLOCATE(W(NDDL))
      
      ! LWORK for DSYGV (optimal size is usually much larger than 3*N, but 3*N is minimum)
      LWORK = MAX(1, 3*NDDL)
      ALLOCATE(WORK(LWORK))

      ! Read data
      READ(UNIT_NO) IADK
      READ(UNIT_NO) JDIK
      READ(UNIT_NO) DIAG_K
      READ(UNIT_NO) LT_K

      IF (NDDLI > 0) THEN
        READ(UNIT_NO) IADI
        READ(UNIT_NO) JDII
        READ(UNIT_NO) ITOK
        READ(UNIT_NO) DIAG_I
        READ(UNIT_NO) LT_I
      END IF

      READ(UNIT_NO) LB
      READ(UNIT_NO) IDDL
      READ(UNIT_NO) NDOF
      READ(UNIT_NO) MS
      READ(UNIT_NO) IN

      CLOSE(UNIT_NO)
      WRITE(*,*) 'File read completed successfully.'

      WRITE(*,*) 'Assembling dense stiffness matrix K...'
      A = 0.0D0

      ! Fill diagonal of K
      DO I = 1, NDDL
        A(I, I) = DIAG_K(I)
      END DO

      ! Fill off-diagonals of K (only Upper triangle needed for DSYGV)
      DO I = 1, NDDL
        DO P = IADK(I), IADK(I+1)-1
          J = JDIK(P)
          ! OpenRadioss stores the upper triangle, so J > I
          A(I, J) = LT_K(P)
        END DO
      END DO

      WRITE(*,*) 'Assembling Consistent mass matrix M from RAD file...'
      CALL BUILD_CONSISTENT_MASS(FNAME_RAD, NDDL, B)
      
      ! Ensure matrix B is strictly positive definite for DSYGV
      ! (Since we simplified rotational DOFs, some might be zero)
      DO I = 1, NDDL
         IF (B(I, I) < EPS) B(I, I) = EPS
      END DO

      ! Call DSYGV to solve A*x = lambda*B*x
      ! ITYPE = 1 (A*x = lambda*B*x)
      ! JOBZ = 'N' (Eigenvalues only, change to 'V' for eigenvectors)
      ! UPLO = 'U' (Upper triangles of A and B are stored)
      WRITE(*,*) 'Solving Generalized Eigenvalue Problem (DSYGV)...'
      CALL DSYGV(1, 'N', 'U', NDDL, A, NDDL, B, NDDL, W, WORK,
     .           LWORK, INFO)

      IF (INFO == 0) THEN
         WRITE(*,*) 'Eigenvalue solution successful!'
         WRITE(*,*) ' '
         WRITE(*,*) 'Mode       Eigenvalue       Frequency (Hz)'
         WRITE(*,*) '-----------------------------------------'
         ! Print first 15 modes
         DO I = 1, MIN(15, NDDL)
            IF (W(I) > 0.0D0) THEN
               FREQ = SQRT(W(I)) / (2.0D0 * PI)
            ELSE
               FREQ = 0.0D0
            END IF
            WRITE(*, '(I4,2E17.6)') I, W(I), FREQ
         END DO
      ELSE
         WRITE(*,*) 'DSYGV failed with INFO = ', INFO
      END IF

      ! Clean up
      DEALLOCATE(A, B, W, WORK)
      DEALLOCATE(IADK, JDIK, DIAG_K, LT_K, LB, IDDL, NDOF, MS, IN)

      END SUBROUTINE RADIOSS_EIGEN
