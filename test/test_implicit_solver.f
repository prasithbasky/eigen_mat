      PROGRAM TEST_IMPLICIT_SOLVER
      IMPLICIT NONE

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

      ! Dense matrix for LAPACK
      DOUBLE PRECISION, ALLOCATABLE :: A(:,:)
      DOUBLE PRECISION, ALLOCATABLE :: WORK_RHS(:)
      INTEGER, ALLOCATABLE :: IPIV(:)

      ! Sparse Matrix-Vector product vector (for residual check)
      DOUBLE PRECISION, ALLOCATABLE :: Y(:)

      ! Local variables
      INTEGER UNIT_NO, IERR, I, J, P, G, GJ, INFO
      CHARACTER(LEN=100) FNAME
      DOUBLE PRECISION RES_NORM, RHS_NORM, REL_RES
      LOGICAL SOLVER_CHOLESKY

      FNAME = 'implicit_matrices.bin'
      UNIT_NO = 10

      WRITE(*,*) '========================================='
      WRITE(*,*) '  STANDALONE IMPLICIT SOLVER TEST ENV'
      WRITE(*,*) '========================================='
      WRITE(*,*) 'Reading binary data from: ', FNAME

      OPEN(UNIT=UNIT_NO, FILE=FNAME, STATUS='OLD',
     .     FORM='UNFORMATTED', ACCESS='STREAM', IOSTAT=IERR)
      IF (IERR /= 0) THEN
        WRITE(*,*) 'Error: Unable to open file ', FNAME
        WRITE(*,*) 'Please run OpenRadioss with the dump subroutine'
        WRITE(*,*) 'first to generate this file.'
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
      WRITE(*,*) '  NDDLI  = ', NDDLI
      WRITE(*,*) '  NNZI   = ', NNZI
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
      ALLOCATE(WORK_RHS(NDDL))
      ALLOCATE(IPIV(NDDL))
      ALLOCATE(Y(NDDL))

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

      ! Replicate Hook Point A: Sparse-to-Dense Assembly
      WRITE(*,*) 'Assembling dense system matrix...'
      A = 0.0D0

      ! Fill diagonal
      DO I = 1, NDDL
        A(I, I) = DIAG_K(I)
      END DO

      ! Fill off-diagonals (CSR lower triangle -> dense lower and upper)
      DO I = 1, NDDL
        DO P = IADK(I), IADK(I+1)-1
          J = JDIK(P)
          A(I, J) = LT_K(P)
          A(J, I) = LT_K(P) ! symmetric copy
        END DO
      END DO

      ! Merge contact interface matrix if NDDLI > 0
      IF (NDDLI > 0) THEN
        WRITE(*,*) 'Merging contact interface matrix...'
        DO I = 1, NDDLI
          G = ITOK(I)
          A(G, G) = A(G, G) + DIAG_I(I)
          DO P = IADI(I), IADI(I+1)-1
            GJ = ITOK(JDII(P))
            A(G, GJ) = A(G, GJ) + LT_I(P)
            A(GJ, G) = A(GJ, G) + LT_I(P) ! symmetric
          END DO
        END DO
      END IF

      ! Copy RHS vector to work vector
      WORK_RHS = LB

      ! Factorize and solve (Hook Point B)
      WRITE(*,*) 'Solving linear system K_eff * x = LB...'
      SOLVER_CHOLESKY = .TRUE.

      ! Try Cholesky factorization DPOTRF (Symmetric Positive Definite)
      WRITE(*,*) 'Attempting Cholesky factorization (DPOTRF)...'
      CALL DPOTRF('L', NDDL, A, NDDL, INFO)

      IF (INFO == 0) THEN
        WRITE(*,*) 'Cholesky factorization succeeded.'
        ! Solve using DPOTRS
        CALL DPOTRS('L', NDDL, 1, A, NDDL, WORK_RHS, NDDL, INFO)
        IF (INFO /= 0) THEN
          WRITE(*,*) 'Error: DPOTRS failed with INFO = ', INFO
        END IF
      ELSE
        WRITE(*,*) 'Warning: DPOTRF failed with INFO = ', INFO
        WRITE(*,*) 'Matrix is not Positive Definite (e.g., constraints/Lagrange multipliers).'
        WRITE(*,*) 'Falling back to General LU solver (DGETRF + DGETRS)...'
        SOLVER_CHOLESKY = .FALSE.

        ! Re-assemble dense matrix because DPOTRF partially overwrote it
        WRITE(*,*) 'Re-assembling dense system matrix for LU...'
        A = 0.0D0
        DO I = 1, NDDL
          A(I, I) = DIAG_K(I)
        END DO
        DO I = 1, NDDL
          DO P = IADK(I), IADK(I+1)-1
            J = JDIK(P)
            A(I, J) = LT_K(P)
            A(J, I) = LT_K(P)
          END DO
        END DO
        IF (NDDLI > 0) THEN
          DO I = 1, NDDLI
            G = ITOK(I)
            A(G, G) = A(G, G) + DIAG_I(I)
            DO P = IADI(I), IADI(I+1)-1
              GJ = ITOK(JDII(P))
              A(G, GJ) = A(G, GJ) + LT_I(P)
              A(GJ, G) = A(GJ, G) + LT_I(P)
            END DO
          END DO
        END IF

        ! Factorize using DGETRF
        CALL DGETRF(NDDL, NDDL, A, NDDL, IPIV, INFO)
        IF (INFO == 0) THEN
          WRITE(*,*) 'LU factorization (DGETRF) succeeded.'
          ! Solve using DGETRS
          CALL DGETRS('N', NDDL, 1, A, NDDL, IPIV, WORK_RHS, NDDL, INFO)
          IF (INFO /= 0) THEN
            WRITE(*,*) 'Error: DGETRS failed with INFO = ', INFO
          END IF
        ELSE
          WRITE(*,*) 'Fatal Error: LU factorization failed with INFO =',
     .               INFO
          STOP
        END IF
      END IF

      ! Solution is now in WORK_RHS
      WRITE(*,*) 'Linear solve completed successfully.'

      ! Verify solution by computing residual: r = K_eff * x - b
      ! using the original sparse matrix representation
      Y = 0.0D0
      DO I = 1, NDDL
        Y(I) = Y(I) + DIAG_K(I) * WORK_RHS(I)
        DO P = IADK(I), IADK(I+1)-1
          J = JDIK(P)
          Y(I) = Y(I) + LT_K(P) * WORK_RHS(J)
          Y(J) = Y(J) + LT_K(P) * WORK_RHS(I)
        END DO
      END DO

      IF (NDDLI > 0) THEN
        DO I = 1, NDDLI
          G = ITOK(I)
          Y(G) = Y(G) + DIAG_I(I) * WORK_RHS(G)
          DO P = IADI(I), IADI(I+1)-1
            GJ = ITOK(JDII(P))
            Y(G) = Y(G) + LT_I(P) * WORK_RHS(GJ)
            Y(GJ) = Y(GJ) + LT_I(P) * WORK_RHS(G)
          END DO
        END DO
      END IF

      RES_NORM = 0.0D0
      RHS_NORM = 0.0D0
      DO I = 1, NDDL
        RHS_NORM = RHS_NORM + LB(I)**2
        RES_NORM = RES_NORM + (Y(I) - LB(I))**2
      END DO
      RHS_NORM = SQRT(RHS_NORM)
      RES_NORM = SQRT(RES_NORM)

      WRITE(*,*) '-----------------------------------------'
      WRITE(*,*) 'Verification Metrics:'
      WRITE(*,*) '  Solver used       : ',
     .            MERGE('Cholesky (DPOTRF)', 'LU (DGETRF)      ',
     .                  SOLVER_CHOLESKY)
      WRITE(*,*) '  RHS Norm ||b||    : ', RHS_NORM
      WRITE(*,*) '  Residual ||K*x-b||: ', RES_NORM
      IF (RHS_NORM > 1.0D-12) THEN
        REL_RES = RES_NORM / RHS_NORM
        WRITE(*,*) '  Relative Residual : ', REL_RES
        IF (REL_RES < 1.0D-6) THEN
          WRITE(*,*) '  RESULT            : SUCCESS (Accurate solve)'
        ELSE
          WRITE(*,*) '  RESULT            : WARNING (High residual)'
        END IF
      ELSE
        WRITE(*,*) '  RESULT            : N/A (Zero RHS)'
      END IF
      WRITE(*,*) '========================================='

      ! Clean up
      DEALLOCATE(IADK)
      DEALLOCATE(JDIK)
      DEALLOCATE(DIAG_K)
      DEALLOCATE(LT_K)
      IF (NDDLI > 0) THEN
        DEALLOCATE(IADI)
        DEALLOCATE(JDII)
        DEALLOCATE(ITOK)
        DEALLOCATE(DIAG_I)
        DEALLOCATE(LT_I)
      END IF
      DEALLOCATE(LB)
      DEALLOCATE(IDDL)
      DEALLOCATE(NDOF)
      DEALLOCATE(MS)
      DEALLOCATE(IN)
      DEALLOCATE(A)
      DEALLOCATE(WORK_RHS)
      DEALLOCATE(IPIV)
      DEALLOCATE(Y)

      END PROGRAM TEST_IMPLICIT_SOLVER
