      PROGRAM TEST_K_EIGEN
      IMPLICIT NONE
      INTEGER NDDL, NNZK, NDDLI, NNZI, NUMNOD
      INTEGER, ALLOCATABLE :: IADK(:), JDIK(:)
      DOUBLE PRECISION, ALLOCATABLE :: DIAG_K(:), LT_K(:)
      DOUBLE PRECISION, ALLOCATABLE :: A(:,:), W(:), WORK(:)
      INTEGER UNIT_NO, IERR, I, J, P, INFO, LWORK
      
      OPEN(UNIT=10, FILE='implicit_matrices.bin', STATUS='OLD',
     .     FORM='UNFORMATTED', ACCESS='STREAM', IOSTAT=IERR)
      READ(10) NDDL, NNZK, NDDLI, NNZI, NUMNOD
      
      ALLOCATE(IADK(NDDL+1), JDIK(NNZK), DIAG_K(NDDL), LT_K(NNZK))
      READ(10) IADK, JDIK, DIAG_K, LT_K
      CLOSE(10)
      
      ALLOCATE(A(NDDL, NDDL), W(NDDL))
      LWORK = MAX(1, 3*NDDL)
      ALLOCATE(WORK(LWORK))
      
      A = 0.0D0
      DO I = 1, NDDL
        A(I, I) = DIAG_K(I)
        DO P = IADK(I), IADK(I+1)-1
          J = JDIK(P)
          A(J, I) = LT_K(P) ! Upper triangle
        END DO
      END DO
      
      CALL DSYEV('N', 'U', NDDL, A, NDDL, W, WORK, LWORK, INFO)
      
      WRITE(*,*) 'First 15 eigenvalues of K:'
      DO I = 1, 15
         WRITE(*, '(I4,E17.6)') I, W(I)
      END DO
      END PROGRAM TEST_K_EIGEN
