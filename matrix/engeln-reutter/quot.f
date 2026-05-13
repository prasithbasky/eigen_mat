C 
        DOUBLE PRECISION FUNCTION QUOT(X,Y,N)
C
C*****************************************************************
C*                                                               *
C*  HILFSROUTINE ZU EIWERT                                       *
C*                                                               *
C*****************************************************************
C
        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
        DIMENSION X(N),Y(N)
        QUOT = 1.0D0
        S    = 0.0D0
        N1   = 0
        DO 100 I=1,N
           IF (X(I) .NE. 0.0D0) THEN
              S  = S + Y(I) / X(I)
              N1 = N1 + 1
           END IF
 100    CONTINUE
        IF (N1 .NE. 0) THEN
           QUOT = S / DBLE(N1)
        END IF
        RETURN
        END