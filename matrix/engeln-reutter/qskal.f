C
        SUBROUTINE QSKAL(X,Y,R,N)
C
C*****************************************************************
C*                                                               *
C*  SKALARPRODUKT ZWEIER DOUBLE PRECISION VEKTOREN               *
C*                                                               *
C*****************************************************************
C
        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
        DIMENSION X(N),Y(N)
        R = 0.0D0
        DO 100 I=1,N
           R = R + X(I)*Y(I)
 100    CONTINUE
        RETURN
        END