C
        SUBROUTINE MAVE(A,X,Y,N,ND)
C
C*****************************************************************
C*                                                               *
C*  BERECHNUNG VON  Y = A * X                                    *
C*                                                               *
C*****************************************************************
C
        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
        DIMENSION A(ND,ND),X(ND),Y(ND)
        DO 200 I=1,N
           Y(I) = 0.0D0
           DO 100 J=1,N
              Y(I) = Y(I) + A(I,J) * X(J)
 100       CONTINUE
 200    CONTINUE
        RETURN
        END