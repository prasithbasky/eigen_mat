C
        SUBROUTINE DBNORM(X,N)
C
C*****************************************************************
C*                                                               *
C*  NORMIERUNG EINES DOUBLE PRECISION VEKTORS AUF NORM  1        *
C*                                                               *
C*****************************************************************
C
        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
        DIMENSION X(N)
        CALL QSKAL(X,X,S,N)
        S = DSQRT(S)
        IF (S .NE. 0.0D0) THEN
           DO 100 I=1,N
              X(I) = X(I) / S
 100       CONTINUE
        END IF
        RETURN
        END