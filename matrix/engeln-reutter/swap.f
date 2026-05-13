C
      SUBROUTINE SWAP(X,Y)
C
C*****************************************************************
C                                                                *
C     ZWECK DES PROGRAMMS:                                       *
C     ====================                                       *
C     SWAP VERTAUSCHT DIE WERTE DER BEIDEN DOUBLE PRECISION-     *
C     VARIABLEN X UND Y.                                         *
C                                                                *
C*****************************************************************
C
      DOUBLE PRECISION X,Y,TEMP
      TEMP = X
      X = Y
      Y = TEMP
      END