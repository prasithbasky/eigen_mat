      SUBROUTINE EIWERT(A,N,ND,M0,EPSI,X,Y,Z,EW,IER)
C
C*****************************************************************
C*                                                               *
C*  BERECHNUNG DES BETRAGSGROESSTEN EIGENWERTES UND DES          *
C*  DAZUGEHOERIGEN EIGENVEKTORS EINER MATRIX A.                  *
C*  VERFAHREN VON VON MISES (KAP. 7.3.1.)                        *
C*                                                               *
C*                                                               *
C*   EINGABEPARAMETER:                                           *
C*   =================                                           *
C*   A   : 2-DIM DOUBLE PRECISION FELD (1:ND,1:N); EINGABEMATRIX *
C*   N   : ORDNUNG VON A                                         *
C*   ND  : FUEHRENDE DIMENSION VON A,WIE IN DEM RUFENDEN         *
C*         PROGRAMM VEREINBART                                   *
C*   M0  : MAXIMALE ITERATIONSZAHL                               *
C*   EPSI: GEFORDERTE RELATIVE FEHLERGENAUIGKEIT                 *
C*         (GROESSER ALS 1E-12) (DOUBLE PRECISION)               *
C*   X   : 1-DIM DOUBLE PRECISION FELD (1:N) ; ARBEITSSPEICHER   *
C*                                                               *
C*                                                               *
C*   AUSGABEPARAMETER:                                           *
C*   =================                                           *
C*   Y  : 1-DIM DOUBLE PRECISION FELD (1:N); EIGENVEKTOR         *
C*   Z  : 1-DIM DOUBLE PRECISION FELD (1:N); VEKTOR DER RESIDUEN *
C*   EW : EIGENWERT (DOUBLE PRECISION)                           *
C*   IER: FEHLERCODE =0: BERECHNUNG ERFOLGREICH BEENDET          *
C*                   =1: MAXIMALE ANZAHL VON ITERATIONEN ERREICHT*
C*                       EIGENWERT/VEKTOR IST KOMPLEX ODER       *
C*                       PROBLEM IST ZU SCHLECHT KONDITIONIERT   *
C*                                                               *
C*---------------------------------------------------------------*
C*                                                               *
C*  BENOETIGTE UNTERPROGRAMME: DBNORM, MAVE, QUOT, QSKAL         *
C*                                                               *
C*                                                               *
C*  QUELLEN : G.ENGELN-MUELLGES / F. REUTTER, FORMELSAMMLUNG     *
C*            ZUR NUMERISCHEN MATHEMATIK MIT STANDARD-FORTRAN-   *
C*            PROGRAMMEN, 4.AUFLAGE 1984,BI, S. 331              *
C*                                                               *
C*****************************************************************
C*                                                               *
C*  AUTOR     : JUERGEN BECKMANN                                 *
C*  DATUM     : 24.10.1985                                       *
C*  QUELLCODE : FORTRAN 77                                       *
C*                                                               *
C*****************************************************************
C
        IMPLICIT DOUBLE PRECISION (A-H,O-Z)
        DIMENSION A(ND,ND),Z(ND),X(ND),Y(ND)
        IER = 0
        DO 100 I=1,N
           Y(I) = 1.0D0
 100    CONTINUE
        CALL DBNORM(Y,N)
        EW = 0.0D0
        ITER  = 0
 200    ITER  = ITER + 1
        EM    = EW
        DO 300 I=1,N
           X(I) = Y(I)
 300    CONTINUE
        CALL MAVE(A,X,Y,N,ND)
        EW = QUOT(X,Y,N)
        CALL DBNORM(Y,N)
        IF (ITER .EQ. 1) THEN
           GOTO 200
        END IF
        DO 400 I = 1,N
           Z(I) = Y(I) - X(I)
 400    CONTINUE
        CALL QSKAL(Z,Z,S,N)
        S = DSQRT(S)
        IF (ITER .EQ. M0) THEN
           IER = 1
           GOTO 500
        END IF
        IF(S .GT. EPSI .OR. DABS(EW-EM) .GT. EPSI) THEN
           GOTO 200
        END IF
 500    RETURN
        END