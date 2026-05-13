C
      INTEGER FUNCTION ELMHES(ND,N,LOW,HIGH,MAT,PERM)
C
C*****************************************************************
C                                                                *
C     ZWECK DES PROGRAMMS:                                       *
C     ====================                                       *
C     GEGEBEN IST EINE UNSYMMETRISCHE MATRIX A(1:N,1:N). DANN    *
C     REDUZIERT DIESE PROZEDUR DIE TEILMATRIX DER ORDNUNG        *
C     HIGH-LOW+1, DIE BEIM ELEMENT A(LOW,LOW) BEGINNT UND BEIM   *
C     ELEMENT A(HIGH,HIGH) ENDET, AUF HESSENBERGFORM H DURCH     *
C     NICHTORTHOGONALE ELEMENTARTRANSFORMATIONEN. DIE TEILMATRIX *
C     WIRD MIT H UEBERSCHRIEBEN, WOBEI DIE EINZELHEITEN DER      *
C     TRANSFORMATIONEN IN DEM UEBRIGBLEIBENDEN DREIECK UNTERHALB *
C     VON H UND IN DEM FELD PERM ABGESPEICHERT WERDEN.           *
C                                                                *
C     EINGABEPARAMETER:                                          *
C     =================                                          *
C     ND:       FUEHRENDE DIMENSION DER MATRIZEN, WIE SIE IM     *
C               HAUPTPROGRAMM VEREINBART WURDEN                  *
C     N:        ORDNUNG DER VOLLEN MATRIX A                      *
C     LOW,HIGH: AUSGABEPARAMETER EINER PROZEDUR, DIE             *
C               A AUFBEREITET (SIEHE 'PARLETT, B. N., AND C.     *
C               REINSCH: BALANCING A MATRIX FOR CALCULATION      *
C               OF EIGENVALUES AND EIGENVECTORS. NUMERISCHE      *
C               MATHEMATIK 13 (1969), SEITEN 293 - 304,          *
C               PROZEDUR BALANCE'). FALLS A NICHT DERART         *
C               AUFBEREITET IST, SETZE LOW:=1, HIGH:=N.          *
C     MAT:      DIE (N,N)-MATRIX A, NORMALERWEISE IN             *
C               AUFBEREITETER FORM (SIEHE OBEN)                  *
C                                                                *
C     AUSGABEPARAMETER:                                          *
C     =================                                          *
C     MAT:      EIN (N,N)-FELD, DAS ZU EINEM TEIL AUS DER        *
C               ABGELEITETEN OBEREN HESSENBERGMATRIX BESTEHT;    *
C               DIE GROESSE N(I,R+1), DIE BEI DER REDUKTION      *
C               EINE ROLLE SPIELT, WIRD IM (I,R)-ELEMENT         *
C               ABGESPEICHERT.                                   *
C     PERM:     EIN INTEGERFELD, DAS DIE BEI DER REDUKTION       *
C               AUSGEFUEHRTEN ZEILEN- UND SPALTENVERTAU-         *
C               SCHUNGEN BESCHREIBT                              *
C                                                                *
C     RUECKGABEWERT:                                             *
C     ==============                                             *
C     0:        KEIN FEHLER                                      *
C                                                                *
C     LOKALE GROESSEN:                                           *
C     ================                                           *
C     ZERO,ONE: GLEITKOMMAKONSTANTEN                             *
C     I,J,M:    ZAEHLVARIABLEN                                   *
C     X,Y:      HILFSVARIABLEN ZUR AUFNAHME VON MATRIXELEMENTEN  *
C               UND ZWISCHENERGEBNISSEN                          *
C                                                                *
C----------------------------------------------------------------*
C                                                                *
C  BENOETIGTE UNTERPROGRAMME: SWAP                               *
C                                                                *
C                                                                *
C  QUELLEN : MARTIN, R. S. UND WILKINSON, J. H., SIEHE [MART70]. *
C                                                                *
C*****************************************************************
C                                                                *
C  AUTOR     : JUERGEN DIETEL                                    *
C  DATUM     : 10.04.1987                                        *
C  QUELLCODE : FORTRAN 77                                        *
C                                                                *
C*****************************************************************
C
      INTEGER N,LOW,HIGH,PERM(N)
      DOUBLE PRECISION MAT(ND,N)
      DOUBLE PRECISION ZERO,ONE
      PARAMETER (ZERO = 0.0D0,ONE = 1.0D0)
      INTEGER I,J,M
      DOUBLE PRECISION X,Y
C
      DO 70 M=LOW+1,HIGH-1
         I = M
         X = ZERO
         DO 10 J=M,HIGH
            IF (ABS(MAT(J,M-1)) .GT. ABS(X)) THEN
               X = MAT(J,M-1)
               I=J
            ENDIF
   10       CONTINUE
         PERM(M) = I
         IF (I .NE. M) THEN
C
C           ZEILEN UND SPALTEN VON MAT VERTAUSCHEN
C
            DO 20 J=M-1,N
   20          CALL SWAP (MAT(I,J),MAT(M,J))
            DO 30 J=1,HIGH
   30          CALL SWAP (MAT(J,I),MAT(J,M))
         ENDIF
         IF (X .NE. ZERO) THEN
            DO 60 I=M+1,HIGH
               Y = MAT(I,M-1)
               IF (Y .NE. ZERO) THEN
                  Y = Y/X
                  MAT(I,M-1) = Y
                  DO 40 J=M,N
   40                MAT(I,J) = MAT(I,J)-Y*MAT(M,J)
                  DO 50 J=1,HIGH
   50                MAT(J,M) = MAT(J,M)+Y*MAT(J,I)
               ENDIF
   60          CONTINUE
         ENDIF
   70    CONTINUE
      ELMHES = 0
      END