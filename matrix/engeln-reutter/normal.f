C
      INTEGER FUNCTION NORMAL (ND,N,V,WI)
C
C*****************************************************************
C                                                                *
C     ZWECK DES PROGRAMMS:                                       *
C     ====================                                       *
C     NORMAL NORMALISIERT DIE EIGENVEKTOREN IN DER MAXIMUMNORM.  *
C                                                                *
C     EINGABEPARAMETER:                                          *
C     =================                                          *
C     ND:     FUEHRENDE DIMENSION DER MATRIX V, WIE SIE IM       *
C             HAUPTPROGRAMM VEREINBART WURDE                     *
C     N:      DIE ORDNUNG DER MATRIX V                           *
C     V:      EIN (N,N)-FELD VOM TYP DOUBLE PRECISION, DAS       *
C             SPALTENWEISE DIE EIGENVEKTOREN ENTHAELT            *
C             (SIEHE EIGEN)                                      *
C     WI:     EIN FELD MIT N KOMPONENTEN VOM TYP                 *
C             DOUBLE PRECISION, DESSEN KOMPONENTEN DIE           *
C             IMAGINAERTEILE DER EIGENWERTE SIND                 *
C                                                                *
C     AUSGABEPARAMETER:                                          *
C     =================                                          *
C     V:      MATRIX DER NORMALISIERTEN EIGENVEKTOREN            *
C                                                                *
C     LOKALE GROESSEN:                                           *
C     ================                                           *
C     ZERO,ONE: GLEITKOMMAKONSTANTEN 0 UND 1                     *
C     I,J:      INDEXVARIABLEN                                   *
C     MAXI:     HILFSVARIABLE ZUR BERECHNUNG DER REELLEN         *
C               VEKTORNORM                                       *
C     TR,TI:    HILFSVARIABLEN ZUR BERECHNUNG DER KOMPLEXEN      *
C               VEKTORNORM                                       *
C                                                                *
C----------------------------------------------------------------*
C                                                                *
C  BENOETIGTE UNTERPROGRAMME: COMABS, COMDIV                     *
C                                                                *
C*****************************************************************
C                                                                *
C  AUTOR     : JUERGEN DIETEL                                    *
C  DATUM     : 10.04.1987                                        *
C  QUELLCODE : FORTRAN 77                                        *
C                                                                *
C*****************************************************************
C
      INTEGER ND,N
      DOUBLE PRECISION V(ND,N),WI(N)
      DOUBLE PRECISION ZERO,ONE
      PARAMETER (ZERO = 0.0D0,ONE = 1.0D0)
      INTEGER I,J
      DOUBLE PRECISION MAXI,TR,TI,COMABS
C
      J = 1
   10 IF (J .GT. N) GOTO 80
         IF (WI(J) .EQ. ZERO) THEN
             MAXI = V(1,J)
             DO 15 I=2,N
   15           IF (ABS(V(I,J)) .GT. ABS(MAXI)) MAXI = V(I,J)
             IF (MAXI .NE. ZERO) THEN
                MAXI = ONE/MAXI
                DO 20 I=1,N
   20              V(I,J) = V(I,J)*MAXI
             ENDIF
             J = J+1
         ELSE
            TR = V(1,J)
            TI = V(1,J+1)
            DO 30 I=2,N
               IF (COMABS(V(I,J),V(I,J+1)) .GT. COMABS(TR,TI))
     *         THEN
                  TR = V(I,J)
                  TI = V(I,J+1)
               ENDIF
   30          CONTINUE
            IF (TR .NE. ZERO .OR. TI .NE. ZERO) THEN
               DO 40 I=1,N
   40             CALL COMDIV (V(I,J),V(I,J+1),TR,TI,
     *                         V(I,J),V(I,J+1))
            ENDIF
            J = J+2
         ENDIF
         GOTO 10
   80 NORMAL = 0
      END