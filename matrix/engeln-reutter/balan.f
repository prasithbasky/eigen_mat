C
      INTEGER FUNCTION BALAN (ND,N,MAT,SKAL,LOW,HIGH,BASIS)
C
C*****************************************************************
C                                                                *
C     ZWECK DES PROGRAMMS:                                       *
C     ====================                                       *
C     DIE PROZEDUR BALAN BALANCIERT EINE GEGEBENE REELLE MATRIX  *
C     IN DER 1-NORM AUS.                                         *
C                                                                *
C     EINGABEPARAMETER:                                          *
C     =================                                          *
C     ND:       FUEHRENDE DIMENSION DER MATRIZEN, WIE SIE IM     *
C               HAUPTPROGRAMM VEREINBART WURDEN                  *
C     N:        DIE ORDNUNG DER GEGEBENEN MATRIX                 *
C     MAT:      EIN (1:N,1:N)-FELD, DAS DIE KOMPONENTEN DER      *
C               GEGEBENEN MATRIX ENTHAELT                        *
C     BASIS:    DIE BASIS DER GLEITKOMMADARSTELLUNG AUF          *
C               DER MASCHINE                                     *
C                                                                *
C     AUSGABEPARAMETER:                                          *
C     =================                                          *
C     MAT:      DIE AUSBALANCIERTE MATRIX                        *
C     LOW,HIGH: ZWEI INTEGERZAHLEN, FUER DIE MAT(I,J)            *
C               GLEICH NULL IST, FALLS GILT:                     *
C               1. I>J UND                                       *
C               2. J=1,...LOW-1 ODER I=HIGH+1,...N               *
C     SKAL:     EIN (1:N)-FELD, DAS DIE INFORMATIONEN UEBER      *
C               DIE DURCHGEFUEHRTEN VERTAUSCHUNGEN UND DIE       *
C               SKALIERUNGSFAKTOREN ENTHAELT.                    *
C                                                                *
C     RUECKGABEWERT:                                             *
C     ==============                                             *
C     0:             KEIN FEHLER                                 *
C                                                                *
C     LOKALE GROESSEN:                                           *
C     ================                                           *
C     ZERO,ONE,PT95: GLEITKOMMAKONSTANTEN                        *
C     I,J,K,L:       ZAEHLVARIABLEN                              *
C     B2:            QUADRAT VON BASIS                           *
C     R,C,F,G,S:     HILFSVARIABLEN ZUR BERECHNUNG VON           *
C                    ZEILENNORMEN, IHREN KEHRWERTEN UND          *
C                    AEHNLICHEM                                  *
C                                                                *
C----------------------------------------------------------------*
C                                                                *
C  BENOETIGTE UNTERPROGRAMME: SWAP                               *
C                                                                *
C                                                                *
C  QUELLEN : PARLETT, B. N. UND REINSCH, C., SIEHE [PARL69].     *
C                                                                *
C*****************************************************************
C                                                                *
C  AUTOR     : JUERGEN DIETEL                                    *
C  DATUM     : 10.04.1987                                        *
C  QUELLCODE : FORTRAN 77                                        *
C                                                                *
C*****************************************************************
C
      INTEGER ND,N,LOW,HIGH,BASIS
      DOUBLE PRECISION SKAL(N),MAT(ND,N)
      DOUBLE PRECISION ZERO,ONE,PT95
      PARAMETER (ZERO = 0.0D0,ONE = 1.0D0,PT95 = 0.95D0)
      INTEGER I,J,K,L,B2
      DOUBLE PRECISION R,C,F,G,S
C
C     DIE NORM VON MAT(1:N,1:N) REDUZIEREN DURCH EXAKTE AEHNLICH-
C     KEITSTRANSFORMATIONEN, DIE IN SKAL(1:N) ABGESPEICHERT WERDEN
C
      B2 = BASIS*BASIS
      L = 1
      K = N
C
C     NACH ZEILEN MIT EINEM ISOLIERTEN EIGENWERT SUCHEN UND SIE
C     NACH UNTEN SCHIEBEN
C
   10 DO 50 J=K,1,-1
         R = ZERO
         DO 20 I=1,K
   20       IF (I .NE. J) R = R+ABS(MAT(J,I))
         IF (R .EQ. ZERO) THEN
            SKAL(K) = J
            IF (J .NE. K) THEN
               DO 30 I=1,K
   30             CALL SWAP(MAT(I,J),MAT(I,K))
               DO 40 I=L,N
   40             CALL SWAP(MAT(J,I),MAT(K,I))
            ENDIF
            K = K-1
            GOTO 10
         ENDIF
   50    CONTINUE
C
C     NACH SPALTEN MIT EINEM ISOLIERTEN EIGENWERT SUCHEN UND SIE
C     NACH LINKS SCHIEBEN
C
   60 DO 100 J=L,K
         C = ZERO
         DO 70 I=L,K
   70       IF (I .NE. J) C = C+ABS(MAT(I,J))
         IF (C .EQ. ZERO) THEN
            SKAL(L) = J
            IF (J .NE. L) THEN
               DO 80 I=1,K
   80             CALL SWAP(MAT(I,J),MAT(I,L))
               DO 90 I=L,N
   90             CALL SWAP(MAT(J,I),MAT(L,I))
            ENDIF
            L = L+1
            GOTO 60
         ENDIF
  100    CONTINUE
C
C     NUN DIE TEILMATRIX IN DEN ZEILEN L BIS K AUSBALANCIEREN
C
      LOW = L
      HIGH = K
      DO 110 I=L,K
  110    SKAL(I) = ONE
  120 DO 180 I=L,K
         C = ZERO
         R = ZERO
         DO 130 J=L,K
            IF (J .NE. I) THEN
               C = C+ABS(MAT(J,I))
               R = R+ABS(MAT(I,J))
            ENDIF
  130       CONTINUE
         G = R/BASIS
         F = ONE
         S = C+R
  140    IF (C .LT. G) THEN
            F = F*BASIS
            C = C*B2
            GOTO 140
         ENDIF
         G = R*BASIS
  150    IF (C .GE. G) THEN
            F = F/BASIS
            C = C/B2
            GOTO 150
         ENDIF
         IF ((C+R)/F .LT. PT95*S) THEN
            G = ONE/F
            SKAL(I) = SKAL(I)*F
            DO 160 J=L,N
  160          MAT(I,J) = MAT(I,J)*G
            DO 170 J=1,K
  170          MAT(J,I) = MAT(J,I)*F
            GOTO 120
         ENDIF
  180    CONTINUE
      BALAN = 0
      END