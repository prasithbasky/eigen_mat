C
      INTEGER FUNCTION BALBAK(ND,N,LOW,HIGH,SKAL,EIVEC)
C
C*****************************************************************
C                                                                *
C     ZWECK DES PROGRAMMS:                                       *
C     ====================                                       *
C     BALBAK FUEHRT EINE RUECKTRANSFORMATION ALLER RECHTSEIGEN-  *
C     VEKTOREN EINER AUSBALANCIERTEN MATRIX IN DIE EIGENVEKTOREN *
C     DER ORIGINALMATRIX DURCH, VON DER DIE BALANCIERTE MATRIX   *
C     DURCH AUFRUF DER PROZEDUR BALAN ABGELEITET WURDE.          *
C                                                                *
C     EINGABEPARAMETER:                                          *
C     =================                                          *
C     ND:       FUEHRENDE DIMENSION DER MATRIZEN, WIE SIE IM     *
C               HAUPTPROGRAMM VEREINBART WURDEN                  *
C     N:        DIE ORDNUNG DER EIGENVEKTOREN (ZAHL DER          *
C               KOMPONENTEN)                                     *
C     LOW,HIGH: ZWEI INTEGERZAHLEN, DIE VON DER PROZEDUR         *
C               BALAN STAMMEN                                    *
C     SKAL:     AUSGABEVEKTOR DER PROZEDUR BALAN                 *
C     EIVEC:    EIN (1:N,1:N)-FELD, VON DEM JEDE SPALTE EINEN    *
C               EIGENVEKTOR (ODER SEINEN REALTEIL ODER SEINEN    *
C               IMAGINAERTEIL) DER AUSBALANCIERTEN MATRIX        *
C               DARSTELLT                                        *
C                                                                *
C     AUSGABEPARAMETER:                                          *
C     =================                                          *
C     EIVEC:    DIE ENTSPRECHENDEN EIGENVEKTOREN (ODER REAL-     *
C               TEILE ODER IMAGINAERTEILE) DER                   *
C               URSPRUENGLICHEN MATRIX                           *
C                                                                *
C     RUECKGABEWERT:                                             *
C     ==============                                             *
C     0:        KEIN FEHLER                                      *
C                                                                *
C     LOKALE VARIABLEN:                                          *
C     =================                                          *
C     I,J,K:    HILFSVARIABLEN ZUR INDEXBILDUNG                  *
C     S:   :    SKALIERUNGSWERT                                  *
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
      INTEGER ND,N,LOW,HIGH
      DOUBLE PRECISION SKAL(N),EIVEC(ND,N)
      INTEGER I,J,K
      DOUBLE PRECISION S
C
      DO 20 I=LOW,HIGH
         S = SKAL(I)
C
C        LINKSEIGENVEKTOREN WERDEN ZURUECKTRANSFORMIERT, INDEM MAN
C        DIE VORIGE ANWEISUNG ERSETZT DURCH: 'S = 1.0D0/SKAL(I)'
C
         DO 10 J=1,N
   10       EIVEC(I,J) = EIVEC(I,J)*S
   20    CONTINUE
      DO 40 I=LOW-1,1,-1
         K=SKAL(I)
         DO 30 J=1,N
   30       CALL SWAP(EIVEC(I,J),EIVEC(K,J))
   40    CONTINUE
      DO 60 I=HIGH+1,N
         K=SKAL(I)
         DO 50 J=1,N
   50       CALL SWAP(EIVEC(I,J),EIVEC(K,J))
   60    CONTINUE
      BALBAK = 0
      END