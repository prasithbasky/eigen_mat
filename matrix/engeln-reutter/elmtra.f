C
      INTEGER FUNCTION ELMTRA(ND,N,LOW,HIGH,MAT,PERM,H)
C
C*****************************************************************
C                                                                *
C     ZWECK DES PROGRAMMS:                                       *
C     ====================                                       *
C     DIEJENIGE MATRIX IN DEM FELD H(1:N,1:N) ABSPEICHERN, DIE   *
C     SICH AUS DEN INFORMATIONEN ERGIBT, DIE DIE PROZEDUR ELMHES *
C     HINTERLASSEN HAT IM UNTEREN DREIECK DER HESSENBERGMATRIX   *
C     H, UND ZWAR IM FELD MAT(1:N,1:N) UND IM INTEGERFELD        *
C     PERM(1:N)                                                  *
C                                                                *
C     EINGABEPARAMETER:                                          *
C     =================                                          *
C     ND:       FUEHRENDE DIMENSION DER MATRIZEN, WIE SIE IM     *
C               HAUPTPROGRAMM VEREINBART WURDEN                  *
C     N:        ORDNUNG DER HESSENBERGMATRIX H                   *
C     LOW,HIGH: INTEGERZAHLEN, DIE VON DER PROZEDUR BALAN        *
C               ERZEUGT WURDEN (FALLS SIE VERWANDT WURDE;        *
C               ANDERNFALLS SETZE LOW:=1, HIGH:=N.)              *
C     PERM:     EIN VON ELMHES ERZEUGTES (N,1)-INTEGERFELD       *
C     MAT:      EIN (N,N)-FELD, DAS VON ELMHES ERZEUGT WURDE     *
C               UND DIE HESSENBERGMATRIX H UND DIE               *
C               MULTIPLIKATOREN ENTHAELT, DIE BENUTZT WURDEN,    *
C               UM ES AUS DER ALLGEMEINEN MATRIX                 *
C               A ZU ERZEUGEN                                    *
C                                                                *
C     AUSGABEPARAMETER:                                          *
C     =================                                          *
C     H:        DASJENIGE (N,N)-FELD, DAS DIE AEHNLICHKEITS-     *
C               TRANSFORMATION VON A IN H DEFINIERT              *
C                                                                *
C     RUECKGABEWERT:                                             *
C     ==============                                             *
C     0:             KEIN FEHLER                                 *
C                                                                *
C     LOKALE GROESSEN:                                           *
C     ================                                           *
C     ZERO,ONE: GLEITKOMMAKONSTANTEN                             *
C     I,J,K:    INDEXVARIABLEN                                   *
C                                                                *
C----------------------------------------------------------------*
C                                                                *
C  BENOETIGTE UNTERPROGRAMME: KEINE                              *
C                                                                *
C                                                                *
C  QUELLEN : PETERS, G. UND WILKINSON, J. H., SIEHE [PETE70].    *
C                                                                *
C*****************************************************************
C                                                                *
C  AUTOR     : JUERGEN DIETEL                                    *
C  DATUM     : 10.04.1987                                        *
C  QUELLCODE : FORTRAN 77                                        *
C                                                                *
C*****************************************************************
C

      INTEGER ND,N,LOW,HIGH,PERM(N)
      DOUBLE PRECISION MAT(ND,N),H(ND,N)
      DOUBLE PRECISION ZERO,ONE
      PARAMETER (ZERO = 0.0D0,ONE = 1.0D0)
      INTEGER I,J,K
      DO 20 I=1,N
         DO 10 J=1,N
   10       H(I,J) = ZERO
         H(I,I) = ONE
   20    CONTINUE
C
      DO 50 I=HIGH-1,LOW+1,-1
         J=PERM(I)
         DO 30 K=I+1,HIGH
   30       H(K,I) = MAT(K,I-1)
         IF (I .NE. J) THEN
            DO 40 K=I,HIGH
               H(I,K)=H(J,K)
               H(J,K)=ZERO
   40          CONTINUE
            H(J,I) = ONE
         ENDIF
   50    CONTINUE
      ELMTRA = 0
      END