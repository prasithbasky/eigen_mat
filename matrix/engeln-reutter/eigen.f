      INTEGER FUNCTION EIGEN
     *   (BASIS,ND,N,MAT,SKAL,EIVEC,WERTR,WERTI,CNT,LOW,HIGH)
C
C*****************************************************************
C                                                                *
C     ZWECK DES PROGRAMMS:                                       *
C     ====================                                       *
C     DIESES FUNKTIONSUNTERPROGRAMM VOM TYP INTEGER BERECHNET    *
C     ALLE EIGENWERTE UND EIGENVEKTOREN EINER BELIEBIGEN         *
C     REELLEN MATRIX MAT.                                        *
C     DIE EIGENWERTE WERDEN IN DEN FELDERN WERTR(1:N)            *
C     (REALTEIL) UND WERTI(1:N) (IMAGINAERTEIL) UND DIE          *
C     EIGENVEKTOREN IM FELD EIVEC(1:N,1:N) ABGESPEICHERT.        *
C                                                                *
C     EINGABEPARAMETER:                                          *
C     =================                                          *
C     N:           ORDNUNG DER MATRIX MAT                        *
C     ND:          FUEHRENDE DIMENSION DER FELDER MAT UND        *
C                  EIVEC, WIE IM RUFENDEN PROGRAMM VEREINBART    *
C     MAT:         EIN (N,N)-FELD VOM TYP DOUBLE PRECISION,      *
C                  DAS DIE MATRIX ENTHAELT, DEREN EIGENWERTE     *
C                  UND EIGENVEKTOREN BERECHNET WERDEN SOLLEN     *
C     BASIS:       DIE BASIS DER GLEITKOMMADARSTELLUNG DES       *
C                  VERWENDETEN RECHNERS (MEISTENS 2 ODER 16)     *
C                                                                *
C     AUSGABEPARAMETER:                                          *
C     =================                                          *
C     MAT:         DER OBERE TEIL DIESES (N,N)-FELDES ENTHAELT   *
C                  DIE EIGENVEKTOREN DER QUASI-DREIECKSMATRIX,   *
C                  DIE VOM QR-VERFAHREN ERZEUGT WIRD.            *
C     WERTR,WERTI: ZWEI (N,1)-FELDER VOM TYP DOUBLE              *
C                  PRECISION, DIE REALTEIL UND IMAGINAER-        *
C                  TEIL DER EIGENWERTE AUFNEHMEN                 *
C     EIVEC:       EIN (N,N)-FELD VOM TYP DOUBLE PRECISION,      *
C                  DAS DIE NORMALISIERTEN EIGENVEKTOREN DER      *
C                  URSPRUENGLICHEN VOLLEN MATRIX AUFNIMMT.       *
C                  FALLS DER I-TE EIGENWERT REELL IST, DANN      *
C                  IST DIE I-TE SPALTE VON EIVEC DER DAZUGE-     *
C                  HOERIGE REELLE EIGENVEKTOR. FALLS DIE         *
C                  EIGENWERTE I UND I+1 EIN KOMPLEXES            *
C                  PAAR BILDEN, GEBEN I-TE UND (I+1)-TE          *
C                  SPALTE REAL- UND IMAGINAERTEIL DESJENIGEN     *
C                  EIGENVEKTORS AN, DER ZU DEM EIGENWERT MIT     *
C                  POSITIVEM IMAGINAERTEIL GEHOERT.              *
C     CNT:         EIN (N,1)-FELD VOM TYP INTEGER, DAS DIE       *
C                  ZAHL DER ITERATIONSSCHRITTE FUER JEDEN        *
C                  EIGENWERT AUFNIMMT. FALLS ZWEI EIGENWERTE     *
C                  ALS PAAR GLEICHZEITIG GEFUNDEN WERDEN, WIRD   *
C                  DIE ZAHL DER ITERATIONSSCHRITTE MIT EINEM     *
C                  POSITIVEN VORZEICHEN FUER DEN ERSTEN UND      *
C                  EINEM NEGATIVEN VORZEICHEN FUER DEN           *
C                  ZWEITEN EIGENWERT EINGETRAGEN.                *
C     SKAL:        EIN (N,1)-FELD VOM TYP DOUBLE PRECISION, DAS  *
C                  DIE INFORMATION UEBER DIE DURCHGEFUEHRTEN     *
C                  VERTAUSCHUNGEN UND DIE SKALIERUNGSFAKTOREN    *
C                  ENTHAELT.                                     *
C                                                                *
C     RUECKGABEWERTE:                                            *
C     ===============                                            *
C     0:      KEIN FEHLER                                        *
C     401:    DIE ORDNUNG N DER MATRIX MAT IST KLEINER ALS 1.    *
C     402:    MAT IST DIE NULLMATRIX.                            *
C     403:    DIE MAXIMALE SCHRITTZAHL VON FUER DAS              *
C             QR-VERFAHREN IST UEBER- SCHRITTEN, OHNE DASS       *
C             ALLE EIGENWERTE BERECHNET WERDEN KONNTEN.          *
C                                                                *
C     LOKALE GROESSEN:                                           *
C     ================                                           *
C     ONE,TWO,HALF: GLEITKOMMAKONSTANTEN                         *
C     EPS:          MASCHINENGENAUIGKEIT                         *
C     TEMP:         HILFSVARIABLE                                *
C                                                                *
C----------------------------------------------------------------*
C                                                                *
C  BENOETIGTE UNTERPROGRAMME: BALAN, ELMHES, ELMTRA, HQR2,       *
C                             BALBAK, NORMAL, SWAP, COMDIV,      *
C                             COMABS                             *
C                                                                *
C                                                                *
C  QUELLEN : 1. MARTIN, R. S. UND WILKINSON, J. H.,              *
C               SIEHE [MART70].                                  *
C            2. PARLETT, B. N. UND REINSCH, C., SIEHE [PARL69].  *
C            3. PETERS, G. UND WILKINSON, J. H., SIEHE [PETE70]. *
C                                                                *
C*****************************************************************
C                                                                *
C  AUTOR     : JUERGEN DIETEL                                    *
C  DATUM     : 10.04.1987                                        *
C  QUELLCODE : FORTRAN 77                                        *
C                                                                *
C*****************************************************************
C
      INTEGER BASIS,ND,N,CNT(N),LOW,HIGH
      DOUBLE PRECISION MAT(ND,N),SKAL(N),EIVEC(ND,N),WERTR(N),
     *                 WERTI(N)
      DOUBLE PRECISION ONE,TWO,HALF
      PARAMETER (ONE = 1.0D0,TWO = 2.0D0,HALF = 0.5D0)
      INTEGER RES,BALAN,ELMHES,ELMTRA,HQR2,BALBAK,NORMAL
      DOUBLE PRECISION EPS,TEMP
C
C     BERECHNUNG DER MASCHINENGENAUIGKEIT EPS (D. H. DER KLEINSTEN
C     POSITIVEN MASCHINENZAHL, FUER DIE AUF DEM RECHNER GILT: 
C     1 + EPS > 1): 
C
      TEMP = TWO
      EPS = ONE
   10 IF (ONE .LT. TEMP) THEN
          EPS = EPS * HALF
          TEMP = ONE + EPS
          GOTO 10
          ENDIF
      EPS = TWO * EPS
      RES = BALAN(ND,N,MAT,SKAL,LOW,HIGH,BASIS)
         IF (RES .NE. 0) THEN
            EIGEN = RES + 100
            RETURN
         ENDIF
      RES = ELMHES(ND,N,LOW,HIGH,MAT,CNT)
         IF (RES .NE. 0) THEN
            EIGEN = RES + 200
            RETURN
         ENDIF
      RES = ELMTRA(ND,N,LOW,HIGH,MAT,CNT,EIVEC)
         IF (RES .NE. 0) THEN
            EIGEN = RES + 300
            RETURN
         ENDIF
      RES = HQR2(ND,N,LOW,HIGH,MAT,WERTR,WERTI,EIVEC,CNT,EPS)
         IF (RES .NE. 0) THEN
            EIGEN = RES + 400
            RETURN
         ENDIF
      RES = BALBAK(ND,N,LOW,HIGH,SKAL,EIVEC)
         IF (RES .NE. 0) THEN
            EIGEN = RES + 500
            RETURN
         ENDIF
      RES = NORMAL(ND,N,EIVEC,WERTI)
         IF (RES .NE. 0) THEN
            EIGEN = RES + 600
            RETURN
         ENDIF
      EIGEN = 0
      RETURN
      END