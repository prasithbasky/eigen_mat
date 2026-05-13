C
      INTEGER FUNCTION HQR2 (ND,N,LOW,HIGH,H,WERTR,WERTI,
     *                       EIVEC,CNT,EPS)
C
C*****************************************************************
C                                                                *
C     ZWECK DES PROGRAMMS:                                       *
C     ====================                                       *
C     FINDET DIE EIGENWERTE UND EIGENVEKTOREN EINER REELLEN      *
C     MATRIX, DIE, AUF OBERE HESSENBERGFORM REDUZIERT, IM FELD   *
C     H(1:N,1:N) STEHT, WOBEI DAS PRODUKT DER BISHER DURCHGE-    *
C     FUEHRTEN TRANSFORMATIONEN IM FELD EIVEC(1:N,1:N) STEHT.    *
C     DIE REAL- UND DIE IMAGINAERTEILE DER EIGENWERTE WERDEN IN  *
C     DEN FELDERN WERTR(1:N),WERTI(1:N) UND DIE EIGENVEKTOREN IM *
C     FELD EIVEC(1:N,1:N) GEBILDET, WO NUR EIN KOMPLEXER VEKTOR, *
C     NAEMLICH DER ZU DER WURZEL MIT POSITIVEM IMAGINAERTEIL GE- *
C     HOERIGE, FUER JEDES KOMPLEXE PAAR VON EIGENWERTEN ERZEUGT  *
C     WIRD. LOW UND HIGH SIND ZWEI INTEGERZAHLEN, DIE BEIM AUS-  *
C     BALANCIEREN ENTSTEHEN, WO EIGENWERTE IN DEN POSITIONEN 1   *
C     BIS LOW-1 UND HIGH+1 BIS N ISOLIERT WERDEN.  FALLS KEINE   *
C     AUSBALANCIERUNG DURCHGEFUEHRT WURDE, SETZE LOW:=1,HIGH:=N. *
C     DAS UNTERPROGRAMM BRICHT MIT EINER FEHLERMELDUNG AB, FALLS *
C     IRGENDEIN EIGENWERT MEHR ALS MAXSTP ITERATIONSSCHRITTE     *
C     BENOETIGT.                                                 *
C                                                                *
C     EINGABEPARAMETER:                                          *
C     =================                                          *
C     N:        ORDNUNG DER HESSENBERGMATRIX H                   *
C     LOW,HIGH: VON BALAN ERZEUGTE INTEGERZAHLEN, FALLS          *
C               BALAN BENUTZT WURDE. ANSONSTEN SETZE             *
C               LOW:=1, HIGH:=N.                                 *
C     EPS:      DIE KLEINSTE ZAHL AUF DEM COMPUTER, FUER         *
C               DIE GILT: 1 + EPS > 1.                           *
C     H:        EIN (N,N)-FELD, DAS DIE MATRIX H IN IHREN        *
C               RELEVANTEN TEILEN ENTHAELT                       *
C     EIVEC:    EIN (N,N)-FELD, DAS DIE MATRIX ENTHAELT, DIE     *
C               DIE AEHNLICHKEITSTRANSFORMATION VON A IN H       *
C               DEFINIERT. (ES WIRD VON ELMTRA ERZEUGT.)         *
C               FALLS H DIE URSPRUENGLICHE MATRIX IST,           *
C               SETZE EIVEC := EINHEITSMATRIX.                   *
C                                                                *
C     AUSGABEPARAMETER:                                          *
C     =================                                          *
C     H:           DER OBERE TEIL DIESES (N,N)-FELDES ENT-       *
C                  HAELT DIE EIGENVEKTOREN DER QUASI-            *
C                  DREIECKSMATRIX, DIE VOM QR-VERFAHREN          *
C                  ERZEUGT WIRD.                                 *
C     WERTR,WERTI: ZWEI (N,1)-FELDER, DIE REAL- UND IMAGI-       *
C                  NAERTEIL DER EIGENWERTE AUFNEHMEN             *
C     CNT:         EIN (N,1)-INTEGERFELD, DAS DIE ZAHL DER       *
C                  ITERATIONSSCHRITTE FUER JEDEN EIGENWERT       *
C                  AUFNIMMT. FALLS ZWEI EIGENWERTE ALS PAAR      *
C                  GLEICHZEITIG GEFUNDEN WERDEN, DANN WIRD       *
C                  DIE ZAHL DER ITERATIONSSCHRITTE MIT EINEM     *
C                  POSITIVEN VORZEICHEN FUER DEN ERSTEN UND      *
C                  EINEM NEGATIVEN VORZEICHEN FUER DEN           *
C                  ZWEITEN EIGENWERT EINGETRAGEN.                *
C     EIVEC:       EIN (N,N)-FELD, DAS DIE NICHTNORMALISIER-     *
C                  TEN EIGENVEKTOREN DER URSPRUENGLICHEN         *
C                  VOLLEN MATRIX AUFNIMMT (FALLS H NICHT DIE     *
C                  AUSGANGSMATRIX WAR). FALLS DER I-TE EIGEN-    *
C                  WERT REELL IST, DANN IST DIE I-TE SPALTE      *
C                  VON EIVEC DER DAZUGEHOERIGE REELLE EIGEN-     *
C                  VEKTOR. FALLS DIE EIGENWERTE I UND I+1 EIN    *
C                  KOMPLEXES PAAR BILDEN, GEBEN I-TE UND         *
C                  (I+1)-TE SPALTE REAL- UND IMAGINAERTEIL       *
C                  DESJENIGEN EIGENVEKTORS AN, DER ZU DEM        *
C                  EIGENWERT MIT POSITIVEM IMAGINAERTEIL         *
C                  GEHOERT.                                      *
C                                                                *
C     RUECKGABEWERTE:                                            *
C     ===============                                            *
C     0:           KEIN FEHLER                                   *
C     1:           DIE PARAMETER N, LOW ODER HIGH HABEN          *
C                  UNERLAUBTE WERTE.                             *
C     2:           ALLE EIGENVEKTOREN SIND DER NULLVEKTOR.       *
C     3:           DIE MAXIMALE SCHRITTZAHL IST UEBERSCHRITTEN.  *
C                                                                *
C     LOKALE GROESSEN:                                           *
C     ================                                           *
C     ZERO,ONE,TWO,PT75,PT4375: WICHTIGE GLEITKOMMAKONSTANTEN    *
C     MAXSTP:                   KONSTANTE FUER DIE MAXIMALE      *
C                               SCHRITTZAHL                      *
C     I,J,K,L,M,N,NA,EN:        INDEXVARIABLEN                   *
C     ITER:                     SCHRITTZAEHLER                   *
C     P,Q,R,S,T,W,X,Y,Z,NORM,                                    *
C       RA,SA,VR,VI:            HILFSVARIABLEN FUER GLEITKOMMA-  *
C                               BERECHNUNGEN                     *
C                                                                *
C----------------------------------------------------------------*
C                                                                *
C  BENOETIGTE UNTERPROGRAMME: COMDIV                             *
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
      INTEGER ND,N,LOW,HIGH,CNT(N)
      DOUBLE PRECISION H(ND,N),EIVEC(ND,N),WERTR(N),WERTI(N),EPS
      DOUBLE PRECISION ZERO,ONE,TWO,PT75,PT4375
      INTEGER MAXSTP
      PARAMETER (ZERO = 0.0D0,ONE = 1.0D0,TWO = 2.0D0,
     *           PT75 = 0.75D0,PT4375 = 0.4375D0,MAXSTP = 100)
      INTEGER I,J,K,L,M,NA,EN,ITER
      DOUBLE PRECISION P,Q,R,S,T,W,X,Y,Z,NORM,RA,SA,VR,VI
C
C     FEHLER 1: DIE PARAMETER N, LOW ODER HIGH HABEN UNERLAUBTE
C     WERTE:
C
      IF (N .LT. 1 .OR. LOW .LT. 1 .OR. HIGH .GT. N) THEN
         HQR2 = 1
         RETURN
      ENDIF
C
C     VORBESETZUNG FUER DIE BEI DER AUSBALANCIERUNG GEFUNDENEN
C     ISOLIERTEN EIGENWERTE: 
C
      DO 10 I=1,N
         IF (I .LT. LOW .OR. I .GT. HIGH) THEN
            WERTR(I) = H(I,I)
            WERTI(I) = ZERO
            CNT(I) = 0
         ELSE
            CNT(I) = -1
         ENDIF
   10    CONTINUE
C
      EN = HIGH
      T = ZERO
   15    IF (EN .LT. LOW) GOTO 333
         ITER = 0
         NA = EN-1
C
C           NACH EINEM EINZELNEN KLEINEN SUBDIAGONALELEMENT
C           SUCHEN: 
C
   20       DO 30 L=EN,LOW+1,-1
   30          IF (ABS(H(L,L-1)) .LE. EPS*
     *             (ABS(H(L-1,L-1))+ABS(H(L,L)))) GOTO 40
   40       X = H(EN,EN)
            IF (L .EQ. EN) THEN
C
C              EINE WURZEL GEFUNDEN: 
C
               WERTR(EN) = X + T
               H(EN,EN) = WERTR(EN)
               WERTI(EN) = ZERO
               CNT(EN) = ITER
               EN = NA
               GOTO 15
            ENDIF
C
            Y = H(NA,NA)
            W = H(EN,NA)*H(NA,EN)
            IF (L .EQ. NA) THEN
C
C              ZWEI WURZELN GEFUNDEN: 
C
               P = (Y-X)/TWO
               Q = P*P+W
               Z = SQRT(ABS(Q))
               H(EN,EN) = X+T
               X = H(EN,EN)
               H(NA,NA) = Y+T
               CNT(EN) = -ITER
               CNT(NA) = ITER
               IF (Q .GE. ZERO) THEN
C
C                 EIN REELLES PAAR GEFUNDEN: 
C
                  IF (P .LT. ZERO) Z = -Z
                  Z = P+Z
                  WERTR(NA) = X+Z
                  WERTR(EN) = X-W/Z
                  WERTI(NA) = ZERO
                  WERTI(EN) = ZERO
                  X = H(EN,NA)
                  R = SQRT(X*X+Z*Z)
                  P = X/R
                  Q = Z/R
C
C                 ZEILENMODIFIKATION: 
C
                  DO 50 J=NA,N
                     Z = H(NA,J)
                     H(NA,J) = Q*Z+P*H(EN,J)
                     H(EN,J) = Q*H(EN,J)-P*Z
   50                CONTINUE
C
C                 SPALTENMODIFIKATION: 
C
                  DO 60 I=1,EN
                     Z = H(I,NA)
                     H(I,NA) = Q*Z+P*H(I,EN)
                     H(I,EN) = Q*H(I,EN)-P*Z
   60                CONTINUE
C
C                 AKKUMULATION: 
C
                  DO 70 I=LOW,HIGH
                     Z = EIVEC(I,NA)
                     EIVEC(I,NA) = Q*Z+P*EIVEC(I,EN)
                     EIVEC(I,EN) = Q*EIVEC(I,EN)-P*Z
   70                CONTINUE
               ELSE
C
C                 KOMPLEXES PAAR: 
C
                  WERTR(NA) = X+P
                  WERTR(EN) = WERTR(NA)
                  WERTI(NA) = Z
                  WERTI(EN) = -Z
               ENDIF
               EN = EN-2
               GOTO 15
            ENDIF
C
            IF (ITER .EQ. MAXSTP) THEN
C
C              FEHLER 3: MAXIMALE SCHRITTZAHL UEBERSCHRITTEN:
C
               CNT(EN) = MAXSTP+1
               HQR2 = 3
               RETURN
            ENDIF
            IF (MOD(ITER,10) .EQ. 0 .AND. ITER .NE. 0) THEN
C
C              EINEN UNGEWOEHNLICHEN SHIFT DURCHFUEHREN: 
C
               T = T+X
               DO 80 I=LOW,EN
   80             H(I,I) = H(I,I)-X
               S = ABS(H(EN,NA))+ABS(H(NA,EN-2))
               X = PT75*S
               Y = X
               W = -PT4375*S*S
            ENDIF
            ITER = ITER+1
C
C           NACH ZWEI AUFEINANDERFOLGENDEN KLEINEN
C           SUBDIAGONALELEMENTEN SUCHEN: 
C
            DO 90 M=EN-2,L,-1
               Z = H(M,M)
               R = X-Z
               S = Y-Z
               P = (R*S-W)/H(M+1,M)+H(M,M+1)
               Q = H(M+1,M+1)-Z-R-S
               R = H(M+2,M+1)
               S = ABS(P)+ABS(Q)+ABS(R)
               P = P/S
               Q = Q/S
               R = R/S
               IF (M .EQ. L) GOTO 100
               IF (ABS(H(M,M-1))*(ABS(Q)+ABS(R)) .LE. EPS*ABS(P)*
     *             (ABS(H(M-1,M-1))+ABS(Z)+ABS(H(M+1,M+1))))
     *             GOTO 100
   90          CONTINUE
  100       DO 110 I=M+2,EN
  110          H(I,I-2) = ZERO
            DO 120 I=M+3,EN
  120          H(I,I-3) = ZERO
C
C           EIN DOPPELTER QR-SCHRITT, DER DIE ZEILEN L BIS EN UND
C           DIE SPALTEN M BIS EN DES GANZEN FELDES BETRIFFT: 
C
            DO 200 K=M,NA
               IF (K .NE. M) THEN
                  P = H(K,K-1)
                  Q = H(K+1,K-1)
                  IF (K .NE. NA) THEN
                     R = H(K+2,K-1)
                  ELSE
                     R = ZERO
                  ENDIF
                  X = ABS(P)+ABS(Q)+ABS(R)
                  IF (X .EQ. ZERO) GOTO 200
                  P = P/X
                  Q = Q/X
                  R = R/X
               ENDIF
               S = SQRT(P*P+Q*Q+R*R)
               IF (P .LT. ZERO) S = -S
               IF (K .NE. M) THEN
                  H(K,K-1) = -S*X
               ELSEIF (L .NE. M) THEN
                  H(K,K-1) = -H(K,K-1)
               ENDIF
               P = P+S
               X = P/S
               Y = Q/S
               Z = R/S
               Q = Q/P
               R = R/P
C
C              ZEILENMODIFIKATION: 
C
               DO 130 J=K,N
                  P = H(K,J)+Q*H(K+1,J)
                  IF (K .NE. NA) THEN
                     P = P+R*H(K+2,J)
                     H(K+2,J) = H(K+2,J)-P*Z
                  ENDIF
                  H(K+1,J) = H(K+1,J)-P*Y
                  H(K,J) = H(K,J)-P*X
  130             CONTINUE
               J = MIN(K+3,EN)
C
C              SPALTENMODIFIKATION: 
C
               DO 140 I=1,J
                  P = X*H(I,K)+Y*H(I,K+1)
                  IF (K .NE. NA) THEN
                     P = P+Z*H(I,K+2)
                     H(I,K+2) = H(I,K+2)-P*R
                  ENDIF
                  H(I,K+1) = H(I,K+1)-P*Q
                  H(I,K) = H(I,K)-P
  140             CONTINUE
C
C              TRANSFORMATIONEN AKKUMULIEREN: 
C
               DO 150 I=LOW,HIGH
                  P = X*EIVEC(I,K)+Y*EIVEC(I,K+1)
                  IF (K .NE. NA) THEN
                     P = P+Z*EIVEC(I,K+2)
                     EIVEC(I,K+2) = EIVEC(I,K+2)-P*R
                  ENDIF
                  EIVEC(I,K+1) = EIVEC(I,K+1)-P*Q
                  EIVEC(I,K) = EIVEC(I,K)-P
  150             CONTINUE
  200          CONTINUE
            GOTO 20
C
C
C     ALLE WURZELN GEFUNDEN, NUN WIRD RUECKTRANSFORMIERT: 
C
C     1-NORM VON H BESTIMMEN:
C
  333 NORM = ZERO
      K=1
      DO 201 I=1,N
         DO 101 J=K,N
  101       NORM = NORM+ABS(H(I,J))
  201    K = I
      IF (NORM .EQ. ZERO) THEN
C        FEHLER 2: 1-NORM VON H IST GLEICH 0: 
         HQR2 = 2
         RETURN
      ENDIF
C
C     RUECKTRANSFORMATION: 
C
      DO 207 EN=N,1,-1
         P = WERTR(EN)
         Q = WERTI(EN)
         NA = EN - 1
         IF (Q .EQ. ZERO) THEN
C
C           REELLER VEKTOR: 
C
            M = EN
            H(EN,EN) = ONE
            DO 63 I=NA,1,-1
               W = H(I,I)-P
               R = H(I,EN)
               DO 38 J=M,NA
   38             R = R+H(I,J)*H(J,EN)
               IF (WERTI(I) .LT. ZERO) THEN
                  Z = W
                  S = R
               ELSE
                  M = I
                  IF (WERTI(I) .EQ. ZERO) THEN
                     IF (W .NE. ZERO) THEN
                        H(I,EN) = -R/W
                     ELSE
                        H(I,EN) = -R/(EPS*NORM)
                     ENDIF
                  ELSE
C
C                    LOESE DAS GLEICHUNGSSYSTEM: 
C                    [ W   X ] [ H(I,EN)   ]   [ -R ]
C                    [       ] [           ] = [    ]
C                    [ Y   Z ] [ H(I+1,EN) ]   [ -S ]
C
                     X = H(I,I+1)
                     Y = H(I+1,I)
                     Q = (WERTR(I)-P)*(WERTR(I) - P)+
     *                   WERTI(I)*WERTI(I)
                     T = (X*S-Z*R)/Q
                     H(I,EN) = T
                     IF (ABS(X) .GT. ABS(Z)) THEN
                        H(I+1,EN) = (-R-W*T)/X
                     ELSE
                        H(I+1,EN) = (-S-Y*T)/Z
                     ENDIF
                  ENDIF
               ENDIF
   63          CONTINUE
         ELSEIF (Q .LT. ZERO) THEN
C
C           KOMPLEXER VEKTOR, DER ZU LAMBDA = P - I * Q GEHOERT: 
C
            M = NA
            IF (ABS(H(EN,NA)) .GT. ABS(H(NA,EN))) THEN
               H(NA,NA) = -(H(EN,EN)-P)/H(EN,NA)
               H(NA,EN) = -Q/H(EN,NA)
            ELSE
               CALL COMDIV(-H(NA,EN),ZERO,H(NA,NA)-P,Q,
     *                   H(NA,NA),H(NA,EN))
            ENDIF
            H(EN,NA) = ONE
            H(EN,EN) = ZERO
            DO 190 I=NA-1,1,-1
               W = H(I,I)-P
               RA = H(I,EN)
               SA = ZERO
               DO 75 J=M,NA
                  RA = RA+H(I,J)*H(J,NA)
                  SA = SA+H(I,J)*H(J,EN)
   75             CONTINUE
               IF (WERTI(I) .LT. ZERO) THEN
                  Z = W
                  R = RA
                  S = SA
               ELSE
                  M = I
                  IF (WERTI(I) .EQ. ZERO) THEN
                      CALL COMDIV(-RA,-SA,W,Q,H(I,NA),H(I,EN))
                  ELSE
C
C                    LOESE DIE KOMPLEXEN GLEICHUNGEN:
C           [ W+Q*I   X   ] [H(I,NA)+H(I,EN)*I    ]   [-RA-SA*I]
C           [             ] [                     ] = [        ]
C           [   Y   Z+Q*I ] [H(I+1,NA)+H(I+1,EN)*I]   [-R-S*I  ]
C
                     X = H(I,I+1)
                     Y = H(I+1,I)
                     VR = (WERTR(I)-P)*(WERTR(I)-P)+
     *                    WERTI(I)*WERTI(I)-Q*Q
                     VI = TWO*Q*(WERTR(I)-P)
                     IF (VR .EQ. ZERO .AND. VI .EQ. ZERO) VR =
     *                  EPS*NORM*
     *                  (ABS(W)+ABS(Q)+ABS(X)+ABS(Y)+ABS(Z))
                     CALL COMDIV(X*R-Z*RA+Q*SA,X*S-Z*SA-Q*RA,
     *                           VR,VI,H(I,NA),H(I,EN))
                     IF (ABS(X) .GT. ABS(Z)+ABS(Q)) THEN
                        H(I+1,NA) = (-RA-W*H(I,NA)+Q*H(I,EN))/X
                        H(I+1,EN) = (-SA-W*H(I,EN)-Q*H(I,NA))/X
                     ELSE
                       CALL COMDIV(-R-Y*H(I,NA),-S-Y*H(I,EN),Z,Q,
     *                           H(I+1,NA),H(I+1,EN))
                     ENDIF
                  ENDIF
               ENDIF
  190          CONTINUE
         ENDIF
  207    CONTINUE
C
C     ZU DEN ISOLIERTEN WURZELN GEHOERIGE VEKTOREN: 
C
      DO 230 I=1,N
         IF (I .LT. LOW .OR. I .GT. HIGH) THEN
            DO 220 J=I+1,N
  220          EIVEC(I,J) = H(I,J)
         ENDIF
  230    CONTINUE
C
C     MIT DER TRANSFORMATIONSMATRIX MULTIPLIZIEREN, UM DIE
C     VEKTOREN DER URSPRUENGLICHEN VOLLEN MATRIX ZU ERHALTEN: 
C
      DO 300 J=N,LOW,-1
         IF (J .LE. HIGH) THEN
            M = J
         ELSE
            M = HIGH
         ENDIF
         L = J-1
         IF (WERTI(J) .LT. ZERO) THEN
            DO 330 I=LOW,HIGH
               Y = ZERO
               Z = ZERO
               DO 320 K=LOW,M
                  Y = Y+EIVEC(I,K)*H(K,L)
                  Z = Z+EIVEC(I,K)*H(K,J)
  320             CONTINUE
               EIVEC(I,L) = Y
               EIVEC(I,J) = Z
  330          CONTINUE
         ELSE
            IF (WERTI(J) .EQ. ZERO) THEN
               DO 350 I=LOW,HIGH
                  Z = ZERO
                  DO 340 K=LOW,M
  340                Z =Z+EIVEC(I,K)*H(K,J)
  350             EIVEC(I,J) = Z
            ENDIF
         ENDIF
  300    CONTINUE
C
C     RUECKGABE VON 0: KEIN FEHLER: 
C
      HQR2 = 0
      END