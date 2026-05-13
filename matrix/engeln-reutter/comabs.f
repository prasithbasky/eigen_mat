C
      DOUBLE PRECISION FUNCTION COMABS(AR,AI)
C
C*****************************************************************
C                                                                *
C     ZWECK DES PROGRAMMS:                                       *
C     ====================                                       *
C     BERECHNUNG DES BETRAGES DER KOMPLEXEN ZAHL AR+I*AI:        *
C     COMABS:=SQRT(AR*AR+AI*AI)                                  *
C                                                                *
C     EINGABEPARAMETER:                                          *
C     =================                                          *
C     AR,AI: REAL- UND IMAGINAERTEIL DER KOMPLEXEN ZAHL, DEREN   *
C            BETRAG ZU BERECHNEN IST                             *
C                                                                *
C     AUSGABEPARAMETER:                                          *
C     =================                                          *
C     KEINE                                                      *
C                                                                *
C     RUECKGABEWERT:                                             *
C     ==============                                             *
C     BETRAG DES KOMPLEXEN PARAMETERS                            *
C                                                                *
C     LOKALE GROESSEN:                                           *
C     ================                                           *
C     ZERO,ONE:    KONSTANTEN                                    *
C     TEMP1,TEMP2: HILFSVARIABLEN ZUR SPEICHERUNG VON            *
C                  ZWISCHENERGEBNISSEN                           *
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
      DOUBLE PRECISION AR,AI
      DOUBLE PRECISION ZERO,ONE
      PARAMETER (ZERO = 0.0D0,ONE = 1.0D0)
      DOUBLE PRECISION TEMP1,TEMP2
      TEMP1 = ABS(AR)
      TEMP2 = ABS(AI)
      IF (AR .EQ. ZERO .OR. AI .EQ. ZERO) THEN
         COMABS = ZERO
         RETURN
      ENDIF
      IF (TEMP2 .GT. TEMP1) CALL SWAP (TEMP1,TEMP2)
      IF (TEMP2 .EQ. ZERO) THEN
         COMABS = TEMP1
      ELSE
         COMABS = TEMP1*SQRT(ONE+(TEMP2/TEMP1)**2)
      ENDIF
      END