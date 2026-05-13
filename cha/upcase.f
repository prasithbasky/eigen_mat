C*****7**1*********2*********3*********4*********5*********6*********7**
      SUBROUTINE UPCASE (WORT1,WORT2)
C
C
CA*   SETZT SRING VON KLEINSCHREIBUNG AUF GROSSSCHREIBUNG
C
CB*   NAME = UPCASE
C            ------
C
C
CD*   PROGRAMM-SYSTEM:
C     ---------------
C
CE*   FUNKTION: HILFSROUTINE
C     --------
C
CS*   PROGRAMMIER-SPRACHE: FORTRAN 77
C     --------------------
C
C
CF*   EINGABE:
C     -------
C
C     WORT1        EINGABESTRING
C
C
CG*   AUSGABE:
C     -------
C
C     WORT2        AUSGABESTRING MIT GROSSBUCHSTABEN
C
C
CH*   INTERNE:
C     -------
C
C     - 9          20
C
C
CI*   COMMON:
C     ------
C
C     -            KEINE
C
C
CJ*   BENOETIGTE UNTERPROGRAMME / FUNKTIONEN:
C     --------------------------------------
C
C     - SUBROUTINE NAME
C
C
CL*   DIESE ROUTINE WIRD AUFGERUFEN VON:
C     ---------------------------------
C
C     - SUBROUTINE NAME
C
C
CM*   ACHTUNG:     KEINE
C     -------
C
C
CN*   BEMERKUNG:   KONTROLL-AUSGABEN SIND MIT "CKA" GEKENNZEICHNET
C     ---------
C
C
CO*   AUTOR:       Axel Haenschke
C     -----        VW-GEDAS
C                  ABTEILUNG: PES/SIM  (TECHNISCHE SIMULATION)
C
C
C
CP*   VERSION:     0.1                                     04-AUG-89
C     -------
C
C
CQ*   UPDATE:      UWE BERNHART                            04-AUG-89
C     -------
C
C
C ----------------------------------------------------------------------
C
CR*   LITERATUR:  'MEINE ERSTE SCHULFIBEL'
C     ---------
C
C*****7**1*********2*********3*********4*********5*********6*********7**
C
C
C ======================================================================
CVD*  V A R I A B L E N   D E K L A R A T I O N
C ======================================================================
C
      CHARACTER * (*) WORT1
      CHARACTER * (*) WORT2
C
C ----------------------------------------------------------------------
C
      INTEGER         I
C
C ======================================================================
C
C ======================================================================
CIT*  I N I T I A L I S I E R U N G S T E I L
C ======================================================================
C
C
C ======================================================================
CEV*  E I N G A B E V E R A R B E I T U N G
C ======================================================================
C
C
C ======================================================================
CVT*  V E R A R B E I T U N G S T E I L
C ======================================================================
C
      DO 10 I = 1,LEN (WORT1)
         IF (ICHAR(WORT1(I:I)) .GE. 97 .AND.
     &       ICHAR(WORT1(I:I)) .LE. 122 ) THEN
            WORT2(I:I) = CHAR(ICHAR(WORT1(I:I)) - 32)
         ELSE
            WORT2(I:I) =  WORT1(I:I)
         ENDIF
  10  CONTINUE
C
C ======================================================================
CAT*  A U S G A B E T E I L
C ======================================================================
C
C *** FEHLERMELDUNGEN:
C
C
C ======================================================================
CFA*  F O R M A T  - A N W E I S U N G E N
C ======================================================================
C
C
C ======================================================================
C
 9999 CONTINUE
C
      RETURN
      END
