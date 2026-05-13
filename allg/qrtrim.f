C*****7**1*********2*********3*********4*********5*********6*********7**
      CHARACTER*(*) FUNCTION QRTRIM(STRING)
C
C
CA*   ENTFERNEN VON FOLGENDEN BLANKS AUS CHARACTER STRINGS
C
CB*   NAME = QRTRIM
C            ------
C
C
CD*   PROGRAMM-SYSTEM:
C     ---------------
C
CE*   FUNKTION: FOLGENDE BLANKS ENTFERNEN
C     --------
C
CS*   PROGRAMMIER-SPRACHE: FORTRAN 77
C     --------------------
C
C
CF*   EINGABE:
C     -------
C
C     - STRING     ORIGINALSTRING
C
C
CG*   AUSGABE:
C     -------
C
C     - QRTRIM     VON FOLGENDENN BLANKS BEFREITER STRING
C                  (TYP WIE STRING IM RUFENDEN PROGRAMM)
C
CH*   INTERNE:
C     -------
C
CJ*   BENOETIGTE UNTERPROGRAMME / FUNKTIONEN:
C     --------------------------------------
C
C     - KEINE
C
C
CL*   DIESE ROUTINE WIRD AUFGERUFEN VON:
C     ---------------------------------
C
C     - SUBROUTINE
C
CM*   ACHTUNG:
C     -------
C
C
CO*   AUTOR:       MICHAEL HASSA
C     -----        VW-GEDAS
C                  ABTEILUNG: PES/SIM  (TECHNISCHE SIMULATION)
C
C
C
CP*   VERSION:     0.1                                     28-AUG-90
C     -------
C
C
CQ*   UPDATE:      NAME DES BEARBEITERS                    DATUM
C     -------      BESCHREIBUNG DER AENDERUNGEN
C
C ----------------------------------------------------------------------
C
CR*   LITERATUR:   20
C     ---------
C
C*****7**1*********2*********3*********4*********5*********6*********7**
C
C ======================================================================
CVD*  V A R I A B L E N   D E K L A R A T I O N
C ======================================================================
C
C
      IMPLICIT     NONE
C
C ----------------------------------------------------------------------
C
      INTEGER      ILAST, I, LS
C
C ----------------------------------------------------------------------
C
      CHARACTER * (*)   STRING
C
C ======================================================================
C
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
C***  REMOVE BLANKS AM ENDE

      ILAST  =  0
      LS     =  LEN ( STRING )

      DO 100 I = LS, 1, -1
         IF ( STRING(I:I).NE.' ' .AND. STRING(I:I).NE.CHAR(0) ) THEN
             ILAST = I
             GOTO 110
         END IF
  100 CONTINUE

  110 CONTINUE
C
C *** WENN NULLSTRING DER LAENGE=1 DANN SETZE BLANK ******
      IF ( ILAST .EQ. 0 ) THEN
         STRING(1:1) = ' '
         ILAST = 1
      ENDIF
C ----------------------------------------------------------

      DO 50  I = 1, LS - ILAST
50      QRTRIM (I:I) =  ' '

      QRTRIM = QRTRIM ( 1:LS -ILAST ) // STRING ( 1:ILAST )

      GOTO 9999
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
