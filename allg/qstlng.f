C*****7**1*********2*********3*********4*********5*********6*********7**
      SUBROUTINE QSTLNG ( STRING, LAENG )
C
C
CA*   UNTERPROGRAMM ZUR ERRECHNEN DER LAENGE EINES STRINGS
C
CB*   QSTLNG = Q-LIB LAENGE STRING
C              -     -  --  --
C
C
CD*   PROGRAMM-SYSTEM:  BASISROUTINEN (Q-LIB)
C     ---------------
C
CE*   FUNKTION: STRINGVERARBEITUNG
C     --------
C
CS*   PROGRAMMIER-SPRACHE: FORTRAN 77
C     --------------------
C
C
CF*   EINGABE:
C     -------
C
C     - STRING     CHARCTER-STRING, DESSEN LAENGE ERRECHNENT WIRD
C
C
CG*   AUSGABE:
C     -------
C
C     - LAENG      LAENGE DES STRINGS
C
C
CH*   INTERNE:
C     -------
C
C     - GLAENG     GESAMTE LAENGE (DEFINITION) DES STRINGS
C     - DUMMY      HILFSSTRING
C     - IANF       ANFANGSADRESSE DES STRINGS IM UEBERGABECHARACTER
C     - IENDE      ENDADRESSE DES STRINGS IM UEBERGABECHARACTER
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
C     - SUBROUTINE Name
C
C
CL*   DIESE ROUTINE WIRD AUFGERUFEN VON:
C     ---------------------------------
C
C     - SUBROUTINE Name
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
CO*   AUTOR:       AXEL HAENSCHKE
C     -----        VW-GEDAS, Berlin
C                  Pascalstrasse 11
C                  Abteilung: Technische Simulation
C
C
CP*   VERSION:     0.1                                     27-MAI-91
C     -------
C
C
CQ*   UPDATE:      Name des Bearbeiters                    Datum
C     -------      Beschreibung der Aenderungen
C
C
C ----------------------------------------------------------------------
C
CR*   LITERATUR:   20
C     ---------
C
C*****7**1*********2*********3*********4*********5*********6*********7**
C
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
      INTEGER      I
      INTEGER      IANF
      INTEGER      IENDE
      INTEGER      GLAENG
      INTEGER      LAENG
C
C ----------------------------------------------------------------------
C
      CHARACTER    DUMMY*1
      CHARACTER    STRING*(*)
C
C ======================================================================
C
C
C ======================================================================
CIT*  I N I T I A L I S I E R U N G S T E I L
C ======================================================================
C
      IANF  = 0
      IENDE = 0
      LAENG = 0
C
C ======================================================================
CEV*  E I N G A B E V E R A R B E I T U N G
C ======================================================================
C
      GLAENG = LEN (STRING)
C
C ======================================================================
CVT*  V E R A R B E I T U N G S T E I L
C ======================================================================
C
C *** SUCHEN DER VORANSTEHENDEN BLANKS
C
      DO 100 I=1, GLAENG
         DUMMY = STRING (I:I)
         IF ( DUMMY .NE. ' ' ) THEN
            IANF = I
            GOTO 101
         ENDIF
  100 CONTINUE
  101 CONTINUE
C
C *** BESTIMMEN DER LAENGE DES STRINGS
C
      DO 200 I=IANF, GLAENG
         DUMMY = STRING (I:I)
         IF ( DUMMY .EQ. ' ' ) THEN
            IENDE = I-1
            GOTO 201
         ENDIF
  200 CONTINUE
  201 CONTINUE
C
      LAENG = ( IENDE + 1 ) - IANF
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
