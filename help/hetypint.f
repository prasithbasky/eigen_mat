C*****7**1*********2*********3*********4*********5*********6*********7**
      SUBROUTINE HETYPINT ( TSTRING, IWERT, IERR )
C
C
CA*   Unterprogramm zum Herausgeben einer INTEGER-Groesse aus dem
C     als INTEGER erkannten Teilstring
C
C
CB*   Name = HETYPINT Hilfsroutine zum Interpretieren von Teilstrings
C            -------- als INTEGER
C
C
CD*   PROGRAMM-SYSTEM: HELP, Routinensammlung zur Programmentwicklung
C     ---------------
C
CE*   FUNKTION: Unterprogramm zum Interpretieren von Teilstrings
C     --------
C
CS*   PROGRAMMIER-SPRACHE: FORTRAN 77
C     --------------------
C
C
CF*   EINGABE:
C     -------
C
C     - TSTRING    zu interpretierender Teilstring
C
C
CG*   AUSGABE:
C     -------
C
C     - IWERT      Interpretierte Integer-Groesse
C     - IERR       Fehlerkennung
C                  IERR =   0, d.h. alles i.o.
C                  IERR = 800, Daten koennen nicht interpretiert werden.
C                              Sind kein INTEGER
C
C
CH*   INTERNE:
C     -------
C
C     - IBASE      Kennung, ab der der String interpretiert werden soll.
C     - IRES       Pointer zur erkannten Position im Auswahl-Opcode.
C     - IWERT      Integer Wert der Interpretation
C     - RWERT      Real Wert der Interpretation
C     - IFIRST     Pointer auf ersten Wert im zu untersuchenden Character
C     - ILAST      Pointer auf letzten Wert im zu untersucheneden Charact.
C
C
CI*   COMMON:
C     ------
C
C     - keine
C
C
CJ*   BENOETIGTE UNTERPROGRAMME / FUNKTIONEN:
C     --------------------------------------
C
C     - SUBROUTINE CRIANY
C     - SUBROUTINE HEINOU
C
C
CL*   DIESE ROUTINE WIRD AUFGERUFEN VON:
C     ---------------------------------
C
C     - vielen
C
C
CM*   ACHTUNG:     Die Routine interpretiert die vorher erkannten Typen
C     -------      aus Teilstring.
C
C
C
C
C
CN*   BEMERKUNG:   Das Unterprogramm interpretiert den Teilstring
C     ---------    und gibt den INTEGER-Wert an das rufende Programm
C                  zurueck. Bei Nichterkennung ist IWERT = 0
C
C
C
CO*   AUTOR:       Axel Haenschke
C     -----        Firma
C                  Anschrift der Firma
C                  Abteilung:
C
C
CP*   VERSION:     0.1                                     13-SEP-94
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
      INTEGER      IERR
      INTEGER      IBASE
      INTEGER      IRES
      INTEGER      IFIRST
      INTEGER      ILAST
      INTEGER      IWERT
      INTEGER      IAUS
C
C ----------------------------------------------------------------------
C
      REAL         RWERT
C
C ----------------------------------------------------------------------
C
      CHARACTER    TSTRING*(*)
C
C ======================================================================
C
      CALL HEINOU ( 'HETYPINT', 'EIN', IERR )
C
C ======================================================================
CIT*  I N I T I A L I S I E R U N G S T E I L
C ======================================================================
C
      IERR  = 0
      IBASE = 1
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
      CALL CRIANY ( 'IRANS', IBASE, TSTRING, IRES, IAUS, RWERT, IFIRST,
     +               ILAST )
C
C *** Interpretieren des Teilstrings
C
      IF ( IRES .EQ. 1 ) THEN
C *** Integer
         IWERT = IAUS
         IERR = 0
         GOTO 9999
      ELSE IF ( IRES .GT. 1 ) THEN
C *** kein Integer
         IWERT = 0
         IERR = 800
         GOTO 9999
      ENDIF
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
      CALL HEINOU ( 'HETYPINT', 'AUS', IERR )
C
      RETURN
      END
