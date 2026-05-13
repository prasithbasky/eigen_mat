C*****7**1*********2*********3*********4*********5*********6*********7**
      SUBROUTINE HETYPDB ( TSTRING, DBWERT, IERR )
C
C
CA*   Unterprogramm zum Herausgeben einer DOUBLE-PRECISION-Groesse aus
C     dem als DB erkannten Teilstring. DOUBLE PRECISION liegt im
C     D-Format vor, wobei das d klein oder gross geschrieben ist.
C
C
CB*   Name = HETYPDB  Hilfsroutine zum Interpretieren von Teilstrings
C            -------  als DOUBLE PRECISION im D-Format
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
C     - DBWERT     Interpretierte DOUBLE PRECISION-Groesse
C     - IERR       Fehlerkennung
C                  IERR =   0, d.h. alles i.o.
C                  IERR = 801, Daten koennen nicht interpretiert werden.
C                              Sind kein DOUBLE PRECISION
C
C
CH*   INTERNE:
C     -------
C
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
C     - SUBROUTINE UPCASE
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
C     ---------    und gibt den DB-Wert an das rufende Programm
C                  zurueck. Bei Nichterkennung ist DB = 0.0D+0
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
      INTEGER      LNTSTR
C
C ----------------------------------------------------------------------
C
      DOUBLE PRECISION DBWERT
C
C ----------------------------------------------------------------------
C
      CHARACTER    TSTRING*(*)
      CHARACTER    CHDUMM*80
C
C ======================================================================
C
      CALL HEINOU ( 'HETYPDB', 'EIN', IERR )
C
C ======================================================================
CIT*  I N I T I A L I S I E R U N G S T E I L
C ======================================================================
C
      IERR  = 0
C
C ======================================================================
CEV*  E I N G A B E V E R A R B E I T U N G
C ======================================================================
C
C *** Stringumwandlung
C
      CALL CHLAEN (TSTRING, LNTSTR)
      CALL UPCASE (TSTRING(1:LNTSTR), CHDUMM(1:LNTSTR) )
C
C ======================================================================
CVT*  V E R A R B E I T U N G S T E I L
C ======================================================================
C
C *** Umwandlung
C
      READ (CHDUMM(1:LNTSTR), *, ERR=2222 ) DBWERT
      GOTO 9999
C
C ======================================================================
CAT*  A U S G A B E T E I L
C ======================================================================
C
C *** FEHLERMELDUNGEN:
C
 2222 CONTINUE
      IERR = 801
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
      CALL HEINOU ( 'HETYPDB', 'AUS', IERR )
C
      RETURN
      END
