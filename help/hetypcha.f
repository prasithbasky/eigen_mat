C*****7**1*********2*********3*********4*********5*********6*********7**
      SUBROUTINE HETYPCHA ( TSTRING, CHWERT, LCH, IERR )
C
C
CA*   Unterprogramm zum Herausgeben einer CHARACTER-Groesse aus dem
C     als CHARACTER erkannten Teilstring sowie seiner Laenge
C     
C
CB*   Name = HETYPCHA Hilfsroutine zum Interpretieren von Teilstrings
C            -------- als CHARACTER
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
C     - CHWERT     Interpretierte CHARACTER-Groesse
C     - LCH        besetzte Laenge des Characters
C     - IERR       Fehlerkennung
C                  IERR =   0, d.h. alles i.o.
C                  IERR = 800, Daten koennen nicht interpretiert werden.
C
C
C
CH*   INTERNE:
C     -------
C
C     - IFIRST     Pointer auf ersten Wert im zu untersuchenden String
C     - ILAST      Pointer auf letzten Wert im zu untersucheneden String
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
C     - SUBROUTINE CHLAEN
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
C     -------      aus Teilstring. CHARACTER und seine Laenge
C                  
C                  
C                  
C
C
CN*   BEMERKUNG:   Das Unterprogramm interpretiert den Teilstring
C     ---------    und gibt den CHARACTER-Wert an das rufende Programm
C                  zurueck. Bei Nichterkennung ist CHWERT = ' '
C                  Der Ausgabewert muss im rufenden Programm gross genug
C                  dimensioniert sein. Am besten genau so gross dimen-
C                  sionieren wie STRING
C
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
      INTEGER      IFIRST
      INTEGER      ILAST
      INTEGER      LCH
C
C ----------------------------------------------------------------------
C
      CHARACTER    TSTRING*(*)
      CHARACTER    CHWERT*(*)
C
C ======================================================================
C
      CALL HEINOU ( 'HETYPCHA', 'EIN', IERR )
C
C ======================================================================
CIT*  I N I T I A L I S I E R U N G S T E I L
C ======================================================================
C
      IERR    = 0
      IFIRST  = 1
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
      CALL CHLAEN ( TSTRING, LCH )
C
C *** Interpretieren des Teilstrings
C
      IF ( LCH .EQ. 0 ) THEN
         IERR = 0
         CHWERT = ' '      
         GOTO 9999
      ENDIF
C
      ILAST  = LCH
      CHWERT(IFIRST:ILAST) = TSTRING(IFIRST:ILAST)
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
      CALL HEINOU ( 'HETYPCHA', 'AUS', IERR )
C
      RETURN
      END
