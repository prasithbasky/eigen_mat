
C*****7**1*********2*********3*********4*********5*********6*********7**
      SUBROUTINE HECHTYP ( TSTRING, CHTYP, IERR )
C
C
CA*   Unterprogramm zum erkennen des Teilstring-Typs
C
C
CB*   Name = HECHTYP Hilfsroutine zum Interpretieren von Teilstrings
C            -------
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
C     - CHTYP      Typ des interpretierten Teilstrings
C                  CHTYP  = 'I', d.h. eine Integer-Groesse liegt vor
C                  CHTYP  = 'F', d.h. eine Real-Groesse wurde gefunden
C                            Real im F-Format vorliegen.
C                  CHTYP  = 'E', d.h. eine Real-Groesse wurde gefunden
C                            Real E-Format vorliegen.
C                  CHTYP  = 'D', d.h. eine Real-Groesse vom Typ
C                            DOUBLE PRECISION wurde erkennt.
C                  CHTYP  = 'C', Teilstring ist mit einem Character-
C                            Wert versehen.
C                  CHTYP  = 'A', Teilstring ist rein alphabetisch und
C                            mit grossen Charactern
C                  CHTYP  = 'X', Teilstring ist real aber im E-Format
C                            mit kleinem e+nn
C                  CHTYP  = 'Y', Teilstring ist DOUBLE PRECISION, aber
C                            mit kleinem d+nn.
C                  CHTYP  = 'Z', Teilstring ist DOUBLE PRECISION, aber
C                            mit grossem D+nn.
C     - IERR       Fehlerkennung
C                  IERR =   0, d.h. alles i.o.
C                  IERR = 900, Daten koennen nicht interpretiert werden.
C                            Werden als Typ Character angezeigt.
C
C
CH*   INTERNE:
C     -------
C
C     - IBASE      Kennung, ab der der String interpretiert werden soll.
C     - IRES       Pointer zur erkannten Position im Auswahl-Opcode.
C     - IWERT      Integer Wert der Interpretation
C     - RWERT      Real Wert der Interpretation
C     - DWERT      Double Precision Wert der Interpretation
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
C     - SUBROUTINE HECHDOT
C     - SUBROUTINE HETYPREE
C     - SUBROUTINE HETYPCHE
C     - SUBROUTINE HETYPDBE
C     - SUBROUTINE HEINOU
C
C
CL*   DIESE ROUTINE WIRD AUFGERUFEN VON:
C     ---------------------------------
C
C     - vielen
C
C
CM*   ACHTUNG:     Die Routine erkennt die Typen der uebergebenen
C     -------      Teilstrings. Teilstrings duerfen nicht durch ein
C                  Komma getrennt werden, da sonst dieser Teilstring
C                  als zwei Strings interpretiert wird.
C
C
C
CN*   BEMERKUNG:   Das Unterprogramm erkennt den Typ des Teilstrings
C     ---------    und gibt diesen an das rufende Programm zurueck.
C                  Das Unterprogramm kann weiterhin auch die Typen
C
C                     CHTYP = 'N', alphanumerical
C
C                  erkennen. Wird aber hier nicht weiter verwendet.
C                  Siehe HECHTYP1 als allgemeine Erweiterung.
C
C
C
CO*   AUTOR:       Axel Haenschke
C     -----        Firma
C                  Anschrift der Firma
C                  Abteilung:
C
C
CP*   VERSION:     0.1                                     05-SEP-94
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
C
C ----------------------------------------------------------------------
C
      REAL         RWERT
C
C ----------------------------------------------------------------------
C
      CHARACTER    TSTRING*(*)
      CHARACTER    CHTYP*1
      CHARACTER    CHDOT*1
      CHARACTER    CHREALE*1
      CHARACTER    CHREALS*1
      CHARACTER    CHREALD*1
C
C ======================================================================
C
      CALL HEINOU ( 'HECHTYP', 'EIN', IERR )
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
      CHTYP   = ' '
      CHDOT   = ' '
      CHREALE = ' '
      CHREALS = ' '
      CHREALD = ' '
C
C ======================================================================
CVT*  V E R A R B E I T U N G S T E I L
C ======================================================================
C
C
      CALL CRIANY ( 'IRANS', IBASE, TSTRING, IRES, IWERT, RWERT, IFIRST,
     +               ILAST )
C
C
C
C *** Erkennen des Teilstrings
C
C
      IF ( IRES .EQ. 1 ) THEN
C
C *** Integer
C
         CALL HECHDOT ( TSTRING, CHDOT, IERR )
         IF ( CHDOT(1:1) .EQ. 'N' ) GOTO 1
         IF ( CHDOT(1:1) .EQ. 'J' ) GOTO 2
C
    1    CONTINUE
C
         CHTYP = 'I'
         IERR = 0
         GOTO 9999
	  ENDIF
C
    2    CONTINUE
      IF ( IRES .EQ. 1 .OR. IRES .EQ. 2 ) THEN
C
C *** Real im F- oder E-Format, falls e+01 dann wird in "Gutmuetigkeits-
C                               Check weiter danach gesucht und
C                               umgesetzt
         CALL HECHDOT ( TSTRING, CHDOT, IERR )
         IF ( CHDOT(1:1) .EQ. 'J' ) THEN
            CALL HETYPRES ( TSTRING, CHREALS, IERR )
            IF ( CHREALS .EQ. 'J' ) THEN
               CHTYP = 'E'
               IERR = 0
               GOTO 9999
            ELSE IF ( CHREALS .EQ. 'N' ) THEN
               CHTYP = 'F'
               IERR = 0
               GOTO 9999
           ENDIF
         ELSE IF ( CHDOT(1:1) .EQ. 'N' ) THEN
            CHTYP = 'C'
            IERR = 0
            GOTO 9999
         ENDIF
      ELSE IF ( IRES .EQ. 3 ) THEN
C
C *** alphabetical, z.B. als Schluesselwort Finder
C
         CHTYP = 'A'
         IERR = 0
         GOTO 9999
      ELSE IF ( IRES .EQ. 4 ) THEN
C
C *** alphanumerical, wird hier nicht unterstuetzt wie unter alphabetical
C
         CHTYP = 'A'
         IERR = 0
         GOTO 9999
      ELSE IF ( IRES .EQ. 5 ) THEN
C
C *** special String, gemischte Gross und Kleinschreibung
C
         CHTYP = 'C'
         IERR = 0
         GOTO 9999
      ELSE IF ( IRES .EQ. 6 ) THEN
C
C *** Gemischtwarenladen, muss noch weiter untersucht werden
C
         CALL HETYPREE ( TSTRING, CHREALE, IERR )
         IF ( CHREALE .EQ. 'J' ) THEN
            CHTYP = 'X'
            IERR = 0
            GOTO 9999
         ENDIF
C
         CALL HECHDOT ( TSTRING, CHDOT, IERR )
         IF ( CHDOT(1:1) .EQ. 'J' ) THEN
            CALL HETYPDBE ( TSTRING, CHREALD, IERR )
            IF ( CHREALD .EQ. 'D' ) THEN
C ***       normale DOUBLE PRECISION Variable
               CHTYP = 'Y'
               IERR = 0
               GOTO 9999
            ELSE IF ( CHREALD .EQ. 'J' ) THEN
C ***       normale DOUBLE PRECISION Variable
               CHTYP = 'Z'
               IERR = 0
               GOTO 9999
            ELSE
C ***          kein Real und kein Double gefunden
               CHTYP = 'C'
               IERR  = 900
               GOTO 9999
            ENDIF
         ELSE IF ( CHDOT(1:1) .EQ. 'N' ) THEN
            CHTYP = 'C'
            IERR = 0
            GOTO 9999
         ENDIF
C
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
CCC   WRITE (*,*)    ' IRES     = ', IRES
CCC	  WRITE (*,*)    ' CHDOT    = ', CHDOT
CCC	  WRITE (*,*)    ' CHREALS  = ', CHREALS
CCC	  WRITE (*,*)    ' CHREALE  = ', CHREALE
CCC	  WRITE (*,*)    ' CHREALD  = ', CHREALD
CCC	  WRITE (*,*)    ' TSTRING  = ', TSTRING (IFIRST:ILAST)
CCC	  CALL HEENTER 
C
      CALL HEINOU ( 'HECHTYP', 'AUS', IERR )
C
      RETURN
      END