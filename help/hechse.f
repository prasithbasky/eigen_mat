C*****7**1*********2*********3*********4*********5*********6*********7**
      SUBROUTINE HECHSE ( STRING, ITEIL, TSTRING, LNTSTR, IFIRST, ILAST,
     +                    IERR )
C
C
CA*   Unterprogramm zum Herauslesen eines Teilstringes aus einem String.
C     Eingabestring vorher auf Anzahl der Teilstrings abgeprÅft.
C
C
CB*   Name = HECHSE  Hilfsroutine zum erkennen der Anzahl der Teil-
C            ------  strings.
C
C
CD*   PROGRAMM-SYSTEM: HELP, Routinensammlung zur Programmentwicklung
C     ---------------
C
CE*   FUNKTION: Unterprogramm zum Selektieren eines Teilstrings
C     --------
C
CS*   PROGRAMMIER-SPRACHE: FORTRAN 77
C     --------------------
C
C
CF*   EINGABE:
C     -------
C
C     - STRING     zu untersuchender Textstring
C     - ITEIL      herauszulesender Teilstring
C
C
CG*   AUSGABE:
C     -------
C
C     - TSTRING    herausgelesener Teilstring
C     - LNTSTR     Laenge des herausgelesenen Teilstrings
C     - IFIRST     Stellung des Teilstrings im Gesamtstring
C                  Anfangsadresse
C     - ILAST      Stellung des Teilstrings im Gesamtstring
C                  Endadresse.
C     - IERR       Fehler return Code
C                  IERR  =  0,   alles i.O.
C                  IERR  =  700, keine Werte im String enthalten
C                  IERR  =  701, ITEIL-Teilstring ist nicht vorhanden
C
C
CH*   INTERNE:
C     -------
C
C     - STRING     Eingelesener RECORD
C                  Der String wird nicht interpretiert.
C     - IBASE      Kennung, ab der der String interpretiert werden soll.
C     - IRES       Pointer zur erkannten Position im Auswahl-Opcode.
C     - IWERT      Integer Wert der Interpretation
C     - RWERT      Real Wert der Interpretation
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
C     - SUBROUTINE CHLAEN
C     - SUBROUTINE HEINOU
C
C
CL*   DIESE ROUTINE WIRD AUFGERUFEN VON:
C     ---------------------------------
C
C     - SUBROUTINE vielen
C
C
CM*   ACHTUNG:     Aus dem uebergebenen String wird der ITEIL
C     -------      Teilstrings herausgelesen und mit seiner LÑenge
C                  an das rufende Programm zurueckgegeben.
C
C
CN*   BEMERKUNG:   keine
C     ---------
C
C
C
CO*   AUTOR:       Axel Haenschke
C     -----        Firma
C                  Anschrift der Firma
C                  Abteilung:
C
C
CP*   VERSION:     0.1                                     18-SEP-94
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
      INTEGER      IANF
      INTEGER      IENDE
      INTEGER      IZ
      INTEGER      ITEIL
      INTEGER      IERR
      INTEGER      IBASE
      INTEGER      IRES
      INTEGER      IFIRST
      INTEGER      ILAST
      INTEGER      IWERT
      INTEGER      LN
      INTEGER      LNTSTR
C
C ----------------------------------------------------------------------
C
      REAL         RWERT
C
C ----------------------------------------------------------------------
C
      CHARACTER    STRING*(*)
      CHARACTER    TSTRING*(*)
C
C ======================================================================
C
      CALL HEINOU ( 'HECHSE', 'EIN', IERR )
C
C ======================================================================
CIT*  I N I T I A L I S I E R U N G S T E I L
C ======================================================================
C
      IERR    = 0
      IANF    = 0
      IENDE   = 0
      IBASE   = 1
      LNTSTR  = 0
      TSTRING = ' '
C
C ======================================================================
CEV*  E I N G A B E V E R A R B E I T U N G
C ======================================================================
C
      IZ      = 0
C
C ======================================================================
CVT*  V E R A R B E I T U N G S T E I L
C ======================================================================
C
C *** Zaehler
C
   11 CONTINUE
C
      IZ = IZ + 1
C
      CALL CRIANY ( 'IRANS', IBASE, STRING, IRES, IWERT, RWERT, IFIRST,
     +               ILAST )
C
C *** Auswertung der Groessen
C
      IF ( ILAST .EQ. 0 ) THEN
         GOTO 2222
      ENDIF
C
      IF ( IZ .EQ. ITEIL ) THEN
         CALL CHLAEN ( STRING(IFIRST:ILAST), LNTSTR)
C
         IENDE   = ILAST
         IANF    = IFIRST
         LN      = IENDE - IANF + 1
C
         TSTRING(1:LN) = STRING(IFIRST:ILAST)
         IERR = 0
         GOTO 9998
      ENDIF
C
      IBASE = ILAST + 1
C
      GOTO 11
C
C ======================================================================
CAT*  A U S G A B E T E I L
C ======================================================================
C
C *** FEHLERMELDUNGEN:
C
C *** Leerstring
C
 2222 CONTINUE
C
      IF ( IZ .EQ. 1 ) THEN
            IERR     =  700
            LNTSTR   =    0
            IFIRST   =    0
            ILAST    =    0
            GOTO 9999
      ELSE IF ( IZ .GT. ITEIL ) THEN
            IERR     = 701
            LNTSTR   =   0
            IFIRST   =   0
            ILAST    =   0
            GOTO 9999
      ENDIF
C
C ======================================================================
CFA*  F O R M A T  - A N W E I S U N G E N
C ======================================================================
C
C
C ======================================================================
C
 9998 CONTINUE
C
C *** Laengenzuweisung
C
      LNTSTR = LN
C
C **** KEINE LAENGENZUWEISUNG, DA FEHLER
C
 9999 CONTINUE
C
      CALL HEINOU ( 'HECHSE', 'AUS', IERR )
C
      RETURN
      END
