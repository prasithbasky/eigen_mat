C*****7**1*********2*********3*********4*********5*********6*********7**
      SUBROUTINE HECHANZ ( STRING, ISTRMAX, IANZST, IERR)
C
C
CA*   Unterprogramm zum Erkennen der Anzahl der Teilstrings aus dem
C     Eingabestring zur Uebergabe an das rufende Programm.
C     
C
CB*   Name = HECHANZ Hilfsroutine zum erkennen der Anzahl der 
C      Teilstrings.
C
C
CD*   PROGRAMM-SYSTEM: HELP, Routinensammlung zur Programmentwicklung
C     ---------------
C
CE*   FUNKTION: Unterprogramm zum Interpretieren von STRINGS
C     --------
C
CS*   PROGRAMMIER-SPRACHE: FORTRAN 77
C     --------------------
C
C
CF*   EINGABE:
C     -------
C
C     - STRING     zu untersuchender Textrecord.
C     - ISTRMAX    maximale Laenge des Strings
C
C
CG*   AUSGABE:
C     -------
C
C     - IANZST     Anzahl der Teilstrings, die dann einzelnd interpretiert
C                  werden koennen.
C     - IERR       Fehler Returncode
C                  IERR  =  0,   alles i.O.
C                  IERR  =  190, keine Werte im String enthalten
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
C     - SUBROUTINE vielen
C
C
CM*   ACHTUNG:     Der eingegebene String wird auf seine Anzahl von
C     -------      Teilstrings untersucht. Der String wird noch
C                  nicht interpretiert
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
      INTEGER      IZ
      INTEGER      ISTRMAX
      INTEGER      IANZST
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
      CHARACTER    STRING*(*)
C
C ======================================================================
C
      CALL HEINOU ( 'HECHANZ', 'EIN', IERR )
C
C ======================================================================
CIT*  I N I T I A L I S I E R U N G S T E I L
C ======================================================================
C
C
      IERR    = 0
      IBASE   = 1
      IANZST  = 0
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
      IBASE = ILAST + 1
C         write (*,*) ' IBASE    = ', IBASE    
C         write (*,*) ' ISTRMAX  = ', ISTRMAX
C         write (*,*) ' IFIRST   = ', IFIRST
C         write (*,*) ' ILAST    = ', ILAST
C         pause
      IF ( IBASE .GT. ISTRMAX ) THEN
          GOTO 9999
      ENDIF
C
      GOTO 11
C
C *** Leerstring
C
 2222 CONTINUE
C
      IF ( IZ .EQ. 1 ) THEN         
            IERR     =  190
            IANZST   = 0
            GOTO 9999
      ELSE IF ( IZ .GT. 1 ) THEN
            IANZST = IZ - 1
            IERR   = 0
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
      CALL HEINOU ( 'HECHANZ', 'AUS', IERR )
C
      RETURN
      END
