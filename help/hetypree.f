C*****7**1*********2*********3*********4*********5*********6*********7**
      SUBROUTINE HETYPREE ( TSTRING, CHREALE, IERR )
C
C
CA*   Unterprogramm zum Untersuchen von Teilstrings auf REAL im E-Format 
C     
C
CB*   Name = HETYPREE  Hilfsroutine zum Interpretieren von Teilstrings
C            --------  REAL Erkennung
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
C     - CHREALE    REAL im E-Format
C                  CHREALE = 'J', d.h. eine REAL-Groesse liegt vor.
C                            im E-Format, wobei das e als kleiner 
C                            Buchstabe abgebildet ist.
C                  CHREALE = 'N', d.h. keine Real-Groesse wurde gefunden
C     - IERR       Fehlerkennung
C                  IERR =   0, d.h. alles i.o.
C                  IERR = 901, Daten koennen nicht interpretiert werden.
C                              Z.B. nur einen Character lang
C                              Werden als Typ Character angezeigt.
C
C
CH*   INTERNE:
C     -------
C
C     - LSTR       Laenge des Strings
C     - CHDUMM     Hilfsstring
C     - CHREAL1    erster Suchstring 
C                  CHREAL1 = e+
C     - CHREAL2    zweiter Suchstring 
C                  CHREAL1 = e-
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
C     - SUBROUTINE HECHTYP
C
C
CM*   ACHTUNG:     Die Routine erkennt die Typen der uebergebenen
C     -------      Teilstrings. Speziell wird hier der bisher 
C                  nicht gefundene Typ im e-Format gesucht.
C                  Typ wird nur dann erkannt, wenn mit Vorzeichen
C                  am Exponenten gearbeitet wird.
C                  
C
C
CN*   BEMERKUNG:   Das Unterprogramm erkennt den Typ des Teilstrings
C     ---------    und gibt diesen, falls er erkannt wirdan das rufende
C                  Programm zurueck.
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
      INTEGER      I
      INTEGER      IERR
      INTEGER      LSTR
C
C ----------------------------------------------------------------------
C
      CHARACTER    TSTRING*(*)
      CHARACTER    CHREAL1*2
      CHARACTER    CHREAL2*2
      CHARACTER    CHDUMM*2
      CHARACTER    CHREALE*1
C
C ======================================================================
C
      CALL HEINOU ( 'HETYPREE', 'EIN', IERR )
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
      CHREAL1(1:2) = 'e+'
      CHREAL2(1:2) = 'e-'
      CHREALE      = ' '
C     CHDUMM       = ' '
      CHDUMM       = '  '
C
C ======================================================================
CVT*  V E R A R B E I T U N G S T E I L
C ======================================================================
C
C *** Ermitteln der Laenge des Teilstrings
C
      CALL CHLAEN ( TSTRING, LSTR )
C
      IF ( LSTR .LE. 1 ) THEN
         IERR = 901
         CHREALE = 'N'
         GOTO 9999
      ENDIF
C
      DO 100 I=1, LSTR-1
         CHDUMM (1:2) = TSTRING (I:I+1) 
         IF ( CHDUMM(1:2) .EQ. CHREAL1(1:2) ) THEN
            CHREALE = 'J'
            IERR  = 0
            GOTO 9999
         ELSE IF ( CHDUMM(1:2) .EQ. CHREAL2(1:2) ) THEN
            CHREALE = 'J'
            IERR  = 0
            GOTO 9999
         ELSE
            CHREALE = 'N'
         ENDIF
  100 CONTINUE
C
C *** alte Schleife über CHDUMM
C
C           DO 100 I=1, LSTR-1
C              CHDUMM (I:I+1) = TSTRING (I:I+1) 
C              IF ( CHDUMM(I:I+1) .EQ. CHREAL1(1:2) ) THEN
C                 CHREALE = 'J'
C                 IERR  = 0
C                 GOTO 9999
C              ELSE IF ( CHDUMM(I:I+1) .EQ. CHREAL2(1:2) ) THEN
C                 CHREALE = 'J'
C                 IERR  = 0
C                 GOTO 9999
C              ELSE
C                 CHREALE = 'N'
C             ENDIF
C         100 CONTINUE
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
      CALL HEINOU ( 'HETYPREE', 'AUS', IERR )
C
      RETURN
      END
