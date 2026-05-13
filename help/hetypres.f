C*****7**1*********2*********3*********4*********5*********6*********7**
      SUBROUTINE HETYPRES ( TSTRING, CHREALS, IERR )
C
C
CA*   Unterprogramm zum Untersuchen von Teilstrings auf REAL im E-Format 
C     (STANDARD)
C
CB*   Name = HETYPRES  Hilfsroutine zum Interpretieren von Teilstrings
C            --------  REAL Erkennung im Standard E-Format
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
C     - CHREALS    REAL im E-Format
C                  CHREALS = 'J', d.h. eine REAL-Groesse liegt vor.
C                            im E-Format, wobei das E als grosser 
C                            Buchstabe abgebildet ist.
C                  CHREALS = 'N', d.h. keine Real-Groesse wurde gefunden
C     - IERR       Fehlerkennung
C                  IERR =   0, d.h. alles i.o.
C                  IERR = 921, Daten koennen nicht interpretiert werden.
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
C                  CHREAL1 = E+
C     - CHREAL2    zweiter Suchstring 
C                  CHREAL2 = E-
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
C                  nicht gefundene Typ im E-Format gesucht.
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
      CHARACTER    CHREAL3*2
      CHARACTER    CHREAL4*2
      CHARACTER    CHDUMM*2
      CHARACTER    CHREALS*1
C
C ======================================================================
C
      CALL HEINOU ( 'HETYPRES', 'EIN', IERR )
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
C *** grosses E
C
      CHREAL1(1:2) = 'E+'
      CHREAL2(1:2) = 'E-'
C
C *** kleines e
C      
      CHREAL3(1:2) = 'e+'
      CHREAL4(1:2) = 'e-'
C	  
      CHREALS      = ' '
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
         CHREALS = 'N'
         GOTO 9999
      ENDIF
C
      DO 100 I=1, LSTR-1
         CHDUMM (1:2) = TSTRING (I:I+1) 
C	   
         IF ( CHDUMM(1:2) .EQ. CHREAL1(1:2) ) THEN
            CHREALS = 'J'
            IERR  = 0
            GOTO 9999
         ELSE IF ( CHDUMM(1:2) .EQ. CHREAL2(1:2) ) THEN
            CHREALS = 'J'
            IERR  = 0
            GOTO 9999
         ELSE IF ( CHDUMM(1:2) .EQ. CHREAL3(1:2) ) THEN
            CHREALS = 'J'
            IERR  = 0
            GOTO 9999
          ELSE IF ( CHDUMM(1:2) .EQ. CHREAL4(1:2) ) THEN
            CHREALS = 'J'
            IERR  = 0
            GOTO 9999
C
        ELSE
            CHREALS = 'N'
         ENDIF
  100 CONTINUE
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
C
C
      CALL HEINOU ( 'HETYPRES', 'AUS', IERR )
C
      RETURN
      END
