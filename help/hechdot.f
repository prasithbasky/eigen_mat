C*****7**1*********2*********3*********4*********5*********6*********7**
      SUBROUTINE HECHDOT ( TSTRING, CHDOT, IERR )
C
C
CA*   Unterprogramm zum Untersuchen von Teilstrings auf Punkte
C     als Trennungszeichen im String
C
C
CB*   Name = HECHDOT   Hilfsroutine zum Erkennen von Punkten im Teil-
C            --------  string
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
C     - CHDOT      Punktindikator
C                  CHDOT   = 'J', d.h. eine Punkt im Teilstring gefunden
C                  CHDOT   = 'N', d.h. kein Punkt vorhanden
C     - IERR       Fehlerkennung
C                  IERR =   0, d.h. alles i.o.
C                  IERR = 911, Teilstring kann nicht interpretiert werden.
C
C
CH*   INTERNE:
C     -------
C
C     - LSTR       Laenge des Strings
C     - CHDUMM     Hilfsstring
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
C     - SUBROUTINE HECHTYP uvm.
C
C
CM*   ACHTUNG:     Die Routine erkennt Punkte in einem Teilstring
C     -------
C
C
C
C
CN*   BEMERKUNG:   Das Unterprogramm erkennt einen Punkt im Teilstring.
C     ---------    und gibt diese Information an das rufende Programm
C                  zurueck. Es wird nur bemerkt, daá ein Punkt vorhan-
C                  den ist oder nicht. Alle weiteren Interpretationen
C                  des Teilstrings werden den folgenden Erkennungs-
C                  routinen ueberlassen.
C
C
C
CO*   AUTOR:       Axel Haenschke
C     -----        Firma
C                  Anschrift der Firma
C                  Abteilung:
C
C
CP*   VERSION:     0.1                                     19-SEP-94
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
      CHARACTER    CHDUMM*1
      CHARACTER    CHDOT*1
      CHARACTER    CHPUNKT*1
C
C ======================================================================
C
      CALL HEINOU ( 'HECHDOT', 'EIN', IERR )
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
      CHDOT         = ' '
      CHDUMM        = ' '
      CHPUNKT(1:1)  = '.'
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
         IERR = 911
         CHDOT = 'N'
         GOTO 9999
      ENDIF
C
      DO 100 I=1, LSTR
         CHDUMM (1:1) = TSTRING (I:I)
         IF ( CHDUMM(1:1) .EQ. CHPUNKT(1:1) ) THEN
            CHDOT = 'J'
            IERR  = 0
            GOTO 9999
         ELSE
            CHDOT = 'N'
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
      CALL HEINOU ( 'HECHDOT', 'AUS', IERR )
C
      RETURN
      END
