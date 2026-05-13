C*****7**1*********2*********3*********4*********5*********6*********7**
      SUBROUTINE HELEER ( OPCODE, IUNIT, IANZ, IERR )
C
C
CA*   Unterprogramm zum Ausgeben von IANZ Leerzeilen auf den Ausgabe-
C     medium gewaehlt in OPCODE.
C
C
CB*   Name = HELEER Hilfsroutine zum Ausgeben von Leerzeilen
C            ------
C
C
CD*   PROGRAMM-SYSTEM: HELP, Routinensammlung zur Programmentwicklung
C     ---------------
C
CE*   FUNKTION: Unterprogramm zur Ausgabe von Leerzeilen
C     --------
C
CS*   PROGRAMMIER-SPRACHE: FORTRAN 77
C     --------------------
C
C
CF*   EINGABE:
C     -------
C
C     - OPCODE     Arbeitscode der Routine
C                  OPCODE  =  'B', Ausgabe der Leerzeilen auf dem 
C                                  Bildschirm
C                  OPCODE  =  'F', Ausgabe der Leerzeilen aud die
C                                  Datei IUNIT
C     - IUNIT      Ausgabekanal fuer die Ausgabe auf Datei
C     - IANZ       Anzahl der Ausgabezeilen
C
C
CG*   AUSGABE:
C     -------
C
C     - IERR       FEHLERRUECKGABE
C                  IERR  =  0, alles i.O.
C                  IERR  = 95, Leerzeilen koennen nicht ausgegeben werden,
C                              da Ausgabemedium nicht geoeffnet.
C
C
CH*   INTERNE:
C     -------
C
C     - I          Schleifenindex
C     - KTW        Terminal schreib Kanal, wird hier speziell gesetzt.
C     - CHBLANK    Leerzeichen
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
C     - SUBROUTINE  HEWRITE
C
C
CL*   DIESE ROUTINE WIRD AUFGERUFEN VON:
C     ---------------------------------
C
C     - vielen
C
C
CM*   ACHTUNG:     Es wird davon ausgegangen, dass die Ausgabedatei
C     -------      auch geoeffnet ist. Falls das nicht der Fall ist
C                  dann gibt die Datei den Fehlercode von HEWRITE 
C                  weiter. 
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
CP*   VERSION:     0.1                                     22-AUG-94
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
      INTEGER      IUNIT
      INTEGER      IANZ
      INTEGER      IERR
      INTEGER      KTW
C
C ----------------------------------------------------------------------
C
      CHARACTER    CHBLANK*1
      CHARACTER    OPCODE*1
C
C ======================================================================
C
C
C ======================================================================
CIT*  I N I T I A L I S I E R U N G S T E I L
C ======================================================================
C
      CHBLANK = ' '
      IERR    = 0
      KTW     = 6
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
      IF ( OPCODE .EQ. 'B' ) THEN
         DO 100 I=1,IANZ
            CALL HEWRITE ( KTW, CHBLANK, IERR )
  100    CONTINUE
      ELSE IF ( OPCODE .EQ. 'F' ) THEN
         DO 200 I=1,IANZ
            CALL HEWRITE ( IUNIT, CHBLANK, IERR )
  200    CONTINUE
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
      RETURN
      END
