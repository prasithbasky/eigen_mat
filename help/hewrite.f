C*****7**1*********2*********3*********4*********5*********6*********7**
      SUBROUTINE HEWRITE ( IUNIT, STRING, IERR )
C
C
CA*   Unterprogramm zum Ausgeben von Character-Strings auf das Ausgabe-
C     medium IUNIT
C
C
CB*   Name = HEWRITE Hilfsroutine zum Ausgeben von STRINGS
C            -------
C
C
CD*   PROGRAMM-SYSTEM: HELP, Routinensammlung zur Programmentwicklung
C     ---------------
C
CE*   FUNKTION: Unterprogramm zur Ausgabe von STRINGS
C     --------
C
CS*   PROGRAMMIER-SPRACHE: FORTRAN 77
C     --------------------
C
C
CF*   EINGABE:
C     -------
C
C     - IUNIT      Ausgabekanal fuer die Ausgabe auf Datei
C     - STRING     Ausgabezeichenkette
C
C
CG*   AUSGABE:
C     -------
C
C     - IERR       FEHLERRUECKGABE
C                  IERR  =  0, alles i.O.
C                  IERR  = 96, STRING kann nicht ausgegeben werden,
C                              da Ausgabemedium nicht geoeffnet.
C
C
CH*   INTERNE:
C     -------
C
C     - keine
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
C     - SUBROUTINE  keine
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
C                  dann gibt die Datei den Fehlercode weiter.
C                  Weiterhin geht das Unterprogramm davon aus, dass
C                  alle Informationen die ausgegeben werden sollen
C                  auf den Ausgabestring geschrieben worden sind.
C                  Die maximale Ausgabegroesse wird nicht in diesem
C                  Unterprogramm festgelegt. 
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
      INTEGER      IUNIT
      INTEGER      IERR
C
C ----------------------------------------------------------------------
C
      CHARACTER    STRING*(*)
C
C ======================================================================
C
C
C ======================================================================
CIT*  I N I T I A L I S I E R U N G S T E I L
C ======================================================================
C
      IERR    = 0
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
      WRITE ( IUNIT, *, ERR=101 ) STRING
      GOTO   9999
C
C ======================================================================
CAT*  A U S G A B E T E I L
C ======================================================================
C
C *** FEHLERMELDUNGEN:
C
  101 CONTINUE
      IERR = 95
      GOTO 9999
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
