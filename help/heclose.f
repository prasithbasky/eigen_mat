C*****7**1*********2*********3*********4*********5*********6*********7**
      SUBROUTINE HECLOSE ( IUNIT, IERR )
C
C
CA*   Unterprogramm zum definierten schliessen von Dateien
C     und fehlerabfangen
C
C
CB*   Name = HECLOSE Hilfsroutine zum Schliessen von Dateien
C            -------
C
C
CD*   PROGRAMM-SYSTEM: HELP, Routinensammlung zur Programmentwicklung
C     ---------------
C
CE*   FUNKTION: Unterprogramm zum Schliessen von Dateien
C     --------
C
CS*   PROGRAMMIER-SPRACHE: FORTRAN 77
C     --------------------
C
C
CF*   EINGABE:
C     -------
C
C     - IUNIT      Kanal der zu schliessenden Datei
C
C
CG*   AUSGABE:
C     -------
C
C     - IERR       FEHLERRUECKGABE
C                  IERR  =  0, alles i.O.
C                  IERR  = 94, Datei kann nicht geschlossen werden
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
CM*   ACHTUNG:     keine
C     -------      
C                  
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
      CLOSE ( IUNIT, ERR=101 ) 
      GOTO   9999
C
C ======================================================================
CAT*  A U S G A B E T E I L
C ======================================================================
C
C *** FEHLERMELDUNGEN:
C
  101 CONTINUE
      IERR = 94
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
