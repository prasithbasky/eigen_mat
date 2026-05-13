C*****7**1*********2*********3*********4*********5*********6*********7**
      SUBROUTINE HEINOU ( NAME, CHINOU, IERR  )
C
C
CA*   Unterprogramm zum ausgeben der Routinenkennung fuer
C     Eingang und Ausgang in das Unterprogramm.
C
CB*   Name = HEINOU Hilfsroutine In / Out Routinensteuerung
C            ------
C
C
CD*   PROGRAMM-SYSTEM: HELP, Routinensammlung zur Programmentwicklung
C     ---------------
C
CE*   FUNKTION: Unterprogramm zur Ausgabe der Routinen-Kennungen
C     --------
C
CS*   PROGRAMMIER-SPRACHE: FORTRAN 77
C     --------------------
C
C
CF*   EINGABE:
C     -------
C
C     - NAME       NAME DES VERWENDETEN UNTERPROGRAMMES
C     - CHINOU     'EIN, 'AUS' KENNUNG
C
C
CG*   AUSGABE:
C     -------
C
C     - IERR       FEHLERKENNUNG,
C                  IERR =  0, ALLES I.O.
C                  IERR = 98, falsche Ausgabekennung
C                  IERR = 99, Programmeinheit ist nicht initialisiert
C                             d.h. der Steuerparameter fuer die
C                             Ausgabe ist 0, sonst nach der Initiali-
C                             sierung ist der Parameter entweder
C                               1 = Ausgabe auf Bildschirm
C                             999 = keine Ausgabe auf Bildschirm
C
C
CH*   INTERNE:
C     -------
C
C     - KTW        Terminal Ausgabe Kanal = 6
C     - KTR        Terminal Lese Kanal    = 5
C     - IAUS       Steuerparameter fuer die Ausgabe,
C                  aus COMMON STEUER
C
C                  IAUS =   0, d.h. Programmeinheit ist nicht
C                              initialisiert.
C                  IAUS =   1, Ausgabe der Kennungen auf Bildschirm
C                  IAUS = 999, Keine Ausgabe auf Bildschirm.
C
C
CI*   COMMON:
C     ------
C
C     - STEUER     STEUERKENNUNGEN FUER DIE VERARBEITUNG
C
C
CJ*   BENOETIGTE UNTERPROGRAMME / FUNKTIONEN:
C     --------------------------------------
C
C     - SUBROUTINE UPCASE
C
C
CL*   DIESE ROUTINE WIRD AUFGERUFEN VON:
C     ---------------------------------
C
C     - SUBROUTINE allen Unterprogrammen (Standard).
C
C
CM*   ACHTUNG:     Das Unterprogramm wird standardmaessig in
C     -------      den FORTRAN Header eingebracht.
C
C
CN*   BEMERKUNG:   Keine
C     ---------
C
C
CO*   AUTOR:       Axel Haenschke
C     -----        Firma
C                  Anschrift der Firma
C                  Abteilung:
C
C
CP*   VERSION:     0.1                                     08-AUG-94
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
      INTEGER      LNAME
C
C ----------------------------------------------------------------------
C
      CHARACTER    NAME*(*)
      CHARACTER    CHINOU*3
      CHARACTER    CHMAKE*3
C
C ----------------------------------------------------------------------
C
      INCLUDE 'steuer.inc'
C
C ======================================================================
C
C
C ======================================================================
CIT*  I N I T I A L I S I E R U N G S T E I L
C ======================================================================
C
      IERR   = 0
      CHMAKE = ' '
C
C ======================================================================
CEV*  E I N G A B E V E R A R B E I T U N G
C ======================================================================
C

      IF ( IAUS .EQ. 0 ) THEN
         IERR =  0
         GOTO 9999
      ENDIF
C
C ======================================================================
CVT*  V E R A R B E I T U N G S T E I L
C ======================================================================
C
C *** Umsetzung von CHINOU auf Grossbuchstaben
C
      CALL UPCASE ( CHINOU, CHMAKE )
C
C *** Ermitteln der Namenlaenge
C
      CALL CHLAEN ( NAME, LNAME )
C
      IF ( IAUS .EQ. 0 ) THEN
         GOTO 9999
      ENDIF
C
      IF ( IAUS .EQ. 1 ) THEN
         IF (CHMAKE .EQ. 'EIN' ) THEN
            WRITE ( KTW, 5001 ) NAME (1:LNAME)
            IERR = 0
            GOTO 9999
         ELSE IF (CHMAKE .EQ. 'AUS' ) THEN
            WRITE ( KTW, 5002 ) NAME (1:LNAME)
            IERR = 0
            GOTO 9999
         ELSE
            IERR = 98
         ENDIF
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
 5001 FORMAT ( /,
     +/,'**************************************************************'
     +/,'           Eingang in   ',A24,
     +/,'**************************************************************'
     +// )
C
 5002 FORMAT ( /,
     +/,'**************************************************************'
     +/,'           Ausgang aus  ',A24,
     +/,'**************************************************************'
     +// )
C
C ======================================================================
C
 9999 CONTINUE
C
      RETURN
      END
