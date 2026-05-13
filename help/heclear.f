C*****7**1*********2*********3*********4*********5*********6*********7**
      SUBROUTINE HECLEAR  ( IERR )
C
C
CA*   Unterprogramm zum definierten Loeschen des Bildschirms.
C     Unabhaengig von einem System, durch Wahl des Strings in System
C
CB*   Name = HECLEAR
C            -------
C
C
CD*   PROGRAMM-SYSTEM: beliebig
C     ---------------
C
CE*   FUNKTION: Loeschen des Bildschirms
C     --------
C
CS*   PROGRAMMIER-SPRACHE: FORTRAN 77
C     --------------------
C
C
CF*   EINGABE:
C     -------
C
C     - keine
C
C
C
CG*   AUSGABE:
C     -------
C
C     - IERR       Fehler Return Code
C                  IERR  =   0, d.h. alles i.O.
C                  IERR  =  59, falscher OPCODE von CHSYS
C                  IERR  =  58, falscher OPCODE von CHCLEAR
C
C
CH*   INTERNE:
C     -------
C
C     - STRING     Uebergabe an die Routine System
C
C
CI*   COMMON:
C     ------
C
C     - COMMON /STEUER/ STEUERGROESSE FUER DIE UMSETZUNG
C                       DES LOESCHBEFEHLES IN DIE UNTERSCHIEDLICHEN
C                       BETRIEBSSYSTEME
C                       OPCODE = 'D', d.h. der clear-Befehl fuer
C                                     DOS wird abgesetzt
C                       OPCODE = 'U', d.h. der clear-Befehl fuer
C                                     UNIX wird abgesetzt.
C
C
C
CJ*   BENOETIGTE UNTERPROGRAMME / FUNKTIONEN:
C     --------------------------------------
C
C     - SUBROUTINE SYSTEM
C
C
CL*   DIESE ROUTINE WIRD AUFGERUFEN VON:
C     ---------------------------------
C
C     - SUBROUTINE Name
C
C
CM*   ACHTUNG:     KEINE
C     -------
C
C
CN*   BEMERKUNG:   KONTROLL-AUSGABEN SIND MIT "CKA" GEKENNZEICHNET
C     ---------
C
C
CO*   AUTOR:       Axel Haenschke
C     -----        Firma
C                  Anschrift der Firma
C                  Abteilung:
C
C
CP*   VERSION:     0.1                                     16.12.93
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
C
C ----------------------------------------------------------------------
C
      CHARACTER    CHCLEAR1*1
      CHARACTER    CHSYS1*1
C
C ----------------------------------------------------------------------
C
      INCLUDE      'steuer.inc'
C
C ======================================================================
C
C
C ======================================================================
CIT*  I N I T I A L I S I E R U N G S T E I L
C ======================================================================
C
C
C ======================================================================
CEV*  E I N G A B E V E R A R B E I T U N G
C ======================================================================
C
      CALL UPCASE ( CHCLEAR, CHCLEAR1 )
      CALL UPCASE ( CHSYS, CHSYS1 )
C
      IF ( CHSYS1 .EQ. 'U' ) THEN
         IERR  = 0
         GOTO 100
      ELSE IF ( CHSYS1 .EQ. 'D' ) THEN
         IERR  = 0
         GOTO 200
      ELSE
         IERR  = 59
         GOTO 9999
      ENDIF
C
C ======================================================================
CVT*  V E R A R B E I T U N G S T E I L
C ======================================================================
C
C
C *** UNIX-Befehl absetzen
C
  100 CONTINUE
      IF ( CHCLEAR1 .EQ. 'J' ) THEN
         CALL SYSTEM ( 'clear' )
         IERR = 0
         GOTO 9999
      ELSE IF (CHCLEAR1 .EQ. 'N' ) THEN
         IERR = 0
         GOTO 9999
      ELSE
         IERR = 58
		 GOTO 9999
      ENDIF
C
C *** DOS-Befehl absetzen
C
  200 CONTINUE
      IF ( CHCLEAR1 .EQ. 'J' ) THEN
         CALL SYSTEM ( 'clear' )
         GOTO 9999
	  ELSE IF (CHCLEAR1 .EQ. 'N' ) THEN
         IERR = 0
         GOTO 9999	  
      ELSE
         IERR = 58
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
      RETURN
      END
