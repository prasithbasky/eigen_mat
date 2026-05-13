C*****7**1*********2*********3*********4*********5*********6*********7**
      SUBROUTINE HEENTER
C
C
CA*   Unterprogramm zum definierten Anhalten der Ausgabe auf dem
C     Bildschirm. Unabhaengig von einem System
C
CB*   Name = HEENTER
C            -------
C
C
CD*   PROGRAMM-SYSTEM: beliebig
C     ---------------
C
CE*   FUNKTION: Anhalten der Ausgabe und Weitermachen nach der
C     --------  Eingabe von ENTER
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
CG*   AUSGABE:
C     -------
C
C     - keine
C
C
CH*   INTERNE:
C     -------
C
C     - chaus      String, der die Eingabe abfaengt.
C
C
CI*   COMMON:
C     ------
C
C     - COMMON /STEUER/
C     - COMMON /CHA/
C     - COMMON /USER/
C
C
CJ*   BENOETIGTE UNTERPROGRAMME / FUNKTIONEN:
C     --------------------------------------
C
C     - SUBROUTINE Name
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
      CHARACTER    CHAUS*1
      CHARACTER    CHENTER1*1
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
      CHAUS = ' '
C
      CALL UPCASE ( CHENTER, CHENTER1 )
C
C ======================================================================
CEV*  E I N G A B E V E R A R B E I T U N G
C ======================================================================
C
      IF ( CHENTER1 .EQ. 'J' ) THEN
         PRINT*, ' continue with  <Enter> '
         READ ( 5, 7000 ) CHAUS
      ELSE
         GOTO 9999
      ENDIF
C
C ======================================================================
CVT*  V E R A R B E I T U N G S T E I L
C ======================================================================
C
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
 7000 FORMAT ( A1 )
C
C ======================================================================
C
 9999 CONTINUE
C
      RETURN
      END
