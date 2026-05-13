C*****7**1*********2*********3*********4*********5*********6*********7**
      INTEGER FUNCTION MNRLEN (String)
C
C
CA*   Get "real" length of string ignoring trailing blanks.
C     Example: String = 'ab c    ' -> MNRLEN(String) = 4
C
C
CB*   Name = MNRLEN (Menue; Real length)
C            ------  - -    -    ---
C
C
CD*   PROGRAMM-SYSTEM: ALASKA / Grafik
C     ---------------
C
CE*   FUNKTION: get real string length
C     --------
C
CS*   PROGRAMMIER-SPRACHE: FORTRAN 77
C     --------------------
C
C
CF*   EINGABE:
C     -------
C
C     - String     Inputstring
C
C
CG*   AUSGABE:
C     -------
C
C     - KEINE
C
C
CH*   INTERNE:
C     -------
C
C
CI*   COMMON:
C     ------
C
C
CJ*   BENOETIGTE UNTERPROGRAMME / FUNKTIONEN:
C     --------------------------------------
C
C     - SUBROUTINE
C
C
CL*   DIESE ROUTINE WIRD AUFGERUFEN VON:
C     ---------------------------------
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
CO*   AUTOR:       Michael Hassa
C     -----        VW-GEDAS, Berlin
C                  Pascalstrasse 11
C                  Abteilung: Technische Simulation
C
C
CP*   VERSION:     0.1                                     01-JUL-91
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
C
C ----------------------------------------------------------------------
C
      CHARACTER    String *(*)
C
C ======================================================================
C
C
C ======================================================================
CIT*  I N I T I A L I S I E R U N G S T E I L
C ======================================================================
C
C *** Initial length 0
      MNRLEN = 0
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
C *** Loop backwards
      Do 100 I = LEN (String), 1, -1
         If (String (I:I) .NE. ' ') Then
            MNRLEN = I
            GOTO 200
         End If
  100 Continue
  200 Continue
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
