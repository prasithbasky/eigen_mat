C*****7**1*********2*********3*********4*********5*********6*********7**
      SUBROUTINE HEINCH ( IWERT, CHWERT, LWERT, IERR )
C
C
CA*   VERWANDELT EINEN INTEGER_WERT IN EINEN CHARACTER-STRING
C
CB*   HEINCH = HELP-ROUTINE: VERWANDLE INTEGER IN CHARACTER
C              --                      --         --
C
C
CD*   PROGRAMM-SYSTEM:  HELP-LIBRARY
C     ---------------
C
CE*   FUNKTION:  STRING-VERARBEITUNG
C     --------
C
CS*   PROGRAMMIER-SPRACHE: FORTRAN 77
C     --------------------
C
C
CF*   EINGABE:
C     -------
C
C     - IWERT      INTEGER-WERT, DER IN EINEN CHARACTER-STRING VERWANDELT
C                  WERDEN SOLL
C
C
CG*   AUSGABE:
C     -------
C
C     - CHWERT     CHARACTER-STRING, DER DEN WERT IWERT ENTHAELT
C     - LWERT      LAENGE DER VARIABLEN CHWERT
C     - IERR       FEHLER-PARAMETER:
C                     =0  ---> KEIN FEHLER
C                     =1  ---> IWERT HAT MEHR ALS 9 STELLEN,
C                              'not yet supported'
C
CH*   INTERNE:
C     -------
C
C     - NSTELL     ANZAHL DER STELLEN DER ZAHL
C
C
CI*   COMMON:
C     ------
C
C     -            KEINE
C
C
CJ*   BENOETIGTE UNTERPROGRAMME / FUNKTIONEN:
C     --------------------------------------
C
C
C
CL*   DIESE ROUTINE WIRD AUFGERUFEN VON:
C     ---------------------------------
C
C
C
CM*   ACHTUNG:     
C     -------   
C
C
CN*   BEMERKUNG:   KONTROLL-AUSGABEN SIND MIT "CKA" GEKENNZEICHNET
C     ---------
C
C
CO*   AUTOR:       THOMAS DETER
C     -----        TECHNISCHE UNIVERSITAET BERLIN
C                  INSTITUT FUER STRASSEN- UND SCHIENENVERKEHR -
C                  FAHRZEUGTECHNIK
C                  FACHGEBIET KRAFTFAHRZEUGTECHNIK
C
C
CP*   VERSION:     1.0                                     05-MAR-96
C     -------
C
C
CQ*   UPDATE:      Name des Bearbeiters                    Datum
C     -------      Beschreibung der Aenderungen
C
C
C ----------------------------------------------------------------------
C
CR*   LITERATUR:   
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
      INTEGER      NSTELL 
      INTEGER      IERR
      INTEGER      IWERT
      INTEGER      LWERT
C
C ----------------------------------------------------------------------
C
      CHARACTER*(*)   CHWERT
C
C
C ======================================================================
C
C
C ======================================================================
CIT*  I N I T I A L I S I E R U N G S T E I L
C ======================================================================
C
C *** VORBESETZUNG:
      IERR  = 0
      NSTELL = 0
C
C ======================================================================
CEV*  E I N G A B E V E R A R B E I T U N G
C ======================================================================
C
C *** BESTIMME DIE ANZAHL DER BENOETIGTEN ZEICHEN
      IF     (ABS(IWERT).LT.1E1) THEN
        NSTELL=1
      ELSEIF (ABS(IWERT).LT.1E2) THEN
        NSTELL=2
      ELSEIF (ABS(IWERT).LT.1E3) THEN
        NSTELL=3
      ELSEIF (ABS(IWERT).LT.1E4) THEN
        NSTELL=4
      ELSEIF (ABS(IWERT).LT.1E5) THEN
        NSTELL=5
      ELSEIF (ABS(IWERT).LT.1E6) THEN
        NSTELL=6
      ELSEIF (ABS(IWERT).LT.1E7) THEN
        NSTELL=7
      ELSEIF (ABS(IWERT).LT.1E8) THEN
        NSTELL=8
      ELSEIF (ABS(IWERT).LT.1E9) THEN
        NSTELL=9
      ELSE 
        IERR = 1
        GOTO 9999
      ENDIF
      IF (IWERT.LT.0) NSTELL = NSTELL + 1
C
C ======================================================================
CVT*  V E R A R B E I T U N G S T E I L
C ======================================================================
C
      GOTO (1, 2, 3, 4, 5, 6, 7, 8, 9, 10) NSTELL
C
C
C
C *** EINE STELLE:
    1 WRITE (CHWERT(1:1), '(I1)') IWERT
      GOTO 100
C
C *** ZWEI STELLEN:
    2 WRITE (CHWERT(1:2), '(I2)') IWERT
      GOTO 100
C
C *** DREI STELLEN:
    3 WRITE (CHWERT(1:3), '(I3)') IWERT
      GOTO 100
C
C *** VIER STELLEN:
    4 WRITE (CHWERT(1:4), '(I4)') IWERT
      GOTO 100
C
C *** FUENF STELLEN:
    5 WRITE (CHWERT(1:5), '(I5)') IWERT
      GOTO 100
C
C *** SECHS STELLEN:
    6 WRITE (CHWERT(1:6), '(I6)') IWERT
      GOTO 100
C
C *** SIEBEN STELLEN:
    7 WRITE (CHWERT(1:7), '(I7)') IWERT
      GOTO 100
C
C *** ACHT STELLEN:
    8 WRITE (CHWERT(1:8), '(I8)') IWERT
      GOTO 100
C
C *** NEUN STELLEN:
    9 WRITE (CHWERT(1:9), '(I9)') IWERT
      GOTO 100
C
C *** ZEHN STELLEN:
   10 WRITE (CHWERT(1:10), '(I10)') IWERT
      GOTO 100
C
C 
  100 CALL CHLAEN (CHWERT, LWERT)
      IERR = 0
C
      GOTO 9999
C
C ======================================================================
CAT*  A U S G A B E T E I L
C ======================================================================
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

