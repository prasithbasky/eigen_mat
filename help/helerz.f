C*****7**1*********2*********3*********4*********5*********6*********7**
      SUBROUTINE HELERZ (STRING, CHLERZ )
C
C
CA*   UNTERSUCHT OB DER STRING EINE LEERZEILE IST
C
CB*   HELERZ = HELP-ROUTINE: LEERZEILE UNTERSUCHEN
C              --            ----
C
C
CD*   PROGRAMM-SYSTEM:  HELP-LIBRARY
C     ---------------
C
CE*   FUNKTION:  MELDUNGS-AUSGABE    
C     --------
C
CS*   PROGRAMMIER-SPRACHE: FORTRAN 77
C     --------------------
C
C
CF*   EINGABE:
C     -------
C
C     - STRING     ZU UNTERSUCHENDER STRING
C
C
CG*   AUSGABE:
C     -------
C
C     - CHLERZ   --> ='J', WENN ES EINE LEERZEILE IST
C                    ='N', WENN ES KEINE LEERZEILE IST
C
CH*   INTERNE:
C     -------
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
      INTEGER      LSTRING
C
C ----------------------------------------------------------------------
C
      CHARACTER*(*)   CHLERZ, STRING
C
C
C ======================================================================
C
C
C ======================================================================
CIT*  I N I T I A L I S I E R U N G S T E I L
C ======================================================================
C
      CHLERZ = 'N'
C
C ======================================================================
CEV*  E I N G A B E V E R A R B E I T U N G
C ======================================================================
C
      CALL CHLAEN (STRING, LSTRING)
      IF (LSTRING.EQ.0) THEN
        CHLERZ='J'
        GOTO 9999
      ENDIF
C
C ======================================================================
CVT*  V E R A R B E I T U N G S T E I L
C ======================================================================
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
 9999 RETURN 
      END

