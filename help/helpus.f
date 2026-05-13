C*****7**1*********2*********3*********4*********5*********6*********7**
      SUBROUTINE HELPUS 
C
C
CA*   ROUTINE ZUM HELFEN DES BENUTZERS
C     
C
C
CB*   HELPUS = HELP-ROUTINE, HELP THE USER
C              --            --       --   
C
C
CD*   PROGRAMM-SYSTEM:  EPFMOD
C     ---------------
C
CE*   FUNKTION: WIR WERDEN SEHEN
C     --------
C
CS*   PROGRAMMIER-SPRACHE:  FORTRAN 77
C     -------------------
C
C
CF*   EINGABE:
C     -------
C
C
CG*   AUSGABE:
C     -------
C
C
CH*   INTERNE:
C     -------
C
C
CI*   COMMON:
C     ------
C
C     -    KEINE
C
C
CJ*   BENOETIGTE UNTERPROGRAMME / FUNKTIONEN:
C     --------------------------------------
C
C
CL*   DIESE ROUTINE WIRD AUFGERUFEN VON:
C     ---------------------------------
C
C     - NIEMANDEM
C
C
CM*   ACHTUNG:     NIX
C     -------
C
C
CN*   BEMERKUNG:   KONTROLL-AUSGABEN SIND MIT "CKA" GEKENNZEICHNET
C     ---------
C
C
CO*   AUTOR:       CHRISTIAN GROTE
C     -----        TECHNISCHE UNIVERSITAET BERLIN
C                  INSTITUT FUER STRASSEN- UND SCHIENENVERKEHR - FAHRZEUGTECHNIK
C                  FACHGEBIET KRAFTFAHRZEUGTECHNIK
C
C
CP*   VERSION:     0.1                                     26-FEB-96
C     -------
C
C
CQ*   UPDATE:      CHRISTIAN GROTE                         Datum
C     -------      Was wurde gemacht?                      04-MAR-96
C                  
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
      CALL SYSTEM ('more help.txt')
      GOTO 9999
C
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
C *** FEHLER-AUSGABE:
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
C
      RETURN
      END
