C*****7**1*********2*********3*********4*********5*********6*********7**
      SUBROUTINE HEFEHL ( STRING1, STRING2, IUNIT )      
C
C
CA*   ROUTINE ZUM AUSGEBEN VON FEHLERMELDUNGEN AUF BILDSCHIRM
C     
C
C
CB*   HELEZE = HELP-ROUTINE, LESE EINE ZEILE EIN 
C              --            -    -    -     -
C
C
CD*   PROGRAMM-SYSTEM:  HELP
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
C     - UNIT      KANALNUMMER                       
C
CG*   AUSGABE:
C     -------
C
C     - STRING1   EINGELESENER STRING1             
C     - STRING2   EINGELESENER STRING2
C                               
C
CH*   INTERNE:
C     -------
C
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
C     - SUBROUTINE HELERZ    
C     - SUBROUTINE  
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
CO*   AUTOR:       THOMAS DETER     
C     -----        TECHNISCHE UNIVERSITAET BERLIN
C                  INSTITUT FUER STRASSEN- UND SCHIENENVERKEHR - FAHRZEUGTECHNIK
C                  FACHGEBIET KRAFTFAHRZEUGTECHNIK
C
C
CP*   VERSION:     0.1                                     26-FEB-96
C     -------
C
C
CQ*   UPDATE:      THOMAS DETER                            Datum
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
C ----------------------------------------------------------------------
C
      INTEGER   IERR
      INTEGER   IUNIT 
C
C ----------------------------------------------------------------------
C
      CHARACTER*(*) STRING1
      CHARACTER*(*) STRING2
C
C ======================================================================
CIT*  I N I T I A L I S I E R U N G S T E I L
C ======================================================================
C
      IERR = 0
C
C ======================================================================
CEV*  E I N G A B E V E R A R B E I T U N G
C ======================================================================
C
C *** BESTIMME DIE LAENGE DER ZEICHENKETTE
C     LSTRING = LEN (STRING)
C
C ======================================================================
CVT*  V E R A R B E I T U N G S T E I L
C ======================================================================
C
C *** SETZE DEN STRING AUF LEERZEICHEN
C
C
C
C
C ======================================================================
CAT*  A U S G A B E T E I L
C ======================================================================
C
C *** FEHLER-AUSGABE:
C
C
C
C ======================================================================
CFA*  F O R M A T  - A N W E I S U N G E N
C ======================================================================
C
C ======================================================================
C
 9999 CONTINUE
C
C
      RETURN
      END
