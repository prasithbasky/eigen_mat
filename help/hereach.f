C*****7**1*********2*********3*********4*********5*********6*********7**
      SUBROUTINE HEREACH ( RWERT, CHWERT, LWERT, IERR )
C
C
CA*   VERWANDELT EINEN REAL-WERT IN EINEN CHARACTER-STRING
C
CB*   HEINCH = HELP-ROUTINE: VERWANDLE REAL IN CHARACTER
C              --                      ---     --
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
C     - RWERT      REAL-WERT, DER IN EINEN CHARACTER-STRING VERWANDELT
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
C
CH*   INTERNE:
C     -------
C
C     -  KEINE
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
CM*   ACHTUNG:     DIE ZAHL WIRD STETS IM E-FORMAT GESCHRIEBEN!     
C     -------      KEINE GANZAHLIGE VARIABLE UEBERGEBEN DA SONST
C                  MURKS RAUSKOMMT. DAS E-FORMAT KANN WEGEN DER
C                  UNTERSCHIEDLICHEN RECHNERINTERNEN DARSTELLUNG
C                  ZU PROBLEMEN BEI DER AUSGABEN VON GANZZAHLIGEN
C                  VARIABLEN FUEHREN. (SIEHE AUCH FORTRAN 77-
C                  SPRACHUMFANG VOM RRZN, S. 6.15 UNTEN)
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
      INTEGER      IERR
      INTEGER      LWERT
C
C ----------------------------------------------------------------------
C
      REAL         RWERT
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
C
C ======================================================================
CEV*  E I N G A B E V E R A R B E I T U N G
C ======================================================================
C
C ======================================================================
CVT*  V E R A R B E I T U N G S T E I L
C ======================================================================
C
C
      WRITE (CHWERT(1:10), '(E10.3)') RWERT
C
      LWERT = 10
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

