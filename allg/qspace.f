C*****7**1*********2*********3*********4*********5*********6*********7**
      CHARACTER*(*) FUNCTION QSPACE ( NSPACE )
C
C
CA*   LIEFERT N BLANKS
C
CB*   NAME = QSPACE
C
C
CD*   PROGRAMM-SYSTEM: Q B A S I C
C     ---------------
C
CE*   FUNKTION: STRINGVERARBEITUNG
C     --------
C
CS*   PROGRAMMIER-SPRACHE: FORTRAN 77
C     --------------------
C
C
CF*   EINGABE:
C     -------
C
C     - NSPACE     ANZAHL DER BLANKS
C
C
CG*   AUSGABE:
C     -------
C
C     - QSPACE     STRING DER LAENGE N NUR MIT BLANKS
C
C
CH*   INTERNE:
C     -------
C
C     - 9          20
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
C     - SUBROUTINE NAME
C
C
CL*   DIESE ROUTINE WIRD AUFGERUFEN VON:
C     ---------------------------------
C
C     - SUBROUTINE NAME
C
C
CM*   ACHTUNG:     DA ES IN FORTRAN KEIN STRING MIT DER LAENGE 0
C     -------      GIBT, IST QSPACE IMMER MINDESTENS = ' '
C
C
CN*   BEMERKUNG:   KONTROLL-AUSGABEN SIND MIT "CKA" GEKENNZEICHNET
C     ---------
C
C
CO*   AUTOR:       ROLAND KOPETSCH
C     -----        VW-GEDAS
C                  ABTEILUNG: PES/SIM  (TECHNISCHE SIMULATION)
C
C
C
CP*   VERSION:     0.1                                     18-DEZ-90
C     -------
C
C
CQ*   UPDATE:      NAME DES BEARBEITERS                    DATUM
C     -------      BESCHREIBUNG DER AENDERUNGEN
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
         INTEGER      I, NSPACE
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
C
C ======================================================================
CVT*  V E R A R B E I T U N G S T E I L
C ======================================================================
C
      DO 10   I =  1, NSPACE
10       QSPACE ( I : I )  =  ' '

      IF ( NSPACE .EQ. 0 )   QSPACE = ' '
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
