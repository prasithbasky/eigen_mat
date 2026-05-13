C*****7**1*********2*********3*********4*********5*********6*********7**
      SUBROUTINE STRGRB ( CHSTR, IERR )
C
C
CA*   UP ZUM UMWANDELN DES CHARACTER-STRINGS "CHSTR" IN GROSS-BUCHSTABEN
C
CB*   STRGRB = STRING VERARBEITUNG: GROSS-BUCHSTABEN
C              ---                  --    -
C
CD*   PROGRAMM-SYSTEM:      PUBLIC
C
C
CE*   FUNKTION:  STRING VERARBEITUNG
C     --------
C
CS*   PROGRAMMIER-SPRACHE:  FORTRAN 77
C     -------------------
C
C
CF*   EINGABE:
C     -------
C
C     - CHSTR      CHARACTER-STRING VOR DER UMWANDLUNG
C
C
CG*   AUSGABE:
C     -------
C
C     - CHSTR      CHARACTER-STRING NACH DER UMWANDLUNG,
C                  ---> NUR GROSSBUCHSTABEN
C     - IERR       FEHLER-PARAMETER:
C                     =0  ---> KEIN FEHLER
C                     =1  ---> FEHLER IN DEN UBERGABE-PARAMETERN
C                     =2  ---> FEHLER BEI DER BEARBEITUNG
C
C
CH*   INTERNE:
C     -------
C
C     -            KEINE
C
C
CI*   COMMON:
C     ------
C     -            KEINE
C
C
CJ*   BENOETIGTE UNTERPROGRAMME / FUNKTIONEN:
C     --------------------------------------
C
C     - SUBROUTINE TYPERR
C     - SUBROUTINE STR$UPCASE
C
C
C
CM*   ACHTUNG:     ES WERDEN FOLGENDE GERAETE-SPEZ. ROUTINEN VERWENDET:
C     -------         - SUBROUTINE STR$UPCASE     /DEC-VAX-VMS/
C
C
CN*   BEMERKUNG:   KEINE
C     ---------
C
C
C
CO*   AUTOR:       Axel Haenschke
C     -----        VW-GEDAS, Berlin
C                  Pascalstrasse 11
C                  Abteilung: Technische Simulation
C
C
CP*   VERSION:     1.0                                     27-DEC-89
C     -------
C
C                  COPYRIGHT (C) 1989, VW-GEDAS, Berlin
C                  ALL RIGHTS RESERVED
C
C
CQ*   UPDATE:      20                                      60
C     -------
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
      IMPLICIT     LOGICAL  ( A - Z )
C
C ----------------------------------------------------------------------
C
      INTEGER      IERR
C
C ----------------------------------------------------------------------
C
      CHARACTER    CHSTR *(*)
C
C ======================================================================
C
C
C ======================================================================
CIT*  I N I T I A L I S I E R U N G S T E I L
C ======================================================================
C
C *** VORBESETZUNG:
      IERR = 0
C
C
C ======================================================================
CEV*  E I N G A B E V E R A R B E I T U N G
C ======================================================================
C
C
C
C ======================================================================
CVT*  V E R A R B E I T U N G S T E I L
C ======================================================================
C
C =========================================================
C *** DIESER PROGRAMMTEIL IST RECHNER-ABHAENGIG!!!!!!!!!!!!
C     ---> HIER "DEC/MicroVMS"
C =========================================================
C
      CALL STR$UPCASE ( CHSTR, CHSTR )
C
C
C ======================================================================
CAT*  A U S G A B E T E I L
C ======================================================================
C
C ======================================================================
CFA*  F O R M A T  - A N W E I S U N G E N
C ======================================================================
C
 9999 CONTINUE
      IF ( IERR .NE. 0 ) CALL TYPERR ( 'STRGRB',IERR )
C
      RETURN
      END
