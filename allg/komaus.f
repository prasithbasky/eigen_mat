C*****7**1*********2*********3*********4*********5*********6*********7**
      SUBROUTINE KOMAUS ( CHKOMM, IERR )
C
C
CA*   UP ZUM AUSFUEHREN EINES BETRIEBSSYSTEM-KOMMANDOS
C
CB*   KOMAUS = KOMMANDO AUSFUEHREN
C              ---      ---
C
CD*   PROGRAMM-SYSTEM:      PUBLIC
C     ---------------
C
C
CE*   FUNKTION:
C     --------
C
CS*   PROGRAMMIER-SPRACHE:  FORTRAN 77
C     -------------------
C
C
CF*   EINGABE:
C     -------
C
C     - CHKOMM     CHARACTER-STRING MIT DEM "KOMMANDO-TEXT"
C
C
CG*   AUSGABE:
C     -------
C
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
C     - SUBROUTINE LIB$SPAWN
C
C
C
CM*   ACHTUNG:     ES WERDEN FOLGENDE GERAETE-SPEZ. ROUTINEN VERWENDET:
C     -------         - SUBROUTINE LIB$SPAWN     /DEC-VAX-VMS/
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
C                  COPYRIGHT (C) 1989, VW-GEDAS
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
      CHARACTER    CHKOMM *(*)
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
C ======================================================================
CVT*  V E R A R B E I T U N G S T E I L
C ======================================================================
C
C
C =========================================================
C *** DIESER PROGRAMMTEIL IST RECHNER-ABHAENGIG!!!!!!!!!!!!
C     ---> HIER "DEC/MicroVMS"
C =========================================================
C
      CALL LIB$SPAWN ( CHKOMM )
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
      IF ( IERR .NE. 0 ) CALL TYPERR ( 'KOMAUS',IERR )
C
      RETURN
      END

