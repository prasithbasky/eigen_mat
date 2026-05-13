C*****7**1*********2*********3*********4*********5*********6*********7**
      SUBROUTINE GETTIM ( IHOUR, IMIN, ISEC, IHUSEC )
C
C
CA*   UNTERPROGRAMM ZUM ERMITTELN DER ZEIT FUER ALASKA
C
CB*   GETTIM = GET TIME
C              --- ---
C
C
CD*   PROGRAMM-SYSTEM: a l a s k a  
C     ---------------
C
CE*   FUNKTION: basisfunktion
C     --------
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
C     - IHOUR      STUNDENANGABE
C     - IMIN       MINUTENANGABE
C     - ISEC       SEKUNDENANGABE
C     - IHUSEC     HUNDERTSTELSEC.
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
C     - SUBROUTINE DDATE
C
C
CL*   DIESE ROUTINE WIRD AUFGERUFEN VON:
C     ---------------------------------
C
C     - SUBROUTINE Name
C
C
CM*   ACHTUNG:     DDATE RUFT HARDWARE-ABHAENGIGE ROUTINEN AUF
C     -------
C
C
CN*   BEMERKUNG:   KONTROLL-AUSGABEN SIND MIT "CKA" GEKENNZEICHNET
C     ---------    
C                       
C
CO*   AUTOR:       Axel Haenschke
C     -----        VW-GEDAS, Berlin
C                  Pascalstrasse 11
C                  Abteilung: Technische Simulation
C
C
CP*   VERSION:     0.1                                     21-MAR-91
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
      INTEGER      IERR
      INTEGER*2    IHOUR
      INTEGER*2    IMIN
      INTEGER*2    ISEC
      INTEGER*2    IHUSEC
C
C ----------------------------------------------------------------------
C
      CHARACTER*11 CHDATE
      CHARACTER*11 CHTIME
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
      CALL DDATE  ( 'VAX', CHDATE, CHTIME, IERR )
C
C ======================================================================
CVT*  V E R A R B E I T U N G S T E I L
C ======================================================================
C
      READ (CHTIME(1:2),   '(I2)' ) IHOUR
      READ (CHTIME(4:5),   '(I2)' ) IMIN
      READ (CHTIME(7:8),   '(I2)' ) ISEC
      READ (CHTIME(10:11), '(I2)' ) IHUSEC
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
