C*****7**1*********2*********3*********4*********5*********6*********7**
      SUBROUTINE TIMDAT ( ITIM )
C
C
CA*   UNTERPROGRAMM ZUM ERMITTELN DER ZEIT FUER ALASKA
C
CB*   TIMDAT = TIME DATE
C              ---  ---
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
C     - ITIM       ZEIT UND DATUM IM FORMAT 
C                  HH.MM.SSDD.MO.YY
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
      INTEGER*2    IYEAR
      INTEGER*2    IMONTH
      INTEGER*2    IDAY
      INTEGER*2    IHOUR
      INTEGER*2    IMIN
      INTEGER*2    ISEC
      INTEGER*2    IHUSEC
C
C ----------------------------------------------------------------------
C
      CHARACTER*16 ITIM
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
      CALL GETDAT  ( IYEAR, IMONTH, IDAY )
      CALL GETTIM  ( IHOUR, IMIN, ISEC, IHUSEC )
C
C ======================================================================
CVT*  V E R A R B E I T U N G S T E I L
C ======================================================================
C
      WRITE ( ITIM, 1000 ) IHOUR, IMIN, ISEC, IDAY, IMONTH, IYEAR
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
 1000 FORMAT ( I2, '.', I2, '.', 2I2, '.', I2, '.', I2 )
C
C ======================================================================
C
 9999 CONTINUE
C
      RETURN
      END
