C*****7**1*********2*********3*********4*********5*********6*********7**
      SUBROUTINE GETDAT ( IYEAR, IMONTH, IDAY )
C
C
CA*   UNTERPROGRAMM ZUM ERMITTELN DES DATUMS FUER ALASKA
C
CB*   GETTIM = GET DATE
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
C     - IYEAR      JAHRESANGABE
C     - IMONTH     MONATSANGABE
C     - IDAY       TAGESANGABE
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
      READ (CHDATE(10:11), '(I2)' ) IYEAR
C
      READ (CHDATE(1:2),   '(I2)' ) IDAY
C
C *** ERRATEN DES MONATS
C
      IF ( CHDATE(4:6) .EQ. 'JAN' ) IMONTH = 1
      IF ( CHDATE(4:6) .EQ. 'FEB' ) IMONTH = 2
      IF ( CHDATE(4:6) .EQ. 'MAR' ) IMONTH = 3
      IF ( CHDATE(4:6) .EQ. 'APR' ) IMONTH = 4
      IF ( CHDATE(4:6) .EQ. 'MAY' ) IMONTH = 5
      IF ( CHDATE(4:6) .EQ. 'JUN' ) IMONTH = 6
      IF ( CHDATE(4:6) .EQ. 'JUL' ) IMONTH = 7
      IF ( CHDATE(4:6) .EQ. 'AUG' ) IMONTH = 8
      IF ( CHDATE(4:6) .EQ. 'SEP' ) IMONTH = 9
      IF ( CHDATE(4:6) .EQ. 'OCT' ) IMONTH = 10
      IF ( CHDATE(4:6) .EQ. 'NOV' ) IMONTH = 11
      IF ( CHDATE(4:6) .EQ. 'DEC' ) IMONTH = 12
C
C *** DIE DEUTSCHEN MONATE AUCH NOCH ABFANGEN
C
      IF ( CHDATE(4:6) .EQ. 'MAI' ) IMONTH = 5
      IF ( CHDATE(4:6) .EQ. 'OKT' ) IMONTH = 10
      IF ( CHDATE(4:6) .EQ. 'DEZ' ) IMONTH = 12
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
