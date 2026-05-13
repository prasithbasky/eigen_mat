C*****7**1*********2*********3*********4*********5*********6*********7**
      SUBROUTINE HEDEF ( IERR )
C
C
CA*   Unterprogramm zum Setzen der Default, wenn nicht mit der ver-
C     aenderbaren Datei HEINIT.DAT gearbeitet wird.
C
CB*   Name = HEDEF
C            -----
C
C
CD*   PROGRAMM-SYSTEM: HELP
C     ---------------
C
CE*   FUNKTION: Setzen der Defaults, die fuer die Interaktion benoetigt
C     --------  werden
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
C     - IERR       Fehlerparameter
C                  IERR = 0, alles i.o.
C
C
CH*   INTERNE:
C     -------
C
C     - chaus      String, der die Eingabe abfaengt.
C
C
CI*   COMMON:
C     ------
C
C     - COMMON /STEUER/
C     - COMMON /CHA/
C     - COMMON /USER/
C
C
CJ*   BENOETIGTE UNTERPROGRAMME / FUNKTIONEN:
C     --------------------------------------
C
C     - SUBROUTINE Name
C
C
CL*   DIESE ROUTINE WIRD AUFGERUFEN VON:
C     ---------------------------------
C
C     - SUBROUTINE Name
C
C
CM*   ACHTUNG:     KEINE
C     -------
C
C
CN*   BEMERKUNG:   KONTROLL-AUSGABEN SIND MIT "CKA" GEKENNZEICHNET
C     ---------
C
C
CO*   AUTOR:       Axel Haenschke
C     -----        Firma
C                  Anschrift der Firma
C                  Abteilung:
C
C
CP*   VERSION:     0.1                                     16.12.93
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
C
C ----------------------------------------------------------------------
C
      CHARACTER    CHAUS*1
      CHARACTER    CHENTER1*1
C
C ----------------------------------------------------------------------
C
      INCLUDE 'steuer.inc'
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
      CHENTER    = 'J'
      CHCLEAR    = 'N'
      CHSYS      = 'U'
      CHEDIT     = 'V'
C
      KTW        =  6
      KTR        =  5
      IAUS       =  0
      IERR       =  0
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
