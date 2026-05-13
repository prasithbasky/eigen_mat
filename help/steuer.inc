C ======================================================================
C
C ************************* Include Datei   Steuer.inc *****************
C
C ----------------------------------------------------------------------
C
      INTEGER         KTW
      INTEGER         KTR
      INTEGER         IAUS
C
C ----------------------------------------------------------------------
C
      CHARACTER       CHENTER*1
      CHARACTER       CHCLEAR*1
      CHARACTER       CHSYS*1
      CHARACTER       CHEDIT*1
C
C *** Steuergroessen fuer Ein- Ausgabesteuerung
C
      COMMON /STEUER/ KTW, KTR, IAUS
C
C *** Steuergroessen fuer ENTER und CLEAR
C
      COMMON /CHAR/  CHCLEAR, CHENTER, CHSYS, CHEDIT
C
C *** USER-defined Steuergroessen
C 
Cuser COMMON /USER/
C
C ************************* Ende            Steuer.inc *****************
C
