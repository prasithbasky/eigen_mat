C*****7**1*********2*********3*********4*********5*********6*********7**
      SUBROUTINE HEEDIT ( CHNAME, IERR )
C
C
CA*   Unterprogramm zum Aufruf des eingestellten EDITORS
C
C
CB*   Name = HEDIT  Hilfsroutine zum Aufruf eines eingestellten Editors
C            -----
C
C
CD*   PROGRAMM-SYSTEM: HELP, Routinensammlung zur Programmentwicklung
C     ---------------
C
CE*   FUNKTION: Unterprogramm zum Editoraufruf
C     --------
C
CS*   PROGRAMMIER-SPRACHE: FORTRAN 77
C     --------------------
C
C
CF*   EINGABE:
C     -------
C
C     - CHNAME      Filename der zu editierenen Datei
C
C
CG*   AUSGABE:
C     -------
C
C     - IERR       Fehler return code
C                  IERR   =  0, alles i.O
C                  IERR   = 89, Datei nicht vorhanden
C
C
CH*   INTERNE:
C     -------
C
C     - keine
C
C
CI*   COMMON:
C     ------
C
C     - COMMON /CHA/
C
C
CJ*   BENOETIGTE UNTERPROGRAMME / FUNKTIONEN:
C     --------------------------------------
C
C     - SUBROUTINE SYSTEM
C
C
CL*   DIESE ROUTINE WIRD AUFGERUFEN VON:
C     ---------------------------------
C
C     - vielen
C
C
CM*   ACHTUNG:     Es wird zuerst standardmaessig mit dem VI gearbeitet.
C     -------
C
C
C
CN*   BEMERKUNG:   keine
C     ---------
C
C
C
CO*   AUTOR:       Axel Haenschke
C     -----        Firma
C                  Anschrift der Firma
C                  Abteilung:
C
C
CP*   VERSION:     0.1                                     22-AUG-94
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
      CHARACTER    CHNAME*(*)
	  CHARACTER    TEST*57
      CHARACTER    CHED*18
C
C ----------------------------------------------------------------------
C
      INCLUDE      'steuer.inc'
C
C ======================================================================
C
C
C ======================================================================
CIT*  I N I T I A L I S I E R U N G S T E I L
C ======================================================================
C
      IERR = 0
      CHED = ' '
	  TEST = 'D:\01_Dienst_Programme\Notepad++\notepad++.exe HEINIT.DAT'
C
C ======================================================================
CEV*  E I N G A B E V E R A R B E I T U N G
C ======================================================================
C
      IF ( CHEDIT .EQ. 'V' ) THEN
         CHED = 'vi    '
         GOTO 1
      ELSE IF ( CHEDIT .EQ. 'E' ) THEN
         CHED = 'type  '
         GOTO 1
      ELSE IF ( CHEDIT .EQ. 'D' ) THEN
         GOTO 1
      ELSE IF ( CHEDIT .EQ. 'H' ) THEN
         CHED = '/usr/dt/bin/dtpad '
         GOTO 1
      ELSE
         IERR  = 89
         GOTO 9999
      ENDIF
C
C ======================================================================
CVT*  V E R A R B E I T U N G S T E I L
C ======================================================================
C
    1 CONTINUE
C
cccsgi      write (*,*) ched//chname
      call heenter
ccc      CALL SYSTEM ( CHED//CHNAME )
C     mit notepad++
      write (*,*) TEST
	  pause
	  CALL SYSTEM ( TEST )
CCC  +     'D:\01_Dienst_Programme\Notepad++\notepad++.exe HEINI.DAT' )
      call heenter
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
