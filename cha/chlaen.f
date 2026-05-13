C*****7**1*********2*********3*********4*********5*********6*********7**
      SUBROUTINE CHLAEN ( string, lakt  )
C
C
CA*   Ermittelt die aktuelle Laenge eines Strings
C
CB*   Name = chlaen
C            -----
C
C
CD*   PROGRAMM-SYSTEM: util
C     ---------------
C
CE*   FUNKTION: character-manipulation
C     --------
C
CS*   PROGRAMMIER-SPRACHE: FORTRAN 77
C     --------------------
C
C
CF*   EINGABE:
C     -------
C
C     - string     character-string, der untersucht werden soll
C
C
CG*   AUSGABE:
C     -------
C
C     - lakt       aktuelle Laenge des Strings
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
CN*   BEMERKUNG:   Die Routine veraendert den String nicht
C     ---------
C
C
CO*   AUTOR:       Axel Haenschke
C     -----        Firma
C                  Anschrift der Firma
C                  Abteilung: 
C
C
CP*   VERSION:     0.1                                     08-Dez-93
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
      INTEGER      I
      INTEGER      LAKT
      INTEGER      LGES
C
C ----------------------------------------------------------------------
C
      CHARACTER*(*) STRING
      CHARACTER*1   chtest
C
C ======================================================================
C
C
C ======================================================================
CIT*  I N I T I A L I S I E R U N G S T E I L
C ======================================================================
C
      lges = len (string)
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
      lakt   = 0
      chtest = ' '
c
      do 100 i=lges, 1, -1
        chtest(1:1) = string(i:i)
        if ( chtest(1:1) .ne. ' ' ) then
           lakt = i
           goto 9999
        endif
       if ( i .eq. 1 .and. chtest(1:1) .eq. ' ' ) then
          lakt = 0
          goto 9999
       endif
  100 continue
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
