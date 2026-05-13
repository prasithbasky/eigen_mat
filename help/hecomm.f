C*****7**1*********2*********3*********4*********5*********6*********7**
      SUBROUTINE HECOMM ( STRING, CHCOMM )
C
C
CA*   Unterprogramm zum interpretieren der Stringzeile auf Verwertbar-
C     keit. Hier werden Kommentarzeilen als solche erkannt, wenn
C     sie in der ersten Spalte mit einem C beginnen.
C
CB*   Name = HECOMM Hilfsroutine zum Interpretieren von Kommentarzeilen
C            ------
C
C
CD*   PROGRAMM-SYSTEM: HELP, Routinensammlung zur Programmentwicklung
C     ---------------
C
CE*   FUNKTION: Unterprogramm zur Auswerten von Kommentaren.
C     --------
C
CS*   PROGRAMMIER-SPRACHE: FORTRAN 77
C     --------------------
C
C
CF*   EINGABE:
C     -------
C
C     - STRING     Eingelesener, zu interpretierender String
C
C
CG*   AUSGABE:
C     -------
C
C     - CHKOMM     Erkennungsgroesse
C                  CHCOMM   = 'J', Kommentarzeile
C                  CHCOMM   = 'N', keine Kommentarzeile
C
C
CH*   INTERNE:
C     -------
C
C     - CHBUFF     Erste Spalte in STRING.
C     - CHSOLL     Kommentar-Groesse, auf die untersucht werden soll.
C                         CHSOLL1 = 'C'
C                         CHSOLL2 = '/'
C                         CHSOLL3 = '!'
C                   	  CHSOLL4 = '#'
C         
C
CI*   COMMON:
C     ------
C
C     - keine
C
C
CJ*   BENOETIGTE UNTERPROGRAMME / FUNKTIONEN:
C     --------------------------------------
C
C     - SUBROUTINE  UPCASE
C
C
CL*   DIESE ROUTINE WIRD AUFGERUFEN VON:
C     ---------------------------------
C
C     - vielen
C
C
CM*   ACHTUNG:     Es wird davon ausgegangen, dass Kommentarzeilen
C     -------      mit einem einzustellenden, signifikanten Zeicehn
C                  in der ersten Spalte von String versehen sind.
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
CQ*   UPDATE:      Axel Hänschke                           13.04.2026
C     -------      Hinzufügen weitere Kommentar-Zeichen
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
      INTEGER      LCH
C
C ----------------------------------------------------------------------
C
      CHARACTER    CHCOMM*1
      CHARACTER    CHSOLL1*1
      CHARACTER    CHSOLL2*1
      CHARACTER    CHSOLL3*1
      CHARACTER    CHSOLL4*1
      CHARACTER    CHBUFF1*1
      CHARACTER    CHBUFF2*1
      CHARACTER    CHBUFF3*1
      CHARACTER    CHBUFF4*1
      CHARACTER    STRING*(*)
C
C ======================================================================
C
C
C ======================================================================
CIT*  I N I T I A L I S I E R U N G S T E I L
C ======================================================================
C
      CHCOMM  = ' '
      CHSOLL1 = 'C'
      CHSOLL2 = '/'
      CHSOLL3 = '!'
	  CHSOLL4 = '#'
C 
      CHBUFF1 = ' '
      CHBUFF2 = ' '
      CHBUFF3 = ' '
      CHBUFF4 = ' '

C
C ======================================================================
CEV*  E I N G A B E V E R A R B E I T U N G
C ======================================================================
C
      CHBUFF1(1:1) = STRING (1:1)
	  CHBUFF2(1:1) = STRING (1:1) 
C
C ======================================================================
CVT*  V E R A R B E I T U N G S T E I L
C ======================================================================
C
C *** Uebertragen in Grossbuchstaben
C
      If ( CHBUFF1(1:1) .EQ. 'c' ) THEN
         CALL UPCASE ( CHBUFF1(1:1), CHBUFF2(1:1) )
	  ENDIF
C
C*** Prüfen des ersten Characters
C
      IF      ( CHBUFF2(1:1) .EQ. CHSOLL1(1:1) ) THEN
         CHCOMM = 'J'
      ELSE IF ( CHBUFF2(1:1) .EQ. CHSOLL2(1:1) ) THEN
         CHCOMM = 'J'
      ELSE IF ( CHBUFF2(1:1) .EQ. CHSOLL3(1:1) ) THEN
         CHCOMM = 'J'      
	  ELSE IF ( CHBUFF2(1:1) .EQ. CHSOLL4(1:1) ) THEN
         CHCOMM = 'J'
      ELSE 
         CHCOMM = 'N'
C
      ENDIF
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
