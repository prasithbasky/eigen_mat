C*****7**1*********2*********3*********4*********5*********6*********7**      
      SUBROUTINE HEFNAG ( PFNAM, PLFN, FNAM, LFN, GFNAM, GLFN, IERR )
C
CA*   Unterprogramm zum Zusammensetzen eines Filenamen mit dem Pfadnamen
C     zum Gesamtnamen einer Datei
C
C
CB*   NAME = HEFNAG Hilfsroutine zum Errechnen des Filnamen Gesamt
C            ------ -                -             -  -     -
C
C
CD*   PROGRAMM-SYSTEM:   HELP, Routinensammlung zur Programmentwicklung
C     ---------------
C
CE*   FUNKTION:  Unterprogramm zum Zusammensetzen eines Filenamen
C     --------
C
CS*   PROGRAMMIER-SPRACHE:  FORTRAN 77
C     -------------------
C
C
CF*   EINGABE:
C     -------
C
CG*   AUSGABE:
C     -------
C
C     - PFNAM      Character Groesse des Pfadnamen
C     - PLFN       Laenge des Pfadnamen
C     - FNAME      Character Groesse des Filenamen
C     - LNF        Laenge des Namen
C
C
CH*   INTERNE:
C     -------
C
C     - GFNAM      Character Groesse des Gesamtfilenamen
C     - GLFN       Laenge des Gesamtfilenamen
C     - IERR       Fehler Return Code
C                  IERR = 0,    alles O.K.
C                  IERR = 132   Gesamtfilenamen ist Laenger als 132 
C                  IERR = 99    Filename hat die Laenge 0
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
C
C
CL*   DIESE ROUTINE WIRD AUFGERUFEN VON:
C     ---------------------------------
C
C     
C
C
CM*   ACHTUNG:    
C     -------
C
C
CN*   BEMERKUNG:   KONTROLL-AUSGABEN SIND MIT "CKA" GEKENNZEICHNET
C     ---------
C
C
CO*   AUTOR:       Axel Haenschke
C     -----        Ford Werke AG
C
C
CP*   VERSION:     0.1                                     07-Mar-98
C     -------
C
C
CQ*   UPDATE:      Axel Haenschke                                           
C                  Ford Werke AG                           12-March-98 
C
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
      INTEGER IERR
      INTEGER I
      INTEGER N
      INTEGER N1
      INTEGER LENTAB
      INTEGER LEND
      INTEGER LANF
      INTEGER LDIFF
      INTEGER LFN
      INTEGER PLFN
      INTEGER GLFN
C
C ----------------------------------------------------------------------
C
      CHARACTER*(*) PFNAM
      CHARACTER*(*) FNAM
      CHARACTER*(*) GFNAM
      CHARACTER*132 NAMETOT
      CHARACTER*1   DIFF         
C      
C ----------------------------------------------------------------------
C      
C
C
C ======================================================================
CIT*  I N I T I A L I S I E R U N G S T E I L
C ======================================================================
C
      call heinou ( 'HEFNAG', 'EIN', IERR )
C
C --- Nullsetzen des Arbeitsstrings
C
      DO 124 I=1, 132
         NAMETOT(I:I) = ' '
  124 CONTINUE
C
       LEND   = 0
       LANF   = 0
       LDIFF  = 0
C
C
C ======================================================================
CVT*  V E R A R B E I T U N G S T E I L
C ======================================================================
C         
       N1 = PLFN
       DIFF(1:1) = '/'
C
       NAMETOT (1:N1) = PFNAM (1:N1)
C
       LDIFF = N1 + 1
       LANF  = N1 + 2
       LEND  = LANF + LFN
C
       IF ( LEND .GT. 132 ) THEN
          IERR = 132
          GOTO 9999
       ENDIF
       IF ( LFN .EQ. 0 ) THEN
          IERR = 99
          GOTO 9999
       ENDIF
C
       NAMETOT (LDIFF:LDIFF) = DIFF(1:1)
       NAMETOT (LANF:LEND) = FNAM(1:LFN)
C
C      WRITE (*,*)  NAMETOT (1:LEND)
C      call HEENTER
C
       GFNAM(1:LEND) = NAMETOT (1:LEND)
       GLFN          = LEND
C
C ======================================================================
CAT*  A U S G A B E T E I L
C ======================================================================
C
C
C     FEHLERMELDUNGEN
C
C ======================================================================
CFA*  F O R M A T  - A N W E I S U N G E N
C ======================================================================
C
C
C
C ======================================================================
C
C
C *** SCHLUSSBEMERKUNG
C ********************
C
 9999 CONTINUE
C 
      call heinou ( 'HEFNAG', 'AUS', IERR )     
C 
      RETURN
      END




 










      
