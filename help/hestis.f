C*****7**1*********2*********3*********4*********5*********6*********7**
      SUBROUTINE HESTIS ( CHDUSU, CHSUCH, CHGEFU, IFIRST, IERR)
C
C
CA*   SUCHT EINEN TEILSTRING IN EINEM STRING                 
C
CB*   HESTIS = HELP-ROUTINE: SUCHE TEILSTRING IN EINEM STRING
C              --            -     -          -        -
C
C
CD*   PROGRAMM-SYSTEM:  HELP-LIBRARY
C     ---------------
C
CE*   FUNKTION:  STRING-VERARBEITUNG
C     --------
C
CS*   PROGRAMMIER-SPRACHE: FORTRAN 77
C     --------------------
C
C
CF*   EINGABE:
C     -------
C
C     - CHDUSU     DER ZU DURCHSUCHENDE STRING
C     - CHSUCH     DER ZU SUCHENDE TEILSTRING
C
C
CG*   AUSGABE:
C     -------
C
C     - CHGEFU     ZEIGER: ='J' BEDEUTET STRING GEFUNDEN        
C                          ='N' BEDEUTET NICHT GEFUNDEN
C     - IFIRST     ERSTE POSITION DES GESUCHTEN STRINGS
C     - IERR       FEHLER-PARAMETER:
C                     =0  ---> KEIN FEHLER
C                     =1  ---> CHDUSU IST LEER
C                     =2  ---> CHSUCH IST LAENGER ALS CHDUSU
C
CH*   INTERNE:
C     -------
C
C     - LDUSU      LAENGE DES STRINGS CHDUSU  
C     - LSUCH      LAENGE DES STRINGS CHSUCH  
C     - I          LAUFVARIABLE
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
C     SUBROUTINE CHLAEN
C
C
CL*   DIESE ROUTINE WIRD AUFGERUFEN VON:
C     ---------------------------------
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
CO*   AUTOR:       THOMAS DETER
C     -----        TECHNISCHE UNIVERSITAET BERLIN
C                  INSTITUT FUER STRASSEN- UND SCHIENENVERKEHR -
C                  FAHRZEUGTECHNIK
C                  FACHGEBIET KRAFTFAHRZEUGTECHNIK
C
C
CP*   VERSION:     1.0                                     05-MAR-96
C     -------
C
C
CQ*   UPDATE:      Name des Bearbeiters                    Datum
C     -------      Beschreibung der Aenderungen
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
      IMPLICIT     NONE
C
C ----------------------------------------------------------------------
C
      INTEGER      LSUCH 
      INTEGER      IERR
      INTEGER      LDUSU
      INTEGER      I    
      INTEGER      IFIRST    
C
C ----------------------------------------------------------------------
C
      CHARACTER*(*)   CHDUSU
      CHARACTER*(*)   CHSUCH
      CHARACTER       CHGEFU*1
C
C ======================================================================
CIT*  I N I T I A L I S I E R U N G S T E I L
C ======================================================================
C
C *** VORBESETZUNG:
      IERR  = 0
      IFIRST = 0
      CHGEFU = 'N'
C
C ======================================================================
CEV*  E I N G A B E V E R A R B E I T U N G
C ======================================================================
C
C *** IST DER STRING CHDUSU LEER?
C
      CALL CHLAEN (CHDUSU, LDUSU)
      IF (LDUSU.EQ.0) THEN
        IERR = 1
        GOTO 9999
      ENDIF
C
C *** IST DER STRING CHDUSU KUERZER ALS DER STRING CHSUCH?
C
      CALL CHLAEN (CHSUCH, LSUCH)
      IF (LDUSU.LT.LSUCH) THEN
        IERR = 2
        GOTO 9999
       ENDIF
C
C ======================================================================
CVT*  V E R A R B E I T U N G S T E I L
C ======================================================================
C
C
C *** UND NUN LASST UNS SUCHEN
      DO 100 I = 1, LDUSU-LSUCH+1
        IF ( CHDUSU(I:I+LSUCH-1) .EQ. CHSUCH(1:LSUCH) ) THEN
          CHGEFU = 'J'
          IERR = 0
          IFIRST = I
          GOTO 9999
        ENDIF
  100 CONTINUE
C
C
C
      CHGEFU = 'N'
      IERR = 0
      GOTO 9999
C
C ======================================================================
CAT*  A U S G A B E T E I L
C ======================================================================
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

