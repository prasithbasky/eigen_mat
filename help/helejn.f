C*****7**1*********2*********3*********4*********5*********6*********7**
      SUBROUTINE HELEJN ( CHJN, LODEF, CHAUSG )      
C
C
CA*   ROUTINE ZUM ABFRAGEN VON J/N
C     
C
C
CB*   HELEJN = HELP-ROUTINE, LESE JA ODER NOE EIN
C              --            --   -       -
C
C
CD*   PROGRAMM-SYSTEM:  EPFMOD
C     ---------------
C
CE*   FUNKTION: WIR WERDEN SEHEN
C     --------
C
CS*   PROGRAMMIER-SPRACHE:  FORTRAN 77
C     -------------------
C
C
CF*   EINGABE:
C     -------
C
C     - CHJN      ABFRAGEPARAMETER ='J' ODER = 'N'
C     - LODEF     PARAMETER, DER ANGIBT, OB DEFAULT AN-
C                 GEZEIGT WERDEN SOLL
C     - CHAUSG    AUSGABESTRING
C
CG*   AUSGABE:
C     -------
C
C     - CHJN      ABFRAGEPARAMETER ='J' ODER = 'N'
C
C
CH*   INTERNE:
C     -------
C
C     - LSTRING   Laenge des eingelesenen Strings
C     - I         Laufvariable
C     - LAUSG     Laenge des Strings CHAUSG
C
C
CI*   COMMON:
C     ------
C
C     -    KEINE
C
C
CJ*   BENOETIGTE UNTERPROGRAMME / FUNKTIONEN:
C     --------------------------------------
C
C     - SUBROUTINE HECHSE
C     - SUBROUTINE HETYPREA
C     - SUBROUTINE CHLAEN 
C
C
CL*   DIESE ROUTINE WIRD AUFGERUFEN VON:
C     ---------------------------------
C
C     - NIEMANDEM
C
C
CM*   ACHTUNG:     NIX
C     -------
C
C
CN*   BEMERKUNG:   KONTROLL-AUSGABEN SIND MIT "CKA" GEKENNZEICHNET
C     ---------
C
C
CO*   AUTOR:       CHRISTIAN GROTE
C     -----        TECHNISCHE UNIVERSITAET BERLIN
C                  INSTITUT FUER STRASSEN- UND SCHIENENVERKEHR - FAHRZEUGTECHNIK
C                  FACHGEBIET KRAFTFAHRZEUGTECHNIK
C
C
CP*   VERSION:     0.1                                     26-FEB-96
C     -------
C
C
CQ*   UPDATE:      CHRISTIAN GROTE                         Datum
C     -------      Was wurde gemacht?                      04-MAR-96
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
      INTEGER   I
      INTEGER   LSTRING
      INTEGER   LAUSG
C
C ----------------------------------------------------------------------
C
      CHARACTER*1   CHJN
      CHARACTER     STRING*70
      CHARACTER*(*) CHAUSG
C
C ----------------------------------------------------------------------
C     
      LOGICAL   LODEF   
C
C ======================================================================
C
C
C ======================================================================
CIT*  I N I T I A L I S I E R U N G S T E I L
C ======================================================================
C
C *** SETZE DEN STRING AUF LEERZEICHEN
      DO 60 I = 1, 70
        STRING(I:I) = ' '
   60 CONTINUE
C
C
C ======================================================================
CEV*  E I N G A B E V E R A R B E I T U N G
C ======================================================================
C
C *** SCHREIBE EINEN TEXT RAUS:
C
      CALL CHLAEN (CHAUSG, LAUSG)
  100 IF (LODEF) THEN
        WRITE (6,1000) CHAUSG(1:LAUSG), CHJN
      ELSE
        CHJN = ' '
        WRITE (6,1001) CHAUSG(1:LAUSG)
      ENDIF
C
C *** LESE VOM TERMINAL
C
      READ (5,'(A70)') STRING
      CALL CHLAEN (STRING, LSTRING)
      IF (LSTRING.EQ.0) GOTO 9999
C
C *** SUCHE NACH IRGENDWAS VERWERTBAREM IM STRING
      DO 200 I = 1, LSTRING
        IF (STRING(I:I).EQ.'J') THEN
          CHJN = 'J'
          GOTO 9999
        ELSEIF (STRING(I:I).EQ.'j') THEN 
          CHJN = 'J'
          GOTO 9999
        ELSEIF (STRING(I:I).EQ.'N') THEN
          CHJN = 'N'
          GOTO 9999
        ELSEIF (STRING(I:I).EQ.'n') THEN
          CHJN = 'N'
          GOTO 9999
        ELSEIF (STRING(I:I).EQ.'?') THEN
          CALL HELPUS 
        ENDIF
 
  200 CONTINUE
C
C *** HIER KOMMT DAS PROGRAMM NUR HIN, WENN NICHTS BRAUCHBARES IM
C     STRING STEHT, ALSO NOCHMAL EINLESEN!
C
      GOTO 100
C
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
C *** FEHLER-AUSGABE:
C
C
C ======================================================================
CFA*  F O R M A T  - A N W E I S U N G E N
C ======================================================================
C
 1000 FORMAT (/, A, ' ==> (J/N) [<RETURN> = ',A1,']:')
 1001 FORMAT (/, A, ' ==> (J/N) :')
C
C ======================================================================
C
 9999 CONTINUE
C
C
      RETURN
      END
