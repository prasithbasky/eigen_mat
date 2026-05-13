C*****7**1*********2*********3*********4*********5*********6*********7**
      SUBROUTINE HELEREA ( RWERT, LODEF, CHAUSG )      
C
C
CA*   ROUTINE ZUM EINLESEN VON REAL-WERTEN
C     
C
C
CB*   HELEREA = HELP-ROUTINE, LESE REAL-WERT EIN
C               --            --   ---
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
C     - RWERT     REAL-WERT
C     - LODEF     PARAMETER, DER ANGIBT, OB DEFAULT AN-
C                 GEZEIGT WERDEN SOLL
C     - CHAUSG    AUSGABESTRING
C
CG*   AUSGABE:
C     -------
C
C     - RWERT     REAL-WERT
C
C
CH*   INTERNE:
C     -------
C
C     -  IERR     FEHLERVARIABLE
C     -  LAUSG    LAENGE DES STRINGS CHAUSG
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
      INTEGER       I
      INTEGER       IERR
      INTEGER       LSTRING
      INTEGER       LAUSG
      INTEGER       IWERT
C
C ----------------------------------------------------------------------
C
      REAL          RWERT
C
C ----------------------------------------------------------------------
C
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
        WRITE (6,1000) CHAUSG(1:LAUSG), RWERT
      ELSE
        RWERT = 0.0
        WRITE (6,1001) CHAUSG(1:LAUSG)
      ENDIF
C
C *** LESE VOM TERMINAL
C
      READ (5,'(A70)') STRING
      CALL CHLAEN (STRING, LSTRING)
      IF (LSTRING.EQ.0) GOTO 9999
      DO 60 I = 1, LSTRING
        IF (STRING(I:I).EQ.'?') THEN
          CALL HELPUS
          GOTO 100
        ENDIF
  60  CONTINUE
C
C *** Lese einen Real-Wert aus dem String
      CALL HETYPREA (STRING, RWERT, IERR)
      IF (IERR.NE.0) GOTO 100 
C
      GOTO 9999
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
 1000 FORMAT (/, A, ' (Real) ==> [<RETURN> = ',E10.4,']:')
 1001 FORMAT (/, A, ' (Real) ==>  :')
C
C ======================================================================
C
 9999 CONTINUE
C
C
      RETURN
      END
