C*****7**1*********2*********3*********4*********5*********6*********7**
      CHARACTER*(*) FUNCTION QUCASE ( ZEICHEN )
C
C
CA*   WANDELT ALLE BUCHSTABEN EINER ZEICHENKETTE IN GROSSBUCHSTABEN UM
C
CB*   NAME = QUCASE
C            ------
C
C
CD*   PROGRAMM-SYSTEM:
C     ---------------
C
CE*   FUNKTION: STRINGUMWANDLUNG
C     --------
C
CS*   PROGRAMMIER-SPRACHE: FORTRAN 77
C     --------------------
C
C
CF*   EINGABE:
C     -------
C
C     - ZEICHEN    UMZUWANDELNDE ZEICHENKETTE
C
C
CG*   AUSGABE:
C     -------
C
C     - ZEICHEN    UMGEWANDELTE ZEICHENKETTE
C
C
CH*   INTERNE:
C     -------
C
C     - IC         EBCDIC-CODE EINES ZEICHENS INNERHALB DER
C                  ZEICHENKETTE ZEICHEN
C     - I          LAUFVARIABLE FUER UMWANDLUNGSSCHLEIFE
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
C     - KEINE
C
C
CL*   DIESE ROUTINE WIRD AUFGERUFEN VON:
C     ---------------------------------
C
C     -
C
C
CM*   ACHTUNG:     GILT NUR FUR ASCII-CODE
C     -------
C
C
CN*   BEMERKUNG:   KONTROLL-AUSGABEN SIND MIT "CKA" GEKENNZEICHNET
C     ---------
C
C
CO*   AUTOR:       ROLAND KOPETSCH
C     -----        VW-GEDAS
C                  ABTEILUNG: PES/SIM  (TECHNISCHE SIMULATION)
C
C
C
CP*   VERSION:     0.1                                     09-FEB-90
C     -------
C
C
CQ*   UPDATE:      NAME DES BEARBEITERS                    DATUM
C     -------      BESCHREIBUNG DER AENDERUNGEN
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
      IMPLICIT     NONE
C
C ======================================================================
C
      CHARACTER * (*)  ZEICHEN
      INTEGER          I, IC
C
C ======================================================================
CIT*  I N I T I A L I S I E R U N G S T E I L
C ======================================================================
C
      QUCASE = ZEICHEN
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
      DO 10 I = 1, LEN(ZEICHEN)
         IC = ICHAR ( ZEICHEN(I:I) )

         IF ( IC .GE. 97 .AND. IC .LE. 122 ) THEN

            QUCASE(I:I) = CHAR ( IC - 32 )

         ENDIF

10    CONTINUE

      RETURN
      END

