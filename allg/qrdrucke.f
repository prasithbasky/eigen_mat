C*****7**1*********2*********3*********4*********5*********6*********7**
C
      PROGRAM QRDRUCKE
C
C
CA*   HAUPTPROGRAMM ZUM QUERFORMAT DRUCKEN VON DAEIEN 
C
CB*   QRDRUCKE  = QRDRUCKE
C                 --------
C
C
CD*   PROGRAMM-SYSTEM: QRDRUCKE
C     ---------------
C
CE*   FUNKTION: PROGRAMM ZUM DRUCKEN
C     --------  EINER DATEI IM QUERFORMAT
C
C
CS*   PROGRAMMIER-SPRACHE: FORTRAN 77
C     --------------------
C
C
CF*   EINGABE:
C     -------
C
C     - KEINE
C
C
CG*   AUSGABE:
C     -------
C
C     - KEINE
C
C
CH*   INTERNE:
C     -------
C
C     - INPUT      CHARACTER-STRING, DER VON DATEI GELESEN WIRD
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
C     - SUBROUTINE QSTLNG
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
CO*   AUTOR:       AXEL HAENSCHKE
C     -----        VW-GEDAS, Berlin
C                  Pascalstrasse 11
C                  Abteilung: Technische Simulation
C
C
CP*   VERSION:     0.1                                     118-AUG-91
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
      INTEGER      ISTRA
      INTEGER      ISTRE
      INTEGER      IREC
      INTEGER      LNAME
      INTEGER      KTW
      INTEGER      KTR
      INTEGER      MNRLEN
      INTEGER      UNITL
      INTEGER      UNIT1
      INTEGER      UNIT2
C
C ----------------------------------------------------------------------
C
      CHARACTER*1  INPUT(150)
      CHARACTER*150 CHAUS
      CHARACTER*1  BLANK
      CHARACTER*80 CHFILE
      CHARACTER*80 CHNAME
C
C ======================================================================
C
      UNITL  = 10
      UNIT1 = 1
      UNIT2 = 2
C
      KTW   = 6
      KTR   = 5
C
C ======================================================================
CIT*  I N I T I A L I S I E R U N G S T E I L
C ======================================================================
C
C *** EROEFFNEN DER LISTE.TMP DATEI
C
      OPEN ( UNIT=UNITL, FILE = 'LISTE.TMP', STATUS = 'OLD' )
C
C ======================================================================
CEV*  E I N G A B E V E R A R B E I T U N G
C ======================================================================
C
C *** LESEN DES DATEINAMEN
C
   10 CONTINUE
      READ ( UNITL, 1001, END=20 ) CHFILE
C
      CALL QSTLNG (CHFILE, LNAME)
C
C *** AUSGABE AUF BILDSCHIRM
C
      WRITE ( KTW, * ) ' Bearbeitete Datei: ', CHFILE(1:LNAME)
C
C *** EROEFFNEN DER LESE-DATEI
C
      OPEN ( UNIT=UNIT1, FILE = CHFILE(1:LNAME), STATUS = 'OLD' )
C
C *** EROEFFNEN DER SCHREIB-DATEI
C
      OPEN ( UNIT=UNIT2, FILE = 'DUMMY.DAT', STATUS = 'NEW' )
C     +       RECORDTYPE = 'VARIABLE', STATUS = 'NEW' )
C
C *** LESEN UND INTERPRETIEREN
C
C     EINFUEGEN DER ERSTEN ZEILE
C
      BLANK = ' '
      WRITE (UNIT2, 1000 )  BLANK
C
    1 CONTINUE
      READ (UNIT1, 100, END=200) INPUT
C
C *** BESTIMMEN DER RECORD-LAENGE
C
         DO 80 I=1, 150
            CHAUS (I:I) = INPUT(I)
   80 CONTINUE
C
      IREC = MNRLEN ( CHAUS )
C
      WRITE (UNIT2, 1000 )  CHAUS (1:IREC)
      GOTO 1
C
C ======================================================================
CVT*  V E R A R B E I T U N G S T E I L
C ======================================================================
C
  200 CONTINUE
      CLOSE ( UNIT1 )
      CLOSE ( UNIT2 )
      GOTO 10
C
C *** ALLE DATEIEN ABGEARBEITET
C
   20 CONTINUE
      CLOSE ( UNITL )
      GOTO 9999
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
  100 FORMAT (150A1)
C  101 FORMAT (1X,80A1)
  101 FORMAT (80A1)
 1000 FORMAT (1x,A)
 1001 FORMAT (A)
C
C ======================================================================
C
 9999 CONTINUE
C
C      RETURN
      STOP
      END
