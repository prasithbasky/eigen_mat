C*****7**1*********2*********3*********4*********5*********6*********7**
      SUBROUTINE HEREAD ( OPCODE, IUNIT, STRING, IERR )
C
C
CA*   Unterprogramm zum lesen beliebiger Strings. Der String wird
C     von der Datei IUNIT gelesen. Hier kann auch explizit unter-
C     schieden werden, ob von Datei oder vom Bildschirm gelesen
C     werden soll. Beim Lesen vom Bildschirm sind keine Kanalangaben
C     notwendig, koennen aber gemacht werden.
C
CB*   Name = HEREAD Hilfsroutine zum Einlesen von Werten aus Dateien
C            ------
C
C
CD*   PROGRAMM-SYSTEM: HELP, Routinensammlung zur Programmentwicklung
C     ---------------
C
CE*   FUNKTION: Unterprogramm zur einlesen beliebiger Strings
C     --------
C
CS*   PROGRAMMIER-SPRACHE: FORTRAN 77
C     --------------------
C
C
CF*   EINGABE:
C     -------
C
C     - OPCODE     Bearbeitungscode, das Programm reagiert auf folgende
C                  Anweisungen:
C                  OPCODE = 'B', Ein String soll vom Bildschirm gelesen
C                                werden
C                  OPCODE = 'F', Ein String soll von einer Datei gelesen
C                                werden
C     - IUNIT      Kanalnummer der Datei von der gelesen werden soll.
C
C
CG*   AUSGABE:
C     -------
C
C     - STRING     Character Groesse, die entweder von Datei oder
C                  Bildschirm eingelesen wird.
C     - IERR       Fehler-Return Code
C                  Bisher besetzt:
C                  IERR =   0, d.h. kein Fehler beim Lesen
C                  IERR =  10, d.h. End of file beim Lesen von Datei
C                  IERR =  13, nicht lesbar
C                  IERR = 900, Daten koennen nicht eingelesen werden
C                  IERR = 999, unzulaessiger OPCODE
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
C     - keine
C
C
CJ*   BENOETIGTE UNTERPROGRAMME / FUNKTIONEN:
C     --------------------------------------
C
C     - SUBROUTINE UPCASE
C
C
CL*   DIESE ROUTINE WIRD AUFGERUFEN VON:
C     ---------------------------------
C
C     - vielen
C
C
CM*   ACHTUNG:     Die Werte, die aus der Datei gelesen werden,
C     -------      werden hier nicht interpretiert, sondern nur
C                  in den String gelesen. Die zulaessige Laenge
C                  des Strings richtet sich nach der Definition des
C                  rufenden Programms.
C
C
CN*   BEMERKUNG:   Das Unterprogramm liesst nur die Werte aus einer
C     ---------    Datei oder vom Bildschirm.
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
      INTEGER      IOS
      INTEGER      KTR
      INTEGER      IUNIT
C
C ----------------------------------------------------------------------
C
      CHARACTER    OPCODE*1
      CHARACTER    OPCODE1*1
      CHARACTER    STRING*(*)
C
C ======================================================================
C
C       CALL HEINOU ( 'HEREAD', 'EIN', IERR )  
C
C ======================================================================
CIT*  I N I T I A L I S I E R U N G S T E I L
C ======================================================================
C
      IERR  = 0
      KTR   = 5
C
C ======================================================================
CEV*  E I N G A B E V E R A R B E I T U N G
C ======================================================================
C
C
C
      CALL UPCASE ( OPCODE, OPCODE1 )
C
      IF ( OPCODE .EQ. 'B' ) THEN
         GOTO 100
      ELSEIF ( OPCODE .EQ. 'F' ) THEN
         GOTO 200
      ELSE
         IERR = 999
         GOTO 9999
      ENDIF
C
C ======================================================================
CVT*  V E R A R B E I T U N G S T E I L
C ======================================================================
C
C *** Lesen vom Bildschirm
C
  100 CONTINUE
      READ ( KTR, 1000 ) STRING
C       write (*,*) string
C	   CALL HEENTER
      IERR = 0
      GOTO 9999
C
C *** Lesen vom File
C
  200 CONTINUE
      READ ( IUNIT,  5000, END=201, IOSTAT=IOS, ERR=203 ) STRING
cc     write (*,*) '  In HEREAD '
C
CAXEL      READ ( IUNIT,  *, END=201 ) STRING
C
      IERR = 0
      GOTO 9999
C
C *** End of File
C
  201 CONTINUE
      IERR = 10
      GOTO 9999
C
C *** Error
C
  203 CONTINUE
      IERR = 13
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
 1000 FORMAT ( A132 )
 5000 FORMAT ( A132 )
C
C ======================================================================
C
 9999 CONTINUE
C
C       CALL HEINOU ( 'HEREAD', 'AUS', IERR )
C
      RETURN
      END
