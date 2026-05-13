C*****7**1*********2*********3*********4*********5*********6*********7**
      SUBROUTINE HEOPEN ( OPCODE, NEW, CHFN, IUNIT, IERR )
C
C
CA*   Unterprogramm zum Eroeffnen von Dateien, die entweder ueber ihren
C     Namen oder ueber ihre UNIT eroeffnet werden.
C
CB*   Name = HEOPEN Hilfsroutine Open
C            ------
C
C
CD*   PROGRAMM-SYSTEM: HELP, Routinensammlung zur Programmentwicklung
C     ---------------
C
CE*   FUNKTION: Unterprogramm zum Dateieneroeffnen
C     --------
C
CS*   PROGRAMMIER-SPRACHE: FORTRAN 77
C     --------------------
C
C
CF*   EINGABE:
C     -------
C
C     - OPCODE     Operation Code fuer die Bearbeitungssteuerung
C                  OPCODE = 'N', der Filenamen wird zur Eroeffnung
C                                interaktiv abgefragt. IUNIT wird
C                                intern gesetzt.
C                  OPCODE = 'K', der Filenamen wird nicht abgefragt,
C                                sondern muss ueber die Liste uebergeben
C                                werden.
C     - NEW        Kennung fuer den Dateientyp
C                  NEW    = 'N', File existiert bereits (Status = old)
C                  NEW    = 'J', File existiert noch nicht (Status
C                                = unknown)
C     - CHFN       Filename bei OPCODE 'K'
C
C
C
CG*   AUSGABE:
C     -------
C
C     - IUNIT      Kanalnummer, wird gesetzt auf vorgeschlagenen Wert.
C     - IERR       Fehler-Return-Code
C                  IERR =   0, alles i.O.
C                  IERR =  70, falscher OPCODE, Eingabe nicht zulaessig
C                  IERR =  80, File nicht vorhanden
C                  IERR =  81, File ist vorhanden, kann nicht neu angelegt
C                              werden.
C                  IERR =  90, File kann nicht geoeffnet werden, da bereits
C                              geoeffnet
C
C
C
CH*   INTERNE:
C     -------
C
C     - CHSTAT1    Statusvariable bei NEW = 'N',  = 'OLD'
C     - CHSTAT2    Statusvariable bei NEW = 'J',  = 'UNKNOWN'
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
C     - SUBROUTINE  HECLEAR
C     - SUBROUTINE  HEENTER
C     - SUBROUTINE  HEREAD
C     - SUBROUTINE  HEINQNM
C     - SUBROUTINE  HEINQUN
C     - SUBROUTINE  HELEER
C
C
CL*   DIESE ROUTINE WIRD AUFGERUFEN VON:
C     ---------------------------------
C
C     - SUBROUTINE HEINIT
C
C
CM*   ACHTUNG:     Ganz wichtig!
C     -------      Bei der automatischen Vergabe der UNIT-Numbers
C                  ist hier ein Automatismus eingebaut.
C                  Fall beim Eroeffnen der Datei ueber Uebergabeliste
C                  die Datei existiert, so wird das festgestellt, hier-
C                  bei wird die UNIT_number um einen Zaehler herauf-
C                  gestetz. Die aktuelle UNIT-number wird dazu vorher
C                  ueber die INQUIRE-Funktion abgefragt.
C
C
C
CN*   BEMERKUNG:   Das Unterprogramm dient zur leichteren Oeffnung und
C     ---------    Handhabung von Dateien.
C                  Ist z.Zt nicht voll ausgebaut, da IUNIT gesetzt wird.
C                  Es wird empfohlen, die Datei so bald wie moeglich
C                  auch wieder zu schliessen.
C                  Vergebene IUNIT = 73
C
C
CO*   AUTOR:       Axel Haenschke
C     -----        Firma
C                  Anschrift der Firma
C                  Abteilung:
C
C
CP*   VERSION:     0.1                                     08-AUG-94
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
      INTEGER      IUNIT
      INTEGER      IOPEN
      INTEGER      IERR
      INTEGER      LCHFN
C
C ----------------------------------------------------------------------
C
      LOGICAL      LOPEN
C
C ----------------------------------------------------------------------
C
      CHARACTER    OPCODE*1
      CHARACTER    NEW*1
      CHARACTER    CHEXIST*1
      CHARACTER    CHSTAT1*3
      CHARACTER    CHSTAT2*7
      CHARACTER    CHFN*(*)
C
C ======================================================================
C
C
C ======================================================================
CIT*  I N I T I A L I S I E R U N G S T E I L
C ======================================================================
C
      IUNIT   =    73
      IERR    =    0
      CHSTAT1 =   'OLD'
      CHSTAT2 =   'UNKNOWN'
      CHEXIST =   ' '
C
C ======================================================================
CEV*  E I N G A B E V E R A R B E I T U N G
C ======================================================================
C
C *** Ueberpruefen, ob Kanalnummer fuer File schon vergeben
C
  789 CONTINUE
      CALL HEINQUN ( IUNIT, CHEXIST )
      IF ( CHEXIST .EQ. 'J' ) THEN
         IUNIT = IUNIT + 1
         GOTO 789
      ELSE IF ( CHEXIST .EQ. 'J' ) THEN
         GOTO 790
      ENDIF
  790 CONTINUE
C
C *** Normaler Ablauf
C
      IF ( OPCODE .EQ. 'N' ) THEN
         GOTO 100
      ELSE IF ( OPCODE .EQ. 'K' ) THEN
         GOTO 200
      ELSE
         GOTO 300
      ENDIF
C
C ======================================================================
CVT*  V E R A R B E I T U N G S T E I L
C ======================================================================
C
C *** Filename wird interaktiv abgefragt und ausgewertet
C
  100 CONTINUE
      CALL HECLEAR ( IERR )
      CALL HELEER ( 'B', IUNIT, 3, IERR )
      WRITE (*,*) ' Bitte geben Sie den Filenamen ein! '
      WRITE (*,*) ' Maximal 80 Zeichen '
      CALL HEREAD ( 'B', IUNIT, CHFN, IERR )
C
C *** Entscheidung ueber NEW
C
      IF ( NEW .EQ. 'N' ) THEN
         CALL HEINQNM ( CHFN(1:LCHFN), CHEXIST )
         IF ( CHEXIST .EQ. 'N' ) THEN
            IERR = 80
            GOTO 9999
         ENDIF
         IF ( CHEXIST .EQ. 'J' ) THEN
            INQUIRE ( FILE=CHFN(1:LCHFN), OPENED=LOPEN, NUMBER=IOPEN )
C *** Hier muss noch das Abfangen einer geoeffneten Datei rein
            IUNIT = IOPEN + 1
            OPEN ( UNIT=IUNIT, FILE=CHFN, STATUS=CHSTAT1, ERR=101)
            IERR = 0
         ENDIF
      ELSE IF ( NEW .EQ. 'J' ) THEN
         OPEN ( UNIT=IUNIT, FILE=CHFN, STATUS=CHSTAT2, ERR=101)
         IERR = 0
      ENDIF
      GOTO 9999
C
C *** Filenamen ueber Liste uebergeben
C
  200 CONTINUE
C
C *** Ermittlung der Laenge des Filenamen
C
      CALL CHLAEN ( CHFN, LCHFN )
C
C *** Entscheidung ueber NEW
C
      IF ( NEW .EQ. 'N' ) THEN
         CALL HEINQNM ( CHFN(1:LCHFN), CHEXIST )
C
         IF ( CHEXIST .EQ. 'N' ) THEN
            IERR = 80
            GOTO 9999
         ELSE IF ( CHEXIST .EQ. 'J' ) THEN
C
            IF ( CHEXIST .EQ. 'J' ) IUNIT = IUNIT + 1
            OPEN ( UNIT=IUNIT, FILE=CHFN(1:LCHFN), STATUS=CHSTAT1,
     +             IOSTAT=IERR, ERR=101)
C
         ENDIF
      ELSE IF ( NEW .EQ. 'J' ) THEN
C
            OPEN ( UNIT=IUNIT, FILE=CHFN(1:LCHFN), STATUS=CHSTAT2,
     +             IOSTAT=IERR, ERR=101)
C
      ENDIF
      GOTO 9999
C
C ======================================================================
CAT*  A U S G A B E T E I L
C ======================================================================
C
C *** FEHLERMELDUNGEN:
C
  101 CONTINUE
C
      write (*,*) ' In HEOPEN. Fehler beim Eroeffnen der Datei:'
      write (*,*) ' '
      write (*,*) ' IERR   ', IERR
      write (*,*) ' LCHFN  ', LCHFN
      write (*,*) ' CHFN   ', CHFN(1:LCHFN)
      write (*,*) ' NEW    ', NEW
      write (*,*) ' OPCODE ', OPCODE
      write (*,*) ' '
      call HEENTER
      GOTO 9999
C
  300 CONTINUE
      IERR = 70
      GOTO 9999
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
