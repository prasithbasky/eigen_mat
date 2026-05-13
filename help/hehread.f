C*****7**1*********2*********3*********4*********5*********6*********7**
      SUBROUTINE HEHREAD ( IUNIT, KTW, KTR, IAUS, CHCLEAR, CHENTER,
     +                     CHSYS, CHEDIT, IERR )
C
C
CA*   Unterprogramm zum lesen der Werte aus der Steuerdatei fuer die
C     Belegung des Common-Blockes STEUER. Das Unterprogramm setzt die
C     Existenz der Datei HEINIT.DAT voraus
C
CB*   Name = HEHREAD Hilfsroutine zum Einlesen der Werte aus HEINIT.DAT
C            -------
C
C
CD*   PROGRAMM-SYSTEM: HELP, Routinensammlung zur Programmentwicklung
C     ---------------
C
CE*   FUNKTION: Unterprogramm zur Ausgabe der Routinen-Kennungen
C     --------
C
CS*   PROGRAMMIER-SPRACHE: FORTRAN 77
C     --------------------
C
C
CF*   EINGABE:
C     -------
C
C     - IUNIT      Kanalnummer der Datei HEINIT.DAT 
C                  Diese MUSS geoeffnet sein!
C
C
CG*   AUSGABE:
C     -------
C
C     - KTW        Terminal write Kanal
C     - KTR        Terminal read Kanal
C     - IAUS       Steuergroesse, ob die Routinenkennung ausgegeben
C                  werden soll oder nicht.
C                  IAUS = 1,   d.h. Ausgabe der Routinenkennung auf
C                              Bildschirm
C                  IAUS = 0,   d.h. keine Ausgabe auf Bildschirm
C     - CHCLEAR    Steuergroesse fuer Hilfsprogramm CLEAR
C                  CHCLEAR = 'J', Bildschirm wir geloescht
C                  CHCLEAR = 'N', Bildschirm wir nicht geloescht
C     - CHENTER    Steuergroesse fuer Bildschirmpause
C                  CHENTER = 'J', Bildschirm wir angehalten
C                  CHENTER = 'N', Bildschirm wir nicht angehalten
C     - CHSYS      Steuergroesse fuer Betriebssystem
C                  CHSYS   = 'D', DOS Betriebssystem
C                  CHSYS   = 'U', UNIX Betriebssystem
C     - CHEDIT     Editor Steuerstring
C                  CHEDIT  = 'V'
C                  CHEDIT  = 'D'
C                  CHEDIT  = 'E'
C                  CHEDIT  = 'P'
C
C
C
C     - IERR       Fehlerkennung
C                  IERR =   0, d.h. alles i.o.
C                  IERR =  10, End of File beim Lesen
C                  IERR = 900, Daten koennen nicht eingelesen werden
C
C
CH*   INTERNE:
C     -------
C
C     - STRING     Eingelesener RECORD aus der Datei HEINIT.DAT
C                  Der String wird nicht interpretiert.
C     - CHCOMM     Hilfsgroesse, mit der der erste Eintrag verglichen
C                  wird. Falls der erste Eintrag in der ersten Spalte
C                  ein C ist, dann wird diese Zeile als Kommentar
C                  ueberlesen.
C     - IANFANG    Laufvariable ueber Anzahl der Teilstrings
C     - IBASE      Kennung, ab der der String interpretiert werden soll.
C     - IRES       Pointer zur erkannten Position im Auswahl-Opcode.
C     - IWERT      Integer Wert der Interpretation
C     - RWERT      Real Wert der Interpretation
C     - IFIRST     Pointer auf ersten Wert im zu untersuchenden Character
C     - ILAST      Pointer auf letzten Wert im zu untersucheneden Charact.
C     - IUNIT      Lesekanal der Datei HEINIT.DAT
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
C     - SUBROUTINE CRIANY
C     - SUBROUTINE HEREAD
C
C
CL*   DIESE ROUTINE WIRD AUFGERUFEN VON:
C     ---------------------------------
C
C     - SUBROUTINE HEINIT
C
C
CM*   ACHTUNG:     Die Werte, die aus der Datei gelesen werden,
C     -------      werden hier nicht an den COMMON STEUER uebergeben.
C                  Die Uebergabe erfolgt im rufenden Programm.
C
C
CN*   BEMERKUNG:   Das Unterprogramm liesst und interpretiert
C     ---------    die Werte aus HEINIT.DAT
C
C
C
CO*   AUTOR:       Axel Haenschke
C     -----        Firma
C                  Anschrift der Firma
C                  Abteilung:
C
C
CP*   VERSION:     0.1                                     08-AUG-1994
C     -------
C
C
CQ*   UPDATE:      Axel Haenschke                          10-Mar-2023
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
      INTEGER      KTW
      INTEGER      KTR
      INTEGER      IANZST
	INTEGER      IANFANG
      INTEGER      ISTRMAX
      INTEGER      IZ
      INTEGER      IAUS
      INTEGER      IERR
      INTEGER      IUNIT
      INTEGER      IBASE
      INTEGER      IRES
      INTEGER      IFIRST
      INTEGER      ILAST
      INTEGER      IWERT
      INTEGER      LNTSTR
	INTEGER      LCH
C
C ----------------------------------------------------------------------
C
      REAL         RWERT
C
C ----------------------------------------------------------------------
C
      CHARACTER    CHCOMM*1
      CHARACTER    CHCLEAR*1
      CHARACTER    CHSYS*1
      CHARACTER    CHENTER*1
      CHARACTER    CHEDIT*1
      CHARACTER    STRING*132
      CHARACTER    TSTRING*132
      CHARACTER    CHWERT*132
      CHARACTER    CHTYP*1
      CHARACTER    CHOUT*1
C
C ======================================================================
C
C
C ======================================================================
CIT*  I N I T I A L I S I E R U N G S T E I L
C ======================================================================
C
      IERR  = 0
C
      REWIND ( UNIT=IUNIT, IOSTAT=IERR, ERR=73 )
C
C ======================================================================
CEV*  E I N G A B E V E R A R B E I T U N G
C ======================================================================
C
C
      IZ      = 0
      ISTRMAX = 132
      IANZST  = 0
      IBASE   = 1
      IRES    = 0
      IWERT   = 0
      RWERT   = 0.0
      IFIRST  = 0
      ILAST   = 0
      STRING  = ' '
      CHOUT   = ' '
      CHTYP   = ' '
	CHWERT  = ' '
C
C --- Einlesen eines Stringe aus HEINIT.DAT
C
    1 CONTINUE	  	  
C
C --- Anfangswert fuer Teilstring setzen
C
      IANFANG = 1

      CALL HEREAD ( 'F', IUNIT, STRING, IERR )
C
C --- Fehlerbehandlung nach Einlesen
C
      IF ( IERR .EQ. 10 ) GOTO 100
      IF ( IERR .EQ. 900 ) GOTO 200
C
C ======================================================================
CVT*  V E R A R B E I T U N G S T E I L
C ======================================================================
C
C --- Auslesen der Anzahl der Teilstrings
C
      CALL HECHANZ ( STRING, ISTRMAX, IANZST, IERR )
C
C *** Chech des Teilstrings und Ueberlesen von Kommentar
C
C --- Check, if String is empty
C
      IF ( IANZST .EQ. 0 ) THEN
         IFIRST  = 0
         ILAST   = 0
         GOTO 2222
      ENDIF
C
C --- Ruecksprung nach Bearbeitung eines Teilstrings
C
    2 CONTINUE
C
C --- Herauslesen des Teilstrings
C
      CALL HECHSE  ( STRING, IANFANG, TSTRING, LNTSTR, IFIRST, ILAST,  
     +               IERR )

C
C --- Ueberlesen von Kommentarzeilen die mit "C" beginnen
C		 
		 IF ( STRING(1:1) .EQ. 'C' ) THEN
C			 WRITE (*,*) ' Kommentarzeile gefunden '
C              pause
               GOTO 1
		 ENDIF
C
C --- Typenprüfung des Teilstrings
C
      CALL HECHTYP ( TSTRING, CHTYP, IERR )
C
C --- Ruecksetzen der Ausgabewerte
C
      IWERT  = 0
      CHWERT = ' '
C
C --- Transformation in erkannten Variablentyp
C 
C     write (*,*) ' IANZST    = ', IANZST
C     write (*,*) ' STRING    = ', STRING
C     write (*,*) ' TSTRING   = ', TSTRING
C     write (*,*) ' IANFANG   = ', IANFANG
C     write (*,*) ' LNSTTR    = ', LNTSTR
C     write (*,*) ' IFIRST    = ', IFIRST
C     write (*,*) ' ILAST     = ', ILAST
C
C     write (*,*) ' CHTYP     = ', CHTYP
C     pause
C
      IF ( CHTYP .EQ. 'I' ) THEN
C
C ... INTEGER
C
         CALL HECHSE ( STRING, IANFANG, TSTRING, LNTSTR, IFIRST, ILAST, 
     +                 IERR )
         CALL HETYPINT ( TSTRING, IWERT, IERR )	  
C
C --- Zuweisung der gelesenen Integer-Groessen
C
        IF      ( IANFANG .EQ. 1 ) THEN
			   KTW  = IWERT
            ELSE IF ( IANFANG .EQ. 2 ) THEN
			   KTR  = IWERT
            ELSE IF ( IANFANG .EQ. 3 ) THEN
			   IAUS = IWERT
C --- zuviele Werte in Datei-Zeile fuer die Kanal-Zuweisungen
            ELSE IF ( IANFANG .GT. 3 ) THEN
			   GOTO 300
		ENDIF
C
      ELSE IF ( CHTYP .EQ. 'A' ) THEN
C
C ... CHARACTER, Standard
C
         CALL HECHSE ( STRING, IANFANG, TSTRING, LNTSTR, IFIRST, ILAST, 
     +                 IERR )
         CALL HETYPCHA ( TSTRING, CHWERT, LCH, IERR )		 
C
         CALL UPCASE (CHWERT(1:1), CHOUT)
C	
C        write (*,*) ' IANFANG    = ', IANFANG
C        write (*,*) ' TSTRING    = ', TSTRING
C        write (*,*) ' CHWERT     = ', CHWERT
C        write (*,*) ' CHOUT      = ', CHOUT
C        pause
C         
        IF ( IANFANG .EQ. 1 ) THEN
		    CHCLEAR = CHOUT
           ELSE IF ( IANFANG .EQ. 2 ) THEN
		    CHENTER = CHOUT
           ELSE IF ( IANFANG .EQ. 3 ) THEN
		    CHSYS   = CHOUT
           ELSE IF ( IANFANG .EQ. 4 ) THEN
		    CHEDIT  = CHOUT
           ELSE IF ( IANFANG .GT. 4 ) THEN
		    GOTO 300
		ENDIF
	 ENDIF
C    
C
C --- Hochzählen des Teilstring Zeigers und Ruecksprung nach 
C     Verarbeitung eines Teilstrings
C
      IANFANG = IANFANG + 1
C
      IF ( IANFANG .LE. IANZST ) THEN
           GOTO 2
	  ELSE
           GOTO 1
	  ENDIF
C
C *** End of File
C
  100 CONTINUE
C
C     CALL HECLEAR ( IERR )
      WRITE ( *,* ) ' '
      WRITE ( *,* ) ' End of File erreicht!'
      WRITE ( *,* ) ' Initialisierung abgeschlossen '
      WRITE ( *,* ) ' '
      CALL HEENTER
      GOTO 9999
C
C *** Ruecksetzen der Fehler
C
      IERR = 0
      GOTO 9999
C
C ======================================================================
CAT*  A U S G A B E T E I L
C ======================================================================
C
C *** FEHLERMELDUNGEN:
C
   73 CONTINUE
C
      WRITE ( *,* ) ' '
      WRITE ( *,* ) ' Datei kann nicht zurueckgesetzt werden!'
      WRITE ( *,* ) ' '
      CALL HEENTER
      GOTO 9999
C
  200 CONTINUE
      WRITE ( *,* ) ' '
      WRITE ( *,* ) ' Werte aus der Datei HEINIT.DAT koennen nicht '
      WRITE ( *,* ) ' gelesen werden !'
      WRITE ( *,* ) ' '
      CALL HEENTER
      GOTO 9999
C
  300 CONTINUE
      WRITE ( *,* ) ' '
      WRITE ( *,* ) ' Zu viele Werte in der Kanal-Zuweisung - Pruefen '
      WRITE ( *,* ) ' und aendern! '
      WRITE ( *,* ) ' '
      CALL HEENTER
      GOTO 9999
C  
C
 2222 CONTINUE
C
C *** Neuen String einlesen, nachdem einer fertig interpretiert wurde
C     (ist eigentlich kein Fehler)
C
C
      GOTO 1
C
C ======================================================================
CFA*  F O R M A T  - A N W E I S U N G E N
C ======================================================================
C
C
C ======================================================================
C
 9999 CONTINUE
C     WRITE (*,*) ' IANFANG    = ', IANFANG
C     WRITE (*,*) ' KTW        = ', KTW
C     WRITE (*,*) ' KTR        = ', KTR
C     WRITE (*,*) ' IAUS       = ', IAUS
C     WRITE (*,*) ' CHCLEAR    = ', CHCLEAR
C     WRITE (*,*) ' CHENTER    = ', CHENTER
C     WRITE (*,*) ' CHSYS      = ', CHSYS
C     WRITE (*,*) ' CHEDIT     = ', CHEDIT
C     PAUSE	  
C
      RETURN
      END
