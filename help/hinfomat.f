!
!*****7**1*********2*********3*********4*********5*********6*********7**
!
      SUBROUTINE HINFOMAT ( IUNIT, ILINES, ICOLUM, OPCODE, IERR )
!
!
!
!B*   Name = hreadmat
!            --------
!
!
!D*   PROGRAMM-SYSTEM: beliebig
!     ---------------
!
!E*   FUNKTION: Einlesen einer Matrix über Stringfunktionen
!     --------
!
!S*   PROGRAMMIER-SPRACHE: FORTRAN 77
!     --------------------
!
!
!F*   EINGABE:
!     -------
!
!
!     - IUNIT      Kanalnummer der Datei von der gelesen werden soll.
!
!G*   AUSGABE:
!     -------
!
!*****7**1*********2*********3*********4*********5*********6*********7**
!     - OPCODE     Bearbeitungscode, das Programm reagiert auf folgende
!                  Anweisungen:
!                  OPCODE = 'I', Ein String soll im Integer Format
!                                gelesen werden
!                  OPCODE = 'F', Ein String soll im Real Format gelesen
!                                werden. Standard Real
!                  OPCODE = 'E'  Ein String soll im Real Format gelesen 
!                                werden, E-Format, mit großem E
!                  OPCODE = 'C'  Ein String soll im Character Format
!                                gelesen werden. 
!                  OPCODE = 'A'  Ein String soll im alpha-numeric 
!                                Format gelesen werden
!                  OPCODE = 'X'  Ein String soll im Real Format gelesen
!                                werden, e-Format mit kleinem e 
!                  OPCODE = 'Y'  Ein String soll im Double Precision 
!                                Format gelesen werden, D Format
!                  OPCODE = 'Z'  Ein String soll im Double Precision
!                                Format gelesen werden, d-Forma
!     - ILINES     Anzahl der aktuellen Zeilen der Matrix
!     - ICOLUM     Anzahl der aktuellen Spalten der Matrix
!
!     - IERR       Fehler-Return Code
!                  Bisher besetzt:
!                  IERR =   0, d.h. kein Fehler beim Lesen
!                  IERR =  10, d.h. End of file beim Lesen von Datei
!                  IERR = 100, String ist leer - Leerzeile
!                  IERR = 901, Unzulässiger Wert für Matrix-Element 
!                              passt nicht zum zulässigen OPCODE "Y"
!                              Type ist Integer OPCODE = "I"
!                  IERR = 902, Unzulässiger Wert für Matrix-Element 
!                              passt nicht zum zulässigen OPCODE "Y"
!                              Type ist REAL OPCODE = "F"
!                  IERR = 903, Unzulässiger Wert für Matrix-Element 
!                              passt nicht zum zulässigen OPCODE "Y"
!                              Type ist REAL (E-Format) OPCODE = "E"
!                  IERR = 904, Unzulässiger Wert für Matrix-Element 
!                              passt nicht zum zulässigen OPCODE "Y"
!                              Type ist CHARACTER OPCODE = "C"
!                  IERR = 905, Unzulässiger Wert für Matrix-Element 
!                              passt nicht zum zulässigen OPCODE "Y"
!                              Type ist ALPHANUMERIC OPCODE = "A"
!                  IERR = 906, Unzulässiger Wert für Matrix-Element 
!                              passt nicht zum zulässigen OPCODE "Y"
!                              Type ist REAL (e-Format) OPCODE = "X"
!                  IERR = 907, Unzulässiger Wert für Matrix-Element 
!                              passt nicht zum zulässigen OPCODE "Y"
!                              Type ist DOUBLE PRECISION OPCODE = "Z"
!                  IERR = 999, unzulaessiger OPCODE für Matrix
!
!
!
!H*   INTERNE:
!     -------
!
!     - sind hier nicht im einzelnen beschrieben, reine Faulheit.
!
!
!I*   COMMON:
!     ------
!
!     - STEUER     ueber Include Datei 'steuer.inc'
!
!
!J*   BENOETIGTE UNTERPROGRAMME / FUNKTIONEN:
!     --------------------------------------
!
!     - SUBROUTINE HEREAD
!     - SUBROUTINE HECHANZ
!     - SUBROUTINE HECHSE
!     - SUBROUTINE HECHTYP
!     - SUBROUTINE HETYPINI
!     - SUBROUTINE HETYPREA
!     - SUBROUTINE HETYPRIA
!     - SUBROUTINE HETYPREX
!     - SUBROUTINE HETYPCHA
!     - SUBROUTINE HETYPDB
!     - SUBROUTINE HECHDOT
!     - SUBROUTINE HEENTER
!     - SUBROUTINE HECLEAR
!
!L*   DIESE ROUTINE WIRD AUFGERUFEN VON:
!     ---------------------------------
!
!     - SUBROUTINE Name
!
!
!M*   ACHTUNG:     Die Help-Routinen arbeiten nur nach Initialisierung
!     -------      der Help-Umgebung und Anlegen der Datei HEINIT.DAT.
!                  Diese Datei wird, falls sie nicht vorhanden ist, wird
!                  sie auf dem Verzeichnis erstellt, auf dem die Anwendung
!                  aufgerufen wird. Falls die Datei vorhanden ist
!                  kann diese den Beduerfnissen Angepasst werden.
!                  Wenn prinzipiell nicht mit der Initialisierungsdatei
!                  gearbeitet werden soll, dann das Character-Help
!                  System mit der Default-Routine initialisieren.
!
!
!
!
!N*   BEMERKUNG:   Das Hauptprogramm stellt eine jeweils angepasste
!     ---------    Umgebung fuer Character-Interpretationen dar.
!
!                  Weitere Unterprogramme der Library 'libhelp.a'
!                  sind in einem Doku-Ordner beschrieben.
!
!                  Nach Initialisierung der Help-Umgebung und Anlegen
!                  der HEINIT.DAT Datei wird das Programm abgeschlossen.
!                  Das ist nur beim ersten Programmlauf der Fall.
!                  Das Programm muss dann nochmals gestartet werden.
!                  Beim Arbeiten mit den Default-Werten ist das
!                  nicht der Fall.
!
!
!
!O*   AUTOR:       Axel Haenschke
!     -----        Firma
!                  Anschrift der Firma
!                  Abteilung:
!
!
!P*   VERSION:     0.1                                     03.08.94
!     -------
!
!
!Q*   UPDATE:      Axel Haenschke                          24.02.95
!     -------      Anpassung an IBM-RS6000
!                  Compiled for Windows                    20.01.2023
!                  Entwicklungsumgebung Ein/Ausgabe        03.03.2026
!                  für Open Radioss
!            
! ----------------------------------------------------------------------
!
!R*   LITERATUR:   20
!     ---------
!
!*****7**1*********2*********3*********4*********5*********6*********7**
!
!
! ======================================================================
!VD*  V A R I A B L E N   D E K L A R A T I O N
! ======================================================================
!
!
      IMPLICIT     NONE
!
! ----------------------------------------------------------------------
!
      CHARACTER    OPCODE*1
      CHARACTER    STRING*132
      CHARACTER    TSTRING*132
      CHARACTER    CHWERT*132
      CHARACTER    JN*132
      CHARACTER    STRAB*132
      CHARACTER    CHTYP*1
      CHARACTER    CHDOT*1
	  CHARACTER    CHCOMM*1
!
! ----------------------------------------------------------------------
!
      INTEGER      I
      INTEGER      IUNIT
      INTEGER      IANFANG
      INTEGER      IBASE
      INTEGER      IRESULT
      INTEGER      IWERT
      INTEGER      IFIRST
      INTEGER      ILAST
      INTEGER      IERR
	  INTEGER      IERR1
      INTEGER      IOUT
	  INTEGER      ISTRMAX
      INTEGER      IUTEST
      INTEGER      IANZST
	  INTEGER      IDIML
	  INTEGER      IDIMC
	  INTEGER      ILINES
	  INTEGER      ICOLUM
	  INTEGER      ICOUNT
	  INTEGER      J
      INTEGER      LNTSTR
      INTEGER      LCH
!
! ======================================================================
!
      ISTRMAX  = 132
      IERR     = 0
      IOUT     = 0
	  ICOUNT   = 0
      IBASE    = 1
      IANFANG  = 1
	  ILINES   = 1
      CHTYP    = ' '
      CHDOT    = ' '
!
! ======================================================================
!IT*  I N I T I A L I S I E R U N G S T E I L
! ======================================================================
!
      CALL HEINOU ( 'HINFOMAT', 'EIN', IERR )
!
!
! ======================================================================
!EV*  E I N G A B E V E R A R B E I T U N G
! ======================================================================
!
!
!      WRITE (*, 321)
!
!  321 FORMAT ( /
!     +,/,         '    Testdaten werden von der Datei test.dat '
!     +,/,         '    identifiziert ! ',  ///           )
!
!      call heenter
! 
!	  
! ... Ruecksprung aus der Stringverarbeitung, falls gesamter String
!     abgearbeitet.
!
    1 CONTINUE
!
      IANFANG  = 1
      IERR     = 0
!	  
!
!
! *** Lesen eines Strings vom File, Vor dem ersten Zugriff muss
!     die Datei geoeffnet sein.
!     -------------------------------------------------------------
!
      IF ( ILINES .GT. 1 ) GOTO 77
         CALL HEOPEN ( 'K', 'N', 'test.dat' , iutest, ierr )
   77 CONTINUE
!
      IUNIT = iutest
!
      CALL HEREAD ( 'F', iutest, string, ierr )
!
!
! *** Test ob Kommentarzeile
!
      CALL HECOMM ( STRING, CHCOMM )
!
!     write (*,*) ' CHCOMM   = ', CHCOMM
!	  call heenter
!
      IF ( CHCOMM .EQ. 'J' ) GOTO 9998
!
! ... End of File  erkannt und Bearbeitung abgebrochen.
!
      IF ( IERR .EQ. 10 ) THEN
			ILINES = ILINES - 1
            REWIND ( UNIT = iutest )
			CLOSE  ( UNIT = iutest )
         GOTO 9999
      ENDIF
!
! ... Einlesen beenden
!
  320 CONTINUE
!
! ======================================================================
!VT*  V E R A R B E I T U N G S T E I L
! ======================================================================
!
!
! *** 4.) Erkennen der Anzahl der Teilstrings im eingelesenen String
!     --------------------------------------------------------------
!
!
      CALL HECHANZ ( STRING, ISTRMAX, IANZST, IERR )
!
!     Anzahl der Strings ist Anzahl der Spalten 
!
      ICOLUM = 1
!
! ... Ruecksprung nach Abarbeitung des Teilstrings
!
    2 CONTINUE
!
! ... Testen, ob String leer oder besetzt ist und
!     Abbruchkriterium für Zeilenschleife
!
      IF ( IANZST .GT. 0) THEN
	       ICOUNT = IANZST
	  ENDIF
!
      IF ( IANZST .EQ. 0 ) THEN
         IFIRST  =    0
         ILAST   =    0
		 IERR    =  100
!
         ILINES = ILINES -1
		 ICOLUM = ICOUNT
!
            REWIND ( UNIT = iutest )
			CLOSE  ( UNIT = iutest )
!
         GOTO 9999
      ENDIF
!
! *** 5.) Herauslesen des aktuellen Teilstrings
!     -----------------------------------------
!
      CALL HECHSE  ( STRING, IANFANG, TSTRING, LNTSTR, IFIRST, ILAST,
     +               IERR )
!
! *** 6.) Typenpruefung des Teilstrings, der aktuell ausgelesen wurde.
!     ----------------------------------------------------------------
!
      CALL HECHTYP ( TSTRING, CHTYP, IERR )
!
! ... Laufvariable über Anzahl der Teilstrings IOUT
!
!
      IOUT   = IOUT + 1
!
!
! *** 7.) Schleife über Anzahl der Spalten (IANZST)
!
!
      IF ( CHTYP .EQ. 'I' ) THEN
!
! ... INTEGER-Matrix
!
         OPCODE = 'I'
	     IERR   = 901
	     GOTO 9999
!
      ELSE IF ( CHTYP .EQ. 'F' ) THEN
!
! ... REAL, F-Format Standard
!
         OPCODE = 'F'
	     IERR   = 902
	     GOTO 9999	  
! 
      ELSE IF ( CHTYP .EQ. 'E' ) THEN
!
         OPCODE = 'E'
	     IERR   = 903
	     GOTO 9999	  
!
      ELSE IF ( CHTYP .EQ. 'C' ) THEN
!
! ... CHARACTER, Standard
!
         OPCODE = 'C'
         IERR   = 904
		 GOTO 9999
!
      ELSE IF ( CHTYP .EQ. 'A' ) THEN
!
! ... CHARACTER, Alphanumerischer Ausdruck
!
         OPCODE = 'A'
         IERR   = 905
		 GOTO 9999
!
      ELSE IF ( CHTYP .EQ. 'X' ) THEN
!
! ... REAL, e-Format, mit kleinem e
!
         OPCODE = 'X'
         IERR   = 906
		 GOTO 9999	  
!
      ELSE IF ( CHTYP .EQ. 'Y' ) THEN
!
! ... DOUBLE Precision Standard
!
      OPCODE = 'Y'
!
      ELSE IF ( CHTYP .EQ. 'Z' ) THEN
!
! ... DOUBLE Precision, mit kleinem d
!
         OPCODE = 'Z'
         IERR   = 907
		 GOTO 9999		 
!
         ELSE
!
            IERR = 900
		    GOTO 9999		    
!
      ENDIF
!
! ... Hochzaehlen des Teilstring-Zeigers IANFANG
!
      IANFANG = IANFANG + 1
!
! ... Ruecksprung, da noch Teilstrings vorhanden
!
!     IF ( IOUT .LT. IANZST ) THEN
      IF ( ICOLUM .LT. IANZST ) THEN	  
	     ICOLUM = ICOLUM + 1
         GOTO 2
      ENDIF
!
!
!
!
! *** Hochzählen der Zeilenzahl
!
      ILINES = ILINES + 1
!
! ======================================================================
!AT*  A U S G A B E T E I L
! ======================================================================
!
!
 9998 CONTINUE
!
         GOTO 1
!
! ======================================================================
!FA*  F O R M A T  - A N W E I S U N G E N
! ======================================================================
!
 9999 CONTINUE
!
!
      CALL HEINOU ( 'HINFOMAT', 'AUS', IERR1 )
!
!	  
! *** Ende der Subroutine HREADMAT
!
      RETURN
!
      END
