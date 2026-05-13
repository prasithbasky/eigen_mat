!
!*****7**1*********2*********3*********4*********5*********6*********7**
!
      SUBROUTINE HMATERR ( INOPT, OPSTEUER, OUTOPT, IERR )
!
!
!
!B*   Name = hmaterr
!            -------
!
!
!D*   PROGRAMM-SYSTEM: HELP Umgebung
!     ---------------
!
!E*   FUNKTION: Fehlerausgabe auf Bildschirm bei Matrizen 
!     --------
!
!S*   PROGRAMMIER-SPRACHE: FORTRAN 77
!     --------------------
!
!
!F*   EINGABE:
!     -------
!
!     - INOPT       Eingabe OPCODE, aus Matrix
!     - OPSTEUER    Zulässiger, gewuenschte OPCODE
!     - OUTOPT      Ausgabe OPCODE
!
!
!G*   AUSGABE:
!     -------
!
!*****7**1*********2*********3*********4*********5*********6*********7**
!     - OUTOPT     Ausgabe OPCODE zur Steuerung der Fehlermeldungen
!                  Anweisungen:
!                  INOPT  = 'I', Ein String soll im Integer Format
!                  OUTOPT        gelesen werden
!                         = 'F', Ein String soll im Real Format gelesen
!                                werden. Standard Real
!                         = 'E'  Ein String soll im Real Format gelesen 
!                                werden, E-Format, mit großem E
!                         = 'C'  Ein String soll im Character Format
!                                gelesen werden. 
!                         = 'A'  Ein String soll im alpha-numeric 
!                                Format gelesen werden
!                         = 'X'  Ein String soll im Real Format gelesen
!                                werden, e-Format mit kleinem e 
!                         = 'Y'  Ein String soll im Double Precision 
!                                Format gelesen werden, D Format
!                         = 'Z'  Ein String soll im Double Precision
!                                Format gelesen werden, d-Forma
!
!     - IERR       Fehler-Return Code
!                  Bisher besetzt:
!                  IERR =   0, d.h. kein Fehler beim Lesen
!                  IERR = 901, Unzulässiger Wert für Matrix-Element 
!                              passt nicht zum zulässigen OPSTEUER
!                              Type ist Integer OPCODE = "I"
!                  IERR = 902, Unzulässiger Wert für Matrix-Element 
!                              passt nicht zum zulässigen OPSTEUER
!                              Type ist REAL OPCODE = "F"
!                  IERR = 903, Unzulässiger Wert für Matrix-Element 
!                              passt nicht zum zulässigen OPSTEUER
!                              Type ist REAL (E-Format) OPCODE = "E"
!                  IERR = 904, Unzulässiger Wert für Matrix-Element 
!                              passt nicht zum zulässigen OPSTEUER
!                              Type ist CHARACTER OPCODE = "C"
!                  IERR = 905, Unzulässiger Wert für Matrix-Element 
!                              passt nicht zum zulässigen OPSTEUER
!                              Type ist ALPHANUMERIC OPCODE = "A"
!                  IERR = 906, Unzulässiger Wert für Matrix-Element 
!                              passt nicht zum zulässigen OPSTEUER
!                              Type ist REAL (e-Format) OPCODE = 'X'
!                  IERR = 907, Unzulässiger Wert für Matrix-Element 
!                              passt nicht zum zulässigen OPSTEUER
!                              Type ist DOUBLE PRECISION OPCODE = "Y"
!                  IERR = 908, Unzulässiger Wert für Matrix-Element 
!                              passt nicht zum zulässigen OPSTEUER
!                              Type ist DOUBLE PRECISION OPCODE = "Z"
!                  IERR = 999, unzulaessiger OPCODE für Matrix
!
!
!
!H*   INTERNE:
!     -------
!
!     - KTW        Terminal Ausgabe Kanal aus "Steuer.inc"  gelesen
!
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
!     - SUBROUTINE keine
!
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
!........1.........2.........3.........4.........5.........6.........7.........8.........9........10
! 
      IMPLICIT                         NONE
!
! *** INTEGER
!
      INTEGER                          I 
      INTEGER                          IERROUT
      INTEGER                          IERR	  
!
! *** PARAMETER
!
!
! *** REAL Allocation!
! 
!
! *** DOUBLE PRECISION Allocation
!
!
!........1.........2.........3.........4.........5.........6.........7.........8.........9........10
!  
!
!
      CHARACTER                        OPSTEUER*1
      CHARACTER                        INOPT*1
	  CHARACTER                        OUTOPT*1
!
!
!........1.........2.........3.........4.........5.........6.........7.........8.........9........10
! 
       INCLUDE 'steuer.inc'
!
! ======================================================================
!IT*  I N I T I A L I S I E R U N G S T E I L
! ======================================================================
!
!
      CALL HEINOU ( 'HMATERR', 'EIN', IERROUT )
!
!
! ======================================================================
!EV*  E I N G A B E V E R A R B E I T U N G
! ======================================================================
!    
!
! *** Abfangen von Fehlern
!
!	  write (*,*) ' '
!	  write (*,*) ' INOPT     = ', INOPT
!	  write (*,*) ' OPSTEUER  = ', OPSTEUER 
!     write (*,*) ' OUTOPT    = ', OUTOPT	  
!
!     Fehler mit falschem Element oder Matrix (Integer)
!
      If ( INOPT .EQ. 'I' .AND. OPSTEUER .EQ. 'I' ) THEN
! 
         IERR = 0
		 OUTOPT = OPSTEUER
		 GOTO 9999
!     ELSE IF ( INOPT .EQ. 'I' .AND. OPSTEUER .NE. 'I' ) THEN
      ELSE IF ( INOPT .EQ. 'I'  ) THEN
!
	  write (*,*) ' '
	  write (*,*) ' Input Daten sind im INTEGER Format '
	  write (*,*) ' ---------------------------------- '
	  write (*,*) ' '
	  write (*,*) ' Oder ein Element im INTEGER Format'
	  write (*,*) ' Integer-Format nicht zulaessig! '
	  write (*,*) ' Programm Abbruch '
	  write (*,*) ' '
	     IERR = 901
		 		 OUTOPT = OPSTEUER
	     GOTO 9999
!
      ENDIF
!
!     Fehler mit falschem Element oder Matrix (REAL)
!
      If ( INOPT .EQ. 'F' .AND. OPSTEUER .EQ. 'F' ) THEN
!
         IERR = 0
		 OUTOPT = OPSTEUER
		 GOTO 9999
	  ELSE IF ( INOPT .EQ. 'F' .AND. OPSTEUER .NE. 'F' ) THEN
!
	  write (*,*) ' '
	  write (*,*) ' Input Daten sind im Real Format '
	  write (*,*) ' ------------------------------- '
	  write (*,*) ' '
	  write (*,*) ' Oder ein Element im Real Format'
	  write (*,*) ' Real-Format nicht zulaessig! '
	  write (*,*) ' Programm Abbruch '
	  write (*,*) ' '
	     IERR = 902
	     GOTO 9999
!
      ENDIF
!
!     Fehler mit falschem Element oder Matrix (E-REAL)
!
      If ( INOPT .EQ. 'E' .AND. OPSTEUER .EQ. 'E' ) THEN
!
         IERR = 0
		 OUTOPT = OPSTEUER
		 GOTO 9999
	  ELSE IF ( INOPT .EQ. 'E' .AND. OPSTEUER .NE. 'E' ) THEN
!
	  write (*,*) ' '
	  write (*,*) ' Input Daten sind im Real E-Format '
	  write (*,*) ' --------------------------------- '
	  write (*,*) ' '
	  write (*,*) ' Oder ein Element im Real E-Format'
	  write (*,*) ' Real-Format nicht zulaessig! '
	  write (*,*) ' Programm Abbruch '
	  write (*,*) ' '
         IERR = 903
	     GOTO 9999
!
      ENDIF
!
!     Fehler mit falschem Element oder Matrix (Character)
!
      If ( INOPT .EQ. 'C' .AND. OPSTEUER .EQ. 'C'  ) THEN
!
         IERR = 0
		 OUTOPT = OPSTEUER
		 GOTO 9999
      ELSE IF ( INOPT .EQ. 'C' .AND. OPSTEUER .NE. 'C'  ) THEN
!
	  write (*,*) ' '
	  write (*,*) ' Input Daten sind im Character-Format '
	  write (*,*) ' ------------------------------------ '
	  write (*,*) ' '
	  write (*,*) ' Oder ein Element im Character Format'
	  write (*,*) ' Character-Format nicht zulaessig! '
	  write (*,*) ' Programm Abbruch '
	  write (*,*) ' '
	     IERR = 904
	     GOTO 9999
!
      ENDIF
!
!     Fehler mit falschem Element oder Matrix (ALPHANUM.)
!
      If ( INOPT .EQ. 'A' .AND. OPSTEUER .EQ. 'A' ) THEN
!
         IERR = 0
		 OUTOPT = OPSTEUER
		 GOTO 9999
      ELSE IF ( INOPT .EQ. 'A' .AND. OPSTEUER .NE. 'A' ) THEN
!
	  write (*,*) ' '
	  write (*,*) ' Input Daten sind im Alphanum.-Format '
	  write (*,*) ' ------------------------------------ '
	  write (*,*) ' '
	  write (*,*) ' Oder ein Element im Alphanum. Format'
	  write (*,*) ' Alphanum.-Format nicht zulaessig! '
	  write (*,*) ' Programm Abbruch '
	  write (*,*) ' '
         IERR = 905
		 GOTO 9999
!	  
      ENDIF
!
!     Fehler mit falschem Element oder Matrix (e-Format)
!
      If ( INOPT .EQ. 'X' .AND. OPSTEUER .EQ. 'X' ) THEN
!
         IERR = 0
		 OUTOPT = OPSTEUER
		 GOTO 9999
      ELSE IF ( INOPT .EQ. 'X' .AND. OPSTEUER .NE. 'X' ) THEN 
!
	  write (*,*) ' '
	  write (*,*) ' Input Daten sind im REAL e-Format '
	  write (*,*) ' --------------------------------- '
	  write (*,*) ' '
	  write (*,*) ' Oder ein Element im REAL e-Format'
	  write (*,*) ' REAL e-Format nicht zulaessig! '
	  write (*,*) ' Programm Abbruch '
	  write (*,*) ' '
	     IERR = 906
	     GOTO 9999
!	  
      ENDIF
!
!     Fehler mit falschem Element oder Matrix (DOUBLE D)
!
      If ( INOPT .EQ. 'Y' .AND. OPSTEUER .EQ. 'Y') THEN
!
         IERR = 0
		 OUTOPT = OPSTEUER
		 GOTO 9999
!     ELSE IF ( INOPT .EQ. 'Y' .AND. OPSTEUER .NE. 'Y') THEN
      ELSE IF ( INOPT .EQ. 'Y' ) THEN
!
	  write (*,*) ' '
	  write (*,*) ' Input Daten sind im DOUBLE D-Format '
	  write (*,*) ' ----------------------------------- '
	  write (*,*) ' '
	  write (*,*) ' Oder ein Element im DOUBLE D-Format'
	  write (*,*) ' DOUBLE D-Format nicht zulaessig! '
	  write (*,*) ' Programm Abbruch '
	  write (*,*) ' '
	     IERR = 907
	     GOTO 9999
!	  
      ENDIF
!
!     Fehler mit falschem Element oder Matrix (DOUBLE d)
!
      If ( INOPT .EQ. 'Z' .AND. OPSTEUER .EQ. 'Z') THEN
!
         IERR = 0
		 OUTOPT = OPSTEUER
		 GOTO 9999
      ELSE IF ( INOPT .EQ. 'Z' .AND. OPSTEUER .NE. 'Z') THEN
!
	  write (*,*) ' '
	  write (*,*) ' Input Daten sind im DOUBLE d-Format '
	  write (*,*) ' ----------------------------------- '
	  write (*,*) ' '
	  write (*,*) ' Oder ein Element im DOUBLE d-Format'
	  write (*,*) ' DOUBLE d-Format nicht zulaessig! '
	  write (*,*) ' Programm Abbruch '
	  write (*,*) ' '
	     IERR = 908
	     GOTO 9999
!	  
      ENDIF
!
 9999 CONTINUE
!
!	  write (*,*) ' '	  
!     write (*,*) ' OUTOPT = ', OUTOPT
!
      CALL HEINOU ( 'HMATERR', 'AUS', IERROUT )
!	  
      RETURN
!
      END