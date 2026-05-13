!
!*****7**1*********2*********3*********4*********5*********6*********7**
!
      SUBROUTINE HREADMAT ( OPCODE, CHNAME, IUNIT,
     +                      IMAT, RMAT, DMAT, 
     +                      IDIML, IDIMC, ILINES, ICOLUM, IERR )
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
!
!     - CHNAME     Name der Eingabedatei (Matrix)
!     - IMAT       Elemente der Matrix (ILINES,ICOLUM) Integer Werte
!     - RMAT       Elemente der Matrix (ILINES,ICOLUM) REAL Werte
!     - DMAT       Elemente der Matrix (ILINES,ICOLUM) Double Precision
!                  Werte
!     - IDIML      Anzahl der Zeilen der Matrix - Dimension
!     - IDIMC      Anzahl der Spalten der Matrix - Dimension
!
!G*   AUSGABE:
!     -------
!
!     - IUNIT      Kanalnummer der Datei von der gelesen werden soll.
!     - IMAT       Elemente der Matrix (ILINES,ICOLUM) Integer Werte
!     - RMAT       Elemente der Matrix (ILINES,ICOLUM) REAL Werte
!     - DMAT       Elemente der Matrix (ILINES,ICOLUM) Double Precision
!                  Werte
!     - ILINES     Anzahl der aktuellen Zeilen der Matrix
!     - ICOLUM     Anzahl der aktuellen Spalten der Matrix
!
!     - IERR       Fehler-Return Code
!                  Bisher besetzt:
!                  IERR =   0, d.h. kein Fehler beim Lesen
!                  IERR =  10, d.h. End of file beim Lesen von Datei
!                  IERR = 100, String ist leer - Leerzeile
!                  IERR = 900, Unzulässiger Wert für Matrix-Element 
!                              passt nicht zum gewählten OPCODE
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
	  CHARACTER    CHNAME*(*)
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
      INTEGER      IUNIT
      INTEGER      IMAT (IDIML, IDIMC)
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
! ----------------------------------------------------------------------
!
      REAL         RWERT
	  REAL         RMAT (IDIML, IDIMC)
!
! ----------------------------------------------------------------------
!
      DOUBLE PRECISION DBWERT
	  DOUBLE PRECISION DMAT (IDIML, IDIMC)
!
! ----------------------------------------------------------------------
!
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
      CALL HEINOU ( 'HREADMAT', 'EIN', IERR )
!
!
! ======================================================================
!EV*  E I N G A B E V E R A R B E I T U N G
! ======================================================================
!
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
         CALL HEOPEN ( 'K', 'N', CHNAME , iutest, ierr )
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
!			write (*,*) ' ILINES = ', ILINES
!			call heenter
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
!     write (*,*) ' in Spaltenschleife '
!	  write (*,*) ' '
! 	  write (*,*) ' STRING  = ', STRING
! 	  write (*,*) ' TSTRING = ', TSTRING
! 	  write (*,*) ' CHTYP   = ', CHTYP
!	  write (*,*) ' OPCODE  = ', OPCODE
!	  write (*,*) ' ISTRMAX = ', ISTRMAX
! 	  write (*,*) ' IANZST  = ', IANZST
!	  write (*,*) ' IOUT    = ', IOUT
! 	  write (*,*) ' IERR    = ', IERR
! 	  write (*,*) ' ICOLUM  = ', ICOLUM
! 	  write (*,*) ' ILINES  = ', ILINES
! 	  call heenter
!
! ... Ruecksetzen der Ausgabewerte
!
      IWERT  = 0
      RWERT  = 0.0
      DBWERT = 0.0
      CHWERT = ' '
!
!
!
! *** 7.) Transformation in erkannten Variablentyp
!     --------------------------------------------
!     Schleife über Anzahl der Spalten (IANZST)
!
!
      IF ( CHTYP .EQ. 'I' .AND. OPCODE .EQ. 'I' ) THEN
!
! ... INTEGER-Matrix
!
         CALL HECHSE ( STRING, IANFANG, TSTRING, LNTSTR, IFIRST, ILAST,
     +                 IERR )
         CALL HETYPINT ( TSTRING, IWERT, IERR )
!
         IMAT(ILINES, ICOLUM) = IWERT
!
      ELSE IF ( CHTYP .EQ. 'F' .AND. OPCODE .EQ. 'F' ) THEN
!
! ... REAL, F-Format Standard
!
         CALL HECHSE ( STRING, IANFANG, TSTRING, LNTSTR, IFIRST, ILAST,
     +                 IERR )
         CALL HETYPREA ( TSTRING, RWERT, IERR )
!
         RMAT(ILINES, ICOLUM) = RWERT
! 
      ELSE IF ( CHTYP .EQ. 'E' ) THEN
!
! ... REAL, E-Format Standard, wobei noch geprueft wird, ob im Teilstring
!     ein Dezimalpunkt enthalten ist. Falls nicht, dann in Character
!     gelassen. (Diese Einstellung kann auch geaendert werden).
!
         CALL HECHSE ( STRING, IANFANG, TSTRING, LNTSTR, IFIRST, ILAST,
     +                 IERR )
         IF ( CHDOT .EQ. 'N' ) THEN
            CALL HETYPCHA ( TSTRING, CHWERT, LCH, IERR )
         ELSE IF (CHDOT .EQ. 'J' ) THEN
            CALL HETYPRIA ( TSTRING, RWERT, IERR )
         ENDIF
!
         RMAT(ILINES, ICOLUM) = RWERT
      ELSE IF ( CHTYP .EQ. 'C' .AND. OPCODE .EQ. 'C' ) THEN
!
! ... CHARACTER, Standard
!
         CALL HECHSE ( STRING, IANFANG, TSTRING, LNTSTR, IFIRST, ILAST,
     +                 IERR )
         CALL HETYPCHA ( TSTRING, CHWERT, LCH, IERR )
!
         IERR = 999
		 GOTO 9999
!
      ELSE IF ( CHTYP .EQ. 'A' .AND. OPCODE .EQ. 'A' ) THEN
!
! ... CHARACTER, Alphanumerischer Ausdruck
!
         CALL HECHSE ( STRING, IANFANG, TSTRING, LNTSTR, IFIRST, ILAST,
     +                 IERR )
         CALL HETYPCHA ( TSTRING, CHWERT, LCH, IERR )
!
            IERR = 999
		    GOTO 9999
!
      ELSE IF ( CHTYP .EQ. 'X' .AND. OPCODE .EQ. 'X' ) THEN
!
! ... REAL, e-Format, mit kleinem e
!
         CALL HECHSE ( STRING, IANFANG, TSTRING, LNTSTR, IFIRST, ILAST,
     +                 IERR )
         CALL HETYPREX ( TSTRING, RWERT, IERR )
!
         RMAT(ILINES, ICOLUM) = RWERT
!
      ELSE IF ( CHTYP .EQ. 'Y' .AND. OPCODE .EQ. 'Y' ) THEN
!
! ... DOUBLE Precision Standard
!
         CALL HECHSE ( STRING, IANFANG, TSTRING, LNTSTR, IFIRST, ILAST,
     +                 IERR )
         CALL HETYPDB ( TSTRING, DBWERT, IERR )
!
         DMAT(ILINES,ICOLUM) = DBWERT
!
      ELSE IF ( CHTYP .EQ. 'Z' .AND. OPCODE .EQ. 'Z') THEN
!
! ... DOUBLE Precision, mit kleinem d
!
         CALL HECHSE ( STRING, IANFANG, TSTRING, LNTSTR, IFIRST, ILAST,
     +                 IERR )
         CALL HETYPDB ( TSTRING, DBWERT, IERR )
!
         DMAT(ILINES, ICOLUM) = DBWERT
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
      CALL HEINOU ( 'HREADMAT', 'AUS', IERR1 )
!
!	  
! *** Ende der Subroutine HREADMAT
!
      RETURN
!
      END
