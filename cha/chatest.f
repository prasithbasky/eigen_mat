C*****7**1*********2*********3*********4*********5*********6*********7**
      PROGRAM CHATEST
C
C
CA*   Hauptprogramm zum Testen der Character-I/O Routinen
C     Unabhaengig von einem System
C
C     +--------------------------------------------------------------+
C     |                                                              |
C     | Das Hauptprogramm stellt eine Beispielsammlung fuer die      |
C     | Anwendung der Routinen HE... dar.                            |
C     |                                                              |
C     | Ausgefuehrt sind in diesem Testprogramm folgende Schritte:   |
C     |                                                              |
C     | 1.) Initialisieren der Testungebung  *)                      |
C     |                                                              |
C     | 2.) Lesen eines Strings vom Bildschirm                       |
C     |                                                              |
C     | 3.) Lesen eines Strings von einer Datei                      |
C     |                                                              |
C     | 4.) Erkennen der Anzahl der Teilstrings im eingelesenen      |
C     |     String                                                   |
C     |                                                              |
C     | 5.) Herauslesen des Teilstrings                              |
C     |                                                              |
C     | 6.) Erkennen des Typs des Teilstrings                        |
C     |                                                              |
C     | 7.) Ausgabe des Typs auf Variable des erkannten Typs         |
C     |                                                              |
C     | 8.) Anzeigen des Ergebnis                                    |
C     |                                                              |
C     |                                                              |
C     | *) Wenn nicht mit der Testumgebung gearbeitet werden soll,   |
C     |    dann nur mit der Default-Initialisierung arbeiten.        |
C     |    Default-Initialisierung bedeutet mit der Routine HEDEF    |
C     |    arbeiten. Sonst die normale Initialisierung mit HEINIT.   |
C     |    Eine dieser Routinen muss gewaehlt werden, da sonst       |
C     |    Fehlverhalten auftreten kann.                             |
C     |                                                              |
C     |                                                              |
C     |                                                              |
C     | Das Programm lauft in einer Schleife ab, die beliebig oft    |
C     | wiederholt werden kann.                                      |
C     |                                                              |
C     |                                                              |
C     | Bei Fragen an mich wenden.                                   |
C     |                                                              |
C     |                                                              |
C     |                                                              |
C     |                                           Axel Haenschke     |
C     |                                                              |
C     |                                           Stand: 18.12.1995  |
C     |                                           .                  |
C     |                                           .                  |
C     +--------------------------------------------------------------+
C
C
C
C
C
C
C
CB*   Name = CHATEST
C            -------
C
C
CD*   PROGRAMM-SYSTEM: beliebig
C     ---------------
C
CE*   FUNKTION: Character-Interpretation
C     --------
C
CS*   PROGRAMMIER-SPRACHE: FORTRAN 77
C     --------------------
C
C
CF*   EINGABE:
C     -------
C
C     - keine
C
C
CG*   AUSGABE:
C     -------
C
C     - keine
C
C
CH*   INTERNE:
C     -------
C
C     - sind hier nicht im einzelnen beschrieben, reine Faulheit.
C
C
CI*   COMMON:
C     ------
C
C     - STEUER     ueber Include Datei 'steuer.inc'
C
C
CJ*   BENOETIGTE UNTERPROGRAMME / FUNKTIONEN:
C     --------------------------------------
C
C     - SUBROUTINE HEDEF
C     - SUBROUTINE HEINIT
C     - SUBROUTINE HEHEADER ( noch nicht installiert )
C     - SUBROUTINE HEINOU
C     - SUBROUTINE HEOPEN
C     - SUBROUTINE HEREAD
C     - SUBROUTINE HECHANZ
C     - SUBROUTINE HECHSE
C     - SUBROUTINE HECHTYP
C     - SUBROUTINE HETYPINI
C     - SUBROUTINE HETYPREA
C     - SUBROUTINE HETYPRIA
C     - SUBROUTINE HETYPREX
C     - SUBROUTINE HETYPCHA
C     - SUBROUTINE HETYPDB
C     - SUBROUTINE HECHDOT
C     - SUBROUTINE HEENTER
C     - SUBROUTINE HECLEAR
C
CL*   DIESE ROUTINE WIRD AUFGERUFEN VON:
C     ---------------------------------
C
C     - SUBROUTINE Name
C
C
CM*   ACHTUNG:     Die Help-Routinen arbeiten nur nach Initialisierung
C     -------      der Help-Umgebung und Anlegen der Datei HEINIT.DAT.
C                  Diese Datei wird, falls sie nicht vorhanden ist, wird
C                  sie auf dem Verzeichnis erstellt, auf dem die Anwendung
C                  aufgerufen wird. Falls die Datei vorhanden ist
C                  kann diese den Beduerfnissen Angepasst werden.
C                  Wenn prinzipiell nicht mit der Initialisierungsdatei
C                  gearbeitet werden soll, dann das Character-Help
C                  System mit der Default-Routine initialisieren.
C
C
C
C
CN*   BEMERKUNG:   Das Hauptprogramm stellt eine jeweils angepasste
C     ---------    Umgebung fuer Character-Interpretationen dar.
C
C                  Weitere Unterprogramme der Library 'libhelp.a'
C                  sind in einem Doku-Ordner beschrieben.
C
C                  Nach Initialisierung der Help-Umgebung und Anlegen
C                  der HEINIT.DAT Datei wird das Programm abgeschlossen.
C                  Das ist nur beim ersten Programmlauf der Fall.
C                  Das Programm muss dann nochmals gestartet werden.
C                  Beim Arbeiten mit den Default-Werten ist das
C                  nicht der Fall.
C
C
C
CO*   AUTOR:       Axel Haenschke
C     -----        Firma
C                  Anschrift der Firma
C                  Abteilung:
C
C
CP*   VERSION:     0.1                                     03.08.94
C     -------
C
C
CQ*   UPDATE:      Axel Haenschke                          24.02.95
C     -------      Anpassung an IBM-RS6000
C                  Compiled for Windows                    20.01.2023
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
      CHARACTER    STRING*132
      CHARACTER    TSTRING*132
      CHARACTER    CHWERT*132
      CHARACTER    JN*132
      CHARACTER    STRAB*132
      CHARACTER    CHTYP*1
      CHARACTER    CHDOT*1
C
C ----------------------------------------------------------------------
C
      INTEGER      IANFANG
      INTEGER      IBASE
      INTEGER      IRESULT
      INTEGER      IWERT
      INTEGER      IFIRST
      INTEGER      ILAST
      INTEGER      IERR
      INTEGER      IOUT
      INTEGER      IUTEST
      INTEGER      IANZST
      INTEGER      IZEILE
      INTEGER      LNTSTR
      INTEGER      LCH
C
C ----------------------------------------------------------------------
C
      REAL         RWERT
C
C ----------------------------------------------------------------------
C
      DOUBLE PRECISION DBWERT
C
C ----------------------------------------------------------------------
C
C
C ======================================================================
C
      IERR     = 0
      IOUT     = 0
      IBASE    = 1
      IANFANG  = 1
      IZEILE   = 1
      CHTYP    = ' '
      CHDOT    = ' '
C
C ======================================================================
CIT*  I N I T I A L I S I E R U N G S T E I L
C ======================================================================
C
C
C *** 1.) Initialisieren der Help-Umgebung
C     ------------------------------------
C
C ... Definitionen der Help-Defaults
C
      CALL HEDEF ( IERR )
C
C ... Initialisierung der Help-Umgebung
C
      CALL HEINIT ( IERR )
      IF ( IERR .EQ. 1 ) THEN
         WRITE (*, 2000)
         GOTO 9998
      ENDIF
C
C ======================================================================
CEV*  E I N G A B E V E R A R B E I T U N G
C ======================================================================
C
C ... Interaktion zur Arbeitsweise
C     Abfrage, ob vom Bildschirm oder von Datei gelesen werden soll
C
  329 CONTINUE
      CALL HECLEAR ( IERR )
      WRITE (*,*) ' '
      WRITE (*,*) ' '
      WRITE (*,*) ' ---------------------------------------------------'
	  WRITE (*,*) ' '
	  WRITE (*,*) '                  Update: January, 20. 2023 '
	  WRITE (*,*) '                  Dr. Axel Haenschke, CPS Consulting'
      WRITE (*,*) ' '
      WRITE (*,*) ' '
      WRITE (*,*) ' Stringeingabe vom '
      WRITE (*,*) ' '
      WRITE (*,*) ' Bildschirm       = a '
      WRITE (*,*) ' File             = b '
      WRITE (*,*) ' '
      WRITE (*,*) ' Programmabbruch  = s '
      WRITE (*,*) ' '
      WRITE (*,*) ' '
      WRITE (*,*) ' ---------------------------------------------------'
      WRITE (*,*) ' '
      WRITE (*,*) ' '
      WRITE (*,*) ' '
      WRITE (*, 323)

  323 FORMAT ( /
     +,/,         ' '
     +,/,         '    Hier bitte die Eingabe   : ',$ )
C
      CALL HEREAD ( 'B', IUTEST, STRAB, IERR )
C
C ... Ruecksprung aus der Stringverarbeitung, falls gesamter String
C     abgearbeitet.
C
    1 CONTINUE
C
      IBASE    = 1
      IANFANG  = 1
      IERR     = 0
C
      IF ( STRAB(1:1) .EQ.  'A' .OR. STRAB(1:1) .EQ. 'a' ) THEN
         GOTO  328
      ELSEIF ( STRAB(1:1) .EQ. 'B' .OR. STRAB(1:1) .EQ. 'b' ) THEN
         GOTO 327
      ELSEIF ( STRAB(1:1) .EQ. 'S' .OR. STRAB(1:1) .EQ. 's' ) THEN
         GOTO 9999
      ELSE
         WRITE (*,*) ' '
         WRITE (*,*) ' Falsche Eingabe, bitte Eingabe wiederholen!'
         WRITE (*,*) ' '
         CALL HEENTER
         GOTO 329
      ENDIF
C
  328 CONTINUE
C
      CALL HECLEAR ( IERR )
C
C *** 2.) Lesen eines Strings vom Bildschirm
C     --------------------------------------
C
      WRITE (*, 322)
C
  322 FORMAT ( /
     +,/,         '    Eingabe eines Stringes! '
     +,/,         '    Hier bitte die Eingabe   : ',$ )
C
      CALL HEREAD ( 'B', iutest, string, ierr )
      GOTO 320
  327 CONTINUE
      CALL HECLEAR ( IERR )
C
C *** 3.) Lesen eines Strings vom File, Vor dem ersten Zugriff muss
C         die Datei geoeffnet werden.
C     -------------------------------------------------------------
C
      WRITE (*, 321)
C
  321 FORMAT ( /
     +,/,         '    Testdaten werden von der Datei test.dat '
     +,/,         '    eingelesen ! ',  ///           )
C
      CALL HEENTER
      IF ( IZEILE .GT. 1 ) GOTO 77
      CALL HEOPEN ( 'K', 'N', 'test.dat' , iutest, ierr )
   77 CONTINUE
      CALL HEREAD ( 'F', iutest, string, ierr )
C
C ... End of File  erkannt und Bearbeitung abgebrochen.
C
      IF ( IERR .EQ. 10 ) THEN
         CALL HECLEAR ( IERR )
         WRITE (*,*) ' '
         WRITE (*,*) ' '
         WRITE (*,*) '  Ende der Datei erreicht!'
         WRITE (*,*) ' Weiterlesen nicht moeglich '
         WRITE (*,*) ' Abbruch!'
         WRITE (*,*) ' '
         CALL HEENTER
         GOTO 9999
      ENDIF
C
C ... Einlesen beenden
C
  320 CONTINUE
C
C ======================================================================
CVT*  V E R A R B E I T U N G S T E I L
C ======================================================================
C
C
C *** 4.) Erkennen der Anzahl der Teilstrings im eingelesenen String
C     --------------------------------------------------------------
C
      CALL HECHANZ ( STRING, IANZST, IERR )
C
C ... Ruecksprung nach Abarbeitung des Teilstrings
C
    2 CONTINUE
C
C ... Testen, ob String leer oder besetzt ist und
C     Auswertung von den Stringteilen, wobei die Variable IANFANG
C     hochgezaehlt wird.
C
      IF ( IANZST .EQ. 0 ) THEN
         IFIRST  = 0
         ILAST   = 0
         GOTO 2222
      ENDIF
C
C *** 5.) Herauslesen des aktuellen Teilstrings
C     -----------------------------------------
C
      CALL HECHSE  ( STRING, IANFANG, TSTRING, LNTSTR, IFIRST, ILAST,
     +               IERR )
C
C *** 6.) Typenpruefung des Teilstrings, der aktuell ausgelesen wurde.
C     ----------------------------------------------------------------
C
      CALL HECHTYP ( TSTRING, CHTYP, IERR )
C
C ... weitere Pruefung, ob Dezimalpunkt im Teilstring oder nicht
C
      IOUT = IOUT + 1
      CALL HECHDOT ( TSTRING, CHDOT, IERR )
C
C ... Ruecksetzen der Ausgabewerte
C
      IWERT  = 0
      RWERT  = 0.0
      DBWERT = 0.0
      CHWERT = ' '
C
C *** 7.) Transformation in erkannten Variablentyp
C     --------------------------------------------
C
      IF ( CHTYP .EQ. 'I' ) THEN
C
C ... INTEGER
C
         CALL HECHSE ( STRING, IANFANG, TSTRING, LNTSTR, IFIRST, ILAST,
     +                 IERR )
         CALL HETYPINT ( TSTRING, IWERT, IERR )

C
      ELSE IF ( CHTYP .EQ. 'F' ) THEN
C
C ... REAL, F-Format Standard
C
         CALL HECHSE ( STRING, IANFANG, TSTRING, LNTSTR, IFIRST, ILAST,
     +                 IERR )
         CALL HETYPREA ( TSTRING, RWERT, IERR )
C
      ELSE IF ( CHTYP .EQ. 'E' ) THEN
C
C ... REAL, E-Format Standard, wobei noch geprueft wird, ob im Teilstring
C     ein Dezimalpunkt enthalten ist. Falls nicht, dann in Character
C     gelassen. (Diese Einstellung kann auch geaendert werden).
C
         CALL HECHSE ( STRING, IANFANG, TSTRING, LNTSTR, IFIRST, ILAST,
     +                 IERR )
         IF ( CHDOT .EQ. 'N' ) THEN
            CALL HETYPCHA ( TSTRING, CHWERT, LCH, IERR )
         ELSE IF (CHDOT .EQ. 'J' ) THEN
            CALL HETYPRIA ( TSTRING, RWERT, IERR )
         ENDIF
C
      ELSE IF ( CHTYP .EQ. 'C' ) THEN
C
C ... CHARACTER, Standard
C
         CALL HECHSE ( STRING, IANFANG, TSTRING, LNTSTR, IFIRST, ILAST,
     +                 IERR )
         CALL HETYPCHA ( TSTRING, CHWERT, LCH, IERR )
C
      ELSE IF ( CHTYP .EQ. 'A' ) THEN
C
C ... CHARACTER, Alphanumerischer Ausdruck
C
         CALL HECHSE ( STRING, IANFANG, TSTRING, LNTSTR, IFIRST, ILAST,
     +                 IERR )
         CALL HETYPCHA ( TSTRING, CHWERT, LCH, IERR )
C
      ELSE IF ( CHTYP .EQ. 'X' ) THEN
C
C ... REAL, e-Format, mit kleinem e
C
         CALL HECHSE ( STRING, IANFANG, TSTRING, LNTSTR, IFIRST, ILAST,
     +                 IERR )
         CALL HETYPREX ( TSTRING, RWERT, IERR )
C
      ELSE IF ( CHTYP .EQ. 'Y' ) THEN
C
C ... DOUBLE Precision Standard
C
         CALL HECHSE ( STRING, IANFANG, TSTRING, LNTSTR, IFIRST, ILAST,
     +                 IERR )
         CALL HETYPDB ( TSTRING, DBWERT, IERR )
C
      ELSE IF ( CHTYP .EQ. 'Z' ) THEN
C
C ... DOUBLE Precision, mit kleinem d
C
         CALL HECHSE ( STRING, IANFANG, TSTRING, LNTSTR, IFIRST, ILAST,
     +                 IERR )
         CALL HETYPDB ( TSTRING, DBWERT, IERR )
      ENDIF
C
C *** 8.) Anzeigen der Ergebnisse durch Ausgabe auf Bildschirm
C     --------------------------------------------------------
C
      CALL HECLEAR ( IERR )
      WRITE (*,*) ' '
      WRITE (*,*) '    Typenpruefung im Hauptprogramm CHATEST:  '
      WRITE (*,*) '    =======================================  '
      WRITE (*,*) ' '
      WRITE (*,*) ' '
      WRITE (*,*) '    Interpretation der Teilstrings:  '
      WRITE (*,*) '    ---------------------------------'
      WRITE (*,*) ' '
C
      WRITE (*,*) ' '
      WRITE (*,*) '    Der String enthaelt ', ianzst , ' Teilstrings!'
      WRITE (*,*) ' '
C
      IF ( CHDOT(1:1) .EQ. 'J' ) THEN
         WRITE (*,*) ' '
         WRITE (*,*) ' Der Teilstring ', IOUT,
     +   ' enthaelt einen Punkt!  '
         WRITE (*,*) ' '
      ELSE IF (CHDOT(1:1) .EQ. 'N' ) THEN
         WRITE (*,*) ' '
         WRITE (*,*) ' Der Teilstring ', IOUT,
     +   ' enthaelt keinen Punkt!  '
         WRITE (*,*) ' '
      ENDIF
C
      IF ( CHTYP .EQ. 'I' ) WRITE (*,*) ' INTEGER-Groesse erkannt: '
      IF ( CHTYP .EQ. 'F' ) WRITE (*,*) ' REAL-Groesse erkannt: '
      IF ( CHTYP .EQ. 'E' ) WRITE (*,*) ' REAL-Groesse im
     +E-Format erkannt: '
      IF ( CHTYP .EQ. 'C' ) WRITE (*,*) ' CHARACTER-Groesse
     +erkannt: '
      IF ( CHTYP .EQ. 'A' ) WRITE (*,*) ' Alphanumerischer
     +String erkannt: '
      IF ( CHTYP .EQ. 'X' ) WRITE (*,*) ' REAL-Groesse im E-Format
     +aber mit kleinem e: '
      IF ( CHTYP .EQ. 'Y' ) WRITE (*,*) ' DOUBLE PRECISION-Groesse
     +erkannt: '
      IF ( CHTYP .EQ. 'Z' ) WRITE (*,*) ' DOUBLE PRECISION-Groesse
     +aber mit kleinem d:'
C
      WRITE (*,*)    ' '
	  WRITE (*,*)    ' CHTYP   = ', CHTYP
	  WRITE (*,*)    ' CHTYP   = ', CHTYP
C
      WRITE (*,*)    ' IERR    = ', IERR
      WRITE (*,*)    ' IBASE   = ', IBASE
      WRITE (*,*)    ' IRESULT = ', IRESULT
      WRITE (*,*)    ' IWERT   = ', IWERT
      WRITE (*,*)    ' RWERT   = ', RWERT
      WRITE (*,*)    ' DBWERT  = ', DBWERT
C     WRITE (*,888)    DBWERT
C888  FORMAT ( '  DBWERT  = ', D24.12 )
      WRITE (*,*)    ' IFIRST  = ', IFIRST
      WRITE (*,*)    ' ILAST   = ', ILAST
      WRITE (*,*)    ' LNTSTR  = ', LNTSTR
      WRITE (*,*)    ' CHWERT  = ', CHWERT(1:LCH)
      WRITE (*,*)    ' STRING  = ', STRING
C
      IF ( ILAST .GE. IFIRST ) THEN
         WRITE (*,*) ' TSTRING = ', STRING(IFIRST:ILAST)
      ELSE
         WRITE (*,*) ' STRING ist fertig interpretiert '
      ENDIF
C
      WRITE (*,*) ' -----------------------------------'
      WRITE (*,*) ' '
      WRITE (*,*) ' '
      CALL HEENTER
C
C ... Hochzaehlen des Teilstring-Zeigers IANFANG
C
      IANFANG = IANFANG + 1
C
C ... Ruecksprung, da noch Teilstrings vorhanden
C
      IF ( IOUT .LT. IANZST ) THEN
         GOTO 2
      ENDIF
C
C ======================================================================
CAT*  A U S G A B E T E I L
C ======================================================================
C
C *** FEHLERMELDUNGEN: (auch Abfangen der String-Enden)
C
 2222 CONTINUE
      CALL HECLEAR ( IERR )
      WRITE (*,*) ' '
      WRITE (*,*) ' Leerer String oder kein Wort mehr gefunden! '
      WRITE (*,*) ' Ende der Interpretation. '
      WRITE (*,*) ' '
      WRITE (*,*) ' ---------------------------------------'
C
      IF ( ILAST .EQ. 0 ) THEN
         WRITE (*,*) ' STRING  = ', STRING
      ELSE
         WRITE (*,*) ' STRING ist fertig interpretiert '
      ENDIF
C
      WRITE (*,*) ' ---------------------------------------'
      WRITE (*,*) ' '
      WRITE (*,*) ' '
      CALL HEENTER
C
  123 CONTINUE
      CALL HECLEAR ( IERR )
C
      WRITE (*, 372)
C
  372 FORMAT ( /
     +,/,         '    Weiterer String zu interpretieren?  '
     +,/,         '    Bitte J/N eingeben: ', $ )
      CALL HEREAD ( 'B', iutest, JN, ierr )
      IF ( JN(1:1) .EQ. 'J' .OR. JN(1:1) .EQ. 'j' ) THEN
         IOUT = 0
         IZEILE = IZEILE + 1
         GOTO 1
      ELSEIF ( JN(1:1) .EQ. 'N' .OR. JN(1:1) .EQ. 'n' ) THEN
         GOTO 9999
      ELSE
         WRITE (*,*) ' '
         WRITE (*,*) ' Falsche Eingabe, bitte Eingabe wiederholen!'
         WRITE (*,*) ' '
         CALL HEENTER
         GOTO 123
      ENDIF
C
C ======================================================================
CFA*  F O R M A T  - A N W E I S U N G E N
C ======================================================================
C
 1001 FORMAT ( A1 )
 2000 FORMAT ( ' ', //
     +,/,'   ***********************************************************
     +**'
     +,/,'   *
     + *'
     +,/,'   *      Progranm Stop Neustart
     + *'
     +,/,'   *
     + *'
     +,/,'   *
     + *'
     +,/,'   ***********************************************************
     +**', ///
     + )

C
C ======================================================================
C
 9998 CONTINUE
 9999 CONTINUE
C
      CALL HEINOU ( 'TIRE Hauptprogramm', 'AUS', IERR )
C	  
      CALL HEENTER
C
C *** Ende von Chatest
C
      STOP
C
      END
