C*****7**1*********2*********3*********4*********5*********6*********7**
      SUBROUTINE HEFIMK ( IERR )
C
C
CA*   Unterprogramm zum Erstellen der Datei HEINIT.DAT der Hilfsumgebung
C     zur Programmentwicklung
C
CB*   Name = HEFIMK  Hilfsroutine In / Out Routinensteuerung INIT
C            ------  File Make
C
C
CD*   PROGRAMM-SYSTEM: HELP, Routinensammlung zur Programmentwicklung
C     ---------------
C
CE*   FUNKTION: Unterprogramm zur Initialisierung
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
C     - IERR       FEHLERKENNUNG,
C                  IERR =  0, ALLES I.O.
C                  IERR = 99, Datei kann nicht angelegt werden.
C
C
C
CH*   INTERNE:
C     -------
C
C     - IUNIT      Terminal Ausgabe Kanal = 6, wird hier gesetzt
C
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
C     - SUBROUTINE HEOPEN
C
C
CL*   DIESE ROUTINE WIRD AUFGERUFEN VON:
C     ---------------------------------
C
C     - SUBROUTINE HEINIT
C
C
CM*   ACHTUNG:     keine
C     -------
C
C
CN*   BEMERKUNG:   das Unterprogramm dient zur Unterstuetzung der
C     ---------    FORTRAN-Programmentwicklung.
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
      INTEGER      IERR
      INTEGER      IUNIT
C
C ----------------------------------------------------------------------
C
      CHARACTER    FNAME*10
C
C ======================================================================
C
C
C ======================================================================
CIT*  I N I T I A L I S I E R U N G S T E I L
C ======================================================================
C
      IERR     =   0
      FNAME    =  'HEINIT.DAT'
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
C *** Eroeffnen der Datei
C
      CALL HEOPEN ( 'K', 'J', FNAME, IUNIT, IERR )
C
      IF ( IERR .NE. 0 ) GOTO 9999
C
C ======================================================================
CAT*  A U S G A B E T E I L
C ======================================================================
C
      WRITE ( IUNIT, 5000 )
C
      CALL HECLOSE ( IUNIT, IERR )
C
C *** Melden, dass Help_umgebung initialisiert wurde.
C
      WRITE (*, 6000)
C
      CALL HEENTER
C
C *** FEHLERMELDUNGEN:
C
C
C ======================================================================
CFA*  F O R M A T  - A N W E I S U N G E N
C ======================================================================
C
 5000 FORMAT ( 'C', /
     + , 'C'
     +,/,'C ************************************************************
     +***'
     +,/,'C
     +  *'
     +,/,'C     Datei HEINIT.DAT
     +  *'
     +,/,'C     ================
     +  *'
     +,/,'C
     +  *'
     +,/,'C     Datei fuer die Belegung des COMMON-Bereiches STEUER
     +  *'
     +,/,'C     fuer die Unterprogrammsammlung HE.... zur Unterstuetzung
     +  *'
     +,/,'C     der FORTRAN-Programmentwicklung.
     +  *'
     +,/,'C
     +  *'
     +,/,'C     Hinweis:
     +  *'
     +,/,'C     Die Datei wird durch die Initialisierungsroutine
     +  *'
     +,/,'C     im aktuellen Verzeichnis, in dem das Programm aufgerufen
     +  *'
     +,/,'C     wird, erstellt. Falls die Datei schon existiert, wird si
     +e *'
     +,/,'C     ausgelesen und dem COMMON /STEUER/ die Werte zugewiesen.
     +  *'
     +,/,'C
     +  *'
     +,/,'C     Weiterhin existiert der COMMON /CHAR/ mit den Werten
     +  *'
     +,/,'C     zur Steuerung der Routinen ENTER und CLEAR sowie dem
     +  *'
     +,/,'C     COMMON /USER/ der mit User-defined Werten zur Ablauf-
     +  *'
     +,/,'C     steuerung versehen werden kann.
     +  *'
     +,/,'C
     +  *'
     +,/,'C
     +  *'
     +,/,'C     Die Datei enthaelt z.Zt. folgende Groessen, die bei
     +  *'
     +,/,'C     Erweiterung auch im COMMON /STEUER/ eingefuegt werden
     +  *'
     +,/,'C     muessen.
     +  *'
     +,/,'C
     +  *'
     +,/,'C     KTW     ist der Terminal Schreibkanal, default =  6
     +  *'
     +,/,'C     KTR     ist der Terminal Lesekanal,    default =  5
     +  *'
     +,/,'C     IAUS    ist die Ausgabekennung,        default =  1
     +  *'
     +,/,'C             d.h. Ausgabe auf Bildschirm,
     +  *'
     +,/,'C             Weitere Moeglichkeit ist               =  0
     +  *'
     +,/,'C             abschalten der Ausgabe auf dem Bildschirm
     +  *'
     +,/,'C             Alle anderen Werte weden nicht unterstuetzt und
     +  *'
     +,/,'C             fuehren zu fehlern.
     +  *'
     +,/,'C
     +  *'
     +,/,'C     Werte fuer den COMMON /STEUER/
     +  *'
     +,/,'C
     +  *'
     +,/,'C
     +  *'
     +,/,'C     INTEGER    INTEGER   INTEGER
     +  *'
     +,/,'C     KTW        KTR       IAUS
     +  *'
     +,/,'C      6           5        1
     +  *'
     +,/,'C
     +  *'
     +,/,'C      Werte fuer den COMMON /CHAR/
     +  *'
     +,/,'C
     +  *'
     +,/,'C     CHARACTER      CHARACTER      CHARACTER    CHARACTER
     +  *'
     +,/,'C      CHCLEAR        CHENTER         CHSYS       CHEDIT
     +  *'
     +,/,'C        J               J              D          E
     +  *'
     +,/,'C
     +  *'
     +,/,'C      Werte fuer den COMMON /USER/
     +  *'
     +,/,'C      (ist frei zu definieren)
     +  *'
     +,/,'C
     +  *'
     +,/,'C ************************************************************
     +***'
     +,/,'C '
     +,/,'C '
     +,/,'C     KTW        KTR        IAUS '
     +,/,'C '
     +,/,'        6          5          1  '
     +,/,'C '
     +,/,'C '
     +,/,'C '
     +,/,'C     CHCLEAR    CHENTER      CHSYS       CHEDIT '
     +,/,'C '
     +,/,'        J          J            D           E '
     +,/,'C '
     +,/,'C '
     +,/,'C '
     +,/,'C     frei waehlbare Werte fuer den COMMON /USER/ '
     +,/,'C '
     +,/,'C '
     +,/,'C '
     +,/,'C '
     +,/,'C     Ende der Datei HEINIT.DAT  '
     +,/,'C '
     +,/,'C ************************************************************
     +***'
     +,/,'C '
     + )
C
C
C ======================================================================
C
 6000 FORMAT ( ' ', //
     +,/,'   ***********************************************************
     +**'
     +,/,'   *
     + *'
     +,/,'   *
     + *'
     +,/,'   *             F O R T R A N - Help Umgebung
     + *'
     +,/,'   *             =============================
     + *'
     +,/,'   *
     + *'
     +,/,'   *
     + *'
     +,/,'   *  Die FORTRAN Help Umgebung ist erfolgreich initialisiert
     + *'
     +,/,'   *  worden. Die Initialisierungsdatei HEINIT.DAT ist auf
     + *'
     +,/,'   *  dem aktuellen Verzeichnis angelegt worden. Es gelten
     + *'
     +,/,'   *  die Grundeinstellungen, die im Anschluss an diese Mel-
     + *'
     +,/,'   *  dung durch editieren geaendert werden kann.
     + *'
     +,/,'   *  (Nach Aenderung das Programm neu starten!)
     + *'
     +,/,'   *
     + *'
     +,/,'   *
     + *'
     +,/,'   *                            Axel Haenschke
     + *'
     +,/,'   *                            (08.03.2023)
     + *'
     +,/,'   *
     + *'
     +,/,'   *
     + *'
     +,/,'   *
     + *'
     +,/,'   ***********************************************************
     +**'
     +,/,' '
     +,/,' '
     + )
C
C ======================================================================
C
 9999 CONTINUE
C
      RETURN
      END
