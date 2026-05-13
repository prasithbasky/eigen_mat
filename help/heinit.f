C*****7**1*********2*********3*********4*********5*********6*********7**
      SUBROUTINE HEINIT ( IERR )
C
C
CA*   Unterprogramm zur Initialisierung der Hilfsumgebung
C     zur Programmentwicklung
C
CB*   Name = HEINIT Hilfsroutine In / Out Routinensteuerung INIT
C            ------
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
C                  IERR =  1, ALLES I.O. Umgebung angelegt
C                  IERR = 99, Programmeinheit laesst sich nicht
C                             initialisieren,
C                             d.h. der Steuerparameter fuer die
C                             Ausgabe ist 0, sonst nach der Initiali-
C                             sierung ist der Parameter entweder
C                             100 = Ausgabe auf Bildschirm
C                             999 = keine Ausgabe auf Bildschirm
C
C
CH*   INTERNE:
C     -------
C
C     - KTW        Terminal Ausgabe Kanal = 6, wird hier gesetzt
C     - IAUS       Steuerparameter fuer die Ausgabe,
C                  aus COMMON STEUER
C
C                  IAUS =   0, d.h. Programmeinheit ist nicht
C                              initialisiert.
C                  IAUS = 100, Ausgabe der Kennungen auf Bildschirm
C                  IAUS = 999, Keine Ausgabe auf Bildschirm.
C                  Wird aus Datei HEINIT.DAT eingelesen. Der Aufbau
C                  der Datei ist z.Zt. folgender.
C                  - wobei alle Zeilen mit C als Kommentar ueberlesen
C                    werden -
C
C      - IUNIT     73, IST Z.ZT. NOCH FEST VERDRAHTET11111
C
C
C
C
C ---------------------------------------------------------------------
C
C     C ***************************************************************
C     C                                                               *
C     C     Datei HEINIT.DAT                                          *
C     C     ================                                          *
C     C                                                               *
C     C     Datei fuer die Belegung des COMMON-Bereiches STEUER       *
C     C     fuer die Unterprogrammsammlung HE.... zur Unterstuetzung  *
C     C     der FORTRAN-Programmentwicklung.                          *
C     C                                                               *
C     C     Hinweis:                                                  *
C     C     Die Datei wird durch die Initialisierungsroutine          *
C     C     im aktuellen Verzeichnis, in dem das Programm aufgerufen  *
C     C     wird, erstellt. Falls die Datei schon existiert, wird sie *
C     C     ausgelesen und dem COMMON /STEUER/ die Werte zugewiesen.  *
C     C                                                               *
C     C     Weiterhin existiert der COMMON /CHAR/ mit den Werten      *
C     C     zur Steuerung der Routinen ENTER und CLEAR sowie dem      *
C     C     COMMON /USER/ der mit User-defined Werten zur Ablauf-     *
C     C     steuerung versehen werden kann.                           *
C     C                                                               *
C     C                                                               *
C     C     Die Datei enthaelt z.Zt. folgende Groessen, die bei       *
C     C     Erweiterung auch im COMMON /STEUER/ eingefuegt werden     *
C     C     muessen.                                                  *
C     C                                                               *
C     C     KTW     ist der Terminal Schreibkanal, default =  6       *
C     C     KTR     ist der Terminal Lesekanal,    default =  5       *
C     C     IAUS    ist die Ausgabekennung,        default =  1       *
C     C             d.h. Ausgabe auf Bildschirm,                      *
C     C             Weitere Moeglichkeit ist               =  0       *
C     C             abschalten der Ausgabe auf dem Bildschirm         *
C     C             Alle anderen Werte weden nicht unterstuetzt und   *
C     C             fuehren zu fehlern.                               *
C     C                                                               *
C     C                                                               *
C     C     INTEGER    INTEGER   INTEGER                              *
C     C     KTW        KTR       IAUS                                 *
C     C      6           5        1                                   *
C     C                                                               *
C     C      Werte fuer den COMMON /CHAR/                             *
C     C                                                               *
C     C     CHARACTER      CHARACTER        CHARACTER    CHARACTER    *
C     C      CHCLEAR        CHENTER          CHSYS         CHEDIT     *
C     C        J               J               D             V        *
C     C                                                               *
C     C      Werte fuer den COMMON /USER/                             *
C     C      (ist frei zu definieren)                                 *
C     C                                                               *
C     C ***************************************************************
C     C
C     C
C     C     KTW        KTR        IAUS
C     C
C             6          5         100
C     C
C     C
C     C
C     C
C           CHCLEAR   CHENTER      CHSYS        CHEDIT
C     C
C             J         J           D             V
C     C
C     C
C     C
C     C     frei waehlbar fuer den COMMON /USER/
C     C
C     C
C     C
C     C
C     C     Ende der Datei HEINIT.DAT
C     C
C     C ***************************************************************
C     C
C
C
C ---------------------------------------------------------------------
C
C
CI*   COMMON:
C     ------
C
C     - STEUER     STEUERKENNUNGEN FUER DIE VERARBEITUNG
C
C
CJ*   BENOETIGTE UNTERPROGRAMME / FUNKTIONEN:
C     --------------------------------------
C
C     - SUBROUTINE HEINQU
C     - SUBROUTINE HEOPEN
C     - SUBROUTINE HECLOSE
C     - SUBROUTINE HEWRITE
C     - SUBROUTINE HEREAD
C     - SUBROUTINE HEFIMK
C     - SUBROUTINE HECORR
C
C
CL*   DIESE ROUTINE WIRD AUFGERUFEN VON:
C     ---------------------------------
C
C     - SUBROUTINE allen Unterprogrammen (Standard).
C
C
CM*   ACHTUNG:     keine
C     -------
C
C
CN*   BEMERKUNG:   das Unterprogramm dient zur Unterstuetzung der
C     ---------    FORTRAN-Programmentwicklung.
C
C                  Die Datei HEINIT.DAT kann zum Anpassen der
C                  Ausgaben editiert werden. Dabei sollte der Terminal
C                  Ausgabekanal moeglichst nicht veraender werden.
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
      CHARACTER    CHEXIST*1
      CHARACTER    CHCLEAR1*1
      CHARACTER    CHENTER1*1
      CHARACTER    CHSYS1*1
      CHARACTER    CHEDIT1*1
C
C ----------------------------------------------------------------------
C
      INCLUDE 'steuer.inc'
C
C ======================================================================
C
C
C ======================================================================
CIT*  I N I T I A L I S I E R U N G S T E I L
C ======================================================================
C
      IERR      =   0
      KTW       =   6
      KTR       =   5
      IAUS      =   1
      IUNIT     =  73
      CHEXIST   = ' '
C
      CHCLEAR   = 'J'
      CHENTER   = 'J'
      CHSYS     = 'D'
      CHEDIT    = 'V'
C      
C      CHCLEAR   = 'J'
C      CHENTER   = 'J'
C      CHSYS     = 'D'
C      CHEDIT    = 'V'
C
      CHCLEAR1  = ' '
      CHENTER1  = ' '
      CHSYS1    = ' '
      CHEDIT1   = ' '
C
C ======================================================================
CEV*  E I N G A B E V E R A R B E I T U N G
C ======================================================================
C
C
C *** Pruefen, ob die Datei HEINIT.DAT schon auf dem aktuellen
C     Verzeichnis existiert. CHEXIST kann entweder 'J' oder 'N' sein.
C
      CALL HEINQU ( CHEXIST )
C
C ======================================================================
CVT*  V E R A R B E I T U N G S T E I L
C ======================================================================
C
C *** Falls die Datei existiert, dann koenne die aktuellen Werte
C     mit dem Unterprogramm HEREAD ausgelesen und dem
C     COMMON /STEUER/, dem COMMON /CHAR/ und dem COMMON /USER/ zuge-
C     wiesen werden.
C
C
      IF ( CHEXIST .EQ. 'J' ) THEN
C
         CALL HEOPEN ( 'K', 'N', 'HEINIT.DAT', IUNIT, IERR )
C
         CALL HEHREAD ( IUNIT, KTW, KTR, IAUS, CHCLEAR1, CHENTER1,
     +                  CHSYS1, CHEDIT1, IERR )
C
C
C *** Definiertes Umbesetzen der Character-Groessen
C
         CHCLEAR = CHCLEAR1
         CHENTER = CHENTER1
         CHSYS   = CHSYS1
         CHEDIT  = CHEDIT1
C
         CALL HECLOSE ( IUNIT, IERR )
      ELSE IF ( CHEXIST .EQ. 'N' ) THEN
         CALL HEFIMK ( IERR )
         CALL HECORR ( IERR )
         IERR  = 1
      ELSE
         IAUS = 1
         IERR = 99
         KTR  = 5
         KTW  = 6
         GOTO 9999
      ENDIF
C
C *** hier muss der Fehler noch abgefangen werden
C
CCCC
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
C
C ======================================================================
C
 9999 CONTINUE
      RETURN
      END
