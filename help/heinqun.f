C*****7**1*********2*********3*********4*********5*********6*********7**
      SUBROUTINE HEINQUN ( IUNIT, CHEXIST )
C
C
CA*   Unterprogramm zum feststellen, ob die Datei mit der uebergebenen
C     UNIT bereits existiert und geoeffnet ist.
C
C
CB*   Name = HEINQUN  Hilfsroutine Inquire Datei UNIT
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
C     - IUNIT      Kanalnummer der zu untersuchenden Datei
C
C
CG*   AUSGABE:
C     -------
C
C     - CHEXIST    Character Kennung, ob Datei existiert oder nicht.
C                  CHEXIST = 'J', Datei existiert, Werte koennen ausge-
C                                 lesen werden, d.h. ist auch geoeffnet.
C                  CHEXIST = 'N', Datei existiert nichtund oder ist
C                                 nicht geoeffnet.
C
C
CH*   INTERNE:
C     -------
C
C     - EX         Logical-Variable, ob Datei existiert oder
C                  nicht.
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
C     - keine
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
CN*   BEMERKUNG:   Das Unterprogramm prueft, ob die Datei IUNIT
C     ---------    vorhanden und geoeffnet ist.
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
      LOGICAL      EX
      LOGICAL      LOPEN
C
C ----------------------------------------------------------------------
C
      INTEGER      IUNIT
C
C ----------------------------------------------------------------------
C
      CHARACTER    CHEXIST*1
C
C ======================================================================
C
C
C ======================================================================
CIT*  I N I T I A L I S I E R U N G S T E I L
C ======================================================================
C
      EX = .FALSE.
      LOPEN = .FALSE.
      CHEXIST = ' '
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
C *** Ueberpruefen, ob Datei IUNIT auf dem Verzeichnis existiert und
C     geoeffnet ist.
C
      INQUIRE ( UNIT=IUNIT, EXIST=EX, OPENED=LOPEN )
C
      IF ( LOPEN ) THEN
         CHEXIST = 'J'
      ELSE
         CHEXIST = 'N'
      ENDIF
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
C
      RETURN
      END
