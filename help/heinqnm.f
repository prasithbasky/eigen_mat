C*****7**1*********2*********3*********4*********5*********6*********7**
      SUBROUTINE HEINQNM ( CHNAME, CHEXIST )
C
C
CA*   Unterprogramm zum feststellen, ob die Datei mit uebergebenem Namen
C     bereits existiert.  
C
CB*   Name = HEINQU Hilfsroutine Inquire Name der Datei 
C            ------
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
C     - CHNAME     Name der zu ueberpruefenden Datei
C
C
CG*   AUSGABE:
C     -------
C
C     - CHEXIST    Character Kennung, ob Datei existiert oder nicht.
C                  CHEXIST = 'J', Datei existiert, Werte koennen ausge-
C                                 lesen werden
C                  CHEXIST = 'N', Datei existiert nicht, 
C                                 Datei muss angelegt und beschrieben 
C                                 werden. Erst dann ist das Hilfs-
C                                 system aktiv.
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
C     - SUBROUTINE CHLAEN
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
CN*   BEMERKUNG:   Das Unterprogramm prueft, ob die Datei HEINIT.DAT 
C     ---------    vorhanden ist oder nicht. Falls die Datei nicht 
C                  existiert, dann wird diese waehrend des Initiali-
C                  sierungsvorganges erzeugt.
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
C
C ----------------------------------------------------------------------
C
      INTEGER      LN
C
C ----------------------------------------------------------------------
C
      CHARACTER    CHEXIST*1
      CHARACTER    CHNAME*(*)
C
C ======================================================================
C
C
C ======================================================================
CIT*  I N I T I A L I S I E R U N G S T E I L
C ======================================================================
C
      EX = .FALSE.
      CHEXIST = ' '
C
C ======================================================================
CEV*  E I N G A B E V E R A R B E I T U N G
C ======================================================================
C
      CALL CHLAEN ( CHNAME, LN )
C
C ======================================================================
CVT*  V E R A R B E I T U N G S T E I L
C ======================================================================
C
C *** Ueberpruefen, ob Datei HEINIT.DAT auf dem Verzeichnis existiert.
C 
      INQUIRE ( FILE=CHNAME(1:LN), EXIST=EX )
C
      IF ( EX ) THEN
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
