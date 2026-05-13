      SUBROUTINE HECHS2 ( STRING, ITEIL, TSTRING, IANF, IEND, IERR )
C
C
CA*   Unterprogramm zum Herauslesen eines Teilstringes aus einem String.
C     Teilstrings sind hierbei durch zwei Leerzeichen getrennt.
C     Eingabestring vorher auf Anzahl der Teilstrings abgepruft.
C
C
CB*   Name = HECHS2  Hilfsroutine zum erkennen der Anzahl der Teil-
C            ------  strings.
C
C
CD*   PROGRAMM-SYSTEM: HELP, Routinensammlung zur Programmentwicklung
C     ---------------
C
CE*   FUNKTION: Unterprogramm zum Selektieren eines Teilstrings
C     --------
C
CS*   PROGRAMMIER-SPRACHE: FORTRAN 77
C     --------------------
C
C
CF*   EINGABE:
C     -------
C
C     - STRING     zu untersuchender Textstring
C     - ITEIL      herauszulesender Teilstring
C
C
CG*   AUSGABE:
C     -------
C
C     - TSTRING    herausgelesener Teilstring
C     - LNTSTR     Laenge des herausgelesenen Teilstrings
C     - IFIRST     Stellung des Teilstrings im Gesamtstring
C                  Anfangsadresse
C     - ILAST      Stellung des Teilstrings im Gesamtstring
C                  Endadresse.
C     - IERR       Fehler return Code
C                  IERR  =  0,   alles i.O.
C                  IERR  =  700, keine Werte im String enthalten
C                  IERR  =  701, ITEIL-Teilstring ist nicht vorhanden
C
C
CH*   INTERNE:
C     -------
C
C     - STRING     Eingelesener RECORD
C                  Der String wird nicht interpretiert.
C     - IBASE      Kennung, ab der der String interpretiert werden soll.
C     - IRES       Pointer zur erkannten Position im Auswahl-Opcode.
C     - IWERT      Integer Wert der Interpretation
C     - RWERT      Real Wert der Interpretation
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
C     - SUBROUTINE CHLAEN
C     - SUBROUTINE HEINOU
C
C
CL*   DIESE ROUTINE WIRD AUFGERUFEN VON:
C     ---------------------------------
C
C     - SUBROUTINE vielen
C
C
CM*   ACHTUNG:     Aus dem uebergebenen String wird der ITEIL
C     -------      Teilstrings herausgelesen und mit seiner Laenge
C                  an das rufende Programm zurueckgegeben.
C
C
CN*   BEMERKUNG:   keine
C     ---------
C
C
C
CO*   AUTOR:       Thomas Deter    
C     -----        Firma
C                  Anschrift der Firma
C                  Abteilung:
C
C
CP*   VERSION:     0.1                                     18-SEP-94
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
      INTEGER      IANF
      INTEGER      IEND 
      INTEGER      I 
      INTEGER      J 
      INTEGER      ITEIL
      INTEGER      IERR
      INTEGER      LNTSTR
      INTEGER      DANF(50)
      INTEGER      DEND(50)
C
C ----------------------------------------------------------------------
C
      CHARACTER    STRING*(*)
      CHARACTER    DUMMY*120
      CHARACTER    TSTRING*(*)
C
C ======================================================================
CIT*  I N I T I A L I S I E R U N G S T E I L
C ======================================================================
C
      IERR    = 0
      IANF    = 0
      IEND    = 0
      LNTSTR  = 0
      TSTRING = ' '
      DO 50 I = 1, 50
        DEND (I) = 0
        DANF (I) = 0
  50  CONTINUE
C
C ======================================================================
CEV*  E I N G A B E V E R A R B E I T U N G
C ======================================================================
C
C ======================================================================
CVT*  V E R A R B E I T U N G S T E I L
C ======================================================================
C
C
C *** BESTIMME DIE LAENGE DES STRING
      CALL CHLAEN ( STRING, LNTSTR)
      IF (LNTSTR.EQ.0) GOTO 991
C
C
C *** KOPIERE DEN STRING NACH DUMMY
C
      DO 60  I = 1, LNTSTR
        WRITE (DUMMY(I:I), '(A1)') STRING(I:I)
  60  CONTINUE
C
C *** FAENGT DER STRING MIT LEERZEICHEN AN?
C
  65  IF (DUMMY(1:1).EQ.' ') THEN
        DO 70 I = 1, LNTSTR
          WRITE (DUMMY(I:I), '(A1)')  DUMMY(I+1:I+1)
  70    CONTINUE
        LNTSTR = LNTSTR - 1
        GOTO 65
      ENDIF
C
C *** SUCHE NACH DEM ITEIL-1 TEN DOPPEL-LEERZEICHEN
C     J IST INDEX DER DOPPEL-LEERZEICHEN
C     I IST DIE STELLE DES ZEICHENS
C
      J = 0
      I = 0
  100 I = I + 1
      IF (DUMMY(I:I+1).EQ.'  ') THEN
         J = J + 1
         DANF(J) = I
         DEND(J) = I + 1
C
C ***    IST DAS NAECHSTE ZEICHEN AUCH NOCH EIN LEERZEICHEN?
  110    IF (DUMMY(I+2:I+2).EQ.' ') THEN
           I = I + 1
           DEND(J) = DEND(J) + 1
           GOTO 110
         ENDIF
      ENDIF
      IF (I.GE.LNTSTR-1) GOTO 200
      GOTO 100
C
C *** NACHDEM DIE ANZAHL UND POSITION DER DOPPELS ERMITTELT SIND,
C     KANN DER TEILSTRING GELESEN WERDEN.
C
  200 IF (ITEIL.GT.J+1) THEN
        GOTO 992
      ENDIF
      IF (ITEIL.NE.1) THEN
        IANF = DEND(ITEIL-1) + 1
      ELSE
        IANF = 1
      ENDIF
      IF (ITEIL.EQ.J+1) THEN
        IEND = LNTSTR
      ELSE
        IEND = DANF(ITEIL)-1          
      ENDIF
      TSTRING = DUMMY (IANF:IEND)
C
C ======================================================================
CAT*  A U S G A B E T E I L
C ======================================================================
C
C ======================================================================
CFA*  F O R M A T  - A N W E I S U N G E N
C ======================================================================
C
C
  991 IERR = 1
      RETURN
  992 IERR = 99
      RETURN

 9999 CONTINUE
C
      RETURN
      END

