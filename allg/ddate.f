C*****7**1*********2*********3*********4*********5*********6*********7**
      SUBROUTINE DDATE ( OPCODE, CHDATE, CHTIME, IERR )
C
C
CA*   UNTERPROGRAMM ZUR ABFRAGE DER SYSTEMZEIT UND DES SYSTEMDATUMS
C     VON UNTERSCHIEDLICHEN BETRIEBSSYSTEMEN.
C
CB*   DDATE = DMKS DATE
C             -    ---
C
C
CD*   PROGRAMM-SYSTEM: DMKS  
C     ---------------
C
CE*   FUNKTION: HILFSPROGRAMMS
C     --------
C
CS*   PROGRAMMIER-SPRACHE: FORTRAN 77
C     --------------------
C
C
CF*   EINGABE:
C     -------
C
C     - OPCODE     OPERATION CODE FUER DIE  BEARBEITUNG DER DATEN
C                  UNTER DEN VERSCHIEDENEN BETRIEBSSYSTEMEN.
C                  OPCODE = 'VAX', VMS-BETRIEBSSYSTEM
C                  OPCODE = 'UNI', UNIX-BETRIEBSSYSTEM
C                  OPCODE = 'IBM', MVS-BETRIEBSSYSTEM
C                  OPCODE = 'CDC', NOS-VE-BETRIEBSSYSTEM
C
C
CG*   AUSGABE:
C     -------
C
C     - CHDATE     DATUM ALS STRING DES FORMATES
C                  DD-MMM-YYYY, (LAENGE 11)
C     - CHTIME     UHRZEIT ALS STRING DES FORMATES
C                  HH:MM:SS.SS, (LAENGE 11) 
C     - IERR       FEHLER-CODE, 
C                  IERR = 0,  ALLES O.K.
C                  IERR = 99, FALSCHER OPCODE
C                  IERR = 98, SYSTEMROUTINE NOCH NICHT INSTALLIERT
C
C
CH*   INTERNE:
C     -------
C
C     - CHSTR      STRING DER LAENGE 23, FUER DIE VAX-SYSTEM-ROUTINE
C
C
CI*   COMMON:
C     ------
C
C     -            KEINE
C
C
CJ*   BENOETIGTE UNTERPROGRAMME / FUNKTIONEN:
C     --------------------------------------
C
C     - SUBROUTINE LIB$DATE_TIME (VAX)
C
C
CL*   DIESE ROUTINE WIRD AUFGERUFEN VON:
C     ---------------------------------
C
C     - SUBROUTINE Name
C
C
CM*   ACHTUNG:     KEINE
C     -------
C
C
CN*   BEMERKUNG:   KONTROLL-AUSGABEN SIND MIT "CKA" GEKENNZEICHNET
C     ---------    
C                       
C
CO*   AUTOR:       AXEL HAENSCHKE
C     -----        VW-GEDAS, Berlin
C                  Pascalstrasse 11
C                  Abteilung: Technische Simulation
C
C
CP*   VERSION:     0.1                                     18-AUG-90
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
CCC      PARAMETER    20
C
C ----------------------------------------------------------------------
C
      INTEGER      IERR
C
C ----------------------------------------------------------------------
C
      CHARACTER*3  OPCODE
      CHARACTER*23 CHSTR
      CHARACTER*11 CHDATE
      CHARACTER*11 CHTIME    
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
C ======================================================================
CEV*  E I N G A B E V E R A R B E I T U N G
C ======================================================================
C
C *** SPRUNGANWEISUNG AN DIE SYSTEMROUTINEN
C
      IF ( OPCODE(1:3) .EQ. 'VAX' ) GOTO 100
      IF ( OPCODE(1:3) .EQ. 'UNI' ) GOTO 200
      IF ( OPCODE(1:3) .EQ. 'IBM' ) GOTO 300
      IF ( OPCODE(1:3) .EQ. 'CDC' ) GOTO 400
C
C *** FEHLERHAFTER OPCODE
C
      IERR = 99
      GOTO 9999
C
C ======================================================================
CVT*  V E R A R B E I T U N G S T E I L
C ======================================================================
C
C *** VAX-BETRIEBSSYSTEM (VMS)
C
  100 CONTINUE
C      CALL LIB$DATE_TIME (CHSTR)
C      CHDATE(1:11) = CHSTR(1:11)
C      CHTIME(1:11) = CHSTR(13:23)
C Redirect to UNIX implementation for portability
      GOTO 200
CTEST      WRITE ( 6, * ) CHSTR
CTEST      WRITE ( 6, * ) CHDATE
CTEST      WRITE ( 6, * ) CHTIME
C
      IERR = 0
      GOTO 9999
C
C *** UNIX-BETRIEBSSYSTEM
C
  200 CONTINUE
      BLOCK
        CHARACTER*8  F_DATE
        CHARACTER*10 F_TIME
        CHARACTER*3  MONTHS(12)
        INTEGER      VALUES(8)
        DATA MONTHS /'JAN','FEB','MAR','APR','MAY','JUN',
     +               'JUL','AUG','SEP','OCT','NOV','DEC'/
        CALL DATE_AND_TIME(F_DATE, F_TIME, ZONE=CHSTR(1:5), 
     +                     VALUES=VALUES)
        WRITE(CHDATE, '(I2.2,"-",A3,"-",I4.4)') 
     +        VALUES(3), MONTHS(VALUES(2)), VALUES(1)
        WRITE(CHTIME, '(I2.2,":",I2.2,":",I2.2,".",I2.2)') 
     +        VALUES(5), VALUES(6), VALUES(7), VALUES(8)/10
      END BLOCK
      IERR = 0
      GOTO 9999
C
C *** IBM-BETRIEBSSYSTEM (MVS)
C
  300 CONTINUE
      IERR = 98
      GOTO 9999
C
C *** CDC-BETRIEBSSYSTEM (NOS-VE)
C
  400 CONTINUE
      IERR = 98
      GOTO 9999
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
