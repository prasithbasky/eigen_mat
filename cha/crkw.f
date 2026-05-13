      CHARACTER*(*) FUNCTION CRKW (I)
C***********************************************************************
C
C     NAME    : CRKW    FORTRAN                   CREATED: 15 FEB 1985
C     PROJECT : CHARACTER ROUTINES 2.0
C     AUTHOR  : Axel Haenschke
C
C   - F U N C T I O N :
C       KEYWORD TRANSPORTER FOR CONSTRUCTION OF MESSAGES
C
C   - I N P U T :
C       I       : INT        KEYWORD SELECTOR
C
C   - O U T P U T :
C       CRKW    : CHAR*(*)   SELECTED KEYWORD, SEE BELOW
C
C   - C A L L E D    B Y
C       CRCHAN, CRMAIN, CRMASK, CRMSG , CRNAMS, CRPROM, CRREAL, CRSWIC
C
C   - R E M A R K S :
C       > KEYWORD ROUTINE CAN BE EXPANDED ACCORDING TO REQUIREMENTS.
C       > IF <I OUT OF RANGE> THEN CRKW = ' *-??-* '
C
C                                                  UPDATE: 15 Feb 1985
C**********************************************************************
      IMPLICIT     LOGICAL(A-Z)
      INTEGER      NWORDS
      PARAMETER   (NWORDS = 47)
      CHARACTER*8  WORDS (NWORDS)
      INTEGER      I
      DATA         WORDS ( 1) / ' AFTER  ' /
      DATA         WORDS ( 2) / '  AND   ' /
      DATA         WORDS ( 3) / ' AGAIN  ' /
      DATA         WORDS ( 4) / ' CHECK  ' /
      DATA         WORDS ( 5) / ' CLEAR  ' /
      DATA         WORDS ( 6) / ' CLOSED ' /
      DATA         WORDS ( 7) / 'COMMAND ' /
      DATA         WORDS ( 8) / 'DATABASE' /
      DATA         WORDS ( 9) / 'DIVISION' /
      DATA         WORDS (10) / ' EMPTY  ' /
      DATA         WORDS (11) / ' ENTER  ' /
      DATA         WORDS (12) / ' ENTRY  ' /
      DATA         WORDS (13) / 'EPSILON ' /
      DATA         WORDS (14) / ' ERROR  ' /
      DATA         WORDS (15) / ' EXIT   ' /
      DATA         WORDS (16) / '  FILE  ' /
      DATA         WORDS (17) / ' FOUND  ' /
      DATA         WORDS (18) / '  FULL  ' /
      DATA         WORDS (19) / ' INPUT  ' /
      DATA         WORDS (20) / 'INTEGER ' /
      DATA         WORDS (21) / 'INVALID ' /
      DATA         WORDS (22) / '  IS    ' /
      DATA         WORDS (23) / '  LIST  ' /
      DATA         WORDS (24) / 'LOGICAL ' /
      DATA         WORDS (25) / 'MISSING ' /
      DATA         WORDS (26) / '  NOT   ' /
      DATA         WORDS (27) / ' OPCODE ' /
      DATA         WORDS (28) / '  OPEN  ' /
      DATA         WORDS (29) / 'OPERAND ' /
      DATA         WORDS (30) / '  OR    ' /
      DATA         WORDS (31) / ' OUT OF ' /
      DATA         WORDS (32) / ' OUTPUT ' /
      DATA         WORDS (33) / ' PARAM. ' /
      DATA         WORDS (34) / 'PROGRAM ' /
      DATA         WORDS (35) / ' RANGE  ' /
      DATA         WORDS (36) / '  REAL  ' /
      DATA         WORDS (37) / ' REPEAT ' /
      DATA         WORDS (38) / ' RESULT ' /
      DATA         WORDS (39) / '  SOME  ' /
      DATA         WORDS (40) / '  SPACE ' /
      DATA         WORDS (41) / ' SYNTAX ' /
      DATA         WORDS (42) / ' SYSTEM ' /
      DATA         WORDS (43) / '  TRY   ' /
      DATA         WORDS (44) / '  USE   ' /
      DATA         WORDS (45) / ' VALID  ' /
      DATA         WORDS (46) / ' VALUE  ' /
      DATA         WORDS (47) / '  ZERO  ' /
      SAVE         WORDS
      IF  (I.GE.1) THEN
           IF  (I.LE.NWORDS) THEN
                CRKW = WORDS(I)
           ELSE
                GOTO 10
           ENDIF
      ELSE
   10      CRKW = ' *-??-* '
      ENDIF
      RETURN
      END
