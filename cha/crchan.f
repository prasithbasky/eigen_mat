      SUBROUTINE CRCHAN(OPCODE, CHANID, CHANNL, LENGTH)
C***********************************************************************
C
C     NAME    : CRCHAN  FORTRAN                   CREATED: 12 DEC 1984
C     PROJECT : CHARACTER ROUTINES 2.0
C     AUTHOR  : Axel Haenschke
C
C   - F U N C T I O N :
C         INTERFACE TO CHANNEL NUMBERS OF CHARACTER-ROUTINES
C
C   - I N P U T :
C         OPCODE: CHAR*(*)   OPERATION CODE
C                            'GET'   : GET CHANNEL NUMBER OF 'CHANID'
C                            'SET'   : SET CHANNEL NUMBER OF 'CHANID'
C         CHANID: CHAR*(*)   CHANNEL IDENTIFICATION
C                            'READ'  : CURRENT READ-CHANNEL
C                            'WRITE' : CURRENT WRITE-CHANNEL
C                            'PRINT' : SECONDARY WRITE-CHANNEL
C                                      USED FOR PRINTING OF PROTOCOL
C
C   - T W O   W A Y   P A R A M E T E R S :
C         CHANNL: INT        CHANNEL NUMBER > 0
C         LENGTH: INT        MAX LINE LENGTH TO BE READ OR WRITTEN > 0
C
C   - C O M M O N - B L O C K S :
C     ---
C
C   - C A L L S :
C         CRKW
C
C   - C A L L E D   B Y :
C         CRREAD, CRWRIT
C
C   - R E M A R K S :
C       > THE DEFAULT VALUES OF THE CHANNEL NUMBERS ARE SET TO
C         READ = 5 AND WRITE = 6, WHICH IS THE STANDARD ASSIGNMENT
C         OF TERMINAL I/O, AND PRINT = 60.
C
C       > THE DEFAULT LENGTHS OF LINES TO BE READ OR WRITTEN ARE SET
C         TO LREAD  = 80 , LWRITE = 80 , LPRINT = 79.
C
C       > LREAD MUST NOT EXCEED 80 AS THE INTERNAL
C         BUFFER OF CRLINE CAN HOLD 80 CHARACTERS PER ENTRY ONLY.
C
C       > LWRITE/LPRINT DO NOT INCLUDE THE PRINTER CONTROL CHARACTER,
C         THUS THE ACTUAL LINE LENGTH IS LWRITE + 1 / LPRINT + 1.
C
C       > CAUTION: IF 'READ' IS NOT 5 THEN CRREAD WILL NOT
C                  PRODUCE THE PROMPT MESSAGE
C
C       > USERS OF TEKTRONIX 4115 UNDER TR/41 :
C         WRITE-CHANNEL MAY BE SET TO '-6' TO SEND OUTPUT VIA TRMSG
C
C                                                  UPDATE: 22 FEB 1985
C**********************************************************************
      IMPLICIT      LOGICAL(A-Z)
      CHARACTER*(*) OPCODE, CHANID
      CHARACTER*8   CRKW
      INTEGER       CHANNL, LENGTH, CREAD , CWRITE, CPRINT,
     1              LREAD , LWRITE, LPRINT
      DATA          CREAD , LREAD   /  5, 80 /
      DATA          CWRITE, LWRITE  /  6, 80 /
      DATA          CPRINT, LPRINT  / 60, 79 /
      SAVE          CREAD , CWRITE, CPRINT, LREAD , LWRITE, LPRINT
      IF       (OPCODE.EQ.'GET') THEN
                IF      (CHANID.EQ.'READ') THEN
                         CHANNL = CREAD
                         LENGTH = LREAD
                ELSE IF (CHANID.EQ.'WRITE') THEN
                         CHANNL = CWRITE
                         LENGTH = LWRITE
                ELSE IF (CHANID.EQ.'PRINT') THEN
                         CHANNL = CPRINT
                         LENGTH = LPRINT
                ELSE
                         GOTO 10
                ENDIF
      ELSE IF  (OPCODE.EQ.'SET') THEN
                IF      (CHANID.EQ.'READ') THEN
                         CREAD  = CHANNL
                         LREAD  = LENGTH
                ELSE IF (CHANID.EQ.'WRITE') THEN
                         CWRITE = CHANNL
                         LWRITE = LENGTH
                ELSE IF (CHANID.EQ.'PRINT') THEN
                         CPRINT = CHANNL
                         LPRINT = LENGTH
                ELSE
                         GOTO 10
                ENDIF
      ELSE
                GOTO 10
      ENDIF
      RETURN
   10 CONTINUE
      WRITE (CWRITE,'(1X,A)') 'CRCHAN : '//CRKW(21)//CRKW(19)
     1                                   //CRKW(33)//CRKW(17)
CCC      WRITE (CWRITE,'(1X,A   )') '----> OPCODE = '//OPCODE(1:)
CCC      WRITE (CWRITE,'(1X,A/1X)') '----> CHANID = '//CHANID(1:)
      RETURN
      END
