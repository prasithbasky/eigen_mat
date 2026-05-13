      SUBROUTINE CRIANY(OPCODE, BASE, STRING, RESULT, IPAR, RPAR, I, J)
C***********************************************************************
C
C     NAME    : CRIANY  FORTRAN                   CREATED: 18 JAN 1985
C     PROJECT : CHARACTER ROUTINES 2.0
C     AUTHOR  : Axel Haenschke
C
C   - F U N C T I O N :
C         INTERPRETS FIRST WORD IN STRING. WORD MAY BE ANY OF INTEGER
C         REAL, ALPHABETICAL, ALPHANUMERICAL OR SPECIAL.
C         'OPCODE' CONTAINS DESCRIPTION OF WHICH DATA-TYPE TO TRY
C         FIRST AND WITH WICH TO PROCEED IF FIRST ATTEMPT HAD FAILED.
C
C   - I N P U T :
C         OPCODE: CHAR*(*)   SEQUENCE OF SINGLE LETTERS, EACH
C                            REPRESENTING THE TYPE OF DATA TO BE
C                            INTERPRETED. OPCODE MAY CONTAIN ANY
C                            SEQUENCE OF THE FOLLOWING:
C                            'I' FOR INTEGER
C                            'R' FOR REAL
C                            'A' FOR ALPHABETICAL
C                            'N' FOR ALPHANUMERICAL
C                            'S' FOR SPECIAL CHARACTERS
C         BASE  : INT        BASE POINTER . SEARCH FOR WORD BEGINS WITH
C                            CHARACTER AT POSITION OF BASE POINTER.
C         STRING: CHAR*(*)   STRING TO BE INTERPRETED
C
C   - O U T P U T :
C         RESULT: INTEGER    POINTER TO POSITION OF CHARACTER IN
C                            OPCODE. THE DATA-TYPE DESCRIBED BY THAT
C                            CHARACTER HAS BEEN INTERPRETED.
C                            IF <NO SUCCESSFUL INTERPRETATION> THEN
C                                RESULT = LOP + 1
C         IPAR  : INTEGER    RESULT OF INTEGER INTERPRETATION.
C         RPAR  : REAL       RESULT OF REAL INTERPRETATION.
C         I     : INTEGER    IF < THERE IS A WORD > THEN
C                                 I = POINTER TO FIRST CHARACTER
C                                     OF FIRST WORD IN STRING
C                            ELSE
C                                 I = POINTER OF LAST CHARACTER
C                                     INSPECTED IN STRING
C                            ENDIF
C         J     : INTEGER    IF < THERE IS A WORD IN STRING > THEN
C                                 J = POINTER TO LAST CHARACTER
C                                     OF FIRST WORD STRING
C                            ELSE
C                                 J = 0
C                            ENDIF
C   - C O M M O N - B L O C K S :
C     ---
C
C   - C A L L S :
C         CRINTE, CRREAL, CRFIND, CRWHAT
C
C   - C A L L E D   B Y :
C         CRDIAG
C
C   - R E M A R K S :
C         ALPHANUMERICAL STRINGS MUST HAVE AT LEAST ONE DIGIT AND
C         ONE ALPHABETICAL CHARACTER
C
C                                                  UPDATE: 18 Jan 1985
C**********************************************************************
      IMPLICIT      LOGICAL(A-Z)
      CHARACTER*(*) OPCODE, STRING
      CHARACTER*1   LETTER
      INTEGER       RESULT, IPAR  , I     , J     , CRWHAT, BASE  ,
     1              BAS   , IER   ,LOP
      REAL          RPAR
C
      IPAR = 0
      RPAR = 0.
      LOP  = LEN ( OPCODE )
      BAS  = MIN0( MAX0 ( 1, BASE ) , LEN (STRING))
      CALL CRFIND(BAS, STRING(1:),I, J)
      IF  (J.NE.0) THEN
           DO 10 RESULT = 1, LOP
                 LETTER = OPCODE(RESULT:RESULT)
                 IF       (LETTER.EQ.'I') THEN
                           CALL CRINTE(STRING(I:J), IPAR, IER)
                           IF  (IER.EQ.0) RETURN
                 ELSE IF  (LETTER.EQ.'R') THEN
                           CALL CRREAL(STRING(I:J), RPAR, IER)
                           IF  (IER.EQ.0) RETURN
                 ELSE IF  (LETTER.EQ.'A') THEN
                           IF  (CRWHAT(STRING(I:J)).EQ.3) RETURN
                 ELSE IF  (LETTER.EQ.'N') THEN
                           IF  (CRWHAT(STRING(I:J)).EQ.4) RETURN
                 ELSE IF  (LETTER.EQ.'S') THEN
                           IF  (CRWHAT(STRING(I:J)).EQ.5) RETURN
                 ENDIF
   10     CONTINUE
      ENDIF
      RESULT = LOP + 1
      RETURN
      END
