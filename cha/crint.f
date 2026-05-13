      SUBROUTINE CRINT (WORD, INUM, IER)
C
C=====CHARACTER ROUTINES====2.0========Axel Haenschke======22 FEB 85===
C
C   - F U N C T I O N :
C        CONVERTS STRING REPRESENTATION OF INTEGER INTO INTEGER
C        STRING MUST CONTAIN <DIGIT> AND/OR <SIGN> ONLY
C
C   - I N P U T :
C        WORD   : CHAR*(*)  STRING REPRESENTATION OF INTEGER
C
C   - O U T P U T :
C        INUM   : INT       CONVERTED STRING
C        IER    : INT       ERROR CODE
C                           0 : O.K.
C                           1 : INVALID SYNTAX
C                           2 : INTEGER OUT OF RANGE
C
C   - C A L L S :
C        CRDIGI, CRSIGN
C
C   - C A L L E D   B Y :
C        CRREAL, CRINTE
C
C   - R E M A R K S :
C        ADJUSTMENTS TO IMPLEMENTATION :
C          <MAXDIG>  MUST CONTAIN MAXIMUM NUMBER OF DIGITS IN INTEGER
C          <MAXINT>  MUST CONTAIN CHARACTER REPRESENTATION OF
C                    LARGEST POSSIBLE INTEGER
C          <FILE>
C          <RESULT>  MUST BE DECLARED AS 'CHARACTER*<MAXDIG>'
C          <MAXINT>
C
C          <RESULT>  MUST BE FILLED WITH <MAXDIG> ZEROS
C          <IFORM>   MUST CONTAIN '(I<MAXDIG>)'
C
C======================================================================
C
      IMPLICIT     LOGICAL(A-Z)
C
C------- TYPE DECLARATIONS
C
      CHARACTER*(*) WORD
      CHARACTER*10  FILE  , RESULT, MAXINT
      CHARACTER*5   IFORM
      CHARACTER*1   LETTER
      INTEGER       CRSIGN, SIGN  , FIRST , LENGTH, PNONUL, MAXDIG,
     1              I     , INUM  , IER
      LOGICAL       CRDIGI
      PARAMETER    (MAXDIG = 10, MAXINT = '2147483647',
     1              IFORM  ='(I10)')
C
      LENGTH = LEN(WORD)
      INUM   = 0
      IER    = 0
C
C------- BEGIN CONVERSION
C
      FIRST  = 1
      RESULT ='0000000000'
C
C------- CHECK FOR LEADING SIGN
C
      SIGN   = CRSIGN (WORD(1:1))
      IF (SIGN.NE.0) THEN
          FIRST = 2
      ELSE
          SIGN  = 1
      ENDIF
C
C------- CHECK IF SIGN ONLY
C
      IF (FIRST.GT.LENGTH) THEN
          IER = 1
          RETURN
      ENDIF
C
C------- CHECK ALL DIGITS, FIND FIRST NON-ZERO DIGIT
C
      PNONUL  = 0
      DO 20 I = FIRST, LENGTH
          LETTER = WORD(I:I)
          IF  (CRDIGI(LETTER)) THEN
               IF  (PNONUL.EQ.0.AND.LETTER.NE.'0') THEN
                    PNONUL = I
               ENDIF
          ELSE
               IER = 1
               RETURN
          ENDIF
   20 CONTINUE
C
C------- CONVERSION AND RANGE TEST
C
      FIRST  = MAXDIG - LENGTH + PNONUL
      I      = MAX0(FIRST, 1)
      IF (PNONUL.EQ.0) THEN
          INUM  = 0
          GOTO 100
      ELSE
          RESULT(I:MAXDIG) = WORD(PNONUL:LENGTH)
          IF (FIRST.LE.0.OR.LGT(RESULT(1:MAXDIG),MAXINT)) THEN
              IER = 2
              RETURN
          ELSE
              WRITE (FILE,'(A)') RESULT
              READ  (FILE,IFORM) I
              INUM  = SIGN * I
              GOTO 100
          ENDIF
      ENDIF
C
  100 RETURN
      END
