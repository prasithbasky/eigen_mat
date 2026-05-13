      SUBROUTINE CRINTE(WORD, INUM, IER)
C
C=====CHARACTER ROUTINES====2.0========Axel Haenschke======22 FEB 85===I
C                                                                      I
C   - F U N C T I O N :                                                I
C        CONVERTS STRING REPRESENTATION OF INTEGER INTO INTEGER        I
C        STRING MUST CONTAIN <DIGIT> AND/OR <SIGN> ONLY                I
C        INTEGER EXPONENT MAY BE SPECIFIED.                            I
C                                                                      I
C   - I N P U T :                                                      I
C        WORD   : CHAR*(*)  STRING REPRESENTATION OF INTEGER           I
C                                                                      I
C   - O U T P U T :                                                    I
C        INUM   : INT       CONVERTED STRING                           I
C        IER    : INT       ERROR CODE                                 I
C                           0 : O.K.                                   I
C                           1 : INVALID SYNTAX                         I
C                           2 : INTEGER OUT OF RANGE                   I
C                                                                      I
C   - C A L L S :                                                      I
C        CRSIGN, CRINT , CRDIGI                                        I
C                                                                      I
C   - C A L L E D   B Y :                                              I
C        CRIANY, CRISEQ                                                I
C                                                                      I
C   - R E M A R K S :                                                  I
C        ADJUSTMENTS TO IMPLEMENTATION :                               I
C          <MAXDIG>  MUST CONTAIN MAXIMUM NUMBER OF DIGITS IN INTEGER  I
C          <MAXINT>  MUST CONTAIN LARGEST POSSIBLE INTEGER             I
C                                                                      I
C======================================================================I
C
      IMPLICIT     LOGICAL(A-Z)
C
C------- TYPE DECLARATIONS
C
      CHARACTER*(*) WORD
      CHARACTER*10  FILE
      CHARACTER*1   LETTER
      INTEGER       EXPON , PEXPON, MAXINT, MAXDIG, LENGTH,
     1              CRSIGN, I     , INUM  , IER
      LOGICAL       CRDIGI
      PARAMETER    (MAXDIG = 10, MAXINT = 2147483647)
C
      LENGTH = LEN(WORD)
      IER    = 0
      INUM   = 0
C
C------- BEGIN CONVERSION
C
      EXPON  = 0
      PEXPON = 0
C
C------- FIND EXPONENT MARKER, ANALYZE EXPONENT
C
      DO 10 I = LENGTH, 1,-1
          LETTER = WORD(I:I)
          IF  (LETTER.EQ.'E') THEN
               PEXPON = I
               IF  (I.LT.LENGTH) THEN
                    IF  (CRSIGN(WORD(I+1:I+1)).EQ.0) THEN
                         CALL CRINT(WORD(I+1:LENGTH), EXPON, IER)
                    ELSE IF (I+1.LT.LENGTH) THEN
                             CALL CRINT(WORD(I+1:LENGTH), EXPON, IER)
                    ELSE
                         EXPON = 0
                         GOTO 20
                    ENDIF
                    IF  (IER.NE.0) THEN
                         RETURN
                    ELSE
                         GOTO 20
                    ENDIF
               ELSE
                    EXPON = 0
                    GOTO 20
               ENDIF
          ENDIF
   10 CONTINUE
      PEXPON = LENGTH + 1
C
C------- CONVERSION AND RANGE TEST
C
   20 PEXPON = PEXPON - 1
      IF  (EXPON.LT.0) THEN
           PEXPON = PEXPON + EXPON
           IF  (PEXPON.LE.0) THEN
                INUM = 0
                RETURN
           ELSE IF (CRSIGN(WORD(1:1)).NE.0.AND.PEXPON.EQ.1) THEN
                INUM = 0
                RETURN
           ELSE
                CALL CRINT(WORD(1:PEXPON), INUM, IER)
                RETURN
           ENDIF
      ELSE
           CALL CRINT(WORD(1:PEXPON), INUM , IER)
           IF  (IER.NE.0) RETURN
           IF  (EXPON.GE.MAXDIG) THEN
                IER = 2
                RETURN
           ELSE
                EXPON = 10**EXPON
                IF  (IABS(INUM).GT.MAXINT/EXPON) THEN
                     IER = 2
                     RETURN
                ENDIF
           ENDIF
           INUM = INUM * EXPON
      ENDIF
C
      RETURN
      END
