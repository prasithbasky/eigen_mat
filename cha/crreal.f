      SUBROUTINE CRREAL(WORD, RNUM, IER)
C
C=====CHARACTER ROUTINES====2.0========Axel Haenschke======22 FEB 84===I
C                                                                      I
C   - F U N C T I O N :                                                I
C        CONVERTS STRING REPRESENTATION OF REAL INTO REAL.             I
C        CHECKS RANGE OF EXPONENT.                                     I
C        'WORD' MUST CONTAIN PROPER REAL SYNTAX                        I
C                                                                      I
C   - I N P U T :                                                      I
C        WORD   : CHAR*(*)   STRING REPRESENTATION OF REAL             I
C                                                                      I
C   - O U T P U T :                                                    I
C        RNUM   : REAL       CONVERTED STRING                          I
C        IER    : INT        ERROR CODE                                I
C                            0 : O.K.                                  I
C                            1 : INVALID SYNTAX                        I
C                            2 : REAL OUT OF RANGE                     I
C                                                                      I
C   - C A L L S :                                                      I
C        CRDIGI, CRSIGN, CRINT, CRMSG , CRKW                           I
C                                                                      I
C   - C A L L E D   B Y :                                              I
C        CRIANY, CRRSEQ                                                I
C                                                                      I
C   - R E M A R K S :                                                  I
C        ADJUSTMENTS TO IMPLEMENTATION                                 I
C          <MAXEXP> MUST CONTAIN LARGEST EXPONENT                      I
C          <MINEXP> MUST CONTAIN SMALLEST EXPONENT                     I
C          <SIGDEC> MUST CONTAIN <NUMBER OF SIGNIFICANT DECIMALS>+1    I
C          <CMANT>  MUST BE DECLARED CHARACTER*<SIGDEC>                I
C                                                                      I
C======================================================================I
C
      IMPLICIT     LOGICAL(A-Z)
C
C------- TYPE DECLARATIONS
C
      CHARACTER*(*) WORD
      CHARACTER*8   CMANT , CRKW
      CHARACTER*1   LETTER
      REAL          R     , RNUM
      INTEGER       PSIGN , PDIGIT, PNONUL, PPOINT, PEXPON, PESIGN,
     1              SIGN  , MANTIS, MANORD, EXPON , CRSIGN, IER   ,
     2              SIGDEC, MAXEXP, MINEXP, I     , J     ,LENGTH
      LOGICAL       CRDIGI
      PARAMETER    (SIGDEC = 8, MAXEXP = 75, MINEXP =-78)
C
      LENGTH = LEN(WORD)
      RNUM   = 0.
      IER    = 0
C
C------- BEGIN CONVERSION
C
      CMANT  ='00000000'
      SIGN   = CRSIGN (WORD(1:1))
      MANTIS = 0
      MANORD = 0
      EXPON  = 0
      PSIGN  = 0
      PDIGIT = 0
      PNONUL = 0
      PPOINT = 0
      PEXPON = 0
      PESIGN = 0
C
C------- CHECK FOR SIGN
C
      IF  (SIGN.EQ.0) THEN
           SIGN  = 1
           PSIGN = 1
      ELSE
           PSIGN = 2
      ENDIF
C
      IF  (PSIGN.GT.LENGTH) THEN
           IER = 1
           RETURN
      ENDIF
C
C------- FIND ALL SPECIAL CHARACTERS, CHECK ALL DIGITS
C
      DO 20 I = PSIGN, LENGTH
          LETTER = WORD(I:I)
          IF  (CRDIGI(LETTER)) THEN
               IF  (PDIGIT.EQ.0) THEN
                    PDIGIT = I
               ENDIF
               IF  (PNONUL.EQ.0.AND.LETTER.NE.'0') THEN
                    PNONUL = I
               ENDIF
          ELSE IF  (PEXPON.EQ.0.AND.PPOINT.EQ.0.AND.LETTER.EQ.'.') THEN
                    IF  (PDIGIT.EQ.0) THEN
                         J = MIN0( I+1, LENGTH)
                         IF (CRDIGI(WORD(J:J))) THEN
                             PPOINT = I
                         ELSE
                              GOTO 200
                         ENDIF
                    ELSE
                         PPOINT = I
                    ENDIF
          ELSE IF  (PEXPON.EQ.0.AND.PDIGIT.NE.0.AND.LETTER.EQ.'E')THEN
                    PEXPON = I
          ELSE IF  (PESIGN.EQ.0.AND.PEXPON.NE.0.AND.
     1              CRSIGN(LETTER).NE.0) THEN
                    PESIGN = I
          ELSE
               GOTO 200
          ENDIF
   20 CONTINUE
C
C------- STRIP EXPONENT
C
      IF  (PEXPON.EQ.0.OR.PEXPON.EQ.LENGTH.OR.PESIGN.EQ.LENGTH) THEN
           EXPON = 0
           IF  (PEXPON.EQ.0) PEXPON = LENGTH + 1
      ELSE
           CALL CRINT(WORD((PEXPON+1):LENGTH), EXPON ,IER)
           IF  (IER.NE.0)  RETURN
      ENDIF
C
C------- GET ORDER OF MANTISSA
C
      IF  (PNONUL.EQ.0) THEN
           RETURN
      ELSE
           IF  (PPOINT.EQ.0) PPOINT = PEXPON
           MANORD = PPOINT - PNONUL
           IF  (MANORD.GT.0) MANORD = MANORD - 1
      ENDIF
C
C------- GET SIGNIFICANT DIGITS FROM MANTISSA
C
      J      = 0
      PEXPON = PEXPON - 1
      DO 30 I = PNONUL, PEXPON
            IF  (I.EQ.PPOINT) GOTO 30
            IF  (J.GE.SIGDEC) GOTO 40
            J          = J + 1
            CMANT(J:J) = WORD(I:I)
   30 CONTINUE
C
C------- CONVERT MANTISSA
C
   40 CALL CRINT(CMANT(1:J), MANTIS, IER)
      IF  (IER.NE.0) THEN
           CALL CRMSG (0,'CRREAL : '//CRKW(34)//CRKW(14)//CRKW(1)//
     1                'CONVERSION OF MANTISSA')
           RETURN
      ENDIF
C
C------- ASSEMBLE REAL NUMBER, RANGE TEST
C
      EXPON  = EXPON + MANORD
      IF  (EXPON.LT.MINEXP.OR.EXPON.GT.MAXEXP) THEN
           IER = 2
      ELSE
           EXPON  = EXPON - J + 1
           RNUM   = FLOAT(MANTIS) * 10.** EXPON
           IF (SIGN.LT.0) RNUM = -RNUM
      ENDIF
C
  100 RETURN
  200 IER = 1
      RETURN
      END
