      INTEGER FUNCTION CRSIGN(LETTER)
C
C=====CHARACTER ROUTINES====2.0========Axel Haenschke======19 FEB 85===I
C                                                                      I
C   - F U N C T I O N :                                                I
C        + 1   IF LETTER IS '+'                                        I
C          0   IF LETTER IS NOT A <SIGN>                               I
C        - 1   IF LETTER IS '-'                                        I
C                                                                      I
C   - I N P U T :                                                      I
C        LETTER : CHAR*1    LETTER TO BE CHECKED                       I
C                                                                      I
C   - O U T P U T :                                                    I
C        CRSIGN : INT       RESULT                                     I
C                                                                      I
C   - C A L L E D   B Y :                                              I
C        CRINT , CRINTE, CRREAL, CRWHAT                                I
C                                                                      I
C======================================================================I
C
      CHARACTER*1  LETTER
      CRSIGN = 0
      IF (LETTER.EQ.'-') THEN
          CRSIGN =-1
          RETURN
      ELSE IF (LETTER.EQ.'+') THEN
               CRSIGN = 1
               RETURN
      ENDIF
      RETURN
      END
