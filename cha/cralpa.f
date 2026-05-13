      LOGICAL FUNCTION CRALPA(LETTER)
C
C=====CHARACTER ROUTINES====2.0========Axel Haenschke======19 FEB 85===
C
C   - F U N C T I O N :
C        .TRUE.   IF LETTER IS ELEMENT OF ALPHABET
C        .FALSE.  OTHERWISE
C
C   - I N P U T :
C        LETTER : CHAR*1    LETTER TO BE CHECKED
C
C   - O U T P U T :
C        CRALPA : LOG       RESULT
C
C   - C A L L E D    B Y :
C        CRSPEC, CRWHAT
C
C======================================================================
C
      CHARACTER*1  LETTER
      CRALPA = LGE(LETTER,'A').AND.LLE(LETTER,'Z')
      RETURN
      END
