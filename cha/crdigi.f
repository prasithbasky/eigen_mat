      LOGICAL FUNCTION CRDIGI(LETTER)
C
C=====CHARACTER ROUTINES====2.0========Axel Haenschke======19 FEB 85===
C
C   - F U N C T I O N :
C        .TRUE.   IF LETTER IS DIGIT
C        .FALSE.  OTHERWISE
C
C   - I N P U T :
C        LETTER : CHAR*1    LETTER TO BE CHECKED FOR DIGIT
C
C   - O U T P U T :
C        CRDIGI : LOG       RESULT
C
C   - C A L L E D    B Y :
C        CRINT  , CRINTE, CRREAL, CRSPEC, CRWHAT
C
C======================================================================
C
      CHARACTER*1  LETTER
      CRDIGI = LGE(LETTER,'0').AND.LLE(LETTER,'9')
      RETURN
      END
