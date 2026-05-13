      LOGICAL FUNCTION CRDELI(LETTER)
C
C=====CHARACTER ROUTINES====2.0========Axel Haenschke======19 FEB 85===
C
C   - F U N C T I O N :
C        .TRUE.   IF LETTER IS DELIMITER
C                 <DELIMITER> = <BLANK> <COMMA> <END-OF-STRING>
C
C        .FALSE.  OTHERWISE
C
C   - I N P U T :
C        LETTER : CHAR*1    LETTER TO BE CHECKED
C
C   - O U T P U T :
C        CRDELI : LOG       RESULT
C
C   - C A L L S :
C        CREOS
C
C   - C A L L E D   B Y :
C        CRFIND, CRSPEC, CRWHAT
C
C======================================================================
C
      CHARACTER*1  LETTER
      LOGICAL      CREOS , BLANK, COMMA
      BLANK  = LETTER.EQ.' '
      COMMA  = LETTER.EQ.','
      CRDELI = CREOS(LETTER).OR.BLANK.OR.COMMA
      RETURN
      END
