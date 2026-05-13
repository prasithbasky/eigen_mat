      LOGICAL FUNCTION CREOS(LETTER)
C
C=====CHARACTER ROUTINES====2.0========Axel Haenschke======19 FEB 85===
C
C   - F U N C T I O N :
C        .TRUE.   IF LETTER IS END-OF-STRING CHARACTER ';'
C        .FALSE.  OTHERWISE
C
C   - I N P U T :
C        LETTER :  CHAR*1     CHARACTER TO BE TESTED FOR EOS
C
C   - O U T P U T :
C        CREOS  :  LOG        RESULT OF TEST
C
C   - C A L L E D    B Y :
C        CRDELI, CREDIT, CRFIND, CRLINE, CRMASK, CRWHAT
C
C======================================================================
C
      IMPLICIT     LOGICAL(A-Z)
      CHARACTER*1  LETTER
      CREOS = LETTER.EQ.';'
      RETURN
      END
