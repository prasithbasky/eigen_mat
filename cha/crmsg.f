      SUBROUTINE CRMSG (DELTAI, STRING)
C***********************************************************************
C
C     NAME    : CRMSG   FORTRAN                   CREATED: 15 FEB 1985
C     PROJECT : CHARACTER ROUTINES 2.0
C     AUTHOR  : Axel Haenschke
C
C   - F U N C T I O N :
C       OUTPUT OF MESSAGE WITH INDENTATION
C
C   - I N P U T :
C       DELTAI  : INTEGER    DELTA INDENTATION
C       STRING  : CHAR*(*)   MESSAGE TO BE SENT TO USER
C
C   - C O M M O N - B L O C K S :
C       ---
C
C   - C A L L S :
C       CRWRIT
C
C   - C A L L E D   B Y :
C       CRCOMM, CRDIAG, CRMAIN , CRMASK, CRNAMS, CRPROM, CRREAL,
C       CRSWIC, CRTRAC
C
C   - R E M A R K S :
C       MAXIMUM INDENTATION IS 40 CHARACTERS
C
C                                                  UPDATE: 15 FEB 1985
C**********************************************************************
      IMPLICIT      LOGICAL(A-Z)
      CHARACTER*(*) STRING
      CHARACTER     CRKW*8, ARROW*40
      INTEGER       INDENT, DELTAI
      DATA          INDENT / 1 /
      DATA          ARROW  /'--------------------------------------> '/
      SAVE          INDENT
      EXIT   = .FALSE.
      IF  (DELTAI.GE.0) INDENT = INDENT + DELTAI
      INDENT = MAX0(      1, INDENT)
      INDENT = MIN0( INDENT,     40)
      CALL CRWRIT (ARROW ((41 - INDENT):40)//STRING(1:))
      IF  (DELTAI.LT.0) INDENT = INDENT + DELTAI
      RETURN
      END
