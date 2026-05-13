      LOGICAL FUNCTION CRPROT (DUMMY)
C***********************************************************************
C
C     NAME    : CRPROT  FORTRAN                   CREATED: 15 FEB 1985
C     PROJECT : CHARACTER ROUTINES 2.0
C     AUTHOR  : Axel Haenschke
C
C   - F U N C T I O N :
C       TESTS IF PROTOCOL IS ENABELED
C
C   - I N P U T :
C       DUMMY   : INT          DUMMY PARAMETER
C
C   - O U T P U T :
C       CRPROT  : LOG          .TRUE.  : PROTOCOL IS ENABELED
C                              .FALSE. : PROTOCOL IS NOT ENABELED
C
C   - C O M M O N - B L O C K S :
C       CRCBFL = CHARO COMMON FLAGS, IN CRPROT CRSWIC CRNAMS CRPROT
C
C   - C A L L S :
C     ---
C
C   - C A L L E D   B Y :
C       CRWRIT
C
C   - R E M A R K S :
C     ---
C                                                  UPDATE: 15 Feb 1985
C**********************************************************************
      IMPLICIT      LOGICAL(A-Z)
      COMMON      / CRCBFL / NOTRA1, NOTRA2, ALTRAC, TRAMOD, POINTR,
     1                       PROT  , IBREAK, NBREAK
      LOGICAL       ALTRAC, TRAMOD , PROT
      INTEGER       NOTRA1, NOTRA2 , POINTR, IBREAK, NBREAK, DUMMY
      CRPROT = PROT
      RETURN
      END
