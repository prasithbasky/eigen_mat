      SUBROUTINE CRWRIT(STRING)
C
C=====CHARACTER ROUTINES====2.0========Axel Haenschke======19 FEB 85===I
C                                                                      I
C   - F U N C T I O N :                                                I
C        WRITES STRING TO OUTPUT DEVICE                                I
C                                                                      I
C   - I N P U T :                                                      I
C        STRING : CHAR*(*)   CONTAINS CHARACTERS TO BE WRITTEN         I
C                                                                      I
C   - O U T P U T :                                                    I
C        NO OUTPUT                                                     I
C                                                                      I
C   - C O M M O N - B L O C K S :                                      I
C        NO COMMON BLOCKS                                              I
C                                                                      I
C   - C A L L S :                                                      I
C        CRCHAN, CRPROT, TRMSG                                         I
C                                                                      I
C   - C A L L E D   B Y :                                              I
C        CRREAD, CRLINE, CREDIT, CRMSG                                 I
C                                                                      I
C   - R E M A R K S :                                                  I
C        19 FEB 85 : TERMINAL WRITE ONLY                               I
C        LATER     : DISK MAY BE SPECIFIED                             I
C        STRING IS WRITTEN ONLY UP TO LENGTH SPECIFIED BY CRCHAN       I
C        IF  <WRITE-CHANNEL = -6> THEN                                 I
C             TRMSG IS USED FOR OUTPUT                                 I
C        ENDIF                                                         I
C        IF  <PROTOCOL IS ENABLED> THEN
C             PROTOCOL IS WRITTEN TO PRINTER                           I
C        ENDIF                                                         I
C                                                                      I
C======================================================================I
C
      IMPLICIT      LOGICAL(A-Z)
      CHARACTER*(*) STRING
      LOGICAL       CRPROT
      INTEGER       TW    , L
      CALL CRCHAN('GET', 'WRITE', TW, L)
      L = MIN0(L, LEN(STRING(1:)))
      IF  (TW.EQ.-6) THEN
C         CALL TRMSG(STRING(1:L))
          PRINT *, STRING (1:L)
      ELSE
           WRITE (TW,'(1X,A)') STRING(:L)
      ENDIF
      IF  (CRPROT(0)) THEN
           CALL CRCHAN('GET', 'PRINT', TW, L)
           L = MIN0(L, LEN(STRING(1:)))
           WRITE (TW,'(1X,A)') STRING(:L)
      ENDIF
      RETURN
      END
