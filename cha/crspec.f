      LOGICAL FUNCTION CRSPEC(LETTER)
C
C=====CHARACTER ROUTINES====2.0========Axel Haenschke======19 FEB 85===I
C                                                                      I
C   - F U N C T I O N :                                                I
C        .TRUE.   IF LETTER IS SPECIAL CHARACTER                       I
C                 <SPECIAL CHARACTER> = NOT <ALPA>                     I
C                                       NOT <DIGI>                     I
C                                       NOT <DELIMITER>                I
C                                                                      I
C        .FALSE.  OTHERWISE                                            I
C                                                                      I
C   - I N P U T :                                                      I
C        LETTER : CHAR*1    LETTER TO BE CHECKED                       I
C                                                                      I
C   - O U T P U T :                                                    I
C        CRSPEC : LOG       RESULT                                     I
C                                                                      I
C   - C A L L S :                                                      I
C        CRALPA, CRDIGI, CRDELI                                        I
C                                                                      I
C   - C A L L E D   B Y :                                              I
C        CRWHAT                                                        I
C                                                                      I
C======================================================================I
C
      CHARACTER*1  LETTER
      LOGICAL      CRALPA, CRDIGI, CRDELI
      INTEGER      CRSIGN
      CRSPEC = .FALSE.
      IF  ( CRALPA(LETTER)      ) RETURN
      IF  ( CRDIGI(LETTER)      ) RETURN
      IF  ( CRDELI(LETTER)      ) RETURN
      CRSPEC = .TRUE.
      RETURN
      END
