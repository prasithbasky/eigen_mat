      INTEGER FUNCTION CRWHAT(STRING)
C
C=====CHARACTER ROUTINES====2.0========Axel Haenschke======22 FEB 85===I
C                                                                      I
C   - F U N C T I O N :                                                I
C        FINDS OUT WHAT IS IN STRING. RETURNS THE FOLLOWING CODES      I
C                                                                      I
C        1 : STRING CAN BE INTEGER, SEQUENCE OF SYNTAX NOT CHECKED     I
C        2 : STRING CAN BE REAL, SEQUENCE OF SYNTAX NOT CHECKED        I
C        3 : STRING CONTAINS <ALPHA> ONLY                              I
C        4 : STRING CONTAINS <ALPHA> AND <DIGIT> ONLY                  I
C        5 : STRING CONTAINS <SPEC> ONLY                               I
C        6 : STRING CONTAINS <WORD> = {<ALPHA>]<DIGIT>]<SIGN>]<POINT>] I
C                                      <SPEC>}                         I
C        7 : STRING CONTAINS <DELIMITER> = .NOT.<WORD> ONLY            I
C        8 : STRING CONTAINS A MIX OF DIFFERENT CHARACTER TYPES        I
C                                                                      I
C   - I N P U T :                                                      I
C        STRING : CHAR*(*)    STRING TO BE CHECKED FOR CONTENT         I
C                                                                      I
C   - O U T P U T :                                                    I
C        CRWHAT : INT         RESULT OF CHECK                          I
C                                                                      I
C   - C O M M O N - B L O C K S :                                      I
C        NO COMMON BLOCKS                                              I
C                                                                      I
C   - C A L L S :                                                      I
C        CRALPA, CRDIGI, CRSIGN, CREOS, CRSPEC, CRDELI                 I
C                                                                      I
C   - C A L L E D   B Y :                                              I
C        CRIANY, CRRSEQ, CRISEQ                                        I
C                                                                      I
C   - R E M A R K S :                                                  I
C        NO REMARKS                                                    I
C                                                                      I
C======================================================================I
C
      IMPLICIT     LOGICAL(A-Z)
C
C------- TYPE DECLARATIONS
C
      CHARACTER*(*) STRING
      CHARACTER*1   LETTER
      INTEGER       CRSIGN, NALPA , NDIGIT, NSIGN , NPOINT, NEXPON,
     1              NDELIM, NSPEC , LENGTH, EOS   , I
      LOGICAL       CRALPA, CRDIGI, CREOS , CRSPEC, CRDELI
C
C------- INIT
C
      LENGTH = LEN(STRING)
      NALPA  = 0
      NDIGIT = 0
      NSIGN  = 0
      NPOINT = 0
      NEXPON = 0
      NDELIM = 0
      NSPEC  = 0
      EOS    = LENGTH
C
C------- COUNT NUMBER OF EACH TYPE OF LETTER
C
      DO 10 I=1, LENGTH
          LETTER = STRING(I:I)
          IF  (CRALPA(LETTER)) THEN
               NALPA = NALPA + 1
               IF  (LETTER.EQ.'E') THEN
                    NEXPON = NEXPON + 1
               ENDIF
          ELSE IF  (CRDIGI(LETTER)) THEN
                    NDIGIT = NDIGIT + 1
          ELSE IF  (CRSIGN(LETTER).NE.0) THEN
                    NSIGN  = NSIGN  + 1
          ELSE IF  (CRSPEC(LETTER)) THEN
                    NSPEC  = NSPEC  + 1
          ELSE IF  (LETTER.EQ.'.') THEN
                    NPOINT = NPOINT + 1
          ELSE IF  (CREOS(LETTER)) THEN
                    EOS    = I - 1
                    GOTO 20
          ELSE
                    NDELIM = NDELIM + 1
          ENDIF
   10 CONTINUE
C
C------- INTERPRET
C
   20 LENGTH = EOS
      IF  (LENGTH.EQ.0) THEN
                CRWHAT = 7
                RETURN
      ENDIF
      IF  (NDELIM.EQ.0) THEN
           IF  (NSPEC.EQ.0) THEN
                IF  (NALPA.EQ.NEXPON) THEN
                     IF  (NALPA.LE.1) THEN
                          IF  (NDIGIT.GE.1) THEN
                               IF  (NSIGN-1.LE.NEXPON) THEN
                                    IF  (NPOINT.EQ.0) THEN
                                         CRWHAT = 1
                                    ELSE IF  (NPOINT.EQ.1) THEN
                                              CRWHAT = 2
                                    ELSE
                                              CRWHAT = 6
                                    ENDIF
                               ELSE
                                    CRWHAT = 6
                               ENDIF
                          ELSE IF  (NSIGN+NPOINT.EQ.0) THEN
                                    CRWHAT = 3
                          ELSE
                                    CRWHAT = 6
                          ENDIF
                     ELSE
                          CRWHAT = 3
                     ENDIF
                ELSE IF  (NDIGIT.EQ.0) THEN
                          IF  (NPOINT+NSIGN.EQ.0) THEN
                               CRWHAT = 3
                          ELSE
                               CRWHAT = 6
                          ENDIF
                ELSE IF  (NSIGN+NPOINT.EQ.0) THEN
                          CRWHAT = 4
                ELSE
                          CRWHAT = 6
                ENDIF
           ELSE IF  (NALPA+NDIGIT+NPOINT+NSIGN.EQ.0) THEN
                     CRWHAT = 5
           ELSE
                     CRWHAT = 6
           ENDIF
      ELSE IF  (NSPEC+NALPA+NDIGIT+NPOINT+NSIGN.EQ.0) THEN
                CRWHAT = 7
      ELSE
                CRWHAT = 8
      ENDIF
      RETURN
      END
