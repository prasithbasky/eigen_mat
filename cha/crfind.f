      SUBROUTINE CRFIND(BASE, STRING, FIRST, LAST)
C
C=====CHARACTER ROUTINES====2.0========Axel Haenschke======22 FEB 84===
C
C   - F U N C T I O N :
C        RETURNS POINTER TO FIRST AND LAST CHARACTER OF FIRST
C        <WORD> IN STRING. <WORD> IS PRECEEDED AND SUCCEEDED
C        BY A <DELIMITER>:
C
C                <DELIMITER><WORD><DELIMITER>
C
C        <DELIMITER> IS ONE OF THE FOLLOWING :
C                <START OF STRING>
C                <END OF STRING>
C                <BLANK>
C                <COMMA>
C
C   - I N P U T :
C        BASE   : INT        BASE POINTER. SEARCH BEGINS WITH
C                            CHARACTER AT POSITION OF BASE POINTER
C        STRING : CHAR*(*)   STRING CONTAINING <WORD>
C
C   - O U T P U T :
C        FIRST  : INT        FIRST POSITION OF FIRST WORD IN STRING
C                            IF <NOWORD> THEN
C                                FIRST = POSITION OF LAST CHARACTER
C                                        INSPECTED IN STRING
C        LAST   : INT        LAST POSITION OF FIRST WORD IN STRING
C                            IF <NOWORD> THEN
C                                LAST  = 0
C
C   - C A L L S :
C        CRDELI, CREOS
C
C   - C A L L E D   B Y :
C        CRCOMM, CRIANY, CRRSEQ, CRISEQ
C
C   - R E M A R K S :
C        NO REMARKS
C
C======================================================================
C%Z
      IMPLICIT      LOGICAL(A-Z)
      CHARACTER*(*) STRING
      CHARACTER     LETTER*1, CRICHA*8, IFORM*2 , CRKW*8
      INTEGER       LENGTH  , BASE    , FIRST   , LAST   , BAS
      LOGICAL       CRDELI  , CREOS
      PARAMETER    (IFORM = 'I8')
C
      BAS    = MAX0( 1, BASE)
      FIRST  = BAS
      LAST   = 0
      LENGTH = LEN(STRING)
      IF  (BAS .LE.LENGTH) THEN
C
C------- FIND FIRST CHARACTER OF <WORD>
C
           DO 10 FIRST = BAS, LENGTH
              LETTER = STRING(FIRST:FIRST)
              IF (     CREOS (LETTER)) RETURN
              IF (.NOT.CRDELI(LETTER)) GOTO 30
   10      CONTINUE
           FIRST  = LENGTH
           RETURN
C
C------- FIND LAST CHARACTER OF <WORD> IN STRING
C
   30      DO 40 LAST = FIRST, LENGTH
              IF (CRDELI(STRING(LAST:LAST))) GOTO 50
   40      CONTINUE
           LAST  = LENGTH + 1
   50      LAST  = LAST - 1
           RETURN
      ELSE
           LAST  = 0
           FIRST = LENGTH
      ENDIF
      RETURN
      END
