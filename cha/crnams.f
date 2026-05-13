      SUBROUTINE CRNAMS (OPCODE, NAME)
C***********************************************************************
C
C     NAME    : CRNAMS  FORTRAN                   CREATED: 15 FEB 1985
C     PROJECT : CHARACTER ROUTINES 2.0
C     AUTHOR  : Axel Haenschke
C
C   - F U N C T I O N :
C       INTERFACE TO LIST OF SUBROUTINE NAMES TO BE TRACED
C
C   - I N P U T :
C       OPCODE  : CHAR*(*)  OPCODE
C                           'SET' : SET NAME ON LIST
C                           'DEL' : DELETE NAME FROM LIST
C                           'CLR' : CLEAR LIST
C                           'INQ' : INQUIRE NUMBER OF FREE ENTRIES
C                           'DISP': DISPLAY CURRENT CONTENT OF NAMELIST
C                                   VIA 'CRMSG'
C
C   - T W O   W A Y   P A R A M E T E R S:
C       NAME    : CHAR*(*)  'SET' : I : NAME OF SUBROUTINE TO BE TRACED
C                           'DEL' : I : DELETE NAME FROM LIST
C                           'CLR' : DON'T CARE
C                           'INQ' : O : NAME(1:2) CONTAINS NUMBER OF
C                                       FREE ENTRIES IN I2 FORMAT
C                           'DISP': DON'T CARE
C
C   - C O M M O N - B L O C K S :
C       CRCBFL = CHARO COMMON FLAGS, IN CRTRAC CRSWIC CRNAMS CRPROT
C       CRCBNA = CHARO COMMON NAMES, IN CRTRAC CRSWIC CRNAMS
C
C   - C A L L S :
C       CRMSG , CRKW
C
C   - C A L L E D   B Y :
C       CRDIAG
C
C   - R E M A R K S :
C     > THE ROUTINE OPERATES ONLY AFTER CRSWIC HAS BEEN CALLED
C     > THE FIRST ENTRY IN NAMELIST IS 'MAIN'. 'MAIN' CANNOT BE DELETED
C       FROM THE LIST .
C     > 'INQ' : MAKE SHURE THAT 'NAME' CAN HOLD TWO CHARACTERS
C     > LENGTH MUST BE A MULTIPLE OF '5'
C
C                                                  UPDATE: 15 Feb 1985
C**********************************************************************
      IMPLICIT      LOGICAL(A-Z)
      INTEGER       LENGTH
      PARAMETER   ( LENGTH = 20 )
      COMMON      / CRCBFL / NOTRA1, NOTRA2, ALTRAC, TRAMOD, POINTR,
     1                       PROT  , IBREAK, NBREAK
      COMMON      / CRCBNA / RNAMES
      CHARACTER*6   RNAMES (LENGTH), LOCNAM
      CHARACTER*8   CRKW
      CHARACTER*(*) OPCODE, NAME
      LOGICAL       ALTRAC, TRAMOD, PROT
      INTEGER       NOTRA1, NOTRA2, POINTR, I     , J     , K     ,
     1              IBREAK, NBREAK
C
C------------ OPERATE ONLY IF 1..POINTR..LENGTH
C
      IF       (POINTR.LT.1.OR.POINTR.GT.LENGTH) RETURN
      IF       (OPCODE.EQ.'SET') THEN
                LOCNAM = NAME
                DO 10 I = 1, POINTR
                   IF  (LOCNAM.EQ.RNAMES(I)) RETURN
   10           CONTINUE
                IF  (POINTR.LT.LENGTH) THEN
                     POINTR         = POINTR + 1
                     RNAMES(POINTR) = NAME
                ENDIF
      ELSE IF  (OPCODE.EQ.'DEL') THEN
                LOCNAM = NAME
                DO 20 I = 1, POINTR
                   IF  (LOCNAM.EQ.RNAMES(I).AND.I.NE.1) THEN
                        IF  (I.NE.POINTR) THEN
                             RNAMES (I) = RNAMES(POINTR)
                        ELSE
                             RNAMES (I) = ' '
                        ENDIF
                        POINTR = POINTR - 1
                        RETURN
                   ENDIF
   20           CONTINUE
      ELSE IF  (OPCODE.EQ.'CLR') THEN
                POINTR = 1
                DO 22 I = 2, LENGTH
   22              RNAMES(I) = ' '
      ELSE IF  (OPCODE.EQ.'INQ') THEN
                I = LENGTH - POINTR
                WRITE (NAME(1:2),'(I2)') I
      ELSE IF  (OPCODE.EQ.'DISP') THEN
                IF (POINTR.LT.LENGTH.AND.POINTR.GT.1) THEN
                    K = POINTR + 1
                    DO 30 I = K, LENGTH
                       RNAMES(I) = ' '
   30               CONTINUE
                ENDIF
                CALL CRMSG ( 0, 'CRNAMS : LIST OF NAMES IS :')
                CALL CRMSG ( 9, ' ')
                I = 0
                K = LENGTH/5
                DO 40 J = 1, K
                   CALL CRMSG(0, RNAMES(I+1)//'  '//RNAMES(I+2)//'  '//
     1                           RNAMES(I+3)//'  '//RNAMES(I+4)//'  '//
     2                           RNAMES(I+5))
                   I = I + 5
                   IF (I.GE.POINTR) GOTO 50
   40           CONTINUE
   50           CALL CRMSG(-9,' ')
      ELSE
                CALL CRMSG ( 0,'CRNAMS :'//CRKW(27)//' '''//
     1                          OPCODE(1:)//''' '//CRKW(21))
      ENDIF
      RETURN
      END
