C
      SUBROUTINE COMDIV (AR,AI,BR,BI,RESR,RESI)
C
C*****************************************************************
C                                                                *
C     ZWECK DES PROGRAMMS:                                       *
C     ====================                                       *
C     KOMPLEXE DIVISION: RESR+I*RESI := (AR+I*AI)/(BR+I*BI).     *
C     (DIESE PROZEDUR SOLLTE NICHT MIT                           *
C      BR=BI=0 AUFGERUFEN WERDEN.)                               *
C                                                                *
C     EINGABEPARAMETER:                                          *
C     =================                                          *
C     AR,AI:     REAL- UND IMAGINAERTEIL DES DIVIDENDEN          *
C     BR,BI:     REAL- UND IMAGINAERTEIL DES DIVISORS            *
C                                                                *
C     AUSGABEPARAMETER:                                          *
C     =================                                          *
C     RESR,RESI: REAL- UND IMAGINAERTEIL DES QUOTIENTEN          *
C                                                                *
C     LOKALE GROESSEN:                                           *
C     ================                                           *
C     ZERO:              GLEITKOMMAKONSTANTE 0                   *
C     TEMP1,TEMP2,TEMP3: HILFSVARIABLEN ZUR SPEICHERUNG VON      *
C                        ZWISCHENERGEBNISSEN                     *
C                                                                *
C----------------------------------------------------------------*
C                                                                *
C  BENOETIGTE UNTERPROGRAMME: KEINE                              *
C                                                                *
C                                                                *
C  QUELLEN : MARTIN, R. S. UND WILKINSON, J. H., SIEHE [MART70]. *
C                                                                *
C*****************************************************************
C                                                                *
C  AUTOR     : JUERGEN DIETEL                                    *
C  DATUM     : 10.04.1987                                        *
C  QUELLCODE : FORTRAN 77                                        *
C                                                                *
C*****************************************************************
C
      DOUBLE PRECISION AR,AI,BR,BI,RESR,RESI
      DOUBLE PRECISION ZERO
      PARAMETER (ZERO = 0.0D0)
      DOUBLE PRECISION TEMP1,TEMP2,TEMP3
C
      IF (BR .EQ. ZERO .AND. BI .EQ. ZERO) THEN
         RESR = ZERO
         RESI = ZERO
         RETURN
      ENDIF
      IF (ABS(BR) .GT. ABS(BI)) THEN
         TEMP1 = BI/BR
         TEMP2 = TEMP1*BI+BR
         TEMP3 = (AR+TEMP1*AI)/TEMP2
         RESI = (AI-TEMP1*AR)/TEMP2
         RESR = TEMP3
      ELSE
         TEMP1 = BR/BI
         TEMP2 = TEMP1*BR+BI
         TEMP3 = (TEMP1*AR+AI)/TEMP2
         RESI = (TEMP1*AI-AR)/TEMP2
         RESR = TEMP3
      ENDIF
      END