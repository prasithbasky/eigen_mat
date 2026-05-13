      PROGRAM QTEST
C     TESTEN ALLER Q-BASIC-MODULE

      CHARACTER * 50 CHTEST, QLTRIM, QRTRIM, QUCASE, QLCASE

      WRITE ( *,   *  ) 'Test-String:> '
      READ  ( *, '(A)') CHTEST

      WRITE ( *, * ) '>>>', CHTEST,'<<<'

      CHTEST = QRTRIM ( CHTEST )
      WRITE ( *, * ) '>>>', CHTEST,'<<<'

      CHTEST = QLTRIM ( CHTEST )
      WRITE ( *, * ) '>>>', CHTEST,'<<<'

      CHTEST = QRTRIM ( CHTEST )
      WRITE ( *, * ) '>>>', CHTEST,'<<<'

      CHTEST = QLCASE ( CHTEST )
      WRITE ( *, * ) '>>>', CHTEST,'<<<'

      CHTEST = QUCASE ( CHTEST )
      WRITE ( *, * ) '>>>', CHTEST,'<<<'



      END
