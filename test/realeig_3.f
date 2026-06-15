!
! REALEI_3  TEST Routine for the ARPACK Subroutines DNAUPD          2026/05/26
!                                              DNEUPD
!
!                                              (Axel Hänschke)
!........1.........2.........3.........4.........5.........6.........7.........8.........9........10
!*****************************************************************************
!                                                                            *
!     Test (Main) Program for the ARPACK Subroutines                         *
!                                              DNAUPD                        *
!                                              DNEUPD                        *
!                                                                            *
!     The routine calculates eigenvalues and eigenvectors of a general        *
!     real non-symmetric matrix using the Implicitly Restarted Arnoldi        *
!     Method (IRAM) from ARPACK.                                             *
!                                                                            *
!     MIT DEN VORGEGEBENEN DATEN ERHALTEN SIE AUF dem Laptop HP PROBOOK      *
!     FOLGENDES TESTERGEBNIS:                                                *
!                                                                            *
!     TESTBEISPIEL:                                                          *
!     =============                                                          *
!     ZU BERECHNENDE MATRIX:                                                 *
!                                                                            *
!       -.2000D+01    .2000D+01    .2000D+01    .2000D+01                    *
!       -.3000D+01    .3000D+01    .2000D+01    .2000D+01                    *
!       -.2000D+01    .0000D+00    .4000D+01    .2000D+01                    *
!       -.1000D+01    .0000D+00    .0000D+00    .5000D+01                    *
!                                                                            *
!     -BERECHNETE EIGENWERTE (NEV=2 groesste):  REALTEIL  I  IMAGINAERTEIL   *
!                             -------------------------------                *
!                                 .40000D+01 I     .00000D+00                *
!                                 .30000D+01 I     .00000D+00                *
!                                                                            *
!     WEITERE TESTLAEUFE SIND DURCH MODIFIZIERUNG DER WERTE IN DEN           *
!     DATA-ANWEISUNGEN JEDERZEIT MOEGLICH.                                   *
!                                                                            *
!                                                                            *
!*****************************************************************************
!
      SUBROUTINE REALEIG_3 ( CHSELECT, IERRIN )
!
!........1.........2.........3.........4.........5.........6.........7.........8.........9........10
!
      IMPLICIT                         NONE
!
! *** INTEGER
!
      INTEGER                          I
      INTEGER                          J
      INTEGER                          N
      INTEGER                          LDA
      INTEGER                          IERR
      INTEGER                          IERRIN
      INTEGER                          IUNIT
      CHARACTER                        CHSELECT*1
      INTEGER                          IDIML
      INTEGER                          IDIMC
      INTEGER                          ILINES
      INTEGER                          ICOLUM
!
! *** PARAMETER
!
      PARAMETER                        (N     = 4)
      PARAMETER                        (IDIML = N)
      PARAMETER                        (IDIMC = N)
      PARAMETER                        (LDA   = N)
!
! *** INTEGER Allocation
!
      INTEGER                          IMAT (IDIML, IDIMC)
!
! *** REAL Allocation
!
      REAL                             RMAT (IDIML, IDIMC)
!
! *** DOUBLE PRECISION Allocation
!
      DOUBLE PRECISION                 DMAT (IDIML, IDIMC)
!
!........1.........2.........3.........4.........5.........6.........7.........8.........9........10
!
      CHARACTER                        OPCODE*1
!
! *** ARPACK parameters
!
      INTEGER                          NEV
      INTEGER                          NCV
      INTEGER                          LDV
      INTEGER                          LWORKL
      INTEGER                          IDO
      INTEGER                          INFO
      INTEGER                          IPARAM(11)
      INTEGER                          IPNTR(14)
      PARAMETER                        (NEV    = 2)
      PARAMETER                        (NCV    = N)
      PARAMETER                        (LDV    = N)
      PARAMETER                        (LWORKL = 3*NCV*NCV + 6*NCV)
      LOGICAL                          RVEC
      LOGICAL                          SELECT(NCV)
      DOUBLE PRECISION                 TOL
      DOUBLE PRECISION                 SIGMAR
      DOUBLE PRECISION                 SIGMAI
      DOUBLE PRECISION                 RESID(N)
      DOUBLE PRECISION                 V(LDV, NCV)
      DOUBLE PRECISION                 WORKD(3*N)
      DOUBLE PRECISION                 WORKL(LWORKL)
      DOUBLE PRECISION                 DR(NEV+1)
      DOUBLE PRECISION                 DI(NEV+1)
      DOUBLE PRECISION                 Z(N, NEV+1)
      DOUBLE PRECISION                 WORKEV(3*NCV)
      CHARACTER                        BMAT*1
      CHARACTER                        WHICH*2
      CHARACTER                        HOWMNY*1
!
!........1.........2.........3.........4.........5.........6.........7.........8.........9........10
!
!
! *** HELP Initialisierung
!
      CALL HEINOU ( 'realeig_3', 'EIN', IERR )
!
      IUNIT   = 0
	  ILINES  = 0
      ICOLUM  = 0
      OPCODE  = ' '	  
!
! *** Info aus Matrix-Datei lesen
!
      CALL HINFOMAT ( IUNIT, 'test.dat', ILINES, ICOLUM, OPCODE, IERR )
!	  
! *** Error check
!
      If ( IERR .NE. 0 ) THEN
         write (*,*) ' '
         write (*,*) ' Wrong Format in Matrix not allowed! '
	     write (*,*) ' Input file in test.dat '
		 write (*,*) ' '
         write (*,*) ' Element at line and column = ', ILINES, ICOLUM
	     write (*,*) ' has wrong format '
         write (*,*) ' '		 
         write (*,*) ' Program Stop '
         write (*,*) ' '
		    REWIND ( UNIT = IUNIT )
      GOTO 9999
	  ENDIF
!
! *** Only Double Precision allowed
!
      If ( OPCODE .NE. 'Y' ) THEN
         write (*,*) ' '
         write (*,*) ' Wrong Format in Matrix not allowed! '
	     write (*,*) ' Input file in test.dat '
		 write (*,*) ' '
         write (*,*) ' Element at line and column = ', ILINES, ICOLUM
	     write (*,*) ' has wrong format '
         write (*,*) ' '		 
         write (*,*) ' Program Stop '
         write (*,*) ' '
		    REWIND ( UNIT = IUNIT )
      GOTO 9999
	  ENDIF
!
! *** Einlesen des Testbeispiels aus 'test.dat' im Real Format
!
      CALL HREADMAT ( OPCODE, 'test.dat', IUNIT,
     +                IMAT, RMAT, DMAT, IDIML,
     +                IDIMC, ILINES, ICOLUM, IERR )
!
!
! *** Testausgabe
!
      WRITE (*, 321)
!
  321 FORMAT ( /
     +,/,         '    Testdaten werden von der Datei test_sym.dat '
     +,/,         '    eingelesen ! ',  ///           )
!
!
! *** Double Precision Matrix DMAT output
!
         write (*,*) ' '
         write (*,*) ' Input Daten sind im Double Precision Format '
         write (*,*) ' ------------------------------------------- '
         write (*,*) ' '
!
         WRITE(*,2001)
            DO 11 I=1,ILINES
   11          WRITE(*,2011)(DMAT(I,J),J=1,ICOLUM)
         call heenter
!
! *** Systembefehl absetzen
!
      CALL SYSTEM ( 'cls' )
!
      WRITE (*,*) ' '
      WRITE (*,*) ' '
      WRITE (*,*) '---------------------------------------------------'
      WRITE (*,*) ' '
      WRITE (*,*) ' Program: REALEIG_3'
      WRITE (*,*) ' =================='
      WRITE (*,*) '    '
      WRITE (*,*) ' ARPACK DNAUPD/DNEUPD: Implicitly Restarted Arnoldi'
      WRITE (*,*) ' Method (IRAM) for general real non-symmetric '
	  WRITE (*,*) '         matrix.'
      WRITE (*,*) ' '
      WRITE (*,*) ' Computes NEV = 2 eigenvalues of largest magnitude'
      WRITE (*,*) ' (WHICH = LM) and corresponding eigenvectors.'
      WRITE (*,*) ' '
      WRITE (*,*) ' Mode 1: A*x = lambda*x  (standard eigenvalue)'
      WRITE (*,*) ' '
      WRITE (*,*) ' Source: ARPACK-NG 3.9.1'
      WRITE (*,*) '         https://github.com/opencollab/arpack-ng'
      WRITE (*,*) ' '
      WRITE (*,*) ' Suitable for large sparse matrices - standard '
	  WRITE (*,*) '          FEA.'
      WRITE (*,*) ' '
      WRITE (*,*) ' Achtung: Eingabematrix nur im Double Precision '
      WRITE (*,*) '          Format zulaessig!'
      WRITE (*,*) '          Eingabe im standard REAL Format folgt'
      WRITE (*,*) '          Programm Abbruch!!! '
      WRITE (*,*) ' '
      WRITE (*,*) '---------------------------------------------------'
      WRITE (*,*) ' '
      WRITE (*,*) ' '
!
      call heenter
!
!........1.........2.........3.........4.........5.........6.........7.........8.........9........10
!
! *** ARPACK DNAUPD/DNEUPD - Implicitly Restarted Arnoldi Method
!
! *** Initialisierung
!
      BMAT   = 'I'
      WHICH  = 'LM'
      TOL    = 0.0D0
      IDO    = 0
      INFO   = 0
      RVEC   = .TRUE.
      HOWMNY = 'A'
      SIGMAR = 0.0D0
      SIGMAI = 0.0D0
!
      IPARAM(1)  = 1
      IPARAM(2)  = 0
      IPARAM(3)  = 300
      IPARAM(4)  = 1
      IPARAM(5)  = 0
      IPARAM(6)  = 0
      IPARAM(7)  = 1
      IPARAM(8)  = 0
      IPARAM(9)  = 0
      IPARAM(10) = 0
      IPARAM(11) = 0
!
! *** Reverse Communication Interface (RCI) Loop
!
   90 CONTINUE
      CALL DNAUPD ( IDO, BMAT, N, WHICH, NEV, TOL, RESID,
     &              NCV, V, LDV, IPARAM, IPNTR, WORKD,
     &              WORKL, LWORKL, INFO )
!
      IF ( IDO .EQ. -1 .OR. IDO .EQ. 1 ) THEN
!
!        Y = A * X  (matrix-vector product)
!
         CALL DGEMV ( 'N', N, N, 1.0D0, DMAT, LDA,
     &                WORKD(IPNTR(1)), 1,
     &                0.0D0, WORKD(IPNTR(2)), 1 )
         GOTO 90
!
      END IF
!
! *** Check for DNAUPD errors
!
      WRITE(*,*) ' DNAUPD: INFO = ', INFO
      WRITE(*,*) ' DNAUPD: IPARAM(5) converged Ritz values = ',
     &             IPARAM(5)
!
      IF ( INFO .LT. 0 ) THEN
         WRITE (*,*) ' DNAUPD error: INFO = ', INFO
         GOTO 9999
      END IF
!
! *** Post-process with DNEUPD to extract eigenpairs
!
      CALL DNEUPD ( RVEC, HOWMNY, SELECT, DR, DI, Z, N,
     &              SIGMAR, SIGMAI, WORKEV, BMAT, N, WHICH,
     &              NEV, TOL, RESID, NCV, V, LDV,
     &              IPARAM, IPNTR, WORKD, WORKL, LWORKL, IERR )
!
!     AUSGABE DER LOESUNG
!
      WRITE(*,*) ' DNEUPD: IERR = ', IERR
      WRITE(*,*) ' '
!
      IF ( IERR .EQ. 0 ) THEN
      WRITE (*,*) ' '
      WRITE (*,*) ' '
      WRITE (*,*) '---------------------------------------------------'
      WRITE (*,*) ' '
      WRITE (*,*) ' Ergebnisse:'
      WRITE (*,*) ' ==========='
      WRITE (*,*) ' '
         WRITE(*,2020)
         WRITE(*,2030)(DR(I),DI(I),I=1,NEV)
!
         call heenter
      ENDIF
!
      WRITE(*,2040)
      DO 70 I=1,NEV
   70 WRITE(*,2041)(Z(J,I), J=1,N)
!
      WRITE (*,*) ' '
      WRITE (*,*) ' '
      WRITE (*,*) '---------------------------------------------------'
      WRITE (*,*) ' '
      WRITE (*,*) ' '
!
      write (*,*) 'BERECHNUNG PROGRAMMGEMAESS BEENDET!'
         goto 9998
!
!
!     Format Instructions
!
!........1.........2.........3.........4.........5.........6.........7.........8.........9........10

 2001 FORMAT(14H TESTBEISPIEL:,/,24(1H=),/,
     &       1X,'ZU BERECHNENDE MATRIX DMAT:',/)
 2011 FORMAT(10(1X,D12.4))
!
 2020 FORMAT(/,1X,'-BERECHNETE EIGENWERTE:    REALTEIL    I  ',
     +        'IMAGINAERTEIL',/,25X,31(1H-))
 2030 FORMAT(24X,D15.5,' I',D15.5)
!
 2040 FORMAT(/,1X,'-MATRIX DER NORMALISIERTEN EIGENVEKTOREN: - Z -',/)
 2041 FORMAT(10(1X,D12.4))
!
 9998 CONTINUE
!
! *** Rewind and close input file
!
            REWIND ( UNIT = IUNIT )
			CLOSE  ( UNIT = IUNIT )
!
 9999 CONTINUE
!
      CALL HEINOU ( 'realeig_3', 'AUS', IERR )
	  call heenter
!
! *** Ende von realeig_3
!
      RETURN
!
      END
