!
! REALEIG_2  TEST Routine for the LAPACK Subroutine DGEEV           2026/04/02
!                                              DGEQR oder DORGQR
!                                              DHSEQR
!                                              (Axel Hänschke)
!........1.........2.........3.........4.........5.........6.........7.........8.........9........10
!*****************************************************************************
!                                                                            *
!     Test (Main) Program for the LAPACK Subroutine                          *
!                                              DGEEV                         *
!                                                                            *
!                                                                            *
!     BERECHNUNG ALLER EIGENWERTE UND EIGENVEKTOREN EINER MATRIX A           *
!     NACH DEM VERFAHREN von LAPACK 3.12.1                                   *
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
!     -BERECHNETE EIGENWERTE:    REALTEIL    I  IMAGINAERTEIL                *
!                             -------------------------------                *
!                                 .10000D+01 I     .00000D+00                *
!                                 .20000D+01 I     .00000D+00                *
!                                 .30000D+01 I     .00000D+00                *
!                                 .40000D+01 I     .00000D+00                *
!                                                                            *
!     -MATRIX DER NORMALISIERTEN EIGENVEKTOREN:                              *
!                                                                            *
!        .1000D+01    .1000D+01    .1000D+01    .1000D+01                    *
!        .7500D+00    .1000D+01    .1000D+01    .1000D+01                    *
!        .5000D+00    .6667D+00    .1000D+01    .1000D+01                    *
!        .2500D+00    .3333D+00    .5000D+00    .1000D+01                    *
!       STOP  BERECHNUNG PROGRAMMGEMAESS BEENDET!                            *
!                                                                            *
!     WEITERE TESTLAEUFE SIND DURCH MODIFIZIERUNG DER WERTE IN DEN           *
!     DATA-ANWEISUNGEN JEDERZEIT MOEGLICH.                                   *                                  
!                                                                            *
!                                                                            *
!*****************************************************************************
!
      PROGRAM REALEIG_2
!
!........1.........2.........3.........4.........5.........6.........7.........8.........9........10
! 
      IMPLICIT                         NONE
!
! *** INTEGER
!
      INTEGER                          I 
	  INTEGER                          J
      INTEGER                          K
	  INTEGER                          N
	  INTEGER                          LDA
	  INTEGER                          LDVL
	  INTEGER                          LDVR
	  INTEGER                          LWORK
	  INTEGER                          IERR
	  INTEGER                          INFO	
      INTEGER                          IUNIT
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
	  PARAMETER                        (LDVL  = N)
	  PARAMETER                        (LDVR  = N)	  
 	  PARAMETER                        (LWORK = 4*N)
!
! *** INTEGER Allocation
!
      INTEGER                          IMAT (IDIML, IDIMC) 
!
! *** REAL Allocation!
! 
      REAL                             RMAT (IDIML, IDIMC)
!
! *** DOUBLE PRECISION Allocation
!
      DOUBLE PRECISION                 DMAT (IDIML, IDIMC)
!
!........1.........2.........3.........4.........5.........6.........7.........8.........9........10
!  
!     DOUBLE PRECISION,   allocatable::A(:,:)
      DOUBLE PRECISION                 A(LDA,N)
	  DOUBLE PRECISION                 VL(LDVL,N)
	  DOUBLE PRECISION                 VR(LDVR,N)
	  DOUBLE PRECISION                 WORK (LWORK)
	  DOUBLE PRECISION                 WR(N)
	  DOUBLE PRECISION                 WI(N)
!
!
      CHARACTER                        OPCODE*1
	  CHARACTER                        INOPT*1
	  CHARACTER                        OUTOPT*1
	  CHARACTER                        OPSTEUER*1
      CHARACTER                        JOBVL*1
	  CHARACTER                        JOBVR*1
!
!
!........1.........2.........3.........4.........5.........6.........7.........8.........9........10
!     
!
      JOBVL  = 'V'
      JOBVR  = 'V'
!
      ILINES = N
	  ICOLUM = N 
!
! 
! *** HELP Initialisierung 
! 
      CALL HEDEF ( IERR )	  
!
! ... Initialisierung der Help-Umgebung
!
      CALL HEINIT ( IERR )
      IF ( IERR .EQ. 1 ) THEN 
         WRITE (*, 5000)
         GOTO 9998
      ENDIF
!
!
! *** Info aus Matrix-Datei lesen
!
      CALL HINFOMAT ( IUNIT, ILINES, ICOLUM, OPCODE, IERR )
!
! *** Abfangen von Fehlern - nur Double Precision zugelassen
!
!
      OPSTEUER = 'Y'
      INOPT    = OPCODE
!
      write (*,*) ' Vor HMATERR'
      write (*,*) ' INOPT = ', INOPT
      write (*,*) ' IERR  = ', IERR
      write (*,*) ' '
      write (*,*) ' '
      write (*,*) ' '
!
      CALL HMATERR ( INOPT, OPSTEUER, OUTOPT, IERR )  
!
      write (*,*) ' Nach HMATERR'
      write (*,*) ' INOPT  = ', INOPT
      write (*,*) ' OUTOPT = ', OUTOPT
      write (*,*) ' IERR   = ', IERR
      write (*,*) ' '
      write (*,*) ' '
!
      call heenter
!
!     Fehler Ruecksprung, wenn IERR .NE. 0
!
      IF ( IERR .NE. 0 ) THEN
	     GOTO 9999
	  ENDIF
!
! *** Einlesen des Testbeispiels aus 'test.dat' im Real Format
!
      CALL HREADMAT ( OUTOPT, 'test.dat', IUNIT, 
     +                IMAT, RMAT, DMAT, IDIML, 
     +                IDIMC, ILINES, ICOLUM, IERR )
!
!
! *** Systembefehl absetzen
!
      CALL SYSTEM ( 'clear' )
!
	  WRITE (*,*) ' '
	  WRITE (*,*) ' '
	  WRITE (*,*) '---------------------------------------------------'
	  WRITE (*,*) ' '
	  WRITE (*,*) ' Program: REALEIG_2'
	  WRITE (*,*) ' =================='
	  WRITE (*,*) '    '
      WRITE (*,*) ' DGEEV computes for an N-by-N real nonsymmetric '
	  WRITE (*,*) ' matrix A, the eigenvalues and, optionally, the '
	  WRITE (*,*) ' left and/or right eigenvectors.'
      WRITE (*,*) ' '
      WRITE (*,*) ' The right eigenvector v(j) of A satisfies '
      WRITE (*,*) '      A * v(j) = lambda(j) * v(j) '
      WRITE (*,*) ' where lambda(j) is its eigenvalue.'
      WRITE (*,*) ' The left eigenvector u(j) of A satisfies '
      WRITE (*,*) '      u(j)**H * A = lambda(j) * u(j)**H'
      WRITE (*,*) ' where u(j)**H denotes the conjugate-transpose of'
	  WRITE (*,*) ' u(j). '
      WRITE (*,*) ' '
      WRITE (*,*) ' The computed eigenvectors are normalized to have'
      WRITE (*,*) ' Euclidean norm equal to 1 and largest component '
	  WRITE (*,*) ' ' 
	  WRITE (*,*) ' '
	  WRITE (*,*) ' Compute all eigenvalues and eigenvectors'
	  WRITE (*,*) ' '
	  WRITE (*,*) ' Source: LAPACK 3.12.1'
	  WRITE (*,*) '         --'
	  WRITE (*,*) ' https://www.netlib.org/lapack/explore-html/index.html '
	  WRITE (*,*) ' '
	  WRITE (*,*) ' '
	  WRITE (*,*) ' Suitable for medium matrix size - for small FEA '
	  WRITE (*,*) ' issues.'
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
! *** Testausgabe
! 
      WRITE (*, 321)
!
  321 FORMAT ( /
     +,/,         '    Testdaten werden von der Datei test.dat '
     +,/,         '    eingelesen ! ',  ///           )
!
! *** Double Precision Input Format
!
	  write (*,*) ' '
	  write (*,*) ' Input Daten sind im Double Precision Format '
	  write (*,*) ' ------------------------------------------- '
	  write (*,*) ' '
      call heenter
!
!     write (*,*) ' '
	  write (*,*) ' OUTOPT = ', OUTOPT
      write (*,*) ' '  
      write (*,*) ' IERR   = ', IERR 
      write (*,*) ' IUNIT  = ', IUNIT
      write (*,*) ' ILINES = ', ILINES	
      write (*,*) ' ICOLUM = ', ICOLUM	  	  
!
!     AUSGABE DES TESTBEISPIELS
!
!
! *** Double Precision Matrix DMAT
!
         WRITE(*,2001)
            DO 11 I=1,ILINES
   11          WRITE(*,2011)(DMAT(I,J),J=1,ICOLUM)
!
!
! *** Aufruf der Lapack Routine DGEHRD 
! 
!>
!> DGEEV computes for an N-by-N real nonsymmetric matrix A, the
!> eigenvalues and, optionally, the left and/or right eigenvectors.
!>
!> The right eigenvector v(j) of A satisfies
!>                  A * v(j) = lambda(j) * v(j)
!> where lambda(j) is its eigenvalue.
!> The left eigenvector u(j) of A satisfies
!>               u(j)**H * A = lambda(j) * u(j)**H
!> where u(j)**H denotes the conjugate-transpose of u(j).
!>
!> The computed eigenvectors are normalized to have Euclidean norm
!> equal to 1 and largest component real.
!> 
!
!     [in]
!>          JOBVL is CHARACTER*1
!>          = 'N': left eigenvectors of A are not computed;
!>          = 'V': left eigenvectors of A are computed.
!> 
!
!     [in]
!>          JOBVR is CHARACTER*1
!>          = 'N': right eigenvectors of A are not computed;
!>          = 'V': right eigenvectors of A are computed.
!> 
!
!     [in]
!>          N is INTEGER
!>          The order of the matrix A. N >= 0.
!> 
!
!     [in,out]
!>          A is DOUBLE PRECISION array, dimension (LDA,N)
!>          On entry, the N-by-N matrix A.
!>          On exit, A has been overwritten.
!> 
!
!     [in]
!>          LDA is INTEGER
!>          The leading dimension of the array A.  LDA >= max(1,N).
!
!     [out]
!>          WR is DOUBLE PRECISION array, dimension (N)
!> 
!
!     [out]
!>          WI is DOUBLE PRECISION array, dimension (N)
!>          WR and WI contain the real and imaginary parts,
!>          respectively, of the computed eigenvalues.  Complex
!>          conjugate pairs of eigenvalues appear consecutively
!>          with the eigenvalue having the positive imaginary part
!>          first.
!> 
!
!     [out]
!>          VL is DOUBLE PRECISION array, dimension (LDVL,N)
!>          If JOBVL = 'V', the left eigenvectors u(j) are stored one
!>          after another in the columns of VL, in the same order
!>          as their eigenvalues.
!>          If JOBVL = 'N', VL is not referenced.
!>          If the j-th eigenvalue is real, then u(j) = VL(:,j),
!>          the j-th column of VL.
!>          If the j-th and (j+1)-st eigenvalues form a complex
!>          conjugate pair, then u(j) = VL(:,j) + i*VL(:,j+1) and
!>          u(j+1) = VL(:,j) - i*VL(:,j+1).
!> 
!
!     [in]
!>          LDVL is INTEGER
!>          The leading dimension of the array VL.  LDVL >= 1; if
!>          JOBVL = 'V', LDVL >= N.
!> 
!
!     [out]
!>          VR is DOUBLE PRECISION array, dimension (LDVR,N)
!>          If JOBVR = 'V', the right eigenvectors v(j) are stored one
!>          after another in the columns of VR, in the same order
!>          as their eigenvalues.
!>          If JOBVR = 'N', VR is not referenced.
!>          If the j-th eigenvalue is real, then v(j) = VR(:,j),
!>          the j-th column of VR.
!>          If the j-th and (j+1)-st eigenvalues form a complex
!>          conjugate pair, then v(j) = VR(:,j) + i*VR(:,j+1) and
!>          v(j+1) = VR(:,j) - i*VR(:,j+1).
!> 
!
!     [in]
!>          LDVR is INTEGER
!>          The leading dimension of the array VR.  LDVR >= 1; if
!>          JOBVR = 'V', LDVR >= N.
!> 
!
!     [out]
!>          WORK is DOUBLE PRECISION array, dimension (MAX(1,LWORK))
!>          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
!> 
!
!     [in]
!>          LWORK is INTEGER
!>          The dimension of the array WORK.  LWORK >= max(1,3*N), and
!>          if JOBVL = 'V' or JOBVR = 'V', LWORK >= 4*N.  For good
!>          performance, LWORK must generally be larger.
!>
!>          If LWORK = -1, then a workspace query is assumed; the routine
!>          only calculates the optimal size of the WORK array, returns
!>          this value as the first entry of the WORK array, and no error
!>          message related to LWORK is issued by XERBLA.
!> 
!
!     [out]
!>          INFO is INTEGER
!>          = 0:  successful exit
!>          < 0:  if INFO = -i, the i-th argument had an illegal value.
!>          > 0:  if INFO = i, the QR algorithm failed to compute all the
!>                eigenvalues, and no eigenvectors have been computed;
!>                elements i+1:N of WR and WI contain eigenvalues which
!>                have converged.
!> 
!
!........1.........2.........3.........4.........5.........6.........7.........8.........9........10
!
!
!
      call heenter
!
!     Real Matrix RMAT
!
!      If ( OPCODE .EQ. 'F' ) THEN
!      call DGEEV ( JOBVL, JOBVR, N, RMAT, LDA, WR, WI, VL, LDVL, 
!     &             VR, LDVR, WORK, LWORK, INFO )
!
! *** Double Precision Matrix DMAT
!
!      ELSE IF ( OPCODE .EQ. 'Y' ) THEN
      call DGEEV ( JOBVL, JOBVR, N, DMAT, LDA, WR, WI, VL, LDVL, 
     &             VR, LDVR, WORK, LWORK, INFO )
!      ENDIF
!
!     AUSGABE DER LOESUNG
!
! *** Ausgabe der Eigenwerte und Eigenvektoren
!
       WRITE(*,*) ' INFO    = ', INFO
       WRITE(*,*) ' '
!
       IF ( INFO .EQ. 0 ) THEN
	  WRITE (*,*) ' '
	  WRITE (*,*) ' '
	  WRITE (*,*) '---------------------------------------------------'
	  WRITE (*,*) ' '
	  WRITE (*,*) ' Ergebnisse:'
	  WRITE (*,*) ' ==========='
	  WRITE (*,*) ' '	  
          WRITE(*,2020)
          WRITE(*,2030)(WR(I),WI(I),I=1,N)
!
          call heenter
	   ENDIF
!
      WRITE(*,2040)
      DO 70 I=1,N
   70 WRITE(*,2041)(VL(J,I), J=1,N)
!
!      write (*,*) VL
!
      WRITE(*,2050)
      DO 71 I=1,N
   71 WRITE(*,2051)(VR(J,I), J=1,N)
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

 2000 FORMAT(14H TESTBEISPIEL:,/,24(1H=),/,  
     &       1X,'ZU BERECHNENDE MATRIX RMAT:',/)
 2010 FORMAT(10(1X,F12.4))
!
 2001 FORMAT(14H TESTBEISPIEL:,/,24(1H=),/,  
     &       1X,'ZU BERECHNENDE MATRIX DMAT:',/)
 2011 FORMAT(10(1X,D12.4))
!
 2020 FORMAT(/,1X,'-BERECHNETE EIGENWERTE:    REALTEIL    I  ',
     +        'IMAGINAERTEIL',/,25X,31(1H-))
 2030 FORMAT(24X,D15.5,' I',D15.5)
!
! 
! 
 2040 FORMAT(/,1X,'-MATRIX DER NORMALISIERTEN EIGENVEKTOREN: - VL -',/)
 2041 FORMAT(10(1X,D12.4))
!
 2050 FORMAT(/,1X,'-MATRIX DER NORMALISIERTEN EIGENVEKTOREN: - VR -',/)
 2051 FORMAT(10(1X,D12.4))
!
 5000 FORMAT ( ' ', //
     +,/,'   ***********************************************************
     +**'
     +,/,'   *
     + *'
     +,/,'   *      Progranm Stop Neustart
     + *'
     +,/,'   *
     + *'
     +,/,'   *
     + *'
     +,/,'   ***********************************************************
     +**', ///
     + )
!
 9998 CONTINUE 
!      call heenter
 9999 CONTINUE
!
      CALL HEINOU ( 'realeig_2 Hauptprogramm', 'AUS', IERR )
!	  
      CALL HEENTER
!
! *** Ende von realeig_2
!
      STOP
!
      END