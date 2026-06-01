!
! EIGENVALUE  TEST Routine for all methods                        2026/05 /02
!
!
!                                              
!........1.........2.........3.........4.........5.........6.........7.........8.........9........10
!*****************************************************************************
!                                                                            *
!     Test (Main) Program for the all methods                                *
!                                              MISTST                        *
!                                              EIGTST                        *
!                                              REALEIG_2                     *
!                                              REALEIG_3                     *
!                                              REALEIG_4                     *
!                                              :                             *
!                                                                            *
!                                                                            *           
!                                                     (Axel Hänschke)        *
!                                                                            *
!*****************************************************************************
!
      PROGRAM EIGENVALUE
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
	  INTEGER                          IERRIN
	  INTEGER                          INFO	
      INTEGER                          IUNIT
      INTEGER                          IDIML
      INTEGER                          IDIMC	  
      INTEGER                          ILINES
      INTEGER                          ICOLUM
      INTEGER                          IUTEST
      INTEGER                          KTR	  
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
	  CHARACTER                        CHSELECT*1
!
!
!........1.........2.........3.........4.........5.........6.........7.........8.........9........10
!     
!
!      JOBVL  = 'V'
!      JOBVR  = 'V'
!
!      ILINES = N
!	  ICOLUM = N 
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
! *** Systembefehl absetzen
!
 1000 CONTINUE
!
      CALL HECLEAR ( IERR )
!
	  WRITE (*,*) ' '
	  WRITE (*,*) ' '
	  WRITE (*,*) '---------------------------------------------------'
	  WRITE (*,*) ' '
	  WRITE (*,*) ' Program: EIGENVALUE'
	  WRITE (*,*) ' =================='
	  WRITE (*,*) '    '
      WRITE (*,*) '  Program to select the Eigenvalue and Eigenvector '
	  WRITE (*,*) '  calculation method.  '
	  WRITE (*,*) '    '
      WRITE (*,*) '  Please select:'
      WRITE (*,*) '    a) Program mistst      one eigenvalue/vector   '
      WRITE (*,*) '    b) Program eigtst      all eigenvalues/vectors '
      WRITE (*,*) '    c) Program realeig_2   LAPACK DGEEV '
      WRITE (*,*) '    d) Program realeig_3   ARPACK DNAUPD nonsymmetric'
      WRITE (*,*) '    e) Program realeig_4   ARPACK DSAUPD symmetric   '
      WRITE (*,*) ' '
      WRITE (*,*) '    s) Program STOP '
	  WRITE (*,*) ' '
	  WRITE (*,*) '---------------------------------------------------'
	  WRITE (*,*) ' '
	  WRITE (*,*) ' '	
!
      WRITE (*, 323)

  323 FORMAT ( /
     +,/,         ' '
     +,/,         '    Select Input please   : ',$ )
!
      KTR = 5
!
      READ ( KTR, 1111 ) CHSELECT
!
! 
 1111 FORMAT (A1)
!
      WRITE (*,*) ' '
	  WRITE (*,*) ' CHSELECT = ', CHSELECT
	  WRITE (*,*) ' '
	  call heenter
!
! *** Selection of methods
!
!........1.........2.........3.........4.........5.........6.........7.........8.........9........10 
!
	 IF ( CHSELECT(1:1) .EQ. 'A' .OR. CHSELECT(1:1) .EQ. 'a' ) THEN
         GOTO  1001
      ELSEIF ( CHSELECT(1:1) .EQ.'B' .OR. CHSELECT(1:1) .EQ. 'b' ) THEN
         GOTO  1002
      ELSEIF ( CHSELECT(1:1) .EQ.'C' .OR. CHSELECT(1:1) .EQ. 'c' ) THEN
         GOTO  1003
      ELSEIF ( CHSELECT(1:1) .EQ.'D' .OR. CHSELECT(1:1) .EQ. 'd' ) THEN
         GOTO  1004
      ELSEIF ( CHSELECT(1:1) .EQ.'E' .OR. CHSELECT(1:1) .EQ. 'e' ) THEN
         GOTO  1005
!
      ELSEIF ( CHSELECT(1:1) .EQ.'S' .OR. CHSELECT(1:1) .EQ. 's' ) THEN
         GOTO 9999
      ELSE
         WRITE (*,*) ' '
         WRITE (*,*) ' Wrong Input parameter! '
		 WRITE (*,*) ' Please select suitable input! '
         WRITE (*,*) ' '
         CALL HEENTER
         GOTO 1000
      ENDIF
!
! *** Selection of method a)
!
 1001 CONTINUE
!
      CALL MISTST ( CHSELECT, IERRIN )
!
      GOTO 1000
!
! *** Selection of method b)
!
 1002 CONTINUE
!
      CALL EIGTST ( CHSELECT, IERRIN )
!
      GOTO 1000
!
! *** Selection of method c)
!
 1003 CONTINUE
!
      CALL REALEIG_2 ( CHSELECT, IERRIN )
!
      GOTO 1000
!
! *** Selection of method d)
!
 1004 CONTINUE
!
      CALL REALEIG_3 ( CHSELECT, IERRIN )
!
      GOTO 1000
!
! *** Selection of method e)
!
 1005 CONTINUE
!
      CALL REALEIG_4 ( CHSELECT, IERRIN )
!
      GOTO 1000
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
      CALL HEINOU ( 'eigenvalue Main Program', 'AUS', IERR )
!	  
      CALL HEENTER
!
! *** Ende von eigenvalue
!
      STOP
!
      END