
!
! EIGTST  TESTPROGRAMM ZUM UNTERPROGRAMM EIGEN                        87/12/18
!                                              (THOMAS MEUSER)
!*****************************************************************************
!                                                                            *
!     TESTPROGRAMM ZUM UNTERPROGRAMM  P 7.8  EIGEN                           *
!     BERECHNUNG ALLER EIGENWERTE UND EIGENVEKTOREN EINER MATRIX A           *
!     NACH DEM VERFAHREN VON MARTIN,PARLETT,PETERS,REINSCH UND WILKINSON.    *
!                                                                            *
!     MIT DEN VORGEGEBENEN DATEN ERHALTEN SIE AUF DER CYBER 175              *
!     DES TH-RECHENZENTRUM FOLGENDES TESTERGEBNIS:                           *
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
!*****************************************************************************
!
      SUBROUTINE EIGTST ( DMAT, ILINE, ICOLUMN, IERRIN )
!
!........1.........2.........3.........4.........5.........6.........7.|.......8 
!
! ======================================================================
!IT*  I N I T I A L I S I E R U N G S T E I L
! ======================================================================
! 
!     ISO Fortran Standard and precision definition
! 
      use precision_fem
!
!     use matrix_fem
!
!
      IMPLICIT                         NONE
!
! *** Dimensions
!
      INTEGER                          I 
	  INTEGER                          J 
      INTEGER                          N
!
! *** INTEGER
!
!     INTEGER                          CNT (ILINE)
      integer,      allocatable ::     CNT(:)
!
	  INTEGER                          EIGEN
	  INTEGER                          BASIS
	  INTEGER                          RES
	  INTEGER                          LOW
	  INTEGER                          HIGH
	  INTEGER                          IERR
!
      integer,         intent(in)  ::  iline
      integer,         intent(in)  ::  icolumn
      integer,         intent(out) ::  ierrin
!
      real(real64),    intent(in)  ::  dmat(iline, icolumn)
!
! *** Double PRECISION in new Fortran
!
!	  DOUBLE PRECISION                 WERTR(ILINE)
!	  DOUBLE PRECISION                 WERTI(ILINE)
!	  DOUBLE PRECISION                 EIVEC(ILINE, ICOLUMN)
!	  DOUBLE PRECISION                 SKAL(ILINE)
!
      real(real64),     allocatable :: A(:,:)
      real(real64),     allocatable :: WERTR(:)
      real(real64),     allocatable :: WERTI(:)
      real(real64),     allocatable :: EIVEC(:,:)
      real(real64),     allocatable :: SKAL(:)
!
! *** Field dimensions
!
      N     = ILINE
!
! *** Storage allocation of internal fields
!
      allocate (CNT(n))
      allocate (A(n,n))
      allocate (WERTR(n))
      allocate (WERTI(n))
      allocate (EIVEC(n,n))
      allocate (SKAL(n))
!
! ======================================================================
!VT*  V E R A R B E I T U N G S T E I L
! ======================================================================
!	  
!
! *** HELP Initialisierung 
!
      CALL HEINOU ( 'eigtst', 'EIN', IERR ) 
!
! *** Test, if iline .eg. icolumn (quadratic matrix is necessary)
! 
      if (iline /= icolumn) then
         ierrin = 1
         return
      end if	
!
! *** Basis der Gleitkommadarstellung (Basis = 2)
!
      BASIS = 2
	  IERR  = 0
!
! *** Copy DMAT to internal Matrix a
!
      A = DMAT
!
! *** Testausgabe
!
      CALL SYSTEM ( 'cls' )
!
         write (*,*) ' '
		 write (*,*) ' '
         write (*,*) ' Test if Input Data in EIGTST       '
         write (*,*) ' Input Data Double Precision Format '
         write (*,*) ' ---------------------------------- '
         write (*,*) ' '
!
         WRITE(*,2001)
            DO 11 I=1,N
   11          WRITE(*,2011)(DMAT(I,J),J=1,N)
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
	  WRITE (*,*) ' Programm: EIGTST'
	  WRITE (*,*) ' ================'
	  WRITE (*,*) ' TESTPROGRAMM ZUM UNTERPROGRAMM  P 7.8  EIGEN    '
      WRITE (*,*) ' BERECHNUNG ALLER EIGENWERTE UND EIGENVEKTOREN EINER' 
	  WRITE (*,*) ' MATRIX A NACH DEM VERFAHREN VON MARTIN,PARLETT,'
	  WRITE (*,*) ' PETERS,REINSCH UND WILKINSON. '
	  WRITE (*,*) ' '
	  WRITE (*,*) ' Quelle: G. Engeln-Muellges / F. Reutter'
	  WRITE (*,*) '         --'
	  WRITE (*,*) '         Formelsammlung zur numerischen Mathematik'
	  WRITE (*,*) '         mit Standard Fortran 77-Programmen'
	  WRITE (*,*) ' '
	  WRITE (*,*) ' B.I. Wissenschaftsverlag Mannheim/Wien/Zuerich'
	  WRITE (*,*) ' '
      WRITE (*,*) ' 6. Auflage 1988 '
	  WRITE (*,*) ' '
	  WRITE (*,*) ' Nur fuer kleine und gut konditionierte Matrizen '
	  WRITE (*,*) ' geeignet!'
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
! *** Integer Function for all Eigenvalues and Eigenvectors
!
!........1.........2.........3.........4.........5.........6.........7.|.......8 
!
      RES = EIGEN (BASIS, N, N, A, SKAL, EIVEC, WERTR, WERTI,
     +             CNT, LOW, HIGH)
!
!
      IF (RES .GT. 0) THEN
          RES = RES-400
!
!     AUSGABE EINER ABBRUCHMELDUNG GEMAESS RES (0/401/402/403)
!
      GOTO (30,40,50),RES
   30 write (*,*) 'FEHLER: DIE ORDNUNG DER MATRIX IST KLEINER ALS 1 !'
         call heenter
         goto 9998
   40 write (*,*) 'ABBRUCH: DIE MATRIX IST DIE NULLMATRIX !'
         call heenter
         goto 9998
   50 write (*,*) 'ABBRUCH: MAX.SCHRITTZAHL FUER QR-VERFAHREN 
     +UEBERSCHRITTEN!'
	     call heenter
	     goto 9998
      ELSE
!
!     AUSGABE DER LOESUNG
!
	  WRITE (*,*) ' '
	  WRITE (*,*) ' '
	  WRITE (*,*) '---------------------------------------------------'
	  WRITE (*,*) ' '
	  WRITE (*,*) ' Ergebnisse:'
	  WRITE (*,*) ' ==========='
	  WRITE (*,*) ' '	  
      WRITE(*,2015)
      WRITE(*,2020)(WERTR(I),WERTI(I),I=1,N)
	  call heenter
      WRITE(*,2030)
      DO 20 I=1,iline
   20 WRITE(*,2040)(EIVEC(I,J),J=1,N)
!
	  WRITE (*,*) ' '
	  WRITE (*,*) ' '
	  WRITE (*,*) '---------------------------------------------------'
	  WRITE (*,*) ' '
	  WRITE (*,*) ' '	  
!
      write (*,*) 'BERECHNUNG PROGRAMMGEMAESS BEENDET!'
	     call heenter
	     goto 9998
      ENDIF
!
! *** Kontrollausgaben der Matrix
!
 2000 FORMAT(14H TESTBEISPIEL:,/,24(1H=),/,  
     &       1X,'ZU BERECHNENDE MATRIX RMAT:',/)
 2010 FORMAT(10(1X,F12.4))
!
 2001 FORMAT(14H TESTBEISPIEL:,/,24(1H=),/,  
     &       1X,'ZU BERECHNENDE MATRIX DMAT:',/)
 2011 FORMAT(10(1X,D12.4))
!
!
!
 2015 FORMAT(/,1X,'-BERECHNETE EIGENWERTE:    REALTEIL    I  ',
     +       'IMAGINAERTEIL',/,25X,31(1H-))
 2020 FORMAT(24X,D15.5,' I',D15.5)
 2030 FORMAT(/,1X,'-MATRIX DER NORMALISIERTEN EIGENVEKTOREN:',/)
 2040 FORMAT(10(1X,D12.4))
!
 9998 CONTINUE 
!
      deallocate   (A)
      deallocate   (WERTR)
      deallocate   (WERTI)
      deallocate   (EIVEC)
      deallocate   (SKAL)
!
 9999 CONTINUE
!
      CALL HEINOU ( 'eigtst', 'AUS', IERR )
!	  
! *** Ende von eigtst
!
      RETURN
!
      END