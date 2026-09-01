!
! MISTST  HAUPTPROGRAMM ZUR SUBROUTINE  EIWERT                        87/09/14
!                                                ( THOMAS MEUSER )
!****************************************************************************
!                                                                           *
!     TESTPROGRAMM ZUM UNTERPROGRAMM  P 7.3 EIWERT                          *
!     BERECHNUNG DES BEITRAGSGROESSTEN EIGENWERTES UND DES DAZUGEHOERIGEN   *
!     EIGENVEKTORS EINER MATRIX A NACH DEM VERFAHREN VON VON MISES.         *
!                                                                           *
!     MIT DEN VORGEGEBENEN TESTDATEN ERHALTEN SIE AUF DER CYBER 175         *
!     DES TH-RECHENZENTRUM FOLGENDES TESTERGEBNIS:                          *
!                                                                           *
!     TESTBEISPIEL:                                                         *
!     =============                                                         *
!     MATRIX DMAT read from file                                            *
!                                                                           *
!                                                                           *
!     GEWUENSCHTE PARAMETER:                                                *
!                                                                           *
!        MAX. 100 ITARATIONEN                                               *
!        FEHLERSCHRANKE:   .100E-13                                         *
!                                                                           *
!                                                                           *
!****************************************************************************
!
      SUBROUTINE MISTST ( DMAT, ILINE, ICOLUMN, IERRIN )
!........1.........2.........3.........4.........5.........6.........7.|.......8
!
!
! ======================================================================
!IT*  I N I T I A L I S I E R U N G S T E I L
! ======================================================================
! 
!     ISO Fortran Standard and precision definition
! 
      use precision_fem
!
! *** Cannels for file output
!
      use in_out_channels
!
      IMPLICIT                         NONE
!
! *** Dimensions
!
      INTEGER                          I 
	  INTEGER                          J 
      INTEGER                          N
      INTEGER                          MO
!
! *** INTEGER
!
      INTEGER                          IER
	  INTEGER                          IERR
	  integer                          ios
      integer,         intent(in)  ::  iline
      integer,         intent(in)  ::  icolumn
      integer,         intent(out) ::  ierrin
!
!
! *** Double PRECISION in new Fortran
!
      real(real64),     allocatable :: A(:,:)
	  real(real64),     allocatable :: Z(:)
	  real(real64),     allocatable :: Y(:)
	  real(real64),     allocatable :: X(:)
!
!	  DOUBLE PRECISION                 Z (ILINE)
!	  DOUBLE PRECISION                 X (ILINE)
!	  DOUBLE PRECISION                 Y (ILINE)
!
      real(real64)                     EPSI
	  real(real64)                     EW
!
      real(real64),    intent(in)  ::  dmat(iline, icolumn)
!
      character(len=256)           ::  errmsg
!
! *** Field dimensions
!
      N     = ILINE
!
!
! *** Storage allocation of internal fields
!
      allocate (A(n,n))
	  allocate (Z(n))
	  allocate (Y(n))
	  allocate (X(n))
!
! ======================================================================
!VT*  V E R A R B E I T U N G S T E I L
! ======================================================================
!	  
!
! *** HELP Initialisierung 
! 
      CALL HEINOU ( 'mistst', 'EIN', IERR )
!
! *** Test, if iline .eg. icolumn (quadratic matrix is necessary)
! 
      if (iline /= icolumn) then
         ierrin = 1
         return
      end if	  
!
! *** von Mises parameter
! 
       IERR = 0
       MO   = 100
	   EPSI = 1.E-14
!
! *** Copy DMAT to internal Matrix a
!
      A = DMAT
!
! *** Testausgabe auf Bildschirm 
!
      CALL SYSTEM ( 'cls' )
!
         write (*,*) ' '
		 write (*,*) ' '
         write (*,*) ' Test if Input Data in MISTST       '
         write (*,*) ' Input Data Double Precision Format '
         write (*,*) ' ---------------------------------- '
         write (*,*) ' '
!
         WRITE(*,2001)
            DO 11 I=1,ILINE
   11          WRITE(*,2011)(DMAT(I,J),J=1,ICOLUMN)
         call heenter
!
!
! *** Systembefehl absetzen
!
      CALL SYSTEM ( 'cls' )
!
	  WRITE (*,*) ' '
	  WRITE (*,*) ' '
	  WRITE (*,*) '---------------------------------------------------'
	  WRITE (*,*) ' '
	  WRITE (*,*) ' Programm: MISTST'
	  WRITE (*,*) ' ================'
	  WRITE (*,*) ' TESTPROGRAMM ZUM UNTERPROGRAMM  P 7.3 EIWERT '
      WRITE (*,*) ' BERECHNUNG DES BEITRAGSGROESSTEN EIGENWERTES UND '
	  WRITE (*,*) ' DES DAZUGEHOERIGEN EIGENVEKTORS EINER MATRIX A NACH '
      WRITE (*,*) ' DEM VERFAHREN VON VON MISES. '
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
! *** Ausgabe in Output File 
!
      open (iunit_out_1, file='mistst.out', status='new', action='write', &
                         iostat=ios, iomsg=errmsg)  
!
      if (ios /= 0) then
             write(*,*) 'Error Opening the Output File:'
			 write(*,*) ' '
             write(*,*) trim(errmsg)
                goto 9998
      endif	
!                              ---        132 Columns Printout       ---
!........1.........2.........3.........4.........5.........6.........7.|.......8.........9.........1.........2.........3.........4|
	  WRITE (iunit_out_1,'(A)') ' '
	  WRITE (iunit_out_1,'(A)') ' '
	  WRITE (iunit_out_1,'(A)') '      +------------------------------------------------------------------------------------------+'
	  WRITE (iunit_out_1,'(A)') '      |                                                                                          |'
	  WRITE (iunit_out_1,'(A)') '      |                  Programm: EIGENVALUE                                                    |'
	  WRITE (iunit_out_1,'(A)') '      |                  ====================                                                    |'
	  WRITE (iunit_out_1,'(A)') '      |                                                                                          |'
	  WRITE (iunit_out_1,'(A)') '      |                  +--------------------------------------------+                          |'	  	  
	  WRITE (iunit_out_1,'(A)') '      |                  | >>>>>>      Subroutine: MISTST      <<<<<< |                          |'  
	  WRITE (iunit_out_1,'(A)') '      |                  +--------------------------------------------+                          |'
	  WRITE (iunit_out_1,'(A)') '      |                                                                                          |'
	  WRITE (iunit_out_1,'(A)') '      |                                                                                          |'	  	  
	  WRITE (iunit_out_1,'(A)') '      |                  TESTPROGRAMM ZUM UNTERPROGRAMM  P 7.3 EIWERT                            |'
      WRITE (iunit_out_1,'(A)') '      |                  BERECHNUNG DES BEITRAGSGROESSTEN EIGENWERTES UND                        |'
	  WRITE (iunit_out_1,'(A)') '      |                  DES DAZUGEHOERIGEN EIGENVEKTORS EINER MATRIX A NACH                     |'
      WRITE (iunit_out_1,'(A)') '      |                  DEM VERFAHREN VON VON MISES.                                            |'
	  WRITE (iunit_out_1,'(A)') '      |                                                                                          |'
	  WRITE (iunit_out_1,'(A)') '      |                       Quelle: G. Engeln-Muellges / F. Reutter                            |'
	  WRITE (iunit_out_1,'(A)') '      |                                                                                          |'
	  WRITE (iunit_out_1,'(A)') '      |                               Formelsammlung zur numerischen Mathematik                  |'
	  WRITE (iunit_out_1,'(A)') '      |                               mit Standard Fortran 77-Programmen                         |'
	  WRITE (iunit_out_1,'(A)') '      |                                                                                          |'
	  WRITE (iunit_out_1,'(A)') '      |                               B.I. Wissenschaftsverlag Mannheim/Wien/Zuerich             |'
	  WRITE (iunit_out_1,'(A)') '      |                                                                                          |'
      WRITE (iunit_out_1,'(A)') '      |                               6. Auflage 1988                                            |'
	  WRITE (iunit_out_1,'(A)') '      |                                                                                          |'
	  WRITE (iunit_out_1,'(A)') '      |                  Nur fuer kleine und gut konditionierte Matrizen                         |'
	  WRITE (iunit_out_1,'(A)') '      |                  geeignet!                                                               |'
      WRITE (iunit_out_1,'(A)') '      |                                                                                          |'
      WRITE (iunit_out_1,'(A)') '      |                  Achtung: Eingabematrix nur im Double Precision                          |'	  
	  WRITE (iunit_out_1,'(A)') '      |                           Format zulaessig!                                              |'	  
	  WRITE (iunit_out_1,'(A)') '      |                           Eingabe im standard REAL Format folgt                          |'
      WRITE (iunit_out_1,'(A)') '      |                           Programm Abbruch!!!                                            |'	  
	  WRITE (iunit_out_1,'(A)') '      |                                                                                          |'
	  WRITE (iunit_out_1,'(A)') '      +------------------------------------------------------------------------------------------+'
	  WRITE (iunit_out_1,'(A)') ' '
	  WRITE (iunit_out_1,'(A)') ' '		  
!
! *** Double Precision Matrix
!
      CALL EIWERT (A, N, N, MO, EPSI, X, Y, Z, EW, IER)
!
! *** Printout to screen
!
	  WRITE (*,*) ' '
	  WRITE (*,*) ' '
	  WRITE (*,*) '---------------------------------------------------'
	  WRITE (*,*) ' '
	  WRITE (*,*) ' Ergebnisse:'
	  WRITE (*,*) ' ==========='
	  WRITE (*,*) ' '	  
      WRITE (*,2030) EW
      WRITE (*,2040) (Y(I),I=1,N)
	  WRITE (*,*) ' '
	  WRITE (*,*) ' '
	  WRITE (*,*) '---------------------------------------------------'
	  WRITE (*,*) ' '
	  WRITE (*,*) ' '	  
! 
!         call heenter
!
! *** Printout to Output File
!
!........1.........2.........3.........4.........5.........6.........7.|.......8.........9.........1.........2.........3.........4|
	  WRITE (iunit_out_1,'(A)') ' '
	  WRITE (iunit_out_1,'(A)') ' '
	  WRITE (iunit_out_1,'(A)') ' '
	  WRITE (iunit_out_1,'(A)') '      +-----------------------------------------------------------------------------------------+'
	  WRITE (iunit_out_1,'(A)') ' '
	  WRITE (iunit_out_1,'(A)') '                Ergebnisse:'
	  WRITE (iunit_out_1,'(A)') '                ==========='
	  WRITE (iunit_out_1,'(A)') ' '	  
      WRITE (iunit_out_1,2030) EW
      WRITE (iunit_out_1,2040) (Y(I),I=1,N)
	  WRITE (iunit_out_1,'(A)') ' '
	  WRITE (iunit_out_1,'(A)') ' '
	  WRITE (iunit_out_1,'(A)') '      +-----------------------------------------------------------------------------------------+'
	  WRITE (iunit_out_1,'(A)') ' '
	  WRITE (iunit_out_1,'(A)') ' '	  
!
!     AUSGABE EINER ABBRUCHMELDUNG GEMAESS IER (0/1)
!
      IER=IER+1
!
!
	  WRITE (*,*) ' '
      GOTO (30,40),IER
!........1.........2.........3.........4.........5.........6.........7.|.......8.........9.........1.........2.........3.........4|
   30 write (*,*)               '                 ===== BERECHNUNG PROGRAMMGEMAESS BEENDET! =====                                 '
      write (iunit_out_1,'(A)') '                 ===== BERECHNUNG PROGRAMMGEMAESS BEENDET! =====                                 '
         goto 9998
   40 write (*,*)               '                 ===== ABBRUCH,EINGABEN UEBERPRUEFEN! =====                                      '
      write (iunit_out_1,'(A)') '                 ===== ABBRUCH,EINGABEN UEBERPRUEFEN! =====                                      '
         goto 9999
!
!
 2000 FORMAT(14H TESTBEISPIEL:,/,24(1H=),/,  &
             1X,'ZU BERECHNENDE MATRIX RMAT:',/)
 2010 FORMAT(10(1X,F12.4))
!
 2001 FORMAT(14H TESTBEISPIEL:,/,24(1H=),/,  &
             1X,'ZU BERECHNENDE MATRIX DMAT:',/)
 2011 FORMAT(10(1X,D12.4))
!
 2020 FORMAT (/,1X,'GEWUENSCHTE PARAMETER:',//,4X,'MAX.',I4,1X, &
      'ITERATIONEN',/,4X,'FEHLERSCHRANKE:',E10.3,/)
!
 2030 FORMAT (/,16X,'EIGENWERT:',1PD15.5)
 2040 FORMAT (/,16X,'EIGENVEKTOR:',/,10x,5(2X,1PD15.5))
!2040 FORMAT (/,16X,'EIGENVEKTOR:',/,5(2X,1PD15.5))
!
 9998 CONTINUE 
!
      deallocate   (A)
	  deallocate   (Z)
	  deallocate   (Y)
	  deallocate   (X)
!
      close (iunit_out_1)
!
 9999 CONTINUE
!
      CALL HEINOU ( 'mistst', 'AUS', IERR )
!	  
      CALL HEENTER
!
! *** Ende von mistst
!
      RETURN
!
      END