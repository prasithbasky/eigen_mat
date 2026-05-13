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
!     MATRIX A:                                                             *
!                                                                           *
!        -2.00000D+00     2.00000D+00     2.00000D+00     2.00000D+00       *
!        -3.00000D+00     3.00000D+00     2.00000D+00     2.00000D+00       *
!        -2.00000D+00      .00000D+00     4.00000D+00     2.00000D+00       *
!        -1.00000D+00      .00000D+00      .00000D+00     5.00000D+00       *
!                                                                           *
!     GEWUENSCHTE PARAMETER:                                                *
!                                                                           *
!        MAX. 100 ITARATIONEN                                               *
!        FEHLERSCHRANKE:   .100E-13                                         *
!                                                                           *
!                                                                           *
!     LOESUNG                                                               *
!                                                                           *
!     EIGENWERT:  4.00000D+00                                               *
!                                                                           *
!     EIGENVEKTOR:                                                          *
!          5.00000D-01    5.00000D-01    5.00000D-01    5.00000D-01         *
!         STOP BERECHNUNG PROGRAMMGEMAESS BEENDET!                          *
!                                                                           *
!****************************************************************************
!
      PROGRAM MISTST
!........1.........2.........3.........4.........5.........6.........7.|.......8
!
      IMPLICIT                         NONE
!
! *** Dimensions
!
      INTEGER                          I 
	  INTEGER                          J 
      INTEGER                          N
	  INTEGER                          ND
	  INTEGER                          IDIML
	  INTEGER                          IDIMC
!
      PARAMETER                        (N     = 4)
      PARAMETER                        (ND    = N)	  
	  PARAMETER                        (IDIML = N)
	  PARAMETER                        (IDIMC = N)

!     IMPLICIT DOUBLE PRECISION (A-H,O-Z)
!
! *** INTEGER
!
	  INTEGER                          IERR
	  INTEGER                          IER
	  INTEGER                          MO
	  INTEGER                          IUNIT
	  INTEGER                          IMAT (IDIML, IDIMC) 
	  INTEGER                          ILINES
	  INTEGER                          ICOLUM
!
! *** REAL
!
      REAL                             RMAT (IDIML, IDIMC)
!
!
! *** Double PRECISION
!
      DOUBLE PRECISION                 A (ND, ND)
	  DOUBLE PRECISION                 Z (ND)
	  DOUBLE PRECISION                 X (ND)
	  DOUBLE PRECISION                 Y (ND)
!
      DOUBLE PRECISION                 MAT(ND,N)
	  DOUBLE PRECISION                 WERTR(N)
	  DOUBLE PRECISION                 WERTI(N)
	  DOUBLE PRECISION                 EIVEC(ND,N)
	  DOUBLE PRECISION                 EW
!
      DOUBLE PRECISION                 EPSI
!
	  DOUBLE PRECISION                 SKAL(N)
      DOUBLE PRECISION                 DMAT (IDIML, IDIMC)
!
! *** CHARACTER
!
      CHARACTER                        OPCODE*1
!	  
!
!  
       MO   = 100
!      MO   = 10000
	  EPSI = 1.E-14
! 
!*** HELP Initialisierung 
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
! *** Info aus Matrix-Datei lesen
!
      CALL HINFOMAT ( IUNIT, ILINES, ICOLUM, OPCODE, IERR )
!	  
! *** Einlesen des Testbeispiels aus 'test.dat' im Real Format
!
      CALL HREADMAT ( OPCODE, 'test.dat', IUNIT, 
     +                IMAT, RMAT, DMAT, IDIML, 
     +                IDIMC, ILINES, ICOLUM, IERR )
!
!
!
! *** Systembefehl absetzen
!
      CALL HECLEAR ( IERR )
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
! *** Testausgabe
! 
      WRITE (*, 321)
!
  321 FORMAT ( /
     +,/,         '    Testdaten werden von der Datei test.dat '
     +,/,         '    eingelesen ! ',  ///           )
!
! *** Real Input Format
!
      If ( OPCODE .EQ. 'F' ) THEN
	  write (*,*) ' '
	  write (*,*) ' Input Daten sind im Real Format '
	  write (*,*) ' ------------------------------- '
	  write (*,*) ' '
      call heenter	
      ELSE IF ( OPCODE .EQ. 'Y' ) THEN
!
! *** Double Precision Input Format
!
	  write (*,*) ' '
	  write (*,*) ' Input Daten sind im Double Precision Format '
	  write (*,*) ' ------------------------------------------- '
	  write (*,*) ' '
      call heenter
      ENDIF	   
!
!     write (*,*) ' '
!	  write (*,*) ' OPCODE = ', OPCODE
      write (*,*) ' '  
      write (*,*) ' IERR   = ', IERR 
      write (*,*) ' IUNIT  = ', IUNIT
      write (*,*) ' ILINES = ', ILINES	
      write (*,*) ' ICOLUM = ', ICOLUM	  	  
!
!     AUSGABE DES TESTBEISPIELS
!
!
!     Real Matrix RMAT
!
      If ( OPCODE .EQ. 'F' ) THEN
!
	  write (*,*) ' '
	  write (*,*) ' Real-Format nicht zulaessig! '
	  write (*,*) ' Programm Abbruch '
	  write (*,*) ' '
	  GOTO 9999
!
! *** Double Precision Matrix DMAT
!
      ELSE IF ( OPCODE .EQ. 'Y' ) THEN
         WRITE(*,2001)
            DO 11 I=1,ILINES
   11          WRITE(*,2011)(DMAT(I,J),J=1,ICOLUM)
      ENDIF
!
!  100 WRITE (*,2010) (A(I,J),J=1,N)
!
      WRITE (*,2020) MO,EPSI
!
         call heenter
!
! *** Aufruf des Eigenwert-Solver
!
!........1.........2.........3.........4.........5.........6.........7.|.......8
!
      If ( OPCODE .EQ. 'F' ) THEN
!
! *** Real Matrix
!
      CALL EIWERT (RMAT, N, ND, MO, EPSI, X, Y, Z, EW, IER)
!
! *** Double Precision Matrix
!
      ELSE IF ( OPCODE .EQ. 'Y' ) THEN
      CALL EIWERT (DMAT, N, ND, MO, EPSI, X, Y, Z, EW, IER)
!
      ENDIF
!
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
!     AUSGABE EINER ABBRUCHMELDUNG GEMAESS IER (0/1)
!
      IER=IER+1
!
      WRITE (*,*) ' IER   = ', IER
	  WRITE (*,*) ' '
      GOTO (30,40),IER
   30 write (*,*) 'BERECHNUNG PROGRAMMGEMAESS BEENDET!'
         goto 9998
   40 write (*,*) 'ABBRUCH,EINGABEN UEBERPRUEFEN!'
         goto 9998
!
!
 2000 FORMAT(14H TESTBEISPIEL:,/,24(1H=),/,  
     &       1X,'ZU BERECHNENDE MATRIX RMAT:',/)
 2010 FORMAT(10(1X,F12.4))
!
 2001 FORMAT(14H TESTBEISPIEL:,/,24(1H=),/,  
     &       1X,'ZU BERECHNENDE MATRIX DMAT:',/)
 2011 FORMAT(10(1X,D12.4))
!
 2020 FORMAT (/,1X,'GEWUENSCHTE PARAMETER:',//,4X,'MAX.',I4,1X,
     +'ITERATIONEN',/,4X,'FEHLERSCHRANKE:',E10.3,/)
!
 2030 FORMAT (/,1X,'EIGENWERT:',1PD15.5)
 2040 FORMAT (/,1X,'EIGENVEKTOR:',/,5(1X,1PD15.5))
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
      CALL HEINOU ( 'mistst Hauptprogramm', 'AUS', IERR )
!	  
      CALL HEENTER
!
! *** Ende von mistst
!
      STOP
!
      END