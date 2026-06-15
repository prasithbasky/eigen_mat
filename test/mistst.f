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
      SUBROUTINE MISTST ( CHSELECT, IERRIN )
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
	  INTEGER                          IERRIN
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
	  CHARACTER                        CHSELECT*1
!	  
!
! *** HELP Initialisierung 
! 
      CALL HEINOU ( 'mistst', 'EIN', IERR )
!  
       IERR = 0
!
       MO   = 100
!      MO   = 10000
	   EPSI = 1.E-14
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
     +,/,         '    Testdaten werden von der Datei test.dat '
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
! *** Double Precision Matrix
!
      CALL EIWERT (DMAT, N, ND, MO, EPSI, X, Y, Z, EW, IER)
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
!
	  WRITE (*,*) ' '
      GOTO (30,40),IER
   30 write (*,*) 'BERECHNUNG PROGRAMMGEMAESS BEENDET!'
         goto 9998
   40 write (*,*) 'ABBRUCH,EINGABEN UEBERPRUEFEN!'
         goto 9999
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
      CALL HEINOU ( 'mistst', 'AUS', IERR )
!	  
      CALL HEENTER
!
! *** Ende von mistst
!
      RETURN
!
      END