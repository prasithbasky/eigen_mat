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
      SUBROUTINE EIGTST (CHSELECT, IERRIN )
!
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
!
! *** INTEGER
!
      INTEGER                          CNT (N)
	  INTEGER                          EIGEN
	  INTEGER                          BASIS
	  INTEGER                          RES
	  INTEGER                          LOW
	  INTEGER                          HIGH
	  INTEGER                          IERR
	  INTEGER                          IERRIN 
	  INTEGER                          IUNIT
	  INTEGER                          IMAT (IDIML, IDIMC) 
	  INTEGER                          ILINES
	  INTEGER                          ICOLUM
!
! *** REAL
!
      REAL                             RMAT (IDIML, IDIMC)
!
! *** Double PRECISION
!
      DOUBLE PRECISION                 MAT(ND,N)
	  DOUBLE PRECISION                 WERTR(N)
	  DOUBLE PRECISION                 WERTI(N)
	  DOUBLE PRECISION                 EIVEC(ND,N)
	  DOUBLE PRECISION                 SKAL(N)
      DOUBLE PRECISION                 DMAT (IDIML, IDIMC)
!
! *** CHARACTER
!
      CHARACTER                        OPCODE*1
	  CHARACTER                        CHSELECT*1
!
! *** Basis der Gleitkommadarstellung (Basis = 2)
!
      BASIS = 2
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
      CALL SYSTEM ( 'clear' )
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
!
!........1.........2.........3.........4.........5.........6.........7.|.......8
!
      If ( OPCODE .EQ. 'F' ) THEN
!
! *** Real Matrix
!
      RES = EIGEN (BASIS, ND, N, RMAT, SKAL, EIVEC, WERTR, WERTI,
     +             CNT, LOW, HIGH)
!
! *** Double Precision Matrix
!
      ELSE IF ( OPCODE .EQ. 'Y' ) THEN
      RES = EIGEN (BASIS, ND, N, DMAT, SKAL, EIVEC, WERTR, WERTI,
     +             CNT, LOW, HIGH)
!
      ENDIF
!
!
      IF (RES .GT. 0) THEN
          RES = RES-400
!
!     AUSGABE EINER ABBRUCHMELDUNG GEMAESS RES (0/401/402/403)
!
      GOTO (30,40,50),RES
   30 write (*,*) 'FEHLER: DIE ORDNUNG DER MATRIX IST KLEINER ALS 1 !'
         goto 9998
   40 write (*,*) 'ABBRUCH: DIE MATRIX IST DIE NULLMATRIX !'
         goto 9998
   50 write (*,*) 'ABBRUCH: MAX.SCHRITTZAHL FUER QR-VERFAHREN 
     +UEBERSCHRITTEN!'
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
      WRITE(*,2030)
      DO 20 I=1,N
   20 WRITE(*,2040)(EIVEC(I,J),J=1,N)
!
	  WRITE (*,*) ' '
	  WRITE (*,*) ' '
	  WRITE (*,*) '---------------------------------------------------'
	  WRITE (*,*) ' '
	  WRITE (*,*) ' '	  
!
      write (*,*) 'BERECHNUNG PROGRAMMGEMAESS BEENDET!'
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
      call heenter
 9999 CONTINUE
!
      CALL HEINOU ( 'eigtst Hauptprogramm', 'AUS', IERR )
!	  
      CALL HEENTER
!
! *** Ende von eigtst
!
      RETURN
!
      END