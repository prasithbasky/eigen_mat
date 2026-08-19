!
!*****7**1*********2*********3*********4*********5*********6*********7**
!
      SUBROUTINE HDMAT ( CHFILE, ILINE, ICOLUMN, IERRALLOC, IERR  )
!
!
!
!B*   Name = hreadfem
!            --------
!
!
!D*   PROGRAMM-SYSTEM: Fortran-Test Umgebung für OpenRadioss
!     ---------------  Implicit Solver Tests
!
!E*   FUNKTION: Vorbereitung zum Einlesen einer Matrix (einfach) 
!     --------
!               Es werden folgende Daten erwartet
!               gesteuert über CHFILE (siehe Eingabebeschreibung).
!
!               Die Routine öffner und schliesst die selektierte  
!               Datei. 
!               
!               
!
!S*   PROGRAMMIER-SPRACHE: FORTRAN 90
!     --------------------
!
!
!F*   EINGABE:
!     -------
!
!     - OPCODE     Einlesesteuerung
!                  CHFILE = 'N', nicht symmetrische Matrix
!                           Beispiel: "test_new.dat"
!                  CHFILE = 'S', symmetrische Matrix
!                           Beispiel: "test_sym_new.dat"
!                  CHFILE = 'G', generelle Matrix 
!                           Beispiel: test_gen_new.dat" 
!
! *** OPCODES to read from any file and convert to the requested input files
!
!                  CHFILE = 'U', reads non symetric matrix from any file 
!                            and creates the input file "test_new.dat"
!                            the file name will be prompted
!                  CHFILE = 'V', reads symemtric matrix from any file 
!                            and creates the input file "test_sym_new.dat"
!                            the file name will be prompted
!                  CHFILE = 'W', reads the sparse matrix M (mass) and K 
!                           (stiffness) and transform both to full symmetrc
!                           format. Creates the input files 
!                           mass_full.dat and stiffness_full.dat.
!                  CHFILE = 'X', , reads the sparse matrix M (mass) and K 
!                           (stiffness) and creates the input files 
!                            mass_sparse.dat and stiffness_sparse.dat 
! 
!
!G*   AUSGABE:
!     -------
!
!     - ILINE      Anzahl der aktuellen Zeilen der Matrix
!     - ICOLUMN    Anzahl der aktuellen Spalten der Matrix

!     - IERRALLOC  Fehler beim allokieren des Speicherplatzes, 
!                  IERRALLOC /= 0,
!
!     - IERR       Fehler-Return Code
!                  Bisher besetzt:
!                  IERR =   0, d.h. kein Fehler beim Lesen
!                  IERR =  10, d.h. End of file beim Lesen von Datei
!                  IERR = 100, String ist leer - Leerzeile
!                  IERR = 900, Unzulässiger Wert für Matrix-Element 
!                              passt nicht zum gewählten OPCODE
!
!
!
!H*   INTERNE:
!     -------
!
!     - DMAT       eingelesene Matrix der Dimension (DMAT (iline,Icolumn))
!                  wird über modul "matrix_fem" übergeben!
!
!
!I*   MODULs:
!     ------
!  
!     - precision_fem
!     - matrix_fem
!
!
!J*   BENOETIGTE UNTERPROGRAMME / FUNKTIONEN:
!     --------------------------------------
!
!     - SUBROUTINE HEENTER
!     - SUBROUTINE HECLEAR
!     - SUBROUTINE HEOPEN
!     - SUBROUTINE HECLOSE
!     - SUBROUTINE HREADFEM
!
!L*   DIESE ROUTINE WIRD AUFGERUFEN VON:
!     ---------------------------------
!
!     - Main       EIGENVALUE
!
!
!M*   ACHTUNG:     Die eingelesenen Matrizen-Dateien müssen die folgende    
!     -------      Form aufweisen
!
!                  ILINE (Anzahle der Zeilen)
!                  ICOLUMN (Anzahl der Spalten)
!                  (n x n ) Matrix-Elemente
!                
!                  Die Matrix muss quqdratische sein.
!       
!      
!                  Die Help-Routinen arbeiten nur nach Initialisierung
!                  der Help-Umgebung und Anlegen der Datei HEINIT.DAT.
!                  Diese Datei wird, falls sie nicht vorhanden ist, wird
!                  sie auf dem Verzeichnis erstellt, auf dem die Anwendung
!                  aufgerufen wird. Falls die Datei vorhanden ist
!                  kann diese den Beduerfnissen Angepasst werden.
!                  Wenn prinzipiell nicht mit der Initialisierungsdatei
!                  gearbeitet werden soll, dann das Character-Help
!                  System mit der Default-Routine initialisieren.
!
!
!
!
!N*   BEMERKUNG:   Das Hauptprogramm stellt eine jeweils angepasste
!     ---------    Umgebung fuer Character-Interpretationen dar.
!
!                  Weitere Unterprogramme der Library 'libhelp.a'
!                  sind in einem Doku-Ordner beschrieben.
!
!                  Nach Initialisierung der Help-Umgebung und Anlegen
!                  der HEINIT.DAT Datei wird das Programm abgeschlossen.
!                  Das ist nur beim ersten Programmlauf der Fall.
!                  Das Programm muss dann nochmals gestartet werden.
!                  Beim Arbeiten mit den Default-Werten ist das
!                  nicht der Fall.
!
!
!
!O*   AUTOR:       Axel Haenschke
!     -----        Firma
!                  Anschrift der Firma
!                  Abteilung:
!
!
!P*   VERSION:     0.1                                     26.07.2026
!     --------     
!                  
!                  
!                  
!            
! ----------------------------------------------------------------------
!
!R*   LITERATUR:   20
!     ---------
!
!*****7**1*********2*********3*********4*********5*********6*********7**
!
!
! ======================================================================
!VD*  V A R I A B L E N   D E K L A R A T I O N
! ======================================================================
!
!
! 
!     ISO Fortran Standard and precision definition
! 
      use precision_fem
!
      use matrix_fem
!
      IMPLICIT                         NONE
!
! ----------------------------------------------------------------------
!
      INTEGER      I
	  INTEGER      J
      INTEGER      IUNIT
      INTEGER      IERR
	  INTEGER      IERR1
	  INTEGER      IERR2
	  INTEGER      IERRHE
	  INTEGER      IERRALLOC
	  INTEGER      ILINE
	  INTEGER      ICOLUMN
!
! ----------------------------------------------------------------------
!
!          real (dp), allocatable :: DMAT(:,:)
!          real (dp), allocatable :: M(:,:)
!		   real (dp), allocatable :: K(:,:)
!
! ----------------------------------------------------------------------
!
      CHARACTER    CHFILE*1
!
! ======================================================================
!
!
! ======================================================================
!IT*  I N I T I A L I S I E R U N G S T E I L
! ======================================================================
!
      CALL HEINOU ( 'HREADFEM', 'EIN', IERR1 )
!
!
! ======================================================================
!EV*  E I N G A B E V E R A R B E I T U N G
! ======================================================================
!      
      IERR1      = 0
	  IERR2      = 0
!     IERR       = 0
	  IERRALLOC  = 0
!
! ======================================================================
!VT*  V E R A R B E I T U N G S T E I L
! ======================================================================
!	  	  
! *** Einlesen der Dimensionen der Matrix iline, icolumn und der 
!     Matrix DMAT
!
    1 CONTINUE
!
! *** CHFILE = N,  non symmetric matrix
!
      IF ( CHFILE .EQ. 'N' .OR. CHFILE .EQ. 'n' ) THEN
!        
          call heopen ( 'K', 'N', 'test_new.dat', IUNIT, IERR )
          IF (IERR  /= 0) THEN 
!                      Fehler - muss in EIGENVALUE abgefragt werden.
		     goto 9999
          ENDIF
!
	      call hreadfem ( 'D', IUNIT, ILINE, ICOLUMN, IERRALLOC, IERR )
!
!
! *** Error handling ILINE /= ICOLUMN - IERR = 10, IERRALLOC /0 0
!
          IF ( IERR .EQ. 10 ) then 
		      goto 9997
		  ENDIF
!
              IF ( IERRALLOC /= 0 ) THEN
!
 				   goto 9998
!                  deallocate und Fehlerabfangen in EIGENVALUE
 		     ENDIF
!
	      call hreadfem ( 'M', IUNIT, ILINE, ICOLUMN, IERRALLOC, IERR )
!
             IF ( IERRALLOC /= 0 ) THEN
!
				   goto 9998
!                  deallocate und Fehlerabfangen in EIGENVALUE
		     ENDIF
             goto 9999           			 
!
! *** CHFILE = S, symmetric matrix
!
      ELSE IF ( CHFILE .EQ. 'S' .OR. CHFILE .EQ. 's' ) THEN
! 
          call heopen ( 'K', 'N', 'test_sym_new.dat', IUNIT, IERR )
          IF (IERR  /= 0) THEN 
!                      Fehler - muss in EIGENVALUE abgefragt werden.
		     goto 9999
          ENDIF
!
	      call hreadfem ( 'D', IUNIT, ILINE, ICOLUMN, IERRALLOC, IERR )
!
! *** Error handling ILINE /= ICOLUMN - IERR = 10
!
          IF ( IERR .EQ. 10 ) then 
		      goto 9997
		  ENDIF
!
	      call hreadfem ( 'M', IUNIT, ILINE, ICOLUMN, IERRALLOC, IERR )
!
             IF ( IERRALLOC /= 0 ) THEN
!
				   goto 9998
!                  deallocate und Fehlerabfangen in EIGENVALUE
		     ENDIF	
			 goto 9999
!
! *** CHFILE = G, general matrix
!
      ELSE IF ( CHFILE .EQ. 'G' .OR. CHFILE .EQ. 'g' ) THEN
! 
          call heopen ( 'K', 'N', 'test_general_new.dat', IUNIT, IERR )
          IF (IERR  /= 0) THEN
!                      Fehler - muss in EIGENVALUE abgefragt werden.
		     goto 9999
          ENDIF
!
	      call hreadfem ( 'D', IUNIT, ILINE, ICOLUMN, IERRALLOC, IERR )
!
	      call hreadfem ( 'M', IUNIT, ILINE, ICOLUMN, IERRALLOC, IERR )
!
             IF ( IERRALLOC /= 0 ) THEN
!
				   goto 9998
!                  deallocate und Fehlerabfangen in EIGENVALUE
		     ENDIF
			 goto 9999
	  ENDIF			  
!
! ======================================================================
!AT*  A U S G A B E T E I L
! ======================================================================
!
 9997 CONTINUE
!
      write (*,*) ' '
      write (*,*) ' ==================================================='
      write (*,*) ' '
      write (*,*) ' '
      write (*,*) ' ILINE .NE. ICOLUMN  '
      write (*,*) ' DMAT not quadratic form!'
	  write (*,*) ' Check input file! '
      write (*,*) ' '
	  write (*,*) '                        Program stop! '
      write (*,*) ' '
      write (*,*) ' ==================================================='
      write (*,*) ' '	
!
      call heenter
      goto 9999	  
!
 9998 CONTINUE
!
      write (*,*) ' '
      write (*,*) ' ==================================================='
      write (*,*) ' '
      write (*,*) ' '
      write (*,*) ' Fehler beim Allokieren des Speichers  '
      write (*,*) ' DMAT zu gross!!'
      write (*,*) ' '
	  write (*,*) ' Programm wird nicht weiter ausgefuehrt!!! - STOP '
      write (*,*) ' '
      write (*,*) ' ==================================================='
      write (*,*) ' '
!
      call heenter	  
!
!
! ======================================================================
!FA*  F O R M A T  - A N W E I S U N G E N
! ======================================================================
!
 9999 CONTINUE
!
      call heclose ( IUNIT, IERR2 )
!
      CALL HEINOU ( 'HDMAT', 'AUS', IERR1 )
!
!	  
! *** Ende der Subroutine HREADFEM
!
      RETURN
!
      END
