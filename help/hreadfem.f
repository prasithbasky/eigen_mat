!
!*****7**1*********2*********3*********4*********5*********6*********7**
!
      SUBROUTINE HREADFEM ( OPCODE, IUNIT,
     +                      ILINE, ICOLUMN, IERRALLOC, IERR  )
!
!
!
!B*   Name = hreadfem
!            --------
!
!
!D*   PROGRAMM-SYSTEM: beliebig
!     ---------------
!
!E*   FUNKTION: Einlesen einer Matrix (einfach) 
!     --------
!               Es werden folgende Daten erwartet:
!      
!               READ iline    (Anzahl der Zeilen)
!               READ icolumn  (Anzahl der Spalten)
!
!               Die Routine setzt voraus, dass die Datei vorhanden 
!               und geöffner ist!!
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
!                  OPCODE = "D", einlesen der Dimensionen iline, ICOLUMN
!                  OPCODE = "M", einlesen der Matrixwerte in einer Schleife
!     - IUNIT      Kanalnummer der Datei von der gelesen werden soll.
!
!G*   AUSGABE:
!     -------
!
!     - ILINE      Anzahl der aktuellen Zeilen der Matrix
!     - ICOLUMN    Anzahl der aktuellen Spalten der Matrix
!     - DMAT       eingelesene Matrix der Dimension (DMAT (iline,Icolumn))
!     - IERRALLOC  Fehler beim allokieren des Speicherplatzes, 
!                  IERRALLOC /= 0,
!
!     - IERR       Fehler-Return Code
!                  Bisher besetzt:
!                  IERR =   0, d.h. kein Fehler beim Lesen
!                  IERR =  10, d.h. ILINE .NE. ICOLUMN, Matrix nicht
!                              quadratisch, Abbruch
!                  IERR = 100, String ist leer - Leerzeile
!                  IERR = 900, Unzulässiger Wert für Matrix-Element 
!                              passt nicht zum gewählten OPCODE
!
!
!
!H*   INTERNE:
!     -------
!
!     - sind hier nicht im einzelnen beschrieben, reine Faulheit.
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
!     - FUNCTION   count_columns - Integer function
!
!L*   DIESE ROUTINE WIRD AUFGERUFEN VON:
!     ---------------------------------
!
!     - SUBROUTINE Name
!
!
!M*   ACHTUNG:     Die Help-Routinen arbeiten nur nach Initialisierung
!     -------      der Help-Umgebung und Anlegen der Datei HEINIT.DAT.
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
!P*   VERSION:     0.1                                     03.08.94
!     -------
!
!
!Q*   UPDATE:      Axel Haenschke                          24.02.95
!     -------      Anpassung an IBM-RS6000
!                  Compiled for Windows                    20.01.2023
!                  Entwicklungsumgebung Ein/Ausgabe        03.03.2026
!                  für Open Radioss
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
	  INTEGER      n
	  INTEGER      ios
	  INTEGER      count_columns
      INTEGER      IUNIT
      INTEGER      IERR
	  INTEGER      IERR1
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
      CHARACTER                   OPCODE*1
	  CHARACTER                   chcomm*1
	  character(len=1000)    ::   chline
!
! ======================================================================
!
      IERR       = 0
      IERRALLOC  = 0
!
! ======================================================================
!IT*  I N I T I A L I S I E R U N G S T E I L
! ======================================================================
!
      CALL HEINOU ( 'HREADFEM', 'EIN', IERR )
!
!
! ======================================================================
!EV*  E I N G A B E V E R A R B E I T U N G
! ======================================================================
!	  
! *** Einlesen der Dimensionen der Matrix iline, icolumn
!
    1 CONTINUE
!
      IF ( OPCODE .EQ. 'D' ) THEN
!          read	(iunit, *) ILINE
!          read  (iunit, *) ICOLUMN
!
!-----------------------------------------
! *** Count number of rows
!-----------------------------------------
!
      iline = 0
!
      do
           read(iunit,'(A)',iostat=ios) chline
		   call hecomm (chline, chcomm)
		      if (chcomm .eq. 'N' ) THEN
			     goto 70
			  else if (chcomm .eq. 'J' ) THEN
			     goto 71
			  endif			     
!-----------------------------------------
! *** count number of lines
!-----------------------------------------
   70 CONTINUE 
            if (ios /= 0) exit
!
            if (len_trim(chline) > 0) then
               iline = iline + 1
            end if
   71 CONTINUE
!
      end do
!
! 		   write (*,*) ' iline    = ', iline
!		   call heenter
!
      rewind(iunit)
!-----------------------------------------
! *** count number of columns
!-----------------------------------------
   72 CONTINUE
       read(iunit,'(A)') chline
		   call hecomm (chline, chcomm)
		      if (chcomm .eq. 'N' ) THEN
			     goto 73
			  else if (chcomm .eq. 'J' ) THEN
			     goto 72
			  endif			     	   
!
! ***  Count number of entries in line
!
   73 CONTINUE
       icolumn = count_columns (chline)
!
       rewind(IUNIT)
!
! *** check, if matrix is quadratic
!
!
          write (*,*) ' '
 		  write (*,*) ' in hreadfem.f '
 		  write (*,*) ' '
 		  write (*,*) ' ILINE       = ', ILINE
 		  write (*,*) ' ICOLUMN     = ', ICOLUMN
 		  write (*,*) ' '
 		  call heenter
 
		  if (ILINE .NE. ICOLUMN) THEN
		     IERR = 10
		     goto 9999
		  endif
!
!         Matric dynamisch reservieren
!
	      allocate (DMAT(ILINE, ICOLUMN), stat = IERRALLOC )
!
            write (*,*) ' IERRALLOC   = ', IERRALLOC 
            Write (*,*) ' =========================='
            Write (*,*) ' '
             IF ( IERRALLOC /= 0 ) THEN
				   goto 9999
		     ENDIF
!
!
      ENDIF
!
! ======================================================================
!VT*  V E R A R B E I T U N G S T E I L
! ======================================================================
!
!
! *** Allokieren des Speichers und Einlesen der Matrix-Elemente
!
      IF (OPCODE .EQ. 'M' ) THEN
!
!         Matrix einlesen
!
            do i = 1, ILINE
   75 CONTINUE
			read(iunit,'(A)') chline
			call hecomm (chline, chcomm)
		      if (chcomm .eq. 'N' ) THEN
			     goto 74
			  else if (chcomm .eq. 'J' ) THEN
			     goto 75
			  endif	
   74 CONTINUE
                backspace (iunit)			  
			    read (iunit, *) (DMAT (i,j), j=1, icolumn)
            end do	
!
            ierr = 0
			ierralloc = 0
			goto 9999
      ENDIF
	  
!
! ======================================================================
!AT*  A U S G A B E T E I L
! ======================================================================
!
!
! ======================================================================
!FA*  F O R M A T  - A N W E I S U N G E N
! ======================================================================
!
 9999 CONTINUE
!

      CALL HEINOU ( 'HREADFEM', 'AUS', IERR1 )
!
!	  
! *** Ende der Subroutine HREADFEM
!
      RETURN
!
      END
!