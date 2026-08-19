!
!*****7**1*********2*********3*********4*********5*********6*********7**
!
      integer function count_columns(line)
!
!
!
!B*   Name = count_columns
!            -------------
!
!
!D*   PROGRAMM-SYSTEM: beliebig
!     ---------------
!
!E*   FUNKTION: Bestimmen der Anzahl der Spalten einer Matrix
!     --------
!               Es wird eine Zeile der Matrix eingelesen und die 
!               Anzahl der Zahlen ermittelt.
!               Seperator dabei ist das Lerzeichen ' '
!               
!
!S*   PROGRAMMIER-SPRACHE: FORTRAN 90
!     --------------------
!
!
!F*   EINGABE:
!     -------
!
!     - iline      Character String der ersten Zeile 
! 
!
!G*   AUSGABE:
!     -------
!
!     - ICOLUMN    Anzahl der aktuellen Spalten in der rufenden
!                  Routine
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
!     - keine
!
!
!J*   BENOETIGTE UNTERPROGRAMME / FUNKTIONEN:
!     --------------------------------------
!
!     - keine
!
!L*   DIESE ROUTINE WIRD AUFGERUFEN VON:
!     ---------------------------------
!
!     - SUBROUTINE HREADFEM
!
!
!M*   ACHTUNG:     
!     -------      
!
!
!
!N*   BEMERKUNG:   
!     ---------     
!
!
!
!O*   AUTOR:       Axel Haenschke
!     -----        CÜS Consulting
!
!
!P*   VERSION:     0.1                                     11.08.2026
!     -------
!
!
!Q*   UPDATE:      Axel Haenschke                          11.08.2026
!     -------      
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
          implicit none
!
          character(len=*), intent(in) :: line
!
          integer :: i
          integer :: n
          logical :: in_value
!
! ======================================================================
!EV*  E I N G A B E V E R A R B E I T U N G
! ======================================================================
!      
          n = 0
          in_value = .false.
!
! ======================================================================
!VT*  V E R A R B E I T U N G S T E I L
! ======================================================================
!	
          do i = 1, len_trim(line)
!
              if (line(i:i) /= ' ') then
!
                 if (.not. in_value) then
                     n = n + 1
                     in_value = .true.
                 end if
!
              else
                 in_value = .false.
              end if
!
          end do
!
! ======================================================================
!AT*  A U S G A B E T E I L
! ======================================================================
!
          count_columns = n

       end function count_columns
!
