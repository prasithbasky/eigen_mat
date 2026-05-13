@echo off
setlocal

echo ===================================================
echo Building Eigen Mat project with Intel ifx
echo ===================================================

:: Set compiler flags for legacy compatibility
:: /assume:dollar allows $ in identifiers (standard in ifx but explicit here)
:: /extend_source:132 allows longer lines (legacy code often exceeds 72 cols)
:: /I:help adds the help directory for INCLUDE 'steuer.inc'
:: /I:matrix\INSTALL is needed for some LAPACK internal includes
set IFC_FLAGS=/nologo /c /assume:dollar /extend_source:132 /I:help /I:matrix\INSTALL

echo [1/6] Compiling allg...
:: We skip getsyi.f as it is VMS specific and won't compile on standard Windows/Linux
ifx %IFC_FLAGS% allg\chlaen.f allg\clear.f allg\ddate.f allg\enter.f allg\getdat.f allg\gettim.f allg\header.f allg\komaus.f allg\mnrlen.f allg\qlcase.f allg\qltrim.f allg\qrdrucke.f allg\qrtrim.f allg\qspace.f allg\qstlng.f allg\qtest.f allg\qucase.f allg\strgrb.f allg\timdat.f

echo [2/6] Compiling cha...
ifx %IFC_FLAGS% cha\*.f

echo [3/6] Compiling help...
ifx %IFC_FLAGS% help\*.f

echo [4/6] Compiling engeln-reutter...
ifx %IFC_FLAGS% matrix\engeln-reutter\*.f

echo [5/6] Compiling BLAS...
ifx %IFC_FLAGS% matrix\blas\*.f

echo [6/6] Compiling LAPACK...
:: Compiling the core LAPACK files needed for DGEEV
ifx %IFC_FLAGS% matrix\lapack\*.f
ifx %IFC_FLAGS% matrix\INSTALL\dlamch.f

echo ===================================================
echo Linking Executables...
echo ===================================================

:: Note: eigenvalue.f is the main menu.
:: realeig_2.f, realeig_3.f, mistst.f, and eigtst.f have been 
:: converted to subroutines in this version to be called by the menu.
ifx /nologo test\eigenvalue.f test\mistst.f test\eigtst.f test\realeig_2.f test\realeig_3.f *.obj /exe:test\eigenvalue.exe

:: Standalone test programs (contain their own 'PROGRAM' main)
ifx /nologo test\main_mistst.f *.obj /exe:test\mistst_exe.exe
ifx /nologo test\main_eigtst.f *.obj /exe:test\eigtst_exe.exe
ifx /nologo test\main_realeig_2.f *.obj /exe:test\realeig_2_exe.exe

echo ===================================================
echo Cleanup...
echo ===================================================
del *.obj

echo.
echo Done! Executables created in the test\ directory:
echo   - test\eigenvalue.exe   (Main Menu)
echo   - test\mistst_exe.exe   (Von Mises Test)
echo   - test\eigtst_exe.exe   (All Eigenvalues Test)
echo   - test\realeig_2_exe.exe (LAPACK Test)
echo.
pause
