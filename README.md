# Eigen Mat Project

## Overview
This project is a collection of Fortran routines for calculating eigenvalues and eigenvectors of matrices. It was originally developed by Axel Haenschke on Windows/VMS and has been ported to Ubuntu.

The project is a first step towards contributing an implicit solver to OpenRadioss, with the goal of reducing dependency on external solvers like MUMPS.

## Methods Implemented
1.  **Von Mises Method (`mistst`)**: Calculates the largest eigenvalue/vector. Suitable for small systems.
2.  **Martin-Parlett-Peters-Reinsch-Wilkinson Method (`eigtst`)**: Calculates all eigenvalues/vectors. Suitable for medium systems.
3.  **LAPACK DGEEV (`realeig_2`)**: Uses the standard LAPACK library. Suitable for larger systems.
4.  **ARPACK (`realeig_3`)**: Placeholder for sparse matrix support (currently still using LAPACK).

## Project Structure
- `allg/`: General utility functions (date, time, string handling).
- `cha/`: Character handling and parsing.
- `help/`: Interactive help and I/O system.
- `matrix/engeln-reutter/`: Core numerical algorithms for Mises and Martin-Parlett methods.
- `matrix/blas/` & `matrix/lapack/`: Standard linear algebra libraries.
- `test/`: Main programs and test data.

## Build Instructions
The project uses CMake for building on Linux.

### Prerequisites
- CMake (>= 3.10)
- GFortran (supporting -fdollar-ok, -fallow-argument-mismatch)

### Building
```bash
mkdir build
cd build
cmake ..
make
```

### Executables
- `build/test/eigenvalue`: Main menu program.
- `build/test/mistst_exe`: Standalone Von Mises test.
- `build/test/eigtst_exe`: Standalone all-eigenvalues test.
- `build/test/realeig_2_exe`: Standalone LAPACK test.

## Porting Notes (Windows to Ubuntu)
- Screen clearing changed from `cls` to `clear`.
- VMS-specific `LIB$DATE_TIME` replaced with standard Fortran `DATE_AND_TIME`.
- VMS-specific `getsyi.f` removed.
- Filename `realeig_3 .f` renamed to `realeig_3.f`.
- Compiler flags `-fdollar-ok`, `-ffixed-line-length-none`, and `-fallow-argument-mismatch` are required for GFortran.
