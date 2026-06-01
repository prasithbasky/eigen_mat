# Eigen Mat Project

## Overview
This project is a collection of Fortran routines for calculating eigenvalues and eigenvectors of matrices. It was originally developed by Axel Haenschke on Windows/VMS and has been ported to Ubuntu.

The project is a first step towards contributing an implicit solver to OpenRadioss, with the goal of reducing dependency on external solvers like MUMPS.

## Methods Implemented

| Option | Subroutine | Algorithm | Library | Matrix Type | Use Case |
|--------|-----------|-----------|---------|-------------|----------|
| `a` | `mistst` | Von Mises power iteration | engeln-reutter | General | Single dominant eigenvalue, small systems |
| `b` | `eigtst` | Martin-Parlett-Peters-Reinsch-Wilkinson QR | engeln-reutter | General | All eigenvalues/vectors, small–medium dense |
| `c` | `realeig_2` | DGEEV (Hessenberg + QR) | LAPACK 3.12.1 | General dense | All eigenvalues/vectors, medium dense |
| `d` | `realeig_3` | DNAUPD/DNEUPD — Implicitly Restarted Arnoldi (IRAM) | ARPACK-NG 3.9.1 | General non-symmetric | Few eigenvalues, large sparse non-symmetric |
| `e` | `realeig_4` | DSAUPD/DSEUPD — Implicitly Restarted Lanczos (IRLM) | ARPACK-NG 3.9.1 | Symmetric | Few eigenvalues, large sparse symmetric (FEA) |

### Test Results :

**Methods a–d** use the same 4×4 non-symmetric test matrix (from `test.dat`):
```
 -2  2  2  2
 -3  3  2  2
 -2  0  4  2
 -1  0  0  5
```
Eigenvalues: **1.0, 2.0, 3.0, 4.0** (all real)

**Method e** uses a 4×4 symmetric tridiagonal test matrix (from `test_sym.dat`):
```
  2 -1  0  0
 -1  2 -1  0
  0 -1  2 -1
  0  0 -1  2
```
Eigenvalues: **≈ 0.382, 1.382, 2.618, 3.618**
(Analytical: λ_k = 2 − 2·cos(k·π/5), k = 1..4)

## Project Structure

```
eigen_mat/
├── allg/                      General utility functions (date, time, strings)
├── cha/                       Character handling and parsing
├── help/                      Interactive help and I/O system
├── matrix/
│   ├── engeln-reutter/        Core algorithms: Von Mises, Martin-Parlett QR
│   ├── blas/                  BLAS (Basic Linear Algebra Subprograms) — bundled source
│   ├── lapack/                LAPACK 3.12.1 — bundled source, static library
│   └── arpack/                ARPACK-NG 3.9.1 — bundled source, static library
│                              (non-symmetric: dna*.f  |  symmetric: dsa*.f)
└── test/
    ├── eigenvalue.f           Main interactive menu program (methods a–e)
    ├── mistst.f               Method a: Von Mises
    ├── eigtst.f               Method b: Martin-Parlett QR
    ├── realeig_2.f            Method c: LAPACK DGEEV
    ├── realeig_3.f            Method d: ARPACK DNAUPD/DNEUPD (non-symmetric)
    ├── realeig_4.f            Method e: ARPACK DSAUPD/DSEUPD (symmetric)
    ├── test.dat               Test matrix (non-symmetric, 4×4 double precision)
    └── test_sym.dat           Test matrix (symmetric tridiagonal, 4×4 double precision)
```

## Static Library Architecture

All numerical libraries are compiled **from source** and statically linked — no
runtime library dependencies are required beyond GFortran itself.

| Library | Version | Source directory | Description |
|---------|---------|-----------------|-------------|
| BLAS | Reference | `matrix/blas/` | Level 1/2/3, double precision |
| LAPACK | 3.12.1 | `matrix/lapack/` | Full double-precision suite; used by methods c, d, e |
| ARPACK-NG | 3.9.1 | `matrix/arpack/` | IRAM/IRLM iterative solvers; used by methods d, e |

Dependency chain: `ARPACK → LAPACK → BLAS`

## Build Instructions

### Prerequisites
- CMake >= 3.10
- GFortran (supporting `-fdollar-ok`, `-fallow-argument-mismatch`)

### Building
```bash
mkdir build
cd build
cmake ..
make -j4
```
#### Building on Windows 
- for Windows there is a build.bat script, follow these steps
- Open the "Intel oneAPI command prompt for Visual Studio".
- Navigate to the eigen_mat folder.
- Type build.bat and press Enter.


### Executables
- `build/test/eigenvalue`    — Main interactive menu program (all 5 methods)
- `build/test/mistst_exe`    — Standalone Von Mises test
- `build/test/eigtst_exe`    — Standalone all-eigenvalues test
- `build/test/realeig_2_exe` — Standalone LAPACK DGEEV test

### Running
```bash
cd build/test
./eigenvalue
# Select a–e for the desired method, s to stop
```

## ARPACK Configuration Details

### Method d — DNAUPD/DNEUPD (non-symmetric)
- **BMAT** = `'I'`  → standard eigenvalue problem A·x = λ·x
- **WHICH** = `'LM'` → NEV = 2 eigenvalues of largest magnitude
- **NCV** = 4, **NEV** = 2  (constraint for non-symmetric: NCV ≥ NEV + 2)
- **Mode** 1: direct matrix-vector product via DGEMV

### Method e — DSAUPD/DSEUPD (symmetric)
- **BMAT** = `'I'`  → standard eigenvalue problem A·x = λ·x
- **WHICH** = `'LA'` → NEV = 2 eigenvalues of largest algebraic value
- **NCV** = 4, **NEV** = 2  (constraint for symmetric: NCV > NEV)
- **Mode** 1: direct matrix-vector product via DGEMV
- Returns real eigenvalues only (symmetric matrices have real spectra)

## ARPACK Source Files (matrix/arpack/)

**Non-symmetric** (method d):
`dnaupd.f`, `dneupd.f`, `dnaup2.f`, `dnaitr.f`, `dnapps.f`, `dneigh.f`,
`dngets.f`, `dnconv.f`, `dstatn.f`

**Symmetric** (method e):
`dsaupd.f`, `dseupd.f`, `dsaup2.f`, `dsaitr.f`, `dsapps.f`, `dseigt.f`,
`dsgets.f`, `dsesrt.f`, `dstqrb.f`, `dsconv.f`, `dstats.f`

**Shared** (both):
`dgetv0.f`, `dsortr.f`, `dsortc.f`

**Utilities**:
`dvout.f`, `dmout.f`, `ivout.f`, `second_NONE.f`

**Headers**: `debug.h`, `stat.h`, `version.h`

## Porting Notes (Windows/VMS to Ubuntu)
- Screen clearing changed from `cls` to `clear`.
- VMS-specific `LIB$DATE_TIME` replaced with standard Fortran `DATE_AND_TIME`.
- VMS-specific `getsyi.f` removed.
- Filename `realeig_3 .f` renamed to `realeig_3.f`.
- Compiler flags `-fdollar-ok`, `-ffixed-line-length-none`, and `-fallow-argument-mismatch` required for GFortran.
