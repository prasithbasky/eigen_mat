# OpenRadioss Implicit Solver Integration Guide

## Integrating a Custom Solver (eigen_mat / LAPACK / ARPACK) to Replace MUMPS

**Project:** `/home/wsl/OpenRadioss/OpenRadioss/`  
**eigen_mat library:** `/home/wsl/OpenRadioss/eigen_mat/`  
**Goal:** Extract the fully assembled stiffness and mass matrices from the OpenRadioss
implicit pipeline, redirect them to a custom solver, and remove the MUMPS dependency.

---

## Table of Contents

1. [Architecture Overview](#1-architecture-overview)
2. [The Complete Matrix Pipeline](#2-the-complete-matrix-pipeline)
3. [All Matrices Available at the Solver Entry Point](#3-all-matrices-available-at-the-solver-entry-point)
4. [File-by-File Reference](#4-file-by-file-reference)
5. [The Exact Entry Points](#5-the-exact-entry-points)
6. [The IDSC Refactorization Flag](#6-the-idsc-refactorization-flag)
7. [Solver Control Variables](#7-solver-control-variables)
8. [How MUMPS Currently Works (What You Are Replacing)](#8-how-mumps-currently-works-what-you-are-replacing)
9. [Step-by-Step Implementation Plan](#9-step-by-step-implementation-plan)
10. [Build System Changes](#10-build-system-changes)
11. [Verification Strategy](#11-verification-strategy)

---

## 1. Architecture Overview

The OpenRadioss implicit solver follows a Newton-Raphson loop at each time step.
The big picture for one Newton iteration:

```
RESOL (resol.F:8536)                   ← main engine time-step driver
  └── IMP_SOLV (imp_solv.F:147)        ← implicit solver orchestrator
        ├── STIFFNESS ASSEMBLY (if ISETK==1)
        │     ├── ZERO1(DIAG_K, LT_K)              zero out matrix
        │     ├── IND_GLOB_K  (ind_glob_k.F)        rebuild sparsity pattern
        │     ├── IMP_GLOB_KHP (imp_glob_k.F)       assemble element Ke → K
        │     ├── IMP_DYNAM   (imp_dyna.F:299)      add mass + damping → K_eff
        │     └── UPD_GLOB_K  (upd_glob_k.F)        apply BCs, constraints
        │
        ├── ─── IMP_MUMPS1 called here ──────────────  ← HOOK POINT A
        │        (imp_solv.F ~line 1302)
        │
        └── LIN_SOLV (lin_solv.F:63)
              └── LIN_SOLVP2 (lin_solv.F:527)
                    └── IMP_MUMPS2 called here ──────  ← HOOK POINT B
                         (lin_solv.F ~line 577)
```

**Hook Point A** is where you replace matrix assembly / factorization initialization.  
**Hook Point B** is where you replace factorization + forward/back substitution.

---

## 2. The Complete Matrix Pipeline

Before any solver is called, five phases run in `imp_solv.F` (triggered when `ISETK==1`):

### Phase 1 — Zero-fill (`imp_solv.F:724`)
```fortran
CALL ZERO1(DIAG_K, NDDL)   ! clear diagonal
CALL ZERO1(LT_K,   NNZK)   ! clear lower triangle
```
Arrays `DIAG_K` and `LT_K` are cleared to zero every time the stiffness
must be recomputed.

### Phase 2 — Sparsity Pattern (`ind_glob_k.F` via `IMP_GLOB_KHP`)
```fortran
CALL IND_GLOB_K(...)        ! builds IADK, JDIK from element connectivity
```
`IND_GLOB_K` walks all element groups and constraint types, counts connected
DOF pairs, and fills the CSR row-pointer/column-index arrays.
Only rerun when topology changes (large deformation remesh, new contact).

### Phase 3 — Element Assembly (`imp_glob_k.F`)
```fortran
CALL IMP_GLOB_KHP(... DIAG_K, LT_K, IADK, JDIK ...)
```
Loops over every element group. For each element it calls the element-type
kernel (e.g., `S4KE3` for shell, `S10KE3` for solid 10-node, `CBAKE3` for beam)
to compute the local stiffness matrix `Ke`, then calls `ASSEM_KIJ` / `ASSEM_KII`
to scatter `Ke` into the global `DIAG_K` / `LT_K` using `IDDL` for DOF mapping.

**After Phase 3:** `DIAG_K(i)` and `LT_K(j)` hold the pure structural
stiffness, nothing else.

### Phase 4 — Dynamic Effective Stiffness (`imp_dyna.F:299`)
```fortran
IF (IDYNA>0 .OR. IQSTAT>0)
  CALL IMP_DYNAM(NODFT, NODLT, IDDL, NDOF, DIAG_K, MS, IN,
                 HHT_A, WEIGHT, IADK, LT_K)
```
`IMP_DYNAM` modifies `DIAG_K` and `LT_K` in-place to produce the
**effective stiffness K_eff** needed by the Newmark/HHT time integrator:

```
K_eff  =  K * (1 + S0)  +  BDT * M_lumped

  S   = (1 + HHT_A) * beta * dt²
  S0  = (1 + HHT_A) * DAMPB_IMP * gamma / S      (stiffness damping)
  BDT = (1/dt²  +  (1 + HHT_A) * DAMPA_IMP * gamma) / S   (mass term)

  gamma ≈ 0.5 (Newmark),  beta ≈ 0.25 (Newmark)
  DAMPA_IMP = Rayleigh alpha (mass-proportional)
  DAMPB_IMP = Rayleigh beta  (stiffness-proportional)
```

The mass is added **only to the diagonal** of K_eff, one DOF at a time:
```fortran
MKF = ABS(MS(node)) * BDT * WEIGHT(node)
DIAG_K(IDDL(node) + dof) += MKF
```

> **Important:** After `IMP_DYNAM`, `DIAG_K` no longer holds pure structural
> stiffness — it holds the full effective stiffness. The raw nodal masses
> `MS(NUMNOD)` remain untouched if you need them separately.

### Phase 5 — Constraint Application (`upd_glob_k.F`)
```fortran
CALL UPD_GLOB_K(... NDDL, NNZK, IADK, JDIK, DIAG_K, LT_K, IKC, LB ...)
```
Applies all kinematic constraints:
- Fixed BCs (`BC_IMP0`)
- Prescribed velocity/displacement (`BC_IMP1`)
- Rigid body elements (`RBE2_IMP0`, `RBE3_IMP0`)
- Contact interface stiffness (`I2_IMP0`, `UPD_INT_K`)
- Condensation: removes constrained rows/columns from K (`CONDENS_K`)

After this phase `NDDL` is the **final active DOF count** (constrained DOFs
have been eliminated). The RHS `LB` is also condensed.

---

## 3. All Matrices Available at the Solver Entry Point

Everything below is **fully assembled and in scope** at the moment `IMP_MUMPS1`
is called in `imp_solv.F:~1302`.

### 3.1 Effective Stiffness K (always present)

| Array | Size | Fortran Type | Description |
|-------|------|-------------|-------------|
| `DIAG_K` | `NDDL` | `REAL*8` | Diagonal of K_eff: K(i,i) + mass + damping contributions |
| `LT_K` | `NNZK` | `REAL*8` | Lower-triangle off-diagonal K_eff values |
| `IADK` | `NDDL+1` | `INTEGER` | CSR row pointers: row i spans `IADK(i)..IADK(i+1)-1` |
| `JDIK` | `NNZK` | `INTEGER` | CSR column indices for each LT_K entry |
| `NDDL` | scalar | `INTEGER` | Active (free) DOFs after constraint removal |
| `NNZK` | scalar | `INTEGER` | Off-diagonal non-zeros in lower triangle |

**CSR format rule:** Matrix is symmetric. Only lower triangle + diagonal stored.
```
K(i,i)  = DIAG_K(i)
K(i,j)  = LT_K(p)   where p in IADK(i)..IADK(i+1)-1 and JDIK(p)==j  (j < i)
K(j,i)  = K(i,j)    (by symmetry)
```

To reconstruct the full dense K(i,j) for LAPACK:
```fortran
A(i,i) = DIAG_K(i)
DO p = IADK(i), IADK(i+1)-1
  j = JDIK(p)
  A(i,j) = LT_K(p)   ! lower triangle
  A(j,i) = LT_K(p)   ! upper triangle (symmetric copy)
END DO
```

### 3.2 Contact / Interface Sub-matrix I (only when contact is active)

When contact interfaces are present (`NDDLI > 0`), a separate sparse matrix
represents the interface coupling. It is merged into the global system at solve time.

| Array | Size | Fortran Type | Description |
|-------|------|-------------|-------------|
| `DIAG_I` | `NDDLI` | `REAL*8` | Interface diagonal contributions |
| `LT_I` | `NNZI` | `REAL*8` | Interface lower-triangle values |
| `IADI` | `NDDLI+1` | `INTEGER` | CSR row pointers for interface matrix |
| `JDII` | `NNZI` | `INTEGER` | CSR column indices for interface matrix |
| `ITOK` | `NDDLI` | `INTEGER` | Maps interface DOF `i` → global DOF `ITOK(i)` |
| `NDDLI` | scalar | `INTEGER` | Number of interface DOFs |
| `NNZI` | scalar | `INTEGER` | Interface off-diagonal non-zeros |

To merge I into your assembled dense matrix:
```fortran
DO i = 1, NDDLI
  g = ITOK(i)
  A(g,g) = A(g,g) + DIAG_I(i)
  DO p = IADI(i), IADI(i+1)-1
    gj = ITOK(JDII(p))
    A(g,gj) = A(g,gj) + LT_I(p)
    A(gj,g) = A(gj,g) + LT_I(p)   ! symmetric
  END DO
END DO
```

### 3.3 Preconditioner Matrix M (for iterative solvers)

Used by the iterative solvers (PCG, Lanczos). Not passed to MUMPS.

| Array | Size | Fortran Type | Description |
|-------|------|-------------|-------------|
| `DIAG_M` | `NDDL` | `REAL*8` | Diagonal of preconditioning matrix |
| `LT_M` | `MAX(NNZK,MAX_L)` | `REAL*8` | Off-diagonal of M (same sparsity as K) |
| `IADM` | `NDDL+1` | `INTEGER` | Same row pointers as IADK |
| `JDIM` | `NNZK` | `INTEGER` | Same column indices as JDIK |

`DIAG_M/LT_M` can be used as a preconditioner matrix for your iterative solver
(e.g., ARPACK or PCG). It is filled by `IMP_CHECM` and similar routines.
For ARPACK's eigenvalue mode (`BMAT='G'`), this is the mass matrix B in K·x = λ·B·x.

### 3.4 Nodal Mass Arrays (raw, not assembled into sparse format)

| Array | Size | Fortran Type | Description |
|-------|------|-------------|-------------|
| `MS(NUMNOD)` | nodes | `REAL*8` | Lumped translational mass per node |
| `IN(NUMNOD)` | nodes | `REAL*8` | Rotational inertia per node (if `IRODDL≠0`) |

These are the **original lumped masses before they are added into DIAG_K**.
If your solver needs the mass matrix M separately (e.g., to build M·v products
for ARPACK), use these arrays directly with the `IDDL` and `NDOF` mapping:
```fortran
! Build M*v for node i, DOF j:
dof_index = IDDL(i) + j
Mv(dof_index) = ABS(MS(i)) * v(dof_index)
```

### 3.5 Right-Hand Side — Residual Force Vector

| Array | Size | Fortran Type | Description |
|-------|------|-------------|-------------|
| `LB` | `NDDL` | `REAL*8` | Full Newton residual: F_ext − F_int − F_inertia − F_damp |

This is the `F` passed to `LIN_SOLV` and ultimately to the solver as the RHS of
`K_eff · Δu = LB`. Your solver receives it as argument `F` in `LIN_SOLVP2`.

### 3.6 DOF Mapping Arrays

| Array | Size | Fortran Type | Description |
|-------|------|-------------|-------------|
| `IDDL(NUMNOD)` | nodes | `INTEGER` | Node n → first DOF index in K (1-based) |
| `NDOF(NUMNOD)` | nodes | `INTEGER` | DOF count per node: 3 (translation) or 6 (with rotation) |
| `INLOC(NUMNOD)` | nodes | `INTEGER` | DOF index → owning node (inverse of IDDL) |
| `IKC(NDDL)` | DOFs | `INTEGER` | Constraint flag: 0=free, ≥1=constrained (eliminated) |

---

## 4. File-by-File Reference

### Core Implicit Source Files (`engine/source/implicit/`)

| File | Purpose |
|------|---------|
| `imp_sol_init.F` | One-time initialization: allocates all matrix arrays, calls `DIM_GLOB_K` and `IND_GLOB_K` to set up structure |
| `ind_glob_k.F` | **Sparsity builder.** `DIM_GLOB_K` counts DOFs/nonzeros. `IND_GLOB_K` fills `IADK`, `JDIK` from element connectivity |
| `imp_glob_k.F` | **Element assembly kernel.** `IMP_GLOB_KHP` loops elements, computes `Ke`, scatters into `DIAG_K`/`LT_K` via `ASSEM_KII`/`ASSEM_KIJ` |
| `imp_dyna.F` | **Dynamic effective stiffness.** `IMP_DYNAM` adds `BDT*M` and Rayleigh damping to `DIAG_K`/`LT_K`. `IMP_DYNAR` assembles inertial RHS |
| `upd_glob_k.F` | **Constraint application.** `UPD_GLOB_K` applies BCs, RBEs, contact. `CONDENS_K` removes constrained rows/cols and reindexes `JDIK` |
| `imp_solv.F` | **Main orchestrator.** Controls stiffness rebuild, calls all assembly phases, then calls `IMP_MUMPS1` and `LIN_SOLV`. **HOOK POINT A is here (~line 1302)** |
| `lin_solv.F` | **Linear solver dispatcher.** `LIN_SOLV` selects solver based on `ISOLV`. `LIN_SOLVP2` executes direct solve path. **HOOK POINT B is here (~line 577)** |
| `imp_mumps.F` | MUMPS wrapper: `IMP_MUMPS1` (converts CSR→COO, loads MUMPS structure), `IMP_MUMPS2` (runs factorize + solve) |
| `nl_solv.F` | Newton-Raphson nonlinear loop. Calls `LIN_SOLV` each Newton iteration |
| `imp_pcg.F` | Preconditioned Conjugate Gradient iterative solver (`ISOLV=1`) |
| `imp_lanz.F` | Lanczos eigenvalue solver (`ISOLV=2`) — existing eigenvalue path |
| `imp_dt.F` | Implicit time step control |
| `imp_setb.F` | Assembles external force vector LB |
| `prec_solv.F` | Preconditioner solve utilities |
| `assem_s4.F`, `assem_s8.F`, `assem_s10.F`, `assem_s20.F` | Shell/solid element stiffness kernels (called from `imp_glob_k.F`) |
| `assem_c3.F`, `assem_c4.F`, `assem_r3.F`, `assem_q4.F` | More element stiffness kernels |
| `integrator.F` | Time integration scheme (Newmark/HHT) |

### MPI Wrappers (`engine/source/mpi/implicit/`)

| File | Purpose |
|------|---------|
| `imp_spmd.F` | All MPI wrappers for MUMPS: `SPMD_MUMPS_INI`, `SPMD_MUMPS_DEAL`, `SPMD_MUMPS_EXEC`, `SPMD_MUMPS_RHS`, `SPMD_MUMPS_GATH`, `SPMD_MUMPS_COUNT` |
| `imp_fri.F` | MPI frontier element handling for iterative solvers |
| `imp_spmd.F` | Parallel DOF synchronization |

### Data Structures and Includes

| File | Purpose |
|------|---------|
| `engine/share/modules/impbufdef_mod.F` | **Master module.** `TYPE IMPBUF_STRUCT_` holds all K, M, CSR, DOF arrays. The `IMPBUF_TAB` variable in `imp_solv.F` is of this type |
| `engine/share/includes/impl1_c.inc` | **COMMON /IMPL1/.** Integer solver controls: `ISOLV`, `IDSC`, `IMUMPSV`, `ISETK`, `IKPAT`, `NDDL_G`, `NNZK_G`, `IRODDL`, etc. |
| `engine/share/includes/impl2_c.inc` | **COMMON /IMPL2/.** Real parameters: `DT_IMP`, `HHT_A`, `DAMPA_IMP`, `DAMPB_IMP`, `L_TOL`, `N_TOL`, `DY_G`, `DY_B` |
| `engine/source/engine/resol.F` | **Main driver.** Declares `TYPE(DMUMPS_STRUC) MUMPS_PAR` (line 1128), calls `IMP_SOL_INIT` (line 2354), calls `IMP_SOLV` (line 8536) |

---

## 5. The Exact Entry Points

### Hook Point A — Matrix Assembly & Factorization Setup

**File:** `engine/source/implicit/imp_solv.F`  
**Approximate line:** 1302  
**Condition:** `IMUMPSV > 0 .AND. IDSC==1 .AND. IMCONV>=0`

```fortran
! Existing MUMPS block (lines ~1300-1311):
#if defined(MUMPS5)
        IF (IMUMPSV >0 .AND.IDSC==1.AND.IMCONV>=0)
     .    CALL IMP_MUMPS1(NDDL0,   NNZK0,     NDDL,   NNZK,  NNMAX,
     .                    NODGLOB, IDDL,      NDOF,   INLOC, IKC,
     .                    IADK,    JDIK,      DIAG_K, LT_K,  IAD_ELEM,
     .                    FR_ELEM, MUMPS_PAR, CDDLP,  IADI,  JDII,
     .                    ITOK,    DIAG_I,    LT_I,   NDDLI, NNZI,
     .                    IPRINT0, IT)
#else
        WRITE(6,*) "Fatal error: MUMPS is required"
        CALL ARRET(5)
#endif
```

**Your replacement block goes here (add after or inside `#else`):**
```fortran
#if defined(LAPACK_SOLV)
        IF (ISOLV==10 .AND. IDSC==1 .AND. IMCONV>=0)
     .    CALL IMP_LAPACK1(NDDL,  NNZK,
     .                     IADK,  JDIK,  DIAG_K, LT_K,
     .                     NDDLI, NNZI,
     .                     IADI,  JDII,  ITOK, DIAG_I, LT_I)
#endif
```

At this point ALL 5 assembly phases are complete:
- `DIAG_K`, `LT_K` hold **K_eff** (structural + mass + damping + constraints applied)
- `LB` holds the Newton residual RHS
- `IKC` has been applied (constrained DOFs removed from NDDL)
- `NDDL` is the final free DOF count

### Hook Point B — Factorize and Solve

**File:** `engine/source/implicit/lin_solv.F`  
**Subroutine:** `LIN_SOLVP2`  
**Approximate line:** 577

```fortran
! Existing code in LIN_SOLVP2:
      IF (ISOLV==3) THEN
        CALL MUMPSLB(F, F1, NDDL, IAD_ELEM, FR_ELEM,
   1                 IDDL, IKC, INLOC, NDOF, ITAG)
        CALL IMP_MUMPS2(MUMPS_PAR, CDDLP, F1, X, NDDL)
      ELSEIF (ISOLV==4) THEN
        ...
      ENDIF
```

**Add your branch (no MUMPSLB reordering needed for dense LAPACK):**
```fortran
#if defined(LAPACK_SOLV)
      ELSEIF (ISOLV==10) THEN
        CALL IMP_LAPACK2(F, X, NDDL)
#endif
```

After this call, `X(1:NDDL)` must contain the displacement increment `Δu`
solving `K_eff · Δu = F`.

---

## 6. The IDSC Refactorization Flag

`IDSC` (from `impl1_c.inc`) controls whether to redo the expensive factorization
or reuse the previous one.

| Value | Meaning | When set |
|-------|---------|---------|
| `IDSC = 1` | **Refactorize:** rebuild internal factor data | Stiffness changed: `ISETK=1`, new contact, wall impact, first iteration |
| `IDSC = 0` | **Reuse:** skip factorization, only do forward/back substitution | Same stiffness as last call (same K_eff, same sparsity) |

**In your solver:**
```fortran
SUBROUTINE IMP_LAPACK2(F, X, NDDL)
  USE LAPACK_SOLVER_MOD    ! access A_FACT (stored factored matrix)
#include "impl1_c.inc"     ! access IDSC

  IF (IDSC == 1) THEN
    CALL DPOTRF('L', NDDL, A_FACT, NDDL, INFO)  ! Cholesky factorize
    ! INFO > 0 means matrix is not positive definite
    ! fallback: CALL DGESV for general (possibly indefinite) K_eff
  END IF

  WORK(1:NDDL) = F(1:NDDL)
  CALL DPOTRS('L', NDDL, 1, A_FACT, NDDL, WORK, NDDL, INFO)
  X(1:NDDL) = WORK(1:NDDL)
END SUBROUTINE
```

`A_FACT` must persist across calls (allocated in a module, see Step 1 of the
implementation plan). It is the dense N×N matrix, overwritten by `DPOTRF` with
the Cholesky factor L.

---

## 7. Solver Control Variables

### From `impl1_c.inc` — COMMON /IMPL1/ (integer controls)

| Variable | Meaning | Relevant Value |
|----------|---------|----------------|
| `ISOLV` | Solver selector | 1,7,8=PCG iterative; 2=Lanczos; **3=MUMPS direct**; 5,6=Hybrid DDM. **Use 10 for your solver** |
| `IDSC` | Refactorize flag | 1=factorize now, 0=reuse |
| `IMUMPSV` | MUMPS enable flag | >0=MUMPS active, 0=disabled |
| `ISETK` | Stiffness changed | 1=rebuild K, 0=reuse K |
| `IKPAT` | CSR storage order | 0=lower-triangle, 1=upper-triangle |
| `IRODDL` | Rotational DOFs | 0=3 DOF/node (translation only), 1=6 DOF/node |
| `NSPMD` | MPI process count | 1=serial, >1=parallel |
| `ISPMD` | MPI rank | 0=master process |
| `IMCONV` | Convergence state | ≥0=continue solving, <0=skip (negative pivot) |
| `IDYNA` | Dynamic flag | 0=static, >0=dynamic |
| `IQSTAT` | Quasi-static flag | >0=quasi-static (mass scaled) |
| `NDDL_G` | Global DOF count | For MPI: total DOFs across all domains |

### From `impl2_c.inc` — COMMON /IMPL2/ (real parameters)

| Variable | Meaning |
|----------|---------|
| `DT_IMP` | Implicit time step Δt |
| `HHT_A` | HHT-α dissipation (≥0 for numerical damping) |
| `DY_G` | Newmark gamma (≈ 0.5) |
| `DY_B` | Newmark beta (≈ 0.25) |
| `DAMPA_IMP` | Rayleigh alpha (mass-proportional damping) |
| `DAMPB_IMP` | Rayleigh beta (stiffness-proportional damping) |
| `L_TOL` | Linear solver tolerance |
| `N_TOL` | Nonlinear (Newton) convergence tolerance |

---

## 8. How MUMPS Currently Works (What You Are Replacing)

Understanding the current flow helps you replicate it correctly.

### `IMP_MUMPS1` (`imp_mumps.F:43`) — called once per refactorization

1. `CALL SPMD_MUMPS_DEAL(MUMPS_PAR)` — free previous internal allocations
2. `CALL SPMD_MUMPS_INI(MUMPS_PAR, 1)` — initialize MUMPS structure, set symmetry
3. Configure `MUMPS_PAR%ICNTL(*)` — ordering method, output level, MPI mode
4. Convert K + I from CSR format → COO triplet format `(IRN, JCN, A)`:
   - Diagonal terms from `DIAG_K`, off-diagonal from `LT_K`
   - Interface terms from `DIAG_I`, `LT_I` using `ITOK` mapping
   - For MPI: distributed assembly via `SPMD_MUMPS_GATH`
5. Set `MUMPS_PAR%N = NDDL`, `MUMPS_PAR%NZ = NDDL + NNZK + NDDLI + NNZI`

### `IMP_MUMPS2` (`imp_mumps.F:497`) — called every Newton iteration

1. If `IDSC==1`: `CALL SPMD_MUMPS_EXEC(MUMPS_PAR, 1)` → MUMPS JOB=2 (numerical factorization)
2. `CALL SPMD_MUMPS_RHS(F, CDDLP, MUMPS_PAR%RHS, ...)` — copy F to MUMPS RHS array (with reordering)
3. `CALL SPMD_MUMPS_EXEC(MUMPS_PAR, 2)` → MUMPS JOB=3 (solve via LU factors)
4. `CALL SPMD_MUMPS_RHS(X, CDDLP, MUMPS_PAR%RHS, ...)` — copy solution back to X

Your solver must replicate this contract:  
**Input:** `F(NDDL)` (RHS residual)  
**Output:** `X(NDDL)` (displacement increment solving K_eff·X = F)

---

## 9. Step-by-Step Implementation Plan

### Step 1 — Create module for solver state

**New file:** `engine/share/modules/lapack_solver_mod.F`

```fortran
MODULE LAPACK_SOLVER_MOD
  IMPLICIT NONE
  DOUBLE PRECISION, ALLOCATABLE :: A_FACT(:,:)  ! N×N factored matrix
  INTEGER :: N_FACT = 0                          ! current matrix size
  LOGICAL :: IS_FACTORED = .FALSE.
CONTAINS
  SUBROUTINE LAPACK_SOLVER_ALLOC(N)
    INTEGER, INTENT(IN) :: N
    IF (N_FACT /= N) THEN
      IF (ALLOCATED(A_FACT)) DEALLOCATE(A_FACT)
      ALLOCATE(A_FACT(N, N))
      N_FACT = N
    END IF
    A_FACT = 0.0D0
    IS_FACTORED = .FALSE.
  END SUBROUTINE
  SUBROUTINE LAPACK_SOLVER_FREE()
    IF (ALLOCATED(A_FACT)) DEALLOCATE(A_FACT)
    N_FACT = 0
  END SUBROUTINE
END MODULE
```

---

### Step 2 — Create solver wrapper file

**New file:** `engine/source/implicit/imp_lapack_solv.F`

#### Subroutine `IMP_LAPACK1` — sparse-to-dense assembly + Cholesky factorize

```fortran
      SUBROUTINE IMP_LAPACK1(NDDL,  NNZK,
     .                        IADK,  JDIK,  DIAG_K, LT_K,
     .                        NDDLI, NNZI,
     .                        IADI,  JDII,  ITOK, DIAG_I, LT_I)
        USE LAPACK_SOLVER_MOD
#include "implicit_f.inc"     ! provides my_real (REAL*8 or REAL*4)
#include "impl1_c.inc"        ! provides IDSC

        INTEGER NDDL, NNZK, NDDLI, NNZI
        INTEGER IADK(*), JDIK(*), IADI(*), JDII(*), ITOK(*)
        my_real DIAG_K(*), LT_K(*), DIAG_I(*), LT_I(*)

        INTEGER I, J, P, G, GJ, INFO

        ! Allocate / re-allocate dense matrix
        CALL LAPACK_SOLVER_ALLOC(NDDL)

        ! Fill diagonal
        DO I = 1, NDDL
          A_FACT(I, I) = DIAG_K(I)
        END DO

        ! Fill lower + upper triangle from CSR
        DO I = 1, NDDL
          DO P = IADK(I), IADK(I+1)-1
            J = JDIK(P)
            A_FACT(I, J) = LT_K(P)   ! lower
            A_FACT(J, I) = LT_K(P)   ! upper (symmetric)
          END DO
        END DO

        ! Merge interface matrix I via ITOK
        DO I = 1, NDDLI
          G = ITOK(I)
          A_FACT(G, G) = A_FACT(G, G) + DIAG_I(I)
          DO P = IADI(I), IADI(I+1)-1
            GJ = ITOK(JDII(P))
            A_FACT(G,  GJ) = A_FACT(G,  GJ) + LT_I(P)
            A_FACT(GJ, G)  = A_FACT(GJ, G)  + LT_I(P)
          END DO
        END DO

        ! Cholesky factorize (for symmetric positive definite K_eff)
        CALL DPOTRF('L', NDDL, A_FACT, NDDL, INFO)
        IF (INFO /= 0) THEN
          WRITE(6,*) 'LAPACK DPOTRF failed, INFO=', INFO
          ! Fallback: try LU decomposition via DGESV
        END IF
        IS_FACTORED = .TRUE.

      END SUBROUTINE
```

#### Subroutine `IMP_LAPACK2` — forward/back substitution (solve)

```fortran
      SUBROUTINE IMP_LAPACK2(F, X, NDDL)
        USE LAPACK_SOLVER_MOD
#include "implicit_f.inc"
#include "impl1_c.inc"     ! provides IDSC

        INTEGER NDDL
        my_real F(*), X(*)

        my_real WORK(NDDL)
        INTEGER INFO

        ! Copy RHS to work buffer (DPOTRS overwrites it with solution)
        WORK(1:NDDL) = F(1:NDDL)

        CALL DPOTRS('L', NDDL, 1, A_FACT, NDDL, WORK, NDDL, INFO)
        IF (INFO /= 0) THEN
          WRITE(6,*) 'LAPACK DPOTRS failed, INFO=', INFO
        END IF

        X(1:NDDL) = WORK(1:NDDL)

      END SUBROUTINE
```

---

### Step 3 — Hook assembly into `imp_solv.F`

**File:** `engine/source/implicit/imp_solv.F`  
**After the existing `#if defined(MUMPS5) ... #endif` block (~line 1302-1312):**

```fortran
#if defined(LAPACK_SOLV)
        IF (ISOLV==10 .AND. IDSC==1 .AND. IMCONV>=0)
     .    CALL IMP_LAPACK1(NDDL,  NNZK,
     .                     IADK,  JDIK,  DIAG_K, LT_K,
     .                     NDDLI, NNZI,
     .                     IADI,  JDII,  ITOK, DIAG_I, LT_I)
#endif
```

> All arguments are already in scope at that exact line — they are the same
> arrays that IMP_MUMPS1 receives.

---

### Step 4 — Hook solve into `lin_solv.F` (inside `LIN_SOLVP2`)

**File:** `engine/source/implicit/lin_solv.F`  
**Inside subroutine `LIN_SOLVP2`, after the `ELSEIF (ISOLV==4)` block (~line 600):**

```fortran
#if defined(LAPACK_SOLV)
      ELSEIF (ISOLV==10) THEN
        CALL IMP_LAPACK2(F, X, NDDL)
#endif
```

No `MUMPSLB` reordering is needed — LAPACK works in natural DOF order.

---

### Step 5 — Register the new module

**File:** `engine/share/modules/CMakeLists.txt`  
Add `lapack_solver_mod.F` to the module sources list.

---

### Step 6 — Build system: link eigen_mat libraries

**File:** `engine/CMakeLists.txt`

```cmake
# Path to eigen_mat build output
set(EIGEN_MAT_BUILD ${CMAKE_SOURCE_DIR}/../../eigen_mat/build)

# LAPACK/BLAS static libraries from eigen_mat
set(EIGEN_LAPACK_LIB ${EIGEN_MAT_BUILD}/matrix/lapack/liblapack.a)
set(EIGEN_BLAS_LIB   ${EIGEN_MAT_BUILD}/matrix/blas/libblas.a)

# Add compile definition and link
target_compile_definitions(engine PRIVATE LAPACK_SOLV)
target_link_libraries(engine ${EIGEN_LAPACK_LIB} ${EIGEN_BLAS_LIB})
```

For ARPACK (future):
```cmake
set(EIGEN_ARPACK_LIB ${EIGEN_MAT_BUILD}/matrix/arpack/libarpack.a)
target_link_libraries(engine ${EIGEN_ARPACK_LIB})
target_compile_definitions(engine PRIVATE ARPACK_SOLV)
```

---

### Step 7 — Remove MUMPS dependency (full removal)

When your solver is validated, to completely cut MUMPS:

| Action | File | What to change |
|--------|------|----------------|
| Remove `DMUMPS_STRUC` declaration | `resol.F:1128` | Delete `TYPE(DMUMPS_STRUC) MUMPS_PAR` |
| Remove MUMPS init | `resol.F:2378` | Delete `CALL SPMD_MUMPS_INI(MUMPS_PAR, 1)` |
| Remove MUMPS cleanup | `resol.F:9647` | Delete `CALL DEALLOCM_IMP(MUMPS_PAR)` |
| Remove MUMPS5 compile flag | `engine/CMakeLists.txt` | Delete `-DMUMPS5` from compile flags |
| Remove MUMPS library links | `engine/CMakeLists.txt` | Delete MUMPS lib targets |
| Remove MUMPS header includes | `imp_mumps.F`, `lin_solv.F`, `imp_solv.F`, `resol.F` | Delete `#include "dmumps_struc.h"` (8 files) |
| Remove MUMPS_PAR argument | all call sites | Substitute dummy integer |

> **Note:** All production code inside `#if defined(MUMPS5)` blocks compiles
> to nothing when `-DMUMPS5` is absent. Removing that flag is safe and
> immediately eliminates the MUMPS dependency at link time.

---

### Step 8 — ARPACK Eigenvalue Solver (Phase 2 — Large Scale)

For problems too large for dense LAPACK, integrate ARPACK's DSAUPD/DSEUPD
(already in eigen_mat's `matrix/arpack/`).

**Use case:** Modal analysis / frequency extraction. Replaces/supplements
the existing Lanczos in `imp_lanz.F` (activated by `ISOLV=2`).

New file: `engine/source/implicit/imp_arpack_solv.F`

ARPACK's reverse communication interface requires:
1. `DSAUPD` loop: at each iteration, compute `v_new = K · v` (sparse matvec)
2. Sparse matvec using `DIAG_K`, `LT_K`, `IADK`, `JDIK` — no dense conversion needed
3. `DSEUPD` to extract eigenvalues/vectors after convergence

Add `ISOLV=11` branch in `lin_solvh0.F` (alongside the `ISOLV==2` Lanczos path).

---

## 10. Build System Changes

### Build eigen_mat first

```bash
cd /home/wsl/OpenRadioss/eigen_mat
mkdir -p build && cd build
cmake .. -DCMAKE_BUILD_TYPE=Release
make -j4
# Produces:
#   matrix/lapack/liblapack.a
#   matrix/blas/libblas.a
#   matrix/arpack/libarpack.a
```

### Build OpenRadioss engine with LAPACK solver

```bash
cd /home/wsl/OpenRadioss/OpenRadioss
# Add -DLAPACK_SOLV flag to engine cmake configuration
# Link against eigen_mat/build/matrix/lapack/liblapack.a
# Link against eigen_mat/build/matrix/blas/libblas.a
cmake -B build/engine -DLAPACK_SOLV=ON ...
cmake --build build/engine -j4
```

---

## 11. Verification Strategy

### Step 1 — Verify eigen_mat builds

```bash
cd /home/wsl/OpenRadioss/eigen_mat/build/test
./eigenvalue   # pick option e (ARPACK symmetric) — should find eigenvalues ≈ 0.382, 1.382, 2.618, 3.618
```

### Step 2 — Baseline run with MUMPS (ISOLV=3)

Run an existing OpenRadioss test case. Note the displacement output as baseline.

### Step 3 — Run same case with LAPACK solver (ISOLV=10)

Set `ISOLV=10` in the `/IMPL/SOLVER` input deck card.  
The displacement output should match MUMPS to within machine precision (~1e-14).

### Step 4 — Check residual convergence

In the OpenRadioss output file, look for:
```
DIRECT SOLVER TERMINATED WITH RELATIVE ||R|| = ...
```
The residual should be ≤ `L_TOL` (default ~1e-6).

### Step 5 — Test with contact (NDDLI > 0)

Run a sliding contact test. Verify that `DIAG_I`/`LT_I` are correctly merged
via `ITOK` into your dense matrix (wrong merge → wrong normal force / penetration).

### Step 6 — Memory check for scale

Dense N×N storage requires `N² × 8` bytes:
- N=1000 → 8 MB (fine)
- N=5000 → 200 MB (marginal)
- N=10000 → 800 MB (move to ARPACK)

For problems where N > ~5000, use ARPACK (Phase 2) or a sparse iterative method.

---

## Quick Reference: The Two Lines That Matter Most

```
HOOK A (assembly):  imp_solv.F  ~line 1302  → add CALL IMP_LAPACK1(...)
HOOK B (solve):     lin_solv.F  ~line 577   → add ELSEIF (ISOLV==10) CALL IMP_LAPACK2(...)
```

At Hook A, every matrix listed in Section 3 is fully assembled and correct.  
At Hook B, `F(NDDL)` is the RHS and you write `X(NDDL)` as the solution.

---

*Guide written for OpenRadioss commit history as of 2026-06 — main branch.*  
*eigen_mat project: `/home/wsl/OpenRadioss/eigen_mat/`*  
*OpenRadioss source: `/home/wsl/OpenRadioss/OpenRadioss/`*
