# GenevaERS Performance Engine — Architecture

## Architecture overview: MR95/MR96 — a two-pass JIT compiler wrapped in a multi-threaded I/O harness

### The fundamental design

`GVBMR95` is **not an interpreter**. The Logic Table (LT) is a compiler IR, and the system JIT-compiles it into raw z/Architecture machine code before any event record is touched. This is the central fact everything else flows from.

---

### Phase 0 — Program startup (`GVBMR95` entry)

`GVBMR95` enters in AMODE 31 and immediately executes `SAM64` to switch to 64-bit addressing for the rest of its life. All 16 GPRs are zero-extended with `LLGTR` to eliminate high-order garbage before any pointer use.

Storage for the main **thread work area** (`THRDLEN`) is obtained with `CHECKZERO=YES` — the kernel is asked to confirm whether the storage is pre-zeroed so the expensive `MVCL` fill can be skipped.

`GVBURALI` is called immediately after startup. The returned 8-byte module alias name (stored in `NAMEPGM`) governs every subsequent file open — `EXTRRPT`/`REFRRPT`, `EXTRLOG`/`REFRLOG`, `EXTRTRAC`/`REFRTRAC`, `EXTRDUMP`/`REFRDUMP`. There is no separate PARM-driven mode switch; **the alias name is the mode selector**. MR95 then calls `GVBMR96` via `BASSM`.

---

### Phase 1 — `GVBMR96` initialization (the compiler front-end)

MR96 runs entirely before the first event record is ever read. Its job is to take the Logic Table from disk and produce executable machine code in memory. The mainline sequences these steps:

**Step 1 — VDP and Logic Table load (`VDPLOAD` → `LTBLLOAD`)**

`VDPLOAD` reads the VDP file and chains all record types into linked lists by type (type 50 = control record, type 200B = file records, type 210 = exit program records, type 1000 = view records). Each record type gets its own `first_NNN` anchor pointer so MR96 can find records via `LOCVIEW`, `LOCEXIT`, `LOCCID` helpers.

`LTBLLOAD` reads the Logic Table sequentially into a contiguous in-memory array. The `LOGICTBL` DSECT header for each row carries: `LTMAJFUN`/`LTSUBFUN` (the 4-char function code), `LTTRUE`/`LTFALSE` (branch-row pointers, still as row numbers at this stage), `LTCODSEG` (will hold the generated code address after PASS1), and a flags halfword encoding optimisation decisions.

**Step 2 — Thread area construction (`THRDBLD`)**

Thread count = `min(EXECDISK, actual_disk_LFs)` + `min(EXECTAPE, actual_tape_LFs)` + pipe/other count. For each thread, `THRDCOPY` calls `GETMAIN` for a `THRDLEN + thread_vars_sz` block and chains the areas through `THRDNEXT`. Thread-level Logic Table variables (`NV`-type) are initialised here — packed-decimal fields via `ZAP`→`P'0'`, binary fields via `XC`.

**Step 3 — Clone RE/ES sets (`CLONLTBL`)**

When multiple threads share a logical file, the RE/ES set is cloned. Each clone receives its own literal pool copy (`MVCL`) and its own lookup-buffer chain, with updated `LTESLPAD`/`LTESLPSZ` fields.

**Step 4 — Memory-resident reference tables (`LOADLKUP`)**

Reference files are read in their entirety and sorted into binary-searchable in-memory tables. Table sizes are derived from the LT view-column definitions.

**Step 5 — Literal pool allocation (`ALLOCLIT`)**

One **1 MB page-aligned pool** is allocated per work unit (thread). This becomes the per-thread literal pool — a flat buffer that PASS1 fills with per-ES data: the `LITP_HDR` header, token-offset arrays, per-view accumulator areas, and serialised addresses of runtime variables. The base register (`R2` at runtime) is set to `pool_start + 512K` so that all offsets fit within ±512K, enabling 20-bit signed displacements throughout the generated code.

**Step 6 — PASS1: code generation (the JIT compiler)**

PASS1 walks every LT row in sequence and for each row:

1. Looks up the **function table entry** (`LTFUNTBL`) for the model code address (`FCMODELA`) and length (`FCCODELN`).
2. Copies the model code verbatim into the code buffer with `EX Rx,MVCMODEL`.
3. Saves the code buffer address back into `LTCODSEG` of the LT row — this is how PASS2 and the runtime find each row's code.
4. For `NV` (new-view) rows: writes the `NVPROLOG` — a fixed-layout header holding `NVNXVIEW` (next-view prolog address), `NVLAYR8` (a `LAY R8,offset(R7)` instruction whose displacement PASS2 will patch with the actual extract-record column offset), and `NVNOP` (a `JLU *+offset` whose target PASS2 patches for disabled-view skipping).
5. For `ES` (end-of-set) rows: installs an `EVNTPREV` branch back to the event loop.
6. For lookup-prefix rows (`LTLKUPRE` flag set): prepends a fixed-length prefix code block before the main code segment.
7. **CFCC verb** (constant-field compare-compare): resolves the comparison at compile time using `EX` on a model `CLC` instruction, then substitutes either NOOP code or a false-branch stub. No runtime comparison ever runs for a CFCC row.
8. **Cookies** (date/time tokens with negative length fields): resolves `RunDay`, `RunMonth`, `RunYear`, fiscal quarter boundaries, etc. to actual date values at compile time. No cookie expansion happens at event-record time.

**Step 7 — Open extract files (`OPENEXTF`)**

Walks the LT for `WR_XT` rows, finds each associated `VDP0200B` record, determines the file type (QSAM, VSAM, PIPE, NULL-PIPE, TOKEN), and opens DCBs in AMODE 31 (`SAM31`/`OPEN`/`SAM64`). Pipe/null-pipe cases patch the `EXTPUTA`/`EXTCHKA`/`EXTPUT_6431`/`EXTCHK_6431` function vectors in the `EXTFILE` control block to the appropriate stubs. Page-fixing (`EXECPAGF=Y`) sets `DCBEFLG3|=DCBEBFXU`.

---

### Phase 2 — Back in `GVBMR95`: PASS2 and execution

**PASS2 — branch relocation**

PASS1 left branch targets as raw row addresses. PASS2 converts them to **relative halfword displacements** by walking the relocation table (`FCRELOCA`) entry by entry:
- Types 12/13 → true-branch displacement: load `LTTRUE` row address, find its `LTCODSEG`, subtract current position, halve, store via `STH`.
- Types 14/15 → same via `LTFALSE`.
- Type 29 → title-key column offset: compute `EXSRTKEY-EXTREC + LTSORTLN + LTFLDPOS`, patch the `MVC` displacement.
- Branches targeting `NV` or `ES` rows: special-cased through `NVNXVIEW` to find the next-view code address before computing the displacement.

After PASS2, the generated code is fully self-contained z/Architecture machine code with real branch displacements. The SNAPped dump (`DUMP_LT_AND_GENERATED_CODE=Y`) writes this to `EXTRDUMP`/`REFRDUMP`.

**Enclave and ZIIP setup**

If APF-authorized: `MODESET SUP` → `IWM4ECRE` (create dependent enclave) → `SYSEVENT ENCASSOC JOIN` → `IWMEJOIN` → call ZIIP module with `ZIIP_OCT` to enable SRB offload. The enclave token is saved for teardown.

**Thread dispatch**

- *Single-thread* (`EXECUTE_IN_MAIN_TASK=1` or `A`): `PICKEVNT` selects the first event file, `R13` switches to the thread work area, branches to `MAIN`.
- *Multi-thread*: `EVENTS ENTRIES=(N+1)` allocates the ECB wait-list; `ATTACHX EP=MR95THRD,SHSPV=15,SZERO=YES` launches each subtask. One extra ECB slot is reserved for the ESTAE. The main task waits in an `EVENTS WAIT` loop, handling ESTAE-stop and daughter-task completion events.

**`MAIN` — per-thread initialization**

Each thread (main task or attached subtask):
1. Saves FP8–FP15; loads `FP8/10 = 0`; loads `FP9/11 = DFP_QUANTUM` (these FPR pairs must never be clobbered by generated code — they hold the DFP zero and quantum values used by accumulator arithmetic throughout the run).
2. Allocates **Pause Element Tokens** (`IEA4APE`) — TCB PETs and SRB PETs for the PAUSE/RELEASE write-serialisation protocol.
3. Installs ESTAE (`ESTAEX`), sets PSW overflow mask.

**`RESTART` → per-file setup**

Resets per-file thread variables, copies DDNAMEs from the LT RE row, optionally calls `DYNALLOC` (SVC 99 via `GVBUR35`) to allocate the dataset, then dispatches to the I/O driver via a `SELECT` on the access-method ID:

| Access method code | Driver |
|---|---|
| `SEQFILE` | `GVBMRBS` |
| `KSDSFILE` | `GVBMRVK` |
| `DB2SQL` | `MRSQADDR` → `GVBMRSQ` |
| `DB2HPU` | `MRSUADDR` → `GVBMRSU` |
| `CALLADA` | `MRADADDR` → `GVBMRAD` |

Write exits (`LTWRADDR`) and lookup exits (`LBSUBADR`) are then called with their OP (initialize) function code by walking all NV rows in the ES.

**`EVNTLOOP` — the hot path**

```
R6 += GPRECLEN          advance to next record in buffer
if R6 >= EODADDR        buffer exhausted:
  ├── TCB→SRB mode switch (ZIIP) if needed
  ├── call EVNTREAD (GVBMRBS EXCP read)
  └── SRB→TCB mode switch after I/O completes

STG R6, RECADDR           save event record pointer
STY R0, GPEVENTA          write pointer into GENPARM

R7 = GPEXTRA              extract record work area
R8 = THRDES               ES row address
R2 = THRDLITP + 512K      literal pool base
R15 = LTESCODE            generated ES entry point
BR  R15                   → EXECUTE GENERATED CODE
```

The generated code runs the full view set for this ES. It calls back into `GVBSRCHR` for binary-search lookups and `GVBDL96` for field formatting. When a WR row fires, the `EXTPUTA` / `EXTPUT_6431` vector in the `EXTFILE` control block is invoked to write the extract record.

**Teardown**

After all threads complete: `IWMEQTME` gathers enclave CPU/ZIIP times → `CLOSFILE` → `PREP_SORTS` → report routines (`ISRC`, `OWRT`, `LKUP`, `FINAL`) → `TERM_UEXITS` → `SYSEVENT ENCASSOC_LEAVE` → `IWMELEAV` → `IWM4EDEL` → `PR` (pop linkage stack, return).

---

### End-to-end flow summary

```
GVBMR95 entry (A31 → A64, BAKR)
  │
  ├─ GVBURALI       → 8-byte alias name → all DDNAMEs
  ├─ Open EXTRLOG/REFRLOG
  └─ BASSM → GVBMR96
               │
               ├─ VDPLOAD    → linked lists of VDP record types (50, 200B, 210, 1000…)
               ├─ LTBLLOAD   → in-memory LT array
               ├─ THRDBLD    → N thread work areas + thread-variable areas
               ├─ CLONLTBL   → copy RE/ES sets + literal pools per extra thread
               ├─ LOADLKUP   → reference files → sorted in-memory tables
               ├─ ALLOCLIT   → 1 MB literal pool per work unit
               ├─ PASS1      → JIT compile: model code → code buffer
               │               resolve cookies and CFCC at compile time
               │               set LTCODSEG per row
               └─ OPENEXTF   → open extract DCBs, patch write vectors
  │
  └─ back in GVBMR95
      ├─ PASS2       → patch true/false branch displacements, NV offsets
      ├─ Enclave     → IWM4ECRE + IWMEJOIN + ZIIP offload enable
      ├─ ATTACHX     → N−1 subtasks, each enters MAIN
      └─ MAIN (per thread)
           ├─ IEA4APE  → allocate Pause Element Tokens
           ├─ ESTAEX   → install error/abend recovery
           ├─ RESTART  → per-ES file open + exit OP calls
           └─ EVNTLOOP
                ├─ advance R6 through buffer
                ├─ EVNTREAD if buffer exhausted (TCB ↔ SRB flip for ZIIP)
                └─ BR LTESCODE  ← generated machine code executes
                     ├─ GVBSRCHR  (binary-search lookup)
                     ├─ GVBDL96   (field format/conversion)
                     └─ EXTPUTA   (write extract record)
```

> **Key insight:** The Logic Table is **never interpreted at event-record time**. By the time MR95 reads the first event record, the LT has been fully compiled into native z/Architecture machine code with real branch displacements, accumulator offsets baked into `LAY` instructions, and all constant comparisons pre-resolved. The only runtime overhead per event record is the I/O and the execution of that generated code.

---

### How a job actually runs: the three-step execution pattern

The architecture above describes what happens inside a single MR95 invocation. A real production job — such as the [Demo `RUNEXT1.JCL`](https://github.com/genevaers/Demo/blob/main/JCL/RUNEXT1.JCL) — orchestrates **three distinct steps** that must run in order, and understanding that sequence is essential context for anyone working with this codebase.

#### Step 1 — Control file generation (Java RCA, `PSTEP200`/`JAVA`)

Before MR95 is invoked at all, a Java program (`org.genevaers.rcapps.Runner`) reads a Workbench XML export (`WBXMLI`) containing the view definitions and compiles it into three binary control files:

| DD | Dataset suffix | Content |
|---|---|---|
| `VDPNEW` | `.VDP` | View Definition Parameters — all view, file, column, and exit metadata (RECFM=VB, LRECL=32756) |
| `JLTNEW` | `.JLT` | Join Logic Table — logic for the reference phase (RECFM=VB, LRECL=820) |
| `XLTNEW` | `.XLT` | Extract Logic Table — logic for the extract phase (RECFM=VB, LRECL=820) |

The `DBVIEWS` DD selects which view IDs to include. The `RCAPARM` DD controls the compiler: `INPUT_TYPE=WBXML`, `GENERATE_RC_FILES=Y`, and report flags. **These three files are the sole input to every subsequent MR95 run.**

#### Step 2 — Reference phase (`GVBMR95R`, `PSTEP505`)

The first MR95 execution runs as the alias `GVBMR95R`. It uses the **JLT** (Join Logic Table) and its job is exclusively to sort and pre-stage the reference (lookup) data into files that the extract phase can load directly into memory.

**Key DD names for `GVBMR95R`:**

| DD | Role |
|---|---|
| `MR95VDP` | The `.VDP` file from step 1 |
| `REFRLTBL` | The **`.JLT`** (Join Logic Table — reference phase logic) |
| `REFRPARM` | Execution parameters |
| `REFRREH` | Reference extract header file (RECFM=FB, LRECL=100) — indexes into the RED files |
| `REFR001`–`REFR006` | Pre-sorted reference data files (RECFM=VB, LRECL=4144) |
| `REFRRPT` / `REFRLOG` / `REFRTRAC` | Diagnostic output |

**`REFRPARM` parameter keywords (all optional, shown with defaults):**

```
DISK_THREAD_LIMIT=10          default: 9999
TAPE_THREAD_LIMIT=10          default: 9999
IO_BUFFER_LEVEL=8             default: 4
OPTIMIZE_PACKED_OUTPUT=N      default: Y
PAGE_FIX_IO_BUFFERS=N         default: Y
TREAT_MISSING_VIEW_OUTPUTS_AS_DUMMY=Y   default: N
ABEND_ON_CALCULATION_OVERFLOW=N         default: Y
TRACE=Y                       default: N
DUMP_LT_AND_GENERATED_CODE=Y  default: N
SOURCE_RECORD_LIMIT=100       default: (no limit)
EXECUTE_IN_MAIN_TASK=1        default: N
```

#### Step 3 — Extract phase (`GVBMR95E`, `PSTEP705`)

The second MR95 execution runs as the alias `GVBMR95E`. It uses the **XLT** and reads both the pre-staged reference data from step 2 and the actual event (source transaction) files.

**Key DD names for `GVBMR95E`:**

| DD | Role |
|---|---|
| `MR95VDP` | Same `.VDP` file — shared between both phases |
| `EXTRLTBL` | The **`.XLT`** (Extract Logic Table — extract phase logic) |
| `EXTRPARM` | Execution parameters |
| `EXTRREH` | The reference header file written by `GVBMR95R` |
| `REFR001`–`REFR006` | The pre-sorted `.RED` files from step 2 — loaded into memory by `LOADLKUP` |
| `ORDIT001`–`ORDIT003` | Event (source/driver) files |
| `EXTR001`–`EXTR003` | Extract output files for the format phase (`.EXT`, RECFM=VB, LRECL=8192) |
| `SORT001`–`SORT003` | Sort control records for each extract file |
| `OUTPUT01`–`OUTPUT03` | Direct extract-only view outputs — bypasses format phase entirely |

**`EXTRPARM` additional keywords (superset of reference phase):**

```
RUN_DATE=20170105             default: current date
FISCAL_DATE_DEFAULT=20161231  default: RUN_DATE
FISCAL_DATE_OVERRIDE=1:20160731   default: FISCAL_DATE_DEFAULT
EXECUTE_IN_PARENT_THREAD=A    default: N
```

`RUN_DATE` and `FISCAL_DATE_*` feed the cookie resolution at PASS1 time — they become the concrete date values substituted for `RunDay`, `RunMonth`, `FiscalDay` etc. tokens in the LT.

#### Step 4 — Chained format jobs (`JSTEPNX1`–`JSTEPNX3`)

The final steps use `IEBGENER` to submit up to three format jobs (`RUNFMT1`–`RUNFMT3`) to the internal reader. Each format job runs `GVBMR87`+`GVBMR88` against one of the `EXTR00x`/`SORT00x` extract file pairs.

#### The complete job-to-job data flow

```
WBXMLI (Workbench XML)
    │
    └─ Java RCA ──────────────────────────────────┐
                                                   │
                         ┌─────────────────────────┤
                         │   .VDP   .JLT   .XLT    │
                         └──────┬──────────────────┘
                                │
            ┌───────────────────┼──────────────────────┐
            │                   │                      │
     GVBMR95R (alias)     GVBMR95E (alias)        (shared VDP)
     reads: .JLT              reads: .XLT
     reads: CUSTNAME etc       reads: REFRREH
     writes: REFRREH           reads: REFR001-006
             REFRRTH           reads: ORDIT001-003
             REFR001-006       writes: EXTR001-003
                                       SORT001-003
                                       OUTPUT01-03
                                           │
                               ┌───────────┴────────────┐
                           RUNFMT1               RUNFMT2/3
                        GVBMR87+MR88           GVBMR87+MR88
                        reads: EXTR001          reads: EXTR002/3
                               SORT001                 SORT002/3
                        writes: reports         writes: reports
```

> **Note:** The `.JLT` and `.XLT` are separate compilations of the same view set, optimised for their respective phases. A single `.VDP` file is shared by both — it is the complete metadata catalogue for the run.

---

## Postscript: Doug Kunkel and the memory architecture drawings

The code in this repository is the product of more than four decades of continuous engineering. The Performance Engine was first developed in the early 1980s when **Rick Roth** and **Doug Kunkel** worked on the Alaska Statewide Accounting System, developing the large-scale, single-pass reporting concepts that everything since has been built upon. Through Oregon DOT (1987–1990) and Wyoming DOT (1989–1992), the extract engine was converted to assembler and given its first name: GENEVA. Development continued through the Sacramento-based GENEVA Development Center (1992–1996), major financial services deployments (1996–2010), IBM's SAFR era, and — since April 2020 — as an open source project under the Linux Foundation's Open Mainframe Project.

**Doug Kunkel** was the principal inventor of the extract engine's memory architecture. In 1994, while at the Sacramento Development Center, he drew a set of technical diagrams to explain the internal memory structures and pointer chains of MR95 to the maintenance team. These are the earliest surviving technical documentation of the structures this architecture document describes. Doug died while still a member of the team. His work is present in every `LTCODSEG` pointer, every `NVPROLOG` field layout, and every binary-search path in this repository.

The four 1994 drawings are reproduced here. Images are from the [GenevaERS project website](https://genevaers.org/history/1994/06/11/technical-diagrams-internal-memory-structures/) and are owned by the GenevaERS project.

---

### Drawing 1 — Sub-Task Work Areas

![Sub-Task Work Areas](slide2.jpeg)

This diagram shows the **thread control area (TCA) hierarchy** established during `THRDBLD` in MR96. A single main TCA (top) spawns *n* reader sub-task TCAs, each chained through `THRDNEXT`. At runtime, `ATTACHX` launches one OS sub-task per TCA; each sub-task's R13 is loaded from its own TCA so that all per-thread state — Logic Table variable areas, literal pool pointer, I/O buffers, and ESTAE save area — is completely independent. This is the foundation of the parallel I/O model: multiple reader threads process separate logical files simultaneously, with no shared mutable state between them.

---

### Drawing 2 — Extraction Logic Management

![Extraction Logic Management](slide3.jpeg)

This diagram shows the **JIT compilation output** and how the runtime navigates it. The Logic Table (left) contains rows with NR (new-reader) and other function codes. Each row's `LTCODSEG` field (set by PASS1) points to its generated machine-code segment in the code buffer (right, "Generated Machine Code"). Register R13 points to the Thread Control Area for the current reader, and R2 points into the per-thread literal pool (base = pool + 512K). At `EVNTLOOP` time, the engine loads `R15 = LTESCODE` from the ES row and branches — after that, only native machine code runs until the ES row's `EVNTPREV` branch returns control. The `NR` row shown corresponds to the start of a reader's ES set; its code segment is the entry point that PASS2 has already patched with real branch displacements.

---

### Drawing 3 — Reference Data Lookup

![Reference Data Lookup](slide1.jpeg)

This diagram shows the **in-memory lookup structure** built by `LOADLKUP` and navigated at runtime by `GVBSRCHR`. The Logic Table contains `RL` (reference lookup) and `DL` (direct lookup) rows; each holds a pointer into a chain of buffer-descriptor blocks (centre column). Each buffer descriptor in turn carries a pointer into the Reference Table (right) — a contiguous, binary-searchable in-memory array loaded from the pre-sorted `.RED` files produced by the reference phase (`GVBMR95R`). The Thread Control Area (top left, "reader") anchors the chain. `GVBSRCHR` walks this structure: it loads the RL/DL pointer from the LT row, follows the buffer descriptor chain, and performs a binary search on the Reference Table. The null-pointer terminator at the bottom of the buffer chain marks end-of-set.

---

### Drawing 4 — View Extract Data Management

![View Extract Data Management](slide4.jpeg)

This diagram shows the **extract output path** — what happens when a WR (write) LT row fires. Multiple reader Thread Control Areas (left) share an Extract File Table (centre), which maps each logical extract file to a File Control Block. The FCB contains the `EXTPUTA`/`EXTPUT_6431` function vector patched by `OPENEXTF` and a linked ring of Data Buffers. The writer Thread Control Area (top right) owns the current write buffer pointer; reader threads write into the current buffer and, when it fills, the writer TCA drains it to the extract file via BSAM/QSAM. Readers that arrive while the writer is flushing are held on the Wait Queue (left, "WAIT QUEUE") using the Pause Element Token protocol (`IEA4APE`/`PAUSE`/`RELEASE`). This design allows multiple reader threads to produce extract records concurrently with zero contention on the common buffers except at the flush boundary.
