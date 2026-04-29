# T1 Baseline — Step 1 of T2_mvp.md

Captured: 2026-04-29
Branch: `lukas/erts/beamjit2`
Commit: (run `git rev-parse HEAD` for current)
Build: `./otp_build configure && make` from clean tree, default flags.
Platform: aarch64-apple-darwin25.4.0 (Apple Silicon, macOS).

## Setup

```bash
cd PLAN/mvp
$ERL_TOP/bin/erlc t2_mvp.erl
$ERL_TOP/bin/erl -noshell -pa . -eval 'Stats = t2_mvp:time_run(1_000_000, 50), io:format("~p~n", [Stats]), init:stop().'
```

To regenerate the disassembly:

```bash
cd /tmp && $ERL_TOP/bin/erl +JDdump true -pa $ERL_TOP/PLAN/mvp -noshell -eval 't2_mvp:run(10), init:stop().'
cp t2_mvp.asm $ERL_TOP/PLAN/mvp/t1_baseline.asm
```

## Numbers

`run(1_000_000)` — net total of 1M-element transaction list:

| metric | value |
|--------|-------|
| median | 1.85 ms |
| min    | 1.77 ms |
| max    | 6.96 ms (one outlier — first run, jitter) |
| ns/iter (median, n = 1M) | **~1.85 ns/iter** |

`run(1000)` correctness check returns `497497`, matching the analytic
result `sum(i=1..1000) - sum(i rem 7 for i=1..1000) = 500500 - 3003`.

## Per-iteration disassembly (T1)

The hot loop is the body of `total/2` reached by the tail-call back to
the function entry. Reproduced from `t1_baseline.asm` lines 138–192:

```
total/2:
  ;; function prologue (executed on every iteration via tail-call)
  str x30, [x20, -8]!         ; push CP onto Erlang stack
  b   L47                      ; skip i_breakpoint_trampoline
  bl  L27                      ; (trampoline target)
L47:
  adr x2, total/2              ; i_test_yield: set p->i for resume
  subs w22, w22, 1             ; FCALLS--
  b.le L29                     ; yield if zero

  ;; is_nonempty_list X[0]
  tbnz x25, 1, @label_8-3      ; bit 1 of list ptr → empty/improper

  ;; get_list X[0] -> head/tail
  and x8, x25, -8              ; untag list ptr
  ldp x27, x28, [x8]           ; head -> x27, tail -> x28

  ;; is_tuple_of_arity head, 2
  tbnz x27, 0, label_6         ; not boxed
  and x0, x27, -8              ; untag tuple ptr
  ldr x8, [x0]                 ; load arity word
  cmp x8, 128                  ; arity-2 header
  b.ne label_6

  ;; allocate 2 stack slots + heap-and-stack check
  add x2, x23, 48              ; HTOP + 48 (worst-case bignum headroom)
  cmp x2, x20                  ; vs E
  b.ls L49
  mov x3, 4
  bl  L32                      ; GC entry
L49:
  sub x20, x20, 16             ; allocate 2 stack slots

  ;; store_two_values: save tail (x28) and Net (x26) across call
  stp x28, x26, [x20]

  ;; load_tuple_ptr + get_two_tuple_elements
  and x0, x27, -8              ; untag tuple ptr
  ldp x25, x26, [x0, 8]        ; A -> x25, F -> x26

  ;; i_call diff/2  — full call frame
  bl @diff/2-4

  ;; i_plus: Net + result, with combined small-int + overflow check
  ldr x1, [x20, 8]             ; reload Net from stack
  and x8, x25, -16             ; untag the call result (x25)
  adds x0, x1, x8              ; Net + result (overflow flag set)
  and x8, x1, x25              ; tag-bit AND
  and x8, x8, 15
  ccmp x8, 15, 0, 9            ; both small AND no overflow?
  b.eq L51                     ; ok
  mov x2, x25
  bl  L53                      ; slow path: bignum / not-small handler
L51:
  mov x26, x0                  ; new Net = result

  ;; move_call_last (tail call): pop and branch
  ldr x25, [x20], 16           ; pop tail back into x25 (and free slot)
  ldr x30, [x20], 8            ; pop CP
  b   total/2                  ; branch back
```

…and `diff/2` itself adds another **14 instructions of overhead** per
call before any useful work happens (prologue: 2 push + 1 b, yield
check: 3 instructions; epilogue: 4 instructions; the actual subtract
is only ~5 instructions).

## Instruction count (fast path, per iteration)

Reproduced from the disasm above (excluding labels, slow paths, and
the trampoline `bl L27`):

| Section | Insn count |
|---|---|
| Prologue / yield (`str x30`, `b L47`, `adr`, `subs`, `b.le`) | 5 |
| `is_nonempty_list` (tbnz) | 1 |
| `get_list` (and + ldp) | 2 |
| `is_tuple_of_arity` (tbnz + and + ldr + cmp + b.ne) | 5 |
| `allocate_tt` heap+stack check (add + cmp + b.ls + sub) | 4 |
| `stp` save | 1 |
| Tuple ptr untag + element load (and + ldp) | 2 |
| `bl @diff/2-4` | 1 |
| **Inside `diff/2`**: prologue (str + b) | 2 |
| **Inside `diff/2`**: yield (adr + subs + b.le) | 3 |
| **Inside `diff/2`**: arith (and + subs + and + and + ccmp + b.eq) | 6 |
| **Inside `diff/2`**: epilogue (mov + ldr + subs + b.mi + ret) | 5 |
| `i_plus` after return (ldr + and + adds + and + and + ccmp + b.eq) | 7 |
| `mov` commit | 1 |
| Tail-call (ldr + ldr + b) | 3 |
| **Total fast-path instructions / iter** | **~48** |

## What T2 needs to remove

T2's job is to bring this down. With the inlining + tag fusion + raw
arithmetic from `T2_mvp.md`:

- The `bl @diff/2-4` call frame is gone (-1 + ~16 inside-diff = **-17**).
- Tag and arity checks fuse: 5 (`is_tuple_of_arity`) + 7 (`i_plus`)
  = 12 instructions collapse to ~5 with a combined header compare and
  a single fixnum-tag check on `(A | F)`.
- Heap-headroom of 48 is no longer needed (no bignum path on fast path);
  may keep stack-only check.
- Net stays in a register across iterations; the `stp`/`ldp` save/reload
  is gone (-2 saved, -1 reloaded post-call = **-3**).
- The post-call `i_plus` reload of Net (`ldr x1, [x20, 8]`) is gone
  (-1).

Estimated T2 fast-path: ~15–18 instructions / iter — a **~3× reduction
in instruction count**. Whether this translates to ≥2× wall-clock
depends on aarch64 microarch behaviour (out-of-order execution can
hide some of the redundant work).

## Status of MVP plan steps

- [x] Step 1: Benchmark + T1 baseline.
- [ ] Step 2: Side-exit round-trip.
- [ ] Step 3: SSA → asmjit codegen (no inlining).
- [ ] Step 4: Inlining of `diff/2`.
- [ ] Step 5: Measurement + writeup.
