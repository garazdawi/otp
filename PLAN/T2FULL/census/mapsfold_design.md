# maps:fold T2 specialization — Stage 1 implementation spec

Exploration goal: how far can T2 be pushed with type-profiling + cross-module
inlining + loop opts, on the real dominating hot function (maps:fold_1 =
26.7% of a map-heavy service's own-time, PLAN/T2FULL/20). This spec is the
**smallest end-to-end runnable slice**. Purge/trace safety is OUT OF SCOPE
for now (assume code doesn't change under us).

## Validated framing (measured)

- **Unit = the caller**, already T2-eligible. `fold_bench:sum_vals/1 =
  maps:fold(fun(_K,V,A) -> A+V end, 0, Map)` decodes to `make_fun3`
  (supported) + `call_ext_only {maps,fold,3}` (supported); the lambda
  `-sum_vals/1-fun-0-/3` (`A+V`) is eligible. T2 already builds the caller
  SSA with a `CallExt{maps,fold,3}` whose fun operand resolves to a
  `MakeFun`. Sibling of the lists:foldl intrinsic (t2_intrinsics.cpp, wired
  t2_compile.cpp:360).
- **Win is real and needs the direct walk.** Measured T1 vs proxies:
  removing the iterator but keeping a per-element fun (values+foldl) buys
  ~0; the ~2x win requires no per-element fun call AND a tight direct
  reduce (lists:sum(maps:values) ≈ 2x faster than maps:fold at size 32).
  The T2 direct flatmap walk (inlined add, no allocation) should match or
  beat that. So the backend ops below are the win, not a last 20%.
- **Correctness oracle:** fold_bench checksums are identical across sizes;
  the T2 result MUST equal T1. Order-sensitive check: `fun(K,_,A)->[K|A] end`
  must equal T1 exactly (ascending key-array order — T1's map_next builds the
  chain in ascending array order, erl_map.c:3719-3748, so a direct i=0..n-1
  walk matches).

## Flatmap internals (erl_map.h)

- `flatmap_t { Eterm thing_word; Uint size; Eterm keys; }` (erl_map.h:57-73).
  Values are INLINE after the header: `flatmap_get_values(mp) = (Eterm*)mp+3`
  (MAP_HEADER_FLATMAP_SZ=3, erl_map.h:89,198). Byte offsets from the
  UNTAGGED map ptr: size at +8 (raw Uint), keys term at +16, V_i at +24+8*i.
- Keys: `flatmap_get_keys(mp) = tuple_val(mp->keys)+1` (erl_map.h:90) — untag
  the keys term (boxed), skip the arity word; K_i at +8+8*i from untagged
  keys-tuple ptr.
- Header discriminator: `thing_word & _HEADER_MAP_SUBTAG_MASK` (erl_map.h:168)
  == `HAMT_SUBTAG_HEAD_FLATMAP` (erl_map.h:215) for flatmap.
- Size cap: `MAP_SMALL_MAP_LIMIT` = 32 (3 in DEBUG — USE THE CONSTANT, not a
  literal, or debug/opt diverge; erl_map.h:93-97).

## The shape guard (7 aarch64 instrs; T1 precedent instr_map.cpp:415-436)

`maps:fold/3`'s 3rd arg may legally be an iterator term, so unlike T1's
get_map_elements we MUST boxed-check first:
```
and TMP0,src,#_TAG_PRIMARY_MASK ; cmp TMP0,#TAG_PRIMARY_BOXED ; b.ne else
untag TMP1,src ; ldp TMP2,TMP3,[TMP1]            ; thing_word + size
and TMP2,TMP2,#_HEADER_MAP_SUBTAG_MASK ; cmp TMP2,#HAMT_SUBTAG_HEAD_FLATMAP ; b.ne else
cmp TMP3,#MAP_SMALL_MAP_LIMIT ; b.hi else
```

## Deopt model (the elegant part — NO new deopt machinery)

- **Slow edge = the ORIGINAL `CallExt{maps,fold,3}`** moved to a slow block
  with its original sync map/operands/dst_reg. Non-flatmap is not an error;
  it's the general case. Hashmaps run at T1 speed minus one guard. Nothing
  baked in → NO maps dep in dep_hdrs (only the own-module dep for the inlined
  fun, as t2_compile.cpp:413-426 already records).
- **Every fast-path deopt = "re-execute the erased call":** side-exit to the
  site's own T1 PC `ERTS_T2_PC_CALL` (erts_t2_pc_lookup_kind, kinds
  t2_pctab.h:74-83; the intrinsic already requires this entry,
  t2_intrinsics.cpp:1182-1190) with the call-boundary sync map `cmap`
  (Fun,Acc0,Map in X0..X2 + live frame). Sound because the fast loop is
  effect-free/alloc-free and writes nothing to X0..X2 or Y until exit, so the
  call-boundary state is intact. Mechanically the existing trampoline:
  `fail_label_num(call_t1_pc,nullptr)` → emit_fail_trampolines
  (t2_emit.cpp:523-554), no CP push.
- **One new flag `T2_OP_SPEC_CALLSITE = 1<<10`** (free bit; t2_hir.hpp:402-477).
  Resolved in spec_deopt_pc (t2_isel.cpp:333-358) as pc_lookup(beam_idx,
  ERTS_T2_PC_CALL), t1_pc_cont=nullptr. All synthesized ops carry
  beam_idx = call->beam_idx (intrinsic already does this).

## Reductions: batch the whole fold (≤32 iters) as one FoldBudget

Precedent: T1's map_next materializes all n elements in ONE non-yielding BIF
(erl_map.c:3731-3737). Preheader op:
```
TMP = R_const + R_per_elem*(size>>4) ; cmp FCALLS,TMP ; b.le <deopt CALL PC, uncharged>
sub FCALLS,FCALLS,TMP
```
R_const/R_per_elem MUST equal T1's real charges for the whole
fold→iterator→next→BIF→fold_1→{Fun,try_next,self}×n chain INCLUDING aarch64
return charges (lists accounting precedent t2_intrinsics.cpp:69-91). DO NOT
eyeball: measure process_info(self(),reductions) deltas around T1 folds at two
sizes, solve a+b*n, encode, and assert in a test. Expect ~a∈6..10, b≈5.

## Expansion template (§2.4)

```
b_pre:  ...orig ops, call removed
        IsFlatmapBounded(Map) -> b_fast | b_slow
b_slow: res_s = CallExt maps:fold/3 (F,A0,Map)   ; orig op, orig cmap ; jump b_join
b_fast: n = FlatmapSize(Map)                       ; tagged small, invariant
        FoldBudget(n)                    [deopt CALL PC, sync=cmap]
        i0 = ConstInt 0 ; jump b_head
b_head: acc = phi(A0,acc') @x3 ; i = phi(i0,i') @x4
        CmpLt(i,n) -> b_body | b_exit
b_body: k = FlatmapKeyAt(Map,i) @x5 ; v = FlatmapValAt(Map,i) @x6
        <spliced fun body, args (k,v,acc), fresh homes from x7,
         convert_arith → AddSmall/SubSmall stamped T2_OP_SPEC_CALLSITE>
        acc' = ret (committed @x3, direct-commit as t2_intrinsics.cpp:1582-1600)
        i'  = AddSmall(i,1) @x4          ; provably in-range, deopt CALL PC ; jump b_head
b_exit: res_f = Copy acc @res_home ; jump b_join
b_join: res = phi(res_s,res_f) @res_home ; ...orig post-call ops
```
Loop-carried acc,i in X≥3 as TAGGED smalls (i tagged: +（1<<4), untag lsr#4 in
loads; T2_LIR_VF_UNTAGGED forbids raw words in homes, t2_lir.hpp:557-567).
cmap->x_live==3 so X3+ dead at every deopt boundary. X0..X2 untouched till exit.

## Reused from t2_intrinsics.cpp unchanged

Site scan run() (:1801-1819); fun resolve resolve_copies→MakeFun live==0,
imm_int>=0, lambda arity check vs ret->lambdas (:1145-1167); admit_fun
(:441-513) + splice_fun (:526-899, SSA clone-by-value-map, fresh X homes, error
blocks → deopt block, sets T2_OP_INLINED) + fun_op_ok (:336-390); convert_arith
(:912-1009) — only the deopt CLASS of its guards changes (stamp
T2_OP_SPEC_CALLSITE not WINDOW_CALLEE, :990-1004); block split + result phi +
replace_value (:1246-1281,1748-1755,217-241); own-module dep (:1761-1770).
For maps: mfa_m==maps, mfa_f==fold, index==3, lam->arity-lam->num_free==3.

## File-by-file (§4)

1. **t2_hir.hpp** — T2OpKind: IsFlatmapBounded, FlatmapSize, FlatmapKeyAt,
   FlatmapValAt, FoldBudget (after GetMapElement ~:178); flag
   T2_OP_SPEC_CALLSITE=1<<10 (~:477).
2. **t2_hir.cpp** — t2_op_kind_name (:50); t2_op_produces_value (FlatmapSize/
   KeyAt/ValAt/IsFlatmapBounded yes, FoldBudget no); validator operand-count
   arms (value ops like GetTupleElement); run_speculation_checks accepts the
   callsite class (sync present, == call-boundary shape).
3. **t2_intrinsics.cpp** — expand_maps_fold_site(T2Op*call): recognition
   (mirror :1134-1228, only ERTS_T2_PC_CALL required, no CONT); block split
   (:1246-1281); §2.4 template; move orig call to b_slow; splice_fun(callee,
   {kv,vv,acc_phi},{x5,x6,x3}, deopt-block that SideExits CALL PC on fun error
   edges); convert_arith stamping T2_OP_SPEC_CALLSITE; own-module dep only.
   Scan hook in run(). Gate T2_NO_MAPS_INTRIN, trace T2_INTRIN_TRACE.
4. **t2_lir.hpp/.cpp** — LIR kinds IsFlatmapBounded (guard succ_then/else),
   FlatmapSize, FlatmapKeyAt, FlatmapValAt, FoldBudget (+names, not terminators).
5. **t2_isel.cpp** — 1:1 lowering; spec_deopt_pc (:333) callsite class;
   IsFlatmapBounded like other test+branch fusions; FoldBudget t1_pc_fail=
   pc_lookup(beam_idx,ERTS_T2_PC_CALL), sync=cmap, imm=R_per_elem,imm2=R_const.
6. **t2_emit.cpp** — 4 emitters (§ backend ops) + dispatch in emit_op(:597-769).
7. **t2_regalloc.cpp** — new value ops via generic srcs/dst liveness; ensure the
   two-edge guard kind is in kind-driven tables (mirror BsTestTail).
8. **t2_loop.cpp** — t2_validate_windows (:634): callsite-class rule — sync==cmap,
   dominated by shape guard, no write to X0..cmap->x_live-1, no frame op.

## Backend ops (§5)

| Op | Semantics | Fail |
|---|---|---|
| IsFlatmapBounded m | boxed ∧ subtag==FLATMAP ∧ size≤LIMIT; consumed by Branch | none (both edges in blob) |
| FlatmapSize m→small | ldr size(+8); tag lsl#4,orr#0xF; invariant, preheader | never |
| FlatmapKeyAt m,i→term | untag m; ldr keys(+16); untag; [ks+8+(i>>4)<<3] | never (i<n by guard) |
| FlatmapValAt m,i→term | untag m; [m+24+(i>>4)<<3] | never |
| FoldBudget n (effect) | TMP=imm2+imm*(n>>4); cmp FCALLS; b.le deopt; sub | uncharged side-exit CALL PC, sync=cmap |
| flag T2_OP_SPEC_CALLSITE | 3rd deopt class for SpeculateType/AddSmall/SubSmall | t1_pc_fail=CALL PC, no CP push |

No new global fragments, no stub classes, no runtime C helpers.

## Gates

Stage 1 done when: sum_vals ≥2x vs T1 at n∈{4,8,16,32} (or the measured
ceiling); result + order + reductions match T1; maps_SUITE green;
T2_NO_MAPS_INTRIN=1 restores baseline. Add a per-trampoline erts_t2_ stats
counter so deopt storms are visible (risk #3 — no re-tier machinery exists).
