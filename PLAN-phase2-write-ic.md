# Phase 2 (continued): Write Inline Cache for Flatmap Updates

## Context

The read IC is implemented and working: `i_get_map_element` passes `$NEXT_INSTRUCTION` as a callsite identifier, looks up the cached `(shape, key) → index` mapping in a per-scheduler table, and returns the value directly on hit.

Write operations (`update_map_assoc` and `update_map_exact`) currently just record attempt+miss counters with no actual caching. This plan adds write IC paths for the common single-key-update case.

## Scope Limitation: Single-Update Only

Both `erts_gc_update_map_assoc` and `erts_gc_update_map_exact` accept `n` key-value pairs (n/2 updates). Multi-key updates have complex merge logic that doesn't benefit much from caching a single index.

**Decision:** Only cache the `n == 2` case (one key, one value). For `n > 2`, fall through to the existing slow path immediately. This covers the overwhelmingly common Erlang pattern `M#{key => val}` or `M#{key := val}`.

## Two Write IC Flavors

### 1. Exact Update IC (`update_map_exact`, `M#{key := val}`)

**What it caches:** For a given callsite + shape + key → the index of the key in the flatmap.

**Fast path:** Validate shape, look up cached index, write new value at that index directly. The key tuple is unchanged (exact update never adds keys). This is structurally identical to the read IC — same shape, same key, same index — so we can **reuse the existing `ErtsMapIcReadEntry` table** and `erts_map_ic_try_get_flatmap_index`.

**Implementation:** The exact update fast path:
1. Check `n == 2` (single update)
2. Check `is_flatmap(map)`
3. Call `erts_map_ic_try_get_flatmap_index(new_p, map, key, &index)`
4. On hit: allocate new value array, copy all old values, overwrite `values[index]` with new value, reuse old `mp->keys`
5. On miss: run slow path, then call `erts_map_ic_update_flatmap_index` to fill

### 2. Assoc Update IC (`update_map_assoc`, `M#{key => val}`)

**What it caches:** Same as exact — the index of the key — but only for the **shape-preserving** case (key already exists in the map).

**Why no shape-changing cache:** When a new key is added, the output shape differs from the input shape. Caching the insert position + output shape adds significant complexity (transition caches). The shape-preserving case is the common one for `maps:put/3` or `M#{key => val}` on existing keys.

**Fast path:** Identical to exact update IC:
1. Check `n == 2` (single update)
2. Check `is_flatmap(map)`
3. Call `erts_map_ic_try_get_flatmap_index(new_p, map, key, &index)`
4. On hit: allocate new value array, copy all old values, overwrite `values[index]`, reuse old `mp->keys`
5. On miss: run full slow path, then fill cache only if key was found (shape preserved)

**Shape-changing case:** If assoc adds a new key (shape changes), the slow path handles it normally. The IC entry is NOT filled (the fill function already handles this: when `key_found == false`, it sets state to DISABLED). This means a callsite that always introduces new keys will be disabled after the first miss — correct behavior.

## Callsite Identity

Both update instructions pass `$NEXT_INSTRUCTION` as `new_p` to the C functions. This pointer is the address of the key-value pair data in the bytecode, which is stable and unique per callsite. We use `new_p` as the site pointer for IC lookups (same as read IC uses `$NEXT_INSTRUCTION`).

## IC Entry Reuse

The existing `ErtsMapIcReadEntry` structure and per-scheduler table are sufficient for writes:

```c
typedef struct {
    const void *site;   // new_p pointer (callsite identity)
    Eterm key;          // cached key (immediate only)
    Eterm shape;        // mp->keys (interned, stable)
    Uint index;         // key position in flatmap
    ErtsMapIcState state; // EMPTY / ACTIVE / DISABLED
} ErtsMapIcReadEntry;
```

Read and write callsites have different `$NEXT_INSTRUCTION` addresses, so they naturally hash to different slots. No structural changes needed.

## Implementation Steps

### Step 1: Add `erts_gc_update_map_exact` IC fast path

In `beam_common.c`, replace the current attempt+miss counters with actual IC logic:

```c
Eterm
erts_gc_update_map_exact(Process* p, Eterm* reg, Uint live,
                         Uint n, const Eterm* new_p)
{
    ...
    map = reg[live];

    if (erts_map_ic_enabled() && is_flatmap(map) && n == 2) {
        int ic_status;
        Uint index;
        Eterm new_key;

        erts_map_ic_note_attempt();
        GET_TERM(new_p[0], new_key);

        ic_status = erts_map_ic_try_get_flatmap_index(new_p, map, new_key, &index);
        if (ic_status > 0) {
            /* IC hit — shape-preserving exact update */
            Eterm new_val;
            flatmap_t *old_mp = (flatmap_t *)flatmap_val(map);
            Uint num_old = flatmap_get_size(old_mp);
            Uint need = num_old + MAP_HEADER_FLATMAP_SZ;
            Eterm *hp, *old_vals;
            flatmap_t *mp;
            Eterm res;

            if (HeapWordsLeft(p) < need) {
                erts_garbage_collect(p, need, reg, live+1);
                map = reg[live];
                old_mp = (flatmap_t *)flatmap_val(map);
            }

            hp = p->htop;
            res = make_flatmap(hp);
            mp = (flatmap_t *)hp;
            hp += MAP_HEADER_FLATMAP_SZ;
            mp->thing_word = MAP_HEADER_FLATMAP;
            mp->size = num_old;
            mp->keys = old_mp->keys;

            old_vals = flatmap_get_values(old_mp);
            sys_memcpy(hp, old_vals, num_old * sizeof(Eterm));

            GET_TERM(new_p[1], new_val);
            if (hp[index] == new_val) {
                /* Value unchanged — return original map */
                return map;
            }
            hp[index] = new_val;
            p->htop = hp + num_old;
            return res;
        } else if (ic_status < 0) {
            /* IC disabled for this site — fall through to slow path */
        } else {
            /* IC miss — run slow path then fill */
            goto exact_slow_path;
        }
    }

exact_slow_path:
    /* ... existing implementation ... */
```

The slow path fill for exact update: after the existing slow path completes successfully, call:
```c
if (erts_map_ic_enabled() && n == 2 && is_flatmap(map)) {
    Eterm new_key;
    GET_TERM(new_p[0], new_key);
    erts_map_ic_update_flatmap_index(new_p, map, new_key, 1 /* key_found */, matched_index);
}
```

We need to track the matched index through the slow path. The current exact update code doesn't explicitly track it, but we can compute it: `i` at the point where `EQ(*old_keys, new_key)` succeeds tells us the index.

### Step 2: Add `erts_gc_update_map_assoc` IC fast path

Similar structure, but only the shape-preserving case (key exists):

```c
Eterm
erts_gc_update_map_assoc(Process* p, Eterm* reg, Uint live,
                         Uint n, const Eterm* new_p)
{
    ...
    map = reg[live];

    if (erts_map_ic_enabled() && is_flatmap(map) && n == 2) {
        int ic_status;
        Uint index;
        Eterm new_key;

        erts_map_ic_note_attempt();
        GET_TERM(new_p[0], new_key);

        ic_status = erts_map_ic_try_get_flatmap_index(new_p, map, new_key, &index);
        if (ic_status > 0) {
            /* IC hit — shape-preserving assoc update (key exists) */
            /* Same fast path as exact update */
            ...
            return res;
        } else if (ic_status < 0) {
            /* IC disabled — fall through */
        } else {
            goto assoc_slow_path;
        }
    }

assoc_slow_path:
    /* ... existing implementation ... */
```

Slow path fill for assoc: after the existing slow path, determine whether any new keys were added (`changed_keys`). Only fill if no new keys were introduced:
```c
if (erts_map_ic_enabled() && n == 2 && is_flatmap(map) && !changed_keys) {
    Eterm new_key;
    GET_TERM(new_p[0], new_key);
    erts_map_ic_update_flatmap_index(new_p, old_map, new_key, 1, matched_index);
}
```

If `changed_keys` is true (new key added), fill with `key_found = 0` to disable the site:
```c
if (erts_map_ic_enabled() && n == 2 && changed_keys) {
    Eterm new_key;
    GET_TERM(new_p[0], new_key);
    erts_map_ic_update_flatmap_index(new_p, old_map, new_key, 0, 0);
}
```

### Step 3: Extract shared IC fast-path helper

The exact-update and assoc-update fast paths are identical (both do shape-preserving single-key value replacement). Extract a shared function:

```c
static Eterm
erts_gc_update_map_ic_hit(Process *p, Eterm *reg, Uint live,
                          const Eterm *new_p, Eterm map, Uint index)
{
    flatmap_t *old_mp = (flatmap_t *)flatmap_val(map);
    Uint num_old = flatmap_get_size(old_mp);
    Uint need = num_old + MAP_HEADER_FLATMAP_SZ;
    Eterm *hp, *old_vals, new_val;
    flatmap_t *mp;
    Eterm res;

    if (HeapWordsLeft(p) < need) {
        erts_garbage_collect(p, need, reg, live+1);
        map = reg[live];
        old_mp = (flatmap_t *)flatmap_val(map);
    }

    hp = p->htop;
    res = make_flatmap(hp);
    mp = (flatmap_t *)hp;
    hp += MAP_HEADER_FLATMAP_SZ;
    mp->thing_word = MAP_HEADER_FLATMAP;
    mp->size = num_old;
    mp->keys = old_mp->keys;

    old_vals = flatmap_get_values(old_mp);
    sys_memcpy(hp, old_vals, num_old * sizeof(Eterm));

    GET_TERM(new_p[1], new_val);
    if (hp[index] == new_val) {
        return map;  /* Value unchanged */
    }
    hp[index] = new_val;
    p->htop = hp + num_old;
    return res;
}
```

### Step 4: Track matched index in slow paths

**`erts_gc_update_map_exact` slow path:**

The loop at line 2356 iterates `i` from 0 to num_old. When `EQ(*old_keys, new_key)` matches, `i` is the index. For `n == 2` we only have one key to match, so we track the single match:

```c
Uint matched_index = 0;
...
for (i = 0; i < num_old; i++) {
    if (!EQ(*old_keys, new_key)) {
        *hp++ = *old_vals;
    } else {
        matched_index = i;
        ...
    }
    ...
}
/* After success: */
if (erts_map_ic_enabled() && n_orig == 2) {
    GET_TERM(new_p_orig[0], fill_key);
    erts_map_ic_update_flatmap_index(new_p_orig, map_orig, fill_key, 1, matched_index);
}
```

We need to save the original `new_p` and `map` values before GC may move them (but `new_p` is in code space, so it's stable; `map` changes after GC but the IC fill uses the post-GC map whose shape is the same interned pointer).

**`erts_gc_update_map_assoc` slow path:**

The merge loop maintains sorted order. For `n == 2` with a single key, we need to track:
- Whether the key was found (= `!changed_keys` when `n == 2`)
- The index of the key in the final map

The index tracking is trickier here because the assoc merge interleaves old and new keys. For the shape-preserving case (`!changed_keys`), the key's index in the output map equals its index in the old map. We can find this index by comparing:

```c
Uint matched_index = 0;
/* In the merge loop, when c == 0 (key match): */
matched_index = kp - (boxed_val(mp->keys) + 1); /* NOT QUITE — kp is building the new tuple */
```

Actually simpler: since shape is preserved (same keys), the matched index is the position in the old key array. We can compute it after the fact or track it during the merge. For the `n == 2` case, we can do a simple scan of old_keys to find the index:

```c
if (erts_map_ic_enabled() && n_orig == 2 && !changed_keys) {
    Eterm *ok = flatmap_get_keys(old_mp);
    Uint sz = flatmap_get_size(old_mp);
    Eterm fill_key;
    GET_TERM(new_p_orig[0], fill_key);
    for (Uint j = 0; j < sz; j++) {
        if (EQ(ok[j], fill_key)) {
            erts_map_ic_update_flatmap_index(new_p_orig, map_orig, fill_key, 1, j);
            break;
        }
    }
}
```

This scan is O(n) but only happens once (on IC fill), and flatmaps are small (≤32). After fill, subsequent calls hit the fast path.

## Files to Modify

| File | Changes |
|------|---------|
| `erts/emulator/beam/beam_common.c` | Add IC fast paths and fill logic to both update functions |

No other files need changes — the IC infrastructure (per-scheduler table, try_get, update functions, counters) is already in place.

## Verification

1. **Build:** `make -j$(nproc)`
2. **Exact update test:**
   ```erlang
   M = #{a => 1, b => 2},
   M2 = M#{a := 10},           % First call: miss + fill
   M3 = M#{a := 20},           % Second call: should hit
   erlang:garbage_collect(),
   M4 = M#{a := 30},           % After GC: should still hit (interned shape)
   ```
3. **Assoc update test (shape-preserving):**
   ```erlang
   M = #{a => 1},
   M2 = M#{a => 10},           % miss + fill (key exists)
   M3 = M#{a => 20},           % hit
   ```
4. **Assoc update test (shape-changing — should disable):**
   ```erlang
   M = #{a => 1},
   M2 = M#{b => 2},            % miss, new key → disable
   M3 = M#{c => 3},            % disabled, slow path
   ```
5. **Multi-key update (should bypass IC):**
   ```erlang
   M = #{a => 1, b => 2},
   M2 = M#{a := 10, b := 20},  % n == 4, no IC
   ```
6. **Counter verification:** IC hits should be > 0 for repeated single-key updates
