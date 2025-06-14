/*
 * %CopyrightBegin%
 *
 * SPDX-License-Identifier: Apache-2.0
 *
 * Copyright Ericsson AB 1996-2025. All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * %CopyrightEnd%
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_sys_driver.h"
#include "erl_vm.h"
#include "global.h"
#include "hash.h"
#include "atom.h"
#include "erl_global_literals.h"


#define ATOM_SIZE  3000

IndexTable erts_atom_table;	/* The index table */

static erts_rwmtx_t atom_table_lock;

#define atom_read_lock()	erts_rwmtx_rlock(&atom_table_lock)
#define atom_read_unlock()	erts_rwmtx_runlock(&atom_table_lock)
#define atom_write_lock()	erts_rwmtx_rwlock(&atom_table_lock)
#define atom_write_unlock()	erts_rwmtx_rwunlock(&atom_table_lock)

#if 0
#define ERTS_ATOM_PUT_OPS_STAT
#endif
#ifdef ERTS_ATOM_PUT_OPS_STAT
static erts_atomic_t atom_put_ops;
#endif

static Uint atom_space;		/* Amount of atom text space used */

/*
 * Print info about atom tables
 */
void atom_info(fmtfn_t to, void *to_arg)
{
    int lock = !ERTS_IS_CRASH_DUMPING;
    if (lock)
	atom_read_lock();
    index_info(to, to_arg, &erts_atom_table);
#ifdef ERTS_ATOM_PUT_OPS_STAT
    erts_print(to, to_arg, "atom_put_ops: %ld\n",
	       erts_atomic_read_nob(&atom_put_ops));
#endif

    if (lock)
	atom_read_unlock();
}




/*
 * Calculate atom hash value (using the hash algorithm
 * hashpjw from the Dragon Book).
 */

static HashValue
atom_hash(Atom* obj)
{
    byte* p = obj->u.name;
    int len = obj->len;
    HashValue h = 0, g;
    byte v;

    while(len--) {
	v = *p++;
	/* latin1 clutch for r16 */
	if (len && (v & 0xFE) == 0xC2 && (*p & 0xC0) == 0x80) {
	    v = (v << 6) | (*p & 0x3F);
	    p++; len--;
	}
	/* normal hashpjw follows for v */
	h = (h << 4) + v;
	if ((g = h & 0xf0000000)) {
	    h ^= (g >> 24);
	    h ^= g;
	}
    }
    return h;
}

const byte *erts_atom_get_name(const Atom *atom)
{
    byte *name;
    Uint size;
    Uint offset;
    ERTS_GET_BITSTRING(atom->u.bin, name, offset, size);
    ASSERT(offset == 0 && (size % 8) == 0);
    (void) size;
    (void) offset;
    return name;
}

static int 
atom_cmp(Atom* tmpl, Atom* obj)
{
    if (tmpl->len == obj->len &&
	sys_memcmp(tmpl->u.name, erts_atom_get_name(obj), tmpl->len) == 0)
	return 0;
    return 1;
}


static Atom*
atom_alloc(Atom* tmpl)
{
    Atom *obj = (Atom*) erts_alloc(ERTS_ALC_T_ATOM, sizeof(Atom));

    {
        Eterm *hp;
        Uint heap_size = 0;
        ErtsHeapFactory factory;
        ErlOffHeap oh;
        struct erl_off_heap_header **literal_ohp;
        
        if (tmpl->len <= ERL_ONHEAP_BINARY_LIMIT) {
            heap_size = heap_bits_size(NBITS(tmpl->len));
        } else {
            heap_size = ERL_REFC_BITS_SIZE;
        }

        hp = erts_global_literal_allocate(heap_size, &literal_ohp);
        ERTS_INIT_OFF_HEAP(&oh);
        oh.first = *literal_ohp;
        
        erts_factory_static_init(&factory, hp, heap_size, &oh);
        *literal_ohp = oh.first;
        obj->u.bin = erts_hfact_new_binary_from_data(&factory, 
                                                    0, 
                                                    tmpl->len, 
                                                    tmpl->u.name);
        erts_global_literal_register(&obj->u.bin);
    }
    
    obj->len = tmpl->len;
    obj->latin1_chars = tmpl->latin1_chars;
    obj->slot.index = -1;
    atom_space += tmpl->len;
    

    /*
     * Precompute ordinal value of first 3 bytes + 7 bits.
     * This is used by erl_utils.h:erts_cmp_atoms().
     * We cannot use the full 32 bits of the first 4 bytes,
     * since we use the sign of the difference between two
     * ordinal values to represent their relative order.
     */
    {
	unsigned char c[4];
	int i;
	int j;

	j = (tmpl->len < 4) ? tmpl->len : 4;
	for(i = 0; i < j; ++i)
	    c[i] = tmpl->u.name[i];
	for(; i < 4; ++i)
	    c[i] = '\0';
	obj->ord0 = (c[0] << 23) + (c[1] << 15) + (c[2] << 7) + (c[3] >> 1);
    }
    return obj;
}

static void
atom_free(Atom* obj)
{
}

static void latin1_to_utf8(byte* conv_buf, Uint buf_sz,
                           const byte** srcp, Uint* lenp)
{
    byte* dst;
    const byte* src = *srcp;
    Uint i, len = *lenp;

    ASSERT(len <= MAX_ATOM_CHARACTERS);
    ASSERT(buf_sz >= MAX_ATOM_SZ_FROM_LATIN1);

    for (i=0 ; i < len; ++i) {
	if (src[i] & 0x80) {
	    goto need_convertion;
	}
    }
    return;

need_convertion:
    sys_memcpy(conv_buf, src, i);
    dst = conv_buf + i;
    for ( ; i < len; ++i) {
	unsigned char chr = src[i];
	if (!(chr & 0x80)) {
	    *dst++ = chr;
	}
	else {
	    *dst++ = 0xC0 | (chr >> 6);
	    *dst++ = 0x80 | (chr & 0x3F);
	}
    }
    *srcp = conv_buf;	
    *lenp = dst - conv_buf;
}

/*
 * erts_atom_put_index() may fail. Returns negative indexes for errors.
 */
int
erts_atom_put_index(const byte *name, Sint len, ErtsAtomEncoding enc, int trunc)
{
    byte utf8_copy[MAX_ATOM_SZ_FROM_LATIN1];
    const byte *text = name;
    Uint tlen;
    Sint no_latin1_chars;
    Atom a;
    int aix;

    ERTS_UNDEF(no_latin1_chars, -1);

#ifdef ERTS_ATOM_PUT_OPS_STAT
    erts_atomic_inc_nob(&atom_put_ops);
#endif

    if (len < 0) {
        if (trunc) {
            len = 0;
        } else {
            return ATOM_MAX_CHARS_ERROR;
        }
    }

    tlen = len;

    switch (enc) {
    case ERTS_ATOM_ENC_7BIT_ASCII:
	if (tlen > MAX_ATOM_CHARACTERS) {
	    if (trunc)
		tlen = MAX_ATOM_CHARACTERS;
	    else
		return ATOM_MAX_CHARS_ERROR;
	}
#ifdef DEBUG
	for (aix = 0; aix < len; aix++) {
	    ASSERT((name[aix] & 0x80) == 0);
	}
#endif
	no_latin1_chars = tlen;
	break;
    case ERTS_ATOM_ENC_LATIN1:
	if (tlen > MAX_ATOM_CHARACTERS) {
	    if (trunc)
		tlen = MAX_ATOM_CHARACTERS;
	    else
		return ATOM_MAX_CHARS_ERROR;
	}
	no_latin1_chars = tlen;
	latin1_to_utf8(utf8_copy, sizeof(utf8_copy), &text, &tlen);
	break;
    case ERTS_ATOM_ENC_UTF8:
	/* First sanity check; need to verify later */
	if (tlen > MAX_ATOM_SZ_LIMIT && !trunc)
	    return ATOM_MAX_CHARS_ERROR;
	break;
    }

    a.len = tlen;
    a.u.name = (byte *) text;
    atom_read_lock();
    aix = index_get(&erts_atom_table, (void*) &a);
    atom_read_unlock();
    if (aix >= 0) {
	/* Already in table no need to verify it */
	return aix;
    }

    if (enc == ERTS_ATOM_ENC_UTF8) {
	/* Need to verify encoding and length */
	const byte *err_pos;
	Uint no_chars;
	switch (erts_analyze_utf8_x((byte *) text,
				    (Uint) tlen,
				    &err_pos,
				    &no_chars, NULL,
				    &no_latin1_chars,
				    MAX_ATOM_CHARACTERS)) {
	case ERTS_UTF8_OK:
	    ASSERT(no_chars <= MAX_ATOM_CHARACTERS);
	    break;
	case ERTS_UTF8_OK_MAX_CHARS:
	    /* Truncated... */
	    if (!trunc)
		return ATOM_MAX_CHARS_ERROR;
	    ASSERT(no_chars == MAX_ATOM_CHARACTERS);
	    tlen = err_pos - text;
	    break;
	default:
	    /* Bad utf8... */
	    return ATOM_BAD_ENCODING_ERROR;
	}
    }

    ASSERT(tlen <= MAX_ATOM_SZ_LIMIT);
    ASSERT(-1 <= no_latin1_chars && no_latin1_chars <= MAX_ATOM_CHARACTERS);

    a.len = tlen;
    a.latin1_chars = (Sint16) no_latin1_chars;
    a.u.name = (byte *) text;
    atom_write_lock();
    aix = index_put(&erts_atom_table, (void*) &a);
    atom_write_unlock();
    return aix;
}

/*
 * erts_atom_put() may fail. If it fails THE_NON_VALUE is returned!
 */
Eterm
erts_atom_put(const byte *name, Sint len, ErtsAtomEncoding enc, int trunc)
{
    Sint aix = erts_atom_put_index(name, len, enc, trunc);
    if (aix >= 0)
	return make_atom(aix);
    else
	return THE_NON_VALUE;
}

Eterm
am_atom_put(const char* name, Sint len)
{
    /* Assumes 7-bit ascii; use erts_atom_put() for other encodings... */
    return erts_atom_put((byte *) name, len, ERTS_ATOM_ENC_7BIT_ASCII, 1);
}

int atom_table_size(void)
{
    int ret;
    int lock = !ERTS_IS_CRASH_DUMPING;
    if (lock)
	atom_read_lock();
    ret = erts_atom_table.entries;
    if (lock)
	atom_read_unlock();
    return ret;
}

int atom_table_sz(void)
{
    int ret;
    int lock = !ERTS_IS_CRASH_DUMPING;
    if (lock)
	atom_read_lock();
    ret = index_table_sz(&erts_atom_table);
    if (lock)
	atom_read_unlock();
    return ret;
}

int
erts_atom_get(const char *name, Uint len, Eterm* ap, ErtsAtomEncoding enc)
{
    byte utf8_copy[MAX_ATOM_SZ_FROM_LATIN1];
    Atom a;
    int i;
    int res;

    switch (enc) {
    case ERTS_ATOM_ENC_LATIN1:
        if (len > MAX_ATOM_CHARACTERS) {
            return 0;
        }

        latin1_to_utf8(utf8_copy, sizeof(utf8_copy), (const byte**)&name, &len);

        a.u.name = (byte*)name;
        a.len = (Sint16)len;
        break;
    case ERTS_ATOM_ENC_7BIT_ASCII:
        if (len > MAX_ATOM_CHARACTERS) {
            return 0;
        }

        for (i = 0; i < len; i++) {
            if (name[i] & 0x80) {
                return 0;
            }
        }

        a.len = (Sint16)len;
        a.u.name = (byte*)name;
        break;
    case ERTS_ATOM_ENC_UTF8:
        if (len > MAX_ATOM_SZ_LIMIT) {
            return 0;
        }

        /* We don't need to check whether the encoding is legal as all atom
         * names are stored as UTF-8 and we know a lookup with a badly encoded
         * name will fail. */

        a.len = (Sint16)len;
        a.u.name = (byte*)name;
        break;
    }

    atom_read_lock();
    i = index_get(&erts_atom_table, (void*) &a);
    res = i < 0 ? 0 : (*ap = make_atom(i), 1);
    atom_read_unlock();

    return res;
}

void
erts_atom_get_text_space_sizes(Uint *reserved, Uint *used)
{
    int lock = !ERTS_IS_CRASH_DUMPING;
    if (lock)
	atom_read_lock();
    if (reserved)
	*reserved = atom_space;
    if (used)
	*used = atom_space;
    if (lock)
	atom_read_unlock();
}

void
init_atom_table(void)
{
    HashFunctions f;
    int i;
    Atom a;
    erts_rwmtx_opt_t rwmtx_opt = ERTS_RWMTX_OPT_DEFAULT_INITER;

    rwmtx_opt.type = ERTS_RWMTX_TYPE_FREQUENT_READ;
    rwmtx_opt.lived = ERTS_RWMTX_LONG_LIVED;

#ifdef ERTS_ATOM_PUT_OPS_STAT
    erts_atomic_init_nob(&atom_put_ops, 0);
#endif

    erts_rwmtx_init_opt(&atom_table_lock, &rwmtx_opt, "atom_tab", NIL,
        ERTS_LOCK_FLAGS_PROPERTY_STATIC | ERTS_LOCK_FLAGS_CATEGORY_GENERIC);

    f.hash = (H_FUN) atom_hash;
    f.cmp  = (HCMP_FUN) atom_cmp;
    f.alloc = (HALLOC_FUN) atom_alloc;
    f.free = (HFREE_FUN) atom_free;
    f.meta_alloc = (HMALLOC_FUN) erts_alloc;
    f.meta_free = (HMFREE_FUN) erts_free;
    f.meta_print = (HMPRINT_FUN) erts_print;

    erts_index_init(ERTS_ALC_T_ATOM_TABLE, &erts_atom_table,
		    "atom_tab", ATOM_SIZE, erts_atom_table_size, f);

    /* Ordinary atoms. a is a template for creating an entry in the atom table */
    for (i = 0; erl_atom_names[i] != 0; i++) {
	int ix;
	a.len = sys_strlen(erl_atom_names[i]);
	a.latin1_chars = a.len;
	a.u.name = (byte*)erl_atom_names[i];
	a.slot.index = i;


#ifdef DEBUG
	/* Verify 7-bit ascii */
	for (ix = 0; ix < a.len; ix++) {
	    ASSERT((a.u.name[ix] & 0x80) == 0);
	}
#endif
	ix = index_put(&erts_atom_table, (void*) &a);
    (void) ix;
    /* Assert that the entry in the atom table is not a template */
    ASSERT(erts_atom_get_name(atom_tab(ix)));
    }

}

void
dump_atoms(fmtfn_t to, void *to_arg)
{
    int i = erts_atom_table.entries;

    /*
     * Print out the atom table starting from the end.
     */
    while (--i >= 0) {
	if (erts_index_lookup(&erts_atom_table, i)) {
	    erts_print(to, to_arg, "%T\n", make_atom(i));
	}
    }
}

Uint
erts_get_atom_limit(void)
{
    return erts_atom_table.limit;
}
