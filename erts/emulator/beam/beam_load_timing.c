/*
 * %CopyrightBegin%
 *
 * SPDX-License-Identifier: Apache-2.0
 *
 * Copyright Ericsson AB 2026. All Rights Reserved.
 *
 * %CopyrightEnd%
 */

#ifdef HAVE_CONFIG_H
#    include "config.h"
#endif

#include "sys.h"
#include "global.h"
#include "beam_load_timing.h"

static int enabled;

void erts_beam_load_timing_init(void)
{
    char value[2];
    size_t size = sizeof(value);

    enabled = erts_sys_explicit_8bit_getenv("ERL_JIT_CACHE_TIMING",
                                            value,
                                            &size) == 1;
}

int erts_beam_load_timing_is_enabled(void)
{
    return enabled;
}

ErtsSysHrTime erts_beam_load_timing_start(void)
{
    return enabled ? erts_sys_hrtime() : 0;
}

void erts_beam_load_timing_report(const char *phase, ErtsSysHrTime start)
{
    if (start) {
        erts_fprintf(stderr,
                     "JIT_LOAD_TIMING %s %b64u\n",
                     phase,
                     (Uint64)(erts_sys_hrtime() - start));
    }
}
