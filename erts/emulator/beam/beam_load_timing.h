/*
 * %CopyrightBegin%
 *
 * SPDX-License-Identifier: Apache-2.0
 *
 * Copyright Ericsson AB 2026. All Rights Reserved.
 *
 * %CopyrightEnd%
 */

#ifndef BEAM_LOAD_TIMING_H
#define BEAM_LOAD_TIMING_H

#include "sys.h"

void erts_beam_load_timing_init(void);
int erts_beam_load_timing_is_enabled(void);
ErtsSysHrTime erts_beam_load_timing_start(void);
void erts_beam_load_timing_report(const char *phase, ErtsSysHrTime start);

#endif
