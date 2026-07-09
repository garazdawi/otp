# %CopyrightBegin%
#
# SPDX-License-Identifier: Apache-2.0
#
# Copyright Ericsson AB 2025. All Rights Reserved.
#
# %CopyrightEnd%
[
  # PLAN/ holds T2 planning + verification scaffolding (not shipped OTP
  # sources), so keep it out of the format check -- mirrors the PLAN/
  # exclusion in scripts/license-header.es.
  inputs:
    ["**/*.exs", "**/*.ex"]
    |> Enum.flat_map(&Path.wildcard/1)
    |> Enum.reject(&String.starts_with?(&1, "PLAN/"))
]
