#!/bin/sh

## %CopyrightBegin%
##
## SPDX-License-Identifier: Apache-2.0
##
## Copyright Ericsson AB 2026. All Rights Reserved.
##
## Licensed under the Apache License, Version 2.0 (the "License");
## you may not use this file except in compliance with the License.
## You may obtain a copy of the License at
##
##     http://www.apache.org/licenses/LICENSE-2.0
##
## Unless required by applicable law or agreed to in writing, software
## distributed under the License is distributed on an "AS IS" BASIS,
## WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
## See the License for the specific language governing permissions and
## limitations under the License.
##
## %CopyrightEnd%

## Give applications that differ from the parent release branch without a
## version change a CI-only version, leaving ct_release_test an upgrade to run.

set -eu

otp_test_root=${OTP_TEST_ROOT:-$(pwd)}

for app in ${SYNTHETIC_VERSION_BUMPS:-}; do
    if test "$app" = erts; then
        vsn_file="$otp_test_root/erts/vsn.mk"
        vsn_variable=VSN
    else
        vsn_file="$otp_test_root/lib/$app/vsn.mk"
        vsn_variable=$(printf '%s_VSN' "$app" | tr '[:lower:]' '[:upper:]')
    fi
    test -f "$vsn_file" || continue

    old_vsn=$(sed -n "s/^${vsn_variable}[[:space:]]*=[[:space:]]*//p" "$vsn_file")

    case "$old_vsn" in
        ''|*[!0-9.]*)
            echo "Cannot synthetically bump $vsn_file: unexpected version '$old_vsn'" >&2
            exit 1
            ;;
    esac

    new_vsn="$old_vsn.1"
    sed -i.bak "s/^\(${vsn_variable}[[:space:]]*=[[:space:]]*\).*/\1${new_vsn}/" "$vsn_file"
    rm -f "$vsn_file.bak"
    echo "Synthetic ct_release_test version: $app $old_vsn -> $new_vsn"
done
