#!/bin/bash

set -xe

CACHE=$1
shift
ARCHIVE=$1
shift
TARGET=$1
shift
DELETED="$*"

TMP_DIR=$(mktemp -d)
CACHE_DIR="${TMP_DIR}"
ARCHIVE_DIR="${TMP_DIR}/archive"

mkdir "${ARCHIVE_DIR}"
tar -C "${CACHE_DIR}/" -xzmf "${CACHE}"
mkdir "${CACHE_DIR}/cache"
cp -rp "${CACHE_DIR}/otp" "${CACHE_DIR}/cache/"
sleep 5
tar -C "${ARCHIVE_DIR}/" -xzmf "${ARCHIVE}"

rsync -avcu --no-t "${ARCHIVE_DIR}/otp/" "${CACHE_DIR}/otp/"

for delete in $DELETED; do
    if [ -d "${CACHE_DIR}/otp/${delete}" ]; then
        rm -r "${CACHE_DIR}/otp/${delete}"
    elif [ -f "${CACHE_DIR}/otp/${delete}" ]; then
        rm "${CACHE_DIR}/otp/${delete}"
    else
        echo "Could not find $delete to delete"
        exit 1
    fi
done

tar -czf "${TARGET}" -C "${TMP_DIR}" otp

rm -rf "${TMP_DIR}"
