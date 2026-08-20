#!/bin/bash

set -eo pipefail

BASE_BRANCH="$1"
LATEST_ERLANG_VERSION="unknown"

case "${BASE_BRANCH}" in
    maint-*)
        LATEST_ERLANG_VERSION=${BASE_BRANCH#"maint-"}
        ;;
    master|maint)
        ;;
    *)
        ## Every branch in this repository is based on maint-26, so build a
        ## 26 base image rather than falling back to master.  Otherwise kerl
        ## installs the three newest releases and the test tools (proper,
        ## jsx) get compiled by one of those, which produces BEAM files that
        ## OTP 26 cannot read.
        BASE_BRANCH="maint-26"
        LATEST_ERLANG_VERSION="26"
        ;;
esac

if [ -z "${BASE_TAG}" ]; then
    BASE_TAG=$(grep "ARG BASE=" ".github/dockerfiles/Dockerfile.${2}" | head -1 | tr '=' ' ' | awk '{print $3}')
    ## If this script is used on pre 25 releases
    if [ -z "${BASE_TAG}" ]; then
        BASE_TAG=$(grep "FROM " ".github/dockerfiles/Dockerfile.${2}" | head -1 | awk '{print $2}')
    fi
fi

case "${BASE_TAG}" in
    *i386-debian-base)
        BASE="i386/debian:bookworm"
        BASE_TYPE=debian-base
        ;;
    *debian-base)
        BASE="debian:bookworm"
        BASE_TYPE=debian-base
        ;;
    *ubuntu-base)
        BASE="ubuntu:22.04"
        BASE_TYPE=ubuntu-base
        ;;
esac

echo "BASE=${BASE}" >> $GITHUB_OUTPUT
echo "BASE_TAG=${BASE_TAG}" >> $GITHUB_OUTPUT
echo "BASE_TYPE=${BASE_TYPE}" >> $GITHUB_OUTPUT

if [ -f "otp_docker_base.tar" ]; then
    docker load -i "otp_docker_base.tar"
    echo "BASE_BUILD=loaded" >> $GITHUB_OUTPUT
elif [ -f "otp_docker_base/otp_docker_base.tar" ]; then
    docker load -i "otp_docker_base/otp_docker_base.tar"
    echo "BASE_BUILD=loaded" >> $GITHUB_OUTPUT
else
    if [ "${BASE_USE_CACHE}" != "false" ]; then
        docker pull "${BASE_TAG}:${BASE_BRANCH}"
        docker tag "${BASE_TAG}:${BASE_BRANCH}" "${BASE_TAG}:latest"
        BASE_CACHE="--cache-from ${BASE_TAG}"
    fi

    BASE_IMAGE_ID=$(docker images -q "${BASE_TAG}:latest")

    docker build --pull --tag "${BASE_TAG}:latest" \
       ${BASE_CACHE} \
       --file ".github/dockerfiles/Dockerfile.${BASE_TYPE}" \
       --build-arg MAKEFLAGS=-j$(($(nproc) + 2)) \
       --build-arg USER=otptest --build-arg GROUP=uucp \
       --build-arg uid="$(id -u)" \
       --build-arg LATEST_ERLANG_VERSION="${LATEST_ERLANG_VERSION}" \
       --build-arg BASE="${BASE}" .github/

    NEW_BASE_IMAGE_ID=$(docker images -q "${BASE_TAG}:latest")
    if [ "${BASE_IMAGE_ID}" = "${NEW_BASE_IMAGE_ID}" ]; then
        echo "BASE_BUILD=cached" >> $GITHUB_OUTPUT
    else
        echo "BASE_BUILD=re-built" >> $GITHUB_OUTPUT
        docker save "${BASE_TAG}:latest" > "otp_docker_base.tar"
    fi
fi
