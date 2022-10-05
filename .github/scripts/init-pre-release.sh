#!/bin/bash

## We create a tar ball that is used later by build-otp-tar
## to create the pre-built tar ball

AUTOCONF=0

## This script is used to create archives for older releases
## so if configure does not exist in the git repo we need to
## create it.
if [ ! -f configure ]; then
    ./otp_build autoconf
    find . -name aclocal.m4 | xargs git add -f
    find . -name configure | xargs git add -f
    find . -name config.h.in | xargs git add -f
    find . -name config.guess | xargs git add -f
    find . -name config.sub | xargs git add -f
    find . -name install-sh | xargs git add -f
    if ! git config user.name; then
        git config user.email "you@example.com"
        git config user.name "Your Name"
    fi
    git commit --no-verify -m 'Add generated configure files'
    AUTOCONF=1
fi

## if possible, we use tar to create the archive as that preserves the timestamps
## of the files. If we use git archive then the timestamps of all files are from
## the latest commit, not of the latest commit for each file
# UNTRACKED=$(git status -s -unormal | grep "^??" | awk '{print $2}')
# EXCLUDE_UNTRACKED=("--exclude=.git")
# for file in $UNTRACKED; do
#     if [ -d $file ]; then
#         EXCLUDE_UNTRACKED=("--exclude=$file*" "${EXCLUDE_UNTRACKED[@]}")
#     else
#         EXCLUDE_UNTRACKED=("--exclude=$file" "${EXCLUDE_UNTRACKED[@]}")
#     fi
# done
# if ! tar -czf otp_src.tar.gz ${EXCLUDE_UNTRACKED[@]} --transform "s/^./otp/" .; then
#     git archive --prefix otp/ -o otp_src.tar.gz HEAD
# fi
git archive --prefix otp/ -o otp_src.tar.gz HEAD

if [ "$AUTOCONF" = 1 ]; then
    git reset --hard HEAD~1
fi
