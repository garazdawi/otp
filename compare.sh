#!/bin/bash

set -e

TTF=aarch64-apple-darwin25.4.0

if [ "$1" = "orig" ]; then
    git clean -xfdq erts
    ./otp_build setup -t --disable-jit
    cp bin/$TTF/beam.emu bin/$TTF/beam.emu.orig
    dsymutil bin/$TTF/beam.emu.orig
    strip -o bin/$TTF/beam.emu.orig.stripped bin/$TTF/beam.emu.orig
fi

if [ "$1" = "new" ]; then
    git clean -xfdq erts
    ./otp_build configure -m
    make emulator
    cp bin/$TTF/beam.emu bin/$TTF/beam.emu.new
    dsymutil bin/$TTF/beam.emu.new
    strip -o bin/$TTF/beam.emu.new.stripped bin/$TTF/beam.emu.new
fi


ORIG_SIZE=$(ls -l bin/$TTF/beam.emu.orig.stripped | awk '{print $5}')
NEW_SIZE=$(ls -l bin/$TTF/beam.emu.new.stripped | awk '{print $5}')

bloaty bin/$TTF/beam.emu.new -d compileunits --dsym=bin/$TTF/beam.emu.new.dSYM

bloaty bin/$TTF/beam.emu.new -n 50 -d compileunits --dsym=bin/$TTF/beam.emu.new.dSYM -- bin/$TTF/beam.emu.orig --dsym=bin/$TTF/beam.emu.orig.dSYM

echo "Original size: $ORIG_SIZE"
echo "New size: $NEW_SIZE"
echo "Size difference: $((NEW_SIZE - ORIG_SIZE)) ($(((NEW_SIZE - ORIG_SIZE) / 1024)) KB)"
