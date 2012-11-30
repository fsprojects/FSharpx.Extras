#!/bin/sh
export MONO_IOMAP=all
exec mono $MONO_OPTIONS packages/FAKE.1.64.18.0/tools/FAKE.exe build.fsx v35 v40
