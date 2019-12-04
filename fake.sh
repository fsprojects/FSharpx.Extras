#!/usr/bin/env bash

dotnet restore build.proj
if [[ $# -eq 0 ]] ; then
    dotnet fake run build.fsx
else
    dotnet fake run build.fsx --target $@
fi
