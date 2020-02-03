#!/usr/bin/env bash

dotnet tool restore
dotnet restore
if [[ $# -eq 0 ]] ; then
    dotnet fake run build.fsx --parallel 3
else
    dotnet fake run build.fsx --parallel 3 --target $@
fi
