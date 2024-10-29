#!/usr/bin/env bash

set -eu
set -o pipefail

dotnet tool restore
dotnet restore
dotnet run -v:m --project ./build/build.fsproj -- -t "$@"