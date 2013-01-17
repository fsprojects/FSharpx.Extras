module FSharpx.TypeProviders.Management.Tests

open System
open System.Management
open FSharpx.TypeProviders.Management

type Local = WmiProvider<"localhost">
let data = Local.GetDataContext()

[ for b in data.Win32_DiskDrive -> b.Name, b.Description]