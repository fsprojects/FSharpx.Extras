module FSharpx.TypeProviders.Management.Tests

open System
open System.Management
open FSharpx.TypeProviders.Management

type Local = WmiProvider<"localhost">
let data = Local.GetDataContext()

// Add a handler to watch WMI queries getting executed
data.QueryExecuted.Add(printfn "Query executed: %s")

let data1 = [ for b in data.Win32_DiskDrive -> b.Name, b.Description]

// Access some WMI data from the data connection
let data2 = 
    [for dd in data.CIM_DiskDrive -> 
        [for c in dd.Capabilities -> c.Is_SMART_Notification]]
