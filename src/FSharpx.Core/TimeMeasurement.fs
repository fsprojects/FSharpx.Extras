module FSharpx.TimeMeasurement

/// Stops the runtime for a given function
let stopTime f = 
    let sw = new System.Diagnostics.Stopwatch()
    sw.Start()
    let result = f()
    sw.Stop()
    result,float sw.ElapsedMilliseconds

/// Stops the average runtime for a given function and applies it the given count
let stopAverageTime count f = 
    let sw = new System.Diagnostics.Stopwatch()
    let list = [1..count]
    sw.Start()
    let results = List.map (fun _ -> f()) list
    sw.Stop()
    results,float sw.ElapsedMilliseconds / float count