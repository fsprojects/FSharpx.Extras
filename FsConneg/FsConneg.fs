module FsConneg

open System
open System.Globalization

let inline internal split (sep: char) (s: string) =
    s.Split [|sep|]

type Double with
    static member internal parse a =
        Double.Parse(a, NumberStyles.Number, CultureInfo.InvariantCulture)

let inline internal nth i arr = Array.get arr i

let inline internal lower (s: string) = s.Trim().ToLowerInvariant()

let inline internal startsWith (substr: string) (s: string) = s.StartsWith substr

let internal parseQ (s: string[]) =
    let e = 0.0001
    let s = Array.map lower s
    let qi = Array.tryFindIndex (startsWith "q=") s
    let q = 
        match qi with
        | None -> 1.
        | Some i -> s |> nth i |> split '=' |> nth 1 |> Double.parse
    let wildcards = Seq.filter ((=) '*') s.[0] |> Seq.length
    let q = q - e * float wildcards
    let nonQs = Seq.filter (startsWith "q=" >> not) s |> Seq.length
    let q = q + e * float (nonQs - 1)
    let values = 
        match qi with
        | None -> s
        | Some i -> Array.append s.[..i-1] s.[i+1..s.Length-1]
    String.Join(";", values), q

let parseAccept l =
    split ',' l
    |> Seq.map (split ';')
    |> Seq.map parseQ
    |> Seq.filter (snd >> (<) 0.)
    |> Seq.sortBy (snd >> (*) -1.)
    |> Seq.map fst
    |> Seq.toList

let parseMediaTypes l =
    parseAccept l
    |> Seq.map (fun a -> a, split '/' a)
    |> Seq.map (fun (a,p) -> a,(p.[0],p.[1]))
    |> Seq.toList

let bestMediaType media all =
    parseMediaTypes all 
    |> List.tryFind (fun (v,(typ,subtype)) -> typ = media) 
    |> Option.map fst

let filterMediaTypes media all =
    parseMediaTypes all 
    |> Seq.filter (fun (v,(typ,subtype)) -> typ = media) 
    |> Seq.map fst
    |> Seq.toList