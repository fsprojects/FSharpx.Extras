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

let parseQ s =
    let e = 0.0001
    let s = Array.map lower s
    let qi = Array.tryFindIndex (startsWith "q=") s
    let q = 
        match qi with
        | None -> 1.
        | Some i -> s |> nth i |> split '=' |> nth 1 |> Double.parse
    let wildcards = Seq.filter ((=) '*') s.[0] |> Seq.length
    let q = q - e * float wildcards
    let otherParameters = Seq.filter (startsWith "q=" >> not) s |> Seq.length
    let q = q + e * float (otherParameters - 1)
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

let splitMediaTypeSubtype m =
    let p = split '/' m 
    p.[0],p.[1]

let parseMediaTypes l =
    parseAccept l
    |> Seq.map (fun a -> a, splitMediaTypeSubtype a)
    |> Seq.toList

let filterMediaTypes media all =
    parseMediaTypes all 
    |> Seq.filter (fun (v,(typ,subtype)) -> typ = media) 
    |> Seq.map fst
    |> Seq.toList

let bestMediaType media all =
    match filterMediaTypes media all with
    | x::_ -> Some x
    | _ -> None

let matchMedia serves accepts =
    let tserves,sserves = splitMediaTypeSubtype serves
    let taccepts,saccepts = splitMediaTypeSubtype accepts
    match tserves,sserves,taccepts,saccepts with
    | "*","*",_,_ -> Some accepts
    | _,_,"*","*" -> Some serves
    | a,"*",c,_ when a = c -> Some accepts
    | a,_,c,"*" when a = c -> Some serves
    | a,b,c,d when a = c && b = d -> Some accepts
    | _ -> None

let filterSortMedia media all =
    let all = parseAccept all 
    let inline (>>=) a f = Seq.collect f a
    let r = all >>= fun a -> media >>= fun m -> matchMedia m a |> Seq.singleton
    Seq.choose id r |> Seq.distinct |> Seq.toList

let bestMedia media all =
    match filterSortMedia media all with
    | a::_ -> Some a
    | _ -> None

let (|Accepts|_|) media all =
    let isMatch = matchMedia media >> Option.isSome
    List.tryFind isMatch all
    |> Option.map ignore