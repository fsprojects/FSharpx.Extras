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

/// <summary>
/// Parses any Accept-* header
/// </summary>
/// <param name="l"></param>
let parseAccept l =
    split ',' l
    |> Seq.map (split ';')
    |> Seq.map parseQ
    |> Seq.filter (snd >> (<) 0.)
    |> Seq.sortBy (snd >> (*) -1.)
    |> Seq.map fst
    |> Seq.toList

/// <summary>
/// Splits media type and subtype, e.g. "text/html" -> "text","html"
/// </summary>
/// <param name="m"></param>
let splitMediaTypeSubtype m =
    let p = split '/' m 
    p.[0],p.[1]

/// <summary>
/// Parses an Accept header into a list of media,(media type, media subtype)
/// E.g. "text/html",("text","html")
/// </summary>
/// <param name="l"></param>
let parseMediaTypes l =
    parseAccept l
    |> Seq.map (fun a -> a, splitMediaTypeSubtype a)
    |> Seq.toList

/// <summary>
/// Filters an Accept header by type.
/// E.g. <c>filterMediaTypes "image" "image/png,text/html"</c> -> <c>["image/png"]</c>
/// </summary>
/// <param name="mediaType"></param>
/// <param name="accepts"></param>
let filterMediaTypes mediaType accepts =
    parseMediaTypes accepts 
    |> Seq.filter (fun (v,(typ,subtype)) -> typ = mediaType) 
    |> Seq.map fst
    |> Seq.toList

/// <summary>
/// Picks the best media for a defined media type
/// </summary>
/// <param name="mediaType">Desired media type (e.g. "image")</param>
/// <param name="accepts">Accept header</param>
let bestMediaType mediaType accepts =
    match filterMediaTypes mediaType accepts with
    | x::_ -> Some x
    | _ -> None

/// <summary>
/// Finds a match between two media, handling wildcards.
/// Returns <c>None</c> if no match, otherwise <c>Some media</c>
/// Example: <c>matchMedia "text/*" "text/plain"</c> -> <c>"text/plain"</c>
/// </summary>
/// <param name="serves"></param>
/// <param name="accepts"></param>
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

/// <summary>
/// Intersects accepted and served media. 
/// Returns a list of viable media, sorted by client preference in descending order
/// </summary>
/// <param name="serves">Served media</param>
/// <param name="accepts">Accept header</param>
let filterSortMedia serves accepts =
    let accepts = parseAccept accepts
    let inline (>>=) a f = Seq.collect f a
    let r = accepts >>= fun a -> serves >>= fun m -> matchMedia m a |> Seq.singleton
    Seq.choose id r |> Seq.distinct |> Seq.toList

/// <summary>
/// Intersects accepted and served media.
/// Returns the preferred viable media, or <c>None</c>.
/// </summary>
/// <param name="serves"></param>
/// <param name="accepts"></param>
let bestMedia serves accepts =
    match filterSortMedia serves accepts with
    | a::_ -> Some a
    | _ -> None

/// <summary>
/// Matches if the media parameter can be handled by the accept list
/// </summary>
/// <param name="serves"></param>
/// <param name="accepts"></param>
let (|AcceptsMedia|_|) serves accepts =
    let isMatch = matchMedia serves >> Option.isSome
    List.tryFind isMatch accepts
    |> Option.map ignore