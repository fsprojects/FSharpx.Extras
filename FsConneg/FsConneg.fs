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

/// <summary>
/// Parses a single Accept-* header item. 
/// Returns item with associated q
/// </summary>
/// <param name="s">Heaer item split by ';'</param>
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
/// Parses any Accept-* header, returns a seq of items with associated q (quality/preference)
/// </summary>
/// <param name="l"></param>
let parseAccept l =
    split ',' l
    |> Seq.map (split ';')
    |> Seq.map parseQ

/// <summary>
/// Takes a list of items with associated numeric quality (preference), and:
/// removes all items with q=0 (i.e. not acceptable by client);
/// sorts by q descending (client preference)
/// </summary>
/// <param name="x"></param>
let filterSortAccept x =
    x 
    |> Seq.filter (snd >> (<) 0.)
    |> Seq.sortBy (snd >> (*) -1.)
    |> Seq.map fst
    |> Seq.toList

/// <summary>
/// Parses any Accept-* header. 
/// Removes all items with q=0 (i.e. not acceptable by client).
/// Sorts by q descending (client preference)
/// </summary>
/// <param name="l"></param>
let parseFilterSortAccept l =
    parseAccept l |> filterSortAccept

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
    parseFilterSortAccept l
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
/// Finds a match between two media types, handling wildcards.
/// Returns <c>None</c> if no match, otherwise <c>Some media</c>
/// Example: <c>matchMediaType "text/*" "text/plain"</c> -> <c>"text/plain"</c>
/// </summary>
/// <param name="serves"></param>
/// <param name="accepts"></param>
let matchMediaType serves accepts =
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
/// Filters and maps two sequences of items using a matcher function.
/// </summary>
/// <param name="matcher"></param>
/// <param name="serves"></param>
/// <param name="accepts"></param>
let negotiateList matcher serves accepts =
    // TODO probably very inefficient, rewrite
    let inline (>>=) a f = Seq.collect f a
    let r = accepts >>= fun a -> serves >>= fun m -> matcher m a |> Seq.singleton
    Seq.choose id r |> Seq.distinct |> Seq.toList

let negotiate matcher serves accepts =
    negotiateList matcher serves (parseFilterSortAccept accepts)

/// <summary>
/// Intersects accepted and served media. 
/// Returns a list of viable media, sorted by client preference in descending order
/// </summary>
/// <param name="serves">Served media</param>
/// <param name="accepts">Accept header</param>
let negotiateMediaType x = negotiate matchMediaType x

let bestOf negotiate serves accepts =
    negotiate serves accepts |> List.tryFind (fun _ -> true)

/// <summary>
/// Intersects accepted and served media.
/// Returns the preferred viable media, or <c>None</c>.
/// </summary>
/// <param name="serves"></param>
/// <param name="accepts"></param>
let bestMedia x = bestOf negotiateMediaType x

/// <summary>
/// Matches if the media parameter can be handled by the accept list
/// </summary>
/// <param name="serves"></param>
/// <param name="accepts"></param>
let (|AcceptsMedia|_|) serves accepts =
    let isMatch = matchMediaType serves >> Option.isSome
    List.tryFind isMatch accepts
    |> Option.map ignore

let matchLanguage serves accepts =
    match serves,accepts with
    | "*",a -> Some a
    | s,"*" -> Some s
    | _ ->
        let pserves = split '-' serves
        let paccepts = split '-' accepts
        let matches = Seq.zip pserves paccepts |> Seq.takeWhile (fun (s,a) -> s = a) |> Seq.length
        if matches < paccepts.Length 
            then None
            else Some serves

let negotiateLanguage x = negotiate matchLanguage x
let bestLanguage x = bestOf negotiateLanguage x

let matchCharset serves accepts =
    match serves,accepts with
    | "*",a -> Some a
    | s,"*" -> Some s
    | s,a when s = a -> Some s
    | _ -> None

let negotiateCharset serves accepts = 
    if String.IsNullOrEmpty accepts
        then serves
        else
            let iso88591 = "iso-8859-1"
            let accepts = if accepts = null then "" else accepts
            let accepts = parseAccept accepts |> Seq.toList
            let has x = accepts |> List.exists (fun (a,_) -> a = x)
            let accepts =
                if not (has iso88591) && not (has "*")
                    then (iso88591, 1.)::accepts
                    else accepts
            let filteredAccepts = filterSortAccept accepts 
            negotiateList matchCharset serves filteredAccepts

let bestCharset x = bestOf negotiateCharset x

