namespace FSharpx.Http

module FsConneg =
    
    open System
    open System.Globalization
    
    let inline internal split (sep: char) (s: string) =
        if s = null
            then [||]
            else s.Split [|sep|]
    
    type Double with
        static member internal parse a =
            Double.Parse(a, NumberStyles.Number, CultureInfo.InvariantCulture)
    
    let inline internal nth i arr = Array.get arr i
    
    let inline internal lower (s: string) = s.Trim().ToLowerInvariant()
    
    let inline internal startsWith (substr: string) (s: string) = 
        if s = null
            then false
            else s.StartsWith substr
    
    let inline internal fst3 (a,_,_) = a
    let inline internal snd3 (_,a,_) = a
    let inline internal thr3 (_,_,a) = a
    
    /// <summary>
    /// Parses a single Accept-* header item. 
    /// Returns item with associated q
    /// </summary>
    /// <param name="s">Header item split by ';'</param>
    let parseQ s =
        let e = 0.0001
        let isQ = startsWith "q="
        let s = Array.map lower s
        let qi = Array.tryFindIndex isQ s
        let q = 
            match qi with
            | None -> 1.
            | Some i -> s |> nth i |> split '=' |> nth 1 |> Double.parse
        let wildcards = Seq.filter ((=) '*') s.[0] |> Seq.length
        let qc = q - e * float wildcards
        let otherParameters = Seq.filter (not << isQ) s |> Seq.length
        let qc = qc + e * float (otherParameters - 1)
        let values = 
            match qi with
            | None -> s
            | Some i -> Array.append s.[..i-1] s.[i+1..s.Length-1]
        String.Join(";", values), q, qc
    
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
        |> Seq.filter (thr3 >> (<) 0.)
        |> Seq.sortBy (thr3 >> (*) -1.)
        |> Seq.map (fun (a,b,_) -> a,b)
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
    /// Parses an Accept header into a list of media,(media type, media subtype),q
    /// E.g. "text/html",("text","html"),0.8
    /// </summary>
    /// <param name="l"></param>
    let parseMediaTypes l =
        parseFilterSortAccept l
        |> Seq.map (fun (a,q) -> a, splitMediaTypeSubtype a, q)
        |> Seq.toList
    
    /// <summary>
    /// Filters an Accept header by type.
    /// E.g. <c>filterMediaTypes "image" "image/png,text/html"</c> -> <c>["image/png"]</c>
    /// </summary>
    /// <param name="mediaType"></param>
    /// <param name="accepts"></param>
    let filterMediaTypes mediaType accepts =
        parseMediaTypes accepts 
        |> Seq.filter (fun (_,(typ,subtype),_) -> typ = mediaType) 
        |> Seq.map (fun (m,_,q) -> m,q)
        |> Seq.toList
    
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
        let r = 
            seq {
                for a in accepts do
                    for m in serves do
                        yield matcher m a
            }
        Seq.choose id r |> Seq.distinctBy fst |> Seq.toList
    
    /// <summary>
    /// Filters and maps a list of served items and a Accept-* header of acceptable items using a matcher function
    /// </summary>
    /// <param name="matcher"></param>
    /// <param name="serves"></param>
    /// <param name="accepts"></param>
    let negotiate matcher serves accepts =
        let matcher a (b,q) = matcher a b |> Option.map (fun r -> r,q)
        negotiateList matcher serves (parseFilterSortAccept accepts)
    
    /// <summary>
    /// Intersects accepted and served media. 
    /// Returns a list of viable media, sorted by client preference in descending order
    /// </summary>
    /// <param name="serves">Served media</param>
    /// <param name="accepts">Accept header</param>
    let negotiateMediaType x = negotiate matchMediaType x
    
    /// <summary>
    /// Gets the first item from a list of negotiated items
    /// </summary>
    /// <param name="negotiate"></param>
    /// <param name="serves"></param>
    /// <param name="accepts"></param>
    let bestOf negotiate serves accepts =
        negotiate serves accepts |> List.tryFind (fun _ -> true)
    
    /// <summary>
    /// Intersects accepted and served media.
    /// Returns the preferred viable media, or <c>None</c>.
    /// </summary>
    /// <param name="serves"></param>
    /// <param name="accepts"></param>
    let bestMediaType x = bestOf negotiateMediaType x
    
    /// <summary>
    /// Matches if the media parameter can be handled by the accept list
    /// </summary>
    /// <param name="serves"></param>
    /// <param name="accepts"></param>
    let (|AcceptsMedia|_|) serves accepts =
        let isMatch = matchMediaType serves >> Option.isSome
        List.tryFind isMatch accepts
        |> Option.map ignore
    
    /// <summary>
    /// Matches two language tags
    /// </summary>
    /// <param name="serves"></param>
    /// <param name="accepts"></param>
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
    
    /// <summary>
    /// Intersects accepted and served languages. 
    /// Returns a list of viable languages, sorted by client preference in descending order
    /// </summary>
    /// <param name="x"></param>
    let negotiateLanguage x = negotiate matchLanguage x
    
    /// <summary>
    /// Intersects accepted and served media.
    /// Returns the preferred viable language, or <c>None</c>.
    /// </summary>
    /// <param name="x"></param>
    let bestLanguage x = bestOf negotiateLanguage x
    
    /// <summary>
    /// Matches two charset tags
    /// </summary>
    /// <param name="serves"></param>
    /// <param name="accepts"></param>
    let matchCharset serves accepts =
        match serves,accepts with
        | "*",a -> Some a
        | s,"*" -> Some s
        | s,a when s = a -> Some s
        | _ -> None
    
    let internal negotiateWithImplicit implicitValue implicitQ matcher serves accepts =
        if String.IsNullOrEmpty accepts
            then serves |> List.map (fun a -> a,1.)
            else
                let accepts = if accepts = null then "" else accepts
                let accepts = parseAccept accepts |> Seq.toList
                let has x = List.exists (fst3 >> (=)x) accepts
                let accepts =
                    if not (has implicitValue) && not (has "*")
                        then (implicitValue, implicitQ, implicitQ)::accepts
                        else accepts
                let filteredAccepts = filterSortAccept accepts 
                let matchCharset a (b,q) = matcher a b |> Option.map (fun r -> r,q)
                negotiateList matchCharset serves filteredAccepts
    
    /// <summary>
    /// Intersects accepted and served charsets. 
    /// Returns a list of viable charsets, sorted by client preference in descending order
    /// </summary>
    /// <param name="serves"></param>
    /// <param name="accepts"></param>
    let negotiateCharset x = negotiateWithImplicit "iso-8859-1" 1. matchCharset x
    
    /// <summary>
    /// Intersects accepted and served charsets.
    /// Returns the preferred viable charset, or <c>None</c>.
    /// </summary>
    /// <param name="x"></param>
    let bestCharset x = bestOf negotiateCharset x
    
    let negotiateEncoding x = negotiateWithImplicit "identity" 0.001 matchCharset x
    
    let bestEncoding x = bestOf negotiateEncoding x
    