module FsConneg

open System
open System.Globalization

type Either<'a,'b> = Left of 'a | Right of 'b

let inline internal split (sep: char) (s: string) =
    s.Split [|sep|]

type Double with
    static member internal tryParse a =
        match Double.TryParse(a, NumberStyles.Number, CultureInfo.InvariantCulture) with
        | false, _ -> None
        | _, v -> Some v

let inline internal nth i arr = Array.get arr i

let inline internal lower (s: string) = s.Trim().ToLowerInvariant()

let internal (|Q|_|) x =
    if x = null 
        then None
        else
            let x = lower x
            if not (x.StartsWith "q=") 
                then None
                else x |> split '=' |> nth 1 |> Double.tryParse

let parseLang langs =
    split ',' langs
    |> Seq.map (split ';')
    |> Seq.map (fun s ->
                    match s with
                    | [|x|] -> lower x, 1.
                    | [|x; Q q|] -> lower x, q
                    | _ -> failwith "%A" s)
    |> Seq.filter (snd >> (<) 0.)
    |> Seq.sortBy (snd >> (*) -1.)
    |> Seq.map fst
    |> Seq.toList

