module FSharpx.JSON.Zipper

/// A zipper for JsonValues
/// inspired by https://gist.github.com/868042 

type JsonZipper = 
| ListZipper of JsonValue * JsonZipper option * JsonValue list * JsonValue list
| MapZipper of JsonValue * JsonZipper option * string * Map<string,JsonValue>

/// Returns the JsonValue under focus
let inline focus zipper = 
    match zipper with
    | ListZipper(f,_,_,_) -> f
    | MapZipper(f,_,_,_) -> f

/// Returns the parent of the current zipper
let inline parent zipper = 
    match zipper with
    | ListZipper(_,p,_,_) -> p
    | MapZipper(_,p,_,_) -> p

/// Changes the element under the focus
let modifyText newValue zipper = 
    match zipper with
    | ListZipper(f,p,ls,rs) -> ListZipper(JsonValue.String newValue,p,ls,rs)
    | MapZipper(f,p,n,m) -> MapZipper(JsonValue.String newValue,p,n,m)

/// Changes the element under the focus
let modifyDecimal newValue zipper = 
    match zipper with
    | ListZipper(f,p,ls,rs) -> ListZipper(JsonValue.NumDecimal newValue,p,ls,rs)
    | MapZipper(f,p,n,m) -> MapZipper(JsonValue.NumDecimal newValue,p,n,m)

/// Changes the element under the focus
let modifyBool newValue zipper = 
    match zipper with
    | ListZipper(f,p,ls,rs) -> ListZipper(JsonValue.Bool newValue,p,ls,rs)
    | MapZipper(f,p,n,m) -> MapZipper(JsonValue.Bool newValue,p,n,m)

/// Moves the zipper to the left
let left zipper = 
    match zipper with
    | ListZipper(f,p,ls,rs) -> 
        match ls with
        | x::xs -> ListZipper(x,p,xs,f::rs)    

/// Moves the zipper to the right
let right zipper = 
    match zipper with
    | ListZipper(f,p,ls,rs) -> 
        match rs with
        | x::xs -> ListZipper(x,p,f::ls,xs)

/// Moves the zipper down
let down zipper =
    match focus zipper with
    | JsonValue.Obj map ->
        let e = Seq.head map
        MapZipper(e.Value,Some zipper,e.Key,Map.remove e.Key map)
    | JsonValue.Array (x::xs) -> ListZipper(x,Some zipper,[],xs)    

/// Moves the zipper to the property with the given name on the same level
let toProperty name zipper =
    match zipper with
    | MapZipper(f,p,n,m) ->
        let map = Map.add n f m
        MapZipper(Map.find name map,p,name,Map.remove name map)

let replaceChildren jsonValue children =
    match jsonValue with
    | JsonValue.Array _ -> JsonValue.Array children
    | JsonValue.Obj map ->
        let keys = [ for kv in map -> kv.Key ]
        let zipped = List.zip keys children
        JsonValue.Obj (Map.ofList zipped)

/// Moves the zipper upwards
let up zipper =
    match zipper with
    | ListZipper(f,p,ls,rs) ->
        match p with
        | Some (ListZipper(p_f,p_p,p_ls,p_rs)) -> ListZipper(replaceChildren p_f ((List.rev ls) @ f :: rs),p_p,p_ls,p_rs)
        | Some (MapZipper(p_f,p_p,p_n,p_m)) -> MapZipper(replaceChildren p_f ((List.rev ls) @ f :: rs),p_p,p_n,p_m)
    | MapZipper(f,p,n,m) -> 
        match p with
        | Some (ListZipper(p_f,p_p,p_ls,p_rs)) -> ListZipper(JsonValue.Obj(Map.add n f m),p_p,p_ls,p_rs)
        | Some (MapZipper(p_f,p_p,p_n,p_m)) -> MapZipper(JsonValue.Obj(Map.add n f m),p_p,p_n,p_m)

/// Creates a Json zipper
let zipper jsonValue =  ListZipper(jsonValue,None,[],[])

/// Returns the whole Json document from the zipper
let rec getJson zipper = 
    match parent zipper with
    | None -> focus zipper
    | Some parent -> up zipper |> getJson