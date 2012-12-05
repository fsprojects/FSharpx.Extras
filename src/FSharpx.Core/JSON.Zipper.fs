module FSharpx.JSON.Zipper

/// A zipper for JsonValues
/// inspired by https://gist.github.com/868042 

type JsonZipper = 
| ListZipper of JsonValue option * JsonZipper option * JsonValue list * JsonValue list
| MapZipper of JsonValue option * JsonZipper option * string * Map<string,JsonValue>

/// Returns the JsonValue under focus
let inline focus zipper = 
    match zipper with
    | ListZipper(Some f,_,_,_) -> f
    | MapZipper(Some f,_,_,_) -> f

/// Returns the parent of the current zipper
let inline parent zipper = 
    match zipper with
    | ListZipper(_,p,_,_) -> p
    | MapZipper(_,p,_,_) -> p

/// Changes the element under the focus
let inline update jsonValue zipper = 
    match zipper with
    | ListZipper(Some f,p,ls,rs) -> ListZipper(Some jsonValue,p,ls,rs)
    | MapZipper(Some f,p,n,m) -> MapZipper(Some jsonValue,p,n,m)

/// Inserts an element at the current position
let insert element zipper =
    match zipper with
    | ListZipper(None,p,ls,rs) -> ListZipper(Some element,p,ls,rs)
    | ListZipper(Some f,p,ls,rs) -> ListZipper(Some element,p,ls,f::rs)

/// Inserts a property with the given name and value into the current focus
let addProperty name element zipper =
    match zipper with
    | MapZipper(None,p,n,m) -> MapZipper(Some element,p,name,m)
    | MapZipper(Some f,p,n,m) -> MapZipper(Some element,p,name,Map.add n f m)

/// Moves the zipper to the left
let left zipper = 
    match zipper with
    | ListZipper(Some f,p,ls,rs) -> 
        match ls with
        | x::xs -> ListZipper(Some x,p,xs,f::rs)    

/// Moves the zipper to the right
let right zipper = 
    match zipper with
    | ListZipper(Some f,p,ls,rs) -> 
        match rs with
        | x::xs -> ListZipper(Some x,p,f::ls,xs)
        | _ -> ListZipper(None,p,f::ls,[])

/// Moves the zipper down
let down zipper =
    match focus zipper with
    | JsonValue.Obj map ->
        if Map.isEmpty map then MapZipper(None,Some zipper,"",map) else
        let e = Seq.head map
        MapZipper(Some e.Value,Some zipper,e.Key,Map.remove e.Key map)
    | JsonValue.Array (x::xs) -> ListZipper(Some x,Some zipper,[],xs)    

/// Moves the zipper to the property with the given name on the same level
let toProperty name zipper =
    match zipper with
    | MapZipper(Some f,p,n,m) ->
        let map = Map.add n f m
        MapZipper(Map.tryFind name map,p,name,Map.remove name map)
    | MapZipper(None,p,n,m) ->
        MapZipper(Map.tryFind name m,p,name,Map.remove name m)

/// Removes the element or property from the current level
let remove zipper =
    match zipper with
    | MapZipper(_,p,n,m) ->  
        if m = Map.empty then MapZipper(None,p,"",m) else
        let kv = Seq.head m
        MapZipper(Some kv.Value,p,kv.Key,Map.remove kv.Key m)

/// Moves the zipper upwards
let up zipper =
    match zipper with
    | ListZipper(Some f,p,ls,rs) ->
        match p with
        | Some (ListZipper(_,p_p,p_ls,p_rs)) -> ListZipper(Some(JsonValue.Array ((List.rev ls) @ f :: rs)),p_p,p_ls,p_rs)
        | Some (MapZipper(Some p_f,p_p,p_n,p_m)) -> MapZipper(Some(JsonValue.Array((List.rev ls) @ f :: rs)),p_p,p_n,p_m)
    | ListZipper(None,p,ls,rs) ->
        match p with
        | Some (ListZipper(_,p_p,p_ls,p_rs)) -> ListZipper(Some(JsonValue.Array ((List.rev ls) @ rs)),p_p,p_ls,p_rs)
        | Some (MapZipper(Some p_f,p_p,p_n,p_m)) -> MapZipper(Some(JsonValue.Array((List.rev ls) @ rs)),p_p,p_n,p_m)
    | MapZipper(Some f,p,n,m) -> 
        match p with
        | Some (ListZipper(_,p_p,p_ls,p_rs)) -> ListZipper(Some(JsonValue.Obj(Map.add n f m)),p_p,p_ls,p_rs)
        | Some (MapZipper(Some p_f,p_p,p_n,p_m)) -> MapZipper(Some(JsonValue.Obj(Map.add n f m)),p_p,p_n,p_m)
    | MapZipper(None,p,n,m) -> 
        match p with
        | Some (ListZipper(_,p_p,p_ls,p_rs)) -> ListZipper(Some(JsonValue.Obj m),p_p,p_ls,p_rs)
        | Some (MapZipper(Some p_f,p_p,p_n,p_m)) -> MapZipper(Some(JsonValue.Obj m),p_p,p_n,p_m)

/// Creates a Json zipper
let toZipper jsonValue =  
    match jsonValue with
    | JsonValue.Array a -> 
        match a with
        | [] -> ListZipper(None,None,[],[])
        | x::xs -> ListZipper(Some x,None,[],xs)
    | JsonValue.Obj map -> 
        if Map.isEmpty map then MapZipper(None,None,"",map) else
        let e = Seq.head map
        MapZipper(Some e.Value,None,e.Key,Map.remove e.Key map)

/// Returns the whole Json document from the zipper
let rec fromZipper zipper = 
    match parent zipper with
    | None ->         
        match zipper with
        | MapZipper(Some f,p,n,m) -> JsonValue.Obj (Map.add n f m)
        | MapZipper(None,p,n,m) -> JsonValue.Obj m
        | ListZipper(Some f,_,ls,rs) -> JsonValue.Array((List.rev ls) @ f :: rs)
        | ListZipper(None,_,_,_) -> JsonValue.Array []
    | Some parent -> up zipper |> fromZipper