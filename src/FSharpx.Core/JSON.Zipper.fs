module FSharpx.JSON.Zipper

/// A zipper for JsonValues
/// inspired by https://gist.github.com/868042 

type JsonZipper = 
| ListZipper of JsonValue option * JsonZipper option * JsonValue list * JsonValue list
| MapZipper of (string*JsonValue) option * JsonZipper option * list<string*JsonValue> * list<string*JsonValue>

/// Returns the JsonValue under focus
let inline focus zipper = 
    match zipper with
    | ListZipper(Some f,_,_,_) -> f
    | MapZipper(Some (n,f),_,_,_) -> f

/// Creates a Json zipper with the given parent
let toZipperWithParent parent jsonValue =  
    match jsonValue with
    | JsonValue.Array a -> 
        match a with
        | [] -> ListZipper(None,parent,[],[])
        | x::xs -> ListZipper(Some x,parent,[],xs)
    | JsonValue.Obj properties -> 
        match properties with
        | [] -> MapZipper(None,parent,[],[])
        | x::xs -> MapZipper(Some x,parent,[],xs)

/// Returns the parent of the zipper
let parent zipper =  
    match zipper with
    | ListZipper(_,p,_,_) -> p
    | MapZipper(_,p,_,_) -> p

/// Creates a Json zipper
let toZipper jsonValue = toZipperWithParent None jsonValue

/// Changes the element under the focus
let inline update jsonValue zipper = 
    match zipper with
    | ListZipper(Some f,p,ls,rs) -> ListZipper(Some jsonValue,p,ls,rs)
    | MapZipper(Some (n,f),p,ls,rs)-> MapZipper(Some (n,jsonValue),p,ls,rs)

/// Inserts an element at the current position
let insert element zipper =
    match zipper with
    | ListZipper(None,p,ls,rs) -> ListZipper(Some element,p,ls,rs)
    | ListZipper(Some f,p,ls,rs) -> ListZipper(Some element,p,ls,f::rs)

/// Inserts a property with the given name and value into the current focus
let addProperty name element zipper =
    match zipper with
    | MapZipper(None,p,ls,rs) -> MapZipper(Some (name,element),p,ls,rs)
    | MapZipper(Some f,p,ls,rs) -> MapZipper(Some (name,element),p,f::ls,rs)

/// Moves the zipper to the left
let left zipper = 
    match zipper with
    | ListZipper(Some f,p,ls,rs) -> 
        match ls with
        | x::xs -> ListZipper(Some x,p,xs,f::rs)    
    | MapZipper(Some f,p,ls,rs) -> 
        match ls with
        | x::xs -> MapZipper(Some x,p,xs,f::rs)    

/// Moves the zipper to the right
let right zipper = 
    match zipper with
    | ListZipper(Some f,p,ls,rs) -> 
        match rs with
        | x::xs -> ListZipper(Some x,p,f::ls,xs)
        | _ -> ListZipper(None,p,f::ls,[])
    | ListZipper(None,p,ls,rs) -> 
        match rs with
        | x::xs -> ListZipper(Some x,p,ls,xs)
    | MapZipper(Some f,p,ls,rs) -> 
        match rs with
        | x::xs -> MapZipper(Some x,p,f::ls,xs)
        | _ -> MapZipper(None,p,f::ls,[])
    | MapZipper(None,p,ls,rs) -> 
        match rs with
        | x::xs -> MapZipper(Some x,p,ls,xs)

/// Moves the zippern positions to the right
let rec moveRight n zipper = 
    if n <= 0 then zipper else
    zipper |> right |> moveRight (n-1)

/// Moves the zipper down
let down zipper =
    match focus zipper with
    | JsonValue.Obj [] -> MapZipper(None,Some zipper,[],[])
    | JsonValue.Obj (x::xs) -> MapZipper(Some x,Some zipper,[],xs)
    | JsonValue.Array [] -> ListZipper(None,Some zipper,[],[])
    | JsonValue.Array (x::xs) -> ListZipper(Some x,Some zipper,[],xs)    

/// Moves the zipper to the property with the given name on the same level
let toProperty name zipper =
    let properties,p =
        match zipper with
        | MapZipper(Some f,p,ls,rs) -> (List.rev ls) @ f :: rs,p
        | MapZipper(None,p,ls,rs) -> (List.rev ls) @ rs,p

    let rec loop z =
        match z with
        | MapZipper(Some (k,_),_,_,_) -> if k = name then z else right z |> loop
        | MapZipper(None,_,_,x::_) -> right z |> loop
        | MapZipper(None,_,_,_) -> z

    loop (MapZipper(None,p,[],properties))

/// Removes the element or property from the current level
let remove zipper =
    match zipper with
    | MapZipper(Some (k,v),p,ls,rs) ->  
        match (List.rev ls) @ rs with
        | x::xs -> MapZipper(Some x,p,[],xs)
        | _ -> MapZipper(None,p,[],[])

/// Moves the zipper upwards
let up zipper =
    match zipper with
    | ListZipper(_,None,_,_) -> zipper
    | MapZipper(_,None,_,_) -> zipper
    | ListZipper(Some f,p,ls,rs) ->
        match p with
        | Some (ListZipper(_,p_p,p_ls,p_rs)) -> ListZipper(Some(JsonValue.Array ((List.rev ls) @ f :: rs)),p_p,p_ls,p_rs)
        | Some (MapZipper(Some (n,_),p_p,p_ls,p_rs)) -> MapZipper(Some(n,JsonValue.Array((List.rev ls) @ f :: rs)),p_p,p_ls,p_rs)
    | ListZipper(None,p,ls,rs) ->
        match p with
        | Some (ListZipper(_,p_p,p_ls,p_rs)) -> ListZipper(Some(JsonValue.Array ((List.rev ls) @ rs)),p_p,p_ls,p_rs)
        | Some (MapZipper(Some (n,_),p_p,p_ls,p_rs)) -> MapZipper(Some(n,JsonValue.Array((List.rev ls) @ rs)),p_p,p_ls,p_rs)
    | MapZipper(Some f,p,ls,rs) -> 
        match p with
        | Some (ListZipper(_,p_p,p_ls,p_rs)) -> ListZipper(Some(JsonValue.Obj((List.rev ls) @ f :: rs)),p_p,p_ls,p_rs)
        | Some (MapZipper(Some (n,_),p_p,p_ls,p_rs)) -> MapZipper(Some(n,JsonValue.Obj((List.rev ls) @ f :: rs)),p_p,p_ls,p_rs)
    | MapZipper(None,p,ls,rs) -> 
        match p with
        | Some (ListZipper(_,p_p,p_ls,p_rs)) -> ListZipper(Some(JsonValue.Obj((List.rev ls) @ rs)),p_p,p_ls,p_rs)
        | Some (MapZipper(Some (n,_),p_p,p_ls,p_rs)) -> MapZipper(Some(n,JsonValue.Obj((List.rev ls) @ rs)),p_p,p_ls,p_rs)

/// Moves the zipper to the top
let rec top zipper = 
    match parent zipper with
    | None -> zipper
    | Some parent -> up zipper |> top

/// Returns the whole Json document from the zipper
let rec fromZipper zipper = 
    match parent zipper with
    | None ->         
        match zipper with
        | MapZipper(Some f,p,ls,rs) -> JsonValue.Obj((List.rev ls) @ f :: rs)
        | MapZipper(None,p,ls,rs) -> JsonValue.Obj((List.rev ls) @ rs)
        | ListZipper(Some f,_,ls,rs) -> JsonValue.Array((List.rev ls) @ f :: rs)
        | ListZipper(None,_,ls,rs) -> JsonValue.Array((List.rev ls) @ rs)
    | Some parent -> top zipper |> fromZipper
