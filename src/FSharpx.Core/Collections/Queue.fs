// XML docs in fsi

namespace FSharpx.Collections

type Queue<'T> (front : list<'T>, rBack : list<'T>) = 

    member internal this.front = front
    member internal this.rBack = rBack

    member this.Conj x = 
        match front, x::rBack with
        | [], r -> Queue((List.rev r), [])
        | f, r -> Queue(f, r)

    member this.Head = 
        match front with
        | hd::_ -> hd
        | _ -> raise (new System.Exception("Queue is empty"))

    member this.TryHead =
        match front with
        | hd::_ -> Some(hd)
        | _ -> None
         
    member this.IsEmpty = front.IsEmpty

    member this.Length = front.Length + rBack.Length

    member this.Rev() = 
        match rBack, front with
        | [], r -> Queue((List.rev r), [])
        | f, r -> Queue(f, r)

    member this.Tail =
        match front with
        | hd::tl -> 
            match tl, rBack with
            | [], r -> Queue((List.rev r), [])
            | f, r -> Queue(f, r)
        | _ -> raise (new System.Exception("Queue is empty"))
            
    member this.TryTail =
        match front with
        | hd::tl ->
            match tl, rBack with
            | [], r -> Some(Queue((List.rev r), []))
            | f, r -> Some(Queue(f, r))
        | _ -> None

    member this.Uncons =  
        match front with
        | hd::tl -> 
            hd, (match tl, rBack with
                | [], r -> Queue((List.rev r), [])
                | f, r -> Queue(f, r))
        | _ -> raise (new System.Exception("Queue is empty"))

    member this.TryUncons =  
        match front with
        | hd::tl -> 
            match tl, rBack with
            | [], r -> Some(hd, Queue((List.rev r), []))
            | f, r -> Some(hd, Queue(f, r))
        | _ -> None

    with
              
    interface System.Collections.Generic.IEnumerable<'T> with

        member this.GetEnumerator() = 
            let e = seq {
                  yield! front
                  yield! (List.rev rBack)}
            e.GetEnumerator()

        member this.GetEnumerator() = (this :> _ seq).GetEnumerator() :> System.Collections.IEnumerator

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Queue =
    //pattern discriminators  (active pattern)
    let (|Cons|Nil|) (q : Queue<'T>) = match q.TryUncons with Some(a,b) -> Cons(a,b) | None -> Nil

    let inline conj (x : 'T) (q : Queue<'T>) = (q.Conj x) 

    let empty<'T> : Queue<'T> = Queue<_>([], []) 

    let fold (f : ('State -> 'T -> 'State)) (state : 'State) (q : Queue<'T>) = 
        let s = List.fold f state q.front
        List.fold f s (List.rev q.rBack)

    let foldBack (f : ('T -> 'State -> 'State)) (q : Queue<'T>) (state : 'State) =  
        let s = List.foldBack f (List.rev q.rBack) state 
        (List.foldBack f q.front s)

    let inline head (q : Queue<'T>) = q.Head

    let inline tryHead (q : Queue<'T>) = q.TryHead

    let inline isEmpty (q : Queue<'T>) = q.IsEmpty

    let inline length (q : Queue<'T>) = q.Length

    let ofList xs = Queue<'T>(xs, [])

    let ofSeq xs = Queue<'T>((List.ofSeq xs), [])

    let inline rev (q : Queue<'T>) = q.Rev()

    let inline tail (q : Queue<'T>) = q.Tail

    let inline tryTail (q : Queue<'T>) = q.TryTail

    let inline uncons (q : Queue<'T>) = q.Uncons

    let inline tryUncons (q : Queue<'T>) = q.TryUncons