namespace FSharpx

type Field = {
    Name: string
    Type: System.Type
    }

type Message = {
    Name: string
    Fields: Field list
    }

module ProtoBufParser =
    let parse fileName =   // TODO: This is just a fake
        { Name = "Person"
          Fields = 
              [ {Name = "id"; Type = typeof<int>}
                {Name = "name"; Type = typeof<string>}
                {Name = "email"; Type = typeof<string option>}] }

