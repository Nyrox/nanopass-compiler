module Japanese

open Language
open System
open System.Collections.Generic

type Topic =
    val ident: string
    new(i) = {ident=i}

type Declaration =
    val ident: string
    new(i) = {ident=i}

type Assignment =
    new() = {}

type Value =
    val v: int32
    new(i) = {v=i}

let j0 = language {
    item (fun x -> Topic "") None
    item (fun x -> Declaration "") None
    item (fun x -> Assignment) (Some typeof<Topic * Value>)
}

let cast<'t> (a: obj) =
    a :?> 't

let mutable varTable = new Dictionary<String, Value>()

let interpret = pass {
    from j0

    transform (fun transform (_: Assignment) (children: Node list) ->
        let [topic; value] = children
        printfn "%A" (topic, value)

        let t = (topic |> cast<Topic>).ident
        varTable.[t] <- (value |> cast<Value>)

        {
            data=0;
            dataType=typeof<int32>;
            children=[]
        }
    )

    transform (fun transform (decl: Declaration) (_: Node list) ->
        varTable.Add(decl.ident, Value 0)
        {
            data=decl;
            dataType=typeof<Declaration>;
            children=[]
        }
    )
}

