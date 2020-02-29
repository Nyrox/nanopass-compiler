open Language
open Japanese
open LanguageSimple

type BinaryOp =
    | Add
    | Subtract
    | Multiply
    | Divide



type Value =
    val value: int32

    new(i) = {value=i}


type BinaryOperation =
    val op: BinaryOp

    new(o) = {op=o}

let mathlang = language {
    item (fun x -> Value 5) None
    item (fun x -> BinaryOperation Add, [Value 5, Value 6]) (Some typeof<Value * Value>)
}



let cast<'t> (a: obj) =
    a :?> 't

let codegen = pass {
    from mathlang

    transform (fun transform (op: BinaryOperation) (children: Node list) ->
        let [left; right] = children
        let l = transform left
        let r = transform right
        let lv = (l.data |> cast<Value>).value
        let rv = (r.data |> cast<Value>).value

        let d = (match op.op with
                | Add -> lv + rv
                | Subtract -> lv - rv
                | _ -> failwith "unimplemented") :> obj

        {
            data=d;
            children=[];
            dataType=typeof<int32>
        }
    )

    transform (fun transform (number: Value) _ ->
        {
            data=Value (number.value);
            children=[];
            dataType=typeof<int32>
        }
    )
}

let leaf (v: 'a): Node =
    {data=v :> obj; children=[]; dataType= typeof<'a>}

let node (v: 'a) (c: Node list): Node =
    {
        data=v;
        children=c;
        dataType=typeof<'a>
    }

type Variable = {
    name: string
}

type If () = class end
type IfOneArmed () = class end
type ExprStmt () = class end

let variable = terminal<Variable> ()

let expr = production "expr" (fun expr ->
   [
        variant<If> [expr; expr; expr]
        variant<IfOneArmed> [expr; expr]
        terminalVariant variable
   ]
)

let stmt = production "stmt" (fun stmt ->
    [
        variant<ExprStmt> [AbstractType.Production expr.id]
    ]
)

let l0 = create [variable] [expr; stmt]

l0 |> printfn "%A"

[<EntryPoint>]
let main argv =

    let leaf (v: 'a): Node =
        {data=v :> obj; children=[]; dataType= typeof<'a>}

    let node (v: 'a) (c: Node list): Node =
        {
            data=v;
            children=c;
            dataType=typeof<'a>
        }


    let statements = [
        leaf (Declaration "x")
        leaf (Declaration "y")
        node (Assignment) [leaf <| Topic "x"; leaf <| Value 5]
        node (Assignment) [leaf <| Topic "y"; leaf <| Value 8]
    ]

    // statements
    //     |> Seq.iter (interpret.Transform >> ignore)

    // varTable |> printfn "%A"


    0 // return an integer exit code




