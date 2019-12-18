// Learn more about F# at http://fsharp.org

open NanopassTP.Generator
open System
open Parse
open System.Reflection

type ITerminal =
    abstract member Name: unit -> string
    abstract member Parse: Parse.Parser<obj>



            
type Language =
    val Items: (Type * Type option) list
    val Entry: Type option

    new() = { Items=[]; Entry=None; }
    new(i, e) = { Items=i; Entry=e }

let EmptyLanguage = Language()

type LanguageBuilder() = 
    member Self.Yield (()) = EmptyLanguage

    [<CustomOperation("extends")>]
    member self.Extends (pre: Language, lang: Language) =
        lang

    [<CustomOperation("item")>]
    member self.Item<'a> (lang: Language, parser: unit -> 'a, childTypes: Type option) =
        Language ((typeof<'a>, childTypes) :: lang.Items, lang.Entry)


let language = LanguageBuilder()

// (node, children)
type Node = {
    data: obj;
    dataType: Type;
    children: Node list;
}

type TransformCallback = {
    nodeType: Type;
    childType: Type;
    erasedCb: (Node -> Node) -> obj -> Node list -> Node;
}




type Transform =
    val from: Language option
    val into: Language option
    val transforms: TransformCallback list

    new() = { from=None; into=None; transforms=[] }
    new(f, i, t) = {from=f; into=i; transforms=t}

    member this._Transform (node: Node) =
        node.dataType |> printfn "%A"
        this.transforms
            |> Seq.find (fun transform -> transform.nodeType = node.dataType)
            |> fun transform ->
                transform.erasedCb this._Transform node.data node.children

                
    
    member this.Transform (node: Node) =
        this._Transform node

type TransformBuilder() = 
    member Self.Yield (()) = Transform ()

    [<CustomOperation("from")>]
    member self.From (prev: Transform, from: Language) =
        Transform (Some from, prev.into, prev.transforms)

    [<CustomOperation("into")>]
    member self.Into (prev: Transform, into: Language) =
        Transform (prev.from, Some into, prev.transforms)
    
    [<CustomOperation("transform")>]
    member self.Transform (prev: Transform, map: (Node -> Node) -> 'tNode -> 'tChildren -> Node) =
        let cb = {
            nodeType = typeof<'tNode>;
            childType = typeof<'tChildren>;
            erasedCb = fun transform node children -> map transform (node :?> 'tNode) (children :> obj :?> 'tChildren)
        }

        Transform (prev.from, prev.into, cb :: prev.transforms)

let pass = TransformBuilder()


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
                | _ -> failwith "unimplemented") :> Object

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


let ast = node (BinaryOperation Add) [
    leaf <| Value 5;
    leaf <| Value 7
]

codegen.Transform ast
    |> printfn "%A"




codegen |> printfn "%A"



[<EntryPoint>]
let main argv =


    0 // return an integer exit code

