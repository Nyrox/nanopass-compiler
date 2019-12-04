// Learn more about F# at http://fsharp.org

open NanopassTP.Generator
open System
open Parse
open System.Reflection

type ITerminal =
    abstract member Name: unit -> string
    abstract member Parse: Parse.Parser<obj>



            
type Language =
    val Items: Type list
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
    member self.Item<'a> (lang: Language, parser: unit -> 'a) =
        Language (typeof<'a> :: lang.Items, lang.Entry)


let language = LanguageBuilder()

type TransformCallback = obj * (obj -> obj) -> obj

type Transform =
    val from: Language option
    val into: Language option
    val transforms: (System.Type * TransformCallback) list

    new() = { from=None; into=None; transforms=[] }
    new(f, i, t) = {from=f; into=i; transforms=t}

    member this._Transform t node =
        t |> printfn "%A"
        this.transforms
            |> Seq.find (fun (p, _) -> p = t)
            |> fun (p, map) ->
                map (node, (fun o -> 
                    this._Transform (o.GetType()) o
                ))
                
    
    member this.Transform<'a> (ast: 'a) =
        this._Transform typeof<'a> ast

type TransformBuilder() = 
    member Self.Yield (()) = Transform ()

    [<CustomOperation("from")>]
    member self.From (prev: Transform, from: Language) =
        Transform (Some from, prev.into, prev.transforms)

    [<CustomOperation("into")>]
    member self.Into (prev: Transform, into: Language) =
        Transform (prev.from, Some into, prev.transforms)
    
    [<CustomOperation("transform")>]
    member self.Transform (prev: Transform, map: ('a * (obj -> obj)) -> obj) =
        let cb: TransformCallback = 
            fun (a: obj, t) -> map (a :?> 'a, t)

        Transform (prev.from, prev.into, (typeof<'a>, cb) :: prev.transforms)

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
    val left: Value
    val right: Value

    new(o, l, r) = {op=o;left=l;right=r}

let mathlang = language {
    item (fun x -> Value 5)
    item (fun x -> BinaryOperation (Add, Value 5, Value 10))
}

let cast<'t> (a: obj) =
    a :?> 't

let codegen = pass {
    from mathlang
    
    transform (fun (op: BinaryOperation, transform) ->
        let l = (transform op.left |> cast<Value>).value
        let r = (transform op.right |> cast<Value>).value

        (match op.op with
        | Add -> l + r
        | Subtract -> l - r
        | _ -> failwith "unimplemented") :> Object
    )
    
    transform (fun (number: Value, transform) -> 
        (Value (number.value)) :> obj
    )
}


let ast = BinaryOperation (Add, Value 12, BinaryOperation (Sub, Value 12, Value 5))

codegen.Transform (ast)
    |> printfn "%A"




codegen |> printfn "%A"



[<EntryPoint>]
let main argv =


    0 // return an integer exit code

