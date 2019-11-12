// Learn more about F# at http://fsharp.org

open NanopassTP.Generator
open System
open Parse

type ITerminal =
    abstract member Name: unit -> string
    abstract member Parse: Parse.Parser<obj>



            
type Language =
    val Terminals: Type list
    val Nonterminals: Type list
    val Entry: Type option

    new() = { Terminals=[]; Nonterminals=[]; Entry=None; }
    new(t, n, e) = { Terminals=t; Nonterminals=n; Entry=e }

    member this.ParseTerminal<'a when 'a :> ITerminal> (): Parser<'a> =
        let termType = Seq.find (fun t -> t = typeof<'a>) this.Terminals
        fun input ->
            (termType.GetMethod "ParseSelf").Invoke(null, [| input :> obj |]) :?> Option<'a> * string
        
let EmptyLanguage = Language()

type LanguageBuilder() = 
    member Self.Yield (()) = EmptyLanguage

    [<CustomOperation("extends")>]
    member self.Extends (pre: Language, lang: Language) =
        lang

    [<CustomOperation("terminal")>]
    member self.Terminal (lang: Language, term: Type) =
        Language (term :: lang.Terminals, lang.Nonterminals, lang.Entry)


    // [<CustomOperation("nonterminal")>]
    // member self.Nonterminal (lang: Language, term: ITerminal) =
    //     { lang with
    //         nonterminals = term :: lang.nonterminals
    //     }

let language = LanguageBuilder()

type NumberLiteral = 
    val value: int32

    new(i) = {value=i}

    static member ParseSelf (input: string) =
        let parse = Parse.mapSome (NumberLiteral) Parse.number
        parse input
    
    interface ITerminal with
        member this.Name () = "Number"
        member this.Parse = Parse.erase Parse.digit
    

let lang = language {
    terminal typeof<NumberLiteral>
    
    // nonterminal "factor" (fun language input ->
    //     language.ParseTerminal<IntegerLiteral>()
    //     ()
    // )
}

// let lang2 = language {
//     extends lang
//     terminal (NumberLiteral ())
// }

lang.ParseTerminal<NumberLiteral>() "123"
    |> fst
    |> (fun f -> f.Value.value)
    |> printfn "%A"


[<EntryPoint>]
let main argv =


    0 // return an integer exit code

