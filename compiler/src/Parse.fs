module Parse

open System

type Parser<'a> = string -> (Option<'a> * string)

let erase<'a> (parser: Parser<'a>) input =
    match parser input with
    | (Some inner, rest) -> Some (inner :> obj), rest
    | None, rest -> None, rest

let peek (input: string) =
    match input.Length with
    | 0 -> None
    | _ -> Some input.[0]

let mapSome<'a, 'b> mapping (parser: Parser<'a>): Parser<'b> =
    fun input ->
        match parser input with
        | Some a, rest -> Some (mapping a), rest
        | None, rest -> None, rest

let digit: Parser<char> = fun input ->
    match Option.map Char.IsDigit (peek input) with
    | Some true -> Some input.[0], input.[1..]
    | _ -> None, input



let many (parser: Parser<'a>): Parser<'a list> =
    let rec inner = fun input ->
        match parser input with
        | Some ret, rest ->
            match inner rest with
            | Some vals, rest -> Some (ret :: vals), rest
            | None, rest -> failwith "not possible?"
        | None, rest -> Some [], rest

    inner


let many1 (parser: Parser<'a>): Parser<'a list> =
    fun input ->
        match many parser input with
        | Some values, rest when values.Length > 0 -> Some values, rest
        | _ -> None, input


let number: Parser<int32> = fun input ->
    match many1 digit input with
    | Some digits, rest -> 
        Some (digits 
            |> System.String.Concat
            |> Int32.Parse), rest
    | None, _ -> None, input