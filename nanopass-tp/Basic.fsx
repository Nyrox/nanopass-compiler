#r "./bin/Debug/netstandard2.0/nanopass-tp-generated.dll"

open Nanopass.Generator

MyType.MyProperty
    |> printfn "%A"

let thing = MyType()
thing.InnerState
    |> printfn "%A"