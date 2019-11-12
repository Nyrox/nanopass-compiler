namespace NanopassTP

open ProviderImplementation.ProvidedTypes
open Microsoft.FSharp.Core.CompilerServices
open System.Reflection


[<TypeProvider>]
type LangProvider (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config)

    let ns = "NanopassTP.Generator"
    let asm = Assembly.GetExecutingAssembly()

    let createTypes () =
        let myType = ProvidedTypeDefinition(asm, ns, "MyType", Some typeof<obj>)
        // let myProp = ProvidedProperty("MyProperty", typeof<string>, isStatic=true, getterCode = (fun args -> <@@ "Hello my World" @@>))

        // myType.AddMember(myProp)
        let staticParams = [ProvidedStaticParameter("bp", typeof<System.Type>)]

        do myType.DefineStaticParameters(
            parameters=staticParams,
            instantiationFunction=(fun typeName parameterValues ->
                match parameterValues with
                | [| :? (string list) as terminals |] ->
                    let ty = ProvidedTypeDefinition(
                        asm,
                        ns,
                        typeName,
                        baseType=Some typeof<obj>
                    )
                    
                    let ctor = ProvidedConstructor([
                        ProvidedParameter("type", typeof<System.Type>)
                    ], invokeCode=fun args -> <@@ 
                                    (%%(args.[0]): System.Type) |> printfn "%A"
                                    "My internal state" :> obj 
                    @@>)
                    ty.AddMember(ctor)

                    ty
                | _-> failwith "unexpected parameter values"
            )
        )


        [myType]


    do
        this.AddNamespace(ns, createTypes())

[<assembly:TypeProviderAssembly>]
do ()
