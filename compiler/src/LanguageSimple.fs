module LanguageSimple

type AbstractType =
    | Production of int
    | Concrete of System.Type

type Variant  = {
    vt: AbstractType
    childTypes: AbstractType list
}

type Production = {
    name: string
    id: int
    variants: Variant list
}

type Language = {
    terminals: System.Type list;
    productions: Production list;
}

let mutable idGenerator = 0
let generateId () =
    idGenerator <- idGenerator + 1
    idGenerator

let create terminals productions =
    let terminals =
        terminals |>
            List.map (fun (AbstractType.Concrete t) -> t)

    {terminals=terminals; productions=productions}


let production name factory =
    let id = generateId ()
    {
        name=name;
        variants=factory (AbstractType.Production id);
        id=id
    }

let terminal<'a> () =
    AbstractType.Concrete typeof<'a>

let variant<'a> childTypes =
    {
        vt=AbstractType.Concrete typeof<'a>
        childTypes=childTypes
    }

let terminalVariant term =
    {
        vt=term
        childTypes=[]
    }


type Transform = {
    from: AbstractType
    into: AbstractType
}

type Pass = {
    source: Language
    target: Language
    transforms: Transform List
}
