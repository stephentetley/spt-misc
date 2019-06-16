// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetTrafo.Base


module Attributes =
    
    open FSharp.Data

    type AttrValue = 
        | AttrBool of bool
        | AttrString of string
        member x.ToJsonValue () : JsonValue = 
            match x with
            | AttrBool bool -> JsonValue.Boolean bool
            | AttrString str -> JsonValue.String str
            
        static member FromJsonValue (input:JsonValue) : Result<AttrValue, string> = 
            match input with
            | JsonValue.Boolean bool -> Ok (AttrBool bool)
            | JsonValue.String str -> Ok (AttrString str)
            | _ -> Error "AttrValue - FromJson"

    [<Struct>]
    type Attributes = 
        | Attributes of Map<string, AttrValue>
        
        member x.Attrs 
            with get () : Map<string, AttrValue> = 
                match x with | Attributes(x) -> x

        member x.ToJsonValue () : JsonValue = 
            x.Attrs 
                |> Map.toArray
                |> Array.map (fun (n,v) -> (n, v.ToJsonValue() )) 
                |> JsonValue.Record 

        static member FromJsonValue (input : JsonValue) : Result<Attributes, string> = 
            match input with
            | JsonValue.Record body ->
                body 
                    |> Array.fold (fun ac (k, v) -> 
                                        match AttrValue.FromJsonValue v with
                                        | Ok a -> Result.map (Map.add k a) ac
                                        | Error msg -> Error msg
                                        ) 
                                   (Ok Map.empty)
                    |> Result.map (fun x -> Attributes x)

            | _ -> Error "Attributes.FromJsonValue - not a record"

