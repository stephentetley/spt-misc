// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetBase

module Attributes =
    
    
    open FSharp.Data

    type Attributes = Map<string, JsonValue>

    let addNotNull (name:string) (value:JsonValue) (attrs:Attributes) : Attributes = 
        match value with
        | JsonValue.Null -> attrs
        | JsonValue.String(s) when s = null || s = ""-> attrs
        | _ -> Map.add name value attrs

    let toJsonRecord (attributes:Attributes) : JsonValue = 
        Map.toArray attributes |>  JsonValue.Record 
