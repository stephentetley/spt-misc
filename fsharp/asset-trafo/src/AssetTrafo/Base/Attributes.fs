// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetTrafo.Base


module Attributes =
    
    open FSharp.Data

    type AttrValue = 
        | AttrBool of bool
        | AttrString of string

    type Attributes = Map<string, AttrValue>
    
    let attrValueToJson ( attrVal : AttrValue) : JsonValue = 
        match attrVal with
        | AttrBool bool -> JsonValue.Boolean bool
        | AttrString str -> JsonValue.String str

    let attributesToJson (attrs:Attributes) : JsonValue = 
        attrs 
            |> Map.toArray
            |> Array.map (fun (n,v) -> (n, attrValueToJson v)) 
            |> JsonValue.Record 
