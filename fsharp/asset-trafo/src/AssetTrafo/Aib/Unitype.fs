// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetTrafo.Aib


module Unitype =
    
    open System.IO
    open FSharp.Data

    open SLFormat.RoseTree

    open AssetTrafo.Base.Attributes
    
    type AibNode = 
        { Uid : string
          NodeName : string
          NodeType : string
          Attributes : Attributes
          Kids : AibNode list
        }


    let toJsonValue (input : AibNode) : JsonValue =
        let rec work asset cont = 
            workList asset.Kids (fun kids -> 
            let jsArr = JsonValue.Array (List.toArray kids)
            cont (JsonValue.Record 
                    [| ("assetReference",   JsonValue.String asset.Uid)
                     ; ("name",             JsonValue.String asset.NodeName)
                     ; ("type",             JsonValue.String asset.NodeType)
                     ; ("attributes",       attributesToJson asset.Attributes)
                     ; ("kids",             jsArr)
                    |]))
        and workList xs cont =
            match xs with
            | [] -> cont []
            | kid1 :: kids -> 
                work kid1 ( fun v1 -> 
                workList kids ( fun vs -> 
                cont (v1::vs)))               
        work input id


    let toRoseTree (labeller: AibNode -> string) (input : AibNode) : RoseTree<string> = 
        let rec work item cont = 
            workList item.Kids (fun kids -> 
            let label = labeller item
            cont (RoseTree(label, kids)))
        and workList xs cont =
            match xs with
            | [] -> cont []
            | kid1 :: kids -> 
                work kid1 ( fun v1 -> 
                workList kids ( fun vs -> 
                cont (v1::vs)))               
        work input id

    let drawTree (labeller: AibNode -> string) (input : AibNode) : unit = 
        let output = toRoseTree (fun node -> node.NodeName) input |> SLFormat.RoseTree.drawTree
        printfn "%s" output