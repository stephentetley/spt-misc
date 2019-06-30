// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetTrafo.Aib


module Unitype =
    
    open System.IO
    open FSharp.Data
    open FSharp.Data.JsonExtensions

    open SLFormat.RoseTree

    open AssetTrafo.Base.JsonWriter
    open AssetTrafo.Base.Attributes

    let private getField (key:string) 
                         (arr:(string * JsonValue) []) 
                         (unwrap : JsonValue -> SFResult<'a, 'ans>) : SFResult<'a, 'ans> = 
        worksf { 
            match Array.tryFind (fun (k,_) -> k = key) arr with
            | None -> return! resultError (sprintf "not found: %s" key)
            | Some (_, json) -> return! unwrap json
        }

    let getString (json:JsonValue) : SFResult<string, 'ans> = 
        worksf { 
            match json with
            | JsonValue.String str -> return str
            | _ -> return! resultError "Not a JsonValue.String"
        }
        
    // let fk () = 
    
    type AibNode = 
        { Uid : string
          NodeName : string
          NodeType : string
          Attributes : Attributes
          Kids : AibNode list
        }

        member x.ToJsonValue () : JsonValue =
            let rec work asset cont = 
                workList asset.Kids (fun kids -> 
                let jsArr = JsonValue.Array (List.toArray kids)
                cont (JsonValue.Record 
                        [| ("assetReference",   JsonValue.String asset.Uid)
                         ; ("name",             JsonValue.String asset.NodeName)
                         ; ("type",             JsonValue.String asset.NodeType)
                         ; ("attributes",       asset.Attributes.ToJsonValue ())
                         ; ("kids",             jsArr)
                        |]))
            and workList xs cont =
                match xs with
                | [] -> cont []
                | kid1 :: kids -> 
                    work kid1 ( fun v1 -> 
                    workList kids ( fun vs -> 
                    cont (v1::vs)))               
            work x id

        static member FromJsonValue (input : JsonValue) : Result<AibNode, string> = 
            match input with
            | JsonValue.Record arr -> 
                runSFResult
                    <| worksf { 
                            let! uid = getField "assetReference" arr getString
                            let! name = getField "name" arr getString
                            return { Uid = uid
                                   ; NodeName = name
                                   ; NodeType = ""
                                   ; Kids = []
                                   ; Attributes = Attributes Map.empty
                                   }
                            }
  
            | _ -> Error "AibNode.FromJsonValue - not a record"


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