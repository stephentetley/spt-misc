﻿// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetBase

module AibFlat =
    
    open System.IO
    open FSharp.Interop.Excel
    open FSharp.Data

    type AibTable = 
        ExcelFile< @"..\data\aib_sample_site.xlsx"
                 , HasHeaders = true
                 , ForceString = true >

    type AibRow = AibTable.Row

    let readAibFlat (path:string) : AibRow list = 
        let table = new AibTable(filename = path)
        let isBlank (row:AibRow) = 
            match row.Reference with 
            | null | "" -> true
            | _ -> false
        table.Data |> Seq.filter (not << isBlank) |> Seq.toList

    /// Assets are built as a rose tree
    type Node = 
        { Sai : string 
          CommonName : string
          HKey : string
          GridRef : string
          AssetStatus : string
          InAide : bool
          InstalledFrom : string
          Kids : Node list
        }

    let commonName1 (fullName: string) : string = 
        let arr = fullName.Split('/')
        let len = arr.Length
        if len = 2 then 
            fullName
        else
            arr.[len - 1]
    
    let isDirectPrefix (parentName:string) (childName:string) : bool =
        if childName.StartsWith(parentName) then
            let arr1 = parentName.Split('/')
            let arr2 = childName.Split('/')
            arr1.Length = arr2.Length - 1
        else 
            false

    let findChildRows (parentName:string) (rows:AibRow list) : AibRow list = 
        rows |> List.filter (fun row -> isDirectPrefix parentName row.``Common Name``)


    let readBool (value:string) : bool = 
        match value with 
        | null | "FALSE" -> false
        | "TRUE" -> true
        | _ -> false

    let hkeyToType (hkey:string) : string = 
        let cleanKey = match hkey with | null -> "" | _ -> hkey
        if cleanKey = "TODO" then 
            "Undefined"
        else
            match cleanKey.Length with
            | 1 -> "BusinessUnit"
            | 4 -> "System"
            | 8 -> "Function"
            | 13 -> "Installation"
            | 18 -> "SubInstallation"
            | 20 -> "ProcessGroup"
            | 24 -> "Process"
            | 31 -> "PlantAssembly"
            | 36 -> "PlantItem"
            | _ -> "Undentified"

    let findType (elementName:string) (hkey:string) : string = 
        if elementName.StartsWith("EQUIPMENT: ") then
            "Equipment"
        else
            hkeyToType hkey


    let makeNode (row:AibRow) (kids:Node list) : Node = 
        { Sai = row.Reference 
          CommonName = row.``Common Name``
          HKey = row.``Hierarchy Key``
          GridRef = row.``Loc.Ref.``
          AssetStatus = row.AssetStatus
          InAide = readBool row.``Asset in AIDE ?``
          InstalledFrom = row.``Installed From``
          Kids = kids
        }

    

    let aibToNode (input:AibRow list) : Node option = 
        let rec work (rows:AibRow list) (parentName:string) (cont : Node list -> Node) = 
            let childRows = findChildRows parentName rows
            workList childRows rows cont
        and workList (kids:AibRow List) (rows:AibRow list) (cont : Node list -> Node) = 
            match kids with
            | [] -> cont []
            | k1 :: rest -> 
                work rows (k1.``Common Name``) (fun ys ->
                let node1 = makeNode k1 ys 
                workList rest rows (fun nodes -> 
                cont (node1 :: nodes)))
        match input with 
        | [] -> None
        | x :: xs -> 
            work xs (x.``Common Name``) (makeNode x) |> Some


    let toJsonValue (input:Node) : JsonValue =
        let rec work asset cont = 
            workList asset.Kids (fun kids -> 
            let jsArr = JsonValue.Array (List.toArray kids)
            let assetType = findType (commonName1 asset.CommonName) asset.HKey
            let hkey = 
                if assetType = "Equipment" then "" else asset.HKey
                    
            cont (JsonValue.Record 
                    [| ("sai",              JsonValue.String asset.Sai)
                     ; ("type",             JsonValue.String assetType)    
                     ; ("hkey",             JsonValue.String hkey)
                     ; ("elementName",      JsonValue.String <| commonName1 asset.CommonName )
                     ; ("commonName",       JsonValue.String asset.CommonName )
                     ; ("gridRef",          JsonValue.String asset.GridRef )
                     ; ("assetStatus",      JsonValue.String asset.AssetStatus )
                     ; ("inAide",           JsonValue.Boolean asset.InAide)  
                     ; ("installedFrom",    JsonValue.String asset.InstalledFrom)
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

    let aibXlsxToJson (inputPath:string) (outputPath:string) : unit = 
        match readAibFlat inputPath |> aibToNode |> Option.map toJsonValue with
        | None -> printfn "Read error"
        | Some json -> 
            use sw = new StreamWriter (path = outputPath)
            json.WriteTo(sw, JsonSaveOptions.None)


