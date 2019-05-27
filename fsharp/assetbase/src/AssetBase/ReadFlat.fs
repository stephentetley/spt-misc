// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetBase

[<AutoOpen>]
module ReadFlat =
    
    open System.IO
    open FSharp.Interop.Excel
    open FSharp.Data

    type AssetTable = 
        ExcelFile< @"..\data\sample_site.xlsx"
                 , HasHeaders = true
                 , ForceString = true >

    type AssetRow = AssetTable.Row

    let readFlat (path:string) : AssetRow list = 
        let table = new AssetTable(filename = path)
        let isBlank (row:AssetRow) = 
            match row.Reference with 
            | null | "" -> true
            | _ -> false
        table.Data |> Seq.filter (not << isBlank) |> Seq.toList

    type Asset = 
        { Sai : string 
          CommonName : string
          GridRef: string
          AssetStatus: string
          Kids : Asset list
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

    let findChildRows (parentName:string) (rows:AssetRow list) : AssetRow list = 
        rows |> List.filter (fun row -> isDirectPrefix parentName row.``Common Name``)



    let makeAsset (row:AssetRow) (kids:Asset list) : Asset = 
        { Sai = row.Reference 
          CommonName = row.``Common Name``
          Kids = kids
          GridRef = row.``Loc.Ref.``
          AssetStatus = row.AssetStatus
        }

    

    let toAsset (input:AssetRow list) : Asset option = 
        let rec work (rows:AssetRow list) (parentName:string) (cont : Asset list -> Asset) = 
            let childRows = findChildRows parentName rows
            workList childRows rows cont
        and workList (kids:AssetRow List) (rows:AssetRow list) (cont : Asset list -> Asset) = 
            match kids with
            | [] -> cont []
            | k1 :: rest -> 
                work rows (k1.``Common Name``) (fun ys ->
                let node1 = makeAsset k1 ys 
                workList rest rows (fun nodes -> 
                cont (node1 :: nodes)))
        match input with 
        | [] -> None
        | x :: xs -> 
            work xs (x.``Common Name``) (makeAsset x) |> Some


    let toJsonValue (input:Asset) : JsonValue =
        let rec work asset cont = 
            workList asset.Kids (fun kids -> 
            let jsArr = JsonValue.Array (List.toArray kids)
            cont (JsonValue.Record 
                    [| ("sai",              JsonValue.String asset.Sai)
                     ; ("elementName",      JsonValue.String <| commonName1 asset.CommonName )
                     ; ("commonName",       JsonValue.String asset.CommonName )
                     ; ("gridRef",          JsonValue.String asset.GridRef )
                     ; ("assetStatus",      JsonValue.String asset.AssetStatus )
                     ; ("assets",           jsArr)
                    |]))
        and workList xs cont =
            match xs with
            | [] -> cont []
            | kid1 :: kids -> 
                work kid1 ( fun v1 -> 
                workList kids ( fun vs -> 
                cont (v1::vs)))               
        work input id

    let xlsxToJson (inputPath:string) (outputPath:string) : unit = 
        match readFlat inputPath |> toAsset |> Option.map toJsonValue with
        | None -> printfn "Read error"
        | Some json -> 
            use sw = new StreamWriter (path = outputPath)
            json.WriteTo(sw, JsonSaveOptions.None)


