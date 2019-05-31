// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetBase

module S4Flat = 
    
    open FSharp.Interop.Excel

    /// PAUSED - need a proper sample of input data


    type S4Table = 
        ExcelFile< @"..\data\s4_sample.xlsx"
                 , HasHeaders = true
                 , ForceString = true >

    type S4Row = S4Table.Row

    let readS4Flat (path:string) : S4Row list = 
        let table = new S4Table(filename = path)
        let isBlank (row:S4Row) = 
            match row.L1_Site_Code with 
            | null | "" -> true
            | _ -> false
        table.Data |> Seq.filter (not << isBlank) |> Seq.toList


    /// Assets are built as a rose tree
    type Node = 
        { Code : string 
          Name : string
          Kids : Node list
        }

    let getAttributes (level:int) (row:S4Row) : {| Code:string; Name: string |} = 
        match level with
        | 1 -> 
            {| Code = row.L1_Site_Code
            ; Name = row.``S/4 Hana Floc Description`` |}
        | 2 -> 
            {| Code = row.``L2_Floc Code``
            ; Name = row.L2_Function |}
        | 3 -> 
            {| Code = row.``L3_Floc Code``
            ; Name = row.``L3_Process Group`` |}
        | 4 -> 
            {| Code = row.``L4_Floc Code``
            ; Name = row.L4_Process |}
        | 5 -> 
            {| Code = row.``L5_Floc Code``
            ; Name = row.``L5_System`` |}
        | _ -> 
            {| Code = "Undefined"; Name = "Undefined" |}
            

    let makeNode (level:int) (row:S4Row) (kids:Node list) : Node = 
        let attrs = getAttributes level row
        { Code = attrs.Code
          Name = attrs.Name
          Kids = kids
        }

    

