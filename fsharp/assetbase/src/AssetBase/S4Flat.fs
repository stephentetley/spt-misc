// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetBase

module S4Flat = 
    
    open FSharp.Interop.Excel


    type S4Table = 
        ExcelFile< @"..\data\s4_sample.xlsx"
                 , HasHeaders = true
                 , ForceString = true >

    type S4Row = S4Table.Row

    let readS4Flat (path:string) : S4Row list = 
        let table = new S4Table(filename = path)
        let isBlank (row:S4Row) = 
            match row.L1_Name with 
            | null | "" -> true
            | _ -> false
        table.Data |> Seq.filter (not << isBlank) |> Seq.toList
