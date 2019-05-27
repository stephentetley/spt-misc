// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

#r "netstandard"
#r "System.Linq.dll"
open System.IO


#I @"C:\Users\stephen\.nuget\packages\ExcelProvider\1.0.1\lib\netstandard2.0"
#r "ExcelProvider.Runtime.dll"

#I @"C:\Users\stephen\.nuget\packages\ExcelProvider\1.0.1\typeproviders\fsharp41\netstandard2.0"
#r "ExcelDataReader.DataSet.dll"
#r "ExcelDataReader.dll"
#r "ExcelProvider.DesignTime.dll"
open FSharp.Interop.Excel


#I @"C:\Users\stephen\.nuget\packages\FSharp.Data\3.1.1\lib\netstandard2.0"
#r "FSharp.Data.dll"
#r "FSharp.Data.DesignTime.dll"
open FSharp.Data
open FSharp.Data.JsonExtensions


#load "..\src\AssetBase\ReadFlat.fs"
open AssetBase

let demo01 () = 
    readFlat @"G:\work\Projects\asset_sync\ald_.xlsx" 
        |> toAsset
        |> Option.map toJsonValue


let demo02 () = 
    xlsxToJson @"G:\work\Projects\asset_sync\ald_.xlsx"  @"G:\work\Projects\asset_sync\ald_.json" 

