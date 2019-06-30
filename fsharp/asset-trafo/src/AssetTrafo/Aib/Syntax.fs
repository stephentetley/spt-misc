// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetTrafo.Aib


module Syntax =
    
    open System.IO
    open FSharp.Data

    open AssetTrafo.Base.Attributes
    open AssetTrafo.Base.JsonReader
    
    type AibUnknown =
        { Uid : string
          Name : string
          Attributes : Attributes
        }


    type AibEquipment =
        { Uid : string
          Name : string
          Attributes : Attributes
        }

    type AibPlantItem =
        { Uid : string
          Name : string
          Attributes : Attributes
          Kids : AibEquipment list
        }

    type AibPlantAssemblyKid = 
        | AibPlantAssemblyKidPlantItem of AibPlantItem
        | AibPlantAssemblyKidEquipment of AibEquipment
        | AibPlantAssemblyKidUnknown of AibUnknown

    type AibPlantAssembly = 
        { Uid : string
          Name : string
          Attributes : Attributes
          Kids : AibPlantAssemblyKid list
        }
    
    type AibProcessKid =
        | AibProcessKidPlantAssembly of AibPlantAssembly
        | AibProcessKidPlantItem of AibPlantItem

    type AibProcess =
        { Uid : string
          Name : string
          Attributes : Attributes
          Kids : AibProcessKid list
        }

    type AibProcessGroupKid =
        | AibProcessGroupKidProcess of AibProcess

    type AibProcessGroup = 
        { Uid : string
          Name : string
          Attributes : Attributes
          Kids : AibProcessGroupKid list
        }

    type AibInstallationKid =
        | AibInstallationKidProcessGroup of AibProcessGroup  
        | AibInstallationKidProcess of AibProcess

    type AibInstallation = 
        { Uid : string
          Name : string
          Attributes : Attributes
          Kids : AibInstallationKid list
        }

    let internal readAibInstallation : JsonReader<AibInstallation> = 
        readRecord 
            <| jsonRecord { 
                    let! uid = readField "assetReference" readString
                    let! name = readField "name" readString
                    let! attrs = readField "attributes" <| Attributes.ReadJson ()
                    return { Uid = uid
                             Name = name
                             Attributes = attrs
                             Kids = []
                        }
                }

    let readAibInstallationJson (inpath : string) : Result<AibInstallation, ErrMsg> = 
        try
            use source = new StreamReader(path = inpath)
            JsonValue.Load(source) |> runJsonReader readAibInstallation 
        with
        | _ -> Error "Could not read input"

        
