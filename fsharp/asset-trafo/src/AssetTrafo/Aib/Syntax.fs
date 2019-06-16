// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetTrafo.Aib


module Syntax =
    
    open System.IO
    open FSharp.Data

    open AssetTrafo.Base.Attributes
    open AssetTrafo.Aib.Unitype
    
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



    let private makeAibNode (uid : string) (name : string) (typeName : string) (attrs : Attributes) (kids : AibNode list) : AibNode = 
        { Uid = uid
          NodeName = name
          NodeType = typeName
          Attributes = attrs 
          Kids = kids
        }


    let private kidsToAibNode (kids : 'item list) 
                              (dispatch : 'item -> (AibNode -> AibNode) -> AibNode)
                              (cont : AibNode list -> AibNode) : AibNode = 
        let rec work (xs : 'item list) (kont : AibNode list -> AibNode) = 
            match xs with
            | [] -> kont []
            | a1 :: rest -> 
                dispatch a1 (fun v1 -> 
                work rest (fun ac -> 
                kont (v1 :: ac)))
        work kids cont


    let private unknownToAibNode (source : AibUnknown) (cont : AibNode -> AibNode) : AibNode = 
        cont (makeAibNode source.Uid source.Name "Unknown" source.Attributes [])

    let private equipmentToAibNode (source : AibEquipment) (cont : AibNode -> AibNode) : AibNode = 
        cont (makeAibNode source.Uid source.Name "Eqipment" source.Attributes [])


    let private plantItemToAibNode (source : AibPlantItem) (cont : AibNode -> AibNode) : AibNode = 
        let matcher x = equipmentToAibNode x
        kidsToAibNode source.Kids matcher (fun kids -> 
        cont (makeAibNode source.Uid source.Name "PlantItem" source.Attributes kids))

    let private plantAssemblyToAibNode (source : AibPlantAssembly) (cont : AibNode -> AibNode) : AibNode = 
        let matcher x = 
            match x with 
            | AibPlantAssemblyKidPlantItem p -> plantItemToAibNode p
            | AibPlantAssemblyKidEquipment p -> equipmentToAibNode p
            | AibPlantAssemblyKidUnknown p -> unknownToAibNode p
        kidsToAibNode source.Kids matcher (fun kids -> 
        cont (makeAibNode source.Uid source.Name "PlantAssembly" source.Attributes kids))
        
    let private processToAibNode (source : AibProcess) (cont : AibNode -> AibNode) : AibNode = 
        let matcher x = 
            match x with 
            | AibProcessKidPlantAssembly p -> plantAssemblyToAibNode p
            | AibProcessKidPlantItem p -> plantItemToAibNode p
        kidsToAibNode source.Kids matcher (fun kids -> 
        cont (makeAibNode source.Uid source.Name "Process" source.Attributes kids))

    let private processGroupToAibNode (source : AibProcessGroup) (cont : AibNode -> AibNode) : AibNode = 
        let matcher x = 
            match x with 
            | AibProcessGroupKidProcess p -> processToAibNode p
        kidsToAibNode source.Kids matcher (fun kids -> 
        cont (makeAibNode source.Uid source.Name "ProcessGroup" source.Attributes kids))
    
    let private installationToAibNode (source : AibInstallation) (cont : AibNode -> AibNode) : AibNode = 
        let matcher x = 
            match x with 
            | AibInstallationKidProcessGroup p -> processGroupToAibNode p
            | AibInstallationKidProcess p -> processToAibNode p
        kidsToAibNode source.Kids matcher (fun kids -> 
        cont (makeAibNode source.Uid source.Name "Installation" source.Attributes kids))


    let toAibNode (site : AibInstallation) : AibNode = 
        installationToAibNode site id

    let siteToJson (site:AibInstallation) (outputPath:string) : unit = 
        let simple = toAibNode site
        let json = simple.ToJsonValue ()
        use sw = new StreamWriter (path = outputPath)
        json.WriteTo(sw, JsonSaveOptions.None)


    let printSite (site:AibInstallation) : unit = 
        toAibNode site |> drawTree (fun node -> node.NodeName)
        
