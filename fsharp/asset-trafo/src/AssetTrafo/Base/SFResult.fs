// Copyright (c) Stephen Tetley 2019
// License: BSD 3 Clause

namespace AssetTrafo.Base


module SFResult =
    
    open FSharp.Core

    type ErrMsg = string

    type Ans<'ans> = Result<'ans, ErrMsg>
    
    type Fk<'ans> = ErrMsg -> Ans<'ans>
    
    type Sk<'a, 'ans> = 'a -> Fk<'ans> -> Ans<'ans>  

    type SFResult<'a, 'ans> = SFResult of (Sk<'a, 'ans> -> Fk<'ans> -> Ans<'ans>)

    let private apply1 (ma : SFResult<'a, 'ans>)
                       (success : Sk<'a, 'ans>)
                       (failure : Fk<'ans>) : Ans<'ans> = 
        let (SFResult f) = ma in f success failure

    let private failM () : SFResult<'a, 'ans> = 
        SFResult <| fun _ fk -> fk "failM"

    let mreturn (x : 'a) : SFResult<'a, 'ans> =
        SFResult <| fun sk fk -> sk x fk

    let bindM (ma : SFResult<'a, 'ans>)
              (next : 'a -> SFResult<'b, 'ans>) : SFResult<'b, 'ans> =
        SFResult <| fun sk -> 
            apply1 ma (fun a -> apply1 (next a) sk)

    let mzero () : SFResult<'a, 'ans> = 
        SFResult <| fun _ _ -> Error "mzero"

    let mplus (ma : SFResult<'a, 'ans>) 
              (mb : SFResult<'a, 'ans>) : SFResult<'a, 'ans> = 
        SFResult <| fun sk fk ->
            match apply1 ma sk fk with
            | Error _ -> apply1 mb sk fk
            | Ok a -> Ok a
    
    let inline private delayM (fn:unit -> SFResult<'a, 'ans>) : SFResult<'a, 'ans> = 
        bindM (mreturn ()) fn 


    type SFResultBuilder() = 
        member self.Return x    = mreturn x
        member self.Bind (p,f)  = bindM p f
        member self.Zero ()     = mzero ()
        member self.Combine (ma, mb) = mplus ma mb
        member self.ReturnFrom(ma:SFResult<'a, 'ans>) : SFResult<'a, 'ans> = ma
        
    let (worksf:SFResultBuilder) = new SFResultBuilder()

    let runSFResult (ma : SFResult<'a, 'a>) : Ans<'a> = 
        apply1 ma (fun a _ -> Ok a) (fun msg -> Error msg)

    let resultError (msg : string) : SFResult<'a, 'ans> = 
        SFResult <| fun _ _ -> Error msg