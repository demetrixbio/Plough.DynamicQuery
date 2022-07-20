module Plough.DynamicQuery.Tests.Test

open System
open Plough.DynamicQuery

type TagId = TagId of int
module TagId =
    let get = function TagId id -> id

type TestPredicate =
    | Id of Operation.Int
    | Reference of Operation.Int
    | Deleted of Operation.Bool
    | Created of Operation.DateTime
    | Name of Operation.String
    | Uid of Operation.Guid
    | TagAssignments of Operation.Set<int>
    interface IPredicate

type TestRecord =
    { Id : int
      Uid : Guid
      Name : string option
      Created : DateTime
      IsDeleted : bool
      Reference : int option
      TagAssignments : int [] }

let query (record : TestRecord) : Filter<TestPredicate> =    
    AND [
        Id =. ANY [| 1; 2; 3; 4 |]
        
        NOT ((Id <=. 1000000000) &&. (Id >=. 2000000000))
        
        (Uid !=. record.Uid) ||. (Uid =. Guid.Empty)
        
        record.Reference
        |> Option.map (fun _ -> NOT (ISNULL Reference))
        |> Option.defaultWith (fun () -> ISNULL Reference)
        
        record.Name
        |> Option.map (fun name -> OR [
            Name =. name
            Name =~. "John%"
            Name =~*. "john invariant%"
            // name begins with a digit - postgres regex
            Name =~. "‘^[0–9].*$’"
        ])
        |> Option.defaultWith (fun () ->
            ISNULL Name)
        
        OR [
            ISNULL Uid
            Uid =. NEITHER [| Guid.Empty; Guid.Empty |]
        ]
        
        Created >=. DateTime.Now.AddYears(-1)
        
        TagAssignments ^. [| 1; 2; 3 |]
        
        Deleted =. true
    ]