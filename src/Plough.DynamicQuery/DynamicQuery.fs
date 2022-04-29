namespace Plough.DynamicQuery

type PageQuery() =
    member val PageSize = 50L with get, set
    member val PageIndex = 0L with get, set

type SortDirection =
    | Ascending
    | Descending
    member this.Toggle() =
        match this with
        | Ascending -> Descending
        | Descending -> Ascending

[<RequireQualifiedAccess>]
module SortDirection =
    let toString = function
        | SortDirection.Ascending -> "ascending"
        | SortDirection.Descending -> "descending"

type ResourceList<'a> =
    { Items : 'a list
      PageSize : int64
      PageIndex : int64
      TotalItemCount : int64 option }
   
module ResourceList =
    let map f resourceList =
        { Items = resourceList.Items |> List.map f
          PageSize = resourceList.PageSize
          PageIndex = resourceList.PageIndex
          TotalItemCount = resourceList.TotalItemCount }
type Condition = And | Or

type IPredicate = interface end
and Filter<'t when 't :> IPredicate> =
    | Simple of 't
    | Complex of Complex<'t>
    | Not of Filter<'t>
    | Empty
    
and Complex<'t when 't :> IPredicate> =
    { Condition : Condition 
      Predicates : Filter<'t> list }
    
[<RequireQualifiedAccess>]
module Operation =
    
    [<RequireQualifiedAccess>]
    type CommonOp<'a> =
        | IsNull
        | Eq of 'a
        | NotEq of 'a
    
    [<RequireQualifiedAccess>]
    type SetOp<'a> =
        | Any of array<'a>
        | Neither of array<'a>
    
    [<RequireQualifiedAccess>]
    type Algebraic<'a> =
        | Common of CommonOp<'a>
        | Set of SetOp<'a>
        | GreaterThen of 'a
        | LessThen of 'a
        | GreaterThenOrEquals of 'a
        | LessThenOrEquals of 'a
        
    type Char = Algebraic<System.Char>
    type Byte = Algebraic<System.Byte>
    type Int16 = Algebraic<System.Int16>
    type Int = Algebraic<System.Int32>
    type Int32 = Algebraic<System.Int32>
    type Int64 = Algebraic<System.Int64>
    type Float = Algebraic<System.Double>
    type Decimal = Algebraic<System.Decimal>
    type DateTime = Algebraic<System.DateTime>
    
    [<RequireQualifiedAccess>]
    type String =
        | Common of CommonOp<System.String>
        | Set of SetOp<System.String>
        | Like of System.String
        | ILike of System.String
    
    [<RequireQualifiedAccess>]
    type Bool =
        | Common of CommonOp<System.Boolean>
     
    [<RequireQualifiedAccess>]
    type Guid =
        | Common of CommonOp<System.Guid>
        | Set of SetOp<System.Guid>
    
    [<RequireQualifiedAccess>]
    type Set<'a> =
        | Common of CommonOp<'a>
        /// Does the first set contain the second, that is, does each element appearing in the second set equal some element of the first set?
        | Contains of array<'a>
        /// Is the first set contained by the second?
        | ContainedBy of array<'a>
        /// Do the sets overlap, that is, have any elements in common?
        | Overlap of array<'a>

[<AutoOpen>] 
module DSL =
    type IsNullOp = private | IsNullOp with
        static member (?<-) (predicate:Operation.String -> #IPredicate, IsNullOp, IsNullOp) =
            Operation.CommonOp.IsNull |> Operation.String.Common |> predicate |> Simple

        static member (?<-) (predicate:Operation.Bool -> #IPredicate, IsNullOp, IsNullOp) =
            Operation.CommonOp.IsNull |> Operation.Bool.Common |> predicate |> Simple

        static member (?<-) (predicate:Operation.Guid -> #IPredicate, IsNullOp, IsNullOp) =
            Operation.CommonOp.IsNull |> Operation.Guid.Common |> predicate |> Simple

        static member (?<-) (predicate:Operation.Algebraic<'a> -> #IPredicate, IsNullOp, IsNullOp) =
            Operation.CommonOp.IsNull |> Operation.Algebraic.Common |> predicate |> Simple

        static member (?<-) (predicate:Operation.Set<'a> -> #IPredicate, IsNullOp, IsNullOp) =
            Operation.CommonOp.IsNull |> Operation.Set.Common |> predicate |> Simple

    type EqOp = private | EqOp with
        static member inline (?<-) (a, EqOp, b) = a =. b
        
        static member (?<-) (predicate:Operation.String -> #IPredicate, EqOp, value : string) =
            Operation.CommonOp.Eq value |> Operation.String.Common |> predicate |> Simple
           
        static member (?<-) (predicate:Operation.String -> #IPredicate, EqOp, value : Operation.SetOp<string>) =
            Operation.String.Set value |> predicate |> Simple
            
        static member (?<-) (predicate:Operation.Bool -> #IPredicate, EqOp, value : bool) =
            Operation.CommonOp.Eq value |> Operation.Bool.Common |> predicate |> Simple
        
        static member (?<-) (predicate:Operation.Guid -> #IPredicate, EqOp, value : System.Guid) =
            Operation.CommonOp.Eq value |> Operation.Guid.Common |> predicate |> Simple
            
        static member (?<-) (predicate:Operation.Guid -> #IPredicate, EqOp, value : Operation.SetOp<System.Guid>) =
            Operation.Guid.Set value |> predicate |> Simple
        
        static member (?<-) (predicate:Operation.Algebraic<'a> -> #IPredicate, EqOp, value : 'a) =
            Operation.CommonOp.Eq value |> Operation.Algebraic.Common |> predicate |> Simple
          
        static member (?<-) (predicate:Operation.Algebraic<'a> -> #IPredicate, EqOp, value : Operation.SetOp<'a>) =
            Operation.Algebraic.Set value |> predicate |> Simple
            
        static member (?<-) (predicate:Operation.Set<'a> -> #IPredicate, EqOp, value : 'a) =
            Operation.CommonOp.Eq value |> Operation.Set.Common |> predicate |> Simple
                
    type NotEq = private | NotEq with 
        static member inline (?<-) (a, NotEq, b) = a !=. b
        
        static member (?<-) (predicate:Operation.String -> #IPredicate, NotEq, value : string) =
            Operation.CommonOp.NotEq value |> Operation.String.Common |> predicate |> Simple
            
        static member (?<-) (predicate:Operation.Bool -> #IPredicate, NotEq, value : bool) =
            Operation.CommonOp.NotEq value |> Operation.Bool.Common |> predicate |> Simple
        
        static member (?<-) (predicate:Operation.Guid -> #IPredicate, NotEq, value : System.Guid) =
            Operation.CommonOp.NotEq value |> Operation.Guid.Common |> predicate |> Simple
        
        static member (?<-) (predicate:Operation.Algebraic<'a> -> #IPredicate, NotEq, value : 'a) =
            Operation.CommonOp.NotEq value |> Operation.Algebraic.Common |> predicate |> Simple
            
        static member (?<-) (predicate:Operation.Set<'a> -> #IPredicate, NotEq, value : 'a) =
            Operation.CommonOp.NotEq value |> Operation.Set.Common |> predicate |> Simple
                
    type GreaterThenOp = private | GreaterThenOp with
        static member inline (?<-) (a, GreaterThenOp, b) = a >. b
        
        static member (?<-) (predicate:Operation.Algebraic<'a> -> #IPredicate, GreaterThenOp, value : 'a) =
            Operation.Algebraic.GreaterThen value |> predicate |> Simple
                

    type GreaterThenOrEqualsOp = private | GreaterThenOrEqualsOp with
        static member inline (?<-) (a, GreaterThenOrEqualsOp, b) = a >=. b
        
        static member (?<-) (predicate:Operation.Algebraic<'a> -> #IPredicate, GreaterThenOrEqualsOp, value : 'a) =
            Operation.Algebraic.GreaterThenOrEquals value |> predicate |> Simple
            
        static member (?<-) (predicate:Operation.Set<'a> -> #IPredicate, GreaterThenOrEqualsOp, value : array<'a>) =
            Operation.Set.Contains value |> predicate |> Simple

    type LessThenOp = private | LessThenOp with   
        static member inline (?<-) (a, LessThenOp, b) = a <. b
        
        static member (?<-) (predicate:Operation.Algebraic<'a> -> #IPredicate, LessThenOp, value : 'a) =
            Operation.Algebraic.LessThen value |> predicate |> Simple

    type LessThenOrEqualsOp = private | LessThenOrEqualsOp with
        static member inline (?<-) (a, LessThenOrEqualsOp, b) = a <=. b
        
        static member (?<-) (predicate:Operation.Algebraic<'a> -> #IPredicate, LessThenOrEqualsOp, value : 'a) =
            Operation.Algebraic.LessThenOrEquals value |> predicate |> Simple
            
        static member (?<-) (predicate:Operation.Set<'a> -> #IPredicate, LessThenOrEqualsOp, value : array<'a>) =
            Operation.Set.ContainedBy value |> predicate |> Simple

    type OverlapOp = private | OverlapOp with
        static member inline (?<-) (a, OverlapOp, b) = a ^. b
        
        static member (?<-) (predicate:Operation.Set<'a> -> #IPredicate, OverlapOp, value : array<'a>) =
            Operation.Set.Overlap value |> predicate |> Simple

    type LikeOp = private | LikeOp with
        static member inline (?<-) (a, LikeOp, b) = a =~. b
        
        static member (?<-) (predicate:Operation.String -> #IPredicate, LikeOp, value : string) =
            Operation.String.Like value |> predicate |> Simple

    type ILikeOp = private | ILikeOp with
        static member inline (?<-) (a, ILikeOp, b) = a =~*. b
        
        static member (?<-) (predicate:Operation.String -> #IPredicate, ILikeOp, value : string) =
            Operation.String.ILike value |> predicate |> Simple
    
    let inline ISNULL a = a ? (IsNullOp) <- IsNullOp
    
    let inline IS (predicate : Filter<#IPredicate>) = predicate
    
    let inline NOT (predicate : Filter<#IPredicate>) = Filter.Not predicate
    
    let inline AND predicates = Complex { Condition = And; Predicates = predicates }
    
    let inline OR predicates = Complex { Condition = Or; Predicates = predicates }

    let inline ANY value = Operation.SetOp.Any value
    
    let inline NEITHER value = Operation.SetOp.Neither value
    
    let inline (&&.) x y =
        match x, y with
        | Simple simple, Complex { Condition = And; Predicates = predicates }
        | Complex { Condition = And; Predicates = predicates }, Simple simple ->
            { Condition = And; Predicates = Simple simple :: predicates } |> Complex
        | Complex { Condition = And; Predicates = p1 }, Complex { Condition = And; Predicates = p2 } ->
            { Condition = And; Predicates = p1 @ p2 } |> Complex
        | _ ->
            { Condition = And
              Predicates = [ x; y ] } |> Complex

    let inline (||.) x y =
        match x, y with
        | Simple simple, Complex { Condition = Or; Predicates = predicates }
        | Complex { Condition = Or; Predicates = predicates }, Simple simple ->
            { Condition = Or; Predicates = Simple simple :: predicates } |> Complex
        | Complex { Condition = Or; Predicates = p1 }, Complex { Condition = Or; Predicates = p2 } ->
            { Condition = Or; Predicates = p1 @ p2 } |> Complex
        | _ ->
            { Condition = Or
              Predicates = [ x; y ] } |> Complex
    
    let inline (=.) a b = a ? (EqOp) <- b
    
    let inline (!=.) a b = a ? (NotEq) <- b
    
    let inline (>.) a b = a ? (GreaterThenOp) <- b
    
    let inline (>=.) a b = a ? (GreaterThenOrEqualsOp) <- b
    
    let inline (<.) a b = a ? (LessThenOp) <- b
    
    let inline (<=.) a b = a ? (LessThenOrEqualsOp) <- b
    
    let inline (^.) a b = a ? (OverlapOp) <- b
    
    let inline (=~.) a b = a ? (LikeOp) <- b
    
    let inline (=~*.) a b = a ? (ILikeOp) <- b