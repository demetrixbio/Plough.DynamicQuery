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
    type ArrayOp<'a> =
        | Any of array<'a>
        | Neither of array<'a>
    
    [<RequireQualifiedAccess>]
    type Set<'a> =
        | Common of CommonOp<'a>
        /// Does the first set contain the second, that is, does each element appearing in the second set equal some element of the first set?
        | Contains of array<'a>
        /// Is the first set contained by the second?
        | ContainedBy of array<'a>
        /// Do the sets overlap, that is, have any elements in common?
        | Overlap of array<'a>
    
    [<RequireQualifiedAccess>]
    type Algebraic<'a> =
        | Common of CommonOp<'a>
        | Array of ArrayOp<'a>
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
        | Array of ArrayOp<System.String>
        | Like of System.String
        | ILike of System.String
    
    [<RequireQualifiedAccess>]
    type Bool =
        | Common of CommonOp<System.Boolean>
     
    [<RequireQualifiedAccess>]
    type Guid =
        | Common of CommonOp<System.Guid>
        | Array of ArrayOp<System.Guid>
    
module Operators =
    type IsNullOp = | IsNullOp with
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

    type EqOp = | EqOp with
        static member inline (?<-) (a, EqOp, b) = a =. b
        
        static member (?<-) (predicate:Operation.String -> #IPredicate, EqOp, value : string) =
            Operation.CommonOp.Eq value |> Operation.String.Common |> predicate |> Simple
           
        static member (?<-) (predicate:Operation.String -> #IPredicate, EqOp, value : Operation.ArrayOp<string>) =
            Operation.String.Array value |> predicate |> Simple
            
        static member (?<-) (predicate:Operation.Bool -> #IPredicate, EqOp, value : bool) =
            Operation.CommonOp.Eq value |> Operation.Bool.Common |> predicate |> Simple
        
        static member (?<-) (predicate:Operation.Guid -> #IPredicate, EqOp, value : System.Guid) =
            Operation.CommonOp.Eq value |> Operation.Guid.Common |> predicate |> Simple
            
        static member (?<-) (predicate:Operation.Guid -> #IPredicate, EqOp, value : Operation.ArrayOp<System.Guid>) =
            Operation.Guid.Array value |> predicate |> Simple
        
        static member (?<-) (predicate:Operation.Algebraic<'a> -> #IPredicate, EqOp, value : 'a) =
            Operation.CommonOp.Eq value |> Operation.Algebraic.Common |> predicate |> Simple
          
        static member (?<-) (predicate:Operation.Algebraic<'a> -> #IPredicate, EqOp, value : Operation.ArrayOp<'a>) =
            Operation.Algebraic.Array value |> predicate |> Simple
            
        static member (?<-) (predicate:Operation.Set<'a> -> #IPredicate, EqOp, value : 'a) =
            Operation.CommonOp.Eq value |> Operation.Set.Common |> predicate |> Simple
                
    type NotEq = | NotEq with 
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
                
    type GreaterThenOp = | GreaterThenOp with
        static member inline (?<-) (a, GreaterThenOp, b) = a >. b
        
        static member (?<-) (predicate:Operation.Algebraic<'a> -> #IPredicate, GreaterThenOp, value : 'a) =
            Operation.Algebraic.GreaterThen value |> predicate |> Simple
                

    type GreaterThenOrEqualsOp = | GreaterThenOrEqualsOp with
        static member inline (?<-) (a, GreaterThenOrEqualsOp, b) = a >=. b
        
        static member (?<-) (predicate:Operation.Algebraic<'a> -> #IPredicate, GreaterThenOrEqualsOp, value : 'a) =
            Operation.Algebraic.GreaterThenOrEquals value |> predicate |> Simple
            
        static member (?<-) (predicate:Operation.Set<'a> -> #IPredicate, GreaterThenOrEqualsOp, value : array<'a>) =
            Operation.Set.Contains value |> predicate |> Simple

    type LessThenOp = | LessThenOp with   
        static member inline (?<-) (a, LessThenOp, b) = a <. b
        
        static member (?<-) (predicate:Operation.Algebraic<'a> -> #IPredicate, LessThenOp, value : 'a) =
            Operation.Algebraic.LessThen value |> predicate |> Simple

    type LessThenOrEqualsOp = | LessThenOrEqualsOp with
        static member inline (?<-) (a, LessThenOrEqualsOp, b) = a <=. b
        
        static member (?<-) (predicate:Operation.Algebraic<'a> -> #IPredicate, LessThenOrEqualsOp, value : 'a) =
            Operation.Algebraic.LessThenOrEquals value |> predicate |> Simple
            
        static member (?<-) (predicate:Operation.Set<'a> -> #IPredicate, LessThenOrEqualsOp, value : array<'a>) =
            Operation.Set.ContainedBy value |> predicate |> Simple

    type OverlapOp = | OverlapOp with
        static member inline (?<-) (a, OverlapOp, b) = a ^. b
        
        static member (?<-) (predicate:Operation.Set<'a> -> #IPredicate, OverlapOp, value : array<'a>) =
            Operation.Set.Overlap value |> predicate |> Simple

    type LikeOp = | LikeOp with
        static member inline (?<-) (a, LikeOp, b) = a =~. b
        
        static member (?<-) (predicate:Operation.String -> #IPredicate, LikeOp, value : string) =
            Operation.String.Like value |> predicate |> Simple

    type ILikeOp = | ILikeOp with
        static member inline (?<-) (a, ILikeOp, b) = a =~*. b
        
        static member (?<-) (predicate:Operation.String -> #IPredicate, ILikeOp, value : string) =
            Operation.String.ILike value |> predicate |> Simple


[<AutoOpen>] 
module DSL =
    open Operators
    
    /// IS NULL.
    let inline ISNULL a = a ? (IsNullOp) <- IsNullOp
    
    /// IS A (identity function).
    let inline IS (predicate : Filter<#IPredicate>) = predicate
    
    /// NOT A.
    let inline NOT (predicate : Filter<#IPredicate>) = Filter.Not predicate
    
    /// A AND B.
    let inline AND predicates = Complex { Condition = And; Predicates = predicates }
    
    /// A OR B.
    let inline OR predicates = Complex { Condition = Or; Predicates = predicates }

    /// Array contains any value.
    let inline ANY value = Operation.ArrayOp.Any value
    
    /// Array doesn't contain any of values.
    let inline NEITHER value = Operation.ArrayOp.Neither value
    
    /// A AND B
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

    /// A OR B.
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
    
    /// Common equals.
    let inline (=.) a b = a ? (EqOp) <- b
    
    /// Common not equals.
    let inline (!=.) a b = a ? (NotEq) <- b
    
    /// Algebraic greater than.
    let inline (>.) a b = a ? (GreaterThenOp) <- b
    
    /// 1) Algebraic greater then or equals. 2) Set contains.
    let inline (>=.) a b = a ? (GreaterThenOrEqualsOp) <- b
    
    /// Algebraic less than.
    let inline (<.) a b = a ? (LessThenOp) <- b
    
    /// 1) Algebraic less than or equals. 2) Set contained by.
    let inline (<=.) a b = a ? (LessThenOrEqualsOp) <- b
    
    /// Arrays overlap.
    let inline (^.) a b = a ? (OverlapOp) <- b
    
    /// String LIKE.
    let inline (=~.) a b = a ? (LikeOp) <- b
    
    /// String ILIKE.
    let inline (=~*.) a b = a ? (ILikeOp) <- b