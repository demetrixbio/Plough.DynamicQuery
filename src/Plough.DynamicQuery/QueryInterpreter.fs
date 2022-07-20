namespace Plough.DynamicQuery

open System
open System.Collections.Generic
open Plough.DynamicQuery

type SqlParameter =
    { Reference : string
      Value : obj }

type SqlOperation =
    { Predicate : string
      Parameter : SqlParameter option }

type QueryInterpreterState =
    { Template : string
      InnerJoins : Dictionary<string, string list>
      LeftJoins : Dictionary<string, string list>
      Parameters : Dictionary<string, obj> }

[<RequireQualifiedAccess>]
module QueryInterpreter =
    let mapOperation<'a, 'b> (serialize : 'a -> 'b) (value : 'a option) (op : string -> string) =
        let parameter =
            value
            |> Option.map (fun v ->
                let ref = Guid.NewGuid().ToString("N")
                let value = serialize v
                { Reference = ref; Value = value })

        let predicate =
            parameter
            |> Option.map (fun p -> op p.Reference)
            |> Option.defaultWith (fun () -> op String.Empty)
        
        { Predicate = predicate; Parameter = parameter }

    let rec translate state mapSimple = function
        | Simple predicate -> mapSimple state predicate
        | Not predicate -> translate state mapSimple predicate |> sprintf "NOT (%s)"
        | Complex { Predicates = [] } | Empty -> "1=1"
        | Complex { Predicates = [ predicate ] } -> translate state mapSimple predicate
        | Complex expr ->
            let condition = match expr.Condition with | And -> " AND " | Or -> " OR "
            expr.Predicates
            |> List.map (fun predicate ->
                        match predicate with
                        | Simple _ -> translate state mapSimple predicate
                        | Not _ -> translate state mapSimple predicate |> sprintf "NOT (%s)"
                        | Complex _ | Empty -> translate state mapSimple predicate |> sprintf "(%s)")
            |> String.concat condition
         
    let interpret (query : Filter<#IPredicate>) (state : QueryInterpreterState) (mapSimple : QueryInterpreterState -> #IPredicate -> string) =
        let predicates = translate state mapSimple query |> sprintf "(%s)"
        let mutable acc = SqlBuilder.init state.Template state.Parameters |> SqlBuilder.where predicates None
        for join in state.InnerJoins |> Seq.collect (fun s -> s.Value) do
            acc <- acc |> SqlBuilder.innerJoin join None
        for join in state.LeftJoins |> Seq.collect (fun s -> s.Value) do
            acc <- acc |> SqlBuilder.leftJoin join None
        acc

type PostgresqlOperation =
    static member map<'a, 'b> (op : Operation.CommonOp<'a>) = fun (serialize : 'a -> 'b) ->
        match op with
        | Operation.CommonOp.IsNull ->
            (None, fun _ -> "IS NULL")
            ||> QueryInterpreter.mapOperation serialize 
        | Operation.CommonOp.Eq value ->
            (Some value, sprintf "= @%s")
            ||> QueryInterpreter.mapOperation serialize
        | Operation.CommonOp.NotEq value ->
            (Some value, sprintf "<> @%s")
            ||> QueryInterpreter.mapOperation serialize

    static member map<'a, 'b> (op : Operation.ArrayOp<'a>) = fun (serialize : 'a -> 'b) ->
        match op with
        | Operation.ArrayOp.Any values ->
            (Some values, sprintf "= ANY(@%s)")
            ||> QueryInterpreter.mapOperation<'a[], 'b[]> (Array.map serialize)
        | Operation.ArrayOp.Neither values ->
            (Some values, sprintf "<> ALL(@%s)")
            ||> QueryInterpreter.mapOperation<'a[], 'b[]> (Array.map serialize)

    static member map<'a, 'b> (op : Operation.Algebraic<'a>) = fun (serialize : 'a -> 'b) ->
        match op with
        | Operation.Algebraic.Common common ->
            PostgresqlOperation.map<'a, 'b> common serialize
        | Operation.Algebraic.Array set ->
            PostgresqlOperation.map<'a, 'b> set serialize 
        | Operation.Algebraic.GreaterThen value ->
            (Some value, sprintf "> @%s")
            ||> QueryInterpreter.mapOperation serialize
        | Operation.Algebraic.GreaterThenOrEquals value ->
            (Some value, sprintf ">= @%s")
            ||> QueryInterpreter.mapOperation serialize
        | Operation.Algebraic.LessThen value ->
            (Some value, sprintf "< @%s")
            ||> QueryInterpreter.mapOperation serialize
        | Operation.Algebraic.LessThenOrEquals value ->
            (Some value, sprintf "<= @%s")
            ||> QueryInterpreter.mapOperation serialize

    static member map (op : Operation.String) =
        match op with
        | Operation.String.Common common ->
            PostgresqlOperation.map common id
        | Operation.String.Array set ->
            PostgresqlOperation.map set id 
        | Operation.String.Like value ->
            (Some value, sprintf "LIKE @%s")
            ||> QueryInterpreter.mapOperation id
        | Operation.String.ILike value ->
            (Some value, sprintf "ILIKE @%s")
            ||> QueryInterpreter.mapOperation id
    
    static member map (op : Operation.Bool) =
        match op with
        | Operation.Bool.Common common ->
            PostgresqlOperation.map common id
    
    static member map (op : Operation.Guid) =
        match op with
        | Operation.Guid.Common common ->
            PostgresqlOperation.map common id
        | Operation.Guid.Array set ->
            PostgresqlOperation.map set id
            
    static member map<'a, 'b> (op : Operation.Set<'a>) = fun (serialize : 'a -> 'b) ->
        match op with
        | Operation.Set.Common common ->
            PostgresqlOperation.map<'a, 'b> common serialize
        | Operation.Set.Contains values ->
            (Some values, sprintf "@> @%s")
            ||> QueryInterpreter.mapOperation<'a[], 'b[]> (Array.map serialize)
        | Operation.Set.ContainedBy values ->
            (Some values, sprintf "<@ @%s")
            ||> QueryInterpreter.mapOperation<'a[], 'b[]> (Array.map serialize)
        | Operation.Set.Overlap values ->
            (Some values, sprintf "&& @%s")
            ||> QueryInterpreter.mapOperation<'a[], 'b[]> (Array.map serialize)