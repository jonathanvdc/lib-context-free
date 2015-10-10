namespace libcontextfree

/// Result<'a> is like 'a option, but the "None" value contains an error message.
type Result<'a> =
| Success of 'a
| Error of string

module Result =
    /// Finds out if the given result value indicates success.
    let isSuccess (x : Result<'a>) = 
        match x with
        | Success _ -> true
        | _         -> false

    /// Determines if the given result was unsuccessful.
    let isError (x : Result<'a>) = 
        not (isSuccess x)

    /// Chain computations that might fail.
    let bind (binder : 'a -> Result<'b>) (result : Result<'a>) : Result<'b> =
        match result with
        | Error e -> Error e
        | Success a -> binder a

    /// Map a function over a Success value.
    let map (f : 'a -> 'b) (result : Result<'a>) : Result<'b> =
        bind (Success << f) result

    /// Return a list of all given results, if they are all Successes; otherwise,
    /// return the first error in the list.
    let rec sequence (rs : #seq<Result<'a>>) : Result<'a list> =
        let folder (state : Result<'a list>) (item : Result<'a>) =
            match state with
            | Error _    -> state
            | Success xs -> map (fun x -> x :: xs) item

        // This behaves just like Seq.fold, except that we're 
        // early-outing if something goes wrong.
        let folded = rs |> Seq.scan folder (Success [])
                        |> Seq.cache
        match Seq.tryFind isError folded with
        | Some error -> error
        | None ->
            folded |> Seq.last
                   |> map List.rev
            

    /// Print an Error, or the result of applying a function to a Success value.
    /// Success and Error values are printed to stdout and stderr, respectively;
    /// in both cases, a newline is appended.
    let printWith (f : 'a -> string) : Result<'a> -> unit =
        function
        | Success a -> printfn "%s" (f a)
        | Error e -> eprintfn "%s" e

    /// Print an Error to stderr, or do nothing for Success ().
    let eprintf : Result<unit> -> unit =
        function
        | Success a -> ()
        | Error e -> eprintfn "%s" e
