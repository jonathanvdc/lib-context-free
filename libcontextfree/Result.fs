namespace libcontextfree

/// Result<'a, 'b> is like 'a option, but the "None" 
/// value contains an error message.
type Result<'success, 'error> =
| Success of 'success
| Error of 'error

/// Result<'a> is like 'a option, but the "None" 
/// value contains an error message.
type Result<'a> = Result<'a, string>

module Result =
    /// Finds out if the given result value indicates success.
    let isSuccess (x : Result<'a, 'b>) = 
        match x with
        | Success _ -> true
        | _         -> false

    /// Determines if the given result was unsuccessful.
    let isError (x : Result<'a, 'b>) = 
        not (isSuccess x)

    /// Retrieve the value inside a Success. Throw an exception if the argument is an Error.
    let get (x : Result<'a>) : 'a =
        match x with
        | Error e -> raise (new System.InvalidOperationException(e))
        | Success a -> a

    /// Chain computations that might fail.
    let bind (binder : 'a -> Result<'b, 'c>) (result : Result<'a, 'c>) : Result<'b, 'c> =
        match result with
        | Error e -> Error e
        | Success a -> binder a

    /// Map a function over a Success value.
    let map (f : 'a -> 'b) (result : Result<'a, 'c>) : Result<'b, 'c> =
        bind (Success << f) result

    /// Return a list of all given results, if they are all Successes; otherwise,
    /// return the first error in the list.
    let rec sequence (rs : #seq<Result<'a, 'b>>) : Result<'a list, 'b> =
        let folder (state : Result<'a list, 'b>) (item : Result<'a, 'b>) =
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

    /// Print an Error, or the Success value.
    /// Success and Error values are printed to stdout and stderr, respectively;
    /// in both cases, a newline is appended.
    let print : Result<string> -> unit = 
        printWith id

    /// Print an Error to stderr, or do nothing for Success ().
    let eprintf : Result<unit> -> unit =
        function
        | Success a -> ()
        | Error e -> eprintfn "%s" e
