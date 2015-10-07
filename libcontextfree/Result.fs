namespace libcontextfree

/// Result<'a> is like 'a option, but the "None" value contains an error message.
type Result<'a> =
| Success of 'a
| Error of string

module Result =
    /// Chain computations that might fail.
    let bind (binder : 'a -> Result<'b>) (result : Result<'a>) : Result<'b> =
        match result with
        | Error e -> Error e
        | Success a -> binder a

    /// Map a function over a Success value.
    let map (f : 'a -> 'b) (result : Result<'a>) : Result<'b> =
        bind (Success << f) result

    /// Print an Error, or the result of applying a function to a Success value.
    /// Success and Error values are printed to stdout and stderr, respectively;
    /// in both cases, a newline is appended.
    let printfnWith (f : 'a -> string) : Result<'a> -> unit =
        function
        | Success a -> printfn "%s" (f a)
        | Error e -> eprintfn "%s" e

    /// Print an Error to stderr, or do nothing for Success ().
    let eprintf : Result<unit> -> unit =
        function
        | Success a -> ()
        | Error e -> eprintfn "%s" e

