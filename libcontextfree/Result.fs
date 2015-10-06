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