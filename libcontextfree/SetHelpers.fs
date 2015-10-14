namespace libcontextfree

/// Defines a number of set helper functions.
module SetHelpers =
    let choose (f : 'T -> 'U option) : Set<'T> -> Set<'U> =
        Set.map f >> Set.remove None >> Set.map Option.get