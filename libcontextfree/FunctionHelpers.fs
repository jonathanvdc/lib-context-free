namespace libcontextfree

open System.Collections.Concurrent

module FunctionHelpers =
    /// Memoizes the given function's application to a single argument.
    let memoize (f : 'a -> 'b) : 'a -> 'b =
        let dict = new ConcurrentDictionary<'a, 'b>()
        fun k -> dict.GetOrAdd(k, f)

    /// Memoizes the given function's application to two arguments.
    let memoize2 (f : 'a -> 'b -> 'c) : 'a -> 'b -> 'c =
        memoize (f >> memoize)

    /// Applies the specified function to the given value 
    /// until the resulting value is equal to its previous value.
    let rec fix (f : 'a -> 'a) (init : 'a) =
        let newVal = f init
        if newVal = init then
            newVal
        else
            fix f newVal