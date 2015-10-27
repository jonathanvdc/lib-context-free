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

    /// Creates a function that wraps map access.
    let ofMap (m : Map<'k, 'v>) (k : 'k) : 'v =
        Map.find k m

    /// Creates a function that wraps map access.
    /// If the map does not contain a given key,
    /// None is returned.
    let ofMapOption (m : Map<'k, 'v>) (k : 'k) : 'v option =
        Map.tryFind k m

    /// Creates a function that wraps map access.
    /// If a key is not associated with a value by the 
    /// specified map, the given default value is returned.
    let ofMapWithDefault (defaultV : 'v) (m : Map<'k, 'v>) (k : 'k) : 'v =
        match Map.tryFind k m with
        | Some x -> x
        | None -> defaultV