namespace libcontextfree

/// Defines a number of map helper functions.
module MapHelpers =
    /// Merge two Map<'k, 'v> objects, using a given function that handles collisions.
    let mergeWith (f : 'v -> 'v -> 'v) (a : Map<'k, 'v>) (b : Map<'k, 'v>) =
        let g acc k v =
            let w = match Map.tryFind k acc with
                    | Some v' -> f v v'
                    | None -> v
            Map.add k w acc
        Map.fold g a b