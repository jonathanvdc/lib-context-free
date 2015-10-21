namespace libcontextfree

/// Defines a number of map helper functions.
module MapHelpers =
    /// Merge two Map<'k, 'v> objects, using a given function that handles collisions.
    let mergeWith (f : 'v -> 'v -> 'v) (a : Map<'k, 'v>) (b : Map<'k, 'v>) =
        let g acc k vb =
            let w = match Map.tryFind k acc with
                    | Some va -> f va vb
                    | None -> vb
            Map.add k w acc
        Map.fold g a b

    /// Group a sequence of (K, V) tuples into lists of V using the K values as keys.
    let groupFst (kvs : seq<'K * 'V>) : Map<'K, 'V list> =
        let g m (k, v) =
            let vs = defaultArg (Map.tryFind k m) []
            Map.add k (v :: vs) m
        Seq.fold g Map.empty kvs

    /// Group a sequence of (K, V) tuples into sets of V using the K values as keys.
    let groupFstSet (kvs : seq<'K * 'V>) : Map<'K, Set<'V>> =
        let g m (k, v) =
            let vs = defaultArg (Map.tryFind k m) Set.empty
            Map.add k (Set.add v vs) m
        Seq.fold g Map.empty kvs

    /// Creates a map of keys to empty sets from the given set.
    let emptySetMap (keys : Set<'a>) : Map<'a, Set<'b>> =
        Set.fold (fun acc a -> Map.add a Set.empty acc) Map.empty keys