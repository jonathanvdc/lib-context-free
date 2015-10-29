namespace libcontextfree

/// Defines a number of set helper functions.
module SetHelpers =
    let choose (f : 'T -> 'U option) : Set<'T> -> Set<'U> =
        Set.map f >> Set.remove None >> Set.map Option.get

    /// Like closure, but provides the preliminary set of results to the
    /// induction function.
    let closure2 (induction : Set<'a> -> 'a -> Set<'a>) (basis : Set<'a>) : Set<'a> =
        let rec addToClosure (basis : Set<'a>) (item : 'a) : Set<'a> =
            let newSet = induction basis item
            let setDiff = Set.difference newSet basis
            let setUnion = Set.union newSet basis
            Set.fold addToClosure setUnion setDiff

        Set.fold addToClosure basis basis

    /// Computes the closure of a basis set by recursively applying 
    /// the given induction function to every element in the closure set,
    /// starting with a given basis set.
    ///
    /// For example:
    ///
    ///     closure (fun _ -> Set.empty) (set [1; 2; 3])
    ///     = set [1; 2; 3]
    ///
    ///     closure (fun x -> if x > 5 then Set.empty else Set.singleton (x + 1)) (set [1])
    ///     = set [1; 2; 3; 4; 5; 6]
    let closure (induction : 'a -> Set<'a>) (basis : Set<'a>) : Set<'a> =
        closure2 (fun _ -> induction) basis

    /// Converts the given sequence to a map where every
    /// item in the set has been assigned a unique index.
    /// (The assigned indices are 0..n-1, where n equals the
    /// number of items in the sequence.)
    let toIndexedMap (items : seq<'a>) : Map<'a, int> =
        items |> Seq.mapi (fun i x -> x, i)
              |> Map.ofSeq