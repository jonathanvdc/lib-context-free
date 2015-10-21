namespace libcontextfree

/// Defines a number of set helper functions.
module SetHelpers =
    let choose (f : 'T -> 'U option) : Set<'T> -> Set<'U> =
        Set.map f >> Set.remove None >> Set.map Option.get

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
        let rec addToClosure (basis : Set<'a>) (item : 'a) : Set<'a> =
            let newSet = induction item
            let setDiff = Set.difference newSet basis
            let setUnion = Set.union newSet basis
            Set.fold addToClosure setUnion setDiff

        Set.fold addToClosure basis basis

    /// Converts the given sequence to a map where every
    /// item in the set has been assigned a unique index.
    let toIndexedMap (items : seq<'a>) : Map<'a, int> =
        items |> Seq.mapi (fun i x -> x, i)
              |> Map.ofSeq