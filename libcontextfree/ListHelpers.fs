namespace libcontextfree

/// Defines a number of list helper functions.
module ListHelpers =
    /// A generic function that `splitAtFirst` and `splitAtLast` are both
    /// based on, which allows you to pass any "order" function that specifies
    /// which order to search the enumerated list in.
    let splitAtFirstOrder (order : (int * 'a) list -> (int * 'a) list)
                          (pred : 'a -> 'b option)
                          (items : 'a list)
                          : ('a list * 'b * 'a list) option =
        let enumerated : (int * 'a) list =
            List.mapi (fun i a -> (i, a)) items
        
        // This function's result is examined for each element in `enumerated`
        // (by `List.tryPick`); as soon as it returns a `Some` result, we are
        // finished. We do so when `pred a` matches `Some b`, and we adjoin
        // the lists of `items` left and right of `a` to the result.
        let picker (i, a) =
            match pred a with
            | None -> None
            | Some b ->
                let ls = Seq.toList (Seq.take i items)
                let rs = Seq.toList (Seq.skip (i + 1) items)
                Some (ls, b, rs)
        
        // Pass the elements of `enumerated` through `picker` in the order
        // defined by `order`.
        List.tryPick picker (order enumerated)

    /// Splits a list at the first element where the given function argument
    /// returns a `Some` value. A tuple containing the preceding list of input
    /// items, the resulting value, and the succeeding list of input items, is returned
    /// if this operation could be performed. Otherwise, None is returned.
    ///
    /// For example:
    ///
    ///     let safeSqrt x = if x >= 0.0 then Some (sqrt x, -sqrt x) else None
    ///     
    ///     splitAtFirst safeSqrt [-7.1; 9.3; 4.7; -1.2; 2.8; -5.1]
    ///     = Some ([-7.1], (3.049590136, -3.049590136), [4.7; -1.2; 2.8; -5.1])
    ///     
    ///     splitAtFirst safeSqrt [-1.1; -2.2; -3.3]
    ///     = None
    ///
    let splitAtFirst (pred : 'a -> 'b option)
                     (items : 'a list)
                     : ('a list * 'b * 'a list) option =
        splitAtFirstOrder id pred items
    
    /// Splits a list at the last element where the given function argument
    /// returns a `Some` value. A tuple containing the preceding list of input
    /// items, the resulting value, and the succeeding list of input items, is returned
    /// if this operation could be performed. Otherwise, None is returned.
    ///
    /// For example:
    ///
    ///     let safeSqrt x = if x >= 0.0 then Some (sqrt x, -sqrt x) else None
    ///     
    ///     splitAtLast safeSqrt [-7.1; 9.3; 4.7; -1.2; 2.8; -5.1]
    ///     = Some ([-7.1; 9.3; 4.7; -1.2], (1.673320053, -1.673320053), [-5.1])
    ///     
    ///     splitAtLast safeSqrt [-1.1; -2.2; -3.3]
    ///     = None
    ///
    let splitAtLast (pred : 'a -> 'b option)
                    (items : 'a list)
                    : ('a list * 'b * 'a list) option =
        splitAtFirstOrder List.rev pred items

    /// Splits the given list into two lists at the given index.
    ///
    /// For example:
    ///
    ///     splitAtIndex 2 [1; 2; 3; 4; 5]
    ///     = [1; 2], [3; 4; 5]
    let rec splitAtIndex (index : int) (items : 'a list) : 'a list * 'a list =
        match index, items with
        | (0, _)       -> [], items
        | (_, [])      -> [], []
        | (_, x :: xs) ->
            let first, second = splitAtIndex (index - 1) xs
            x :: first, second
                

    /// The Cartesian nth power of a list, i.e. l × l × ... × l. (Be careful:
    /// this computes a list containing |l|^n elements, which can be large.)
    let rec cartesianPower (n : int) (l : 'a list) =
        match n with
        | 0 -> [[]]
        | n -> [ for x in l do
                     for xs in cartesianPower (n - 1) l do
                         yield x :: xs ]

    /// Combines the three lists into a list of triples. The result is
    /// truncated whenever any of the lists is empty.
    let rec zip3Truncate (xxs : 'x list) (yys : 'y list) (zzs : 'z list) =
        match xxs, yys, zzs with
        | (x :: xs), (y :: ys), (z :: zs) ->
            (x, y, z) :: zip3Truncate xs ys zs
        | _ -> []

    /// Computes all suffixes of the given list.
    let rec suffixes : 'x list -> ('x * 'x list) list = function
    | x :: xs -> (x, xs) :: suffixes xs
    | [] -> []