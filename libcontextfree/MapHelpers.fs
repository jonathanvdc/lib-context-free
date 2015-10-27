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

    /// Creates a string representation for the given table.
    let showTable (showA : 'a -> string) 
                  (showB : 'b -> string) 
                  (showC : 'c -> string) 
                  (rows : seq<'a>) 
                  (columns : seq<'b>) 
                  (table : Map<'a * 'b, 'c>)
                  : string =
        let cellWidth = Seq.concat [rows |> Seq.map (showA >> String.length); 
                                    columns |> Seq.map (showB >> String.length);
                                    table |> Seq.map (fun (KeyValue((a, b), c)) -> 
                                                      max (String.length (showA a)) (max (String.length (showB b)) (String.length (showC c))));
                                    ] |> Seq.max
        let actualWidth = cellWidth + 2
        let colCount = Seq.length columns
        let horizSep = List.replicate (colCount + 1) (String.replicate actualWidth "-") |> String.concat "+"

        let showCellContents text =
            let text = " " + text
            text + String.replicate (actualWidth - String.length text) " "

        let getCellValue row column =
            match Map.tryFind (row, column) table with
            | Some x -> showCellContents (showC x)
            | None   -> showCellContents ""

        let cells = rows |> Seq.map (fun r -> Seq.append (seq [showCellContents (showA r)]) (Seq.map (fun c -> getCellValue r c) columns))
        let cells = Seq.append [Seq.append [showCellContents ""] (columns |> Seq.map (fun b -> showCellContents(showB b)))] cells

        cells |> Seq.map (String.concat "|")
              |> String.concat (System.Environment.NewLine + horizSep + System.Environment.NewLine)