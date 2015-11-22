namespace libcontextfree
open ChomskyNormalForm

module CYKParser =
    /// Determine whether this grammar can generate the given string using
    /// the Cocke-Younger-Kasami (CYK) algorithm.
    let cykParse (grammar : ChomskyNormalCfg<'nt, 't>) (S' : seq<'t>) : (bool * Map<'nt option, int> * bool[,,] )=
        // The input string as an array.
        let S = Array.ofSeq S'
        let n = Array.length S
        
        // All nonterminals as an array, and a mapping from nonterminals to
        // indices into this array.
        let R = Array.ofSeq (cnfNonterminals grammar)
        let r = Array.length R
        let Rmap = SetHelpers.toIndexedMap R
        
        // The unit and binary productions from the given grammar.
        let unitProductions = cnfUnitProductions grammar
        let binaryProductions = cnfBinaryProductions grammar

        // An (n x n x r) boolean array; initially filled with `false`.
        let P = Array3D.zeroCreate n n r
        
        // Base case: find the unit productions for each terminal in the input.
        for i in 0..n-1 do
          for (Rj, a) in unitProductions do
            if S.[i] = a then
              let j = Rmap.[Rj]
              P.[0,i,j] <- true
        
        // Dynamically match bigger and bigger substrings of S.
        for i in 2..n do           // i = substring length
          for j in 1..n-i+1 do     // j = starting position
            for k in 1..i-1 do     // k = split position
              for (RA, (RB, RC)) in binaryProductions do
                // We've split up a substring starting at j-1, length i, into two parts
                // (split at k). If the parts can respectively be deduced from RB and
                // RC, then the whole can be deduced from RA through the production
                // RA -> RB RC.
                if P.[k-1, j-1, Rmap.[Some RB]] && P.[i-k-1, j+k-1, Rmap.[Some RC]] then
                  // printf "%A -> %A and "  (RB) (System.String.Concat (S.[j-1..j+k-2]))
                  // printf "%A -> %A  =>  " (RC) (System.String.Concat (S.[j-1+k..j+i-2]))
                  // printf "%A -> %A.\n"    (RA) (System.String.Concat (S.[j-1..j+i-2]))
                  P.[i-1, j-1, Rmap.[RA]] <- true
        
        // Can we deduce the whole string from the starting symbol (None)?
        P.[n-1, 0, Rmap.[None]], Rmap, P