namespace libcontextfree

module AsciiBox =
    /// Split a string over newlines and arrange the lines in a rectangular array of characters.
    let makeBox (s : string) : char[,] =
        let lines = StringHelpers.splitLines s
        let h = Array.length lines
        let w = Array.map String.length lines |> Array.max

        Array2D.init h w (fun y x ->
            let line = lines.[y]
            if x < String.length line then line.[x] else ' '
        )

    /// Turn a rectangular array of strings like
    ///
    ///     array2D [| [| "abc"; "d\ne" |]; [| "f\ngg\nh"; "ij" |] |]
    ///
    /// into a fitted ASCII art box representation like:
    ///
    ///     ┌───┬──┐
    ///     │abc│d │
    ///     │   │e │
    ///     ├───┼──┤
    ///     │f  │ij│
    ///     │gg │  │
    ///     │h  │  │
    ///     └───┴──┘
    ///
    let display (cells : string[,]) : string =
        let numRows = Array2D.length1 cells
        let numColumns = Array2D.length2 cells
        let boxes = Array2D.map makeBox cells

        let columnWidths = Array.init numColumns (fun x ->
            Seq.max <| seq {
                for y in 0..numRows-1 ->
                    Array2D.length2 boxes.[y, x]
            }
        )

        let rowHeights = Array.init numRows (fun y ->
            Seq.max <| seq {
                for x in 0..numColumns-1 ->
                    Array2D.length1 boxes.[y, x]
            }
        )

        // We wish to create a char[,] like this:
        //
        //     ┌─────┬──┬───┬───────┐
        //     │abcde│f │j  │o      │  ┐
        //     │     │gh│klm│       │  │  rowHeights.[0]
        //     │     │i │n  │       │  ┘
        //     ├─────┼──┼───┼───────┤
        //     │o    │q │r  │s t u v│  ┐
        //     │p    │  │   │       │  ┘  rowHeights.[1]
        //     └─────┴──┴───┴───────┘
        //
        //      └─┬─┘ └┘ └┬┘ └──┬──┘
        //        │    │  │     │     
        //        ╘════╪══╪═════╪═════ columnWidths.[0]
        //             ╘══╪═════╪═════ columnWidths.[1]
        //                ╘═════╪═════ columnWidths.[2]
        //                      ╘═════ columnWidths.[3]
        //
        // The total height is sum(rowHeights) + numRows + 1.
        // The total width is sum(columnWidths) + numColumns + 1.
        
        let totalHeight = Array.sum rowHeights + numRows + 1
        let totalWidth = Array.sum columnWidths + numColumns + 1
        
        // Calculate where the boxes begin.
        let offsets = Array.scan (fun x w -> x + w + 1) 1
        let rowOffsets = offsets rowHeights
        let columnOffsets = offsets columnWidths

        let result : char[,] =
            Array2D.create totalHeight totalWidth ' '

        for i in 0..numRows do
            for j in 0..numColumns do
                let sy = rowOffsets.[i]
                let sx = columnOffsets.[j]

                if i < numRows && j < numColumns then
                    let b = boxes.[i, j]
                    let h = Array2D.length1 b
                    let w = Array2D.length2 b
                    Array2D.blit b 0 0 result sy sx h w

                let w = if j < numColumns then columnWidths.[j] else 0
                let h = if i < numRows then rowHeights.[i] else 0

                for x in 0..w-1 do result.[sy - 1, sx + x] <- '─'
                for y in 0..h-1 do result.[sy + y, sx - 1] <- '│'

                let cy = if i = 0 then 0 else if i = numRows    then 2 else 1
                let cx = if j = 0 then 0 else if j = numColumns then 2 else 1

                result.[sy - 1, sx - 1] <-
                    match (cy, cx) with
                    | (0, 0) -> '┌' | (0, 1) -> '┬' | (0, 2) -> '┐'
                    | (1, 0) -> '├' | (1, 1) -> '┼' | (1, 2) -> '┤'
                    | (2, 0) -> '└' | (2, 1) -> '┴' | (_, _) -> '┘'
                
        StringHelpers.concatLines <| seq {
            for y in 0..totalHeight-1 ->
                new System.String [| for x in 0..totalWidth-1 -> result.[y, x] |]
        }