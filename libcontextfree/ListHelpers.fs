﻿namespace libcontextfree

/// Defines a number of list helpers.
module ListHelpers =

    /// Splits a list at the first element where the given function argument
    /// returns a non-None value. A tuple containing the preceding list of input
    /// items, the resulting value, and the succeeding list of input items, is returned
    /// if this operation could be performed. Otherwise, None is returned.
    let rec splitAtFirst (pred : 'a -> 'b option) (items : 'a list) : ('a list * 'b * 'a list) option =
        match items with
        | []      -> 
            // Empty list contains no elements that can satisfy `pred`.
            // Return None.
            None
        | x :: xs ->
            // Checks if the first element in the list
            // is the item we're looking for.
            match pred x with
            | Some result -> 
                // `x` was the item we wanted.
                // Since `x` is the first element in the input
                // list, we'll return an empty list, the result, and 
                // the remaining items.
                Some([], result, xs)
            | None        ->
                // `x` was not the element we were looking for.
                // Now, we'll check if the remainder of 
                // the list contains any items that satisfy our
                // query. If so, we'll prepend `x` to the
                // list of preceding items. Otherwise,
                // just return None.
                match splitAtFirst pred xs with 
                | None                        -> None 
                | Some(first, result, second) -> Some (x :: first, result, second)

    /// Splits a list at the last element where the given function argument
    /// returns a non-None value. A tuple containing the preceding list of input
    /// items, the resulting value, and the succeeding list of input items, is returned
    /// if this operation could be performed. Otherwise, None is returned.
    let rec splitAtLast (pred : 'a -> 'b option) (items : 'a list) : ('a list * 'b * 'a list) option =
        match items with
        | []      -> 
            // Empty list contains no elements that can satisfy `pred`.
            // Return None.
            None
        | x :: xs ->
            // Let's check if the list's tail yields any interesting results.
            match splitAtLast pred xs with 
            | Some(first, result, second) -> 
                // If the tail contains the item we're looking for, we'll
                // return that, and prepend `x` to the preceding item list.
                Some (x :: first, result, second)
            | None                        -> 
                // Getting here means that the list's tail was unimpressive.
                // Check if the first element in the list
                // is the item we're looking for.
                match pred x with
                | Some result -> 
                    // `x` was the item we wanted all along.
                    // Since `x` is the first element in the input
                    // list, we'll return an empty list, the result, and 
                    // the remaining items.
                    Some([], result, xs)
                | None        ->
                    // Neither `x` nor `xs` gave us anything useful to work with.
                    None


