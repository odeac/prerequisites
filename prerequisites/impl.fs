module impl

/// Returns the number a * 2
let twice (a : int) : int = a * 2

/// Adds the numbers a and b
let addNumbers (a : int) (b : int) : int = a + b

/// Returns a list containing n times the number x 
let replicate (x : int) (n : int) : int list = List.init n (fun _ -> x)

/// Returns a list containing the first n Fibonacci numbers
/// https://en.wikipedia.org/wiki/Fibonacci_number
let fibonacci (n : int) : int list = 
    let rec impl current next =
        seq {
            yield current
            yield! impl next (current + next)
        }
    impl 0 1
    |> Seq.take n
    |> List.ofSeq
