module tests

open impl

open NUnit.Framework
open FsUnit
open FsCheck

module test_twice =
    [<Test>]
    let ``twice 0 = 0``() =
        twice 0
        |> should equal 0

    [<Test>]
    let ``twice 1 = 2``() =
        twice 1
        |> should equal 2

module test_addNumbers =
    [<Test>]
    let ``addNumbers 1 0 = 1``() =
        addNumbers 1 0
        |> should equal 1

    [<Test>]
    let ``addNumbers 1 -1 = 0``() =
        addNumbers 1 -1
        |> should equal 0
    
module test_replicate =    
    [<Test>]
    let ``replicate 1 0 returns empty list``() =
        replicate 1 0
        |> should equal []

    [<Test>]
    let ``replicate 1 1 returns [1]``() =
        replicate 1 1
        |> should equal [1]
    
    [<Test>]
    let ``replicate 0 2 returns [0;0]``() =
        replicate 0 2
        |> should equal [0;0]

module test_fibonacci =
    [<Test>]
    let ``fibonacci 0 returns empty list``() =
        fibonacci 0
        |> should equal []

    [<Test>]
    let ``fibonacci 1 returns [0]``() =
        fibonacci 1
        |> should equal [0]

    [<Test>]
    let ``fibonacci 5 returns [0;1;1;2;3]``() =
        fibonacci 5
        |> should equal [0;1;1;2;3]

    [<Test>]
    let ``Given any 3 consecutive fibonacci numbers expect the third is the sum of the previous ones`` () =
        fun(n) ->
            let [a;b;c] =
                fibonacci (n + 3)
                |> Seq.skip n
                |> Seq.take 3
                |> List.ofSeq
            a + b = c
        |> Check.VerboseThrowOnFailure
        

