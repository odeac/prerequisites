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

    [<Test>]
    let ``twice n equals n + n``() =
        fun (n) ->
            twice n = n + n
        |> Check.VerboseThrowOnFailure

module test_addNumbers =
    [<Test>]
    let ``addNumbers 1 0 = 1``() =
        addNumbers 1 0
        |> should equal 1

    [<Test>]
    let ``addNumbers 1 -1 = 0``() =
        addNumbers 1 -1
        |> should equal 0
        
    [<Test>]
    let ``Zero is neutral element for addNumbers``() =
        fun (a) ->
            addNumbers 0 a
            |> should equal a
        |> Check.VerboseThrowOnFailure
        
    [<Test>]
    let ``addNumbers is commutative``() =
        fun (a,b) ->
            addNumbers a b
            |> should equal (addNumbers b a)
        |> Check.VerboseThrowOnFailure

    [<Test>]
    let ``addNumbers is associative``() =
        fun (a,b,c) ->
            addNumbers a (addNumbers b c)
            |> should equal (addNumbers (addNumbers a b) c)
        |> Check.VerboseThrowOnFailure
    
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

    [<Test>]
    let ``Sum of the elements equals product of the parameters``() =
        fun (a, n) ->
            n > 0 ==> lazy ( replicate a n
                             |> Seq.sum
                             |> should equal (a * n) )
        |> Check.VerboseThrowOnFailure
        
    [<Test>]
    let ``Replicate 0 times gives an empty list``() =
        fun (a) ->
            replicate a 0
            |> should equal []
        |> Check.VerboseThrowOnFailure

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
            n >= 0 ==> lazy ( let [a;b;c] =
                                    fibonacci (n + 3)
                                    |> Seq.skip n
                                    |> Seq.take 3
                                    |> List.ofSeq
                              a + b 
                              |> should equal c )
        |> Check.VerboseThrowOnFailure
        

