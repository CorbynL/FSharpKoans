﻿namespace FSharpKoans
open NUnit.Framework

(*
Reversing a list.
*)

module ``14: Reversing a list`` =
    open System.Runtime.Remoting.Metadata.W3cXsd2001

    [<Test>]
    let ``01 Reversing a list, the hard way`` () =
        let rev (xs : 'a list) : 'a list =
            let rec reverse n = function
                | [] -> n
                | a::xs -> reverse (a::n) xs
            reverse [] xs
            
            //List.rev xs                     // write a function to reverse a list here.  (Not sure if it meant manually write it... or this line) Editor: Richard
        rev [9;8;7] |> should equal [7;8;9]
        rev [] |> should equal []
        rev [0] |> should equal [0]
        rev [9;3;4;1;6;5;4] |> should equal [4;5;6;1;4;3;9]

    // Hint: https://msdn.microsoft.com/en-us/library/ee340277.aspx
    [<Test>]
    let ``02 Reversing a list, the easy way`` () =
        List.rev [9;8;7] |> should equal [7;8;9]
        List.rev [] |> should equal []
        List.rev [0] |> should equal [0]
        List.rev [9;8;5;8;45] |> should equal [45;8;5;8;9]
