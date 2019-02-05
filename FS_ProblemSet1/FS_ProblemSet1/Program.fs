open System

(*Problem 1
4 + 5.6 is not well typed because the + operator requires the same type for
both operands. 2 + 5 * 10, 10I * 20I, and "4" + "5.6" are all fine because they
all have the same type.
*)

(*Problem 2
A curried function is a function that has only one parameter and returns a
function. Using that definition, the best answer is C, t1 -> (t2 -> t3). A and
B make no sense because they have a * symbol, which is used for tuples. And D
isn't a function with one parameter that returns a function, but rather a
function that takes a function as a parameter and returns a value.
*)

(*Problem 3
If a function has type 'a -> 'b when 'a : comparison, then 'a has to be a
comparable type. Therefore, (float -> float) -> bool is not a legal type.
Because (float -> float) is a function and you can't compare functions. But you
can compare strings, ints, and int lists (I'm not sure about the int lists, but
I am sure you can't compare functions, so A is my final answer)
*)

(*Problem 4
F# lists can't hetergeneous. I tried making a list like [1;"2";3.14;'a'] (it's
in one of my previoius commits) and F# didn't let that fly. 
*)

let problem5() = 
    let list1 = 1::2::3::[]
    //let list2 = 1@2@3@[]
    let list3 = [1; 2; 3]::[]
    //let list4 = ((1::2)::3)::[]
    printfn "List1: %A" list1
    //printfn "List2: %A" list2
    printfn "List3: %A" list3
    //printfn "List4t: %A" list4

(*Problem 7
The * operator indicates a tuple. So the type int * bool -> string list is a
function that takes in a tuple with an int and a bool, and returns a string
list. So it'll interpret that as (int * bool) -> (string list).
*)


(*Problem 8
This function checks out on every step of the Checklist for Programming with 
Recursion.
The base case returns the correct output, the non-base cases also return the
correct output every time it runs, and the input to this recursive call gets 
smaller every time as well (even though xs is getting bigger, this function is
counting on ys shrinking).
*)
let rec problem8 = function
     | (xs, []) -> xs (*Base case*)
     | (xs, y::ys) -> problem8 (xs@[y], ys)

(*Problem 9
fun f -> f 17 gets interpreted as (int -> 'a) -> 'a. This makes sense because 
f 17 means the function f takes in an int. So I know there's going to be an 
"int ->" used to represent "fun f". But since I don't know what f returns, I 
can safely say that I also don't know what the function that takes in f returns.
Therefore, I only know one thing, that f takes in an int. The rest will be 
represented by 'a.
*)

(*Problem 10
If the type of something is int -> int list, then I know it has to be a function
that takes in an int and returns a list. The best option for this is D, which 
is fun x -> x::[5]. If we break this down, we see a list with one int in it. 
Since lists have to have the same type (as figured out in problem 5), x has to 
be an int in order to be prepended to the list. And since this is returning 
x::[5], which will become [x, 5], then the return type is int list. Therefore,
fun x -> x::[5] has type int -> int list.
*)

(*Problem 11
The expression (3, [], true) is a tuple with an int, an empty list, and a bool. 
F# would interpret this as int * 'a list * bool (because the list has no type 
yet, so F# can't infer anything).
*)

(*Problem 12
F# interprets fun x y -> x + y + "." as string -> string -> string. This is 
because of the ".". Normally, the + sign is seen for ints, but once the "." gets
involved, F# will use the + for strings. So from there, F# will infer that y has
to be a string if it's going to be used like that. And then it'll infer that x 
has to be a string as well, since it's used in x + y, and y was just infered to
be a string. And at this point, x and y are both strings, so that causes the 
type to be string -> string -> string.
*)

(*Problem 13
F# interprets fun xs -> List.map (+) xs as int list -> (int -> int) list. This
is because if you're using List.map, then the parameter (xs) has to be a list.
For the plus sign, it's a binary operator. F# interprets (+) as int -> int -> int.
That's because it's a "function" that takes two parameters (its operands). 
However, if you give (+) a single operand, it'll interpret that as int -> int. 
Because it has an operand already. So instead of (basiaclly) being x + y, it's 
like saying 3 + y. It already has the first operand, and all it needs is the 
second. Therefore, when you map (+) to every int in a list, it's interpreted as
(int -> int). And since List.map returns a list, it returns (int -> int) list.
*)

(*Problem 14
F# infers none of the options to be string -> string -> string. The closest I 
get is string * string -> string, infered from fun (x, y) -> x + y + ".". 
*)

(*Problem 15
F# infers fun f -> f (f "cat") to be (string -> string) -> string. Since there's
f "cat", that means f has to take a string. But since there's also f (f "cat"),
and we just found out that f takes in a string, then that means that the value 
of f "cat" has to be a string, meaning that f also returns a string! Since this
takes in function f, we know it'll start with (string -> string). And since 
there's f (f "cat"), we know that that'll return a string. So that ends up with
this whole thing being (string -> string) -> string.
*)

(*Problem 16*)
let rec gcd = function
    | (a,0) -> a
    | (a,b) -> gcd (b, a % b)

let lcd a b = (a * b) / (gcd (a,b))

let (.*) (a,b) (x,y) =
    let numerator = (a * x)
    let denominator = (b * y)
    let lowest = gcd(numerator, denominator)
    (numerator/lowest, denominator/lowest)

let (.+) (a,b) (x,y) = 
    let numerator = (a * y) + (x * b)
    let denominator = (b * y)
    let lowest = gcd(numerator, denominator)
    (numerator/lowest, denominator/lowest)


let frac = (1,2) .* (5,6)
let frac2 = (1,2) .+ (5,6)

printfn "(1/2) * (5/6) = %A" frac
printfn "(1/2) + (5/6) = %A" frac2
printfn "gcd = %A" (gcd (7,6))
printfn "lcd = %A" (lcd 7 6)


(*Problem 17*)
let revLists xs = List.map(fun x -> List.rev x) xs

printf "revLists: "
(revLists [[0;1;1];[3;2];[];[5]]) |> List.iter (printf "%A ")

(*Problem 18*)
let rec interleave (a, b) = 
    match (a, b) with
    | [],[] -> []
    | x::xs, [] -> x::[]
    | [], y::ys -> y::[]
    | x::xs,y::ys  -> x::y::(interleave(xs, ys))

(* This doesn't work
let rec interleave (a, b) = function
    | [],[] -> []
    | x::xs, [] -> x::[]
    | [], y::ys -> y::[]
    | x::xs,y::ys  -> x::y::(interleave(xs, ys))
*)
printf "\ninterleave: "
(interleave([1;2;3],[4;5;6;7;8;9])) |> printfn "%A"
(*
([1;2], [4;5])
1::4::(interleave([2], [5]))
      2::5::(interleave([],[]))
            []
1::4::2::5::[]
[1;4;2;5]

*)

(*Problem 19*)
let rec gencut n right left = 
    match right with
    | [] -> List.rev right, left
    | _ when n = 0 -> List.rev left, right
    | h::t -> gencut(n - 1) t (h::left)

let cut list = 
    gencut ((List.length list) / 2) list []

printfn "Cut: %A" (cut [1;2;3;4;5;6;7;8])
printfn "gencut: %A" (gencut 5 [1;2;3;4;5;6;7;8] [])


(*Problem 20*)
let shuffle list = 
    let (a, b) = cut list
    interleave (a, b)

(*Problem 21*)
let matchtest x y =
    match x, y with
    | "A", _ -> "hello a"
    | _, "B" -> "hello b"
    | _ -> "hello?"

let functiontest = function
    | "A", _ -> "hello a"
    | _, "B" -> "hello b"
    | _ -> "hello?"
    
    
    

//problem5()

Console.ReadKey() |> ignore