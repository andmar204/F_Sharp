open System

let rec listAdd = function
    | [] -> 0
    | n::ns -> n + listAdd ns;;

let gradeFinder = function
    | age when age < 5 -> "Preschool"
    | 5 -> "Kindergarten"
    | age when ((age > 5) && (age < 18)) -> (age - 5).ToString()
    | _ -> "College"

let revLists xs = List.map (fun x -> List.rev) xs

printf "listAdd: %A\n" (listAdd [1..500])
printf "gradeFinder: %s\n" (gradeFinder 21)
printf "revLists: %A" (revLists [[0;1;1];[3;2];[];[5]])

Console.ReadKey() |> ignore