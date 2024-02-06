let q15 =
    6
    |> (*) 5
    |> (+) ((*) 3 4) 
    |> (-) 0

let q16 =
    [0..10]
    |> List.partition (fun n -> n % 3 = 0)
    |> fun (a,b) -> List.rev a @ b

let q17 = 
    let thing x = function | x when x = 1 -> 2 | _ -> 4
    thing 3 1

let q18 =
    let double f x = f ( f x)
    let f a b c d =
        (a b) (c d)
    let g = f double id double double
    g (fun x -> x + 1) 3

let q19 =
    let rec f a b =
        match a,b with
        | _, 0 -> 1
        | 0, _ -> b * f 0 (b - 1)
        | n, _ -> n + (f (n-1) b)
    f 3 4

let q20 = 
    let g f x = x |> f |> f |> f 
    let a n m f = n f >> m f
    let b n m f = m ( n f )
    b ( a g ( b g g ) ) g ( fun x -> x + 10 ) 0

let q21 = 
    let g f x = x |> f |> f |> f 
    let a n m f = n f >> m f
    a ( a g g ) ( fun f x -> f x ) ( fun x -> x + 1 ) 0

let q22 = 
    let f x y = 
        let n = y*y
        match x with 
        | n -> 10
        | 5 -> 5
        | _ -> 1
    f 5 5 

let q8 x = 
    let y = x
    let x = x + 1
    let z = x + y
    x + y + z

let q4 x y z w = x ( y w ) ( z w )

let q3 x y z = x z 

let q2 a b = [[a]; b]

let q1 x = ( fst x ) ( snd x )

printfn "%d" (q8 1)