module intSort

// O(n^2):
let rec selectionSort = function
  | [] -> [] : int list
  | x::xs -> 
    let min, rest =
      List.fold (fun (min, acc) h ->
                      if h < min then (h, min::acc)
                      else (min, h::acc))
        (x, []) xs
    in min::selectionSort rest

let bubbleSort (l : int list) = 
  let rec sortCall acc rev l =
    match l, rev with
    | [], true -> acc |> List.rev
    | [], false -> acc |> List.rev |> sortCall [] true
    | x::y::tl, _ when x > y -> sortCall (y::acc) false (x::tl)
    | hd::tl, _ -> sortCall (hd::acc) rev tl
  sortCall [] true l

let rec insert i = function
  | h::t -> min h i::(insert (max h i) t)
  | _ -> [i]

let insertionSort (l : int list) = List.foldBack insert l []

// O(n·log(n)):
let rec quickSort = function // (average performance)
  | (x :: xs) ->
    let left, right = List.partition (fun i -> i < x) xs
    let sleft = quickSort left
    let sright = quickSort right
    sleft @ (x :: sright)
  | [] -> [] : int list

let f (a,b) c = (b, c::a)
let split xs = List.fold f ( [], [] ) xs

let rec mergeCall = function
  | ( [], ys ) -> ys
  | ( xs, [] ) -> xs
  | ( x::xr, y::yr ) -> if x<=y then x::mergeCall( xr, y::yr ) else y::mergeCall( x::xr, yr )

let rec mergeSort = function
  | [] -> [] : int list
  | [x] -> [x]
  | xs -> let (ys, zs) = split xs
          in mergeCall(mergeSort ys, mergeSort zs)  

type LeftistHeap<'a> = 
  | E 
  | T of int * 'a * LeftistHeap<'a> * LeftistHeap<'a> 

let rank t = match t with E -> 0 | T (r, _, _, _) -> r 

let T(x, a, b) = 
  let a, b = if rank a > rank b then a, b else b, a
  T(rank b, x, a, b) 

let rec merge h1 h2 = 
  match h1, h2 with 
    | h, E | E, h -> h 
    | T(_, x, a1, b1), T(_, y, _, _) when x >= y -> T(x, a1, merge b1 h2) 
    | T(_, x, _, _), T(_, y, a2, b2) -> T(y, a2, merge h1 b2) 

let rec toList xs = function 
  | E -> xs
  | T(_, x, a, b) -> toList (x::xs) <| merge a b 

let heapSort (xs : int list) = 
  toList [] (List.fold (fun h x -> merge (T(x, E, E)) h) E xs)

// O(k·n), but non-comparative sorting algorithm:
let is0 x =
  match x with
    | 0 -> true
    | n -> false

// splits a list of integers in half depending on the value of the nth bit of the integers
let splitBy n xxs =
  let rec splitBy' n' xxs' (oz,iz) =
    match xxs' with
      | [] -> (oz,iz)
      | x :: xs -> splitBy' n' xs (if is0 ((x >>> n') &&& 1) then (x::oz,iz) else (oz,x::iz))
  splitBy' n xxs ([],[])

let join (oz,iz) = List.concat [List.rev oz; List.rev iz]

let sortBy n xs = join (splitBy n xs)

let radixSort xs = // items must be positive
  let rec sort' n xs' =
    match n with
      | 0 -> xs'
      | i -> sortBy (i-1) (sort' (i-1) xs')
  sort' 32 xs