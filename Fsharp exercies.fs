type T<'a> = L | N of T<'a> * 'a * T<'a>
let t = N(N(L, 1, N(N(L, 2, L), 1, L)), 3, L);;


(*Declare a function count a t that can count the number of occurrences of a in the binary
tree t. For example, the number of occurrences of 1 in the tree t is 2.*) 

let rec count a t acc =
    match t with
    |L -> acc 
    |N (t1,b,t2) -> let acc = if b=a then acc+1 else acc 
                    match count a t1 acc with
                        |acc -> count a t2 acc 

(*
Declare a function replace, so that replace a b t is the tree obtained from t by re-
placement of every occurrence of a by b. For example, replace 1 0 t gives the tree
N(N(L, 0, N(N(L, 2, L), 0, L)), 3, L).5
*)
let rec replace a b t =
    match t with 
    |L -> t 
    |N(t1,c,t2) when c=a -> N(replace a b t1,b,replace a b t2) 
    |N(t1,c,t2) -> N(replace a b t1 ,c,replace a b t2)


(*exam December 19th, 2012*)

type Name = string;;
type Score = int;;
type Result = Name * Score;;
Result("hello",23)

let rec legalResults sc =  
    match sc with 
    |(_,s)::l -> if s>=0 && s<=100 then legalResults l else false 
    |_ -> true 



let rec maxScore (sc:Result list) = 
    match sc with 
    |[] -> None
    |sc -> match List.max sc with 
            |(n,s) -> Some(s)


(*Declare a function named "best" that takes in a non-empty list of results and returns the best result.
 If there are multiple results with the best score, an arbitrary result can be chosen. If the input list is empty, 
 the behavior of the function is undefined.

*)
let best (s:Result list) = if s.Length<>0 then Some(List.max s) else None

(*Declare a function named "average" that takes in a non-empty list of results and returns the average score for the list. 
If the input list is empty, then we do not are about the result ofthe function.
.



*)
let rec average (s:Result list) =
    if s.Length<>0 then
        let scores = List.map (fun (n,sc) -> float sc ) s
        Some(scores|>List.average)
    else None

(*
"Declare a function delete: Result -> Result list -> Result list. 
The value of delete r rs is the result list obtained from rs by deletion of the first occurrence of r,
 if such an occurrence exists. If r does not occur in rs, then delete r rs = rs." *)


let rec delete r rs= 
    let result = List.filter (fun x -> x<>r) rs
    result:Result list





(*"Declare a function bestN: Result list -> int -> Result list,
    where the value of bestN rs n, for n >= 0, is a list consisting of the n best results from rs. 
    The function should raise an exception if rs has fewer than n elements." *)

let rec bestN (rs: Result list) n =
    if n<0 then failwith "n cannot be negative"  
    elif n>rs.Length then failwith "n cannot be greater than the length of the results lists"
     else 
    let order= List.sortBy (fun (x,y) -> y) rs |>List.rev
    order |> List.take n





//Problem 2  
type Typ = | Integer
            | Boolean
            | Ft of Typ list * Typ;;
type Decl = string * Typ;;


(*Declare a function distinctVars: Decl list -> bool, where distinctVars decls
returns true if all variables in decls are different *)
let rec distinctVals (dl: Decl list) = 
    match dl with 
    |[] -> true
    |(x,t)::res -> if List.exists(fun (x1,t1)-> x1=x) res then false  else distinctVals res


let toSymbolTable (dl:Decl list)= 
    let myMap = Map.empty<string,Typ>
    let myMap = dl|>List.fold ( fun map (x,y) -> Map.add x y map) myMap 
    myMap





let d:Decl list =[(">",Ft([Integer;Integer],Boolean));("=",Ft([Integer;Integer],Boolean))]

let de = toSymbolTable d


let extendedST sym dl = 
    let updated = dl|> List.fold (fun map (key,value) -> Map.add key value map) sym
    updated 

