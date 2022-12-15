(* Problem 4 from functinal programming exam May 16 ) 

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

