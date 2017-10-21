(* peschwar *)
open Printf;;

let average a b =
	(* (a +. b) /. 2.0;; *)
	let sum = a +. b in
	sum /. 2.0;;

let rec range a b =
	if a > b then [] (* empty list is last element b/c (a+1) > b *)
	else a :: range (a + 1) b;; (* :: = cons  *)

let result = average 1.0 3.0;;
printf "%f\n" result;;

let foo = range 2 10;;
(* let () =  List.iter (printf "%d ") foo;; *)
List.iter (printf "%d ") foo;;
printf("\n");;

(* wildcards *)
let fst(x, _) = x;;
let scnd(_, y) = y;;

let e = fst(1, 2);;
printf "e: %d\n" e;;

(* for lists, if it's null do one thing, if its not, do another *)

(* recursive factorial. *)
let fac n = 
	let rec fac' n' m' = match n' with
		| 0 -> m' (* the second element gets returned when n' gets dec'd to 0 *)
		| n' -> fac' (n' - 1) (n' * m')
	in fac' n 1;;

let bar = fac 5;;
printf "bar: %d\n" bar;;

(* ARRAY, not a list *)
let a = [|1;2;3;4|];;
Array.iter (printf "%d ") a;;
printf "\n";;
(* changing a[2] *)
a.(2) <- 4;;
Array.iter (printf "%d ") a;;
printf "\n";;