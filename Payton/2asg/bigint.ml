(* $Id: bigint.ml,v 1.5 2014-11-11 15:06:24-08 - - $ *)

open Printf

let complain message =
    ( flush stdout;
      Printf.eprintf "%s: %s\n" Sys.argv.(0) message;
      flush stderr )

module Bigint = struct

    type sign     = Pos | Neg
    type bigint   = Bigint of sign * int list
    let  radix    = 10
    let  radixlen =  1

    let car       = List.hd
    let cdr       = List.tl
    let map       = List.map
    let reverse   = List.rev
    let strcat    = String.concat
    let strlen    = String.length
    let strsub    = String.sub
    let zero      = Bigint (Pos, [])

    let rec cmp' value1 value2 =
        if List.length value1 = 0 && List.length value2 = 0 then 0
        else if List.length value1 = 0 && List.length value2 > 0 then -1
        else if List.length value1 > 0 && List.length value2 = 0 then 1
        else if List.hd value1 > List.hd value2 then 1
        else if List.hd value1 < List.hd value2 then -1
        else cmp' (List.tl value1) (List.tl value2)


    let cmp value1 value2 = 
        if (List.length value1) < (List.length value2) then -1
        else if (List.length value1) > (List.length value2) then 1
        else cmp' (List.rev value1) (List.rev value2)

    let bi_cmp (Bigint (neg1, value1)) (Bigint (neg2, value2)) = 
        if neg1 = Pos && neg2 = Neg then 1
        else if neg1 = Neg && neg2 = Pos then -1
        else if neg1 = Pos && neg2 = Pos then (cmp value1 value2)
        else if neg1 = Neg && neg2 = Neg then (cmp value1 value2)
        else 0

    let rec strip_zero' list =
        if (car list) = 0 then strip_zero'(cdr list)
        else list

    let strip_zero list = match list with
        | []     -> []
        | [0]    -> list
        | list   -> strip_zero' (reverse list)

    let charlist_of_string str = 
        let last = strlen str - 1
        in  let rec charlist pos result =
            if pos < 0
            then result
            else charlist (pos - 1) (str.[pos] :: result)
        in  charlist last []

    let bigint_of_string str =
        let len = strlen str
        in  let to_intlist first =
                let substr = strsub str first (len - first) in
                let digit char = int_of_char char - int_of_char '0' in
                map digit (reverse (charlist_of_string substr))
            in  if   len = 0
                then zero
                else if   str.[0] = '_'
                     then Bigint (Neg, to_intlist 1)
                     else Bigint (Pos, to_intlist 0)

    let string_of_bigint (Bigint (sign, value)) =
        match value with
        | []    -> "0"
        | value -> let reversed = strip_zero value
                   in  strcat ""
                       ((if sign = Pos then "" else "-") ::
                        (map string_of_int reversed))

    let actual_value list1 =
        (int_of_string (strcat "" (map string_of_int (reverse list1))))

    let rec add' list1 list2 carry = match (list1, list2, carry) with
        | list1, [], 0       -> list1
        | [], list2, 0       -> list2
        | list1, [], carry   -> add' list1 [carry] 0
        | [], list2, carry   -> add' [carry] list2 0
        | car1::cdr1, car2::cdr2, carry ->
          let sum = car1 + car2 + carry in
          sum mod radix :: add' cdr1 cdr2 (sum / radix)


    let rec sub' list1 list2 carry = match (list1, list2, carry) with
        | list1, [], 0        -> list1
        | [], list2, 0        -> list2
        | list1, [], carry    -> sub' list1 [carry] 0
        | [], list2, carry    -> sub' [carry] list2 0
        | car1::cdr1, car2::cdr2, carry ->
            let car1 = car1 - carry in
            if car1- car2 < 0
            then 
                (
                    let diff = car1 + radix - car2 and carry = 1 in
                    diff mod radix :: sub' cdr1 cdr2 carry
                )
            else 
                (
                    let diff = car1 - car2 in
                    diff mod radix :: sub' cdr1 cdr2 0
                )

    let add (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        (* printf "%d\n" (cmp value1 value2); *)
        if neg1 = neg2
        then Bigint (neg1, add' value1 value2 0)
        else if (neg1 = Pos && neg2 = Neg)
        then (
                if (cmp value1 value2) = 1 
                then Bigint(neg1, sub' value1 value2 0)
                else Bigint(neg2, sub' value2 value1 0)
             )
        else if (neg1 = Neg && neg2 = Pos)
        then (
                 if (cmp value1 value2) = 1
                 then Bigint(neg1, sub' value1 value2 0)
                 else Bigint(neg2, sub' value2 value1 0)
             )
        else (
                if (cmp value1 value2) = 1
                then Bigint(neg1, sub' value1 value2 0)
                else Bigint(neg2, sub' value2 value1 0)
             )


    let sub (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        if neg1 <> neg2
        then (
                (* For subtracting w/ diff. signs, the sign
                 * is that of the biggest abs(num) *)
                if (cmp value1 value2) = 1
                then Bigint (neg1, add' value1 value2 0)
                else Bigint (neg2, add' value2 value1 0)
             )
        else ( 
                if (cmp value1 value2) = 1
                then Bigint (neg1, sub' value1 value2 0)
                else Bigint (neg2, sub' value2 value1 0)
             )

    let double number = add' number number 0

    let rec mul' list1 powerof2 list2 =
        if (actual_value powerof2) > (actual_value list1)
        then list1, [0]
    else let rem, prod = 
        mul' list1 (double powerof2) (double list2) in 
        if (actual_value rem) < (actual_value powerof2)
        then rem, prod 
    else (sub' rem powerof2 0), (add' prod list2 0)

            

    let mul (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        if neg1 = neg2
        then let _, prod =
            mul' value1 [1] value2 in Bigint(Pos, prod)
        else let _, prod =
            mul' value1 [1] value2 in Bigint(Neg, prod)

    let rec divrem' list1 powerof2 list2 =
        if (actual_value list2) > (actual_value list1)
        then [0], list1
        else let quotient, remainder = 
                 divrem' list1 (double powerof2) (double list2) in 
             if (actual_value remainder) < (actual_value list2)
                then quotient, remainder
                else (add' quotient powerof2 0), (sub' remainder list2 0)

    let divrem list1 list2 = divrem' list1 [1] list2

    let div (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        if (actual_value value2) = 0 then raise(Division_by_zero)
        else if neg1 = neg2
        then let quotient, _ = divrem value1 value2 in 
        Bigint (Pos, quotient)
        else let quotient, _ = divrem value1 value2 in 
        Bigint (Neg, quotient)

    let rem (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        if neg1 = neg2
        then let _, remainder = divrem value1 value2 in 
            Bigint(Pos, remainder)
        else let _, remainder = divrem value1 value2 in 
            Bigint(Neg, remainder)

    let is_even list1 = 
        if (actual_value list1) mod 2 = 0 then true
        else false

    let is_odd list1 = 
        if (actual_value list1) mod 2 = 0 then false
        else true

    let rec pow' base expt result  = match expt with
        | [0]                       -> result
        | expt when (is_even expt)  -> pow' (snd(mul' base [1] base)) (fst(divrem expt [2])) result
        | expt                      -> pow' base (sub' expt [1] 0) (snd(mul' base [1] result))

    let pow (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        if neg2 = Neg then Bigint(Pos, [0])
        else if (is_even value2) then Bigint(Pos, pow' value1 value2 [1])
        else Bigint(neg1, pow' value1 value2 [1])

end

