(* $Id: bigint.ml,v 1.5 2014-11-11 15:06:24-08 - - $ *)
(*  Thomas Burch (1314305)
    tburch1@ucsc.edu
    CMPS112-ASG2
*)

open Printf

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

    (*let charlist_of_string str = 
        let last = strlen str - 1
        in  let rec charlist pos result =
            if pos < 0
            then result
            else charlist (pos - 1) (str.[pos] :: result)
        in  charlist last [] *)

    let bigint_of_string str =
        (*let len = strlen str
        in  let to_intlist first =
                let substr = strsub str first (len - first) in
                let digit char = int_of_char char - int_of_char '0' in
                map digit (reverse (charlist_of_string substr))
            in  if   len = 0
                then zero
                else if   str.[0] = '_'
                     then Bigint (Neg, to_intlist 1)
                     else Bigint (Pos, to_intlist 0)*)
        (*ADDED*)
        let rec makelist str first len =
            if   first = len
            then [] 
            else let diff = len - first in
                 let len' = len - radixlen in
                 if   diff < radixlen
                 then [int_of_string (strsub str first diff)]
                 else (int_of_string (strsub str len' radixlen))
                      :: (makelist str first len')
        in  let len = strlen str
            in  if   len = 0
                then Bigint (Pos, [])
                else if   str.[0] = '_'
                     then Bigint (Neg, makelist str 1 len)
                     else Bigint (Pos, makelist str 0 len)

    let string_of_bigint (Bigint (sign, value)) =
        match value with
        | []    -> "0"
        | value -> let reversed = reverse value
                   in  strcat ""
                       ((if sign = Pos then "" else "-") ::
                        (*(map string_of_int reversed))*)
                        (*ADD*)
                        (string_of_int (car reversed)) ::
                        (map (sprintf "%03d") (cdr reversed)))

    let rec add' list1 list2 carry = match (list1, list2, carry) with
        | list1, [], 0       -> list1
        | [], list2, 0       -> list2
        | list1, [], carry   -> add' list1 [carry] 0
        | [], list2, carry   -> add' [carry] list2 0
        | car1::cdr1, car2::cdr2, carry ->
          let sum = car1 + car2 + carry
          in  sum mod radix :: add' cdr1 cdr2 (sum / radix)

    (*ADDED*)
    let rec sub' list1 list2 carry = match (list1, list2, carry) with
        | list1, [], 0       -> list1
        | [], list2, 0       -> list2
        | list1, [], carry   -> sub' list1 [carry] 0
        | [], list2, carry   -> sub' [carry] list2 0
        | car1::cdr1, car2::cdr2, carry ->
          let diff = car1 - car2 - carry
          in diff mod radix :: sub' cdr1 cdr2 (diff / radix)

        (*ADDED*)
    let rec cmp list1 list2 = match (list1, list2) with
        | list1, []                 -> 1
        | [], list2                 -> 0
        | car1::cdr1, car2::cdr2    ->
            if car1 > car2
            then 1
            else if car2 > car1
            then 0
            else cmp cdr1 

    let add (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        if neg1 = neg2
        then Bigint (neg1, add' value1 value2 0)
        (*else zero*)
        (*ADDED*)
        else if (neg1 = Pos && neg2 = Neg)
        then (
            if (cmp value1 value2) = 1 
            then Bigint(neg1, sub' value1 value2 0)
            else Bigint(neg2, sub' value2 value1 0))
        else if (neg1 = Neg && neg2 = Pos)
        then (
             if (cmp value1 value2) = 1
             then Bigint(neg1, sub' value1 value2 0)
             else Bigint(neg2, sub' value2 value1 0))
        else (
                if (cmp value1 value2) = 1
                then Bigint(neg1, sub' value1 value2 0)
                else Bigint(neg2, sub' value2 value1 0)
            )

    
    (*let sub = add*)
    let sub (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        if (neg1 = Pos && neg2 = Pos)
        then (
            if (cmp value1 value2) = 1
            then Bigint (neg1, sub' value1 value2 0)
            else Bigint (Neg, sub' value2 value1 0))
        else if (neg1 = Neg && neg2 = Neg)
        then (
            if (cmp value1 value2) = 1
            then Bigint(neg1, add' value1 value2 0)
            else Bigint(Pos, sub' value2 value1 0))
        else Bigint(neg1, add' value1 value2 0)

    (*let mul = add*)
    let rec mul' val1 val2 =
         if (car val2) = 1
         then val1
         else (add' val1 (mul' val1 (sub' val2 [1] 0)) 0)

    let mul (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
         if neg1 = neg2
         then Bigint (Pos, mul' value1 value2)
         else Bigint (Neg, mul' value1 value2) 

    (*let div = add*)
    let div (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        if (car value2) <> 0 then (
            if neg1 = neg2
            then Bigint(Pos, fst(div' value1 value2 [0]))
            else Bigint(Neg, fst(div' value1 value2 [0]))
        )
        else(printf "dc: division by zero\n"; Bigint(Pos,[0]))

    (*let rem = add*)
    let rem (Bigint (neg1, value1)) (Bigint (neg2, value2)) = 
        if (car value2) <> 0 then (
            if neg1 = neg2
            then Bigint(neg1, snd(div' value1 value2 [0]))
            else Bigint(Neg, snd(div' value1 value2 [0]))
        )
        else (printf "dc: remainder by zero\n"; Bigint(Pos,[0]))

    (*let pow = add*)
    let rec pow' val1 val2 = 
        if (car val2) = 1
        then val1
        else (mul' val1 (pow' val1 (sub' val2 [1] 0)))

    let pow (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        if neg2 = Neg
        then (Bigint (Pos, [])) (*May need be 0 *)
        else if neg1 = Pos
           then (Bigint (neg1, pow' value1 value2))
           else if rem (Bigint (Pos, value2)) (Bigint (Pos, [2])) = 
                       (Bigint (Pos, [1]))
               then (Bigint (Neg, pow' value1 value2))
               else (Bigint (Pos, pow' value1 value2))

end
