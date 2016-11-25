(* this file is to practice ideas of Lists and Strings of Chapter 4 *)


(* recursive length function *)
fun length xs = if xs = nil then 0 else 1 + length (tl xs)

(* tail recursive length function with let expression *)
fun lengthend xs = let fun lengthHilfe (a, xs) = if xs = nil then a else
						 lengthHilfe (a + 1, tl xs)
                   in 
                       lengthHilfe(0, xs)
                   end

fun length1 nil = 0 
  | length1 (x::xs) = 1 + length1 xs 

(* Aufgabe 4.21 S.94 *)
fun strSize (str : string) = length(explode str)

(* Aufgabe 4.22 S.94 *)
fun strReverse (str : string) = implode(rev (explode str))


fun palindrome str =
  let val strReverse = explode str in
    strReverse = rev strReverse 
  end




















































