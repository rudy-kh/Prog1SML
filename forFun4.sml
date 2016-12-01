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

fun revStringList (xs : string list) = map implode (map rev ( map explode  xs))

(*fun flat *)
fun flat (xs : int list list) = foldr op@ nil xs;

fun strListConcat (xs : string list) = foldr op^ "" xs;

(* Aufgabe 4.4 S.80 *)
fun iterdn n m s f = if n<m then s else iterdn (n-1) m (f(n,s)) f
fun enum m n = iterdn (n) m nil (fn (i, xs) => i :: xs) 


(* Aufgabe 4.5 S.80*)
fun ''a member x xs = if xs = nil then false else x= (hd xs) orelse member x (tl xs)
									   
fun member1 x nil = false
  | member1 x (y::xs) = x=y orelse member1 x xs
   
(*fun member2 x xs = #1 (foldr (fn (s, y::ys) => if x = y then (true, ys) else (false, ys)) x xs)
*)

fun countEO xs = let val ab = List.filter (fn x => x mod 2 = 0) xs
		 in
		     (length ab, length xs - length ab)
		 end	 



fun revString  str =  let val strList = explode str
                          fun f strList = foldl  op::  nil strList
                      in implode (f strList) end
































