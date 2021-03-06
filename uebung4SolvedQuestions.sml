(* Aufgabe 4 *)  (* please note here that a and d haven't been solved yet *)
(* b *)
fun member1 n xs = List.exists (fn y => y = n) xs

(* c *)
fun member2 n xs = if (List.filter (fn y => y=n) xs) = [] then false else true

(* Aufgabe 5 *)

(* a *)
fun listevonzahlen m n xs = if m<n then xs else listevonzahlen (m-1) n (m::xs)
(* b *)
fun loesche n nil = nil | loesche n (x::xr) = if (x mod n = 0) then loesche n xr else x::(loesche n xr)
(* c *)
fun sieb’ xs nil = xs | sieb’ xs (y::yr) = sieb’ (xs@[y]) (loesche y yr)
(* d *)
fun siebdeserathostenes n = sieb’ nil (listevonzahlen n 2 nil)


(* Aufgabe 10 *)
(* a *)
fun lengthReal nil = 0.0 |lengthReal (x::xr) = 1.0 + lengthReal xr
fun average xr = (foldl op+ 0.0 xr)/(length xr)
(* b *)
fun average xr = let val (a, b)= foldl (fn (x, (y, z))=>(y+1.0, z+x)) (0.0, 0.0) xr in b/a end


(* Aufgabe 11 *)
(*
(a) 5 : int
(b) 42 : int
(c) 42 : int
(d) 56 : int
(e) Die Ausnahme M atch wird geworfen.
(f) Die Ausnahme Empty wird geworfen.
(g) Es wird kein Muster getroffen, deshalb wird eigentlich die Ausnahme Empty geworfen. Die in der
Vorlesung benutzten SML-Interpreter runden jedoch, weswegen sie zu 42 : int auswerten.

*)


(* Aufgabe 12 *)

(* a *)
fun characterStrength a = foldl (fn (x, y) => ord x + y) 0 (explode a)
(* b *)
fun loveGen (x, y) = (characterStrength x + characterStrength y) mod 100
(* c *)
fun loveGenHarem xr = (foldr (fn (x, y)=> characterStrength x + y) 0 xr) mod 100



(* Aufgabe 13 *)
fun palindrome str =
  let val strReverse = explode str in
    strReverse = rev strReverse
  end



(* Aufgabe 14 *)

fun max xs = foldl Int.max (hd xs) (tl xs)


(* Aufgabe 15 *)
(* a *)
fun append (xs, ys) = foldr op:: ys xs

(* b *)
fun rev xs = foldr (fn (x,ys) => append(ys,[x])) nil xs

(* c *)
fun foldl1 f s xs = rev(foldr f s xs)

(* aufgabe 15.d is not solved yet *)


(* Aufgabe 17 *)

fun sortOddEven (xs : int list) = let val evenNumList = List.filter (fn y => (y mod 2)=0) xs
				      val oddRevList  = rev (List.filter (fn y => (y mod 2) <> 0) xs )
				  in
				      evenNumList @ oddRevList
				  end
