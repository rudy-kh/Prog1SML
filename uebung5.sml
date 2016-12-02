(* Aufgabe E.11 *)
(* Rot, Blau, Gelb, Grün und Weiss 
fun tif xs = let
                fun rwb ("Rot" ,(rs,bs,gs,grs,ws)) = ("Rot"::rs, bs, gs, grs, ws)
                | rwb ( "Blau" ,(rs,bs,gs,grs,ws)) = (rs,"Blau":: bs, gs, grs, ws)
                | rwb ("Gelb",(rs,bs,gs,grs,ws)) = (rs, bs,"Gelb":: gs, grs, ws)
                | rwb ("Grün",(rs,bs,gs,grs,ws)) = (rs, bs, gs,"Grün":: grs, ws)
                | rwb ("Weiss",(rs,bs,gs,grs,ws)) = (rs, bs, gs, grs,"Weiss":: ws)
                | rwb (_,t) = t
                val tflag = foldl rwb ([],[],[],[],[]) xs
             in
                #1 tflag @ #2 tflag @ #3 tflag @  #4 tflag @ #5 tflag
             end
*)


(*********************************************************************************)

(* Aufgabe E.12 *)
fun first (s:int) (p:int->bool) : int = if p s then s else first (s+1) p

fun forall m n p = m>n orelse (p m andalso forall (m+1) n p)								 
								 
fun sqrt (x:int) = first 1 (fn (k:int) => k*k>x) - 1

fun prime x = x>=2 andalso
              forall 2 (sqrt x) (fn k => x mod k <> 0)

fun nextprime x = first (x+1) prime
			
fun primelist n = let val xs = List.tabulate(n, fn x => x)
		  in tl(rev (foldl (fn (x,s) => (nextprime (hd s)) :: s) [0] xs)) end  

(*********************************************************************************)


			     
(* Aufgabe E.13 *)



(*********************************************************************************)

(* Aufgabe E.14 *)
fun intPairCompare ((x,x'),(y,y')) =  case Int.compare(x,y) of
                                          EQUAL => Int.compare(x',y')
                                          | v => v


(*********************************************************************************)
						     

(* Aufgabe E.16 *)
(* a *)
fun partition cmp r xs = let
                            fun spread (x ,( us , vs , ws )) = case cmp (x , r ) of
                                                                  GREATER =>( us , vs , x :: ws )
                                                                  | EQUAL =>( us , x :: vs , ws )
                                                                  | LESS =>( x :: us , vs , ws )
                         in
                            foldl spread ( nil , nil , nil ) xs
                         end

(* b *)
fun pqsort cmp nil = nil
    | pqsort cmp ( x :: xr ) = let
                                  val ( us , vs , ws ) = partition cmp x xr
                               in
                                  pqsort cmp us @ [ x ] @ pqsort cmp vs @ pqsort cmp ws
                               end

(*********************************************************************************)
				   

(* Aufgabe E.3 *)
fun partition1 n xs = foldl (fn  (x,(us,vs)) => if  x  < n then (x ::us, vs) else (us,x ::vs) ) ([],[]) xs

(*********************************************************************************)

			    
(* Aufgabe 4 *)
(* a *)			    
fun qsort nil = nil
  | qsort ( x :: xr ) = let val ( us , vs ) = partition1 x xr
			 in qsort us @ [ x ] @ qsort vs end

(*********************************************************************************)


			    
(* Aufgabe E.5 *)
fun isort' xs = let
                   fun insert (x , nil ) = [ x ]
		     | insert (x , y :: yr ) = if x<y then y :: x :: yr
					       else if x=y then x :: yr
					            else x :: insert (y , yr )
                in foldl insert nil xs end			    


(*********************************************************************************)

		    
(* Aufgabe E.1 *)
fun signum x = case  Int.compare(x,0)   of
		   GREATER => 1
		 | LESS  => ~1
		 | _  => 0 
		

(*********************************************************************************)

			     
(* Aufgabe E.17 *)
(* a *)			     
datatype shape =
	   Circle of real
	 | Square of real
	 | Triangle of real * real * real
	 | Polyglon of real * real				 

(* b *)
fun edgeLength (Circle r ) = 2.0 * Math.pi * r
  | edgeLength (Square a)  = 4.0 * a 
  | edgeLength (Triangle (a, b, c)) = a + b + c
  | edgeLength (Polyglon (n, a)) = n * a 


(* c *)
fun scale s (Circle r ) = Circle (s * r)
  | scale s (Square a ) = Square (s * a)
  | scale s (Triangle (a,b,c)) = Triangle (a * s, b * s, c * s)
  | scale s (Polyglon (n, a)) = Polyglon (n , s * a) 
					   
(*********************************************************************************)
(* Aufgabe E.18 *)
(* a *)			
datatype computer =
	   Netbook of int * int
	 | Notebook of int * int
	 | TowerPC of int * int 
			     



(* b *)
(* Schreiben Sie eine Prozedur compareC : computer * computer → order die zwei Computer vergleicht,
wobei bei einem Netbook die Performance des Speichers zu zwei Drittel und die des Prozessors zu
einem Drittel in die Bewertung eingehen, bei einem Notebook jeweils zur Hälfte und bei einem Tower der
Prozessor zu zwei Dritteln und der Speicher zu einem Drittel. *)
datatype order = LESS | EQUAL | GREATER				     
fun compareC (Netbook(s1, p1),  Netbook(s2, p2)) : order = let
                                                              val d1 = s1 - s2
							      val d2 = p1 - p2
                                                           in
							       if Real.fromInt(d1) > (Real.fromInt(s2) * 0.66) andalso Real.fromInt(d2) > (Real.fromInt(p2) * 0.33) then GREATER
							       else if Real.fromInt(~ d1) > Real.fromInt(s1) * 0.66 andalso Real.fromInt(~ d2) > Real.fromInt(p1) * 0.33 then LESS
 	 						       else EQUAL								      
							   end		
  | compareC (Notebook(s1, p1), Notebook(s2, p2)) : order = let
                                                              val d1 = s1 - s2
							      val d2 = p1 - p2
                                                            in
							       if Real.fromInt(d1) > (Real.fromInt(s2) * 0.5) andalso Real.fromInt(d2) > (Real.fromInt(p2) * 0.5) then GREATER
							       else if Real.fromInt(~ d1) > Real.fromInt(s1) * 0.5 andalso Real.fromInt(~ d2) > Real.fromInt(p1) * 0.5 then LESS
 	 						       else EQUAL								      
                                                            end
  | compareC (TowerPC(s1, p1), TowerPC(s2, p2)) : order = let
                                                              val d1 = s1 - s2
							      val d2 = p1 - p2
                                                            in
							       if Real.fromInt(d1) > (Real.fromInt(s2) * 0.33) andalso Real.fromInt(d2) > (Real.fromInt(p2) * 0.66) then GREATER
							       else if Real.fromInt(~ d1) > Real.fromInt(s1) * 0.33 andalso Real.fromInt(~ d2) > Real.fromInt(p1) * 0.66 then LESS
 	 						       else EQUAL								      
							    end									


(* c *)
(*  Machen Sie sich klar, dass Sie nun in der Lage sind, mithilfe polymorpher Sortier-Prozeduren eine Liste
von Computern anhand ihrer Performance zu sortieren *)				 

fun sortC cs = let
                   fun insert (x , nil ) = [ x ]
		     | insert (x , y :: yr ) = if compareC(x, y) = GREATER then y :: x :: yr
					       else if compareC(x, y)= EQUAL then x :: yr
					            else x :: insert (y , yr )
                in foldl insert nil cs end


(*********************************************************************************)

	     
(* Aufgabe E.19 *)

datatype poly = C of int 
               | X
	       | A of poly * poly 
	       | M of poly * poly
	       | P of poly * int 
				

	     
(* a *)	     
val u = A(A(A(P(X,3),M(C 3,P(X,2))),X),C 2)

(* b *)	 
fun derive (C _) = C 0
  | derive X = C 1
  | derive (A (u, v)) = A (derive u, derive v)
  | derive (M (u, v)) = A (M (derive u, v), M (u, derive v))
  | derive (P (u, n)) = M (M (C n, P(u, n-1)), derive u)

(* c *)
fun simplifyTop ( C x ) = C x
  | simplifyTop ( X  ) = X 
  | simplifyTop ( A (x , y )) = ( case ( simplifyTop x , simplifyTop y ) of
				   ( C u , C v ) => C ( u + v )
				 | (u , v ) =>  A (u , v ))
  | simplifyTop ( M (x , y )) = ( case ( simplifyTop x , simplifyTop y ) of
				   ( C u , C v ) => C ( u * v )
				 | (u , v ) => M (u , v ))

  | simplifyTop (P (u, n))  = (case (n)  of
				0 => C 1
			      | 1 => X 
			      | _ => P (simplifyTop u, n)) 
(*********************************************************************************)

(* Aufgabe E.20 *)
datatype nat = O | S of nat			    
(* a *)			       
fun rep 0 = O
  | rep n = S ( rep ( n - 1))

(* b *)
fun num O = 0
  | num (S x) = 1 + num x
(* c *)

fun unitList O = []
  | unitList (S x) = ():: unitList x 
			
(* d *)
fun add O y = y
  | add (S x) y = add x (S y)
fun mul O y = O
  | mul (S x) y = add y (mul x y)
fun less _ O = false
  | less O _ = true
  | less (S x) (S y) = less x y			
