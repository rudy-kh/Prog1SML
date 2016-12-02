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


(* Aufgabe E.12 *)
fun first (s:int) (p:int->bool) : int = if p s then s else first (s+1) p

fun forall m n p = m>n orelse (p m andalso forall (m+1) n p)								 
								 
fun sqrt (x:int) = first 1 (fn (k:int) => k*k>x) - 1

fun prime x = x>=2 andalso
              forall 2 (sqrt x) (fn k => x mod k <> 0)

fun nextprime x = first (x+1) prime

fun primelist n =  nextprime n



(* Aufgabe E.13 *)





(* Aufgabe E.14 *)
fun intPairCompare ((x,x'),(y,y')) =  case Int.compare(x,y) of
                                          EQUAL => Int.compare(x',y')
                                          | v => v


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


(* Aufgabe E.3 *)
fun partition1 n xs = foldl (fn  (x,(us,vs)) => if  x  < n then (x ::us, vs) else (us,x ::vs) ) ([],[]) xs


(* Aufgabe 4 *)
(* a *)			    
fun qsort nil = nil
  | qsort ( x :: xr ) = let val ( us , vs ) = partition1 x xr
			 in qsort us @ [ x ] @ qsort vs end

(* Aufgabe E.5 *)
fun isort' xs = let
                   fun insert (x , nil ) = [ x ]
		     | insert (x , y :: yr ) = if x<y then y :: x :: yr
					       else if x=y then x :: yr
					            else x :: insert (y , yr )
                in foldl insert nil xs end			    
			    
(* Aufgabe E.1 *)
fun signum x = case  Int.compare(x,0)   of
		   GREATER => 1
		 | LESS  => ~1
		 | _  => 0 
		

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
					   



			     
