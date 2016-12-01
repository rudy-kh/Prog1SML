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
                #1 tflag @ #2 tflag @ #3 tflag #4 tflag #5 tflag
             end
*)


(* Aufgabe E.12 
fun sqrt (x:int) = first 1 (fn (k:int) => k*k>x) - 1

fun prime x = x>=2 andalso
              forall 2 (sqrt x) (fn k => x mod k <> 0)

fun nextprime x = first (x+1) prime

fun primelist n = foldl nextprime n

*)

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
fun split xs = foldl (fn (x ,( l , s )) => (s , x :: l )) ( nil , nil ) xs			   
fun partition1 n xs = foldl (fn  (x,(us,vs)) => if  x  < n then (x ::us, vs) else (us,x ::vs) ) ([],[]) xs

				   
(* Aufgabe E.5 *)
fun signum x = case  Int.compare(x,0)   of
		   GREATER => 1
		 | LESS  => ~1
		 | _  => 0 
		
