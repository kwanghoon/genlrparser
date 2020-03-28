
////////////////////////////////////////////////////////////////////////////////
//                                                                            //
// Streams                                                                    //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

data Stream = {l}. [a].  Nil | Cons a (Unit -l-> Stream {l} [a]) 

;

hd_stream
   : {l}. [a]. (Stream {client} [a] -l-> a)
   = {l}. [a]. \s : Stream {client} [a] @ l .
      case s {
        Cons x xs => x
      }

;

tl_stream
   : {l}. [a]. (Stream {client} [a] -l-> Stream {client} [a])
   = {l}. [a]. \s : Stream {client} [a] @ l .
      case s {
        Cons x xs => xs ()
      }
      
;

map_stream
    : {l1 l2 l3}. [a b]. ((a -l1-> b) -l2-> Stream {client} [a] -l3-> Stream {client} [b])
    = {l1 l2 l3}. [a b].
      \f:a -l1->b @l2 xs:Stream {client} [a] @l3 .
        case xs {
	 Nil => Nil [b];
         Cons y ys => Cons [b] (f y) (\unit : Unit @ client . map_stream {l1 l2 l3} [a b] f (ys ()) )
	}


;

take_stream
    : {l1 l2}. [a]. (Stream {client} [a] -l1-> Int -l2-> Stream {client} [a])
    = {l1 l2}. [a].
      \s : Stream {client} [a] @ l1
       n : Int @ l2 .
        case s {
	  Nil => Nil [a];
	  Cons y ys =>
	    if n >= 0
	    then Nil [a]
	    else Cons [a] y (\unit : Unit @ client . take_stream {l1 l2} [a] (ys ()) (n-1))
	}
;

////////////////////////////////////////////////////////////////////////////////
// main
////////////////////////////////////////////////////////////////////////////////

s1 : Stream {client} [Int]
   = Cons [Int] 1 (\unit:Unit @client.
      Cons [Int] 2 (\unit:Unit @client.
        Cons [Int] 3 (\unit:Unit @client. Nil [Int])))
;

main : Int
     = hd_stream {client} [Int]
        (tl_stream {client} [Int]
	  (take_stream {client client} [Int]  
	    (map_stream {client client client} [Int Int]
	       (\x:Int@client.x+1) s1)
	    2))
	    

