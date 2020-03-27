
////////////////////////////////////////////////////////////////////////////////
//                                                                            //
// Streams                                                                    //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

data Stream a = { Nil | Cons a (Unit -client-> Stream<a>) }

;

hd_stream
   : {l}. [a]. (Stream<a> -l-> a)
   = {l}. [a]. \s : Stream<a> @ l .
      case s {
        Cons x xs => x
      }

;

tl_stream
   : {l}. [a]. (Stream<a> -l-> Stream<a>)
   = {l}. [a]. \s : Stream<a> @ l .
      case s {
        Cons x xs => xs ()
      }
      
;

map_stream
    : {l1 l2 l3}. [a b]. ((a -l1-> b) -l2-> Stream<a> -l3-> Stream<b>)
    = {l1 l2 l3}. [a b].
      \f:a -l1->b @l2 xs:Stream<a> @l3 .
        case xs {
	 Nil => Nil [b];
         Cons y ys => Cons [b] (f y) (\unit : Unit @ client . map_stream {l1 l2 l3} [a b] f (ys ()) )
	}


;

take_stream
    : {l1 l2}. [a]. (Stream<a> -l1-> Int -l2-> Stream<a>)
    = {l1 l2}. [a].
      \s : Stream<a> @ l1
       n : Int @ l2 .
        case s {
	  Nil => Nil [a];
	  Cons y ys =>
	    if n >= 0
	    then Nil [a]
	    else Cons [a] y (\unit : Unit @ client . take_stream {l1 l2} [a] (ys ()) (n-1))
	}

