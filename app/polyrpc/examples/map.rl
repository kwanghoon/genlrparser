
data List a = { Nil | Cons a (List <a>) } ;

map : {l1 l2 l3}. [a b]. (a -l1-> b) -l2-> List<a> -l3-> List<b>
    = {l1 l2 l3}. [a b].
      \f @l2 xs @l3 .
        case xs {
	 Nil => Nil;
         Cons y ys => Cons (f y) (map f ys)
	};
	
	
main : List<Int>
     = map {client client client} [Int Int]
         (\x @client . x) (Cons 1 (Cons 2 (Cons 3 Nil)))
	 