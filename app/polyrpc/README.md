## PolyRPC: A programming language for developing client-server unified programs

### Main features
- Locative function types
- Locative datatypes
- Locative reference types

### (1) How to setup and run helloworld.rl

~~~~
  $ git clone https://github.com/kwanghoon/genlrparser
  $ stack build
  $ cat ./app/polyrpc/examples/helloworld.rl
  
  main : Unit = print {client} "Hello World\n"

  $ stack exec -- polyrpc-exe ./app/polyrpc/examples/helloworld.rl
  
  Hello World
  $
~~~~

### (2) Examples for the main features

- A client identity function 
~~~~
f : Int -client-> Int
  = \x : Int @ client . x
~~~~

- A polymorphic identity function 
~~~~
id : {l}. [a]. (a -l-> a)
   = {l}. [a].
     \x : a @ l. x
~~~~

- A server identity function 
~~~~
g : Int-server-> Int
  = id {server} [Int]
~~~~


- A client factorial function
~~~~
fac : Int -client-> Int
    = \n : Int @ client .
        if n <= 0 then 0
	else n * (fac (n-1))

;

main : Int = fac 3
~~~~

- A polymorphic map function: An example of running a server map function 
with the application of a client function to elements of a list
~~~~

data List = [a]. Nil | Cons a (List [a])  ;

map : {l1 l2}. [a b]. ((a -l1-> b) -l2-> List [a] -l2-> List [b])
    = {l1 l2}. [a b].
      \f:a -l1->b @l2 xs:List [a] @l2 .
        case xs {
	 Nil => Nil [b];
         Cons y ys => Cons [b] (f y) (map {l1 l2} [a b] f ys)
	};
	
	
main : List [Int] 
     = map {client server} [Int Int]
         (\x:Int @client . x + 5) (Cons [Int] 1 (Cons [Int] 2 (Cons [Int] 3 (Nil [Int]))))
~~~~
