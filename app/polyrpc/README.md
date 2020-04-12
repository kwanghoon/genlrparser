## PolyRPC: A programming language for developing client-server unified programs

### Main features
- Locative function 
- Locative datatypes
- Locative references

### A quick start to run helloworld.rl

~~~~
  $ git clone https://github.com/kwanghoon/genlrparser
  $ stack build
  $ cat ./app/polyrpc/examples/helloworld.rl
  
  main : Unit = print {client} "Hello World\n"

  $ stack exec -- polyrpc-exe ./app/polyrpc/examples/helloworld.rl
  
  Hello World
  $
~~~~

### Main features

#### Locative functions

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

#### Locative datatypes

- Locative streams: Stream {client} [Int] for client integer streams, Stream {server} [Int] for server integer streams

~~~~
data Stream = {l}. [a].  Nil | Cons a (Unit -l-> Stream {l} [a]) 

;

client_stream : Stream {client} [Int]
   = Cons {client} [Int] 1 (\unit:Unit @client.
      Cons {client} [Int] 2 (\unit:Unit @client.
        Cons {client} [Int] 3 (\unit:Unit @client. Nil {client} [Int])))
;

server_stream : Stream {server} [Int]
   = Cons {server} [Int] 1 (\unit:Unit @server.
      Cons {server} [Int] 2 (\unit:Unit @server.
        Cons {server} [Int] 3 (\unit:Unit @server. Nil {server} [Int])))
~~~~

- Locative stream functions

~~~~
hd_stream
   : {l1 l2}. [a]. (Stream {l1} [a] -l2-> a)
   = {l1 l2}. [a]. \s : Stream {l1} [a] @ l2 .
      case s {
        Cons x xs => x
      }

;

tl_stream
   : {l1 l2}. [a]. (Stream {l1} [a] -l2-> Stream {l1} [a])
   = {l1 l2}. [a]. \s : Stream {l1} [a] @ l2 .
      case s {
        Cons x xs => xs ()
      }
      
;

map_stream
    : {l1 l2 l3}. [a b]. ((a -l2-> b) -l3-> Stream {l1} [a] -l3-> Stream {l1} [b])
    = {l1 l2 l3}. [a b].
      \f:a -l2->b @l3 xs:Stream {l1} [a] @l3 .
        case xs {
	 Nil => Nil {l1} [b];
         Cons y ys => Cons {l1} [b] (f y) (\unit : Unit @ l1 . map_stream {l1 l2 l3} [a b] f (ys ()) )
	}

;

take_stream
    : {l1 l2}. [a]. (Stream {l1} [a] -l2-> Int -l2-> Stream {l1} [a])
    = {l1 l2}. [a].
      \s : Stream {l1} [a] @ l2
       n : Int @ l2 .
        case s {
	  Nil => Nil {l1} [a];
	  Cons y ys =>
	    if n <= 0
	    then Nil {l1} [a]
	    else Cons {l1} [a] y (\unit : Unit @ l1 . take_stream {l1 l2} [a] (ys ()) (n-1))
	}
~~~~

- Conversion of server streams into client streams

~~~~
serverToclient
  : Stream {server} [Int] -client-> Stream {client} [Int]
  = \server_stream : Stream {server} [Int] @ client .
      case server_stream {
        Nil => Nil {client} [Int];
	Cons y ys =>
	  Cons {client} [Int] y
	    ( \unit:Unit@client. serverToclient (ys ()) )
      }
;

main : Stream {client} [Int] = serverToclient server_stream1
~~~~

#### Locative references

- Create a server reference to "one two three" from the client, update it with "four five six", and then read the referenced string. 
~~~~
x : Ref {server} [String] = ref {server} [String] "one two three" ;

y : Unit = x := {server} [String] "four five six" ;

z : Unit = print {client} ( ! {server} [String] x )
~~~~


