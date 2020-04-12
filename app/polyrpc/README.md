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
  = id {server}
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

- A polymorphic identity function 
~~~~
id : {l}. [a]. (a -l-> a)
   = {l}. [a].
     \x : a @ l. x
~~~~
