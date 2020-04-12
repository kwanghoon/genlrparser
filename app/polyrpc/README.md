## PolyRPC: A programming language for developing client-server unified programs

Main features
- Locative functions, such as Int -client->String, Int -server->String, {l}. Int -l->String
- Locative datatypes, such as Streams {client} [Int] or Streams {server} [Int]
- Locative reference types, Ref {client} [String] or Ref {server} [String]

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

### (2) 
