
x : Ref <String @ server> = ref {server} [String] "one two three" ;

y : Unit = x := {server client} [String] "four five six" ;

z : Unit = print {client} ( ! {server client} [String] x )

