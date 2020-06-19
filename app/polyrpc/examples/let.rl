
x : Int
  = let {

      id : [a]. a -client-> a
         = [a]. \x : a @ client. x
    }
      let {

        y : Int = id [Int] 3

      } y
      end
      
    end ;


z : Int
  = let {

      id : {l}. [a]. a -l-> a
         = {l}. [a]. \x : a @ l. x
    }
      let {

        w : {l}. [b]. b -l-> b
	  = {l}. [b]. \z : b @ l. id {l} [b] z 

      } w {client} [Int] 3
      end
      
    end

