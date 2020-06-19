
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

      id : [a]. a -client-> a
         = [a]. \x : a @ client. x
    }
      let {

        w : [b]. b -client-> b
	  = [b]. \z : b @ client. id [b] z 

      } w [Int] 3
      end
      
    end

