datatype temp = D of int * bool
datatype answers = A of int | B of int * bool | C of string

fun check (f) = 
            case f of
                A (x) =>   let (* fn (x:int):int => x PLUS 3; *)
                                val x1 = x            
                            in
                                (print(Int.toString(x1)))    
                            end 
            |   B (d) =>   let
                                val (x1,x2) = d       
                            in
                                (print(Int.toString(x1)))
                            end
            |   C (x) =>    let 
                                val x1 = x
                            in
                                (print(x1))
                            end