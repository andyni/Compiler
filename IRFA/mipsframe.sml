structure MipsFrame : FRAME = 
struct

type frame = {name: Temp.label, formals: bool list, num: int ref}
datatype access = InFrame of int 
		| InReg of Temp.temp
			       
val wordsize = 4
	   
fun newFrame f = let val {name=label, formals=formals} = f
		 in
		     {name=label, formals=formals, num = ref 0}
		 end
		     
fun name f = let val {name = label, formals = _, num = _} = f
	     in
		 label
	     end
		 
fun forms f = let val {name = _, formals = _, num = num} = f
		  val locals = !num
		  fun createAccessList (0,l) = l
		    | createAccessList (n,l) = createAccessList(n+wordsize,InFrame(n)::l)
	      in
		  createAccessList(locals, [])
	      end

fun allocLocal f = let val {name = _, formals = _, num = num} = f
		   in
		       (fn boolean => case boolean of
					  true => (num := !num - wordsize; InFrame(!num))
					| false => InReg(Temp.newtemp()))
		   end			    						    
end
