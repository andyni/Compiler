structure MipsFrame : FRAME = 
struct

type frame = {name: Temp.label, formals: bool list, num: int ref}
datatype access = InFrame of int 
		| InReg of Temp.temp
datatype frag = PROC of {body: Tree.stm, frame: frame}
	      | STRING of Temp.label * string
			       
val wordSize = 4
val FP = Temp.newtemp()
val RV = Temp.newtemp()	   

fun exp (InFrame(k)) = (fn(expr) => Tree.MEM(Tree.BINOP(Tree.PLUS,expr,Tree.CONST(k))))
  | exp (InReg(register)) = (fn(expr) => Tree.TEMP(register))

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
		    | createAccessList (n,l) = createAccessList(n+wordSize,InFrame(n)::l)
	      in
		  createAccessList(locals, [])
	      end

fun allocLocal f = let val {name = _, formals = _, num = num} = f
		   in
		       (fn boolean => case boolean of
					  true => (num := !num - wordSize; InFrame(!num))
					| false => InReg(Temp.newtemp()))
		   end			    		

fun externalCall (s,args) = Tree.CALL(Tree.NAME(Temp.namedlabel s), args) 				    
fun procEntryExit1 (frame, body) = body

end
