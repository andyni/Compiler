structure MipsFrame : FRAME = 
struct

type frame = {name: Temp.label, formals: bool list, num: int ref}
type register = string

datatype access = InFrame of int 
		| InReg of Temp.temp
datatype frag = PROC of {body: Tree.stm, frame: frame}
	      | STRING of Temp.label * string
			       
val wordSize = 4

fun regsEqual(reg1, reg2) = String.compare(reg1,reg2)=EQUAL

val ZERO = Temp.newtemp()
val SP = Temp.newtemp()
val FP = Temp.newtemp()
val RV = Temp.newtemp()	 
val RA = Temp.newtemp()

(* mapping from special temps to their names, nonspecial temps to NONE *)
val tempMap = foldr (fn ((temp, name), table) => Temp.Table.enter(table, temp, name)) 
                    Temp.Table.empty 
                    [(ZERO, "$zero"), (SP, "$sp"), (FP, "$fp"), (RV, "$rv"), (RA, "$ra")]

(* displays assembly language prior to register allocation *)
fun tempToString(temp) = case Temp.Table.look(tempMap, temp)
							of SOME(register) => register
							 | NONE => Temp.makestring(temp)

val specialregs = [ZERO, SP, FP, RV, RA]

fun createTempList (0, l) = l
  | createTempList (length, l) = createTempList(length-1, Temp.newtemp()::l) 

val argregs = createTempList(4,[])
val calleesaves = createTempList(8,[])
val callersaves = createTempList(10,[])
val calldefs = callersaves @ [RA, RV]

fun string (lab,s) = Symbol.name(lab) ^ ": .asciiz \"" ^ s ^ "\"\n" 

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

(* Procedure entry/exit *)
fun procEntryExit1 (frame, body) = 
    body
fun procEntryExit2 (frame, body) = 
    body @
    [Assem.OPER{assem="",
            src=[ZERO, RA, SP] @ calleesaves,
            dst=[], jump=SOME([])}]
fun procEntryExit3 ({name, params, locals}, body) = 
	{prolog = "PROCEDURE " ^ (Symbol.name name) ^ "\n",
     body = body,
     epilog = "END " ^ (Symbol.name name) ^ "\n"}

end
