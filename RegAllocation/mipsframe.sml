structure MipsFrame : FRAME = 
struct

datatype access = InFrame of int 
		| InReg of Temp.temp

type frame = {name: Temp.label, formals: access list, frameoffset: int ref}

datatype frag = PROC of {body: Tree.stm, frame: frame}
	      | STRING of Temp.label * string

type register = string
			       
val wordSize = 4

fun regsEqual(reg1, reg2) = String.compare(reg1,reg2)=EQUAL

fun getLFormals({name,formals=a::forms,frameoffset}) = (forms)

val ZERO = Temp.newspectemp()
val SP = Temp.newspectemp()
val FP = Temp.newspectemp()
val RV = Temp.newspectemp()	 
val RA = Temp.newspectemp()

fun createTempList (0) = []
  | createTempList (length ) = Temp.newspectemp()::createTempList(length-1) 

val specialregs = [ZERO, SP, FP, RV, RA]
val argregs = createTempList(4)
val calleesaves = createTempList(8)
val callersaves = createTempList(10)
val calldefs = callersaves @ [RA, RV] @ argregs

val reglist = specialregs @ argregs @ calleesaves @ callersaves

(* mapping from special temps to their names, nonspecial temps to NONE *)
val tempMap = foldr (fn ((temp, name), table) => Temp.Table.enter(table, temp, name)) 
                    Temp.Table.empty 
                    [(ZERO, "$0"), (SP, "$sp"), (FP, "$fp"), (RV, "$v0"), (RA, "$ra"),
                     (List.nth(argregs,0), "$a0"), (List.nth(argregs,1), "$a1"), (List.nth(argregs,2), "$a2"), (List.nth(argregs,3), "$a3"),
                     (List.nth(calleesaves,0), "$s0"), (List.nth(calleesaves,1), "$s1"), (List.nth(calleesaves,2), "$s2"), (List.nth(calleesaves,3), "$s3"), (List.nth(calleesaves,4), "$s4"), (List.nth(calleesaves,5), "$s5"), (List.nth(calleesaves,6), "$s6"), (List.nth(calleesaves,7), "$s7"),
                     (List.nth(callersaves,0), "$t0"), (List.nth(callersaves,1), "$t1"), (List.nth(callersaves,2), "$t2"), (List.nth(callersaves,3), "$t3"), (List.nth(callersaves,4), "$t4"), (List.nth(callersaves,5), "$t5"), (List.nth(callersaves,6), "$t6"), (List.nth(callersaves,7), "$t7"), (List.nth(callersaves,8), "$t8"), (List.nth(callersaves,9), "$t9")]

(* displays assembly language prior to register allocation *)
fun tempToString(temp) = case Temp.Table.look(tempMap, temp)
							of SOME(register) => register
							 | NONE => Temp.makestring(temp)

(* list of all register names *)
val registers = map tempToString (callersaves @ calleesaves @ specialregs @ argregs )

(* Formats String assembly *)
fun string (lab,s) = Symbol.name(lab) ^ ": \n" ^ ".word " ^ Int.toString(String.size(s)) ^ "\n" ^ ".asciiz \"" ^ s ^ "\"\n" 

fun exp (InFrame(k)) = (fn(expr) => Tree.MEM(Tree.BINOP(Tree.PLUS,expr,Tree.CONST(k))))
  | exp (InReg(register)) =(fn(expr) => Tree.TEMP(register))


fun printacc (InFrame(k)) = print("MEMORY ACCESS : "^Int.toString(k)^"\n")
  | printacc (InReg(register)) =print( "REG ACCESS: "^Temp.makestring(register)^"\n")

fun newFrame f = let 
		val {name=label, formals=formals} = f
		val offset = ref 0
		fun allocate (escape) = if (escape) then ((offset := !offset - wordSize); InFrame(!offset)) 
											else (InReg(Temp.newtemp()))
		val accesses = map allocate formals
	in
	    {name=label, formals=accesses, frameoffset = offset}
	end
		     
fun name (f:frame) = #name f
fun forms (f:frame) = #formals f

(* Allocates variable in frame *)
fun allocLocal f = let val {name = _, formals = _, frameoffset = frameoffset} = f
		   in
		       (fn boolean => case boolean of
					  true => (frameoffset := !frameoffset - wordSize; InFrame(!frameoffset))
					| false => InReg(Temp.newtemp()))
		   end			    		

(* For external calls (malloc, initarray) *)
fun externalCall (s,args) = Tree.CALL(Tree.NAME(Temp.namedlabel s), args) 				    

(* Recursively creates sequence tree *)
fun seq ([])  = Tree.EXP(Tree.CONST 0)
  | seq ([a]) = a 
  | seq (a::l) = Tree.SEQ(a,seq l)

(* Shifts arguments into frame view *)
fun viewshift (frame : frame) = 
	let val formals' = forms frame
	    fun moveArgs (argReg, access) = Tree.MOVE(exp access (Tree.TEMP FP), Tree.TEMP argReg)
		val l = ListPair.zip(argregs, formals')
	in
		seq(map moveArgs l) 
	end

(* Procedure entry/exit *)
fun procEntryExit1 (frame, body) = 
	let fun moveIn (reg1, InFrame(location)) =
	    	Tree.MOVE(Tree.MEM(Tree.BINOP(Tree.PLUS,Tree.TEMP FP,Tree.CONST location)), Tree.TEMP reg1)
		fun moveOut (reg1, InFrame(location)) =
	    	Tree.MOVE(Tree.TEMP reg1,Tree.MEM(Tree.BINOP(Tree.PLUS,Tree.TEMP FP,Tree.CONST location)))
		val save = RA::calleesaves
		val save' = map (fn _ => allocLocal(frame)(true)) save
		val saved = map (moveIn) (ListPair.zip(save, save'))
		val restored = map (moveOut) (ListPair.zip(save, save'))
		val body' = seq(saved @ [viewshift(frame)] @ [body] @ restored)
	in	
    	   	body'
	end

(* For liveness *)
fun procEntryExit2 (frame, body) = 
    body @
    [Assem.OPER{assem="",
            src=[ZERO, RA, SP, FP, RV] @ calleesaves,
            dst=[], jump=SOME([])}]

fun i2s i = if (i<0) then ("-"^Int.toString(~i)) else Int.toString(i)

(* Add final header/epilog to function fragments, setting up frame *)
fun procEntryExit3 ({name, formals, frameoffset}, body) = 
	{prolog = ((Symbol.name name) ^ ": sw $fp, -4($sp)\naddi $fp, $sp, -4 \naddi $sp, $sp,"^i2s((!frameoffset)-4)^"\n"),
     body = body,
     epilog = ("addi $sp, $sp,"^i2s(~(!frameoffset)+4)^"\nlw $fp,0($fp)\njr $ra\n") }

end
