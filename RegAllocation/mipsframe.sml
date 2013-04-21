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

fun getLFormals({name,formals=a::forms,frameoffset}) = (print ("FORMSLENGTH:"^(Int.toString(length(forms))));forms)

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
val calldefs = callersaves @ [RA, RV]

val reglist = specialregs @ argregs @ calleesaves @ callersaves

(* mapping from special temps to their names, nonspecial temps to NONE *)
val tempMap = foldr (fn ((temp, name), table) => Temp.Table.enter(table, temp, name)) 
                    Temp.Table.empty 
                    [(ZERO, "$zero"), (SP, "$sp"), (FP, "$fp"), (RV, "$rv"), (RA, "$ra"),
                     (List.nth(argregs,0), "$a0"), (List.nth(argregs,1), "$a1"), (List.nth(argregs,2), "$a2"), (List.nth(argregs,3), "$a3"),
                     (List.nth(calleesaves,0), "$s0"), (List.nth(calleesaves,1), "$s1"), (List.nth(calleesaves,2), "$s2"), (List.nth(calleesaves,3), "$s3"), (List.nth(calleesaves,4), "$s4"), (List.nth(calleesaves,5), "$s5"), (List.nth(calleesaves,6), "$s6"), (List.nth(calleesaves,7), "$s7"),
                     (List.nth(callersaves,0), "$t0"), (List.nth(callersaves,1), "$t1"), (List.nth(callersaves,2), "$t2"), (List.nth(callersaves,3), "$t3"), (List.nth(callersaves,4), "$t4"), (List.nth(callersaves,5), "$t5"), (List.nth(callersaves,6), "$t6"), (List.nth(callersaves,7), "$t7"), (List.nth(callersaves,8), "$t8"), (List.nth(callersaves,9), "$t9")]

(* displays assembly language prior to register allocation *)
fun tempToString(temp) = case Temp.Table.look(tempMap, temp)
							of SOME(register) => register
							 | NONE => Temp.makestring(temp)

(* list of all register names *)
val registers = map tempToString (calleesaves @ callersaves)


fun string (lab,s) = Symbol.name(lab) ^ ": .asciiz \"" ^ s ^ "\"\n" 

fun exp (InFrame(k)) = (fn(expr) => Tree.MEM(Tree.BINOP(Tree.PLUS,expr,Tree.CONST(k))))
  | exp (InReg(register)) =(fn(expr) => Tree.TEMP(register))


fun printacc (InFrame(k)) = print("MEMORY ACCESS : "^Int.toString(k)^"\n")
  | printacc (InReg(register)) =print( "REG ACCESS: "^Temp.makestring(register)^"\n")

fun newFrame f = let 
		val {name=label, formals=formals} = f
		val _ = print  (Symbol.name(label)^"FORMfSLENGTH:"^(Int.toString(length(formals))))
		val offset = ref 0
		fun allocate (escape) = if (escape) then ((offset := !offset - wordSize); InFrame(!offset)) 
											else (InReg(Temp.newtemp()))
		val accesses = map allocate formals
		val _ = print  (Symbol.name(label)^"FORMfaSLENGTH:"^(Int.toString(length(accesses))))
	in
	    {name=label, formals=accesses, frameoffset = offset}
	end
		     
fun name (f:frame) = #name f
fun forms (f:frame) = #formals f

fun allocLocal f = let val {name = _, formals = _, frameoffset = frameoffset} = f
		   in
		       (fn boolean => case boolean of
					  true => (frameoffset := !frameoffset - wordSize; InFrame(!frameoffset))
					| false => InReg(Temp.newtemp()))
		   end			    		

fun externalCall (s,args) = Tree.CALL(Tree.NAME(Temp.namedlabel s), args) 				    

(* Recursively creates sequence tree *)
fun seq ([])  = Tree.EXP(Tree.CONST 0)
  | seq ([a]) = a 
  | seq (a::l) = Tree.SEQ(a,seq l)

fun viewshift (frame : frame) = 
	let val formals' = forms frame
	    fun moveArgs (argReg, access) = Tree.MOVE(exp access (Tree.TEMP FP), Tree.TEMP argReg)
		val l = ListPair.zip(argregs, formals')
	in
		seq(map moveArgs l) 
	end

(* Procedure entry/exit *)
fun procEntryExit1 (frame, body) = 
	let fun move (reg1, reg2) = Tree.MOVE(Tree.TEMP reg1, Tree.TEMP reg2)
		val save = RA::calleesaves
		val save' = map (fn _ => Temp.newtemp()) save
		val saved = map (move) (ListPair.zip(save', save))
		val restored = map (move) (ListPair.zip(save, save'))
		val body' = seq(saved @ [body] @ restored)
	in	
    	Tree.SEQ(viewshift(frame), body)
	end

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
