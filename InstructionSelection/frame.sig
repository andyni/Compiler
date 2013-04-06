signature FRAME = 
sig 
    type frame
    type register
    type access
    datatype frag = PROC of {body: Tree.stm, frame: frame}
		  | STRING of Temp.label * string
    val newFrame : {name: Temp.label, formals: bool list} -> frame
    val name : frame -> Temp.label
    val forms : frame -> access list
    val allocLocal : frame -> bool -> access
    val string : Temp.label * string -> string
    val FP : Temp.temp
    val RV : Temp.temp
    val wordSize: int
    val tempMap : register Temp.Table.table
    val tempToString : Temp.temp -> string
    val specialregs : Temp.temp list
    val argregs : Temp.temp list
    val calleesaves : Temp.temp list
    val callersaves : Temp.temp list
    val exp : access -> Tree.exp -> Tree.exp
    val externalCall : string * Tree.exp list -> Tree.exp
    val procEntryExit1 : frame * Tree.stm -> Tree.stm
    val procEntryExit2 : frame * Assem.instr list -> Assem.instr list

end
