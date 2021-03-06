signature TRANSLATE = 
sig
    type level
    type access (* not the same as Frame.access *)
    type exp
    type frag

    val outermost : level
    val newLevel : {parent: level, name: Temp.label, formals: bool list} -> level
    val formals : level -> access list
    val allocLocal : level -> bool -> access					
    
    val NIL : exp
    val binop : Tree.binop * exp * exp -> exp
    val relop : Tree.relop * exp * exp -> exp 
    val makeLetCall : Tree.stm list * exp -> exp
    val strcmp : exp * exp -> exp
    val nstrcmp : exp * exp -> exp
    val recExp : exp list -> exp
    val allocateArr : exp * exp -> exp
    val seqExp : Tree.stm list * exp -> exp
    val makeVar : access * exp -> Tree.stm
    val assigncall : exp * exp -> exp
    val funcall : exp list * Temp.label * level * level -> exp
    val simpleVar : access * level -> exp
    val fieldVar : exp * int -> exp
    val subVar : exp * exp -> exp
    val iftstm : exp * exp -> exp
    val ifstm : exp * exp * exp -> exp
    val getStm : exp -> Tree.stm
    val intexp : int -> exp
    val strexp : string -> exp
    val whileexp : exp * exp * Temp.label -> exp
    val forexp : exp * exp * exp * exp * Temp.label -> exp
    val breakexp : Temp.label -> exp
    val functiondec : level * exp -> unit

    val procEntryExit : {level: level, body: exp} -> unit
    val getResult : unit -> frag list
    val resetfraglist : unit -> unit
end
    
