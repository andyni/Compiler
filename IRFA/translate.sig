signature TRANSLATE = 
sig
    type level
    type access (* not the same as Frame.access *)
    type exp

    val outermost : level
    val newLevel : {parent: level, name: Temp.label, formals: bool list} -> level
    val formals : level -> access list

    val allocLocal : level -> bool -> access					
    val NIL : exp
    val binop : Tree.binop * exp * exp -> exp
    val relop : Tree.relop * exp * exp -> exp 
    val makeLetCall : Tree.stm list * exp -> exp
    val allocateRec : exp * level -> Tree.stm
    val recExp : Tree.stm list * int -> exp
    val seqExp : Tree.stm list * exp -> exp
    val makeVar : access * exp -> Tree.stm
    val assigncall : exp * exp -> exp
    val funcall : exp list * Temp.label * level * level -> exp
    val simpleVar : access * level -> exp
    val fieldVar : exp * Tree.exp -> exp
    val subVar : exp * exp -> exp
    val iftstm : exp * exp -> exp
    val ifstm : exp * exp * exp -> exp
    val getStm : exp -> Tree.stm
    val intexp : int -> exp
    val strexp : string -> exp
    val getCurrOffset : level -> int
end
    
