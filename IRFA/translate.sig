signature TRANSLATE = 
sig
    type level
    type access (* not the same as Frame.access *)
    type exp

    val outermost : level
    val newLevel : {parent: level, name: Temp.label, formals: bool list} -> level
    val formals : level -> access list

    val allocLocal : level -> bool -> access					

    val NIL : unit -> exp
    val simpleVar : access * level -> exp
    val fieldVar : exp * int -> exp
    val subscriptVar : exp * exp -> exp
    val intexp : int -> exp
    val stringexp : string -> exp
    val assignexp : exp * exp -> exp
    val breakexp : Temp.label -> exp
    val ifthenexp : exp * exp -> exp
    val ifthenelseexp: exp * exp * exp -> exp
    val binop : Tree.binop * exp * exp -> exp
    val relop : Tree.relop * exp * exp -> exp
end
    
