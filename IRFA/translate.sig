signature TRANSLATE = 
sig
    type level
    type access (* not the same as Frame.access *)
    type exp

    val outermost : level
    val newLevel : {parent: level, name: Temp.label, formals: bool list} -> level
    val formals : level -> access list
    val allocLocal : level -> bool -> access					

    val simpleVar : access * level -> exp
end
    
