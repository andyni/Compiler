signature TRANSLATE = 
sig
    type level
    type access (* not the same as Frame.access *)

    val outermost : level
    val newLevel : {parent: level, name: Temp.label, formals: bool list} -> level
    val formals : level -> access list
    val allocLocal : level -> bool -> access
end

structure Translate : TRANSLATE = 
struct
type access = level * Frame.access
end
