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

structure Frame : FRAME = MipsFrame


type level = {parent: level, frame: Frame.frame}
type access = level * Frame.access

val outermost = newLevel (parent = nil, name = Temp.namedlabel("TopLevel"), formals = [])

fun newLevel {parent = lev, name = label, formals = formlist} = {parent = lev, frame = Frame.newFrame({name = label, formals = formlist})}

fun formals l = Frame.forms(#frame l)

fun allocLocal l = Frame.allocLocal(#frame l) 

end
