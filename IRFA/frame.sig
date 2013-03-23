signature FRAME = 
sig 
    type frame
    type access
    val newFrame : {name: Temp.label, forms: bool list} -> frame
    val name : frame -> Temp.label
    val forms : frame -> access list
    val allocLocal : frame -> bool -> access
end
