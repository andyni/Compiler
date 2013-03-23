structure Translate : TRANSLATE = 
struct 
  structure Frame : FRAME = MipsFrame
  structure A = Absyn
			      
  type level = {parent: level, frame: Frame.frame}
  type access = level * Frame.access

  val outermost = newLevel (parent = nil, name = Temp.namedlabel("TopLevel"), formals = [])

  fun newLevel {parent = lev, name = label, formals = formlist} = {parent = lev, frame = Frame.newFrame({name = label, formals = formlist})}

  fun formals l = Frame.forms(#frame l)

  fun allocLocal l = Frame.allocLocal(#frame l) 
				     
end
