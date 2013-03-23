structure Translate : TRANSLATE = 
struct 
  structure F : FRAME = MipsFrame
  structure A = Absyn
  
  type level = {parent: level, frame: F.frame}
  type access = level * F.access

  val outermost = newLevel {parent = nil, name = Temp.namedlabel("TopLevel"), formals = []}

  fun newLevel {parent = lev, name = label, formals = formlist} = {parent = lev, frame = F.newFrame({name = label, forms = formlist})}

  fun formals l = let val {parent=_,frame=f} = l
		      val forms = F.forms(f)
		      fun createAccessTuple (a::list) = (l,a)::createAccessTuple(list)
			| createAccessTuple [] = []
		  in
		      createAccessTuple(forms)
		  end

  fun allocLocal l = let val {parent=_,frame=f} = l
		     in
			 (fn(boolean) => (l,F.allocLocal(f)(boolean)))	     
		     end
end
