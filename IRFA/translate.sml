structure Translate : TRANSLATE = 
struct 
  structure F : FRAME = MipsFrame
  structure A = Absyn

  datatype level = Outermost
  	   	 | InnerLevel of {parent: level, frame: F.frame, id : unit ref}

  type access = level * F.access

  val outermost = Outermost

  fun newLevel {parent = lev, name = label, formals = formlist} = InnerLevel{parent = lev, frame = F.newFrame({name = label, formals = formlist}), id = ref ()}
  fun formals l =     let val InnerLevel{parent=_,frame=f, id = _} = l
		      val forms = F.forms(f)
		      fun createAccessTuple (a::list) = (l,a)::createAccessTuple(list)
			| createAccessTuple [] = []
		  in
		      createAccessTuple(forms)
		  end
  fun allocLocal (InnerLevel{parent=p, frame=f, id=id}) = 
			 (fn(boolean) => (InnerLevel{parent=p, frame=f, id=id},F.allocLocal(f)(boolean)))	
end
