structure Translate : TRANSLATE = 
struct 
  structure Frame : FRAME = MipsFrame
  structure A = Absyn
			      
  type level = Frame.frame
  type access = level * Frame.access

 

  fun newLevel {parent = lev, name = label, formals = formlist} = Frame.newFrame({name = label, formals = formlist})

  fun formals t =  let val f =  Frame.forms(t)
      	      	       fun conc (ans, a::l) = conc((t,a)::ans,l)
		           | conc (ans, []) =  ans
		   in
			conc([],f)
		   end	

  fun allocLocal t = (fn boolean => case boolean of
					  true => (t, Frame.allocLocal(t)(true))
					| false => (t, Frame.allocLocal(t)(false)))

  val outermost = newLevel ({parent = nil, name = Temp.newlabel(), formals = []})				     
end
