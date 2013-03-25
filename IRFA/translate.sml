structure Translate : TRANSLATE = 
struct 
  structure F : FRAME = MipsFrame
  structure A = Absyn
  structure T = Tree

  datatype exp = Ex of T.exp
  	       | Nx of T.stm
	       | Cx of Temp.label * Temp.label -> T.stm


  datatype level = Outermost
  	   	 | InnerLevel of {parent: level, frame: F.frame, id : unit ref}

  type access = level * F.access

  val outermost = Outermost

  fun newLevel {parent = lev, name = label, formals = formlist} = InnerLevel{parent = lev, frame = F.newFrame({name = label, formals = formlist}), id = ref ()}

  fun formals l = let val InnerLevel{parent=_,frame=f, id = _} = l
		      val forms = F.forms(f)
		      fun createAccessTuple (a::list) = (l,a)::createAccessTuple(list)
			| createAccessTuple [] = []
		  in
		      createAccessTuple(forms)
		  end

  fun allocLocal (InnerLevel{parent=p, frame=f, id=id}) = (fn(boolean) =>(InnerLevel{parent=p, frame=f, id=id},F.allocLocal(f)(boolean)))	
  

  fun unEx (Ex e) = Ex(e)
    | unEx (Cx genstm) = 
      	   let val r = Temp.newtemp()
	       val t = Temp.newlabel() and f=Temp.newlabel()
	   in
		Ex(T.ESEQ(T.SEQ[T.MOVE(T.TEMP r, T.CONST 1),
			  genstm(t,f),
			  T.LABEL f,
			  T.MOVE(T.TEMP r, T.CONST 0),
			  T.LABEL t], T.TEMP r))
	   end
    | unEx (Nx s) = Ex(T.ESEQ(s,T.CONST 0))

  fun unCx (Cx c) = Cx(c)
    | unCx (Ex e) = let val z = Temp.newlabel()
      	       	    in
       		        Cx(fn(t,f) => T.SEQ[T.CJUMP(T.NE,e,T.CONST 0, t,z),
      	       	  	         T.LABEL z, T.JUMP(T.NAME f, [f])])
		    end
    | unCx (Nx n) = Cx((fn(t,f) => T.JUMP(T.NAME f, [f])))

  fun unNx (Nx n) = Nx(n)
    | unNx (Cx c) = Nx(T.EXP(T.CONST 0))
    | unNx (Ex e) = Nx(T.EXP(e))

  fun simpleVar (access, level) = 
      let val (definitionlevel, acc) = access
	  fun staticLink (InnerLevel(defLevel),InnerLevel(currentLevel)) = 
	      let val {parent = _, frame = _, id = defId } = defLevel 
		  val {parent = currParent, frame = currFrame, id = currId } = currentLevel
     		  val f = #frame currParent
		  val pos = !(#num f)
	      in
		  if (defId = currId) 
		  then T.TEMP(F.FP)
		  (* calculate offset for T.CONST(0) placeholder *)
		  else T.MEM(T.BINOP(T.PLUS, staticLink(InnerLevel(defLevel), currParent), T.CONST(pos)))
	      end
      in
	Ex(F.exp(acc)(staticLink(definitionlevel, level) )) 
      end
end
