structure Translate : TRANSLATE = 
struct 
  structure F : FRAME = MipsFrame
  structure A = Absyn
  structure T = Tree

  datatype level = Outermost
  	   	 | InnerLevel of {parent: level, frame: F.frame, id : unit ref}

  datatype exp = Ex of T.exp
	       | Nx of T.stm
	       | Cx of Temp.label * Temp.label -> T.stm

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

  fun allocLocal (InnerLevel{parent=p, frame=f, id=id}) = 
			 (fn(boolean) => (InnerLevel{parent=p, frame=f, id=id},F.allocLocal(f)(boolean)))	

  fun seq [a] = a
    | seq (a::l) = T.SEQ(a, seq l)
  
  fun unEx (Ex e) = e
    | unEx (Cx genstm) = 
      let val r = Temp.newtemp()
	  val t = Temp.newlabel() and f = Temp.newlabel()
      in 
	  T.ESEQ(seq[T.MOVE(T.TEMP r, T.CONST 1),
		     genstm(t,f),
		     T.LABEL f,
		     T.MOVE(T.TEMP r, T.CONST 0),
		     T.LABEL t],
		 T.TEMP r)
      end
    | unEx (Nx s) = T.ESEQ(s,T.CONST 0)
			  
  fun unNx (Ex e) = T.EXP(e)
    | unNx (Cx genstm) = 
      let val t = Temp.newlabel() and f = Temp.newlabel()
      in
	  genstm(t,f)
      end
    | unNx (Nx s) = s

  fun simpleVar (access, level) = 
      let val (definitionlevel, acc) = access
	  fun staticLink (InnerLevel(defLevel),InnerLevel(currentLevel)) = 
	      let val {parent = _, frame = _, id = defId } = defLevel 
		  val {parent = currParent, frame = currFrame, id = currId } = currentLevel
	      in
		  if (defId = currId) 
		  then T.TEMP(F.FP)
		  (* calculate offset for T.CONST(0) placeholder *)
		  else T.MEM(T.BINOP(T.PLUS, staticLink(InnerLevel(defLevel), currParent), T.CONST(0)))
	      end
      in
	Ex(F.exp(acc)(staticLink(definitionlevel, level) )) 
      end

end
