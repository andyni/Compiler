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

  val NIL = Ex(T.CONST 0)

  fun newLevel {parent = lev, name = label, formals = formlist} = InnerLevel{parent = lev, frame = F.newFrame({name = label, formals = formlist}), id = ref ()}

  fun formals l = let val InnerLevel{parent=_,frame=f, id = _} = l
		      val forms = F.forms(f)
		      fun createAccessTuple (a::list) = (l,a)::createAccessTuple(list)
			| createAccessTuple [] = []
		  in
		      createAccessTuple(forms)
		  end

  fun allocLocal (InnerLevel{parent=p, frame=f, id=id}) = (fn(boolean) =>(InnerLevel{parent=p, frame=f, id=id},F.allocLocal(f)(boolean)))	

  fun unEx (Ex e) = (e)
    | unEx (Cx genstm) = 
      let val r = Temp.newtemp()
	  val t = Temp.newlabel() and f=Temp.newlabel()
      in
	  (T.ESEQ(T.SEQ[T.MOVE(T.TEMP r, T.CONST 1),
			genstm(t,f),
			T.LABEL f,
			T.MOVE(T.TEMP r, T.CONST 0),
			T.LABEL t], T.TEMP r))
      end
    | unEx (Nx s) = (T.ESEQ(s,T.CONST 0))
			
  fun unCx (Cx c) = (c)
    | unCx (Ex e) = let val z = Temp.newlabel()
      	       	    in
       		        (fn(t,f) => T.SEQ[T.CJUMP(T.NE,e,T.CONST 0, t,z),
      	       	  			  T.LABEL z, T.JUMP(T.NAME f, [f])])
		    end
    | unCx (Nx n) = ((fn(t,f) => T.JUMP(T.NAME f, [f])))
			
  fun unNx (Nx n) = (n)
    | unNx (Cx c) = (T.EXP(T.CONST 0))
    | unNx (Ex e) = (T.EXP(e))

  fun binop (oper,exp1,exp2) = Ex(T.BINOP(oper,unEx(exp1),unEx(exp2)))

  fun relop (oper,exp1,exp2) = Cx(fn(t,f)=>T.CJUMP(oper,unEx(exp1),unEx(exp2),t,f))

  fun ifstm (test,exp1,exp2) = let val r = Temp.newtemp()
				   val t = Temp.newlabel() and f=Temp.newlabel() and join=Temp.newlabel()
			       in
				   Ex(T.ESEQ(T.SEQ[
						  unCx(test)(t,f),
						  T.LABEL t,
						  T.MOVE(T.TEMP r, unEx(exp1)),
						  T.JUMP(T.NAME join,[join]),
						  T.LABEL f,
						  T.MOVE(T.TEMP r, unEx(exp2)),
						  T.JUMP(T.NAME join,[join]),
						  T.LABEL join], T.TEMP r))
			       end
	
  fun iftstm (test,exp1) = let val t = Temp.newlabel() and f = Temp.newlabel()
      			   in
			       Nx(T.SEQ[unCx(test)(t,f),
					T.LABEL t,
					T.EXP (unEx(exp1)),
					T.JUMP (T.NAME f, [f]),
					T.LABEL f])
			   end
			       
  fun fieldVar (exp, offset) = Ex(T.MEM(T.BINOP(T.PLUS, unEx(exp), T.CONST(offset*F.wordSize))))
  fun subVar (exp, offset) = Ex(T.MEM(T.BINOP(T.PLUS, unEx(exp), T.BINOP(T.MUL, T.CONST(F.wordSize), unEx(offset)))))    
      
  fun staticLink (InnerLevel(defLevel),InnerLevel(currentLevel)) = 
      let val {parent = _, frame = _, id = defId } = defLevel 
	  val {parent = currParent, frame = currFrame, id = currId} = currentLevel
	  val InnerLevel(l) = currParent
     	  val f = #frame l
	  val pos = !(#num f)
      in
	  if (defId = currId) 
	  then T.TEMP(F.FP)
	  else T.MEM(T.BINOP(T.PLUS, staticLink(InnerLevel(defLevel), currParent), T.CONST(pos)))
      end

  fun simpleVar (access, level) = 
      let val (definitionlevel, acc) = access
      in	
	  Ex(F.exp(acc)(staticLink(definitionlevel, level) )) 
      end
      
<<<<<<< HEAD
      fun makeLetCall(exparr,exp2) = Ex(T.ESEQ(T.SEQ(exparr),unEx(exp2))) 

      fun allocateRec(size) = 
			      in
				F.externalCall("malloc",T.CONST size*F.wordsize)
			      end
      fun recExp(exparr, recpointer) =
      	  		 	 let val r = Temp.newtemp()
				 in
				       	 Ex(T.ESEQ(T.SEQ(T.MOVE(T.TEMP r, recpointer)::movevars(loc),T.MEM(T.BINOP(T.PLUS,T.TEMP(F.FP),T.TEMP r))))

      fun seqExp(exparr, exp) = Ex(T.ESEQ(T.SEQ(exparr),unEx(exp)))
=======
  fun makeLetCall (exparr,exp2) = Ex(T.ESEQ(T.SEQ(exparr),unEx(exp2))) 
				   
  fun allocateRec (initval, level) = 
      let val (lev, access) =  allocLocal(level)(true)
      in
	  T.MOVE(F.exp(access)(T.TEMP F.FP),unEx(initval))
      end
  fun recExp (exparr,currnum) =
      Ex(T.ESEQ(T.SEQ(exparr),T.MEM(T.BINOP(T.PLUS,T.TEMP(F.FP),T.CONST currnum))))
>>>>>>> 8c4c85d98e778d0e1caf7c29f139f20a0b924bcb

  fun seqExp (exparr, exp) = Ex(T.ESEQ(T.SEQ(exparr),unEx(exp)))
			      
  fun makeVar (access, initval) = 
      let val (lev,acc) = access
      in
	  T.MOVE(F.exp(acc)(T.TEMP F.FP), unEx(initval))
      end
      	  		  
  fun getStm (exp) = unNx(exp)
  fun getEx (exp) = unEx(exp)

  fun intexp (a) = Ex(T.CONST(a))
  fun strexp (s) = let val lab = Temp.newlabel()
      		   in
		       Ex(T.NAME(lab))
		   end
		      
  fun assigncall (exp1, exp2) = Nx(T.MOVE(unEx(exp1), unEx(exp2)))       
				 
  fun funcall (args,label,level,mylevel) = 
      let val InnerLevel({parent=topParent, frame=_, id=_}) = mylevel
      in
	  Ex(T.CALL(T.NAME label, staticLink(topParent,level)::(map unEx args)))
      end
	  
  fun breakexp breaklabel = Nx(T.JUMP(T.NAME(breaklabel),[breaklabel]))
			      
  fun whileexp (condition, body, break) = 
      let val testlabel = Temp.newlabel()
	  val bodylabel = Temp.newlabel()
	  val donelabel = Temp.newlabel()
      in
	  Nx(T.SEQ[T.LABEL(testlabel),
		   unCx(condition)(testlabel, donelabel),
		   T.LABEL(bodylabel),
		   unNx(body),
		   T.JUMP(T.NAME(testlabel), [testlabel]),
		   T.LABEL(donelabel),
		   T.LABEL(break)
	  ])
      end
<<<<<<< HEAD
      
=======

  fun forexp (var, lo, hi, body, break) = 
      let val bodylabel = Temp.newlabel()
	  val testlabel = Temp.newlabel()
	  val var' = unEx var
	  val lo' = unEx lo
	  val hi' = unEx hi
      in
	  Nx(T.SEQ[T.MOVE(var', lo'),
		   T.CJUMP(T.LE, var', hi', bodylabel, break),
		   T.LABEL(bodylabel),
		   unNx(body),
		   T.CJUMP(T.LT, var', hi', testlabel, break),
		   T.LABEL(testlabel),
		   T.MOVE(var', T.BINOP(T.PLUS, var', T.CONST(1))),
		   T.JUMP(T.NAME bodylabel, [bodylabel]), 
		   T.LABEL(break)
	    ])
      end

  fun getCurrOffset (InnerLevel{parent,frame,id}) = !(#num(frame))
>>>>>>> 8c4c85d98e778d0e1caf7c29f139f20a0b924bcb
end
