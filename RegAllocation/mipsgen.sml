structure MipsGen : CODEGEN =  
struct
  structure Frame : FRAME = MipsFrame
  structure A = Assem
  structure Tr = Tree

  fun codegen (frame) (stm: Tr.stm) : A.instr list = 
  	let 
  		val ilist = ref (nil : A.instr list)
  		fun emit x = (ilist := x::(!ilist))
  		fun result(gen) = let val t = Temp.newtemp() in gen t; t
  		end

      (* Translates binops into corresponding assembly command*)
      fun getRelop relop = case relop of 
                                Tr.EQ => "beq"
                              | Tr.NE => "bne"
                              | Tr.LT => "blt"
                              | Tr.GT => "bgt"
                              | Tr.LE => "ble"
                              | Tr.GE => "bge"
                              | Tr.ULT => "blt"
                              | Tr.ULE => "ble"
                              | Tr.UGT => "bgt"
                              | Tr.UGE => "bge"

  		fun munchStm (Tr.SEQ(stm1, stm2)) = (munchStm stm1; munchStm stm2) 

        | munchStm (Tr.LABEL lab) = 
            emit(A.LABEL{assem=Symbol.name lab ^ ": ",
                         lab=lab}) 

        (* JUMP *)
        | munchStm (Tr.JUMP (Tr.NAME(name), labellist)) =
            emit(A.OPER{assem="j `j0 \n",
                        src=[], dst=[], jump=SOME(labellist)}) 

        | munchStm (Tr.JUMP (Tr.CONST(i), labellist)) =
            emit(A.OPER{assem="j `j0 \n",
                        src=[], dst=[], jump=SOME(labellist)}) 

        | munchStm (Tr.JUMP (e, labellist)) =
            emit(A.OPER{assem="jr `s0 \n",
                        src=[munchExp e], dst=[], jump=SOME(labellist)}) 

        (* CJUMP *)
        | munchStm (Tr.CJUMP (relop, Tr.CONST(i1), Tr.CONST(i2), lab1, lab2)) =
            emit(A.OPER{assem= getRelop(relop) ^ " " ^ Int.toString(i1) ^ ", " ^ Int.toString(i2) ^ ", " ^ Symbol.name(lab1)^" \n",
                        src=[], dst=[], jump=SOME([lab1, lab2])})

        | munchStm (Tr.CJUMP (relop, Tr.CONST(i), e2, lab1, lab2)) =
            emit(A.OPER{assem= getRelop(relop) ^ " " ^ Int.toString(i) ^ ", `s0, " ^ Symbol.name(lab1)^" \n",
                        src=[munchExp e2], dst=[], jump=SOME([lab1, lab2])})

        | munchStm (Tr.CJUMP (relop, e1, Tr.CONST(i), lab1, lab2)) =
            emit(A.OPER{assem= getRelop(relop) ^ " `s0, " ^ Int.toString(i) ^ ", " ^ Symbol.name(lab1)^ " \n",
                        src=[munchExp e1], dst=[], jump=SOME([lab1, lab2])})

        | munchStm (Tr.CJUMP (relop, e1, e2, lab1, lab2)) =
            emit(A.OPER{assem= getRelop(relop) ^ " `s0, `s1, " ^ Symbol.name(lab1)^" \n",
                        src=[munchExp e1, munchExp e2], dst=[], jump=SOME([lab1, lab2])})

        (* MOVE *)
  		  | munchStm (Tr.MOVE(Tr.MEM(Tr.BINOP(Tr.PLUS,e1,Tr.CONST i)),e2)) = 
            emit(A.OPER{assem="sw `s1," ^ Int.toString(i) ^ "(`s0) \n",
                        src=[munchExp e1, munchExp e2], dst=[], jump=NONE})

        | munchStm (Tr.MOVE(Tr.MEM(Tr.BINOP(Tr.PLUS,Tr.CONST i,e1)),e2)) = 
            emit(A.OPER{assem="sw `s1," ^ Int.toString(i) ^ "(`s0) \n",
                        src=[munchExp e1, munchExp e2], dst=[], jump=NONE})

        | munchStm (Tr.MOVE(Tr.MEM(Tr.CONST i),e2)) =
            emit(A.OPER{assem="sw `s0," ^ Int.toString(i) ^ "(`r0) \n",
                        src=[munchExp e2], dst=[], jump=NONE})

        | munchStm (Tr.MOVE(Tr.MEM(e1),e2)) =
            emit(A.OPER{assem="sw `s1, 0(`s0) \n",
                        src=[munchExp e1, munchExp e2], dst=[], jump=NONE})

	      | munchStm (Tr.MOVE(Tr.TEMP i, Tr.TEMP k)) =
	          emit(A.MOVE{assem="addi `d0,`s0,0 \n", src=k, dst=i})

	      | munchStm (Tr.MOVE(Tr.TEMP i, Tr.CONST k)) = 
	          emit(A.OPER{assem="addi `d0, r0, "^Int.toString(k)^" \n",
            		src=[], dst=[i], jump=NONE})

	      | munchStm (Tr.MOVE(Tr.TEMP i, Tr.CALL(e,args))) =
	          emit(A.OPER{assem="addi `d0, rv, 0 \n", src=[munchExp(Tr.CALL(e,args))], dst=[i], jump=NONE}) 
        
        | munchStm (Tr.MOVE(Tr.TEMP i, e2)) = 
            emit(A.OPER{assem="addi `d0,`s0,0 \n",
                        src=[munchExp e2], dst=[i], jump=NONE})

        (* Call *)
       	| munchStm (Tr.EXP(Tr.CALL(Tr.NAME n,args))) = 
            emit(A.OPER{assem="jal "^(Symbol.name n)^" \n",
                        src=munchArgs(0,args), 
                        dst=Frame.calldefs, jump=SOME([n])})
        
        | munchStm (Tr.EXP(Tr.CALL(e,args))) = 
            emit(A.OPER{assem="jal `s0 \n",
                        src=(munchExp e)::munchArgs(0,args), 
                        dst=Frame.calldefs, jump=NONE})

        | munchStm (Tr.EXP e) = (munchExp (e); ())

      and result(gen) = let val t = Temp.newtemp() in gen t; t end

      and munchExp (Tr.ESEQ(stm, exp)) = (munchStm stm; munchExp exp)

        (* MEMORY *)  		
	      | munchExp(Tr.MEM(Tr.BINOP(Tr.PLUS,e1,Tr.CONST i))) = 
	  	      result(fn r => emit(A.OPER
		        {assem="lw `d0, "^Int.toString(i)^"(`s0) \n",src=[munchExp e1],
		  	    dst=[r], jump=NONE}))

      	| munchExp(Tr.MEM(Tr.BINOP(Tr.PLUS,Tr.CONST i, e1))) =
      	  	result(fn r => emit(A.OPER
      		  {assem="lw `d0, "^Int.toString(i)^"(`s0) \n",src=[munchExp e1],
      		  dst=[r], jump=NONE}))

      	| munchExp(Tr.MEM(Tr.CONST i)) = 
      		  result(fn r => emit(A.OPER
      		  {assem="lw `d0, "^Int.toString(i)^"(r0) \n",src=[],
      			dst=[r], jump=NONE}))

      	| munchExp(Tr.MEM(e1)) =
      		  result(fn r => emit(A.OPER
      		  {assem="lw `d0, 0(`s0) \n",src=[munchExp e1],
      		  dst=[r], jump=NONE}))	

        (* PLUS *)
      	| munchExp(Tr.BINOP(Tr.PLUS,e1,Tr.CONST i)) = 
      	  	result(fn r => emit(A.OPER
      		  {assem="addi `d0, `s0, "^ Int.toString(i) ^ " \n", 
            src=[munchExp e1], dst=[r], jump=NONE}))

      	| munchExp(Tr.BINOP(Tr.PLUS,Tr.CONST i,e1)) = 
      	  	result(fn r => emit(A.OPER
      		  {assem="addi `d0, `s0, "^ Int.toString(i) ^ " \n",
            src=[munchExp e1], dst=[r], jump=NONE}))

        | munchExp(Tr.BINOP(Tr.PLUS,e1,e2)) = 
          result(fn r => emit(A.OPER
            {assem="add `d0,`s0,`s1 \n", 
            src=[munchExp e1,munchExp e2],
            dst=[r], jump=NONE}))

        (* AND/OR *)
        | munchExp(Tr.BINOP(Tr.AND,e1,e2)) =
	         let val t = Temp.newlabel();
	             val f = Temp.newlabel();
	         in 
            result(fn r => emit(A.OPER
            {assem="addi `d0, r0, 1 \n beq `s0,r0,"^(Symbol.name t)^" \n bne `s1,r0,"^(Symbol.name f)^" \n"^(Symbol.name t)^": addi `d0, r0, 0 \n"^(Symbol.name f)^":",
            src=[munchExp e1,munchExp e2],
            dst=[r], jump=NONE}))
	         end
        | munchExp(Tr.BINOP(Tr.OR,e1,e2)) = 
	         let val t = Temp.newlabel();
	             val f = Temp.newlabel();
	         in
            result(fn r => emit(A.OPER
            {assem="addi `d0, r0, 1 \n bne `s0,r0,"^(Symbol.name f)^" \n bne `s1,r0,"^(Symbol.name f)^" \n"^(Symbol.name t)^": addi `d0, r0, 0 \n"^(Symbol.name f)^":",
            src=[munchExp e1,munchExp e2],
            dst=[r], jump=NONE}))
	         end

        (* CONSTANT *)
      	| munchExp(Tr.CONST i) =
      	  	result(fn r => emit(A.OPER
      		  {assem="addi `d0, r0, "^ Int.toString(i) ^ " \n", 
            src=[],	dst=[r], jump=NONE}))

        (* MINUS *)
      	| munchExp(Tr.BINOP(Tr.MINUS,e1,e2)) = 
      	  	result(fn r => emit(A.OPER
      			{assem="sub `d0,`s0,`s1 \n", 
      			src=[munchExp e1,munchExp e2],
      			dst=[r], jump=NONE}))

      	| munchExp(Tr.BINOP(Tr.MUL,e1,e2)) = 
      	  	result(fn r => emit(A.OPER
      			{assem="mult `s0,`s1 \nmflo `d0 \n", 
      			src=[munchExp e1,munchExp e2],
      			dst=[r], jump=NONE}))

      	| munchExp(Tr.BINOP(Tr.DIV,e1,e2)) =
      		result(fn r => emit(A.OPER
      			{assem="div `s0,`s1 \nmflo `d0 \n", 
      			src=[munchExp e1,munchExp e2],
      			dst=[r], jump=NONE}))

      	| munchExp(Tr.TEMP t) = t

        | munchExp(Tr.NAME name) = 
          result(fn r => emit(A.OPER{assem="la `d0, "^(Symbol.name name)^" \n", dst=[r],
          	    src=[], jump=NONE}))

      	| munchExp(Tr.CALL(Tr.NAME name,args)) =
            result(fn r => emit(A.OPER{assem="jal "^(Symbol.name(name))^" \n",
            src=munchArgs(0,args), dst=Frame.calldefs, jump=SOME([name])}))

     	  | munchExp(Tr.CALL(e,args)) =
            result(fn r => emit(A.OPER{assem="jal `s0 \n",
                   src=(munchExp e)::munchArgs(0,args), 
                   dst=Frame.calldefs, jump=NONE}))

      and munchArgs(i,[]) = []
        | munchArgs(i,a::l) =
	       (if (i<4) then
	        emit(A.OPER{assem="add a"^Int.toString(i)^",`s0,r0 \n", 
	     		            src=[munchExp a], dst=[], jump=NONE})
	        else emit(A.OPER{assem="sw `s0,"^Int.toString((~4)*i)^"(fp) \n",
	   		                   src=[munchExp a], dst=[], jump=NONE}); 
	        munchArgs(i+1,l));
   	
    in 
     	munchStm stm; rev (!ilist)
    end
end
