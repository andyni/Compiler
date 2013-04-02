structure MipsGen : CODEGEN =  
struct
  structure Frame : FRAME = MipsFrame
  structure A = Assem
  structure Tr = Tree

  fun codegen (frame) (stm: Tr.stm) : A.instr list = 
  	let 
  		val ilist = ref (nil : A.instr list)
  		fun emit x = (ilist := x::(!ilist))
  		fun result(gen) = let val t = Temp.newtemp() in gen t; t end

  		fun munchStm (Tr.SEQ(stm1, stm2)) = (munchStm stm1; munchStm stm2) 

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
            emit(A.OPER{assem="sw `s1, (`s0) \n",
                        src=[munchExp e1, munchExp e2], dst=[], jump=NONE})

        | munchStm (Tr.MOVE(Tr.TEMP i, e2)) = 
            emit(A.OPER{assem="sw `s0, (`d0) \n",
                        src=[munchExp e2], dst=[i], jump=NONE})

        | munchStm (Tr.LABEL lab) = 
            emit(A.LABEL{assem=Symbol.name lab ^ ": ",
                         lab=lab})  

      and result(gen) = let val t = Temp.newtemp() in gen t; t end

      and munchExp (Tr.ESEQ(stm, exp)) = (munchStm stm; munchExp exp)
  	in 
  		(munchStm stm; rev (!ilist))
  	end	
	| munchExp(Tr.MEM(Tr.BINOP(Tr.PLUS,e1,Tr.CONST i))) = 
	  	result(fn r => emit(A.OPER
		{assem="lw `d0, "^int i^"(`s0)\n",src=[munchExp e1],
		  	dst=[r], jump=NONE}))
	| munchExp(Tr.MEM(Tr.BINOP(Tr.PLUS,Tr.CONST i, e1)))
	  	result(fn r => emit(A.OPER
		{assem="lw `d0, "^int i^"(`s0)\n",src=[munchExp e1],
		  	dst=[r], jump=NONE}))
	| munchExp(Tr.MEM(Tr.CONST i)) = 
		result(fn r => emit(A.OPER
		{assem="lw `d0, "^int i^"(r0)\n",src=[],
		  	dst=[r], jump=NONE}))	
	| munchExp(Tr.MEM(e1)) =
		result(fn r => emit(A.OPER
		{assem="lw `d0, 0(`s0)\n",src=[munchExp e1],
		  	dst=[r], jump=NONE}))	
	| munchExp(Tr.BINOP(Tr.PLUS,e1,Tr.CONST i)) = 
	  	result(fn r => emit(A.OPER
		{assem="ADDI `d0 <- `s0+"^ int i ^ "\n", src=[munchExp e1],
			dst=[r], jump=NONE}))
	| munchExp(Tr.BINOP(Tr.PLUS,Tr.CONST i,e1)) = 
	  	result(fn r => emit(A.OPER
		{assem="ADDI `d0 <- `s0+"^ int i ^ "\n", src=[munchExp e1],
			dst=[r], jump=NONE}))
	| munchExp(Tr.CONST i)
	  	result(fn r => emit(A.OPER
		{assem="ADDI `d0 <- r0+"^ int i ^ "\n", src=[munchExp e1],
			dst=[r], jump=NONE}))	  		
	| munchExp(Tr.BINOP(Tr.PLUS,e1,e2)) = 
		result(fn r => emit(A.OPER
			{assem="add `d0,`s0,`s1\n", 
			src=[munchExp e1,munchExp e2],
			dst=[r], jump=NONE}))
	| munchExp(Tr.BINOP(Tr.MINUS,e1,e2)) = 
	  	result(fn r => emit(A.OPER
			{assem="sub `d0,`s0,`s1\n", 
			src=[munchExp e1,munchExp e2],
			dst=[r], jump=NONE}))
	| munchExp(Tr.BINOP(Tr.MUL,e1,e2)) = 
	  	result(fn r => emit(A.OPER
			{assem="mult `s0,`s1\n", 
			src=[munchExp e1,munchExp e2],
			dst=[], jump=NONE}))
	| munchExp(Tr.BINOP(Tr.DIV,e1,e2)) =
		result(fn r => emit(A.OPER
			{assem="div `s0,`s1\n", 
			src=[munchExp e1,munchExp e2],
			dst=[], jump=NONE}))
	| munchExp(Tr.TEMP t)
	  	result(fn r => emit(A.OPER
			{assem="add `d0,`s0,r0\n",
			src=[t], dst[r], jump=NONE}))
	| munchExp(Tr.CALL(e1,args))
		emit(A.OPER{
			assem="jal `s0\n",
			src=[munchExp e1::munchArgs(0,args)], dst[calldefs]})

		
end
