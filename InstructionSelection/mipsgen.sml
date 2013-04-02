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
            emit(A.OPER{assem="j `j0",
                        src=[], dst=[], jump=SOME(labellist)}) 

        | munchStm (Tr.JUMP (Tr.CONST(i), labellist)) =
            emit(A.OPER{assem="j `j0",
                        src=[], dst=[], jump=SOME(labellist)}) 

        | munchStm (Tr.JUMP (e, labellist)) =
            emit(A.OPER{assem="jr `s0",
                        src=[munchExp e], dst=[], jump=SOME(labellist)}) 

        (* CJUMP *)
        | munchStm (Tr.CJUMP (relop, Tr.CONST(i1), Tr.CONST(i2), lab1, lab2)) =
            emit(A.OPER{assem= getRelop(relop) ^ " " ^ Int.toString(i1) ^ ", " ^ Int.toString(i2) ^ ", " ^ Symbol.name(lab1),
                        src=[], dst=[], jump=SOME([lab1, lab2])})

        | munchStm (Tr.CJUMP (relop, Tr.CONST(i), e2, lab1, lab2)) =
            emit(A.OPER{assem= getRelop(relop) ^ " " ^ Int.toString(i) ^ ", `s0, " ^ Symbol.name(lab1),
                        src=[munchExp e2], dst=[], jump=SOME([lab1, lab2])})

        | munchStm (Tr.CJUMP (relop, e1, Tr.CONST(i), lab1, lab2)) =
            emit(A.OPER{assem= getRelop(relop) ^ " `s0, " ^ Int.toString(i) ^ ", " ^ Symbol.name(lab1),
                        src=[munchExp e1], dst=[], jump=SOME([lab1, lab2])})

        | munchStm (Tr.CJUMP (relop, e1, e2, lab1, lab2)) =
            emit(A.OPER{assem= getRelop(relop) ^ " 's0', `s1, " ^ Symbol.name(lab1),
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
            emit(A.OPER{assem="sw `s1, (`s0) \n",
                        src=[munchExp e1, munchExp e2], dst=[], jump=NONE})

        | munchStm (Tr.MOVE(Tr.TEMP i, e2)) = 
            emit(A.OPER{assem="sw `s0, (`d0) \n",
                        src=[munchExp e2], dst=[i], jump=NONE})

        | munchStm (Tr.EXP(Tr.CALL(e,args))) = ()

        | munchStm (Tr.EXP e) = (munchExp (e); ())

      and munchExp (Tr.ESEQ(stm, exp)) = (munchStm stm; munchExp exp)
  	in 
  		(munchStm stm; rev (!ilist))
  	end	
end