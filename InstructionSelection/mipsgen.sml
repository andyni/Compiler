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
  		and munchExp (Tr.ESEQ(stm, exp)) = (munchStm stm; munchExp exp)
  	in 
  		(munchStm stm; rev (!ilist))
  	end	
end