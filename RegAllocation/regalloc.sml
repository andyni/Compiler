signature REG_ALLOC = 
sig
	structure FRAME:Frame
	type allocation = Fram.register Temp.Table.table
	val alloc : Assem.instr list = Frame.frame -> Assem.instr list *
	allocation
end

structure RegAlloc :> REG_ALLOC
in

end
