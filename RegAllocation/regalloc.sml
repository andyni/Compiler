signature REG_ALLOC = 
sig
	structure Frame : FRAME
	type allocation = Frame.register Temp.Table.table
	val alloc : Assem.instr list * Frame.frame -> 
							Assem.instr list * allocation
end

structure RegAlloc : REG_ALLOC = 
struct
	structure Frame : FRAME = MipsFrame
	structure M = Makegraph
	structure L = Liveness
	structure C = Color
	type allocation = Frame.register Temp.Table.table

	fun alloc (instrs, frame) = 
		let val (flowgraph, nodes) = M.instrs2graph(instrs,TextIO.stdOut)
			val (interferencegraph, liveouts) = L.interferenceGraph(TextIO.stdOut,flowgraph)

			val Flow.FGRAPH {control=_, def=def, use=use, ismove=_} = flowgraph
			val L.IGRAPH {graph=_, tnode=_, gtemp=gtemp, moves=_} = interferencegraph
			fun spillCost node = 
				let val temp = gtemp(node)
					fun countUsesDefs n = let 
						val SOME(defL) = Graph.Table.look (def, node)
						val SOME(useL) = Graph.Table.look (use, node)
						val d = if (List.exists (fn x => (x=temp)) defL) then 1 else 0
						val u = if (List.exists (fn x => (x=temp)) useL) then 1 else 0
						val sum = d + u
					in
						sum
					end
				in
					foldr (fn (n, count) => count + countUsesDefs(n)) 0 nodes
				end 

			val (allocation, spill) = 
					C.color {interference = interferencegraph,
             			     initial = Frame.tempMap,
					         spillCost = spillCost,
					         registers = Frame.registers}

		in
			(instrs, allocation)
		end
end
