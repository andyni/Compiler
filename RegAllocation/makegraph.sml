structure Makegraph :> MAKEGRAPH = 
struct 
	structure A = Assem
	structure G = Flow.Graph

	fun instrs2graph instrs = 
	let 
		val graph = G.newGraph()
		val nodelist = map (fn _ => G.newNode(graph)) instrs
		val instrTuple = ListPair.zip (instrs, nodelist)

		(* Creates def, use, and ismove tables *)
		fun parseInstructions ((A.OPER{assem=assem, dst=dst, src=src, jump=jump}, node), 
							   (defs, uses, ismoves)) =
			(G.Table.enter(defs, node, dst),
			 G.Table.enter(uses, node, src),
			 G.Table.enter(ismoves, node, false))				   
		  | parseInstructions ((A.LABEL{assem=assem, lab=lab}, node), 
		                       (defs, uses, ismoves)) =
			(G.Table.enter(defs, node, []),
			 G.Table.enter(uses, node, []),
			 G.Table.enter(ismoves, node, false))		  
		  | parseInstructions ((A.MOVE{assem=assem, dst=dst, src=src}, node), 
		  					   (defs, uses, ismoves)) =
			(G.Table.enter(defs, node, [dst]),
			 G.Table.enter(uses, node, [src]),
			 G.Table.enter(ismoves, node, true))

		val (defs, uses, ismoves) = foldl parseInstructions (G.Table.empty, G.Table.empty, G.Table.empty) instrTuple

		(* Creates edge for all non-jump instructions *)
		fun connectEdges ((instr1, node1)::l) = 
		    let val (instr2, node2) = hd(l) in
		    	G.mk_edge({from= node1, to=node2})
		    end
		  | connectEdges (a) = ()

		val _ = connectEdges instrTuple  

		
	in
		(Flow.FGRAPH {control = graph, def = defs,
		              use = uses, ismove = ismoves}, nodelist)
	end 
end