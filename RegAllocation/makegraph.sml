structure Makegraph :> MAKEGRAPH = 
struct 
	structure A = Assem
	structure G = Flow.Graph
	structure S = Symbol

	fun instrs2graph instrs = 
	let 
		val graph = G.newGraph()
		val nodelist = map (fn _ => G.newNode(graph)) instrs
		val instrTuple = ListPair.zip (instrs, nodelist)

		(* Creates def, use, and ismove tables *)
		fun parseInstructions ((A.OPER{assem=assem, dst=dst, src=src, jump=jump}, node), 
							   (defs, uses, ismoves, labeltonode)) =
			(G.Table.enter(defs, node, dst),
			 G.Table.enter(uses, node, src),
			 G.Table.enter(ismoves, node, false),
			 labeltonode)				   
		  | parseInstructions ((A.LABEL{assem=assem, lab=lab}, node), 
		                       (defs, uses, ismoves, labeltonode)) =
			(G.Table.enter(defs, node, []),
			 G.Table.enter(uses, node, []),
			 G.Table.enter(ismoves, node, false),
			 S.enter(labeltonode, lab, node))		  
		  | parseInstructions ((A.MOVE{assem=assem, dst=dst, src=src}, node), 
		  					   (defs, uses, ismoves, labeltonode)) =
			(G.Table.enter(defs, node, [dst]),
			 G.Table.enter(uses, node, [src]),
			 G.Table.enter(ismoves, node, true),
			 labeltonode)

		val (defs, uses, ismoves, labeltonode) = foldl parseInstructions 
			(G.Table.empty, G.Table.empty, G.Table.empty, S.empty) 
			instrTuple

		(* Creates edge for all non-jump instructions *)
		fun connectEdges ((instr1, node1)::l) = 
		    let val (instr2, node2) = hd(l) in
		    	G.mk_edge({from= node1, to=node2})
		    end
		  | connectEdges (a) = ()

		val _ = connectEdges instrTuple  

		fun getNode label = 
			case S.look(labeltonode, label) of
				SOME(node) => node
		      | NONE => ErrorMsg.impossible "Cannot find label."

		(* Creates all jump edges *)
		fun connectJumps (A.OPER{assem=assem, dst=dst, src=src, jump=jump}, node) =
		    (case jump of
		          SOME(labels) => (map 
		          	(fn label => G.mk_edge({from=node, to=getNode label})) 
		          	labels; ())
		        | NONE => ())
		  | connectJumps (_, node) = ()   

		val _ = map connectJumps instrTuple

	in
		(Flow.FGRAPH {control = graph, def = defs,
		              use = uses, ismove = ismoves}, nodelist)
	end 
end