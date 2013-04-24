structure Makegraph :> MAKEGRAPH = 
struct 
	structure A = Assem
	structure G = Flow.Graph
	structure S = Symbol

	(* Translates instruction list to graph *)
	fun instrs2graph (instrs) = 
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
		                       (defs, uses, ismoves, labeltonode))=
			 (G.Table.enter(defs, node, []),
			 G.Table.enter(uses, node, []),
			 G.Table.enter(ismoves, node, false),
			 S.enter(labeltonode, lab, node))		  
		  | parseInstructions ((A.MOVE{assem=assem, dst=dst, src=src}, node), 
		  					   (defs, uses,ismoves, labeltonode)) =
			 (G.Table.enter(defs, node, [dst]),
			 G.Table.enter(uses, node, [src]),
			 G.Table.enter(ismoves, node, true),
			 labeltonode)

		val (defs, uses, ismoves, labeltonode) = foldl parseInstructions 
			(G.Table.empty, G.Table.empty, G.Table.empty, S.empty) 
			instrTuple

	    (* Gets node using label key *)
		fun getNode label = 
			case S.look(labeltonode, label) of
				SOME(node) => node
		      | NONE => ErrorMsg.impossible ("Cannot find label "^Symbol.name(label))

        (* Checks if node is in labeltonode to prevent external calls and jumps to other frags *)
		fun checkNode label = 
			case S.look(labeltonode, label) of
				SOME(node) => true
		      | NONE => false

		(* Creates all edges *)
		fun connectJumps (A.OPER{assem=assem, dst=dst, src=src, jump=jump}, node, NONE) =
		    (case jump of
		          SOME(labels) => (map 
		          	(fn label => if(checkNode label) then (G.mk_edge {from=node, to=getNode label}) else ()) 
		          	labels; ())
		        | NONE => ())
		  
		 | connectJumps (A.OPER{assem=assem, dst=dst, src=src, jump=jump}, node, SOME(node2)) =
		    (case jump of
		          SOME(labels) => (map 
		          	(fn label => if(checkNode label) then (G.mk_edge {from=node, to=getNode label}) else ()) 
		          	labels; ())
		        | NONE => (G.mk_edge({from = node, to=node2})))
		  | connectJumps (_, node, SOME(node2)) = (G.mk_edge({from= node, to=node2}))
		  | connectJumps (_, node, NONE) = ()

		(* Creates edge for all instructions *)
		fun connectaEdge ((instr1,node1)::[]) = (connectJumps(instr1,node1,NONE))
		  | connectaEdge ((instr1, node1)::l) = 
		    let val (instr2, node2) = hd(l)
		    	 in
		    	 connectJumps(instr1,node1,SOME(node2)); connectaEdge(l)
		    end

		val _ = connectaEdge instrTuple

	in
		(Flow.FGRAPH {control = graph, def = defs,
		              use = uses, ismove = ismoves}, nodelist)
	end 
end
