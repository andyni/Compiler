structure liveness : LIVENESS = 
struct
	structure G = Flow.Graph
	datatype igraph = 
	IGRAPH of {graph: Graph.graph,
			   tnode: Temp.temp -> Graph.node,
			   gtemp: Graph.node -> Temp.temp,
			   moves: (Graph.node * Graph.node) list}

	type liveSet = unit Temp.Table.table * temp list
	type liveMap = liveSet Flow.Graph.Table.table

	fun interferenceGraph (Flow.FGRAPH{control=control, def=def, 
		                               use=use, ismove=ismove}) =
		let 

		in
			(IGRAPH{graph= , 
				    tnode = , 
				    gtemp = ,
				    move =  },
				     )
		end

	fun show (outstream, IGRAPH{graph=graph, tnode=tnode, 
		                        gtemp=gtemp, moves=moves}) = ()
end