signature LIVENESS = 
sig 
	datatype igraph = 
		IGRAPH of {graph: Graph.graph,
				   tnode: Temp.temp -> Graph.node,
				   gtemp: Graph.node -> Temp.temp,
				   moves: (Graph.node * Graph.node) list}

	val interferenceGraph : 
		TextIO.outstream * Flow.flowgraph -> 
			igraph * (Flow.Graph.node -> Temp.temp list)

	val show : TextIO.outstream * igraph -> unit
end
