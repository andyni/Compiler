signature MAKEGRAPH = 
sig 
	val instrs2graph: Assem.instr list*TextIO.outstream->
						Flow.flowgraph * Flow.Graph.node list
end
