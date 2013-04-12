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
        structure Set = ListSetFn(type key = int,
		      		  val compare = Int.compare)
				 

	fun interferenceGraph (Flow.FGRAPH{control=control, def=def, 
		                               use=use, ismove=ismove}) =
	    let 
		fun createLiveSet (SOME(tempList)) = let val t = foldl (fn (temp, currSet) =>
									   Temp.Table.enter(currSet,temp,()))
								       Temp.Table.empty tempList
						     in
							 (t,tempList)
						     end	
					   
		val nodelist = Graph.nodes control

		val inMap = foldl (fn(n,currMap) =>
		                      G.Table.enter(currMap,n,createLiveSet(SOME([]))))
		                  G.Table.empty
				  nodelist
				  
		val outMap = foldl (fn(n,currMap) =>
		                       G.Table.enter(currMap,n,createLiveSet(SOME([]))))
		                   G.Table.empty
		                   nodelist
	
		   
		fun runNode((inMap,outMap,sameness),n) =
		    let val SOME(inT,inL) = G.Table.look(inMap,n)     	 
		    	val inSet = Set.addList(Set.empty,inL)
			val SOME(outT,outL) = G.Table.look(outMap,n)
			val outSet = Set.addList(Set.empty, outL)
			val SOME(useL) = G.Table.look(uses,n)
			val useSet = Set.addList(Set.empty,useL)
			val SOME(defL) = G.Table.look(def,n)
			val defSet = Set.addList(Set.empty,defL)
			val inN = Set.union(useSet,Set.difference(outSet,defSet))
			val newInMap =	G.Table.enter(inMap,createLiveSet(Set.listItems inN))
			fun createOutSet(succ,outS) =
		    	    let val SOME(succT,succL) = G.Table.look(inMap,succ)     	 
	               		val succSet = Set.addList(Set.empty,succL)
			    in
				Set.union(outS,succSet)
			    end
			val outN = foldl createOutSet	Set.empty G.succ(n)
			val newOutMap =	G.Table.enter(outMap,createLiveSet(Set.listItems outN))
			val unchanged = if (Set.numItems(Set.union(inSet,inN))=Set.numItems(Set.intersection(inSet,inN)))
		    			then true else false
			val unchanged2 = if (Set.numItems(Set.union(outSet,outN))=Set.numItems(Set.intersection(outSet,outN)))
		    			 then true else false
					   
				       
		    in
			(inMap,outMap,sameness andalso unchanged andalso unchanged2)
		    end
		fun runAllNodes(inMap,outMap) =
		    let val (inM, outM, same) = foldl
						    runNode (inMap,outMap,true) nodelist
		    in
			if (same) then	(inM,outM) else runAllNodes(inM,outM)
		    end
		val (finMap,foutMap) = runAllNodes(inMap, outMap)
				
		(* Create Interference Graph *)
		val interGraph = G.newGraph()
		val moves = []

		val temps = foldr (fn(node, l) => valOf(G.Table.look(def, node)) @ l) [] nodelist
		val tempslist = Set.listItems(Set.addList(Set.empty, temps))
					     
		fun createTables (temp, tnode, gtemp) = 
		    let val node = G.newNode(interGraph)
		    in 
			(Temp.Table.enter(tnode, temp, node),
			 G.Table.enter(gtemp, node, temp))
		    end
			
		val (tnode, gtemp) = foldr createTables 
					   (Temp.Table.empty, G.Table.empty) tempslist
					   
			
		fun edgeCreation(n,g) =
		    	      let val SOME(outT,outL) = G.Table.look(foutMap)
			      in
				foldl (fn(nde,grph)=>G.mk_edge{from=n,to=nde}) g outL
			      end

		val finGraph = foldl (fn(n,g)=> edgeCreation(n,g)) interGraph nodeList

		in
			(IGRAPH{graph= interGraph, 
				    tnode = fn t => case Temp.Table.look(tnode, t) of
				    						SOME node => node
				    					  | NONE => ErrorMsg.impossible ("Temp not in table.") 
				    gtemp = fn n => case G.Table.look(gtemp, n) of 
				    						SOME temp => temp 
				    					  | NONE => ErrorMsg.impossible ("Node not in table.")
				    move = moves},
				     )
		end

	fun show (outstream, IGRAPH{graph=graph, tnode=tnode, 
		                        gtemp=gtemp, moves=moves}) = ()
end
