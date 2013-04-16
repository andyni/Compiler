signature COLOR =
sig
	structure Frame : FRAME
	type allocation = Frame.register Temp.Table.table
	val color : {interference : Liveness.igraph,
	    	     initial : allocation,
		     spillCost : Graph.node -> int,
		     registers : Frame.register list}
		     -> allocation * Temp.temp list
end

structure Color :> COLOR
in
	val K = 18
	fun color{interference, initial, spillCost, registers} = 
	    let val {graph,tnode,gtemp,moves} = interference
	    	val nodelist = G.nodes(graph)
	    	fun makeWorkList() =
		    let fun addtolist (node, (list1,list2)) = if (length(G.adj(node))<K) then node::list1 else node::list2
			in
				foldl (addtolist) ([],[]) nodelist
			end
		val (simplifyWorklist, spillWorklist) = makeWorkList()
		fun makeDegreeTable(node,table) =
		    let val degree = length(G.adj(node))
		    	in
				G.Table.enter(table,node,degree)
			end

		val degreeTable = foldl (makeDegreeTable) G.Table.empty nodelist

		fun simplifyNode(node, selectStack, degreeTable, simplifyWorklist, spillWorklist)
		    let val nsimplify' = filter (fn(x)=>x<>node) simplifyWorklist
			val nselect = node::selectStack
		    	val neighbors = G.adj(node)
		    	fun decrementdegree(n,(degreeTable,simplifyWorklist,spillWorklist))
			    let val SOME(degree) =  G.Table.look(degreeTable,n)
			    	upDegree =  G.Table.enter(degreeTable,n,degree-1)
				simplify = if (degree=K) then n::simplifyWorklist else simplifyWorklist
				spill = if (degree=K) then filter (fn (x)=> x <> n) spillWorklist  else spillWorklist			    	
				in
					(upDegree,simplify,spill)
				end
			val (ndegree,nsimplify,nspill) = foldl (decrementdegree) (degreeTable, nsimplify', spillWorklist) neighbors
			in
				(nselect,ndegree,nsimplify,nspill)
			end

		fun spillNode(selectStack, degreeTable, simplifyWorklist,n::spillWorklist)
		    let fun bestnode(n,bestn) = if (spillCost(n)>spillCost(bestn)) then n else bestn		    				
		    	val bestN = foldl (bestnode) n spillWorklist
			interspill = n::spillWorklist
			nspill = filter (fn (x)=> x <> n) interspill
			in
				simplify(bestN, degreeTable, simplifyWorklist, nspill)
			end

		fun handleSimplification(selectStack, degreeTable, [], []) = selectStack 
		    handleSimplification(selectStack, degreeTable, [], spillWorklist) =
						      handleSimplification(spillNode(selectStack, degreeTable, simplifyWorklist, spillWorklist))
		    handleSimplification(selectStack, degreeTable, n::simplifyWorklist, spillWorklist) =
		    				      handleSimplification(simplifyNode(n, selectStack, degreeTable, simplifyWorklist, spillWorklist))

		val selectStack = handleSimplification([], degreeTable, simplifyWorklist, spillWorklist)
		
		fun assignColors(node, (colorTable, spillNodes)) =
		    let val neighbors = G.adj(node)
		    	fun compColor(nbor,reg) = 
			    	let val col = Temp.Table.look(colorTable,gtemp(nbor))
				    val pcol = Temp.Table.look(initial,gtemp(nbor))
				    val isPcol = case pcol of NONE => true
					     	    	    | SOME(r) => String.compare(r,reg)<>EQUAL 
				in
					case col of NONE => isPcol
					     	 |  SOME(r) => String.compare(r,reg)<>EQUAL 
		    		end
		    	fun availReg(nbor,avail) = filter (fn (a) => compColor(nbor,a)) avail
			val availRegs = foldl (availReg) registers neighbors
			nspill = case availRegs of [] => gtemp(node)::spillNodes
			       	      		|  _  => spillNodes
			ncolor = case availRegs of [] => colorTable
			       	      		|  a::l => Temp.Table.enter(colorTable,gtemp(node),a)
		    	in
				(ncolor, nspill)
			end

		val (colorTable, spillTable) = foldl (assignColor) (Temp.Table.empty, []) selectStack

		in
			(colorTable, spillTable)
		end
		

end				
