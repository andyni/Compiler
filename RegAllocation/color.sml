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

structure Color : COLOR =
struct
	structure G = Graph
	structure Frame = MipsFrame
	type allocation = MipsFrame.register Temp.Table.table
	val K = 27
	fun color{interference, initial, spillCost, registers} = 
	    let	val Liveness.IGRAPH{graph,tnode,gtemp,moves} = interference
	    	val nodelist = G.nodes(graph)

	    	fun makeWorkList() =
		    let fun addtolist (node, (list1,list2)) = 
		        let val prec = Temp.Table.look(initial,gtemp(node))
			in
				case prec of SOME(r) =>(list1,list2)
				           | NONE    => if
					     	     (length(G.adj(node))<K) then
					   	     (node::list1,list2) else
					   	     (list1,node::list2)
			end
			in
				foldl (addtolist) ([],[]) nodelist
			end
		val (simplifyWorklist, spillWorklist) = makeWorkList()

		fun makeDegreeTable(node,table) =
		    let val degree = length(G.adj(node))
		    	in
				G.Table.enter(table,node,degree)
			end

		val degreeTable = foldl (makeDegreeTable) G.Table.empty (simplifyWorklist @ spillWorklist)


		fun removeElem(list,item) =
		    List.filter (fn(x)=>(gtemp(x)<gtemp(item) orelse gtemp(x)>gtemp(item))) list

		fun simplifyNode(node, selectStack, degreeTable, simplifyWorklist, spillWorklist)=
		    let val neighbors = G.adj(node)
		        val nsimplify' = removeElem(simplifyWorklist,node)
			val nselect = node::selectStack
		    	fun decrementdegree(n,(degreeTable,simplifyWorklist,spillWorklist))=
			    let val degreeOpt =  G.Table.look(degreeTable,n)
			    	val upDegree = case degreeOpt of SOME(deg) => G.Table.enter(degreeTable,n,deg-1)
							     | NONE => degreeTable
				val degree = case degreeOpt of SOME(deg) => deg
								| NONE => 0
				val simplify = if (degree=K) then n::simplifyWorklist else simplifyWorklist
				val spill = if (degree=K) then removeElem(spillWorklist,n)  else spillWorklist			    	
				in
					(upDegree,simplify,spill)
				end
			val (ndegree,nsimplify,nspill) = foldl (decrementdegree) (degreeTable, nsimplify', spillWorklist) neighbors
			in
				(nselect,ndegree,nsimplify,nspill)
			end

		fun spillNode(selectStack, degreeTable, simplifyWorkList,node::spillWorklist)=
		    let fun bestnode(n,bestn) = if (spillCost(n)>spillCost(bestn)) then n else bestn		    				
		    	val bestN = foldl (bestnode) node spillWorklist
			val interspill = node::spillWorklist
			val nspill = removeElem(interspill,bestN)
			in
				simplifyNode(bestN, selectStack, degreeTable, simplifyWorkList, nspill)
			end

		fun handleSimplification(selectStack, degreeTable, [], []) = selectStack 
		    | handleSimplification(selectStack, degreeTable, [], spillWorklist) =
						      handleSimplification(spillNode(selectStack, degreeTable, [], spillWorklist))
		    | handleSimplification(selectStack, degreeTable, n::simplifyWorklist, spillWorklist) =
		    				      handleSimplification(simplifyNode(n, selectStack, degreeTable, simplifyWorklist, spillWorklist))

		val selectStack = handleSimplification([], degreeTable, simplifyWorklist, spillWorklist)

		
		fun assignColors(node, (colorTable, spillNodes)) =

			let val neighbors = G.adj(node)
		    	fun compColor(nbor,reg) = 
			    	let val col = Temp.Table.look(colorTable,gtemp(nbor))
				   
				in
					case col of NONE => true
					     	 |  SOME(r) => not (Frame.regsEqual(reg,r))
		    		end
		    	fun availReg(nbor,avail) = List.filter (fn (a) => compColor(nbor,a)) avail
			val availRegs = foldl (availReg) registers neighbors
			val nspill = case availRegs of [] => gtemp(node)::spillNodes
			       	      		|  _  => spillNodes
			val ncolor = case availRegs of [] => colorTable
			       	      		|  a::l => Temp.Table.enter(colorTable,gtemp(node),a)
		    	in
				(ncolor, nspill)
			end

		val (colorTable, spillTable) = foldl (assignColors) (initial, []) selectStack

		in
			(colorTable, spillTable)
		end
		

end				
