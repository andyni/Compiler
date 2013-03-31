signature CANON = 
sig
    val canonicalize: Tree.stm -> CTree.stm' list
end

structure Canon : CANON = 
struct

  structure T = Tree
  structure CT = CTree
		 
  structure LS = CTree.LocSet
  structure LH = LabelHot
  type label = Temp.label

  fun cts(T.SEQ(s1,s2)) = (cts s1) @ (cts s2) 
    | cts(T.LABEL l) = [CT.LABEL l]
    | cts(T.JUMP(tar,lbls)) = 
      let val (s,tar') = cte tar
      in
	  s @ [CT.JUMP(tar',lbls)]
      end
    | cts(T.CJUMP(ro,e1,e2,l1,l2)) = 
      let val (s',e1',e2') = twoExps(e1,e2)
      in
	  s'@ [CT.CJUMP(ro,e1',e2',l1,l2)]
      end
    | cts(T.MOVE(T.MEM e1,e2)) = 
      let val (s,e1',e2') = twoExps(e1,e2)
      in
	  s@[CT.MOVE(CT.MEM e1', CT.RVEXP e2')]
      end
    | cts(T.MOVE(l, e)) = 
      let val (s',e') = cte e
      in
	  s'@[CT.MOVE(ctl l, CT.RVEXP e')]
      end
    | cts(T.EXP e) = 
      let val (s',_) = cte e 
      in
	  s'
      end
  and cte(T.BINOP(b,e1,e2)) = 
      let val (s',e1',e2') = twoExps(e1,e2)
      in
	  (s',CT.BINOP(b,e1',e2'))
      end
    | cte(T.LOC(T.MEM e)) = 
      let val (s',e') = cte e
      in
	  (s', CT.LOC(CT.MEM e'))
      end
    | cte(T.LOC l) = ([],CT.LOC (ctl l))
    | cte(T.ESEQ(s,e)) = 
      let val s1' = cts s
	  val (s2',e')= cte e
      in
	  (s1'@s2',e')
      end
    | cte(T.CONST c) = ([], CT.CONST c)
    | cte(T.CALL(f,args)) = 
      let val f' = ctl f  (* will fail on CALL(MEM ...) but we don't do that
                             in Tiger*)

	  fun xlat(slist,alist,[]) = (slist, alist)
	    | xlat(slist,alist,arg::rest) = 
	      let val (s',a') = cte arg
		  val t = Temp.newtemp()
	      in
		  xlat(CT.MOVE(CT.TEMP t, CT.RVEXP a')::(s'@slist),
		       CT.LOC(CT.TEMP t)::alist,
		       rest)
	      end
	  val (slistRev,alistRev) = xlat([],[],args)
	  val rv = Temp.newtemp()
	  (* This move may be superflous, but then  reg alloc 
	   * will have an easy time cleaning it up later 
	   *)
	  val mv = CT.MOVE(CT.TEMP rv, CT.RVCALL(f', rev alistRev))
      in
	  (rev (mv::slistRev), CT.LOC(CT.TEMP rv))
      end
      
  and ctl(T.MEM e) = raise Fail ("Need to do something more complex here")
    | ctl(T.TEMP t) = CT.TEMP t
    | ctl(T.NAME lbl) = CT.NAME lbl
  and twoExps(e1,e2) = 
      let val (s1',e1') = cte e1
	  val (s2',e2') = cte e2
      in
	  if LS.isEmpty(LS.intersection(CT.readSet   e1',
					CT.writeSet' s2'))
	  then (s1'@s2',e1',e2')
	  else let val t = Temp.newtemp ()
		   val move = CT.MOVE(CT.TEMP t, CT.RVEXP e1')
	       in
		   (s1' @ (move::s2'), CT.LOC(CT.TEMP t),e2')
	       end
      end

type bblock = {start: label,
	       stms: CT.stm' list,
	       fall: label option}

fun bcmp(b1:bblock, b2:bblock) = Symbol.compare(#start b1, #start b2)

structure BKey = struct type ord_key = bblock
			val compare = bcmp
		 end
structure LKey = struct type ord_key = label
			val compare = Symbol.compare
		 end
structure LMap = SplayMapFn(LKey)


structure BSet = SplaySetFn(BKey)

fun blks stms = 
    let fun hlp(bb,[],m) = LMap.insert(m,
				       #start bb,
				       {start = #start bb,
					stms = rev (#stms bb),
					fall = NONE})
	  | hlp({start,stms,fall}, CT.LABEL(l)::r,m) =
	    let val thisBlock={start = start,
			       stms = rev stms,
			       fall = SOME l}
	    in
		hlp({start = l,
		     stms = [CT.LABEL l],
		     fall = NONE},
		    r,
		    LMap.insert(m, start, thisBlock))
	    end
	  | hlp({start,stms,fall},a::r,m) = hlp({start = start,
						 stms = a::stms,
					         fall = NONE},
						r,
						m)
    in					    
	case stms of
	    (a as CT.LABEL l)::r => (l,hlp({start = l,stms = [a],fall = NONE},r,LMap.empty))
	  |  _ => raise Fail ("List of statements must start with a label!")
    end

fun linearize (start, blocks) = 
    let val start' = case LMap.find(blocks,start) of
			 SOME x => x
		       | NONE => raise Fail "could not find start block"
	fun targs {start,stms,fall} = case rev stms of
					  CT.JUMP(_,lst)::_ => lst
					| CT.CJUMP(_,_,_,lT,lF)::_ => [lF,lT]
					| _ => (case fall of
						    SOME x => [x]
						  | NONE => (print "FAILING!\n";
							     Printtree.printctree(TextIO.stdOut,
										  stms);
							     raise Fail
								       "No jump, no fall through"))
	   
	fun addSuccs(bb,lst) = 
	    let val succLbls = targs bb
		fun lookBlk x = case LMap.find(blocks,x) of
				    SOME y => y
				  | NONE => raise Fail("Successor of block does not exist!")
		val hotLbls = List.filter LH.isHot succLbls
		val medLbls = List.filter LH.isMed succLbls
		val coldLbls = List.filter LH.isCold succLbls
	    in
		(map lookBlk (hotLbls @ medLbls))@ lst @(map lookBlk coldLbls)
	    end
	    
	fun hlp([],_, ans) = rev ans
	  | hlp(a::l, visited, ans) = 
	    if BSet.member(visited,a) 
	    then hlp(l,visited,ans)
	    else hlp(addSuccs(a,l),BSet.add(visited,a),a::ans)
    in
	hlp([start'],BSet.empty,[])
    end

fun cleanBlocks trace = 
    let fun ensureTargets([],rTrace) = rev rTrace
	  | ensureTargets([b1],rTrace) =
	    (case rev (#stms b1) of
		 (CT.JUMP _) :: _ => ensureTargets([],b1::rTrace)
	       | (CT.CJUMP(rop,e1,e2,lT,lF)) :: r => 
		 let val newL = Temp.newlabel()
		     val newB = {start = newL,
				 stms = [CT.LABEL newL,
					 CT.JUMP(CT.LOC(CT.NAME newL),[newL])],
				 fall = NONE}
		     val newJ = CT.CJUMP(rop,e1,e2,lT,newL)
		 in
		     ensureTargets([],newB::{start = #start b1,
					     stms = rev(newJ::r),
					     fall = SOME newL}::rTrace)
		 end
	       | x => let val newL = valOf(#fall b1)
			  val newJ = CT.JUMP(CT.LOC(CT.NAME newL),[newL])
		      in 
			  ensureTargets([],{start = #start b1,
					    stms = rev(newJ::x),
					    fall = NONE}::rTrace)
		      end)
	  | ensureTargets(b1::b2::l,rTrace) = 
	    let val b2lbl = case #stms b2 of 
				(CT.LABEL x)::_ => x
			      | _ => raise Fail "Block does not start w/ labl"
		val (stms',b3) = case rev (#stms b1) of
				     (CT.JUMP _)::_ => ((#stms b1),NONE)
				   | CT.CJUMP(rop,e1,e2,lT,lF)::r => 
				     if lF = b2lbl
				     then ((#stms b1), NONE)
				     else if lT = b2lbl
				     then (rev(CT.CJUMP(T.flip rop,e1,e2,lF,lT)::r), NONE)
				     else let val newL = Temp.newlabel()
					      val newB = {start = newL,
							  stms = [CT.LABEL newL,
								  CT.JUMP(CT.LOC(CT.NAME newL),
									  [newL])],
							  fall = NONE}
					      val newJ = CT.CJUMP(rop,e1,e2,lT,newL)
					  in
					      (rev(newJ::r),SOME newB)
					  end
				   | x => if case (#fall b1) of 
						 SOME x => x = b2lbl
					       | NONE => false
					  then ((#stms b1), NONE)
					  else let val newL = Temp.newlabel()
						   val newJ = CT.JUMP(CT.LOC(CT.NAME newL),[newL])
					       in
						   (rev(newJ::x),NONE)
					       end
	    in
		ensureTargets(b2::l,case b3 of
					SOME x => x::{start = #start b1,
						      stms = stms',
						      fall = SOME (#start x)} ::rTrace

				      | NONE => {start = #start b1,
						 stms = stms',
						 fall = #fall b1}::rTrace)
	    end
	val buildMap = foldl (fn(b,m)=>LMap.insert(m,#start b, b)) LMap.empty 
	
	fun threadJumps trace  = 
	    let val changed = ref false
		val m = buildMap trace
		fun jmp' t = case LMap.find(m,t) of
				  NONE => raise Fail ("missing target: " ^ (Symbol.name t) )
				| SOME x => case #stms x of
						[CT.LABEL _, 
						 a as CT.JUMP _] => SOME a
					      | _ => NONE

		fun lp(trace',[]) = if !changed
				    then threadJumps(rev trace')
				    else rev trace'
		  | lp(trace',(a:bblock)::l) = 

		    lp({start = # start a,
			stms = (case rev( #stms a ) of
				    CT.JUMP(x, [targ]) ::r => 
				    (case jmp' targ of
					 SOME y => (changed:= true;rev (y::r))
				       | NONE => #stms a)
				  | CT.CJUMP(rop,e1,e2,lT,lF)::r =>
				    (case jmp' lT of
					 SOME(CT.JUMP(_,[lT'])) => (changed:= true;
								    rev(CT.CJUMP(rop,
										 e1,
										 e2,
										 lT',
										 lF)::r))
				       | _ => #stms a)
				  | _ => #stms a),
			fall = #fall a}::trace',l)
	    in
		lp([],trace)
	    end
	    
    in
	ensureTargets(trace, [])
    end
fun dropExtraJmps((j as CT.JUMP(_,[t]))::(l as CT.LABEL(t'))::r,x) =
    if t = t' 
    then dropExtraJmps(r, l::x)
    else dropExtraJmps(r, l::j::x)
  | dropExtraJmps([],x) = rev x
  | dropExtraJmps(a::l,x) = dropExtraJmps(l,a::x)

fun canonicalize stm =
    let val ctree = cts stm
	val b = blks ctree
	val b' = linearize b
	val b'' = cleanBlocks b'
	val stms = map #stms b''
	val lst = foldr (op @) [] stms
    in
	dropExtraJmps (lst,[])
    end
end
