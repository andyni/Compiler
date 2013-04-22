structure Main = 
struct
structure F : FRAME = MipsFrame

   fun emitproc out (F.PROC{body,frame}) =
     let val _ = print ("emit "^ Symbol.name(F.name frame) ^ "\n")
	       val stms = Canon.linearize body
         val _ = app (fn s => Printtree.printtree(TextIO.stdOut,s)) stms; 
         val stms' = Canon.traceSchedule(Canon.basicBlocks stms)
    	   val instrss =   List.concat(map (MipsGen.codegen frame) stms') 
     	   val instrs = F.procEntryExit2(frame, instrss)
	       val (ginstr,l) =  Makegraph.instrs2graph(instrs)
	       val (intgraph,extl) = Liveness.interferenceGraph(ginstr)
	       val (instrs', allocation) = RegAlloc.alloc (instrs, frame)
	       val _ = print "\n PRECOLOREDS: "
	       val _ = map (fn(k) => print (Temp.makestring(k)^":"^valOf(Temp.Table.look(allocation,k))^",")) F.reglist
 	       val format0 = Assem.format(fn(temp)=>valOf(Temp.Table.look(allocation, temp)))
         val {prolog, body=body', epilog} = F.procEntryExit3(frame, instrs')
	    in  
	    	 TextIO.output(out, prolog);
      	 app (fn i => (TextIO.output(out,format0 i))) body';
		     TextIO.output(out, epilog)
      end

   fun emitstring out (F.STRING(lab,s)) = TextIO.output(out,F.string(lab,s))

   fun withOpenFile fname f = 
       let val out = TextIO.openOut fname
        in (f out before TextIO.closeOut out) 
	    handle e => (TextIO.closeOut out; raise e)
       end 

   fun splitFragsList (F.PROC p, (procs, strings)) = ((F.PROC p)::procs, strings)
     | splitFragsList (F.STRING s, (procs, strings)) = (procs, (F.STRING s)::strings)

   fun compile filename = 
       let val _ = Translate.resetfraglist();
           val _ = Temp.reset();
           val absyn = Parse.parse filename
           val frags = (FindEscape.findEscape absyn; Semant.transProg absyn; Translate.getResult())
           val (procs, strings) = foldr (splitFragsList) ([],[]) frags
        in 
            withOpenFile (filename ^ ".s") 
	          (fn out => ((TextIO.output(out, ".text\n.globl main\n"));
                        (map (emitproc out) procs);
                        (TextIO.output(out, "\n.data\n"));
                        (map (emitstring out) strings)))
        end

end
