structure Main = 
struct
structure F : FRAME = MipsFrame

   fun emitproc out (F.PROC{body,frame}) =
     let val _ = print ("emit "^ Symbol.name(F.name frame) ^ "\n")
	       val stms = Canon.linearize body
         val _ = app (fn s => Printtree.printtree(out,s)) stms; 
         val stms' = Canon.traceSchedule(Canon.basicBlocks stms)
         val stms'' = Tree.LABEL(MipsFrame.name(frame))::stms'
	       val instrs =   List.concat(map (MipsGen.codegen frame) stms'') 
	       val (ginstr,l) =  Makegraph.instrs2graph(instrs)
	       val (intgraph,extl) = Liveness.interferenceGraph(ginstr)
	       val _ = (print "Interference Graph: \n"; Liveness.show(out,intgraph))
         val (instrs', allocation) = RegAlloc.alloc (instrs, frame)
         val format0 = Assem.format(fn(temp)=> valOf(Temp.Table.look(allocation, temp)))
	    in  
      	  app (fn i => (TextIO.output(out,format0 i))) instrs
      end
    | emitproc out (F.STRING(lab,s)) = TextIO.output(out,F.string(lab,s))

   fun withOpenFile fname f = 
       let val out = TextIO.openOut fname
        in (f out before TextIO.closeOut out) 
	    handle e => (TextIO.closeOut out; raise e)
       end 

   fun compile filename = 
       let val _ = Translate.resetfraglist();
           val _ = Temp.reset();
           val absyn = Parse.parse filename
           val frags = (FindEscape.findEscape absyn; Semant.transProg absyn; Translate.getResult())
        in 
            withOpenFile (filename ^ ".s") 
	     (fn out => (map (emitproc out) frags))
        end

end
