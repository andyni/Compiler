structure Main = 
struct
structure F : FRAME = MipsFrame

   fun emitproc out (F.PROC{body,frame}) =
     let 
	       val stms = Canon.linearize body
 
         val stms' = Canon.traceSchedule(Canon.basicBlocks stms)
    	   val instrss =   List.concat(map (MipsGen.codegen frame) stms') 
     	   val instrs = F.procEntryExit2(frame, instrss)

	       val (instrs', allocation) = RegAlloc.alloc (instrs, frame)
 	       val format0 = Assem.format(fn(temp)=>
	       	   case Temp.Table.look(allocation, temp) of SOME(reg) => reg
							   | NONE => ErrorMsg.impossible "Requires spilling.")
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

   fun read (file, out) = let
       val input = TextIO.openIn file
       fun readfile f = case TextIO.inputLine f of 
           SOME l => l ^ readfile(f)
         | NONE => ""
       val line = readfile(input)
   in
       TextIO.output (out, line)
   end

   fun compile filename = 
       let val _ = Translate.resetfraglist();
           val _ = Temp.reset();
           val absyn = Parse.parse filename
           val frags = (FindEscape.findEscape absyn; Semant.transProg absyn; Translate.getResult())
           val (procs, strings) = foldr (splitFragsList) ([],[]) frags
        in 
            withOpenFile (filename ^ ".s") 
	          (fn out => ((TextIO.output(out, ".data\n.align 4\n"));
                        (map (emitstring out) strings);
                        (TextIO.output(out, "\n.text\n.align 4\n.globl tig_main\ntig_main:\n"));
                        (map (emitproc out) procs);
                        (read("runtimele.s", out));
                        (read("sysspim.s", out))))
        end

end
