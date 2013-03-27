structure Main = 
struct
  fun typecheck file =
      let 
	  val absyn = Parse.parse file
      in
	  Temp.reset();
	  PrintAbsyn.print(TextIO.stdOut, absyn);
	  FindEscape.findEscape absyn;
          Printtree.printtree(TextIO.stdOut,Semant.transProg absyn) 
      end
end
