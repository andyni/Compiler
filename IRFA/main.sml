structure Main = 
struct
  fun typecheck file =
      let 
	  val myParser = Parse.parse file
      in
	  Temp.reset();
	  PrintAbsyn.print(TextIO.stdOut, myParser);
          Printtree.printtree(TextIO.stdOut,Semant.transProg(Parse.parse file)) 
      end
end
