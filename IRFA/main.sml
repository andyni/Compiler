structure Main = 
struct
  fun typecheck file =
      let 
	  val myParser = Parse.parse file
      in
	  PrintAbsyn.print(TextIO.stdOut, myParser);
          Semant.transProg(Parse.parse file) 
      end
end
