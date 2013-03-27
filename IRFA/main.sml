structure Main = 
struct
structure F : FRAME = MipsFrame


  fun printIRfrags fraglist = 
      let fun printList [] = print "End of frag list.\n"
	    | printList (a::l) = 
	      ((case a of 
		    F.PROC({body, frame}) => Printtree.printtree (TextIO.stdOut, body)
		  | F.STRING(name, formals) => print ("Label of string: " ^ (Symbol.name name) ^ "\n")); 
	       printList(l))
      in
	  print "Beginning of frag list: \n";
	  printList fraglist
      end

  fun typecheck file =
      let 
	  val _ = Translate.resetfraglist();
	  val _ = Temp.reset();
	  val absyn = Parse.parse file
	  val tree = Semant.transProg absyn
      in
	  PrintAbsyn.print(TextIO.stdOut, absyn);
	  printIRfrags (Translate.getResult());
	  FindEscape.findEscape absyn;
          Printtree.printtree(TextIO.stdOut, tree) 
      end

end
