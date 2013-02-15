fun runtest filename = 
  let val myParser = Parse.parse filename
in
  PrintAbsyn.print(TextIO.stdOut, myParser)
end
