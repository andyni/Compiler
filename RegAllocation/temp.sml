(* make this an abstraction sometime *)
structure Temp : TEMP =
struct
    type temp = int
    val temps = ref 140
    val stemps = ref 100
    fun newtemp() = let val t = !temps in temps := t+1; t end
    
    fun newspectemp() = let val t = !stemps in stemps := t+1; t end

    structure Table = IntMapTable(type key = temp
				  fun getInt n = n)

    fun makestring t = "t" ^ Int.toString t

  type label = Symbol.symbol

 local structure F = Format
      fun postinc x = let val i = !x in x := i+1; i end
      val labs = ref 0
 in
    fun newlabel() = Symbol.symbol(F.format "L%d" [F.INT(postinc labs)])
    val namedlabel = Symbol.symbol
    fun resetlabs() = labs:=0
end

    fun reset() = (resetlabs(); temps:=140; stemps:=100)



end
