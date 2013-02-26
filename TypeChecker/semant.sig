signature SEMANT =
sig
    val transProg: Absyn.exp -> unit
end

structure Semant :> SEMANT =
struct
    fun checkInt ({exp,ty}, pos) = case ty of Types.INT => ()
                                | _ => error pos "integer required"
    fun checkExp ({exp,ty1}, symbol, venv, pos) =
    	let val ty2 = Symbol.look(venv, symbol)
	    in 
	       if ty1=getOpt(ty2) then ()
               else error pos "Type mismatch; expected
		"^ty2^"but received "^ty1^"\n"
	    end
    fun checkExpList(arglist, func, venv, pos) = 
	    let fun tuplify (l, f::funlist, a::arglist) = (trexp a,f,venv,pos)::l
			| (l, (), ()) = l
	    in
	      map checkExp (tuplify([],#formals getOpt(Symbol.look(fun)),arglist))
	    end
	  
    fun transProg exp = ()
    fun transExp (venv, tenv) =
      let fun trexp(Abysn.OpExp{left, oper=_, right, pos}) =
        checkInt(trexp left,pos);
	checkInt(trexp right,pos);
        {exp=(), ty=Types.INT}
   	| trexp(Absyn.IntExp a) = {exp=(), ty=Types.INT}
  	| trexp(Absyn.VarExp v) = trvar v
  	| trexp(StringExp s) = {exp=(), ty=Types.STRING}
	| trexp(CallExp{func,arglist,pos}) =
	    checkExpList(arglist,func,venv,pos);
	    {exp=(), ty=#result getOpt(Symbol.look(func))}
	| trexp(Absyn.AssignExp{var, exp, pos})=
	    let {expr,type} = trexp exp
	    	{expr2,type2} = trvar var
	    in
	     if type = type2 then ()
	     else error pos "Type mismatch in assignment";
	     {expr2,type2}
	    end
	    

     
       
       
	     
      
end
