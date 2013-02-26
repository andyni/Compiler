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
	      map checkExp (tuplify([],getOpt(Symbol.look(fun)).formals,arglist))
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
	    {exp=(), ty= getOpt(Symbol.look(func)).result}
	| trexp(Absyn.AssignExp{var, exp, pos})=
	    let val {exp=_,ty=type} = trexp exp
	    	val {exp=_,ty=type2} = trvar var
	    in
	     if type = type2 then ()
	     else error pos "Type mismatch in assignment";
	     {exp,type2}
	    end
	| trexp(Absyn.IfExp{test,thenexp,elseexp,pos}) = 
	    let val {exp=_, ty=tythen} = trexp thenexp
	    	val {exp, tyelse} = trexp elseexp
	    in
	      checkInt(trexp test, pos);
	      if (isSome(tyelse)) then 
	        if (getOpt(tyelse)=tythen) then () else error pos "Type
    mismatch in if statement";
    	      {exp, tythen}
	| trexp(Absyn.WhileExp{test, body, pos}) = 
	     checkInt(trexp test, pos);
	     trexp body
	| trexp(Absyn.ForExp{var, escape, lo, hi, body, pos})=
	     checkInt(trvar var, pos);
	     checkInt(trexp lo, pos);
	     checkInt(trexp hi, pos);
	     trexp body
	| trexp(Absyn.ArrayExp{typ, size, init, pos}) = 
	    let {exp=_, ty=arrtype} = trexp init
	    in
	     checkInt(trexp size, pos);
	     if (Symbol.look(tenv, typ) = arrtype then ()
	     	else error pos "Array type does not match initial value";
	     {exp,arrtype}
	    end
	| trexp(Absyn.SeqExp l) =
	    let fun tycheckseq ([], r) = r
	           tycheckseq (a::l,r) = tycheckseq(l,trexp a)
	    in
	     tycheckseq(l,())
	    end
	| trexp(Absyn.RecordExp{fields, typ, pos}) = 
	    let fun checktypes (symbol, exp, post) = checkExp(trexp exp,
    symbol, tenv, post)
	    in
		map checktypes fields;
		{exp=(), Symbol.look(tenv, typ)}
            end
	  
	and trvar (A.SimpleVar(id,pos)) = 
	    (case Symbol.look(venv,id)
	         of SOME(Env.VarEntry{ty}) => {exp=(), ty=ty}
		| NONE => (error pos "Undefined variable " ^ Symbol.name id); exp=(), ty=Types.INT))
	| trvar(A.FieldVar(v,id,pos)) = 
	    let val {exp=_, ty=vartype} = trvar v
		fun checkList((sym,ty)::l) = if (id=sym) then ty else checkList(l)
		    checkList([]) = error pos "Id not in Record"; Types.INT
            in
		case vartype of SOME(Env.VarEntry{Types.RECORD(fieldlist,u)}) =>  {exp=(), ty=checkList(fieldlist)}
		    | _ => (error pos "Variable is not a record"; {exp=(), ty=Types.INT})
	    end
        | trvar(Absyn.SubscriptVar(v,exp,pos)) =
	    let val {exp=_, ty=vartype} = trvar v
	    in
		checkInt(trexp exp, pos);
		case vartype of SOME(Env.VarEntry{Types.ARRAY(ty,u)}) =>  {exp=(), ty=ty}
		    | _ => (error pos "Variable is not a array"; {exp=(), ty=Types.INT})
	    end
end
