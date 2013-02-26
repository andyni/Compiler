structure Semant :> SEMANT =
struct
    fun checkInt ({exp,ty}, pos) = case ty of Types.INT => ()
                                | _ => ErrorMsg.error pos "integer required"
    fun checkExp ({exp,ty1}, symbol, venv, pos) =
    	let val ty2 = Symbol.look(venv, symbol)
	    in 
	       if ty1=getOpt(ty2,Types.NIL) then ()
               else ErrorMsg.error pos "Type mismatch"
	    end
    fun transProg exp = ()
    fun transExp (venv, tenv, topexp) =
	let fun trexp(Absyn.OpExp{left, oper=_, right, pos}) =
        (checkInt(trexp left,pos);
	checkInt(trexp right,pos);
        {exp=(), ty=Types.INT})
   	| trexp(Absyn.IntExp a) = {exp=(), ty=Types.INT}
  	| trexp(Absyn.VarExp v) = trvar v
  	| trexp(StringExp s) = {exp=(), ty=Types.STRING}
	| trexp(CallExp{func,arglist,pos}) =
	  let
	        fun tuplify (l, f::funlist, a::arglist) = tuplify((trexp a,f,venv,pos)::l,funlist,arglist)
			| tuplify  (l,[],[]) = l
		val funcval = Symbol.look(func)
	    in
		case funcval of SOME(Env.FunEntry{formals, result}) =>
			map checkExp (tuplify([],formals,arglist))
	        | _ => ();
	        case Symbol.look(func) of SOME(Env.FunEntry{formals,result}) =>
		    {exp=(), ty= result}
	        | _ => (ErrorMsg.error pos "Function undefined"; {exp=(), ty=Types.INT})
	    end
	| trexp(Absyn.AssignExp{var, exp, pos}) =
	    let val {exp=_,ty=type1} = trexp exp
	    	val {exp=_,ty=type2} = trvar var
	    in
	     if type1 = type2 then ()
	     else ErrorMsg.error pos "Type mismatch in assignment";
	     {exp=exp,ty=type2}
	    end
	| trexp(Absyn.IfExp{test,thenexp, elseexp, pos}) = 
	    let val {exp=_, ty=tythen} = trexp thenexp
		val {exp=_, ty=tyelse} = if (isSome(elseexp)) then trexp getOpt(elseexp) else {exp=(), ty=Types.INT}
	    in
	      checkInt(trexp test, pos); 
	      if (isSome(elseexp)) then 
	        if (tyelse=tythen) then () else ErrorMsg.error pos "Type mismatch in if statement" else ();
    	      {exp=exp, ty=tythen}
            end
	| trexp(Absyn.WhileExp{test, body, pos}) = 
	    (checkInt(trexp test, pos);
	     trexp body)
	| trexp(Absyn.ForExp{var, escape, lo, hi, body, pos})=
	    (checkInt(trvar var, pos);
	     checkInt(trexp lo, pos);
	     checkInt(trexp hi, pos);
	     trexp body)
	| trexp(Absyn.ArrayExp{typ, size, init, pos}) = 
	    let val {exp=_, ty=arrtype} = trexp init
	    in
	     checkInt(trexp size, pos);
	     if (Symbol.look(tenv, typ) = arrtype) then ()
	     	else ErrorMsg.error pos "Array type does not match initial value";
	     {exp=exp,ty=arrtype}
	    end
	| trexp(Absyn.SeqExp l) =
	    let fun tycheckseq ([], r) = r
	      |     tycheckseq (a::l,r) = tycheckseq(l,trexp a)
	    in
	     tycheckseq(l,())
	    end
	| trexp(Absyn.RecordExp{fields, typ, pos}) = 
	    let fun checktypes (symbol, exp, post) = checkExp(trexp exp,
    symbol, tenv, post)
	    in
		map checktypes fields;
		{exp=(), ty=Symbol.look(tenv, typ)}
            end

	and trvar (Absyn.SimpleVar(id,pos)) = 
	    (case Symbol.look(venv,id)
	         of SOME(Env.VarEntry{ty}) => {exp=(), ty=ty}
		| NONE => (ErrorMsg.error pos "Undefined variable " ^ Symbol.name id); {exp=(), ty=Types.INT})
	| trvar(Absyn.FieldVar(v,id,pos)) = 
	    let val {exp=_, ty=vartype} = trvar v
		fun checkList() = (ErrorMsg.error pos "Id not in Record"; Types.INT)
		  | checkList((sym,ty)::l) = if (id=sym) then ty else checkList(l)		   
            in
		case vartype of SOME(Env.VarEntry{ty as Types.RECORD(fieldlist)}) =>  {exp=(), ty=checkList(fieldlist)}
		    | _ => (ErrorMsg.error pos "Variable is not a record"; {exp=(), ty=Types.INT})
	    end
        | trvar(Absyn.SubscriptVar(v,exp,pos)) =
	    let val {exp=_, ty=vartype} = trvar v
	    in
		checkInt(trexp exp, pos);
		case vartype of SOME(Env.VarEntry{ty as Types.ARRAY(type1)}) =>  {exp=(), ty=type1}
		    | _ => (ErrorMsg.error pos "Variable is not a array"; {exp=(), ty=Types.INT})
	    end
	in
	    trexp(topexp)
        end
end
