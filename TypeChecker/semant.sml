structure Semant :> SEMANT =
struct
    fun checkInt ({exp=_,ty}, pos) = case ty of Types.INT => ()
                                | _ => ErrorMsg.error pos "integer required"
    fun checkExp ({exp=(),ty1}, sym, venv, pos) =
    	let val ty2 = Symbol.look(venv, sym)
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
  	| trexp(Absyn.StringExp s) = {exp=(), ty=Types.STRING}
	| trexp(Absyn.CallExp{func,args,pos}) =
	  let
	        fun tuplify (l, f::funlist, a::arglist) = tuplify((trexp a,f)::l,funlist,arglist)
			| tuplify  (l,[],[]) = l
		fun checkTyExp({exp,ty},ty2) = if ty=ty2 then ()
						  else ErrorMsg.error pos "Type mismatch"
		val funcval = Symbol.look(venv,func)
	    in
	        case funcval of SOME(Env.FunEntry{formals,result}) =>
		    (map checkTyExp (tuplify([],formals,args)); {exp=(), ty=result})
	        | _ => (ErrorMsg.error pos "Function undefined"; {exp=(), ty=Types.INT})
	    end
	| trexp(Absyn.AssignExp{var, exp, pos}) =
	    let val {exp=_,ty=type1} = trexp exp
	    	val {exp=_,ty=type2} = trvar var
	    in
	     if type1 = type2 then ()
	     else ErrorMsg.error pos "Type mismatch in assignment";
	     {exp=(),ty=type2}
	    end
	| trexp(Absyn.IfExp{test, then', else', pos}) = 
	    let val {exp=_, ty=tythen} = trexp then'
	    val {exp=_, ty=tyelse} = trexp (getOpt(else', Absyn.IntExp(0)))
	    in
	      checkInt(trexp test, pos);
	      if (isSome(else')) then if (tyelse = tythen) then () else
	      	  ErrorMsg.error pos "Type mismatch in if statement" else ();
    	      {exp=(), ty=tythen}
            end
	| trexp(Absyn.WhileExp{test, body, pos}) = 
	    (checkInt(trexp test, pos);
	     trexp body)
	| trexp(Absyn.ForExp{var, escape, lo, hi, body, pos})=
	     (checkInt(trexp lo, pos);
	     checkInt(trexp hi, pos);
	     trexp body)
	| trexp(Absyn.ArrayExp{typ, size, init, pos}) = 
	    let val {exp=_, ty=arrtype} = trexp init
	    in
	     checkInt(trexp size, pos);
	     if (getOpt(Symbol.look(tenv, typ),Types.NIL) = arrtype) then ()
	     	else ErrorMsg.error pos "Array type does not match initial value";
	     {exp=(),ty=arrtype}
	    end
	| trexp(Absyn.SeqExp l) =
	    let fun tycheckseq ([], r) = r
	      |     tycheckseq ((exp,pos)::l,r) = tycheckseq(l,trexp exp)
	    in
	     tycheckseq(l,{exp=(), ty=Types.NIL})
	    end
	| trexp(Absyn.RecordExp{fields, typ, pos}) = 
	    let fun checktypes(symbol, exp, post) = checkExp(trexp exp,
    symbol, tenv, pos)
	    in
		map checktypes fields;
		{exp=(), ty=getOpt(Symbol.look(tenv, typ),Types.NIL)}
            end

	and trvar (Absyn.SimpleVar(id,pos)) = 
	    (case Symbol.look(venv,id)
	         of SOME(Env.VarEntry{ty}) => {exp=(), ty=ty}
		| NONE => (ErrorMsg.error pos ("Undefined variable " ^ Symbol.name id); {exp=(), ty=Types.INT}))
	| trvar(Absyn.FieldVar(v,id,pos)) = 
	    let val {exp=_, ty=vartype} = trvar v
		fun checkList([]) = ErrorMsg.error pos "Id not in Record"
		  | checkList((sym,ty)::l) = if (id=sym) then () else checkList(l)		   
            in
		case vartype of Types.RECORD(fieldlist,u) =>  (checkList(fieldlist); {exp=(), ty=vartype})
		    | _ => (ErrorMsg.error pos "Variable is not a record"; {exp=(), ty=Types.INT})
	    end
        | trvar(Absyn.SubscriptVar(v,exp,pos)) =
	    let val {exp=_, ty=vartype} = trvar v
	    in
		checkInt(trexp exp, pos);
		{exp=(), ty=vartype}
	    end
       in
	trexp(topexp)
       end
       

      
(*       fun  transDec (venv,tenv,Abysyn.VarDec{name,typ=NONE,init,...}) =
	  let val {exp,ty} = transExp(venv,tenv,init)
	  in
	   {tenv=tenv, venv=Symbol.enter(venv,name,Env.VarEntry{ty=ty})}
	  end
	| transDec(venv,tenv,Absyn.TypeDec[{name,ty}]) = {venv=venv,
	    tenv=Symbol.enter(tenv,name,transTy(tenv,ty))}
	| transDec(venv, tenv,
	    Absyn.FunctionDec[{name,params,body,pos,result=SOME(rt,pos)}]) = 
	    let val SOME(result_ty) = Symbol.look(tenv,rt)
	    fun transparam{name,typ,pos}=case Symbol.look(tenv,typ) of SOME t =>
	    	{name=name, ty=t}
	    val params' = map transparam params
	    val venv' = Symbol.enter(venv, name, Env.FunEntry{formals = map
	    	#ty params', result=result_ty})
	    fun enterparam({name,ty},venv) =
	    	Symbol.enter(venv,name,Env.VarEntry{access=(), ty=ty})
	    val venv'' = fold enterparam params' venv'
	    in
		transExp (venv'', tenv) body; {venv=venv', tenv=tenv}
	    end
*)
end
