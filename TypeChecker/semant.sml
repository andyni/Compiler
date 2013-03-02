structure Semant :> SEMANT =
struct
    fun checkInt ({exp=_,ty}, pos) = case ty of Types.INT => ()
                                | _ => ErrorMsg.error pos "integer required"
        
    fun checkExp ({exp=(),ty}, sym, venv, pos) =
    	let val ty2 = Symbol.look(venv, sym)
	    in 
	       if ty=getOpt(ty2,Types.NIL) then ()
               else ErrorMsg.error pos "Type mismatch"
	    end

    fun transTy (tenv, ty) = Types.INT
   
    fun transExp (venv, tenv, topexp) =
	let fun trexp(Absyn.OpExp{left, oper=_, right, pos}) =
                     (checkInt(trexp left,pos);
	              checkInt(trexp right,pos);
                      {exp=(), ty=Types.INT})
    	      | trexp(Absyn.IntExp a) = {exp=(), ty=Types.INT}
  	      | trexp(Absyn.VarExp v) = trvar v
	      | trexp(Absyn.NilExp) = {exp=(), ty=Types.NIL}
	      | trexp(Absyn.BreakExp pos) = {exp=(), ty=Types.BOTTOM}
  	      | trexp(Absyn.StringExp s) = {exp=(), ty=Types.STRING}
	      | trexp(Absyn.CallExp{func,args,pos}) =
		let
	            fun tuplify (l, f::funlist, a::arglist) = tuplify((trexp a,f)::l,funlist,arglist)
		      | tuplify (l,[],[]) = l
		      | tuplify (l,_,_) = (ErrorMsg.error pos "The number of parameters does not match function definition."; l)  
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
		    case else' of  
			SOME(else') => (if (tyelse = tythen) then () else ErrorMsg.error pos "Type mismatch in if statement"; {exp=(), ty=tythen})
                      | NONE => (if (tythen = Types.UNIT) then () else ErrorMsg.error pos "Unit required"; {exp=(), ty=Types.UNIT})
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

	      (* Let Expression (calls transdec) *)
	      | trexp (Absyn.LetExp{decs,body,pos}) =
		let val {venv=venv',tenv=tenv'} = transDecs(venv,tenv,decs)
		in 
		    transExp(venv',tenv', body)
		end

	      | trexp(Absyn.RecordExp{fields, typ, pos}) = 
		let fun checktypes(symbol, exp, post) = checkExp(trexp exp, symbol, tenv, pos)
		in
		    map checktypes fields;
		    {exp=(), ty=getOpt(Symbol.look(tenv, typ),Types.NIL)}
		end 
		    
	    and trvar (Absyn.SimpleVar(id,pos)) = 
		(case Symbol.look(venv,id)
	          of SOME(Env.VarEntry{ty}) => {exp=(), ty=ty}
		   | _ => (ErrorMsg.error pos ("Undefined variable " ^ Symbol.name id); {exp=(), ty=Types.BOTTOM}))

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

    and transDecs (venv, tenv, decs) = 
	let
	    fun callTransDec (a::l) = let val env = transDec(venv,tenv,a) 
				      in transDecs(venv,tenv,l)
				      end
	      | callTransDec [] = {venv=venv,tenv=tenv}
	in
	    callTransDec(decs)
	end       

    and transDec (venv,tenv,Absyn.VarDec{name,typ=NONE,init,...}) =
	let val {exp,ty} = transExp(venv,tenv,init)
	in
	    {tenv=tenv,venv=Symbol.enter(venv,name,Env.VarEntry{ty=ty})}
	end
      | transDec (venv,tenv,Absyn.VarDec{name,typ=SOME(rt,pos1),init,...}) =
	{tenv=tenv,venv=venv}
      | transDec(venv,tenv,Absyn.TypeDec[{name,ty,pos}]) = 
	{venv=venv,tenv=Symbol.enter(tenv,name,transTy(tenv,ty))}
      
      | transDec(venv,tenv,Absyn.FunctionDec[{name,params,body,pos,result=SOME(rt,pos1)}]) = 
	let val SOME(result_ty) = Symbol.look(tenv,rt)
	    fun transparam {name,escape,typ,pos}=
		case Symbol.look(tenv,typ) of 
		    SOME t => {name=name, ty=t}
                  | NONE => {name=name, ty=Types.BOTTOM} 
	    val params' = map transparam params
	    val venv' = Symbol.enter(venv, name, Env.FunEntry{formals = map #ty params', result=result_ty})
	    fun enterparam ({name,ty},venv) =
	    	Symbol.enter(venv,name,Env.VarEntry{ty=ty})
	    val venv'' = foldr enterparam venv' params'
	    in
		transExp (venv'', tenv, body); 
		{venv=venv', tenv=tenv}
	    end


    fun transProg exp = #exp (transExp(Env.base_venv,Env.base_tenv,exp))
end
