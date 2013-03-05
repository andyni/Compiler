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
    fun getnamedty(Types.NAME(name,refty)) = getOpt(!refty,Types.BOTTOM)
    	| getnamedty (ty) = ty


    fun transTy (tenv, Absyn.NameTy(name,pos)) = getnamedty(getOpt(Symbol.look(tenv,name), Types.INT))
    | transTy (tenv, Absyn.RecordTy(fieldlist)) =
      	 let fun createRecList(a,{name,escape,typ,pos}::l) = (name, getnamedty(getOpt(Symbol.look(tenv,typ),Types.INT)))::a
    	    | createRecList(a,[]) = a
   	 in
	     Types.RECORD(createRecList([],fieldlist), ref ())
	 end
    | transTy (tenv, Absyn.ArrayTy(name,pos)) = Types.ARRAY(getnamedty(getOpt(Symbol.look(tenv,name), Types.INT)), ref ()) 

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
		    {exp=(),ty=Types.UNIT}
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
	        let val {exp=_, ty=ty1} = trexp body
		in
			checkInt(trexp test, pos);
		 	if (ty1 = Types.UNIT) then () else ErrorMsg.error pos "Unit Required";
		    	{exp=(), ty=ty1}
		end
	      | trexp(Absyn.ForExp{var, escape, lo, hi, body, pos})=
	         let val {exp=_, ty=ty1} = transExp(Symbol.enter(venv, var, Env.VarEntry{ty=Types.INT}),tenv,body) 
		in 
		 checkInt(trexp lo, pos);
		 checkInt(trexp hi, pos);
		 if (ty1 = Types.UNIT) then () else ErrorMsg.error pos "Unit Required in for loop";
      		{exp=(), ty=ty1}
		end
		

	      | trexp(Absyn.ArrayExp{typ, size, init, pos}) = 
		let val {exp=_, ty=arrtype} = trexp init
		in
		    checkInt(trexp size, pos);
		    if (getnamedty(getOpt(Symbol.look(tenv, typ),Types.NIL)) = arrtype) then ()
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
	        let val Types.RECORD(fieldlist,u) =  getnamedty(getOpt(Symbol.look(tenv,typ), Types.RECORD([], ref())))
		fun getType(f,(name,ty)::l) = (print ((Symbol.name name) ^ " " ^ (Symbol.name f) ^ "\n"); if (f=name) then ty else getType(f,l))
		  | getType(f,[]) = (ErrorMsg.error pos "No such field in record"; Types.BOTTOM)
		fun checktypes(symbol, exp, post) = if (getType(symbol,fieldlist)=(#ty (trexp exp))) then () else ErrorMsg.error pos "Type mismatch in record"
		in
		    map checktypes fields;
		    {exp=(), ty=getnamedty(getOpt(Symbol.look(tenv, typ),Types.NIL))}
		end 
		    
	    and trvar (Absyn.SimpleVar(id,pos)) = 
		(case Symbol.look(venv,id)
	          of SOME(Env.VarEntry{ty}) => {exp=(), ty=ty}
		   | _ => (ErrorMsg.error pos ("Undefined variable " ^ Symbol.name id); {exp=(), ty=Types.BOTTOM}))

	      | trvar(Absyn.FieldVar(v,id,pos)) = 
		let val {exp=_, ty=vartype} = trvar v
		    fun checkList([]) = (ErrorMsg.error pos "Id not in Record"; {exp=(), ty=Types.BOTTOM})
		      | checkList((sym,ty)::l) = if (id=sym) then {exp=(), ty=ty} else checkList(l)	
		in
		    case vartype of Types.RECORD(fieldlist,u) =>  checkList(fieldlist)
				  | _ => (ErrorMsg.error pos "Variable is not a record"; {exp=(), ty=Types.BOTTOM})
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
	    fun callTransDec (a::l) = let val {tenv=tenv', venv=venv'} = transDec(venv,tenv,a) 
				      in transDecs(venv',tenv',l)
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
      | transDec (venv,tenv,Absyn.VarDec{name,typ=SOME(rt,pos1),init,...})=
       	{tenv=tenv,venv=Symbol.enter(venv,name,Env.VarEntry{ty=getnamedty(getOpt(Symbol.look(tenv,rt),Types.BOTTOM))})}
      | transDec(venv,tenv,Absyn.TypeDec(tylist)) = 
	let
	    val nameList = []
            val typeList = []
            val _ = let 
		fun extractNameAndType ({name,ty,pos}::l) = (name::nameList; ty::typeList; ())
		  | extractNameAndType [] = ()
	    in
		extractNameAndType(tylist)
	    end
	      
            val tenv' = foldr (fn (name, env) => Symbol.enter(env, name, Types.NAME(name, ref NONE))) tenv nameList  
	    val typeList' = map (fn t => transTy (tenv',t)) typeList
	    fun update (name, typ) = let 
		val Types.NAME(n,r) = valOf(Symbol.look(tenv',name))
	    in
		r:= (SOME typ)
	    end
	    val nameTypeTuples = let
		fun tuplify (l, f::funlist, a::arglist) = tuplify((a,f)::l,funlist,arglist)
		  | tuplify (l,[],[]) = l
		  | tuplify (l,_,_) = l
	    in 
		tuplify([],typeList',nameList)
	    end
	    val _ = map update nameTypeTuples
	in
	    {tenv=tenv', venv=venv}
	end
      | transDec(venv,tenv,Absyn.FunctionDec(fundec)) = 
	   let fun addFun({name,params,body,pos,result=SOME(rt,pos1)},venv)=
	     let val SOME(result_ty) = Symbol.look(tenv,rt)
	         fun transparam {name,escape,typ,pos} = 
	     	   case Symbol.look(tenv,typ) of 
		       SOME t => {name=name, ty=getnamedty(t)}
		     | NONE => {name=name, ty=Types.BOTTOM}
	         val params' = map transparam params
	         val venv' = Symbol.enter(venv,name,Env.FunEntry{formals = map #ty params', result=getnamedty(result_ty)})
	     in
		venv'
	     end
	     | addFun({name,params,body,pos, result=NONE}, venv) = 
	     	 let fun transparam {name,escape,typ,pos} = 
	     	       case Symbol.look(tenv,typ) of 
		           SOME t => {name=name, ty=getnamedty(t)}
		         | NONE => {name=name, ty=Types.BOTTOM}
	             val params' = map transparam params
	             val venv' = Symbol.enter(venv,name,Env.FunEntry{formals = map #ty params', result=Types.UNIT})
	         in
		    venv'
    	         end
	    val venv'= foldr addFun venv fundec
	    fun checkFuns(venv) =
	       let fun doCheck({name,params,body,pos,result=SOME(rt,pos1)}) = 
		   let val SOME(result_ty) = Symbol.look(tenv,rt)
		       fun transparam {name,escape,typ,pos} = 
	                case Symbol.look(tenv,typ) of 
		          SOME t => {name=name, ty=getnamedty(t)}
		        | NONE => {name=name, ty=Types.BOTTOM}
       	               val params' = map transparam params
	    	       fun enterparam({name,ty},venv2) = Symbol.enter(venv2, name,Env.VarEntry{ty=ty})
		       val venv'' = foldr enterparam venv params'
		    in
		 	if (#ty (transExp(venv'',tenv, body)) = getnamedty(result_ty)) 
			   then () 
			   else ErrorMsg.error pos "Return type does not match declared function type"
		    end 
	       | doCheck({name,params,body,pos,result=NONE}) = 
		   let val result_ty = Types.UNIT
		       fun transparam {name,escape,typ,pos} = 
	                case Symbol.look(tenv,typ) of 
		          SOME t => {name=name, ty=getnamedty(t)}
		        | NONE => {name=name, ty=Types.BOTTOM}
       	               val params' = map transparam params
	    	       fun enterparam({name,ty},venv2) = Symbol.enter(venv2, name,Env.VarEntry{ty=ty})
		       val venv'' = foldr enterparam venv params'
		    in
		 	if (#ty (transExp(venv'',tenv, body)) = result_ty) 
			   then () 
			   else ErrorMsg.error pos "Return type does not match declared function type"
		    end 

		in
			map doCheck fundec
		end
	    in
		checkFuns(venv');
		{venv=venv', tenv=tenv}
	    end
    

    fun transProg exp = #exp (transExp(Env.base_venv,Env.base_tenv,exp))
end
