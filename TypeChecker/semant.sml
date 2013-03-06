structure Semant :> SEMANT =
struct
    val breakLevel = ref 0

    fun checkInt ({exp=_,ty}, pos) = case ty of Types.INT => ()
                                | _ => ErrorMsg.error pos "integer required"

    fun getTyOption (SOME(k), pos, errorstmt) = k
      | getTyOption (NONE, pos, errorstmt) = (ErrorMsg.error pos errorstmt; Types.BOTTOM)
        
    fun checkExp ({exp=(),ty}, sym, venv, pos) =
    	let val ty2 = Symbol.look(venv, sym)
	    in 
	       if ty=getOpt(ty2,Types.NIL) then ()
               else ErrorMsg.error pos "Type mismatch"
	    end
    fun getnamedty (Types.NAME(name,refty)) = getnamedty(getTyOption(!refty,0,"Named type does not exist"))
      | getnamedty (ty) = ty

    fun transTy (tenv, Absyn.NameTy(name,pos)) =
    	getTyOption(Symbol.look(tenv,name), pos, ("Type "^Symbol.name name^" does not exist"))
      | transTy (tenv, Absyn.RecordTy(fieldlist)) =
      	let fun createRecList(a,{name,escape,typ,pos}::l) =
    		createRecList((name, getTyOption(Symbol.look(tenv,typ),pos,("Record field type "^Symbol.name typ^" does not exist" )))::a,l)
    	      | createRecList(a,[]) = a
   	in
	    Types.RECORD(createRecList([],fieldlist), ref ())
	end
      | transTy (tenv, Absyn.ArrayTy(name,pos)) =
    	Types.ARRAY(getTyOption(Symbol.look(tenv,name),pos, "Type of Array "^Symbol.name name^" does not exist"), ref ()) 
		   
    fun transExp (venv, tenv, topexp) =
	let fun trexp(Absyn.OpExp{left, oper=_, right, pos}) =
                     (checkInt(trexp left,pos);
	              checkInt(trexp right,pos);
                      {exp=(), ty=Types.INT})
    	      | trexp(Absyn.IntExp a) = {exp=(), ty=Types.INT}
  	      | trexp(Absyn.VarExp v) = trvar v
	      | trexp(Absyn.NilExp) = {exp=(), ty=Types.NIL}
	      | trexp(Absyn.BreakExp pos) = (if !breakLevel>0 then () 
					     else (ErrorMsg.error pos "Break not properly nested.");
	                                     {exp=(), ty=Types.BOTTOM})
  	      | trexp(Absyn.StringExp s) = {exp=(), ty=Types.STRING}
	      | trexp(Absyn.CallExp{func,args,pos}) =
		let
	            fun tuplify (l, f::funlist, a::arglist) = tuplify((trexp a,f)::l,funlist,arglist)
		      | tuplify (l,[],[]) = l
		      | tuplify (l,_,_) = (ErrorMsg.error pos "The number of parameters does not match function definition."; l)  
		    fun checkTyExp({exp,ty},ty2) = if ty=ty2 then ()
						   else ErrorMsg.error pos "Type mismatch in function call"
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
		    else ErrorMsg.error pos ("Type mismatch in assignment Type1: "^(Types.printTy(type1)) ^ " Type2: "^(Types.printTy(type2)));
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
	        (breakLevel:=(!breakLevel)+1;
		let val {exp=_, ty=ty1} = trexp body
		in
			checkInt(trexp test, pos);
		 	if (ty1 = Types.UNIT) then () else ErrorMsg.error pos "Unit Required";
			breakLevel:=(!breakLevel)-1;
		    	{exp=(), ty=Types.UNIT}
		end)

	      | trexp(Absyn.ForExp{var, escape, lo, hi, body, pos})=
	        (breakLevel:=(!breakLevel)+1;
		let val venv' = Symbol.enter(venv, var, Env.VarEntry{ty=Types.INT})
		    val {exp=_, ty=ty1} = transExp(venv',tenv,body) 
		in 
		    checkInt(trexp lo, pos);
		    checkInt(trexp hi, pos);
		    if (ty1 = Types.UNIT) then () else ErrorMsg.error pos "Unit Required in for loop";
		    breakLevel:=(!breakLevel)-1;
      		    {exp=(), ty=Types.UNIT}
		end)
		

	      | trexp(Absyn.ArrayExp{typ, size, init, pos}) = 
		let val {exp=_, ty=arrtype} = trexp init
		val rettype = getnamedty(getTyOption(Symbol.look(tenv,typ),pos,("Type of Array "^Symbol.name typ^" does not exist")))
		val Types.ARRAY(acttype,u) = rettype
		in
		    checkInt(trexp size, pos);
		    print (Types.printTy(arrtype) ^ "\n");
		    print (Types.printTy(getnamedty(acttype)) ^ "\n");
		    if (getnamedty(acttype) = arrtype) then ()
	     	    else ErrorMsg.error pos "Array type does not match initial value";
		    {exp=(),ty=rettype}
		end

	      | trexp(Absyn.SeqExp l) =
		let fun tycheckseq ([], r) = r
		      | tycheckseq ((exp,pos)::l,r) = tycheckseq(l,trexp exp)
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
	        let val Types.RECORD(fieldlist,u) =
	          getnamedty(getTyOption(Symbol.look(tenv,typ), pos, ("Type of record "^Symbol.name typ^" does not exist")))
		fun getType(f,(name,ty)::l) = if (f=name) then getnamedty(ty) else getType(f,l)
		  | getType(f,[]) = (ErrorMsg.error pos "No such field in record"; Types.BOTTOM)
		fun checktypes(symbol, exp, post) = if (getType(symbol,fieldlist)=(#ty (trexp exp))) then () else ErrorMsg.error pos "Type mismatch in record"
		in
		    map checktypes fields;
		    {exp=(), ty=getnamedty(getOpt(Symbol.look(tenv, typ),Types.NIL))}
		end 
		    
	    and trvar (Absyn.SimpleVar(id,pos)) = 
		(case Symbol.look(venv,id)
	          of SOME(Env.VarEntry{ty}) => {exp=(), ty=getnamedty ty}
		   | _ => (ErrorMsg.error pos ("Undefined variable " ^ Symbol.name id); {exp=(), ty=Types.BOTTOM}))

	      | trvar(Absyn.FieldVar(v,id,pos)) = 
		let val {exp=_, ty=vartype} = trvar v
		    fun checkList([]) = (ErrorMsg.error pos "Id not in Record"; {exp=(), ty=Types.BOTTOM})
		      | checkList((sym,ty)::l) = if (id=sym) then {exp=(), ty=getnamedty(ty)} else checkList(l)	
		in
		    case vartype of Types.RECORD(fieldlist,u) =>  checkList(fieldlist)
				  | _ => (ErrorMsg.error pos "Variable is not a record"; {exp=(), ty=Types.BOTTOM})
		end

              | trvar(Absyn.SubscriptVar(v,exp,pos)) =
		let val {exp=_, ty=vartype} = trvar v
		in
		    checkInt(trexp exp, pos);
		    print ("TYPEA : "^ Types.printTy(vartype)^"\n");
		    case vartype of Types.ARRAY(acttype,u) => {exp=(),ty=getnamedty(acttype)}
		    		  | _ => (ErrorMsg.error pos "Not an array"; {exp=(), ty=Types.BOTTOM})
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
	    print ("Var "^Symbol.name name^" has type "^Types.printTy(ty)^"\n");
	    {tenv=tenv,venv=Symbol.enter(venv,name,Env.VarEntry{ty=ty})}
	end
      | transDec (venv,tenv,Absyn.VarDec{name,typ=SOME(rt,pos1),init,pos,...})=
      	let val type1 =
	getnamedty(getTyOption(Symbol.look(tenv,rt),pos,("Declared type of variable "^Symbol.name rt^" does not exist")))
	    val {exp,ty=type2} = transExp(venv,tenv,init) 
	in
		if (type1=type2) then () else ErrorMsg.error pos "Variable type does not match initialization";
		{tenv=tenv,venv=Symbol.enter(venv,name,Env.VarEntry{ty=type1})}
	end
      | transDec(venv,tenv,Absyn.TypeDec(tylist)) = 
	let
            val nameList = let 
		fun extractName (list,{name,ty,pos}::l) = extractName(name::list,l)
		  | extractName (list,[]) = list
	    in
		extractName([], tylist)
	    end

	    val typeList = let 
		fun extractType (list,{name,ty,pos}::l) = extractType(ty::list,l)
		  | extractType (list,[]) = list
	    in
		extractType([], tylist)
	    end	    
  
            val tenv' = foldr (fn (name, env) => Symbol.enter(env, name, Types.NAME(name, ref NONE))) tenv nameList  
	    val typeList' = map (fn t => transTy (tenv',t)) typeList
	    
	    val nameTypeTuples = let
		fun tuplify (l, f::funlist, a::arglist) = tuplify((a,f)::l,funlist,arglist)
		  | tuplify (l,[],[]) = l
		  | tuplify (l,_,_) = l
	    in 
		tuplify([],typeList',nameList)
	    end

	    fun update (name, typ) = let 
		val Types.NAME(n,r) = valOf(Symbol.look(tenv',name))
	    in
	       	r:= (SOME typ)
	    end
	    
	    fun printTypes (name,typ) = let
	    	val SOME(k) = Symbol.look(tenv',name)
		in
		print ("Type of "^(Symbol.name name) ^" "^(Types.printTy(getnamedty(k)))^"\n")
		end
	in
	    (map update nameTypeTuples;
	    map printTypes nameTypeTuples;
	    {tenv=tenv', venv=venv})
	end
      | transDec(venv,tenv,Absyn.FunctionDec(fundec)) = 
	   let fun addFun({name,params,body,pos,result=SOME(rt,pos1)},venv)=
	     let val result_ty =
	    	    getTyOption(Symbol.look(tenv,rt),pos,("Declared function type "^Symbol.name rt^" does not exist"))
	         fun transparam {name,escape,typ,pos} =
	    	    {name=name, ty=getnamedty(getTyOption(Symbol.look(tenv,typ),pos,("Declared parameter type "^Symbol.name typ^" does not exist")))}

	         val params' = map transparam params
	         val venv' = Symbol.enter(venv,name,Env.FunEntry{formals = map #ty params', result=getnamedty(result_ty)})
	     in
		venv'
	     end
	     | addFun({name,params,body,pos, result=NONE}, venv) = 
	     	 let fun transparam {name,escape,typ,pos} = 
		     {name=name, ty=getnamedty(getTyOption(Symbol.look(tenv,typ),pos,("Declared parameter type "^Symbol.name typ^" does not exist")))}
	             val params' = map transparam params
	             val venv' = Symbol.enter(venv,name,Env.FunEntry{formals = map #ty params', result=Types.UNIT})
	         in
		    venv'
    	         end
	    val venv'= foldr addFun venv fundec
	    fun checkFuns(venv) =
	       let fun doCheck({name,params,body,pos,result=SOME(rt,pos1)}) = 
		   let val result_ty =
	    	      getTyOption(Symbol.look(tenv,rt),pos,("Declared function type "^Symbol.name rt^" does not exist"))
		       fun transparam {name,escape,typ,pos} = 
		        {name=name, ty=getnamedty(getTyOption(Symbol.look(tenv,typ),pos,("Declared parameter type "^Symbol.name typ^" does not exist")))}
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
		         {name=name, ty=getnamedty(getTyOption(Symbol.look(tenv,typ),pos,("Declared parameter type "^Symbol.name typ^" does not exist")))}
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
