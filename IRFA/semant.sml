structure Semant :> SEMANT =
struct
    structure T = Tree
    structure Tr = Translate

    (* int reference used to determine nest level of breaks. >0 is valid.*)
    val breakLevel = ref 0

    (* checks if {exp,ty} has type int *)
    fun checkInt ({exp=_,ty}, pos) = case ty of Types.INT => ()
                                | _ => ErrorMsg.error pos "integer required"

    (* checks if {exp,ty} has type string *)
    fun checkString ({exp=_,ty}, pos) = case ty of Types.STRING => ()
                                | _ => ErrorMsg.error pos "string required"

    (* Returns element if SOME, prints error msg if NONE *)
    fun getTyOption (SOME(k), pos, errorstmt) = k
      | getTyOption (NONE, pos, errorstmt) = (ErrorMsg.error pos errorstmt; Types.BOTTOM)
        
    (* Checks if two types are the same *)
    fun checkExp ({exp=(),ty}, sym, venv, pos) =
    	let val ty2 = Symbol.look(venv, sym)
	    in 
	       if ty=getOpt(ty2,Types.NIL) then ()
               else ErrorMsg.error pos "Type mismatch"
	    end

    (* Returns actual type *)
    fun getnamedty (Types.NAME(name,refty)) = (getnamedty(getTyOption(!refty,0,"Named type does not exist")))
      | getnamedty (ty) = ty

    (* Transforms Absyn Ty's into Types *)
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
		   
    fun transExp (venv, tenv, topexp, level, break) =
	let fun trexp(Absyn.OpExp{left, oper, right, pos}) =
	    	let val {exp=exp1, ty=type1} = trexp left
		    val {exp=exp2, ty=type2} = trexp right
		in
		(* Arithmetic Operations *)
		if (oper=Absyn.PlusOp orelse oper=Absyn.MinusOp orelse oper=Absyn.TimesOp orelse oper=Absyn.DivideOp) 
		then
                    (checkInt({exp=exp1,ty=type1},pos);
	             checkInt({exp=exp2,ty=type2},pos);
		     case oper of 
		     	  Absyn.PlusOp => {exp=Tr.binop(T.PLUS,exp1,exp2), ty=Types.INT}
		       	| Absyn.MinusOp => {exp=Tr.binop(T.MINUS,exp1,exp2), ty=Types.INT}
			| Absyn.TimesOp => {exp=Tr.binop(T.MUL,exp1,exp2), ty=Types.INT}
			| Absyn.DivideOp => {exp=Tr.binop(T.DIV,exp1,exp2), ty=Types.INT})
			
		(* Comparison Operations *)
		else if (oper=Absyn.GtOp orelse oper=Absyn.LtOp orelse oper=Absyn.GeOp orelse oper=Absyn.LeOp)
		then
		    (case type1 of
			 Types.INT => (checkInt({exp=exp1,ty=type1}, pos); checkInt({exp=exp2, ty=type2}, pos))
		       | Types.STRING => (checkString({exp=exp1,ty=type1}, pos); checkString({exp=exp2,ty=type2}, pos))
		       | _ => (ErrorMsg.error pos "GE,LE,GT,LT operations not supported");
		     case oper of 
		     	 Absyn.GtOp => {exp=Tr.relop(T.GT,exp1,exp2), ty=Types.INT} 
		      |  Absyn.LtOp => {exp=Tr.relop(T.LT,exp1,exp2), ty=Types.INT} 
		      |  Absyn.GeOp => {exp=Tr.relop(T.GE,exp1,exp2), ty=Types.INT} 
		      |  Absyn.LeOp => {exp=Tr.relop(T.LE,exp1,exp2), ty=Types.INT}) 
			
		(* EQ and NEQ Comparisons *)
		else if (oper=Absyn.EqOp orelse oper=Absyn.NeqOp)
		then
		    let val myexp = case oper of 
		    	    		Absyn.EqOp =>  Tr.relop(T.EQ,exp1,exp2)
				      | Absyn.NeqOp => Tr.relop(T.NE,exp1,exp2)

		    in
			(* INT/STRING/RECORD/ARRAY types can all be compared. RECORD may be NIL *)
			(case (type1,type2) of
			     (Types.INT, Types.INT) => {exp=(myexp),ty=Types.INT}
			   | (Types.STRING, Types.STRING) => {exp=(myexp),ty=Types.INT}
			   | (Types.RECORD(_), Types.RECORD(_)) => {exp=(myexp),ty=Types.INT}
		           | (Types.ARRAY(_), Types.ARRAY(_)) => {exp=(myexp),ty=Types.INT}
			   | (Types.RECORD(_), Types.NIL) => {exp=(myexp),ty=Types.INT}
			   | (Types.NIL, Types.RECORD(_)) => {exp=(myexp),ty=Types.INT}
		           | _ => (ErrorMsg.error pos "Type mismatch in EQ/NEQ comparison."; {exp=(myexp),ty=Types.BOTTOM}))
		    end
		else (ErrorMsg.error pos "Operation not supported"; {exp=(Tr.NIL),ty=Types.BOTTOM})
		end
              (* trvar handles variable cases *)
	      | trexp(Absyn.VarExp v) = trvar v

	      (* IntExp, NilExp, StringExp have types Types.INT/NIL/STRING*)
    	      | trexp(Absyn.IntExp a) = {exp=Tr.intexp(a), ty=Types.INT}
	      | trexp(Absyn.NilExp) = {exp=(Tr.NIL), ty=Types.NIL}
  	      | trexp(Absyn.StringExp(s,pos)) =  {exp=(Tr.strexp(s)), ty=Types.STRING}

	      (* Checks if breaks are properly placed in for/while loops *)
	      | trexp(Absyn.BreakExp pos) = (if !breakLevel>0 then () 
					     else (ErrorMsg.error pos "Break not properly nested.");
	                                     {exp=(Tr.breakexp break), ty=Types.BOTTOM})

	      (* Checks types of all function parameters before function call *)
	      | trexp(Absyn.CallExp{func,args,pos}) =
		let
		    (* Checks if two types are the same. RECORD types can be RECORD or NIL. *)
		    fun checkTyExp({exp,ty},ty2) = 
			case ty2 of Types.RECORD(fs,us) =>
				    if ty=Types.NIL then () 
				    else if ty=ty2 then ()
				    else ErrorMsg.error pos "Type mismatch in function call"
				  | _ =>  if ty=ty2 then ()
					  else ErrorMsg.error pos "Type mismatch in function call"
		    val arglist = map (fn a => trexp a) args;
		    val explist = map (fn {exp=exp1, ty=ty1} => exp1) arglist
		    val funcval = Symbol.look(venv,func)
		in
	            case funcval of SOME(Env.FunEntry{formals,result,level=funlevel,label}) =>
				    (if List.length(arglist)<>List.length(formals) 
	      			     then ErrorMsg.error pos "Mismatch in number of parameters sent to function vs function definition." 
				     else (); 
				     map checkTyExp (ListPair.zip(arglist,formals)); 
				     {exp=(Tr.funcall(explist,label,level,funlevel)), ty=result})
				  | _ => (ErrorMsg.error pos "Function undefined"; {exp=Tr.NIL, ty=Types.INT})
		end

	      (* Checks if variable type and expression type are valid for assignment *)
	      | trexp(Absyn.AssignExp{var, exp, pos}) =
		let val {exp=exp1,ty=type1} = trexp exp
	    	    val {exp=exp2,ty=type2} = trvar var
		in
		    case type2 of Types.RECORD(fields,u) =>
		    	 if (type1=Types.NIL) then () else if type1 = type2 then ()
		    	    else ErrorMsg.error pos ("Type mismatch in assignment Type1: "^(Types.printTy(type1)) ^ " Type2: "^(Types.printTy(type2)))
		    | _ => if type1 = type2 then ()
		    else ErrorMsg.error pos ("Type mismatch in assignment Type1: "^(Types.printTy(type1)) ^ " Type2: "^(Types.printTy(type2)));
		    {exp=(Tr.assigncall(exp2,exp1)),ty=Types.UNIT}

		end

	      (* If else' exists, its type should match then'. Otherwise, then' should be unit. *)
	      | trexp(Absyn.IfExp{test, then', else', pos}) = 
		let val {exp=thenexp, ty=tythen} = trexp then'
	            val {exp=elseexp, ty=tyelse} = trexp (getOpt(else', Absyn.IntExp(0)))
	      	    val {exp=testexp, ty= tytest} = trexp test
		in
		    checkInt({exp=testexp, ty=tytest}, pos);
		    case else' of  
			SOME(else') => (if (tyelse = tythen) then () else ErrorMsg.error pos "Type mismatch in if statement"; {exp=(Tr.ifstm(testexp,thenexp,elseexp)), ty=tythen})
                      | NONE => (if (tythen = Types.UNIT) then () else ErrorMsg.error pos "Unit required"; {exp=Tr.iftstm(testexp,thenexp), ty=Types.UNIT})

		end

	      (* The While loop body should be TYPE.Unit *)
	      | trexp(Absyn.WhileExp{test, body, pos}) = 
	        (breakLevel:=(!breakLevel)+1;
		let val testexp = (trexp test) 
		    val breaklabel = Temp.newlabel()
		    val {exp=bodyexp, ty=ty1} = transExp(venv,tenv,body,level,breaklabel)
		in
			checkInt(testexp, pos);
		 	if (ty1 = Types.UNIT) then () else ErrorMsg.error pos "Unit Required";
			breakLevel:=(!breakLevel)-1;
		    	{exp=Tr.whileexp(#exp testexp, bodyexp, breaklabel), ty=Types.UNIT}
		end)

	      (* The For loop body should be TYPE.Unit *)
	      | trexp(Absyn.ForExp{var, escape, lo, hi, body, pos})=
	        (breakLevel:=(!breakLevel)+1;
		let val venv' = Symbol.enter(venv, var, Env.VarEntry{ty=Types.INT, access=Translate.allocLocal(level)(!escape)})
		    val access = Tr.allocLocal(level)(!escape)
		    val lo' = trexp lo
		    val hi' = trexp hi
		    val breaklabel = Temp.newlabel()
		    val {exp=body', ty=ty1} = transExp(venv',tenv,body,level,breaklabel) 
		in 
		    checkInt(lo', pos);
		    checkInt(hi', pos);
		    if (ty1 = Types.UNIT) then () else ErrorMsg.error pos "Unit Required in for loop";
		    breakLevel:=(!breakLevel)-1;
      		    {exp=Tr.forexp(Tr.simpleVar(access,level), #exp lo', #exp hi', body', breaklabel), ty=Types.UNIT}
		end)
		
	      (* Checks if array type matches initial value *)
	      | trexp(Absyn.ArrayExp{typ, size, init, pos}) = 
		let val {exp=_, ty=arrtype} = trexp init
		val rettype = getnamedty(getTyOption(Symbol.look(tenv,typ),pos,("Type of Array "^Symbol.name typ^" does not exist")))
		val Types.ARRAY(acttype,u) = rettype
		in
		    checkInt(trexp size, pos);
		    case getnamedty(acttype) of Types.RECORD(fs,us) =>
		    	 if (arrtype=Types.NIL) then () else 
			    if (getnamedty(acttype) = arrtype) then ()
	     	    	    else ErrorMsg.error pos "Array type does not match initial value"
		    | _ =>  if (getnamedty(acttype) = arrtype) then ()
	     	    	    else ErrorMsg.error pos "Array type does not match initial value";
		
		    {exp=(Tr.NIL),ty=rettype}
		end

	      (* Recursively checks each exp. Empty exps are of type UNIT *)
              | trexp(Absyn.SeqExp []) = {exp=(Tr.seqExp([], Tr.NIL)), ty=Types.UNIT}
	      | trexp(Absyn.SeqExp l) =
		let fun tycheckseq ([(exp2,pos)], {exp,ty}) = 
			let val {exp=exp1, ty=type1} = trexp exp2
			in
			    {exp=Tr.seqExp(exp,exp1), ty=type1}
			end
		      | tycheckseq ((exp2,pos)::l, {exp,ty}) = 
		      	let val {exp=exp1, ty=type1} = trexp exp2
			in
			    tycheckseq(l,{exp=Tr.getStm exp1::exp,ty=type1})
			end
		in
		    tycheckseq(l,{exp=[], ty=Types.NIL})
		end

	      (* Let Expression (calls transdec) *)
	      | trexp (Absyn.LetExp{decs,body,pos}) =
		let val {venv=venv',tenv=tenv', exp=exp1}=transDecs(venv,tenv,decs,level, break)
		    val {exp=exp2, ty=type2} = transExp(venv', tenv', body, level, break)
		in 
		    {exp=Tr.makeLetCall(exp1,exp2), ty=type2}
		end

	      (* Checks each record field type individually *)
	      | trexp(Absyn.RecordExp{fields, typ, pos}) = 
	        let 
		    fun getRecTyOption (SOME(k), pos, errorstmt) = k
      		    | getRecTyOption (NONE, pos, errorstmt) = (ErrorMsg.error pos errorstmt; Types.RECORD([], ref ()))
		    val Types.RECORD(fieldlist,u) =
			getnamedty(getRecTyOption(Symbol.look(tenv,typ),
	      pos, ("Type of record "^Symbol.name typ^" does not exist")))

		    val offsetNum = Tr.getCurrOffset(level)
		    val recPointer = Tr.allocateRec(length(fieldlist))
		    fun getType(f,(name,ty)::l) = if (f=name) then getnamedty(ty) else getType(f,l)
		      | getType(f,[]) = (ErrorMsg.error pos "No such field in record"; Types.BOTTOM)
		    fun checktypes((symbol, exp, post),set) =
			let val fieldty = getType(symbol,fieldlist)
		    	    val {exp=expexp,ty=expty} = trexp exp
			in
			    case fieldty of Types.RECORD(fs,us) => if expty = Types.NIL then () else 
								   if (fieldty=expty)
								   then () else ErrorMsg.error pos "Type mismatch in record"
					  | _ => if (fieldty=expty)
						 then () else ErrorMsg.error pos "Type mismatch in record";
	      		     Tr.allocateRec(expexp, level)::set	      		    
		        end
		     val exparr = (foldl checktypes [] fields);
		in
		    {exp=(Tr.recExp(exparr,offsetNum)), ty=getnamedty(getOpt(Symbol.look(tenv, typ),Types.NIL))}
		end 
		    
	    (* Looks in venv for variable type mapping. *)
	    and trvar (Absyn.SimpleVar(id,pos)) = 
		(case Symbol.look(venv,id)
	          of SOME(Env.VarEntry{ty,access}) => {exp=(Tr.simpleVar(access,level)), ty=getnamedty ty}
		   | _ => (ErrorMsg.error pos ("Undefined variable " ^ Symbol.name id); {exp=(Tr.NIL), ty=Types.BOTTOM}))

	      (* Record field variables *)
	      | trvar(Absyn.FieldVar(v,id,pos)) = 
		let val {exp=exp1, ty=vartype} = trvar v
		    fun checkList([],numba) = (ErrorMsg.error pos "Id not in Record"; {exp=(Tr.NIL), ty=Types.BOTTOM})
		      | checkList((sym,ty)::l,numba) = if (id=sym) then
	     	      				      {exp=Tr.fieldVar(exp1, numba), ty=getnamedty(ty)} else checkList(l, numba+1)	
		in
		    case vartype of Types.RECORD(fieldlist,u) =>  checkList(fieldlist,0)
				  | _ => (ErrorMsg.error pos "Variable is not a record"; {exp=(Tr.NIL), ty=Types.BOTTOM})
		end

	      (* Array subscript variables *)
              | trvar(Absyn.SubscriptVar(v,exp,pos)) =
		let val {exp=exp1, ty=vartype} = trvar v
		    val {exp=exp2, ty=type2} = trexp exp
		in
		    checkInt({exp=exp2,ty=type2}, pos);
		    case vartype of Types.ARRAY(acttype,u) => {exp=(Tr.subVar(exp1,exp2)),ty=getnamedty(acttype)}
		    		  | _ => (ErrorMsg.error pos "Not an array"; {exp=(Tr.NIL), ty=Types.BOTTOM})
		end
	in
	    trexp(topexp)
	end

    (* Calls transDec for each dec block. *)
    and transDecs (venv, tenv, decs, level, break) = 
	let
	    fun callTransDec (a::l) = let val {tenv=tenv', venv=venv', exp=exp} = transDec(venv,tenv,a, level, break) 
    	       			     	  val {tenv=tenv2, venv=venv2, exp=exp2} = transDecs(venv',tenv',l,level, break)
				      in {tenv=tenv2, venv=venv2, exp=exp::exp2}
				      end
	      | callTransDec [] = {venv=venv,tenv=tenv, exp=[]}
	in
	    callTransDec(decs)
	end     
  
    (* Enters variables with no type dec into venv*)
    and transDec (venv,tenv,Absyn.VarDec{name,typ=NONE,init,pos,escape}, level, break) =
	let val {exp,ty} = transExp(venv,tenv,init,level,break)
	    val access = Translate.allocLocal(level)(!escape)
	in
	    if (ty=Types.NIL) then ErrorMsg.error pos "Can't assign nil without declaring type" else ();
	    {tenv=tenv,venv=Symbol.enter(venv,name,Env.VarEntry{ty=ty,access=access}), exp=Tr.makeVar(access,exp)}
	end
      (* Enters variables with type dec into venv *)
      | transDec (venv,tenv,Absyn.VarDec{name,typ=SOME(rt,pos1),init,pos,escape}, level, break)=
      	let val type1 =
	getnamedty(getTyOption(Symbol.look(tenv,rt),pos,("Declared type of variable "^Symbol.name rt^" does not exist")))
	    val {exp,ty=type2} = transExp(venv,tenv,init,level,break) 
	    val access = Translate.allocLocal(level)(!escape)
	in
		case type1 of Types.RECORD(fieldlist,u) => 
		     if(type2=Types.NIL) then () else if (type1=type2) then () else ErrorMsg.error pos "Variable type does not match initialization"
		     | _ => if (type1=type2) then () else ErrorMsg.error pos "Variable type does not match initialization";
		{tenv=tenv,venv=Symbol.enter(venv,name,Env.VarEntry{ty=type1,access=access}), exp=Tr.makeVar(access,exp)}
	end
      (* Enters type declarations into tenv *)
      | transDec(venv,tenv,Absyn.TypeDec(tylist), level, break) = 
	let
            val nameList = let 
		fun extractName (list,{name,ty,pos}::l) = extractName(name::list,l)
		  | extractName (list,[]) = list
	    in
		extractName([], tylist)
	    end

	    (* Checks if there are repeated type names *)
	    val pos = #pos (List.nth(tylist,0))
	    val blocklist = foldr (fn (item,set) => if Symbol.look(set,item) <> NONE then (ErrorMsg.error pos "Repeated type declaration in mutually recursive block."; set) else Symbol.enter(set,item,0)) Symbol.empty nameList

	    val typeList = let 
		fun extractType (list,{name,ty,pos}::l) = extractType(ty::list,l)
		  | extractType (list,[]) = list
	    in
		extractType([], tylist)
	    end	    
  
            val tenv' = foldr (fn (name, env) => Symbol.enter(env, name, Types.NAME(name, ref NONE))) tenv nameList  
	    val typeList' = map (fn t => transTy (tenv',t)) typeList
	    
	    val nameTypeTuples = ListPair.zip(nameList, typeList')

	    (* Checks if there are infinite loops in type decs *)
	    fun checknamedty (Types.NAME(name,refty), name2, pos) = 
	    	(if name=name2 then (ErrorMsg.error pos "Can not use mutually recursive types except through records/arrays"; false)
	    	else case !refty of SOME(k) => checknamedty(k,name2,pos)
		| NONE => true)
      	    | checknamedty (ty,name,pos) = true

	    fun update (name, typ) = let 
		val Types.NAME(n,r) = valOf(Symbol.look(tenv',name))
	    in
		if checknamedty(typ,name,pos) then r:= (SOME typ) else r:=(SOME Types.BOTTOM)
	    end
	    
	    fun printTypes (name,typ) = let
	    	val SOME(k) = Symbol.look(tenv',name)
		in
		print ("Type of "^(Symbol.name name) ^" "^(Types.printTy(getnamedty(k)))^"\n")
		end
	in
	    (map update nameTypeTuples;
	    {tenv=tenv', venv=venv, exp=T.SEQ([])})
	end

      (* Enters functions into tenv *)
      | transDec(venv,tenv,Absyn.FunctionDec(fundec), level, break) = 
	   let val _ = foldr (fn ({name,pos,...},set) => 
	    	if Symbol.look(set,name) <> NONE then (ErrorMsg.error pos
	    	"Repeated function declaration in mutually recursive block."; set) 
		else Symbol.enter(set,name,0)) Symbol.empty fundec
	   fun addFun({name,params,body,pos,result=SOME(rt,pos1)},venv)=
	     let val result_ty =
	    	    getTyOption(Symbol.look(tenv,rt),pos,("Declared function type "^Symbol.name rt^" does not exist"))
	         fun transparam {name,escape,typ,pos} =
	    	    {name=name, ty=getnamedty(getTyOption(Symbol.look(tenv,typ),pos,("Declared parameter type "^Symbol.name typ^" does not exist")))}

	         val params' = map transparam params
		 val paramesc = foldl (fn({name, escape, typ, pos},ans)=>(!escape :: ans)) [] params
		 val funlabel = Temp.newlabel()
	         val venv' = Symbol.enter(venv,name,Env.FunEntry{formals =map #ty params', result=getnamedty(result_ty), level=Translate.newLevel{parent=level,name=funlabel,formals=true::paramesc}, label = funlabel})
	     in
		venv'
	     end
	     | addFun({name,params,body,pos, result=NONE}, venv) = 
	     	 let fun transparam {name,escape,typ,pos} = 
		     {name=name, ty=getnamedty(getTyOption(Symbol.look(tenv,typ),pos,("Declared parameter type "^Symbol.name typ^" does not exist")))}
	             val params' = map transparam params
		     val paramesc = foldl (fn({name, escape, typ, pos},ans)=>(!escape :: ans)) [] params
		     val funlabel = Temp.newlabel()
	             val venv' =  Symbol.enter(venv,name,Env.FunEntry{formals = map #ty params', result=Types.UNIT, level=Translate.newLevel{parent=level,name=funlabel,formals=true::paramesc}, label=funlabel})
	         in
		    venv'
    	         end
	    val venv'= foldr addFun venv fundec
	    fun checkFuns(venv) =
	       let fun doCheck({name,params,body,pos,result=SOME(rt,pos1)}) = 
		   let val result_ty =
	    	      getTyOption(Symbol.look(tenv,rt),pos,("Declared function type "^Symbol.name rt^" does not exist"))
		       fun transparam {name,escape,typ,pos} = 
		        {name=name,
		 ty=getnamedty(getTyOption(Symbol.look(tenv,typ),pos,("Declared parameter type "^Symbol.name typ^" does not exist"))), escape=escape}
       	               val params' = map transparam params
		       val SOME(Env.FunEntry{level=mylevel,...}) = (Symbol.look(venv', name))
	    	       fun enterparam({name,ty,escape},venv2) =
		 Symbol.enter(venv2, name,Env.VarEntry{ty=ty, access=Translate.allocLocal(mylevel)(!escape)})
		       val venv'' = foldr enterparam venv params'
		       val {exp=_, ty=tytrans} = transExp(venv'',tenv,body,mylevel,break)
		    in
			case getnamedty(result_ty) of Types.RECORD(fs,us)=> 
		 	if (tytrans = Types.NIL) then () else if ((tytrans) = getnamedty(result_ty)) 
			   then () 
			   else ErrorMsg.error pos "Return type does not match declared function type"
		 	|_ => if ((tytrans) = getnamedty(result_ty)) 
			   then () 
			   else ErrorMsg.error pos "Return type does not match declared function type"
		    end 
	       | doCheck({name,params,body,pos,result=NONE}) = 
		   let val result_ty = Types.UNIT
		       fun transparam {name,escape,typ,pos} = 
		         {name=name, ty=getnamedty(getTyOption(Symbol.look(tenv,typ),pos,("Declared parameter type "^Symbol.name typ^" does not exist"))),escape=escape}
       	               val params' = map transparam params
		       val SOME(Env.FunEntry{level=mylevel,...}) = Symbol.look(venv', name)
	    	       fun enterparam({name,ty,escape},venv2) =
		 Symbol.enter(venv2, name,Env.VarEntry{ty=ty, access=Translate.allocLocal(mylevel)(!escape)})
		       val venv'' = foldr enterparam venv params'
		    in
		 	if (#ty (transExp(venv'',tenv, body, level,break)) = result_ty) 
			   then () 
			   else ErrorMsg.error pos "Return type does not match declared function type"
		    end 

		in
			map doCheck fundec
		end
	    in
		checkFuns(venv');
		{venv=venv', tenv=tenv, exp=T.SEQ([])}
	    end
    
    (* Top level recursive function that takes in venv', tenv', and Absyn tree *)
    fun transProg exp = 
	let val breakLabel = Temp.newlabel()
	    val {exp=exp1,ty}=(transExp(Env.base_venv,Env.base_tenv,exp, Translate.outermost, breakLabel))
	in
		exp1
	end
end
