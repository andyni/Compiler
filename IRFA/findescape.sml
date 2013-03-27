structure FindEscape: sig val findEscape: Absyn.exp -> unit 
		      end = 
struct
  type depth = int 
  type escEnv = (depth * bool ref) Symbol.table

  fun traverseVar(env:escEnv, d:depth, Absyn.SimpleVar(id,pos)) : unit = 
      (let val SOME(depth', escape') = Symbol.look(env, id)
       in (if (d>depth') then escape':=true else ())
       end) 
    | traverseVar(env:escEnv, d:depth, Absyn.FieldVar(v,id,pos)) : unit = 
      (traverseVar(env, d, v);
       let val SOME(depth', escape') = Symbol.look(env, id)
       in (if (d>depth') then escape':=true else ())
       end) 
    | traverseVar(env:escEnv, d:depth, Absyn.SubscriptVar(v,exp,pos)) : unit = 
      (traverseVar(env, d, v); traverseExp(env, d, exp))

  and traverseExp(env:escEnv, d:depth, Absyn.VarExp v) : unit = traverseVar(env, d, v)
    | traverseExp(env:escEnv, d:depth, Absyn.NilExp) : unit = ()
    | traverseExp(env:escEnv, d:depth, Absyn.IntExp a) : unit = ()
    | traverseExp(env:escEnv, d:depth, Absyn.StringExp (s,pos)) : unit = ()
    | traverseExp(env:escEnv, d:depth, Absyn.CallExp {func,args,pos}) : unit = 
      ((map (fn a => traverseExp(env, d, a)) args); ())
    | traverseExp(env:escEnv, d:depth, Absyn.OpExp {left, oper, right, pos}) : unit = 
      (traverseExp(env, d, left); 
       traverseExp(env, d, right))
    | traverseExp(env:escEnv, d:depth, Absyn.RecordExp {fields, typ, pos}) : unit = 
      ((map (fn (s, a, pos) => traverseExp(env, d, a)) fields); ())
    | traverseExp(env:escEnv, d:depth, Absyn.SeqExp l) : unit =
      ((map (fn (a, pos) => traverseExp(env, d, a)) l); ())
    | traverseExp(env:escEnv, d:depth, Absyn.AssignExp {var, exp, pos}) : unit = 
      (traverseVar(env, d, var); 
       traverseExp(env, d, exp))
    | traverseExp(env:escEnv, d:depth, Absyn.IfExp {test, then', else', pos}) : unit = 
      (traverseExp(env, d, test); 
       traverseExp(env, d, then');
       case else' of
	   SOME(else') => (traverseExp(env, d, else')) 
	 | NONE => () )
    | traverseExp(env:escEnv, d:depth, Absyn.WhileExp {test, body, pos}) : unit = 
      (traverseExp(env, d, test); 
       traverseExp(env, d, body))
    | traverseExp(env:escEnv, d:depth, Absyn.ForExp {var, escape, lo, hi, body, pos}) : unit = 
      let val env' = (escape:=false; Symbol.enter(env, var, (d, escape)))
      in
	  (traverseExp(env, d, lo);
	   traverseExp(env, d, hi);
	   traverseExp(env', d+1, body)) 
      end
      
    | traverseExp(env:escEnv, d:depth, Absyn.BreakExp pos) : unit = ()
    | traverseExp(env:escEnv, d:depth, Absyn.LetExp {decs, body, pos}) : unit = 
      let val env' = traverseDecs(env, d, decs)
      in 
	  traverseExp(env', d, body)
      end
    | traverseExp(env:escEnv, d:depth, Absyn.ArrayExp {typ, size, init, pos}) : unit = 
      (traverseExp(env, d, size);
       traverseExp(env, d, init))

  and traverseDecs(env, d, [] : Absyn.dec list) : escEnv = env
    | traverseDecs(env, d, a::l : Absyn.dec list) : escEnv = 
      let 
	  fun enter (env:escEnv, l) = foldr (fn({name, escape, typ, pos},env) => (escape := false; Symbol.enter(env, name, (d, escape)))) env l

	  fun traverse (Absyn.VarDec {name, typ, init, pos, escape}) = 
	      (escape:=false; Symbol.enter(env, name, (d, escape)))
	    | traverse (Absyn.TypeDec tylist) = env
	    | traverse (Absyn.FunctionDec({name, params, result, body, pos}::fundec)) = 
	      (traverseExp(enter(env, params), d+1, body); traverse(Absyn.FunctionDec(fundec)))
	    | traverse (Absyn.FunctionDec([])) = env
	      
      in 
	  traverseDecs(traverse a, d, l) 
      end

  fun findEscape(prog: Absyn.exp) : unit = 
      let val depth' : depth = 0
	  val escEnv' : escEnv = Symbol.empty
      in
	  traverseExp(escEnv', depth', prog)
      end
end
