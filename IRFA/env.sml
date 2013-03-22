structure Env :> ENV =
struct
  type access = int
  type ty = Types.ty
  datatype enventry = VarEntry of {ty: Types.ty}
		    | FunEntry of {formals: Types.ty list, result: Types.ty}
  
  structure S = Symbol
  structure T = Types
  fun enterSymbol ((symbol, binding), environment) = S.enter (environment, symbol, binding)

  val base_tenv = foldl enterSymbol S.empty [(S.symbol "string", T.STRING), (S.symbol "int", T.INT)]
  val base_venv = foldl enterSymbol S.empty 
			[(S.symbol "print", FunEntry {formals=[T.STRING],result=T.UNIT}), 
			 (S.symbol "flush", FunEntry {formals=[],result=T.UNIT}), 
			 (S.symbol "getchar", FunEntry {formals=[],result=T.STRING}),
			 (S.symbol "ord", FunEntry {formals=[T.STRING],result=T.INT}), 
			 (S.symbol "chr", FunEntry {formals=[T.INT],result=T.STRING}), 
			 (S.symbol "size", FunEntry {formals=[T.STRING],result=T.INT}), 
			 (S.symbol "substring", FunEntry {formals=[T.STRING, T.INT, T.INT],result=T.STRING}), 
			 (S.symbol "concat", FunEntry {formals=[T.STRING, T.STRING],result=T.STRING}),
			 (S.symbol "not", FunEntry {formals=[T.INT],result=T.INT}), 
			 (S.symbol "exit", FunEntry {formals=[T.INT],result=T.BOTTOM})  
			]
end
 
