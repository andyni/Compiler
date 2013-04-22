structure Env :> ENV =
struct
  type access = int
  type ty = Types.ty
  datatype enventry = VarEntry of {access: Translate.access, 
				   ty: Types.ty}
		    | FunEntry of {level: Translate.level, 
				   label: Temp.label, 
				   formals: Types.ty list, 
				   result: Types.ty}
  
  structure S = Symbol
  structure T = Types
  structure Tr = Translate
  fun enterSymbol ((symbol, binding), environment) = S.enter (environment, symbol, binding)

  val base_tenv = foldl enterSymbol S.empty [(S.symbol "string", T.STRING), (S.symbol "int", T.INT)]
  val base_venv = foldl enterSymbol S.empty 
			[(S.symbol "tig_print", FunEntry {formals=[T.STRING],result=T.UNIT, level=Tr.outermost, label = Temp.namedlabel "print" }), 
			 (S.symbol "tig_flush", FunEntry {formals=[],result=T.UNIT, level=Tr.outermost, label = Temp.namedlabel "flush" }), 
			 (S.symbol "tig_getchar", FunEntry {formals=[],result=T.STRING, level=Tr.outermost, label = Temp.namedlabel "getchar" }),
			 (S.symbol "tig_ord", FunEntry {formals=[T.STRING],result=T.INT, level=Tr.outermost, label = Temp.namedlabel "ord" }), 
			 (S.symbol "tig_chr", FunEntry {formals=[T.INT],result=T.STRING, level=Tr.outermost, label = Temp.namedlabel "chr" }), 
			 (S.symbol "tig_size", FunEntry {formals=[T.STRING],result=T.INT, level=Tr.outermost, label = Temp.namedlabel "size" }), 
			 (S.symbol "tig_substring", FunEntry {formals=[T.STRING, T.INT, T.INT],result=T.STRING, level= Tr.outermost, label = Temp.namedlabel "substring" }), 
			 (S.symbol "tig_concat", FunEntry {formals=[T.STRING, T.STRING],result=T.STRING, level=Tr.outermost, label = Temp.namedlabel "concat" }),
			 (S.symbol "tig_not", FunEntry {formals=[T.INT],result=T.INT, level=Tr.outermost, label = Temp.namedlabel "not"} ), 
			 (S.symbol "tig_exit", FunEntry {formals=[T.INT],result=T.BOTTOM, level=Tr.outermost, label = Temp.namedlabel "exit" })  
			]
end
 
