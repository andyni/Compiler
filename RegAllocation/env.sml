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
			[(S.symbol "print", FunEntry {formals=[T.STRING],result=T.UNIT, level=Tr.outermost, label = Temp.namedlabel "tig_print" }), 
			 (S.symbol "flush", FunEntry {formals=[],result=T.UNIT, level=Tr.outermost, label = Temp.namedlabel "tig_flush" }), 
			 (S.symbol "getchar", FunEntry {formals=[],result=T.STRING, level=Tr.outermost, label = Temp.namedlabel "tig_getchar" }),
			 (S.symbol "ord", FunEntry {formals=[T.STRING],result=T.INT, level=Tr.outermost, label = Temp.namedlabel "tig_ord" }), 
			 (S.symbol "chr", FunEntry {formals=[T.INT],result=T.STRING, level=Tr.outermost, label = Temp.namedlabel "tig_chr" }), 
			 (S.symbol "size", FunEntry {formals=[T.STRING],result=T.INT, level=Tr.outermost, label = Temp.namedlabel "tig_size" }), 
			 (S.symbol "substring", FunEntry {formals=[T.STRING, T.INT, T.INT],result=T.STRING, level= Tr.outermost, label = Temp.namedlabel "tig_substring" }), 
			 (S.symbol "concat", FunEntry {formals=[T.STRING, T.STRING],result=T.STRING, level=Tr.outermost, label = Temp.namedlabel "tig_concat" }),
			 (S.symbol "not", FunEntry {formals=[T.INT],result=T.INT, level=Tr.outermost, label = Temp.namedlabel "tig_not"} ), 
			 (S.symbol "exit", FunEntry {formals=[T.INT],result=T.BOTTOM, level=Tr.outermost, label = Temp.namedlabel "tig_exit" })  
			]
end
 
