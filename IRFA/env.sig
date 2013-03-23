signature ENV = 
sig 
  type access
  type ty
  datatype enventry = VarEntry of {ty: Types.ty, access: Translate.access}
		    | FunEntry of {formals: Types.ty list, result: Types.ty, label : Temp.label, level : Translate.level}
  val base_tenv : Types.ty Symbol.table (* predefined types *)
  val base_venv : enventry Symbol.table (* predefined functions *)
end
