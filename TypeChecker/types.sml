structure Types =
struct

  type unique = unit ref

  datatype ty = 
            RECORD of (Symbol.symbol * ty) list * unique
          | NIL
          | INT
          | STRING
          | ARRAY of ty * unique
	  | NAME of Symbol.symbol * ty option ref
	  | UNIT
	  | BOTTOM
  fun printTy (RECORD(list,u)) = "RECORD"
    | printTy (NIL) = "NIL"
    | printTy (INT) = "INT"
    | printTy (STRING) = "STRING"
    | printTy (ARRAY(t,u)) = "ARRAY"
    | printTy (UNIT) = "UNIT"
    | printTy (BOTTOM) = "BOTTOM"
    | printTy (NAME(s,u)) = "NAME"

end

