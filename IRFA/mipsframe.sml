structure MipsFrame : FRAME =
struct

type frame = {name: Temp.label, forms: bool list, num: int ref}
datatype access = InFrame of int 
		| InReg of Temp.temp

val wordsize = 4

fun newFrame {name = name, forms = forms} = {name=name, forms=forms, num = ref 0}

fun name f = #name f

fun forms f = let val locals = #num f
		  fun createAcessList (0,l) = l
		    | createAcessList (n,l) = createAccessList(n+wordsize,InFrame(n)::l)
	      in
		  createAccessList(locals, [])
	      end

fun allocLocal f = (fn boolean => case boolean of
				      true => ((#num f) := !(#num f) - wordsize; InFrame(!(#num f)))
				    | false => InReg(Temp.newTemp))


end
