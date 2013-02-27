structure Main = 
struct
  fun typecheck file = Semant.transProg(Parse.parse file) 
end
