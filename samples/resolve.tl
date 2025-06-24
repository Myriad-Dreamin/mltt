
val _ = a => b => a(b)
val _ = a => a => a
val _ = a => b
val _ = [A: Type](B: Type) => A
val _ = [A: Type](B: Type) => C

def f0(A: Type): Type = A
def f1(A: Type): Type = f0(A)
def f(A: Type)(b: C): Type = Type
def f0: Type = A
