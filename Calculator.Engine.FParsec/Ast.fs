module Calculator.Ast

type Name = string

//Any term of constant or variable value
type Expr =
//Simple term (constant or variable)
| Const of double
| Fetch of Name
//Function calls
| Call  of Name * Expr list
//BEDMAS operators
| Add of Expr * Expr
| Mul of Expr * Expr
| Sub of Expr * Expr
| Div of Expr * Expr
| Pow of Expr * Expr
| Mod of Expr * Expr
//Inverters
| Neg of Expr

//Assignment of the result of the given expression to memory
type Definition = 
| ValueDef of Name * Expr
| FunctionDef of Name * Name list * Expr