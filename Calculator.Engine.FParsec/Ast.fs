module Calculator.Ast

type Name = string

//Any term of constant or variable value
and Expr =
//Simple term (constant or variable)
| Constant      of double
| Fetch         of Name
//Single-parameter function
| FunctionCall  of Name * Expr list
//BEDMAS operators
| Add           of Expr * Expr
| Multiply      of Expr * Expr
| Subtract      of Expr * Expr
| Divide        of Expr * Expr
| Power         of Expr * Expr
| Modulo        of Expr * Expr
//Inverters
| Negative      of Expr

and Statement =
| Single of Expr
//VarAssignment ops are done en-mass
| DefinitionList of Definition list

//Assignment of the result of the given expression to memory
and Definition = 
| ValueDef of Name * Expr
| FunctionDef of Name * Name list * Expr