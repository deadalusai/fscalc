module Calculator.Ast

type Name = Name of string

//Any term of constant or variable value
and Term =
| Constant of double
| Variable of Name

and Expr =
//Simple term (constant or variable)
| Term      of Term
//Single-parameter function
| Function  of string * Expr
//BEDMAS operators
| Add       of Expr * Expr
| Multiply  of Expr * Expr
| Subtract  of Expr * Expr
| Divide    of Expr * Expr
| Power     of Expr * Expr
| Modulo    of Expr * Expr
//Inverters
| Negative  of Expr

and Command =
| Expr of Expr
//VarAssignment and VarDeletion ops are done en-mass
| Update of Update list

and Update =
//Assignment of the result of the given expression to memory
| Assignment of Name * Expr
//Deletion of a previously assigned variable from memory
| Deletion of Name