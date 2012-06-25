module Calculator.Ast

[<StructuredFormatDisplay("{StructuredFormatDisplay}")>]
type Name = { Key : string }
    with 
        member private t.StructuredFormatDisplay = t.Key

//Any term of constant or variable value
and Expr =
//Simple term (constant or variable)
| Constant      of double
| Variable      of Name
//Single-parameter function
| FunctionCall  of Name * Expr
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
| Assignment of VariableAssignment list

//Assignment of the result of the given expression to memory
and VariableAssignment = Name * Expr