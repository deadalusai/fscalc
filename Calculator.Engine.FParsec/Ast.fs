module Calculator.Ast

type Name = Name of string

and Term =
| Constant of double
| Variable of Name

and Expr =
| Term      of Term
| Add       of Expr * Expr
| Multiply  of Expr * Expr
| Subtract  of Expr * Expr
| Divide    of Expr * Expr
| Power     of Expr * Expr
| Modulo    of Expr * Expr
| Function  of Function

and Function =
| Sqrt      of Expr
| Sin       of Expr
| Cos       of Expr
| Tan       of Expr

and Command =
| Expr of Expr
| VarAssignment of Name * Expr
| VarDeletion of Name