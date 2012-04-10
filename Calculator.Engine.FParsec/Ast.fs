module Calculator.FParsec.Ast

type Term =
| Constant of float
| Variable of string

and Expr =
| Term      of Term
| Add       of Expr * Expr
| Multiply  of Expr * Expr
| Subtract  of Expr * Expr
| Divide    of Expr * Expr
| Power     of Expr * Expr
| Root      of Expr * Expr