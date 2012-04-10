using Calculator;
using Microsoft.FSharp.Core;

namespace WinFormsCalculator
{
    public static class Static
    {
        private static FSharpFunc<Ast.Command.Expr, string> _formatter;
        static Static()
        {
            _formatter = ExtraTopLevelOperators.PrintFormatToString<FSharpFunc<Ast.Command.Expr, string>>(
                new PrintfFormat<FSharpFunc<Ast.Command.Expr, string>, Unit, string, string, string>("%A")
            );
        }

        public static string FormatEquation(Ast.Command.Expr eq)
        {
            return _formatter.Invoke(eq);
        }
    }
}
