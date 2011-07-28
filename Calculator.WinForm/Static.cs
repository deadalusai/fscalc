using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Microsoft.FSharp.Core;
using Calculator.Engine;

namespace WinFormsCalculator
{
    public static class Static
    {
        private static FSharpFunc<CalcCommand.Equation, string> _formatter;
        static Static()
        {
            _formatter = ExtraTopLevelOperators.PrintFormatToString<FSharpFunc<CalcCommand.Equation, string>>(
                new PrintfFormat<FSharpFunc<CalcCommand.Equation, string>, Unit, string, string, string>("%A")
            );
        }

        public static string FormatEquation(CalcCommand.Equation eq)
        {
            return _formatter.Invoke(eq);
        }
    }
}
