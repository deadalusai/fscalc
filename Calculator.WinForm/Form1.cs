using System;
using System.Collections.Generic;
using System.Globalization;
using System.Windows.Forms;
using Calculator;
using Calculator.Implementation;

namespace WinFormsCalculator
{
    public partial class Form1 : Form
    {
        private static Engine.CalcState _state;

        public Form1()
        {
            InitializeComponent();

            var defaults = new Dictionary<string, Double>() {
                { "pi", 3.14159 }
            };

            _state = CalcImpl.initState(defaults);
        }

        private void button1_Click(object sender, EventArgs e)
        {
            string line = textBox1.Text;

            try
            {
                CalcResult result = CalcImpl.parseLine(line);

                if (result.IsError)
                {
                    textBox2.AppendText(((CalcResult.Error) result).Item + Environment.NewLine);
                    return;
                }

                Ast.Command command = ((CalcResult.Result) result).Item;

                double value = CalcImpl.executeCommand(_state, command);

                if (command.IsExpr)
                {
                    textBox2.AppendText("= ");
                    textBox2.AppendText(value.ToString(CultureInfo.InvariantCulture));
                    textBox2.AppendText(Environment.NewLine);
                }
                else if (command.IsVarAssignment)
                {
                    Ast.Name name = ((Ast.Command.VarAssignment) command).Item1;
                    string varName = CalcImpl.evalName(name);

                    textBox2.AppendText(varName);
                    textBox2.AppendText(" = ");
                    textBox2.AppendText(value.ToString(CultureInfo.InvariantCulture));
                    textBox2.AppendText(Environment.NewLine);
                }
                else if (command.IsVarDeletion)
                {
                    Ast.Name name = ((Ast.Command.VarDeletion) command).Item;
                    string varName = CalcImpl.evalName(name);

                    textBox2.AppendText(varName);
                    textBox2.AppendText(" deleted");
                    textBox2.AppendText(Environment.NewLine);
                }
            }
            catch (Exception ex)
            {
                textBox2.AppendText(ex.Message + Environment.NewLine);
            }

            textBox1.Clear();
        }
    }
}
