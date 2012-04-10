using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
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
                var result = CalcImpl.parseLine(line);

                if (result.IsError)
                {
                    textBox2.AppendText(((CalcResult.Error) result).Item + Environment.NewLine);
                    return;
                }

                var command = ((CalcResult.Result) result).Item;

                //try execute the command against the _state
                CalcImpl.executeCommand(_state, command);

                //work out what to tell the user
                if (command.IsExpr)
                {
                    var value = CalcImpl.evalExpr(_state, ((Ast.Command.Expr)command).Item);
                    textBox2.AppendText("= ");
                    textBox2.AppendText(value.ToString(CultureInfo.InvariantCulture));
                    textBox2.AppendText(Environment.NewLine);
                }
                else 
                {
                    var update = ((Ast.Command.Update)command).Item;

                    foreach (var item in update.Where(i => i.IsAssignment).Cast<Ast.Update.Assignment>())
                    {
                        var name = CalcImpl.evalName(item.Item1);
                        var value = CalcImpl.evalExpr(_state, item.Item2);

                        textBox2.AppendText(name);
                        textBox2.AppendText(" = ");
                        textBox2.AppendText(value.ToString(CultureInfo.InvariantCulture));
                        textBox2.AppendText(Environment.NewLine);
                    }
                    foreach (var item in update.Where(i => i.IsDeletion).Cast<Ast.Update.Deletion>())
                    {
                        var name = CalcImpl.evalName(item.Item);

                        textBox2.AppendText(name);
                        textBox2.AppendText(" deleted");
                        textBox2.AppendText(Environment.NewLine);
                    }
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
