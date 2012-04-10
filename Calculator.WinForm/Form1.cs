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
                    textBox2.AppendText(((ParseResult.Error) result).Item + Environment.NewLine);
                    return;
                }

                var command = ((ParseResult.Command)result).Item;

                //try execute the command against the _state
                var commandResult = CalcImpl.executeCommand(_state, command);

                //render to the user
                if (commandResult.IsExprResult)
                {
                    var value = ((CommandResult.ExprResult)commandResult).Item;

                    textBox2.AppendText("= ");
                    textBox2.AppendText(value.ToString(CultureInfo.InvariantCulture));
                    textBox2.AppendText(Environment.NewLine);
                }
                else 
                {
                    var update = ((CommandResult.UpdateResult)commandResult).Item;

                    foreach (var item in update.Where(i => i.IsAssignmentResult).Cast<UpdateResult.AssignmentResult>())
                    {
                        textBox2.AppendText(item.Item1);
                        textBox2.AppendText(" = ");
                        textBox2.AppendText(item.Item2.ToString(CultureInfo.InvariantCulture));
                        textBox2.AppendText(Environment.NewLine);
                    }
                    foreach (var item in update.Where(i => i.IsDeletionResult).Cast<UpdateResult.DeletionResult>())
                    {
                        textBox2.AppendText(item.Item);
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
