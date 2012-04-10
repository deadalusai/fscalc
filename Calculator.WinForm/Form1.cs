using System;
using System.Collections.Generic;
using System.Globalization;
using System.Windows.Forms;
using Calculator;
using Microsoft.FSharp.Core;

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

            _state = Implementation.initCalcState(defaults);
        }

        private Unit OnCommandResult(Implementation.CommandResult commandResult)
        {
            //render to the user
            if (commandResult.IsEvalResult)
            {
                var evalResult = ((Implementation.CommandResult.EvalResult)commandResult);

                textBox2.AppendText("= ");
                textBox2.AppendText(evalResult.Item.ToString(CultureInfo.InvariantCulture));
                textBox2.AppendText(Environment.NewLine);
            }
            else if (commandResult.IsAssignmentResult)
            {
                var assignmentResult = ((Implementation.CommandResult.AssignmentResult)commandResult);

                textBox2.AppendText(assignmentResult.Item1);
                textBox2.AppendText(" = ");
                textBox2.AppendText(assignmentResult.Item2.ToString(CultureInfo.InvariantCulture));
                textBox2.AppendText(Environment.NewLine);
            }
            else if (commandResult.IsDeletionResult)
            {
                var deletionResult = ((Implementation.CommandResult.DeletionResult)commandResult);

                textBox2.AppendText(deletionResult.Item);
                textBox2.AppendText(" deleted");
                textBox2.AppendText(Environment.NewLine);
            }

            return null;
        }

        private void button1_Click(object sender, EventArgs e)
        {
            string line = textBox1.Text;

            try
            {
                var result = Implementation.parseLine(line);

                if (result.IsError)
                {
                    textBox2.AppendText(((Implementation.ParseResult.Error) result).Item + Environment.NewLine);
                }
                else if (result.IsCommand)
                {
                    var command = ((Implementation.ParseResult.Command)result).Item;

                    var callback = FSharpFunc<Implementation.CommandResult, Unit>.FromConverter(OnCommandResult);

                    //try execute the command against the _state
                    Implementation.executeCommand(_state, command, callback);
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
