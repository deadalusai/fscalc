using System;
using System.Collections.Generic;
using System.Globalization;
using System.Windows.Forms;
using Calculator;
using Calculator.Implementation;
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

            _state = CalcImpl.initState(defaults);
        }

        private Unit OnCommandResult(CommandResult commandResult)
        {
            //render to the user
            if (commandResult.IsEvalResult)
            {
                var evalResult = ((CommandResult.EvalResult)commandResult);

                textBox2.AppendText("= ");
                textBox2.AppendText(evalResult.Item.ToString(CultureInfo.InvariantCulture));
                textBox2.AppendText(Environment.NewLine);
            }
            else if (commandResult.IsAssignmentResult)
            {
                var assignmentResult = ((CommandResult.AssignmentResult)commandResult);

                textBox2.AppendText(assignmentResult.Item1);
                textBox2.AppendText(" = ");
                textBox2.AppendText(assignmentResult.Item2.ToString(CultureInfo.InvariantCulture));
                textBox2.AppendText(Environment.NewLine);
            }
            else if (commandResult.IsDeletionResult)
            {
                var deletionResult = ((CommandResult.DeletionResult)commandResult);

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
                ParseResult result = CalcImpl.parseLine(line);

                if (result.IsError)
                {
                    textBox2.AppendText(((ParseResult.Error) result).Item + Environment.NewLine);
                }
                else if (result.IsCommand)
                {
                    var command = ((ParseResult.Command)result).Item;

                    var callback = FSharpFunc<CommandResult, Unit>.FromConverter(OnCommandResult);

                    //try execute the command against the _state
                    CalcImpl.executeCommand(_state, command, callback);
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
