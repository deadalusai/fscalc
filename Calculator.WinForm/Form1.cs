using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;

using Microsoft.FSharp.Core;
using Calculator.Engine;

namespace WinFormsCalculator
{
    public partial class Form1 : Form
    {
        private static CalcState _state;

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
                CalcCommand command = CalcImpl.parseLine(line);

                Double result = CalcImpl.executeCommand(_state, command);

                if (command.IsEquation)
                {
                    textBox2.AppendText("= ");
                    textBox2.AppendText(result.ToString());
                    textBox2.AppendText(Environment.NewLine);
                }
                else if (command.IsVarAssignment)
                {
                    Name name = (command as CalcCommand.VarAssignment).Item1;
                    string varName = CalcImpl.evalName(name);

                    textBox2.AppendText(varName);
                    textBox2.AppendText(" = ");
                    textBox2.AppendText(result.ToString());
                    textBox2.AppendText(Environment.NewLine);
                }
                else if (command.IsVarDeletion)
                {
                    Name name = (command as CalcCommand.VarDeletion).Item;
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
