using System;
using System.Collections.Generic;
using System.Globalization;
using System.Windows.Forms;
using Calculator;

namespace WinFormsCalculator
{
    public partial class Form1 : Form, Implementation.IStateHost
    {
        private readonly Dictionary<string, double> _memory;
        
        public Form1()
        {
            InitializeComponent(); 
            
            _memory = new Dictionary<string, Double>() {
                { "pi", 3.14159 }
            };
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

                    //try execute the command against the _state
                    Implementation.executeCommand(this, command);
                }
            }
            catch (Exception ex)
            {
                textBox2.AppendText(ex.Message + Environment.NewLine);
            }

            textBox1.Clear();
        }

        public double GetVariable(string key)
        {
            double value;
            if (_memory.TryGetValue(key, out value))
                return value;

            throw new KeyNotFoundException("Variable " + key + " does not exist");
        }

        public void ClearVariable(string key)
        {
            if (_memory.ContainsKey(key))
                _memory.Remove(key);

            throw new KeyNotFoundException("Variable " + key + " does not exist");
        }

        public void SetVariable(string key, double value)
        {
            _memory[key] = value;

            if (key != "_")
                textBox2.AppendText(key + " ");

            textBox2.AppendText("= ");
            textBox2.AppendText(value.ToString(CultureInfo.InvariantCulture));
            textBox2.AppendText(Environment.NewLine);
        }


        public double InvokeFunction(string name, double value)
        {
            switch (name)
            {
                case "sin": return Math.Sin(value);
                case "cos": return Math.Cos(value);
                case "tan": return Math.Tan(value);
                case "sqrt": return Math.Sqrt(value);
            }

            throw new InvalidOperationException("Function not implemented: " + name);
        }
    }
}
