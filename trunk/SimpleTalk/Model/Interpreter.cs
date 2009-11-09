using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace SimpleTalk.Model
{
  class Interpreter
  {
    private string _TextOutput;

    public string TextOutput
    {
      get
      {
        return _TextOutput;
      }
      set
      {
        _TextOutput = value;
        OnTextChanged(this, new EventArgs());
      }
    }

    public event EventHandler TextChanged;

    private void OnTextChanged(object sender, EventArgs e)
    {
      if (TextChanged != null)
      {
        TextChanged(sender, e);
      }
    }

    public void ProcessCommand(string command)
    {
      if (command.Length == 1 || Char.IsLetter(command[0]))
      {
        TextOutput += command;
      }
      else
      {
        switch (command)
        {
          case "&back":
            //back space action
            if (TextOutput.Length > 1)
            {
              TextOutput += TextOutput.Remove(TextOutput.Length - 2);
            }
            else
            {
              TextOutput = "";
            }
            break;

          case "&menu":
            //Call menu that jumps to menu form
            TextOutput += "[Menu]";
            //throw new NotImplementedException();
            break;

          case "&auto":
            //Call function that jumps to suggestions from auto completion
            throw new NotImplementedException();
            //break;

          case "&space":
            TextOutput += " ";
            break;

          case "&clear":
            TextOutput = "";
            break;
          default:
            //todo: insert nice exception handler.
            TextOutput = "ERROR";
            break;
        }
      }
    }
  }
}
