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
        _TextOutput += command;
      }
      else
      {
        switch (command)
        {
          case "&back":
            //back space action
            if (_TextOutput.Length > 1)
            {
              _TextOutput += _TextOutput.Remove(_TextOutput.Length - 2);
            }
            else
            {
              _TextOutput = "";
            }
            break;

          case "&menu":
            //Call menu that jumps to menu form
            _TextOutput += "[Menu]";
            //throw new NotImplementedException();
            break;

          case "&auto":
            //Call function that jumps to suggestions from auto completion
            throw new NotImplementedException();
            //break;

          case "&space":
            _TextOutput += " ";
            break;

          case "&clear":
            _TextOutput = "";
            break;
          default:
            //todo: insert nice exception handler.
            _TextOutput = "ERROR";
            break;
        }
      }
    }
  }
}
