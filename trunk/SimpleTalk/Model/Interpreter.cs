﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace SimpleTalk.Model
{
  class Interpreter
  {
    private string _TextOutput;

    //added for debugging
    private List<string> _CommandOutput= new List<string>();
    
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

    //added for debugging
    public List<string> CommandOutput
    {
      get
      {
        return _CommandOutput;
      }
    }

    //added for debugging
    public void ClearCommand()
    {
      _CommandOutput.Clear();
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
              TextOutput = TextOutput.Remove(TextOutput.Length - 1);
            }
            else
            {
              TextOutput = "";
            }
            break;

          case "&menu":
            //Call menu that jumps to menu form
            _CommandOutput.Add("[menu]");
            //throw new NotImplementedException();
            break;

          case "&auto":
            //Call function that jumps to suggestions from auto completion
            
            //added for debugging
            _CommandOutput.Add("[auto]");
            break;

          case "&space":
            TextOutput += " ";
            break;

          case "&clear":
            TextOutput = "";
            break;

          default:
            //Todo: insert nice exception handler.
            _CommandOutput.Add("[ERROR]");
            break;
        }
      }
    }
  }
}