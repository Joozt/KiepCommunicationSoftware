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
      throw new NotImplementedException();
    }


  }
}
