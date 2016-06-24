using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace KiepCommunication.GUI
{
  public class CustomKeyPressedEventArgs : EventArgs
  {
    private string _Keys;

    public CustomKeyPressedEventArgs(string keys)
    {
      _Keys = keys;
    }

    public string Keys
    {
      get
      {
        return _Keys;
      }
    }
  }

  public delegate void CustomKeyPressedEventHandler(Object sender, CustomKeyPressedEventArgs e);
}
