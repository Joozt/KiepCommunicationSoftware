using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Drawing;

namespace SimpleTalk.GUI
{
  public class ButtonDefinition
  {
    string _Text;
    string _Keys;
    Size _Size;

    public ButtonDefinition(string Text, string Keys)
    {
      _Text = Text;
      _Keys = Keys;
      _Size = new Size(0, 0);
    }

    public ButtonDefinition(string Text, string Keys, Size Size)
      : this(Text, Keys)
    {
      _Size = Size;
    }

    public string Text
    {
      get
      {
        return _Text;
      }
    }

    public string Keys
    {
      get
      {
        return _Keys;
      }
    }

    public Size Size
    {
      get
      {
        return _Size;
      }
    }
  }
}
