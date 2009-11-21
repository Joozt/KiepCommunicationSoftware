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
    bool _AutoRescan;

    public ButtonDefinition(string text, string keys)
    {
      _AutoRescan = true;
      _Text = text;
      _Keys = keys;
      _Size = new Size(0, 0);
    }

    public ButtonDefinition(string text, string keys, bool autoRescan)
        : this(text, keys)
    {
        _AutoRescan = autoRescan;
    }

    public ButtonDefinition(string text, string keys, Size size)
      : this(text, keys)
    {
      _Size = size;
    }

    public ButtonDefinition(string text, string keys, Size size, bool autoRescan)
        : this(text, keys, size)
    {
        _AutoRescan = autoRescan;
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

    public bool AutoRescan
    {
        get
        {
            return _AutoRescan;
        }
    }
  }
}
