using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace SimpleTalk.GUI
{
  public class ButtonEventArgs : EventArgs
  {
    private MostButton _Button;

    public ButtonEventArgs(MostButton button)
    {
      _Button = button;
    }

    public MostButton Button
    {
      get
      {
        return _Button;
      }
    }
  }

  public delegate void ButtonEventHandler(Object sender, ButtonEventArgs e);
}
