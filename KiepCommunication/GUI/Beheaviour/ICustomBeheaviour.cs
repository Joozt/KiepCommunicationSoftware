using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace KiepCommunication.GUI
{
  public interface ICustomBeheaviour
  {
    CustomKeyboard Keyboard
    {
      get;
      set;
    }

    TimeSpan Timer
    {
      get;
      set;
    }

    void Attach();
    void Detach();

    void OnButtonPressed( CustomButtonEventArgs e );
  }
}
