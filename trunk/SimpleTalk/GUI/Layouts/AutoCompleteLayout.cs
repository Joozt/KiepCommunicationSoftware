using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Drawing;

namespace SimpleTalk.GUI
{
  class AutoCompleteLayout : CustomLayout
  {
    int _DefaultWidth = 300;
    int _DefaultHeight = 110;
    int _hSpace = 10;
    int _vSpace = 10;

    public override void ConstructLayout()
    {
    }

    public void ClearButtons()
    {
      Keyboard.ClearButtons();
    }

    public void AddButton(ButtonDefinition button)
    {
      RowButtons Row = Keyboard.AddRow();
      Row.AddButton(button.Text, button.Keys, new Size(_DefaultWidth, _DefaultHeight));
    }

    public void AddButtons(List<String> strings, int max)
    {
      int Number = 0;
      foreach (string item in strings)
      {
        if (Number >= max)
          break;
        else
          Number++;

        AddButton(new ButtonDefinition(item.ToUpper(), "#" + item));
      }

      AutoFormat();
    }

    public override void AutoFormat()
    {
      Keyboard.AutoFormat(0, 0, _vSpace, _hSpace);
    }
  }
}
