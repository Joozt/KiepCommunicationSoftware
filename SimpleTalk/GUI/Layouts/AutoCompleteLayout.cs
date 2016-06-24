using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Drawing;

namespace SimpleTalk.GUI
{
  class AutoCompleteLayout : CustomLayout
  {
    int _DefaultWidth = 665;
    int _DefaultHeight = 90;
    int _hSpace = 4;
    int _vSpace = 4;

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
        {
          //only show words that are not too long so they fit on one auto suggestion button (form design dependent)
          int maxWordLength = 18;
          if (item.Length <= maxWordLength)
          {
            AddButton(new ButtonDefinition(item.ToUpper(), "#" + item));
            Number++;
          }
        /*  else
          {
            Number--;
          }*/

        }
      }

      AutoFormat();
    }

    public override void AutoFormat()
    {
      Keyboard.AutoFormat(0, 0, _vSpace, _hSpace);
    }
  }
}
