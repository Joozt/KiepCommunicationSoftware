using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Drawing;
using System.Windows.Forms;

namespace KiepCommunication.GUI
{
  class AutoCompleteLayoutNew : CustomLayout
  {
    int _DefaultWidth = 665;
    int _DefaultHeight = 90;
    int _hSpace = 4;
    int _vSpace = 4;

     List<ButtonDefinition> _NewButtons = new List<ButtonDefinition>();

  
    //TODO: Remove font definition here.
     private Font _defaultFont = new Font("Consolas", 45.00F, FontStyle.Bold);

    public override void ConstructLayout()
    {
    }

    public void ClearButtons()
    {
       Keyboard.ClearButtons();
       _NewButtons.Clear();
    }

    public void AddButton(ButtonDefinition button)
    {
      Graphics dc = Keyboard.GetOwnerControl.CreateGraphics();
       _NewButtons.Add(new ButtonDefinition(button.Text, button.Keys, new Size((int) dc.MeasureString(button.Text, _defaultFont).Width + 20, _DefaultHeight)));
    }

    public void AddButtons(List<String> strings, int max)
    {
      int Number = 0;
      _NewButtons.Clear();

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

      NewRow(_DefaultWidth, _DefaultHeight, _NewButtons);

      AutoFormat();
    }

    public override void AutoFormat()
    {
      Keyboard.AutoFormat(0, 0, _vSpace, _hSpace);
    }


    private void NewRow(int DefaultWidth, int DefaultHeight, IEnumerable<ButtonDefinition> NewButtons)
    {
      RowButtons Row = _Keyboard.AddRow();
      foreach (ButtonDefinition ButtonDef in NewButtons)
      {
        if (ButtonDef == null)
        {
          Row = _Keyboard.AddRow();
          continue;
        }
        else
        {

          Row.AddButton(
              ButtonDef.Text,
              ButtonDef.Keys,
              ButtonDef.Size.IsEmpty ? new Size(DefaultWidth, DefaultHeight) : ButtonDef.Size,
              ButtonDef.AutoRescan,
              ButtonDef.Font);
        }
      }
    }
  }
}
