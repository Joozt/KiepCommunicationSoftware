using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Drawing;

namespace SimpleTalk.GUI
{
  class _123Layout : CustomLayout
  {
    int _DefaultWidth = 105;
    int _DefaultHeight = 90;
    int _hSpace = 4;
    int _vSpace = 4;

    override public void ConstructLayout()
    {
      int Width = (int)Math.Round((5 * _DefaultWidth + 1 * _vSpace) / 4.0);


      NewRow(
               _DefaultWidth,
               _DefaultHeight,
               new ButtonDefinition[3]
                {
                    new ButtonDefinition("SLUITEN", "&close123", new Size(2*Width+ _vSpace, _DefaultHeight), false),
                    new ButtonDefinition("<=", "&back", new Size(Width, _DefaultHeight)),
                    new ButtonDefinition("|<=", "&wordBack", new Size(Width, _DefaultHeight)),
                });

      NewRow(
               _DefaultWidth,
               _DefaultHeight,
               new ButtonDefinition[4]
                {
                    new ButtonDefinition("<-", "&charLeft", new Size(Width, _DefaultHeight)),
                    new ButtonDefinition("->", "&charRight", new Size(Width, _DefaultHeight)),
                    new ButtonDefinition("|<-", "&wordLeft", new Size(Width, _DefaultHeight)),
                    new ButtonDefinition("->|", "&wordRight", new Size(Width, _DefaultHeight)),
                });


      List<ButtonDefinition> NewButtons = new List<ButtonDefinition>();

      for (char c = '0'; c <= '9'; c++)
      {
        if ((c != '0') && ((c - '0') % 5 == 0))
          NewButtons.Add(null);

        NewButtons.Add(new ButtonDefinition(c.ToString(), c.ToString()));
      }

      NewRow(_DefaultWidth, _DefaultHeight, NewButtons);

      NewRow(
              _DefaultWidth,
              _DefaultHeight,
              new ButtonDefinition[3]
                {
                    new ButtonDefinition("SPA", " ", new Size(2*Width+ _vSpace, _DefaultHeight)),
                    new ButtonDefinition("?", "&questionMark", new Size(Width, _DefaultHeight)),
                    new ButtonDefinition("!", "&exclamation", new Size(Width, _DefaultHeight))
                  });

      NewRow(
        _DefaultWidth,
        _DefaultHeight,
        new ButtonDefinition[2]
                {
                    new ButtonDefinition("HOME", "&home", new Size(2*Width+ _vSpace, _DefaultHeight)),
                    new ButtonDefinition("END", "&end", new Size(2*Width+ _vSpace, _DefaultHeight))
                  });

      AutoFormat();
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
              ButtonDef.Skip,
              ButtonDef.Font);
        }
      }
    }

    public override void AutoFormat()
    {
      Keyboard.AutoFormat(0, 0, _vSpace, _hSpace);
    }
  }
}
