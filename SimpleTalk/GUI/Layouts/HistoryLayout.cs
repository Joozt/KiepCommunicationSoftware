using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Drawing;

namespace SimpleTalk.GUI
{
  class HistoryLayout : CustomLayout
  {
    int _DefaultWidth = 200;
    int _BlankWidth = 50;
    int _TextWidth = 1100;
    int _DefaultHeight = 90;
    int _hSpace = 5;
    int _vSpace = 0;

    override public void ConstructLayout()
    {
      //AddButtons(new Dictionary< int, string>(), 0, 1);
      Dictionary<int, string> bla = new Dictionary<int, string>();
      bla.Add(2, "zeg maar iets");
      bla.Add(7, "zeg maar iets ook als steld het niet veel voor ");
      bla.Add(3, "dit is eigenlijk maar een zure functie");
      bla.Add(232, "Zucht");
      bla.Add(4, "zeg maar iets ook als steld het niet veel voor ");
      bla.Add(10, "dit is eigenlijk maar een zure functie");
      bla.Add(70, "Zucht");
      AddButtons(bla, 3, 6); //6 is the default max number (max lines that fit on the screen)
    }

    public void AddButtons(Dictionary<int, string> linesList, int start, int max)
    {

      NewRow(
        _DefaultWidth,
        _DefaultHeight,
        new ButtonDefinition[2]
                {
                    new ButtonDefinition("OMHOOG", "&histUp", new Size(_TextWidth/2-2, _DefaultHeight)),
                    new ButtonDefinition("OMLAAG", "&histDown", new Size(_TextWidth/2-3, _DefaultHeight)),
                    });


      int Number = 0;

      foreach (KeyValuePair<int, string> line in linesList)
      {
        if (Number >= start + max)
          break;
        else
        {
          if (Number >= start)
          {
            int maxLineLength = 20;
            if (line.Value.Length > maxLineLength)
            {
              //Strip end and add dots (...) 
              string shortLine = "";
              shortLine = line.Value.Substring(0, maxLineLength) + "...";

              NewRow(
                  _DefaultWidth,
                  _DefaultHeight,
                  new ButtonDefinition[3]
                {
                    new ButtonDefinition(shortLine.ToUpper(), "%" + line.Value.ToString(), new Size(_TextWidth, _DefaultHeight),false),
                    new ButtonDefinition("", "&empty", new Size(_BlankWidth, _DefaultHeight)),
                    new ButtonDefinition("WIS", "%%" + line.Key.ToString(), new Size(_DefaultWidth, _DefaultHeight))
                });
            }
            else
            {
              NewRow(
                      _DefaultWidth,
                       _DefaultHeight,
                      new ButtonDefinition[3]
                {
                    new ButtonDefinition(line.Value.ToUpper(), "%" + line.Value.ToString(), new Size(_TextWidth, _DefaultHeight),false),
                    new ButtonDefinition("", "&empty", new Size(_BlankWidth, _DefaultHeight)),
                    new ButtonDefinition("WIS", "%%" + line.Key.ToString(), new Size(_DefaultWidth, _DefaultHeight))
                });
            }

          }

          Number++;
        }
      }

      NewRow(
          _DefaultWidth,
          _DefaultHeight,
          new ButtonDefinition[1]
               {
                    new ButtonDefinition("MENU TERUG", "&histClose", new Size(_TextWidth, _DefaultHeight),false)
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
