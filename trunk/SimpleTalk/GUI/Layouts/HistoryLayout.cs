using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Drawing;

namespace SimpleTalk.GUI
{
  class HistoryLayout : CustomLayout
  {
    int _DefaultWidth = 100;
    int _BlankWidth = 20;
    int _TextWidth = 800;
    int _DefaultHeight = 90;
    int _hSpace = 5;
    int _vSpace = 0;

    override public void ConstructLayout()
    {
      //AddButtons(new Dictionary< int, string>(), 0, 1);
      Dictionary<int, string> bla = new Dictionary<int, string>(); 
      bla.Add(2,"zeg maar iets");
      AddButtons(bla , 0, 1);
    }

     public void AddButtons(Dictionary< int, string> linesList, int start, int max)
    {

             NewRow(
               _DefaultWidth,
               _DefaultHeight,
               new ButtonDefinition[2]
                {
                    new ButtonDefinition("OMHOOG", "&histUp", new Size(_TextWidth/2, _DefaultHeight)),
                    new ButtonDefinition("OMLAAG", "&&histDown", new Size(_TextWidth/2, _DefaultHeight)),
                    });


      int Number = 0;
     
       foreach(KeyValuePair<int, string> line in linesList)
      {
        if (Number >= start + max)
          break;
        else
        {
          if (Number > start)
          {
            int maxLineLength = 15;
            if (line.Value.Length > maxLineLength)
            {
              NewRow(
                  _DefaultWidth,
                  _DefaultHeight,
                  new ButtonDefinition[3]
                {
                    new ButtonDefinition(line.Value, "%" + line.Key.ToString(), new Size(_TextWidth, _DefaultHeight),false),
                    new ButtonDefinition("", "&empty", new Size(_BlankWidth, _DefaultHeight)),
                    new ButtonDefinition("WIS", "%del_" + line.Key.ToString(), new Size(_DefaultWidth, _DefaultHeight))
                });
              Number++;
            }
            else
            {

            }

          }
        /*  else
          {
            Number--;
          }*/

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
