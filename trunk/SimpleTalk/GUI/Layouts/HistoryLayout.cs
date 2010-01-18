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
    int _BlankWidth = 30;
    int _TextWidth = 1015;
    int _DefaultHeight = 90;
    int _hSpace = 5;
    int _vSpace = 0;

    private DateTime _CurrentDateTime = DateTime.Now;
    private DateTime? _DownDateTime = null;
    private DateTime? _UpDateTime = null;

    override public void ConstructLayout()
    {
    }

    public void AddButtons(DateTime dateTime)
    {
        _Keyboard.ClearButtons();

        List<KeyValuePair<DateTime, string>> lines = Model.DatabaseFunctions.GetPhraseList(dateTime);

        _DownDateTime = null;

        if (lines.Count >= 7)
        {
            _DownDateTime = lines[6].Key;
        }

                NewRow(
                  _DefaultWidth,
                  _DefaultHeight,
                  new ButtonDefinition[2] 
                { new ButtonDefinition("OMHOOG", "&histUp", new Size(_TextWidth / 3 , _DefaultHeight)), 
                  new ButtonDefinition("OMLAAG", "&histDown", new Size(_TextWidth / 3, _DefaultHeight)) });

                if (lines.Count > 0)
                {
                    for (int index = 0; index <= (lines.Count >= 6 ? 5 : lines.Count - 1); index++)
                    {
                        int maxLineLength = 20;
                        if (lines[index].Value.Length > maxLineLength)
                        {
                            //Strip end and add dots (...) 
                            string shortLine = "";
                            shortLine = lines[index].Value.Substring(0, maxLineLength) + "...";

                            NewRow(
                                _DefaultWidth,
                                _DefaultHeight,
                                new ButtonDefinition[3]
                {
                    new ButtonDefinition(shortLine.ToUpper(), "%" + lines[index].Value.ToString(), new Size(_TextWidth, _DefaultHeight),false),
                    new ButtonDefinition("", "&empty", new Size(_BlankWidth, _DefaultHeight)),
                    new ButtonDefinition("WIS", "%%" + lines[index].Key.ToString(), new Size(_DefaultWidth, _DefaultHeight))
                });
                        }
                        else
                        {
                            NewRow(
                                    _DefaultWidth,
                                     _DefaultHeight,
                                    new ButtonDefinition[3]
                {
                    new ButtonDefinition(lines[index].Value.ToUpper(), "%" + lines[index].Value.ToString(), new Size(_TextWidth, _DefaultHeight),false),
                    new ButtonDefinition("", "&empty", new Size(_BlankWidth, _DefaultHeight)),
                    new ButtonDefinition("WIS", "%%" + lines[index].Key.ToString(), new Size(_DefaultWidth, _DefaultHeight))
                });
                        }
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

    public DateTime? UpDateTime
    {
        get
        {
            return _UpDateTime;
        }
        set
        {
            _UpDateTime = value;
        }
    }

    public DateTime? DownDateTime
    {
        get
        {
            return _DownDateTime;
        }
    }

    public DateTime CurrentDateTime
    {
        get
        {
            return _CurrentDateTime;
        }
    }
  }
}
