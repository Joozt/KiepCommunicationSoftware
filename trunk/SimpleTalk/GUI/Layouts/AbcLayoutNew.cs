using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Drawing;

namespace SimpleTalk.GUI
{
    class AbcLayoutNew : CustomLayout
    {
        int _DefaultWidth = 105;
        int _DefaultHeight = 90;
        int _hSpace = 4;
        int _vSpace = 4;

        override public void ConstructLayout()
        {
            int WidthEmpty = 30;
            int Width = (int)Math.Round((12 * _DefaultWidth + 1 * _vSpace - WidthEmpty) / 6.0);
          

          NewRow(
              _DefaultWidth,
              _DefaultHeight,
              new ButtonDefinition[6]
                {
                    new ButtonDefinition("SPA", " ", new Size(Width, _DefaultHeight)),
                    new ButtonDefinition("<=", "&back", new Size(Width, _DefaultHeight)),
                    new ButtonDefinition("SAY", "&say", new Size(Width, _DefaultHeight)),
                    new ButtonDefinition("123", "&123", new Size(Width, _DefaultHeight)),
                    new ButtonDefinition("", "&empty", new Size(WidthEmpty, _DefaultHeight)),
                    new ButtonDefinition("CLR", "&clear", new Size(Width, _DefaultHeight)),
                    
                });

            List<ButtonDefinition> NewButtons = new List<ButtonDefinition>();

            NewRow(0, _DefaultHeight, new ButtonDefinition[1]{new ButtonDefinition("", "", new Size(0, _DefaultHeight), false, true)});

            for ( char c = 'a'; c <= 'f'; c++)
            {
                NewButtons.Add(new ButtonDefinition( c.ToString().ToUpper(), c.ToString()));
            }

            NewButtons.Add(null);

            for (char c = 'g'; c <= 'l'; c++)
            {
                NewButtons.Add(new ButtonDefinition(c.ToString().ToUpper(), c.ToString()));
            }

            NewButtons.Add(null);

            for (char c = 'm'; c <= 'r'; c++)
            {
                NewButtons.Add(new ButtonDefinition(c.ToString().ToUpper(), c.ToString()));
            }

            NewButtons.Add(new ButtonDefinition("MENU", "&menu", new Size((int)Math.Round((2.0 * _DefaultWidth) + _vSpace), _DefaultHeight), false));

            NewButtons.Add(null);

            for (char c = 's'; c <= 'z'; c++)
            {
                NewButtons.Add(new ButtonDefinition(c.ToString().ToUpper(), c.ToString()));
            }

            NewRow(_DefaultWidth, _DefaultHeight, NewButtons);

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
