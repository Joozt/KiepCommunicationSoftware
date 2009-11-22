﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Drawing;

namespace SimpleTalk.GUI
{
    class AbcLayout : CustomLayout
    {
        int _DefaultWidth = 90;
        int _DefaultHeight = 90;
        int _hSpace = 4;
        int _vSpace = 4;

        override public void ConstructLayout()
        {
            int Width = (int)Math.Round(((3/2.0) * _DefaultWidth) + (0.5 * _vSpace));

            NewRow(
                _DefaultWidth,
                _DefaultHeight,
                new ButtonDefinition[4]
                {
                    new ButtonDefinition("SPA", " ", new Size(Width, _DefaultHeight)),
                    new ButtonDefinition(">>", "&auto", new Size(Width, _DefaultHeight), false),
                    new ButtonDefinition("<=", "&back", new Size(Width, _DefaultHeight)),
                    new ButtonDefinition("CLR", "&clear", new Size(Width, _DefaultHeight)),
                });
    
            List<ButtonDefinition> NewButtons = new List<ButtonDefinition>();
            
            for ( char c = 'a'; c <= 'z'; c++)
            {
                if ((c != 'a') && ((c - 'a') % 6 == 0))
                    NewButtons.Add(null);

                NewButtons.Add(new ButtonDefinition( c.ToString().ToUpper(), c.ToString()));
            }

            Width = (int)Math.Round((2.0 * _DefaultWidth) + _vSpace);

            NewButtons.Add(new ButtonDefinition("MENU", "&menu", new Size(Width, _DefaultHeight), false));
            NewButtons.Add(new ButtonDefinition("SAY", "&say", new Size(Width, _DefaultHeight)));

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
                        ButtonDef.AutoRescan);
                }
            }
        }

        public override void AutoFormat()
        {
          Keyboard.AutoFormat(0, 0, _vSpace, _hSpace);
        }
    }
}
