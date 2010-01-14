using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Drawing;
using SimpleTalk.Model;

namespace SimpleTalk.GUI
{
    class AbcLayoutNew : CustomLayout
    {
        int _DefaultWidth = 105;
        int _DefaultHeight = 90;
        int _hSpace = 4;
        int _vSpace = 4;

        RowButtons _autoComplete = null;

        override public void ConstructLayout()
        {
            int WidthEmpty = 30;
            int Width = (int)Math.Round((12 * _DefaultWidth + 1 * _vSpace - WidthEmpty) / 6.0);
          

          NewRow(
              _DefaultWidth,
              _DefaultHeight,
              new ButtonDefinition[7]
                {
                    new ButtonDefinition("SPA", " ", new Size(Width, _DefaultHeight)),
                    new ButtonDefinition("<=", "&back", new Size(Width, _DefaultHeight)),
                    new ButtonDefinition("SAY", "&say", new Size(Width, _DefaultHeight)),
                    new ButtonDefinition("123", "&123", new Size(Width, _DefaultHeight)),
                    new ButtonDefinition("", "&empty", new Size(WidthEmpty, _DefaultHeight)),
                    new ButtonDefinition("CLR", "&clear", new Size(Width, _DefaultHeight)),
                    new ButtonDefinition("OUD", "&history", new Size(Width + 20, _DefaultHeight), false)
                    
                });

            List<ButtonDefinition> NewButtons = new List<ButtonDefinition>();

            _autoComplete = _Keyboard.AddRow();

            // Empty autocomplete list
            AddAutoCompleteButton(new ButtonDefinition("", "", new Size(0, _DefaultHeight), false));

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

        public void ClearButtons()
        {
            _autoComplete.Buttons.Clear();
        }

        public uint GetButtonWidth(string buttonText)
        {
            Graphics dc = Keyboard.GetOwnerControl.CreateGraphics();

            uint width = (uint)dc.MeasureString(buttonText, Core.DefaultFont).Width + 20;

            return width;
        }

        public void AddAutoCompleteButton(ButtonDefinition ButtonDef)
        {
            _autoComplete.AddButton(
                        ButtonDef.Text,
                        ButtonDef.Keys,
                        ButtonDef.Size,
                        ButtonDef.AutoRescan);
        }

        public void AddAutoCompleteButtons(List<String> strings)
        {
            if (strings.Count == 0)
            {
                _autoComplete.Skip = true;
                AddAutoCompleteButton(new ButtonDefinition("", "", new Size(0, _DefaultHeight), false));
                return;
            }

            uint maxWidth = (uint)System.Windows.Forms.Screen.FromControl(Keyboard.GetOwnerControl).WorkingArea.Width;
            uint curPosition = 0;

            _autoComplete.Buttons.Clear();

            _autoComplete.Skip = false;

            foreach (string item in strings)
            {
                //only show words that are not too long so they fit on one auto suggestion button (form design dependent)
                int maxWordLength = 18;
                if (item.Length <= maxWordLength)
                {
                    uint width = GetButtonWidth(item.ToUpper());

                    if (curPosition + width < maxWidth)
                    {
                        AddAutoCompleteButton(new ButtonDefinition(item.ToUpper(), "#" + item, new Size((int)width, _DefaultHeight)));
                        curPosition += width;
                    }
                    else
                    {
                        break;
                    }
                }
            }

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
