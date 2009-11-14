using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Drawing;

namespace SimpleTalk.GUI
{
    class AbcKeyboard : CustomKeyboard
    {
        public AbcKeyboard(System.Windows.Forms.Control.ControlCollection Controls)
            : base(Controls)
        {
        }

        private class ButtonDefinition
        {
            string _Text;
            string _Keys;
            Size _Size;

            public ButtonDefinition(string Text, string Keys)
            {
                _Text = Text;
                _Keys = Keys;
                _Size = new Size(0, 0);
            }

            public ButtonDefinition(string Text, string Keys, Size Size)
                : this(Text, Keys)
            {
                _Size = Size;
            }
            
            public string Text
            {
                get
                {
                    return _Text;
                }
            }

            public string Keys
            {
                get
                {
                    return _Keys;
                }
            }

            public Size Size
            {
                get
                {
                    return _Size;
                }
            }
        }

        public override void ConstructLayout()
        {
            int DefaultWidth = 110;
            int DefaultHeight = 110;
            int hSpace = 10;
            int vSpace = 10;

            int Width = (int)Math.Round(((3/2.0) * DefaultWidth) + (0.5 * vSpace));

            NewRow(
                DefaultWidth,
                DefaultHeight,
                new ButtonDefinition[4]
                {
                    new ButtonDefinition("Spac", " ", new Size(Width, DefaultHeight)),
                    new ButtonDefinition("Auto", "&auto", new Size(Width, DefaultHeight)),
                    new ButtonDefinition("Back", "&back", new Size(Width, DefaultHeight)),
                    new ButtonDefinition("Clear", "&clear", new Size(Width, DefaultHeight)),
                });
    
            List<ButtonDefinition> NewButtons = new List<ButtonDefinition>();
            
            for ( char c = 'a'; c <= 'z'; c++)
            {
                if ((c != 'a') && ((c - 'a') % 6 == 0))
                    NewButtons.Add(null);

                NewButtons.Add(new ButtonDefinition( c.ToString().ToUpper(), c.ToString()));
            }

            Width = (int)Math.Round((2.0 * DefaultWidth) + vSpace);

            NewButtons.Add(new ButtonDefinition("Menu", "&menu", new Size(Width, DefaultHeight)));
            NewButtons.Add(new ButtonDefinition("??", "&??", new Size(Width, DefaultHeight)));

            NewRow(DefaultWidth, DefaultHeight, NewButtons);

            AutoFormat(0, 0, vSpace, hSpace);
        }

        private void NewRow(int DefaultWidth, int DefaultHeight, IEnumerable<ButtonDefinition> NewButtons)
        {
            RowButtons Row = AddRow();
            foreach (ButtonDefinition ButtonDef in NewButtons)
            {
                if (ButtonDef == null)
                {
                    Row = AddRow();
                    continue;
                }
                else
                {

                    Row.AddButton(
                        ButtonDef.Text,
                        ButtonDef.Keys,
                        ButtonDef.Size.IsEmpty ? new Size(DefaultWidth, DefaultHeight) : ButtonDef.Size
                        );
                }
            }
        }
    }
}
