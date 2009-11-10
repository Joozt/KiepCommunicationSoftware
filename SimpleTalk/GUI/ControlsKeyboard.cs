using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Drawing;

namespace SimpleTalk.GUI
{
    class ControlsKeyboard : CustomKeyboard
    {
        public ControlsKeyboard(System.Windows.Forms.Control.ControlCollection Controls)
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
            int DefaultWidth = 50;
            int DefaultHeight = 50;
            int hSpace = 10;
            int vSpace = 10;

            //Scan speed settting
            int Width = (int)Math.Round(1.0* DefaultWidth + 0.0*vSpace);
            NewRow(
                DefaultWidth,
                DefaultHeight,
                new ButtonDefinition[2]
                {  
                   new ButtonDefinition("-", "&scanSpeedDown", new Size(Width, DefaultHeight)),
                   new ButtonDefinition("+", "&scanSpeedUp", new Size(Width, DefaultHeight)),
                });
            
            //Automatic scan speed on/off
              Width = (int)Math.Round(2.0 * DefaultWidth + 1.0*vSpace);
                 NewRow(
                DefaultWidth,
                DefaultHeight,
                new ButtonDefinition[1]
                {  
                   new ButtonDefinition("Aan/Uit", "&scanSpeedFixed", new Size(Width, DefaultHeight)),
                });

             //Wave shaped buttton highlighting for scanning on/off
             Width = (int)Math.Round(2.0 * DefaultWidth + 1.0 * vSpace);
                 NewRow(
                DefaultWidth,
                DefaultHeight,
                new ButtonDefinition[1]
                {  
                   new ButtonDefinition("Aan/Uit", "&scanHighlightWave", new Size(Width, DefaultHeight)),
                });

              //Suggestions for automatic word completion on/off
              Width = (int)Math.Round(2.0 * DefaultWidth + 1.0 * vSpace);
                 NewRow(
                DefaultWidth,
                DefaultHeight,
                new ButtonDefinition[1]
                {  
                   new ButtonDefinition("Aan/Uit", "&autoWordComplete", new Size(Width, DefaultHeight)),
                });

              //Menu go back
              Width = (int)Math.Round(2.0 * DefaultWidth + 1.0 * vSpace);
                 NewRow(
                DefaultWidth,
                DefaultHeight,
                new ButtonDefinition[1]
                {  
                   new ButtonDefinition("Menu terug", "&menuBack", new Size(Width, DefaultHeight)),
                });

            //NewRow(DefaultWidth, DefaultHeight, new ButtonDefinition[1]("Aan/Uit", "&scanSpeedFixed, new Size(Width, DefaultHeight)));


    
           /* List<ButtonDefinition> NewButtons = new List<ButtonDefinition>();
            
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
          */
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
