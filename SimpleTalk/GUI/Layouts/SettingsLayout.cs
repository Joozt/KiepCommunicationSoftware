using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Drawing;

namespace SimpleTalk.GUI
{
    class SettingsLayout : CustomLayout
    {
        int _DefaultWidth = 100;
        int _DefaultHeight = 100;
        int _hSpace = 10;
        int _vSpace = 10;

        override public void ConstructLayout()
        {
            //Scan speed settting
            int Width = (int)Math.Round(1.0* _DefaultWidth + 0.0*_vSpace);
            NewRow(
                _DefaultWidth,
                _DefaultHeight,
                new ButtonDefinition[2]
                {  
                   new ButtonDefinition("-", "&scanSpeedDown", new Size(Width, _DefaultHeight)),
                   new ButtonDefinition("+", "&scanSpeedUp", new Size(Width, _DefaultHeight)),
                });
            
            //Automatic scan speed on/off
              Width = (int)Math.Round(2.0 * _DefaultWidth + 1.0*_vSpace);
                 NewRow(
                _DefaultWidth,
                _DefaultHeight,
                new ButtonDefinition[1]
                {  
                   new ButtonDefinition("Aan/Uit", "&scanSpeedFixed", new Size(Width, _DefaultHeight)),
                });

             //Wave shaped buttton highlighting for scanning on/off
             Width = (int)Math.Round(2.0 * _DefaultWidth + 1.0 * _vSpace);
                 NewRow(
                _DefaultWidth,
                _DefaultHeight,
                new ButtonDefinition[1]
                {  
                   new ButtonDefinition("Aan/Uit", "&scanHighlightWave", new Size(Width, _DefaultHeight)),
                });

              //Suggestions for automatic word completion on/off
              Width = (int)Math.Round(2.0 * _DefaultWidth + 1.0 * _vSpace);
                 NewRow(
                _DefaultWidth,
                _DefaultHeight,
                new ButtonDefinition[1]
                {  
                   new ButtonDefinition("Aan/Uit", "&autoWordComplete", new Size(Width, _DefaultHeight)),
                });

              //Menu go back
              Width = (int)Math.Round(2.0 * _DefaultWidth + 1.0 * _vSpace);
                 NewRow(
                _DefaultWidth,
                _DefaultHeight,
                new ButtonDefinition[1]
                {  
                   new ButtonDefinition("Menu terug", "&menuBack", new Size(Width, _DefaultHeight)),
                });

                 AutoFormat();
        }

        private void NewRow(int DefaultWidth, int DefaultHeight, IEnumerable<ButtonDefinition> NewButtons)
        {
            RowButtons Row = Keyboard.AddRow();
            foreach (ButtonDefinition ButtonDef in NewButtons)
            {
                if (ButtonDef == null)
                {
                    Row = Keyboard.AddRow();
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

        public override void AutoFormat()
        {
          Keyboard.AutoFormat(0, 0, _vSpace, _hSpace);
        }
    }
}
