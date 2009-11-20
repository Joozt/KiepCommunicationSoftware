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
            //Scan speed settting (-)
            int Width = (int)Math.Round(7.0* _DefaultWidth + 1.0*_hSpace);
            NewRow(
                _DefaultWidth,
                _DefaultHeight,
                new ButtonDefinition[1]
                {  
                   new ButtonDefinition("Loop snelheid -", "&ScanSpeedDown", new Size(Width, _DefaultHeight)),
                });

            //Scan speed settting (+)
            Width = (int)Math.Round(7.0 * _DefaultWidth + 1.0 * _hSpace);
            NewRow(
                _DefaultWidth,
                _DefaultHeight,
                new ButtonDefinition[1]
                {  
                   new ButtonDefinition("Loop snelheid +", "&ScanSpeedUp", new Size(Width, _DefaultHeight)),
                });

            //Next word proposed by auto complete on/off
              Width = (int)Math.Round(7.0 * _DefaultWidth + 1.0*_hSpace);
                 NewRow(
                _DefaultWidth,
                _DefaultHeight,
                new ButtonDefinition[1]
                {  
                   new ButtonDefinition("2e woord suggestie", "&SecundWordSuggestion", new Size(Width, _DefaultHeight)),
                });

             //Auto complete suggestion on/off
             Width = (int)Math.Round(7.0 * _DefaultWidth + 1.0 * _hSpace);
                 NewRow(
                _DefaultWidth,
                _DefaultHeight,
                new ButtonDefinition[1]
                {  
                   new ButtonDefinition("Woord voltooien", "&AutoWordComplete", new Size(Width, _DefaultHeight)),
                });

              //Go back to form main
              Width = (int)Math.Round(7.0 * _DefaultWidth + 1.0 * _hSpace);
                 NewRow(
                _DefaultWidth,
                _DefaultHeight,
                new ButtonDefinition[1]
                {  
                   new ButtonDefinition("Terug naar hoofdscherm", "&GoBack", new Size(Width, _DefaultHeight)),
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
