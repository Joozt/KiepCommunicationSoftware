using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Drawing;

namespace SimpleTalk.GUI
{
    class SettingsLayout : CustomLayout
    {
        int _DefaultWidth = 1100;
        int _DefaultHeight = 86;
        int _hSpace = 4;
        int _vSpace = 4;

        override public void ConstructLayout()
        {
            //Scan speed settting (-)
            NewRow(
                _DefaultWidth,
                _DefaultHeight,
                new ButtonDefinition[1]
                {  
                   new ButtonDefinition("KNOP OPLICHT TIJD -", "&ScanSpeedDown"),
                });

            //Scan speed settting (+)
            NewRow(
                _DefaultWidth,
                _DefaultHeight,
                new ButtonDefinition[1]
                {  
                   new ButtonDefinition("KNOP OPLICHT TIJD +", "&ScanSpeedUp"),
                });

            //Next word proposed by auto complete on/off
             
                 NewRow(
                _DefaultWidth,
                _DefaultHeight,
                new ButtonDefinition[1]
                {  
                   new ButtonDefinition("VOLGEND WOORD", "&SecondWordSuggestion"),
                });

             //Auto complete suggestion on/off
            
                 NewRow(
                _DefaultWidth,
                _DefaultHeight,
                new ButtonDefinition[1]
                {  
                   new ButtonDefinition("WOORD VOLTOOIEN", "&AutoWordComplete"),
                });

                 //Yes No back ground color time (-)
                 NewRow(
                     _DefaultWidth,
                     _DefaultHeight,
                     new ButtonDefinition[1]
                {  
                   new ButtonDefinition("JA/NEE KLEUR TIJD -", "&YesNoColorTimeDown"),
                });

                 //Yes No back ground color time (+)
                 NewRow(
                     _DefaultWidth,
                     _DefaultHeight,
                     new ButtonDefinition[1]
                {  
                   new ButtonDefinition("JA/NEE KLEUR TIJD +", "&YesNoColorTimeUp"),
                });

              //Underscore space adding on/off
           
                 NewRow(
                _DefaultWidth,
                _DefaultHeight,
                new ButtonDefinition[1]
                {  
                   new ButtonDefinition("SPATIE KARAKTER", "&UnderscoreSpace"),
                });


                 //Go back to form main

                 NewRow(
                _DefaultWidth,
                _DefaultHeight,
                new ButtonDefinition[1]
                {  
                   new ButtonDefinition("MENU TERUG", "&GoBack"),
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
