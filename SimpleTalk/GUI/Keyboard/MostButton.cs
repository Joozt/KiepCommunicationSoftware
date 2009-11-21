using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Windows.Forms;

namespace SimpleTalk.GUI
{
    public class MostButton: Button, ICustomButton
    {
        string _Keys = "";
        bool _AutoSizeFont = true;
        bool _AutoRescan = true;

        public MostButton()
        {
          Font = new System.Drawing.Font("Consolas", 38.00F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
        }

        public MostButton(string keys) : this()
        {
            _Keys = keys;
        }

        #region ICustomButton Members

        public bool AutoFontSize
        {
            get
            {
                return _AutoSizeFont;
            }
            set
            {
                _AutoSizeFont = value;
            }
        }

        public string Keys
        {
            get
            {
                return _Keys;
            }
            set
            {
                _Keys = value;
            }
        }

        public bool AutoRescan
        {
            get
            {
                return _AutoRescan;
            }
            set
            {
                _AutoRescan = value;
            }
        }

        #endregion
    }
}
