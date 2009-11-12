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

        public MostButton()
        {
          Font = new System.Drawing.Font("Microsoft Sans Serif", 28, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
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

        #endregion
    }
}
