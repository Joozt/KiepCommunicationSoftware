using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace SimpleTalk.GUI
{
    abstract class CustomLayout : ICustomLayout
    {
        protected CustomKeyboard _Keyboard;

        #region ICustomLayout Members

        public CustomKeyboard Keyboard
        {
            get
            {
                return _Keyboard;
            }
            set
            {
                _Keyboard = value;
            }
        }

        abstract public void ConstructLayout();

        abstract public void AutoFormat();

        #endregion
    }
}
