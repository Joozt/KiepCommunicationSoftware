using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace KiepCommunication.GUI
{
    public enum ButtonType
    {
        None,
        ScanButton,
        YesButton,
        NoButton
    }

    public class CustomButtonEventArgs : EventArgs
    {
        private ButtonType _Button;

        public CustomButtonEventArgs(ButtonType button)
        {
            _Button = button;
        }

        public ButtonType Button
        {
            get
            {
                return _Button;
            }
        }
    }

    public delegate void CustomButtonEventHandler(Object sender, CustomButtonEventArgs e);
}
