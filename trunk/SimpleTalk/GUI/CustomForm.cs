using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Windows.Forms;

namespace SimpleTalk.GUI
{
    public class CustomForm : Form, IMessageFilter
    {
        const UInt32 WM_KEYDOWN = 0x0100;
        const UInt32 WM_KEYUP = 0x0101;
        const UInt32 WM_SYSKEYDOWN = 0x0104;
        const UInt32 WM_SYSKEYUP = 0x0104;

        public event CustomButtonEventHandler CustomButtonDown;
        public event CustomButtonEventHandler CustomButtonUp;

        public CustomForm()
        {
            if (!DesignMode)
                Application.AddMessageFilter(this);
        }

        protected override void Dispose(bool disposing)
        {
            if (!DesignMode)
                Application.RemoveMessageFilter(this);

            base.Dispose(disposing);
        }

        private void OnCustomButtonDown(object sender, CustomButtonEventArgs e)
        {
            // Only trigger event if a button is pressed
            if (e.Button == ButtonType.None)
                return;

            if (CustomButtonDown != null)
            {
                CustomButtonDown(sender, e);
            }
        }

        private void OnCustomButtonUp(object sender, CustomButtonEventArgs e)
        {
            // Only trigger event if a button is pressed
            if (e.Button == ButtonType.None)
                return;

            if (CustomButtonUp != null)
            {
                CustomButtonUp(sender, e);
            }
        }

        protected virtual ButtonType CheckButton(Keys keyData)
        {
            throw new NotImplementedException();
        }

        #region IMessageFilter Members

        bool _KeyPressed = false;

        public bool PreFilterMessage(ref Message m)
        {
          // TODO: Only perform the keyup and keydown events if form is active (on top)
          

            if ((m.Msg != WM_KEYDOWN) &&
                (m.Msg != WM_KEYUP))
                return false;

            ButtonType ButtonPressed = ButtonType.None;

            Keys keyData = (Keys)(int)m.WParam & Keys.KeyCode;

            if ((m.Msg == WM_KEYDOWN) && (!_KeyPressed))
            {
                _KeyPressed = true;
                ButtonPressed = CheckButton(keyData);
                OnCustomButtonDown(this, new CustomButtonEventArgs(ButtonPressed));
                if (ButtonPressed != ButtonType.None)
                    return true;
            }
            else if (m.Msg == WM_KEYUP)
            {
                _KeyPressed = false;
                ButtonPressed = CheckButton(keyData);
                OnCustomButtonUp(this, new CustomButtonEventArgs(ButtonPressed));
                if (ButtonPressed != ButtonType.None)
                    return true;
            }

            return false;
        }

        #endregion
    }
}

