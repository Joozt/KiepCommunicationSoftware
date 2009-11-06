using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Drawing;

namespace Most.GUI
{
    //public delegate void OnEventHandler(object sender, EventArgs e);

    public class RowButtons
    {
        private List<MostButton> _Buttons = new List<MostButton>();

        public List<MostButton> Buttons
        {
            get
            {
                return _Buttons;
            }
        }

        public event EventHandler Click;

        private void OnClick(object sender, EventArgs e)
        {
            if (Click != null)
            {
                Click(sender, e);
            }
        }

        #region AddButton methods

        public MostButton AddButton()
        {
            MostButton Button = new MostButton();
            Buttons.Add(Button);
            Button.Click += new EventHandler(OnClick);
            return Button;
        }

        public MostButton AddButton(string text, string keys)
        {
            MostButton Button = AddButton();
            Button.Text = text;
            Button.Keys = keys;
            return Button;
        }

        public MostButton AddButton(string text, string keys, Point location)
        {
            MostButton Button = AddButton(text, keys);
            Button.Location = location;
            return Button;
        }

        public MostButton AddButton(string text, string keys, Size size)
        {
            MostButton Button = AddButton(text, keys);
            Button.Size = size;
            return Button;
        }

        public MostButton AddButton(string text, string keys, Point location, Size size)
        {
            MostButton Button = AddButton(text, keys, location);
            Button.Size = size;
            return Button;
        }

        public void AutoFormat(int left, int top, int hSpace)
        {
            Point Location = new Point() { X = left, Y = top };

            foreach (MostButton Button in Buttons)
            {
                Button.Location = Location;
                Location.X += Button.Size.Width + hSpace;
            }
        }

        public int GetMaxHeight()
        {
            int MaxHeight = 0;

            foreach (MostButton Button in Buttons)
            {
                MaxHeight = MaxHeight < Button.Size.Height ? Button.Size.Height : MaxHeight;
            }

            return MaxHeight;
        }

        #endregion
    }

    public class CustomKeyPressedEventArgs : EventArgs
    {
        private string _Keys;

        public CustomKeyPressedEventArgs(string keys)
        {
            _Keys = keys;
        }

        public string Keys
        {
            get
            {
                return _Keys;
            }
        }
    }

    public delegate void CustomKeyPressedEventHandler(Object sender, CustomKeyPressedEventArgs e);

    public abstract class CustomKeyboard
    {
        private List<RowButtons> _RowButtons = new List<RowButtons>();

        public CustomKeyboard(System.Windows.Forms.Control.ControlCollection Controls)
        {
            // First construct the layout of the keyboard
            ConstructLayout();

            // Add all buttons to the control collection
            AddButtonsToControlCollection(Controls);
        }

        public List<RowButtons> RowButtons
        {
            get
            {
                return _RowButtons;
            }
        }

        public void AddButtonsToControlCollection(System.Windows.Forms.Control.ControlCollection Controls)
        {
            foreach (RowButtons u in _RowButtons)
            {
                foreach (MostButton w in u.Buttons)
                {
                    Controls.Add(w);
                }
            }
        }

        public event CustomKeyPressedEventHandler CustomKeyPressed;

        private void OnClick(object sender, EventArgs e)
        {
            if (CustomKeyPressed != null)
            {
                MostButton Button = sender as MostButton;

                CustomKeyPressed(sender, new CustomKeyPressedEventArgs(Button.Keys));
            }
        }

        protected RowButtons AddRow()
        {
            RowButtons Row = new RowButtons();
            RowButtons.Add(Row);
            Row.Click += new EventHandler(OnClick);
            return Row;
        }

        public virtual void ConstructLayout()
        {
            throw new NotImplementedException();
        }

        public void AutoFormat(int left, int top, int vSpace, int hSpace)
        {
            Point Location = new Point() { X = left, Y = top };

            foreach (RowButtons Row in RowButtons)
            {
                Row.AutoFormat(Location.X, Location.Y, hSpace);
                Location.Y += Row.GetMaxHeight() + vSpace;
            }
        }
    }
}
