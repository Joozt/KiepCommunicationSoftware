using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Drawing;
using System.Threading;

namespace SimpleTalk.GUI
{
    //public delegate void OnEventHandler(object sender, EventArgs e);

    public class RowButtons
    {
        private List<ICustomButton> _Buttons = new List<ICustomButton>();

        public List<ICustomButton> Buttons
        {
            get
            {
                return _Buttons;
            }
        }

        public event ButtonEventHandler NewButton;

        private void OnNewButton(object sender, ButtonEventArgs e)
        {
          if (NewButton != null)
          {
            NewButton(sender, e);
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
            OnNewButton(this, new ButtonEventArgs(Button));
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

        public MostButton AddButton(string text, string keys, Size size, bool autoRescan)
        {
            MostButton Button = AddButton(text, keys, size);
            Button.AutoRescan = autoRescan;
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

    public class CustomKeyboard
    {
        private List<RowButtons> _RowButtons = new List<RowButtons>();
        private ICustomLayout _KeyboardLayout;
        private ICustomBeheaviour _KeyboardBeheaviour;
        System.Windows.Forms.Control.ControlCollection _Controls;

        public ICustomLayout KeyboardLayout
        {
          get
          {
            return _KeyboardLayout;
          }
          set
          {
            _KeyboardLayout = value;
          }
        }

        public ICustomBeheaviour KeyboardBeheaviour
        {
          get
          {
            return _KeyboardBeheaviour;
          }
          set
          {
            _KeyboardBeheaviour = value;
          }
        }

        public CustomKeyboard(System.Windows.Forms.Control.ControlCollection Controls, ICustomLayout keyboardLayout, ICustomBeheaviour keyboardBeheaviour)
        {
            _Controls = Controls;

            KeyboardLayout = keyboardLayout;
            KeyboardBeheaviour = keyboardBeheaviour;

            KeyboardLayout.Keyboard = this;
            KeyboardBeheaviour.Keyboard = this;

            // First construct the layout of the keyboard
            KeyboardLayout.ConstructLayout();

            KeyboardBeheaviour.Attach();
        }

        ~CustomKeyboard()
        {
          KeyboardBeheaviour.Detach();
        }

        public List<RowButtons> RowButtons
        {
            get
            {
                return _RowButtons;
            }
        }

        public void AddButtonsToControlCollection()
        {
            foreach (RowButtons u in _RowButtons)
            {
                foreach (MostButton w in u.Buttons)
                {
                    _Controls.Add(w);
                }
            }
        }

        public event CustomKeyPressedEventHandler CustomKeyPressed;

        private void OnClick(object sender, EventArgs e)
        {
            if (CustomKeyPressed != null)
            {
                ICustomButton Button = sender as ICustomButton;

                CustomKeyPressed(sender, new CustomKeyPressedEventArgs(Button.Keys));
            }
        }

        public RowButtons AddRow()
        {
            RowButtons Row = new RowButtons();
            RowButtons.Add(Row);
            Row.Click += new EventHandler(OnClick);
            Row.NewButton += new ButtonEventHandler(OnNewButton);
            return Row;
        }

        void OnNewButton(object sender, ButtonEventArgs e)
        {
          _Controls.Add(e.Button);
        }

        public void ClearButtons()
        {
          foreach (RowButtons u in _RowButtons)
          {
            foreach (MostButton w in u.Buttons)
            {
              _Controls.Remove(w);
            }
          }

          RowButtons.Clear();
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

        public void OnButtonPressed(CustomButtonEventArgs e)
        {
          _KeyboardBeheaviour.OnButtonPressed(e);
        }

        public int GetNumberOfRows()
        {
          return RowButtons.Count;
        }

        public int GetNumberOfColumns(int row)
        {
          if (row >= GetNumberOfRows())
            return 0;

          return RowButtons[row].Buttons.Count;
        }

        public ICustomButton GetButton(int row, int column)
        {
          if (row >= GetNumberOfRows())
            return null;

          if (column >= GetNumberOfColumns(row))
            return null;

          return RowButtons[row].Buttons[column];
        }

        public IEnumerable<ICustomButton> GetButtonsRow(int row)
        {
          if ((GetNumberOfRows() < 0) || (row >= GetNumberOfRows()))
            return new List<ICustomButton>();

          return RowButtons[row].Buttons;
        }

        public void OnClick(ICustomButton button)
        {
          OnClick( button, new EventArgs());
        }

        public void OnClick(int row, int column)
        {
          OnClick(GetButton(row, column));
        }
    }
}
