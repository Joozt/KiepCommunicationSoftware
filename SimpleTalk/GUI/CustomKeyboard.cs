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

    public class UpdateSelectionEventArgs : EventArgs
    {
      private TimeSpan _CurrentTime;
      private TimeSpan _Duration;
      private Color _BgColor;
      private Color _FgColor;
      private double _Result;
      private bool _Selected;
      private bool _Done;

      public UpdateSelectionEventArgs(TimeSpan currenttime, TimeSpan duration)
      {
        _CurrentTime = currenttime;
        _Duration = duration;
        _Selected = false;
        _Done = false;
      }

      public TimeSpan CurrentTime
      {
        get
        {
          return _CurrentTime;
        }
      }

      public TimeSpan Duration
      {
        get
        {
          return _Duration;
        }
      }

      public Color BgColor
      {
        get
        {
          return _BgColor;
        }
        set
        {
          _BgColor = value;
        }
      }

      public Color FgColor
      {
        get
        {
          return _FgColor;
        }
        set
        {
          _FgColor = value;
        }
      }

      public double Result
      {
        get
        {
          return _Result;
        }
        set
        {
          _Result = value;
        }
      }

      public bool Selected
      {
        get
        {
          return _Selected;
        }
        set
        {
          _Selected = value;
        }
      }

      public bool Done
      {
        get
        {
          return _Done;
        }
        set
        {
          _Done = value;
        }
      }
    }

    public delegate void UpdateSelectionEventHandler(Object sender, UpdateSelectionEventArgs e);
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

        TimeSpan _SelectDuration;
        bool _CancelSelection;

        int _CurrentRow = -1;
        int _CurrentColumn = -1;

        public bool NextRow()
        {
          _CurrentRow += 1;

          if (_CurrentRow == RowButtons.Count)
          {
            _CurrentRow = -1;
            return false;
          }

          return true;
        }

        public bool NextColumn()
        {
          _CurrentColumn += 1;

          if (_CurrentColumn < 0)
            return false;

          if (RowButtons[_CurrentRow].Buttons.Count == _CurrentColumn)
          {
            _CurrentColumn = -1;
            return false;
          }

          return true;
        }

        public int RowSelected
        {
          get
          {
            return _CurrentRow;
          }
        }

        public int ColumnSelected
        {
          get
          {
            return _CurrentColumn;
          }
        }

        public event EventHandler SelectionChanged;
        public event EventHandler TimeStarted;
        public event UpdateSelectionEventHandler UpdateSelection;
        public event EventHandler TimePassed;

        private void OnSelectionChanged(object sender, EventArgs e)
        {
          if (SelectionChanged != null)
          {
            SelectionChanged(sender, e);
          }
        }

        private void OnTimeStarted(object sender, EventArgs e)
        {
          if (TimeStarted != null)
          {
            TimeStarted(sender, e);
          }
        }

        private void OnUpdateSelection(object sender, UpdateSelectionEventArgs e)
        {
          if (UpdateSelection != null)
          {
            UpdateSelection(sender, e);
          }
        }

        private void OnTimePassed(object sender, EventArgs e)
        {
          if (TimePassed != null)
          {
            TimePassed(sender, e);
          }
        }

        protected void DoSelection()
        {
          _CancelSelection = false;
          _CurrentColumn = -1;
          _CurrentRow = 0;

          UpdateSelectionEventArgs SelectionArgs = new UpdateSelectionEventArgs(new TimeSpan(0,0,0), _SelectDuration);
          UpdateButtons(
            Color.FromKnownColor(KnownColor.Control),
            Color.FromKnownColor(KnownColor.ControlText));

          OnTimeStarted(this, new EventArgs());

          do
          {
            DateTime StartTime = DateTime.Now;

            while (StartTime.Add(_SelectDuration).CompareTo(DateTime.Now) > 0)
            {
              if (_CancelSelection)
                break; // Exit thread

              SelectionArgs = new UpdateSelectionEventArgs(DateTime.Now - StartTime, _SelectDuration);

              // Update selelection
              OnUpdateSelection(this, SelectionArgs);

              // TODO: Quit if user has made his selection
              if ((SelectionArgs.Selected) || (SelectionArgs.Done))
                break;

              UpdateButtons(SelectionArgs.BgColor, SelectionArgs.FgColor);

              Thread.Sleep(100);
            }

            if (SelectionArgs.Done)
            {
              if ((_CurrentColumn > -1) && (_CurrentRow > -1))
              {
                OnClick(RowButtons[_CurrentRow].Buttons[_CurrentColumn], new EventArgs());
              }

              break;
            }

            if (_CancelSelection)
              break;

            UpdateButtons(
              Color.FromKnownColor(KnownColor.Control),
              Color.FromKnownColor(KnownColor.ControlText));

            // Trigger event next row/column
            OnSelectionChanged(this, new EventArgs());

            // Update selection

          } while (!_CancelSelection); // TODO: User has selected or no selection is made or User canceled selection

          // Trigger OnTimePassed
          OnTimePassed(this, new EventArgs());

          UpdateButtons(
            Color.FromKnownColor(KnownColor.Control),
            Color.FromKnownColor(KnownColor.ControlText));

          _CurrentRow = -1;
          _CurrentColumn = -1;
        }

        private void UpdateButtons(Color bgColor, Color fgColor)
        {
          if (_CurrentRow == -1)
            return;

          if (_CurrentColumn == -1)
          {
            // Show one row
            foreach (MostButton Button in RowButtons[_CurrentRow].Buttons)
            {
              Button.BackColor = bgColor;
              Button.ForeColor = fgColor;
            }
          }
          else
          {
            // Show one button
            MostButton Button = RowButtons[_CurrentRow].Buttons[_CurrentColumn];

            Button.BackColor = bgColor;
            Button.ForeColor = fgColor;
          }
        }

        public void StartSelection(TimeSpan duration)
        {
          _SelectDuration = duration;

          // Start new thread
          Thread SelectionThread = new Thread(DoSelection);
          SelectionThread.Start();
          //DoSelection();
        }

        public void StopSelection()
        {
          _CancelSelection = true;
        }
    }
}
