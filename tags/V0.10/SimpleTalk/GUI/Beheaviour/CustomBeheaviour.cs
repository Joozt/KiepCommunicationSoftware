using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Drawing;
using System.Threading;

namespace SimpleTalk.GUI
{
  abstract class CustomBeheaviour : ICustomBeheaviour
  {
    private CustomKeyboard _Keyboard;
    private TimeSpan _Timer = new TimeSpan(0, 0, 0, 1); // Default 1 sec.
    Thread SelectionThread = null;

    bool _CancelSelection;

    int _CurrentRow = -1;
    int _CurrentColumn = -1;

    public bool NextRow()
    {
      _CurrentRow += 1;

      if (_CurrentRow == Keyboard.GetNumberOfRows())
      {
        _CurrentRow = -1;
        return false;
      }

      return true;
    }

    public bool NextColumn()
    {
      _CurrentColumn += 1;

      if ((_CurrentColumn < 0) || (_CurrentRow < 0))
        return false;

      if (Keyboard.GetNumberOfColumns(_CurrentRow) == _CurrentColumn)
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
      set
      {
        _CurrentRow = value;
      }
    }

    public int ColumnSelected
    {
      get
      {
        return _CurrentColumn;
      }
      set
      {
        _CurrentColumn = value;
      }
    }

    #region Events...

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

    #endregion

    static Object _aLock = new Object();

    protected void DoSelection()
    {
      _CancelSelection = false;

      if (Keyboard.GetNumberOfRows() <= 0)
        _CancelSelection = true;

      _CurrentColumn = -1;
      _CurrentRow = 0;

      UpdateSelectionEventArgs SelectionArgs = new UpdateSelectionEventArgs(new TimeSpan(0, 0, 0), _Timer);
      UpdateButtons(
        Color.FromKnownColor(KnownColor.Control),
        Color.FromKnownColor(KnownColor.ControlText));

      OnTimeStarted(this, new EventArgs());

      do
      {
        DateTime StartTime = DateTime.Now;

        while (StartTime.Add(_Timer).CompareTo(DateTime.Now) > 0)
        {
          if (_CancelSelection)
            break; // Exit thread

          lock (_aLock)
          {
              SelectionArgs = new UpdateSelectionEventArgs(DateTime.Now - StartTime, _Timer);

              // Update selelection
              OnUpdateSelection(this, SelectionArgs);
          }

          // TODO: Quit if user has made his selection
          if ((SelectionArgs.Selected) || (SelectionArgs.Done))
            break;

          UpdateButtons(SelectionArgs.BgColor, SelectionArgs.FgColor);

          Thread.Sleep(10);
        }

        if (SelectionArgs.Done)
        {
            if ((ColumnSelected > -1) && (RowSelected > -1))
            {
                Keyboard.OnClick(RowSelected, ColumnSelected);
            }

            if (!SelectionArgs.AutoRestart)
                break;
        }

        if (_CancelSelection)
          break;

        ResetButtonColor();

        if (SelectionArgs.AutoRestart)
        {
            _CurrentColumn = -1;
            _CurrentRow = 0;
        }
        else
        {
            // Trigger event next row/column
            OnSelectionChanged(this, new EventArgs());
        }

        // Update selection

      } while (!_CancelSelection); // TODO: User has selected or no selection is made or User canceled selection

      // Trigger OnTimePassed
      OnTimePassed(this, new EventArgs());

      ResetButtonColor();

      _CurrentRow = -1;
      _CurrentColumn = -1;
    }

    private void ResetButtonColor()
    {
      UpdateButtons(
        Color.FromKnownColor(KnownColor.Window),
        Color.FromKnownColor(KnownColor.ControlText));
    }

    private void UpdateButtons(Color bgColor, Color fgColor)
    {
      if (_CurrentRow == -1)
        return;

      if (_CurrentColumn == -1)
      {
        // Show one row
        foreach (MostButton Button in Keyboard.GetButtonsRow(RowSelected))
        {
          Button.BackColor = bgColor;
          Button.ForeColor = fgColor;
        }
      }
      else
      {
        // Show one button
        MostButton Button = Keyboard.GetButton(RowSelected, ColumnSelected) as MostButton;
        Button.BackColor = bgColor;
        Button.ForeColor = fgColor;
      }
    }

    public void StartSelection()
    {
      // Start new thread
      SelectionThread = new Thread(DoSelection);
      SelectionThread.Start();
    }

    public void StopSelection()
    {
      _CancelSelection = true;

      if (SelectionThread == null)
        return;

      while ((SelectionThread.ThreadState & ThreadState.Running) != 0)
        Thread.Sleep(100);
    }

    #region ICustomBeheaviour Members

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

    public TimeSpan Timer
    {
      get
      {
        return _Timer;
      }
      set
      {
        _Timer = value;
      }
    }

    abstract public void Attach();

    abstract public void Detach();

    abstract public void OnButtonPressed(CustomButtonEventArgs e);

    #endregion
  }
}
