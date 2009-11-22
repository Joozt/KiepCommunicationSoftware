using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Drawing;

namespace SimpleTalk.GUI
{
  class SimpleBeheaviour : CustomBeheaviour
  {
    private bool _RowSelect;
    private bool _Selected;
    private bool _Done;

    public override void Attach()
    {
      TimeStarted += new EventHandler(OnTimeStarted);
      UpdateSelection += new UpdateSelectionEventHandler(OnUpdateSelection);
      SelectionChanged += new EventHandler(OnSelectionChanged);
      TimePassed += new EventHandler(OnTimePassed);
    }

    public override void Detach()
    {
      TimeStarted -= OnTimeStarted;
      UpdateSelection -= OnUpdateSelection;
      SelectionChanged -= OnSelectionChanged;
      TimePassed -= OnTimePassed;
    }

    public override void OnButtonPressed(CustomButtonEventArgs e)
    {
      if (e.Button == ButtonType.ScanButton)
      {
        if ((ColumnSelected == -1) && (RowSelected == -1))
        {
          StartSelection();
        }
        else if ((ColumnSelected == -1) && (RowSelected != -1))
        {
          _RowSelect = false;
          _Selected = true;

          _Done = (Keyboard.GetNumberOfColumns(RowSelected) == 1);
        }
        else if ((ColumnSelected >= 0) && (RowSelected >= 0))
        {
          _Selected = true;
          _Done = true;
        }
      }
    }

    void OnTimePassed(object sender, EventArgs e)
    {
      Console.Write("\nSelection ended\n");
      Reset();
    }

    void OnSelectionChanged(object sender, EventArgs e)
    {
        if (_Done)
        {
            Reset();
            return;
        }

        if (_RowSelect)
        {
            if (!NextRow())
            {
                StopSelection();
            }
        }
        else
        {
            if (!NextColumn())
            {
                StopSelection();
            }
        }
    }

    private void Reset()
    {
      _RowSelect = true;
      _Selected = false;
      _Done = false;
    }

    public double GetSelectValue(TimeSpan currentTime, TimeSpan duration)
    {
        return 1;
      //if (currentTime.Ticks == 0)
      //  return 0;
      //else if (currentTime > duration)
      //  return 0;
      //else
      //  return Math.Sin(Math.PI / (duration.Ticks / (double)currentTime.Ticks));
    }

    public Color GetSelectColor(TimeSpan currentTime, TimeSpan duration)
    {
      /* TODO: Other color function
      int StartValue = Color.FromKnownColor(KnownColor.Control).B - 70;
      int ColorValue = (int)Math.Round(70 + StartValue * (1-GetSelectValue(currentTime, duration)));
      return Color.FromArgb(ColorValue, ColorValue, ColorValue);
       */
      int Alpha = (int)Math.Round(255 * GetSelectValue(currentTime, duration));
      return Color.FromArgb(Alpha, Color.Black);
    }

    void OnUpdateSelection(object sender, UpdateSelectionEventArgs e)
    {
      e.BgColor = GetSelectColor(e.CurrentTime, e.Duration);
      e.FgColor = Color.White;

      if (_Selected)
      {
        if ((RowSelected > -1) && (Keyboard.GetNumberOfColumns(RowSelected) == 1))
        {
          ColumnSelected = 0;
        }

        if (ColumnSelected >= 0)
        {
          e.Done = true;
          e.AutoRestart = Keyboard.GetButton(RowSelected, ColumnSelected).AutoRescan;
        }

        e.Selected = true;
        _Selected = false;
      }
    }

    void OnTimeStarted(object sender, EventArgs e)
    {
      Console.Write("Start selection\n");

      _RowSelect = true;
      _Selected = false;
    }
  }
}
