using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Drawing;

namespace SimpleTalk.GUI
{
  public class UpdateSelectionEventArgs : EventArgs
  {
    private TimeSpan _CurrentTime;
    private TimeSpan _Duration;
    private Color _BgColor;
    private Color _FgColor;
    private double _Result;
    private bool _Selected;
    private bool _Done;
    private bool _AutoRestart;

    public UpdateSelectionEventArgs(TimeSpan currenttime, TimeSpan duration)
    {
      _CurrentTime = currenttime;
      _Duration = duration;
      _Selected = false;
      _Done = false;
      _AutoRestart = false;
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

    public bool AutoRestart
    {
        get
        {
            return _AutoRestart;
        }
        set
        {
            _AutoRestart = value;
        }
    }
  }

  public delegate void UpdateSelectionEventHandler(Object sender, UpdateSelectionEventArgs e);
}
