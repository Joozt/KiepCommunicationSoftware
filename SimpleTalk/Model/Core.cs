using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using SimpleTalk.GUI;
using System.Windows.Forms;

namespace SimpleTalk.Model
{
  public sealed class Core
  {
    #region Singleton

      static Core _Instance;
    static readonly object _PadLock = new object();

    public static Core Instance
    {
      get
      {
        lock (_PadLock)
        {
          if (_Instance == null)
          {
            _Instance = new Core();
            _Instance.Initialize();
          }
          return _Instance;
        }
      }
    }

    #endregion

    private frmMain _MainForm;
    private frmSettings _SettingsForm;
    private Interpreter _Interpreter;
    private Sounds _Sounds;
    private TextToSpeech _TextToSpeech;

    public Interpreter Interpreter
    {
      get
      {
        return _Interpreter;
      }
    }

    public Sounds Sounds
    {
      get
      {
        return _Sounds;
      }
    }

    public TextToSpeech TextToSpeech
    {
      get
      {
        return _TextToSpeech;
      }
    }

    public Form MainForm
    {
        get
        {
            return _MainForm;
        }
    }

    public Form SettingsForm
    {
        get
        {
            return _SettingsForm;
        }
    }

    public event EventHandler SpeedChanged;

    private void OnSpeedChanged(object sender, EventArgs e)
    {
        if (SpeedChanged != null)
        {
            SpeedChanged(sender, new EventArgs());
        }
    }
    #region settings

    //scan speed in 1/10 of seconds
    public int ScanSpeed
    {
      get
      {
        return Properties.Settings.Default.scanSpeed;
      }
      set
      {
        if (value >= 5) //minimum button highlight time (scanSpeed) is 0.5 seconds
          Properties.Settings.Default.scanSpeed = value;
        else
          Properties.Settings.Default.scanSpeed = 5;

        OnSpeedChanged(this, new EventArgs());
      }
    }

    public TimeSpan GetScanSpeed()
    {
        return new TimeSpan(0, 0, 0, 0, (int)(Core.Instance.ScanSpeed / 10.0 * 1000));
    }

    public bool NextWordSuggestionOn
    {
      get
      {
        return Properties.Settings.Default.nextWordSuggestionOn;
      }
      set
      {
        Properties.Settings.Default.nextWordSuggestionOn = value;
      }
    }

    public bool AutoWordCompeltionOn
    {
      get
      {
        return Properties.Settings.Default.autoWordCompletionOn;
      }
      set
      {
        Properties.Settings.Default.autoWordCompletionOn = value;
      }
    }

    #endregion 

    Core()
    {
        // Initialize objects base objects
        _Interpreter = new Interpreter();
        _Sounds = new Sounds();
        _TextToSpeech = new TextToSpeech();
    }

    private void Initialize()
    {
        // Initialize objects that could need the base objects at constructing
        _MainForm = new frmMain();
        _SettingsForm = new frmSettings();
    }
  }
}
