using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using KiepCommunication.GUI;
using System.Windows.Forms;
using System.Drawing;

namespace KiepCommunication.Model
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
    private frm123 __123Form;
    private frmHistory _HistoryForm;
    private Interpreter _Interpreter;
    private Sounds _Sounds;
    private TextToSpeech _TextToSpeech;

    public static Font DefaultFont
    {
        get
        {
            return new System.Drawing.Font("Consolas", 58.00F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
        }
    }

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

    public frmMain MainForm
    {
        get
        {
            return _MainForm;
        }
    }

    public frmSettings SettingsForm
    {
        get
        {
            return _SettingsForm;
        }
    }

    public frm123 _123Form
    {
      get
        {
           return __123Form;
        }
    }

    public frmHistory HistoryForm
    {
      get
      {
        return _HistoryForm;
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
        {
          if (value <= 40) //minimum button highlight time (scanSpeed) is 4 seconds
          {
            Properties.Settings.Default.scanSpeed = value;
          }
          else
          {
            Properties.Settings.Default.scanSpeed = 40;
          }
        }
        else
          Properties.Settings.Default.scanSpeed = 5;

        Properties.Settings.Default.Save();

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
        Properties.Settings.Default.Save();
      }
    }

    public bool UnderscoreSpace
    {
        get
        {
            return Properties.Settings.Default.UnderscoreSpace;
        }
        set
        {
            Properties.Settings.Default.UnderscoreSpace = value;
            Properties.Settings.Default.Save();
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
        Properties.Settings.Default.Save();
      }
    }

    public int YesNoDisplayTime
    {
      get
      {
        return Properties.Settings.Default.yesNoDisplayTime;
      }
      set
      {
        if (value > 0) 
        {
          Properties.Settings.Default.yesNoDisplayTime = value;
        }
        else
        {
          Properties.Settings.Default.yesNoDisplayTime = 0;
        }
        Properties.Settings.Default.Save();
      }
    }


    public ICustomLayout GetAbcLayout()
    {
        if (!UseAbcLayoutWithAutoComplete)
            return new AbcLayout();
        else
            return new AbcLayoutNew();
    }

    public bool UseAbcLayoutWithAutoComplete
    {
        get
        {
            return Properties.Settings.Default.UseAbcLayoutWithAutoComplete;
        }
        set
        {
            Properties.Settings.Default.UseAbcLayoutWithAutoComplete = value;
            Properties.Settings.Default.Save();
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
        __123Form = new frm123();
        _HistoryForm = new frmHistory();
        
        // Update all keyboards
        OnSpeedChanged(this, new EventArgs());
    }
  }
}
