using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace SimpleTalk.Model
{
  public sealed class Core
  {
    #region Singleton

    static Core _Instance = null;
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
          }
          return _Instance;
        }
      }
    }

    #endregion

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

    #region settings

    //scan speed in 1/10 of seconds
    public int scanSpeed
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
      }
    }

    public bool nextWordSuggestionOn
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

    public bool autoWordCompeltionOn
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
      _Interpreter = new Interpreter();
      _Sounds = new Sounds();
      _TextToSpeech = new TextToSpeech();
    }
  }
}
