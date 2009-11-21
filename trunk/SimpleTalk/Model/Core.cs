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

    //TODO: save these settings: 
    private int _scanSpeed = 15;
    private bool _nextWordSuggestionOn = true;
    private bool _autoWordCompeltionOn = true;

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

    //scan speed in 1/10 of seconds
    public int scanSpeed
    {
      get
      {
        return _scanSpeed;
      }
      set
      {
        if (value >= 5)
          _scanSpeed = value;
        else
          _scanSpeed = 5;
      }
    }

    public bool nextWordSuggestionOn
    {
      get
      {
        return _nextWordSuggestionOn;
      }
      set
      {
        _nextWordSuggestionOn = value;
      }
    }

    public bool autoWordCompeltionOn
    {
      get
      {
        return _autoWordCompeltionOn;
      }
      set
      {
        _autoWordCompeltionOn = value;
      }
    }

    public TextToSpeech TextToSpeech
    {
        get
        {
            return _TextToSpeech;
        }
    }

    Core()
    {
      _Interpreter = new Interpreter();
      _Sounds = new Sounds();
      _TextToSpeech = new TextToSpeech();
    }
  }
}
