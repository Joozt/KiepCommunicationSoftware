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
    private int _scanSpeed;
    private bool _nextWordSuggestionOn;
    private bool _autoWordCompeltionOn;

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

    public int scanSpeed
    {
      get
      {
        return _scanSpeed;
      }
      set
      {
        _scanSpeed=value;
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
