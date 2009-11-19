﻿using System;
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

    Core()
    {
      _Interpreter = new Interpreter();
      _Sounds = new Sounds();
    }
  }
}