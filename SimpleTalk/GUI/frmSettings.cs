﻿using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using SimpleTalk.Model;

namespace SimpleTalk.GUI
{
  public partial class frmSettings : CustomForm
  {
    private CustomKeyboard _Keyboard;
    private CustomLayout _SettingsLayout = new SettingsLayout();
    private CustomBeheaviour _SimpleBeheaviour = new SimpleBeheaviour();

    public frmSettings()
    {
      InitializeComponent();
      _Keyboard = new CustomKeyboard(pnlControls.Controls, _SettingsLayout, _SimpleBeheaviour);
      _Keyboard.CustomKeyPressed += new CustomKeyPressedEventHandler(OnKeyPressed);

      CustomButtonDown += new CustomButtonEventHandler(OnButtonDown);
    }

    void OnButtonDown(object sender, CustomButtonEventArgs e)
    {
      _Keyboard.OnButtonPressed(e);
    }

    void OnKeyPressed(object sender, CustomKeyPressedEventArgs e)
    {
      if (InvokeRequired)
      {
        this.BeginInvoke(new CustomKeyPressedEventHandler(OnKeyPressed), new Object[] { sender, e });
      }
      else
      {
        Core.Instance.Interpreter.ProcessCommand(e.Keys);
      }
    }

    private void frmSettings_FormClosing(object sender, FormClosingEventArgs e)
    {
      // TODO: Automatically stop selection when stopping the application
      _SimpleBeheaviour.StopSelection();
    }


  
  }
}
