using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using KiepCommunication.Model;

namespace KiepCommunication.GUI
{
  public partial class frm123 : CustomForm
  {
    private CustomKeyboard _Keyboard;
    private CustomLayout _123Layout = new _123Layout();
    private CustomBeheaviour _SimpleBeheaviour = new SimpleBeheaviour();

    public frm123()
    {
      InitializeComponent();
      _Keyboard = new CustomKeyboard(pnlControls.Controls, _123Layout, _SimpleBeheaviour);
      _Keyboard.CustomKeyPressed += new CustomKeyPressedEventHandler(OnKeyPressed);

      CustomButtonDown += new CustomButtonEventHandler(OnButtonDown);

      Core.Instance.SpeedChanged += new EventHandler(OnSpeedChanged);
    }

    void OnSpeedChanged(object sender, EventArgs e)
    {
        _SimpleBeheaviour.Timer = Core.Instance.GetScanSpeed();
    }

    void OnButtonDown(object sender, CustomButtonEventArgs e)
    {
      _Keyboard.OnButtonPressed(e);

      if (e.Button == ButtonType.YesButton || e.Button == ButtonType.NoButton)
      {
        Core.Instance.MainForm.OnButtonDown(sender, e);
      }
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

    private void frm123_FormClosing(object sender, FormClosingEventArgs e)
    {
      Core.Instance.SpeedChanged -= OnSpeedChanged;

      _SimpleBeheaviour.StopSelection();
    }



    private void frm123_VisibleChanged(object sender, EventArgs e)
    {
        if (Visible)
            OnButtonDown(this, new CustomButtonEventArgs(ButtonType.ScanButton));

        _SimpleBeheaviour.StopSelection();
    }  
  }
}
