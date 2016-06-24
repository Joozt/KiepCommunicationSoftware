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

    private void frmSettings_FormClosing(object sender, FormClosingEventArgs e)
    {
      Core.Instance.SpeedChanged -= OnSpeedChanged;

      _SimpleBeheaviour.StopSelection();
    }


    public void UpdateSettingsDisplay()
    {
      btnScanTime.Text = String.Format("{0:0.0}", (float)Core.Instance.ScanSpeed * 0.1);

      if (Core.Instance.AutoWordCompeltionOn)
        btnAutoComplete.Text = "AAN";
      else
        btnAutoComplete.Text = "UIT";

      if (Core.Instance.NextWordSuggestionOn)
        btnNextWord.Text = "AAN";
      else
        btnNextWord.Text = "UIT";

      if (Core.Instance.UnderscoreSpace)
          btnUnderscoreSpace.Text = "AAN";
      else
          btnUnderscoreSpace.Text = "UIT";

      btnYesNoDisplayTime.Text = String.Format("{0:0.0}", (float)Core.Instance.YesNoDisplayTime * 0.001);
    }

    private void frmSettings_VisibleChanged(object sender, EventArgs e)
    {
        if (Visible)
            OnButtonDown(this, new CustomButtonEventArgs(ButtonType.ScanButton));

        _SimpleBeheaviour.StopSelection();
    }  
  }
}
