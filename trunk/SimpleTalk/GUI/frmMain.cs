using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using System.IO;

namespace SimpleTalk.GUI
{
  public partial class frmMain : CustomForm
  {
    private AbcKeyboard Keyboard;

    public frmMain()
    {
      InitializeComponent();

      Keyboard = new AbcKeyboard(pnlKeyboard.Controls);
      Keyboard.CustomKeyPressed += new CustomKeyPressedEventHandler(OnKeyPressed);
      CustomButtonDown += new CustomButtonEventHandler(OnButtonDown);
      CustomButtonUp += new CustomButtonEventHandler(OnButtonUp);
    }

    void OnButtonUp(object sender, CustomButtonEventArgs e)
    {
      textBoxOutput.Text += String.Format("(up {0})", e.Button.ToString());
    }

    void OnButtonDown(object sender, CustomButtonEventArgs e)
    {
      textBoxOutput.Text += String.Format("(down {0})", e.Button.ToString());
    }

    void OnKeyPressed(object sender, CustomKeyPressedEventArgs e)
    {
      textBoxOutput.Text += String.Format("(screen {0})", e.Keys);
    }

    protected override ButtonType CheckButton(Keys keyData)
    {
      switch (keyData)
      {
        case Keys.Oemplus:
          return ButtonType.FirstButton;
        case Keys.OemMinus:
          return ButtonType.SecondButton;
        default:
          return ButtonType.None;
      }
    }
  }
}
