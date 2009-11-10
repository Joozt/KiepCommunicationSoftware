using System;
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
    private ControlsKeyboard _Keyboard;

    public frmSettings()
    {
      InitializeComponent();
      _Keyboard = new ControlsKeyboard(pnlControls.Controls);

    }
  }
}
