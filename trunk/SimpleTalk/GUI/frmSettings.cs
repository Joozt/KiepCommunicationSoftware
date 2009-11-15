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
    private CustomKeyboard _Keyboard;
    private CustomLayout _SettingsLayout = new SettingsLayout();
    private CustomBeheaviour _SimpleBeheaviour = new SimpleBeheaviour();

    public frmSettings()
    {
      InitializeComponent();
      _Keyboard = new CustomKeyboard(pnlControls.Controls, _SettingsLayout, _SimpleBeheaviour);

    }
  }
}
