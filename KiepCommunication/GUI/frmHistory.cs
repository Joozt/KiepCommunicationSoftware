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
  public partial class frmHistory : CustomForm
  {
    private CustomKeyboard _Keyboard;
    private HistoryLayout _HistoryLayout = new HistoryLayout();
    private CustomBeheaviour _SimpleBeheaviour = new SimpleBeheaviour();

      private int _PageNumber = 0;

    public frmHistory()
    {
      InitializeComponent();
      _Keyboard = new CustomKeyboard(pnlControls.Controls, _HistoryLayout, _SimpleBeheaviour);
      _Keyboard.CustomKeyPressed += new CustomKeyPressedEventHandler(OnKeyPressed);

      CustomButtonDown += new CustomButtonEventHandler(OnButtonDown);

      Core.Instance.SpeedChanged += new EventHandler(OnSpeedChanged);
    }

    public void UpdatePageNumber()
    {
        int numberOfPages = (int)Math.Ceiling(Model.DatabaseFunctions.PhrasesCount() / 6.0);

        if (_PageNumber < 1)
        {
            _PageNumber = 1;
        }

        if (_PageNumber > numberOfPages)
        {
            _PageNumber = numberOfPages;
        }

        txtNumberOfPages.Text = numberOfPages.ToString();
        txtPageNumber.Text = _PageNumber.ToString();
    }

    public void AddButtons(DateTime dateTime)
    {
        _HistoryLayout.AddButtons(dateTime);
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


    private void frmSettings_VisibleChanged(object sender, EventArgs e)
    {
        if (Visible)
            OnButtonDown(this, new CustomButtonEventArgs(ButtonType.ScanButton));

        _SimpleBeheaviour.StopSelection();
    }

    private void frmSettings_Load(object sender, EventArgs e)
    {

    }

    public DateTime? UpDateTime
    {
        get
        {
            return _HistoryLayout.UpDateTime;
        }
        set
        {
            _HistoryLayout.UpDateTime = value;
        }
    }

    public DateTime? DownDateTime
    {
        get
        {
            return _HistoryLayout.DownDateTime;
        }
    }

    public DateTime CurrentDateTime
    {
        get
        {
            return _HistoryLayout.CurrentDateTime;
        }
    }

    public int PageNumber
    {
        get
        {
            return _PageNumber;
        }
        set
        {
            _PageNumber = value;
        }
    }
  }
}
