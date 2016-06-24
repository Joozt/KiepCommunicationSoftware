using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading;
using System.Windows.Forms;
using System.IO;
using System.Timers;
using KiepCommunication.Model;
using KiepCommunication;
using System.Diagnostics;

namespace KiepCommunication.GUI
{
  public partial class frmMain : CustomForm, IDisposable
  {
    private CustomKeyboard _Keyboard;
    private ICustomLayout _AbcLayout = Core.Instance.GetAbcLayout();
    private CustomBeheaviour _SimpleBeheaviour = new SimpleBeheaviour();

    private CustomKeyboard _AutoKeyboard;
    private AutoCompleteLayoutNew _AutoLayout = new AutoCompleteLayoutNew();
    private CustomBeheaviour _AutoBeheaviour = new SimpleBeheaviour();

    private bool _AutoActive;
    //private bool _keyPressed = false;

    private System.Timers.Timer _timerBgColor;


    public frmMain()
    {
      
      InitializeComponent();

      _Keyboard = new CustomKeyboard(pnlKeyboard.Controls, _AbcLayout, _SimpleBeheaviour);
      _Keyboard.CustomKeyPressed += new CustomKeyPressedEventHandler(OnKeyPressed);
      Core.Instance.SpeedChanged += new EventHandler(OnSpeedChanged);

      _AutoKeyboard = new CustomKeyboard(pnlAutoComplete.Controls, _AutoLayout, _AutoBeheaviour);
      _AutoKeyboard.CustomKeyPressed += new CustomKeyPressedEventHandler(OnKeyPressed);
      _AutoBeheaviour.TimePassed += new EventHandler(OnEndSelection);
      _AutoActive = false;

      Core.Instance.Interpreter.AutoComplete += new EventHandler(OnAutoComplete);
      Core.Instance.Interpreter.TextChanged += new EventHandler(OnTextChanged);

      DatabaseFunctions.Initialize();
      DatabaseFunctions.SuggestionsChanged += new EventHandler(OnSuggestionsChanged);

      if (Core.Instance.UseAbcLayoutWithAutoComplete)
      {
          splitContainer1.Panel2Collapsed = true;
      }

      CustomButtonDown += new CustomButtonEventHandler(OnButtonDown);
      CustomButtonUp += new CustomButtonEventHandler(OnButtonUp);

      txtOutput.Font = Core.DefaultFont;
      OnTextChanged(this, new EventArgs());

      _timerBgColor = new System.Timers.Timer();
    }

    public int SelectedTxtStart 
    {
     get
        {
        return txtOutput.SelectionStart;
        }
    }

    public int SelectedTxtLenght 
    {
     get
        {
        return txtOutput.SelectionLength;
      }
    }

  /*  public int CursorPos
    {
        get
        {
            return txtOutput.SelectionStart;
        }
    }*/
    void OnSpeedChanged(object sender, EventArgs e)
    {
        _SimpleBeheaviour.Timer = Core.Instance.GetScanSpeed();
        _AutoBeheaviour.Timer = Core.Instance.GetScanSpeed();
    }

    private void frmMain_FormClosed(object sender, FormClosedEventArgs e)
    {
        if (!string.IsNullOrEmpty(Core.Instance.Interpreter.Text))
        {
            DatabaseFunctions.AddPhrase(Core.Instance.Interpreter.Text);
        }

      Core.Instance.SpeedChanged -= OnSpeedChanged;

      // Automatically stop selection when stopping the application
      _SimpleBeheaviour.StopSelection();
      _AutoBeheaviour.StopSelection();

      //Disconnect database
      DatabaseFunctions.Dispose();

      //Save settings
      Properties.Settings.Default.Save();
    }

    void OnSuggestionsChanged(object sender, EventArgs e)
    {
        if (!Core.Instance.UseAbcLayoutWithAutoComplete)
        {
            _AutoBeheaviour.StopSelection();
            _AutoLayout.ClearButtons();
            _AutoLayout.AddButtons(DatabaseFunctions.Suggestions, 6); //max number of shown suggestion is 6 (this is limited by the form design)
        }
        else
        {
            ((AbcLayoutNew)_AbcLayout).AddAutoCompleteButtons(DatabaseFunctions.Suggestions);
        }
    }

    void OnAutoComplete(object sender, EventArgs e)
    {
      if (DatabaseFunctions.Suggestions.Count > 0)
      {
        _AutoActive = true;
        _AutoKeyboard.OnButtonPressed(new CustomButtonEventArgs(ButtonType.ScanButton)); // TODO: Hack to start autocomplete automatically
      }
    }

    void OnEndSelection(object sender, EventArgs e)
    {
      _AutoActive = false;
    }

    void OnTextChanged(object sender, EventArgs e)
    {
      txtOutput.Text = Core.Instance.Interpreter.TextOutput;
    }

    void OnButtonUp(object sender, CustomButtonEventArgs e)
    {
      // TODO: Need a better keyup and down capture way
      UpdateButtons(new CustomButtonEventArgs(ButtonType.None)); // Uncheck all buttons
    }

    public void OnButtonDown(object sender, CustomButtonEventArgs e)
    {
      if (!_AutoActive)
        _Keyboard.OnButtonPressed(e);
      else
        _AutoKeyboard.OnButtonPressed(e);

      UpdateButtons(e);
      if ((e.Button == ButtonType.YesButton))
      {
        Core.Instance.TextToSpeech.Say("Ja"); //Core.Instance.Sounds.PlaySound(SoundFiles.Ja);

        if (Core.Instance.YesNoDisplayTime > 0)
        {
          txtOutput.BackColor = System.Drawing.Color.LimeGreen;
          _timerBgColor.Interval = Core.Instance.YesNoDisplayTime;
          _timerBgColor.Elapsed += new ElapsedEventHandler(_timerBgColorTick);
          _timerBgColor.Enabled = true;
        }
      }
      if ((e.Button == ButtonType.NoButton))
      {
        Core.Instance.TextToSpeech.Say("Nee"); //Core.Instance.Sounds.PlaySound(SoundFiles.Nee);

        if (Core.Instance.YesNoDisplayTime > 0)
        {
          txtOutput.BackColor = System.Drawing.Color.Red;
          _timerBgColor.Interval = Core.Instance.YesNoDisplayTime;
          _timerBgColor.Elapsed += new ElapsedEventHandler(_timerBgColorTick);
          _timerBgColor.Enabled = true;
        }
      }
    }

    public void _timerBgColorTick(object sender, EventArgs eArgs)
    {
      if (sender == _timerBgColor)
      {
        txtOutput.BackColor = System.Drawing.Color.White;
      }
    }

    private void UpdateButtons(CustomButtonEventArgs e)
    {

      //cbButton1.Checked = (e.Button == ButtonType.ScanButton);
      //cbButton2.Checked = (e.Button == ButtonType.YesButton);

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

    private void button1_Click(object sender, EventArgs e)
    {
      DatabaseFunctions.ResetDatabase();
    }

    private void button2_Click(object sender, EventArgs e)
    {
      Core.Instance.Interpreter.ProcessCommand("a"); // Letter
      Core.Instance.Interpreter.ProcessCommand("&menu"); // Special function
    }


    private void button4_Click(object sender, EventArgs e)
    {
      // Speaking must be done in a seperate thread to avoid hanging the application!!
      //_TextToSpeech.Say("Dit is een gesproken tekst om te testen of ie werkt", W2);
      Core.Instance.TextToSpeech.Say(Core.Instance.Interpreter.TextAutoComplete);  //txtOutput.Tex
    }

    private void button5_Click(object sender, EventArgs e)
    {
      // Possiblity to stop a very long text
      //_TextToSpeech.StopSpeaking(W2);
      //CreateProcessWindow();
    }

    private void button7_Click(object sender, EventArgs e)
    {
      // Possiblity to stop a very long during sound
      Core.Instance.Sounds.StopSound();
    }

    private void button9_Click(object sender, EventArgs e)
    {
      Core.Instance.Sounds.VolumeUp();
    }

    private void button10_Click(object sender, EventArgs e)
    {
      Core.Instance.Sounds.VolumeDown();
    }

    private void txtOutput_TextChanged(object sender, EventArgs e)
    {
      //TODO: set autocompelete on dedicated event (from
      DatabaseFunctions.OnTextChanged(Core.Instance.Interpreter.TextAutoComplete);
      
      //scroll output text box to the bottom (show the last two lines by default)
      txtOutput.SelectionStart = Core.Instance.Interpreter.cursorPos;
      txtOutput.ScrollToCaret();
    }

    private void label1_Click(object sender, EventArgs e)
    {
      _SimpleBeheaviour.StopSelection();
    }
    
    private void nSelectionTime_ValueChanged(object sender, EventArgs e)
    {
      _SimpleBeheaviour.Timer = new TimeSpan(0, 0, 0, 0, (int)(Core.Instance.ScanSpeed / 10 * 1000));
      _AutoBeheaviour.Timer = new TimeSpan(0, 0, 0, 0, (int)(Core.Instance.ScanSpeed / 10 * 1000));
    }

    #region IDisposable Members

    void IDisposable.Dispose()
    {
      Dispose(true);

      Core.Instance.Interpreter.AutoComplete -= OnAutoComplete;
      Core.Instance.Interpreter.TextChanged -= OnTextChanged;

      GC.SuppressFinalize(this);
    }

    #endregion

    private void buttonImportWords_Click(object sender, EventArgs e)
    {
      //_AutoComplete.ImportWords(textBoxFilePathName.Text, Convert.ToInt32(numericUpDownCountOffset.Value), 0);
    }

    private void buttonClearDatabase_Click(object sender, EventArgs e)
    {
      DatabaseFunctions.ClearDatabase();
    }

    private void splitContainer1_Panel2_Paint(object sender, PaintEventArgs e)
    {

    }


  }
}
