using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using System.IO;
using SimpleTalk.Model;
using System.Threading;
using SimpleTalk;

namespace SimpleTalk.GUI
{
  public partial class frmMain : CustomForm, IDisposable
  {
    private CustomKeyboard _Keyboard;
    private CustomLayout _AbcLayout = new AbcLayout();
    private CustomBeheaviour _SimpleBeheaviour = new SimpleBeheaviour();

    private CustomKeyboard _AutoKeyboard;
    private AutoCompleteLayout _AutoLayout = new AutoCompleteLayout();
    private CustomBeheaviour _AutoBeheaviour = new SimpleBeheaviour();

    private AutoComplete _AutoComplete;
    private TextToSpeech _TextToSpeech;

    private bool _AutoActive;
    private int W2;
    //private bool _keyPressed = false;

    public frmMain()
    {
      InitializeComponent();

      nSelectionTime_ValueChanged(this, new EventArgs()); // Update Timer

      _Keyboard = new CustomKeyboard(pnlKeyboard.Controls, _AbcLayout, _SimpleBeheaviour);
      _Keyboard.CustomKeyPressed += new CustomKeyPressedEventHandler(OnKeyPressed);

      _AutoKeyboard = new CustomKeyboard(pnlAutoComplete.Controls, _AutoLayout, _AutoBeheaviour);
      _AutoKeyboard.CustomKeyPressed += new CustomKeyPressedEventHandler(OnKeyPressed);
      _AutoBeheaviour.TimePassed += new EventHandler(OnEndSelection);
      _AutoActive = false;

      Core.Instance.Interpreter.AutoComplete += new EventHandler(OnAutoComplete);
      Core.Instance.Interpreter.TextChanged += new EventHandler(OnTextChanged);

      _AutoComplete = new AutoComplete();
      _AutoComplete.SuggestionsChanged += new EventHandler(OnSuggestionsChanged);

      _TextToSpeech = new TextToSpeech();

      CustomButtonDown += new CustomButtonEventHandler(OnButtonDown);
      CustomButtonUp += new CustomButtonEventHandler(OnButtonUp);

      CreateProcessWindow();
    }

    void CreateProcessWindow()
    {
        _TextToSpeech.MakeProcess();

        ActiveWindow AW = new ActiveWindow();
        W2 = AW.GetActiveWindow();
    }

    void OnSuggestionsChanged(object sender, EventArgs e)
    {
      _AutoLayout.ClearButtons();
      _AutoLayout.AddButtons(_AutoComplete.Suggestions, 6);
    }

    void OnAutoComplete(object sender, EventArgs e)
    {
      if (_AutoComplete.Suggestions.Count > 0)
      {
        _AutoActive = true;
        _AutoKeyboard.OnButtonPressed(new CustomButtonEventArgs(ButtonType.FirstButton)); // TODO: Hack to start autocomplete automatically
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

    void OnButtonDown(object sender, CustomButtonEventArgs e)
    {
      if (!_AutoActive)
        _Keyboard.OnButtonPressed(e);
      else
        _AutoKeyboard.OnButtonPressed(e);

      UpdateButtons(e);
      if ((e.Button == ButtonType.SecondButton)) Core.Instance.Sounds.PlaySound(SoundFiles.Ja);
      if ((e.Button == ButtonType.ThirdButton)) Core.Instance.Sounds.PlaySound(SoundFiles.Nee);
    }

    private void UpdateButtons(CustomButtonEventArgs e)
    {
      
      cbButton1.Checked = (e.Button == ButtonType.FirstButton);
      cbButton2.Checked = (e.Button == ButtonType.SecondButton);
     
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

    private void btnGetAutoList_Click(object sender, EventArgs e)
    {
      lbAutoSuggestions.Items.Clear();

      foreach (string item in _AutoComplete.GetAutoCompleteList())
      {
        lbAutoSuggestions.Items.Add(item);
      }
    }

    private void button1_Click(object sender, EventArgs e)
    {
      _AutoComplete.Reset();
    }

    private void button2_Click(object sender, EventArgs e)
    {
      Core.Instance.Interpreter.ProcessCommand("a"); // Letter
      Core.Instance.Interpreter.ProcessCommand("&menu"); // Special function
    }

    private void button3_Click(object sender, EventArgs e)
    {
      //txtOutput.Text = _Interpreter.TextOutput;
             
      
      foreach (string item in Core.Instance.Interpreter.CommandOutput)
      {
        lbAutoSuggestions.Items.Add(item);
      }
      Core.Instance.Interpreter.ClearCommand();
     
    }

    private void button4_Click(object sender, EventArgs e)
    {
      // Speaking must be done in a seperate thread to avoid hanging the application!!
        //_TextToSpeech.Say("Dit is een gesproken tekst om te testen of ie werkt", W2);
        _TextToSpeech.Say(txtOutput.Text, W2);
    }

    private void button5_Click(object sender, EventArgs e)
    {
      // Possiblity to stop a very long text
      _TextToSpeech.StopSpeaking(W2);
      CreateProcessWindow();
    }

    private void button6_Click(object sender, EventArgs e)
    {
      // Sound must be played in a seperate thread to avoid hanging the application!!
      //_Sounds.PlaySound(@"c:\spraakprogramma\audiofiles\Ja.wav");
    }

    private void button7_Click(object  sender, EventArgs e)
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
       _AutoComplete.OnTextChanged(txtOutput.Text);
      lbAutoSuggestions.Items.Clear();
      foreach (string item in _AutoComplete.GetAutoCompleteList())
      {
        lbAutoSuggestions.Items.Add(item);
      }
    }

    private void label1_Click(object sender, EventArgs e)
    {
      _SimpleBeheaviour.StopSelection();
    }

    private void frmMain_FormClosed(object sender, FormClosedEventArgs e)
    {
      // TODO: Automatically stop selection when stopping the application
      _SimpleBeheaviour.StopSelection();
      _AutoBeheaviour.StopSelection();
    }

    private void button11_Click(object sender, EventArgs e)
    {
    }

    private void button12_Click(object sender, EventArgs e)
    {
    }

    private void frmMain_Load(object sender, EventArgs e)
    {

    }

    private void nSelectionTime_ValueChanged(object sender, EventArgs e)
    {
      _SimpleBeheaviour.Timer = new TimeSpan(0, 0, 0, 0, (int)(nSelectionTime.Value * 1000));
      _AutoBeheaviour.Timer = new TimeSpan(0, 0, 0, 0, (int)(nSelectionTime.Value * 1000));
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

    private void button8_Click(object sender, EventArgs e)
    {
      Core.Instance.Interpreter.ProcessCommand("&menu");
    }
  }
}
