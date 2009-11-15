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
  public partial class frmMain : CustomForm
  {
    private CustomKeyboard _Keyboard;
    private CustomLayout _AbcLayout = new AbcLayout();
    private CustomBeheaviour _SimpleBeheaviour = new SimpleBeheaviour();

    private CustomKeyboard _AutoKeyboard;
    private AutoCompleteLayout _AutoLayout = new AutoCompleteLayout();
    private CustomBeheaviour _AutoBeheaviour = new SimpleBeheaviour();

    private Interpreter _Interpreter;
    private AutoComplete _AutoComplete;
    private TextToSpeech _TextToSpeech;
    private Sounds _Sounds;

    private bool _AutoActive;

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

      _Interpreter = new Interpreter();
      _Interpreter.AutoComplete += new EventHandler(OnAutoComplete);

      _AutoComplete = new AutoComplete();
      _AutoComplete.SuggestionsChanged += new EventHandler(OnSuggestionsChanged);

      _TextToSpeech = new TextToSpeech();
      _Sounds = new Sounds();

      CustomButtonDown += new CustomButtonEventHandler(OnButtonDown);
      CustomButtonUp += new CustomButtonEventHandler(OnButtonUp);

      _Interpreter.TextChanged += new EventHandler(OnTextChanged);
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
      txtOutput.Text = _Interpreter.TextOutput;
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
      if ((e.Button == ButtonType.SecondButton)) _Sounds.PlaySound(SoundFiles.Ja);
      if ((e.Button == ButtonType.ThirdButton)) _Sounds.PlaySound(SoundFiles.Nee);
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
        _Interpreter.ProcessCommand(e.Keys);
      }
    }

    protected override ButtonType CheckButton(Keys keyData)
    {
      switch (keyData)
      {
        case Keys.Add:
          return ButtonType.FirstButton;
        case Keys.Subtract:
          return ButtonType.SecondButton;
        case Keys.Multiply:
          return ButtonType.ThirdButton;
        default:
          return ButtonType.None;
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
      _Interpreter.ProcessCommand("a"); // Letter
      _Interpreter.ProcessCommand("&menu"); // Special function
    }

    private void button3_Click(object sender, EventArgs e)
    {
      //txtOutput.Text = _Interpreter.TextOutput;
             
      
      foreach (string item in _Interpreter.CommandOutput)
      {
        lbAutoSuggestions.Items.Add(item);
      }
      _Interpreter.ClearCommand();
     
    }

    private void button4_Click(object sender, EventArgs e)
    {
      // Speaking must be done in a seperate thread to avoid hanging the application!!
      _TextToSpeech.Say("Dit is een gesproken tekst om te testen of ie werkt");
    }

    private void button5_Click(object sender, EventArgs e)
    {
      // Possiblity to stop a very long text
      _TextToSpeech.StopSpeaking();
    }

    private void button6_Click(object sender, EventArgs e)
    {
      // Sound must be played in a seperate thread to avoid hanging the application!!
      //_Sounds.PlaySound(@"c:\spraakprogramma\audiofiles\Ja.wav");
    }

    private void button7_Click(object  sender, EventArgs e)
    {
      // Possiblity to stop a very long during sound
      _Sounds.StopSound();
    }

    private void button9_Click(object sender, EventArgs e)
    {
      _Sounds.VolumeUp();
    }

    private void button10_Click(object sender, EventArgs e)
    {
      _Sounds.VolumeDown();
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
      _SimpleBeheaviour.StopSelection();
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
  }
}
