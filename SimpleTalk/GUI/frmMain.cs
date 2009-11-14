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



namespace SimpleTalk.GUI
{
  public partial class frmMain : CustomForm
  {
    private AbcKeyboard _Keyboard;
    private Interpreter _Interpreter;
    private AutoComplete _AutComplete;
    private TextToSpeech _TextToSpeech;
    private Sounds _Sounds;


    //private bool _keyPressed = false;


    private bool _RowSelect;
    private bool _Selected;

    


    public frmMain()
    {
      InitializeComponent();

      _Keyboard = new AbcKeyboard(pnlKeyboard.Controls);
      _Interpreter = new Interpreter();
      _AutComplete = new AutoComplete();
      _TextToSpeech = new TextToSpeech();
      _Sounds = new Sounds();

      _Keyboard.CustomKeyPressed += new CustomKeyPressedEventHandler(OnKeyPressed);
      _Keyboard.TimeStarted += new EventHandler(OnTimeStarted);
      _Keyboard.UpdateSelection += new UpdateSelectionEventHandler(OnUpdateSelection);
      _Keyboard.SelectionChanged += new EventHandler(OnSelectionChanged);
      _Keyboard.TimePassed += new EventHandler(OnTimePassed);

      CustomButtonDown += new CustomButtonEventHandler(OnButtonDown);
      CustomButtonUp += new CustomButtonEventHandler(OnButtonUp);

      _Interpreter.TextChanged += new EventHandler(OnTextChanged);
    }

    void OnTimePassed(object sender, EventArgs e)
    {
      Console.Write("\nSelection ended\n");
    }

    void OnSelectionChanged(object sender, EventArgs e)
    {
      if (_RowSelect)
      {
        if (!_Keyboard.NextRow())
        {
          _Keyboard.StopSelection();
        }
      }
      else
      {
        if (!_Keyboard.NextColumn())
        {
          _Keyboard.StopSelection();
        }
      }
    }

    public double GetSelectValue(TimeSpan currentTime, TimeSpan duration)
    {
      if (currentTime.Ticks == 0)
        return 0;
      else if (currentTime > duration)
        return 0;
      else
        return Math.Sin(Math.PI / (duration.Ticks / (double)currentTime.Ticks));
    }

    public Color GetSelectColor(TimeSpan currentTime, TimeSpan duration)
    {
        /* TODO: Other color function
        int StartValue = Color.FromKnownColor(KnownColor.Control).B - 70;
        int ColorValue = (int)Math.Round(70 + StartValue * (1-GetSelectValue(currentTime, duration)));
        return Color.FromArgb(ColorValue, ColorValue, ColorValue);
         */
      int Alpha = (int)Math.Round(255 * GetSelectValue(currentTime, duration));
      return Color.FromArgb(Alpha, Color.DarkGray);
    }

    void OnUpdateSelection(object sender, UpdateSelectionEventArgs e)
    {
      e.BgColor = GetSelectColor(e.CurrentTime, e.Duration);
      e.FgColor = Color.Black;

      if (_Selected)
      {
        if (_Keyboard.ColumnSelected >= 0)
        {
          e.Done = true;
        }

        e.Selected = true;
        _Selected = false;
      }
    }

    void OnTimeStarted(object sender, EventArgs e)
    {
      Console.Write("Start selection\n");

      _RowSelect = true;
      _Selected = false;
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
      if (e.Button == ButtonType.FirstButton)
      {
        if ((_Keyboard.ColumnSelected == -1) && (_Keyboard.RowSelected == -1))
        {
          _Keyboard.StartSelection(new TimeSpan(0, 0, 0, 0, (int)(nSelectionTime.Value * 1000)));
        }
        else if ((_Keyboard.ColumnSelected == -1) && (_Keyboard.RowSelected != -1))
        {
          _RowSelect = false;
          _Selected = true;
        }
        else if ((_Keyboard.ColumnSelected >= 0) && (_Keyboard.RowSelected >= 0))
        {
          _Selected = true;
        }
      }

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
      foreach (string item in _AutComplete.GetAutoCompleteList())
      {
        lbAutoSuggestions.Items.Add(item);
      }
    }

    private void button1_Click(object sender, EventArgs e)
    {
      _AutComplete.Reset();
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
       _AutComplete.OnTextChanged(txtOutput.Text);
      lbAutoSuggestions.Items.Clear();
      foreach (string item in _AutComplete.GetAutoCompleteList())
      {
        lbAutoSuggestions.Items.Add(item);
      }
    }

    private void label1_Click(object sender, EventArgs e)
    {
      _Keyboard.StopSelection();
    }

    private void frmMain_FormClosed(object sender, FormClosedEventArgs e)
    {
      _Keyboard.StopSelection();
    }

    private void button11_Click(object sender, EventArgs e)
    {
      _Keyboard.NextColumn();
    }

    private void button12_Click(object sender, EventArgs e)
    {
      _Keyboard.NextRow();
    }

    private void frmMain_Load(object sender, EventArgs e)
    {

    }
  }
}
