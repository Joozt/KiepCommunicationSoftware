﻿using System;
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

    public frmMain()
    {
      InitializeComponent();

      _Keyboard = new AbcKeyboard(pnlKeyboard.Controls);
      _Interpreter = new Interpreter();
      _AutComplete = new AutoComplete();
      _TextToSpeech = new TextToSpeech();
      _Sounds = new Sounds();

      _Keyboard.CustomKeyPressed += new CustomKeyPressedEventHandler(OnKeyPressed);
      CustomButtonDown += new CustomButtonEventHandler(OnButtonDown);
      CustomButtonUp += new CustomButtonEventHandler(OnButtonUp);
      _Interpreter.TextChanged += new EventHandler(OnTextChanged);
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
      UpdateButtons(e);
    }

    private void UpdateButtons(CustomButtonEventArgs e)
    {
      cbButton1.Checked = (e.Button == ButtonType.FirstButton);
      cbButton2.Checked = (e.Button == ButtonType.SecondButton);
    }

    void OnKeyPressed(object sender, CustomKeyPressedEventArgs e)
    {
      _Interpreter.ProcessCommand(e.Keys);
    }

    protected override ButtonType CheckButton(Keys keyData)
    {
      switch (keyData)
      {
        case Keys.Add:
          return ButtonType.FirstButton;
        case Keys.Subtract:
          return ButtonType.SecondButton;
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
      _Sounds.PlaySound("Ja.wav");
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
  }
}