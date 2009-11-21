using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using SimpleTalk.GUI;

namespace SimpleTalk.Model
{
  public class Interpreter
  {
    private string _TextOutput;
    private string _TextOutputUpper;
    private frmSettings _formSettings = new frmSettings();

    public Interpreter()
    {
      //Hide settings form
      _formSettings.Hide();
    }

    public string TextOutput
    {
      get
      {
        if (!String.IsNullOrEmpty(_TextOutput))
        {
          _TextOutputUpper = _TextOutput.ToUpper() + '|';
        }
        else
        {
          _TextOutputUpper = "|";
        }
        return _TextOutputUpper;
      }
      set
      {
        _TextOutput = value;
        OnTextChanged(this, new EventArgs());
      }
    }

    public string TextAutoComplete
    {
      get
      {
        return _TextOutput;
      }
    }

    public event EventHandler TextChanged;

    private void OnTextChanged(object sender, EventArgs e)
    {
      if (TextChanged != null)
      {
        TextChanged(sender, e);
      }
    }

    public event EventHandler AutoComplete;

    private void OnAutoComplete(object sender, EventArgs e)
    {
      if (AutoComplete != null)
      {
        AutoComplete(sender, e);
      }
    }

    public void ProcessCommand(string command)
    {

      if (command.Length == 1 || Char.IsLetter(command[0]))
      {
        //if command is a single char add it to the output text
        _TextOutput += command;
      }
      else
      {
        //Check if command is word for autocompletion
        if (command[0] == '#')
        {
          string word = command.TrimStart('#');
          //LastWord is?
          List<string> wordArray = new List<string>(_TextOutput.Split(new string[] { " ", "\r\n" }, StringSplitOptions.RemoveEmptyEntries));

          if (wordArray != null && wordArray.Count > 0)
          {
            string lastWord = wordArray[wordArray.Count() - 1];
            if (_TextOutput[_TextOutput.Length - 1] != ' ')
            {
              //Relpace partial word with complete word and add space
              _TextOutput = _TextOutput.Substring(0, _TextOutput.Length - lastWord.Length) + word + " ";
            }
            else
            {
              //Or add next word and add space
              _TextOutput = _TextOutput + word + " ";
            }
          }
        }
        else
        {
          switch (command)
          {

            #region keyBoardCommands

            case "&back":
              //back space action
              if (!String.IsNullOrEmpty(_TextOutput))
              {
                _TextOutput = _TextOutput.Remove(_TextOutput.Length - 1);
              }
              break;

            case "&space":
              _TextOutput += " ";
              break;

            case "&clear":
              _TextOutput = "";
              break;

            #endregion

            #region variousMainFormCommands

            case "&menu":
              //Open settings form
              _formSettings.updateSettingsDisply();
              _formSettings.Show();
              _formSettings.Focus();
              break;

            case "&auto":
              //Call function that jumps to auto completion / next word suggestion list
              OnAutoComplete(this, new EventArgs());
              break;

            case "&say":
              //TODO: call text to speach engine

              break;

            #endregion

            #region settingMenuCommands

            case "&ScanSpeedDown":
              //TODO: decrease scanning highlight time (speed)
              Core.Instance.scanSpeed--;
              _formSettings.updateSettingsDisply();
              break;

            case "&ScanSpeedUp":
              //TODO: increase scanning highlight time (speed)
              Core.Instance.scanSpeed++;
              _formSettings.updateSettingsDisply();
              break;

            case "&SecondWordSuggestion":
              //Toggle next word suggestion on/off
              Core.Instance.nextWordSuggestionOn = !Core.Instance.nextWordSuggestionOn;
              _formSettings.updateSettingsDisply();
              break;

            case "&AutoWordComplete":
              //Toggle automatic word completion on/off
              Core.Instance.autoWordCompeltionOn = !Core.Instance.autoWordCompeltionOn;
              _formSettings.updateSettingsDisply();
              break;

            case "&GoBack":
              //Close settings form
              _formSettings.Hide();
              break;

            #endregion

            default:
              //Todo: insert nice exception handler.

              break;
          }
        }
      }
      OnTextChanged(this, new EventArgs());
      //Core.Instance.
    }
  }
}
