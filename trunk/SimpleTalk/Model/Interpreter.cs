using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using SimpleTalk.GUI;

namespace SimpleTalk.Model
{
  public class Interpreter
  {
    private string _TextOutputBegin = "";      //ouput text string before cursor
    private string _TextOutputEnd = "";        //ouput text string after cursor
    private string _TextOutput = "";           //total ouput text string
    private string _TextOutputFormatted = "";

    public Interpreter()
    {
    }

    public string TextOutput
    {
      get
      {
        _TextOutputFormatted = _TextOutputBegin + '|' + _TextOutputEnd;
        _TextOutputFormatted = _TextOutputFormatted.ToUpper();

        if (Core.Instance.UnderscoreSpace)
        {
          _TextOutputFormatted = _TextOutputFormatted.Replace(" ", "_ ");  //add underscore (for better visibility) and space (for correct functioning of multiline)
        }

        return _TextOutputFormatted;
      }
      set
      {
        _TextOutputBegin = value;
        _TextOutputEnd = "";
        OnTextChanged(this, new EventArgs());
      }
    }

    public string TextAutoComplete
    {
      get
      {
        //Note that the auto completion requires only the first part of the sentence because it should complete the word the is currently begin typed
        return _TextOutputBegin;
      }
    }

    public int cursorPos //The text lenght is required for the cursor position
    {
      get
      {
        //The begin text lenght with underscores added is returned as cursor position
        return _TextOutputBegin.Replace(" ", "_ ").Length;
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
      int _SelLenght = Core.Instance.MainForm.SelectedTxtLenght;
      int _SelStart = Core.Instance.MainForm.SelectedTxtStart;

      if (command.Length == 1 || Char.IsLetter(command[0]))
      {
        //if command is a single char add it to the output text
        _TextOutputBegin += command;

  /*    if (_TextOutput.Length <= _SelStart)
          _TextOutput += command;
        else
        {
          if (_SelLenght == 0)
          {
            _TextOutput = _TextOutput.Insert(_SelStart, command);
          }
          else
          {
            if (_TextOutput.Length >= _SelLenght + _SelStart)
              _TextOutput = _TextOutput.Remove(_SelStart, _SelLenght);
            else
              _TextOutput = _TextOutput.Remove(_SelStart, _SelLenght - 1);
              _TextOutput = _TextOutput.Insert(_SelStart, command);
          }

        } */

      }
      else
      {
        //Check if command is word for autocompletion
        if (command[0] == '#')
        {
          string word = command.TrimStart('#');
          //Find lastWord
          List<string> wordArray;
          if (!string.IsNullOrEmpty(_TextOutputBegin))
            wordArray = new List<string>(_TextOutputBegin.Split(new string[] { " ", "\r\n" }, StringSplitOptions.RemoveEmptyEntries));
          else
            wordArray = null;

          if (wordArray != null && wordArray.Count > 0)
          {
            string lastWord = wordArray[wordArray.Count() - 1];
            if (_TextOutputBegin[_TextOutputBegin.Length - 1] != ' ')
            {
              //Relpace partial word with complete word and add space
              _TextOutputBegin = _TextOutputBegin.Substring(0, _TextOutputBegin.Length - lastWord.Length) + word + " ";
            }
            else
            {
              //Or add next word and add space
              _TextOutputBegin = _TextOutputBegin + word + " ";
            }
          }
          else
          {
            _TextOutputBegin = _TextOutputBegin + word + " ";
          }
        }
        else
        {
          if (command[0] == '%' && command.Length>1)
          {
            if (command[1] != '%')
            {
              //command is a new line that should be displayed on the main form

              //TODO: decide if old line should be stored in the database before new line is shown.

              _TextOutputBegin = command.Trim('%');
              _TextOutputEnd = "";

              //Close history form after line is displayed on the main form
              Core.Instance.HistoryForm.Hide();
              Core.Instance.MainForm.Focus();

            }
            else
            {
              //command is a database id of the line that should be removed from the database

              //TODO: remove line with ID///

            }
          }
          else
          {
            switch (command)
            {

              #region keyBoardCommands

              case "&back":
                //back space action
                if (!String.IsNullOrEmpty(_TextOutputBegin))
                {
                  _TextOutputBegin = _TextOutputBegin.Remove(_TextOutputBegin.Length - 1);
                }
                break;

              case "&space":
                _TextOutputBegin += " ";
                break;

              case "&clear":
                
                _TextOutputBegin = "";
                _TextOutputEnd = "";

                /*if (_SelLenght == 0)
                {
                  _TextOutput = "";
                }
                else
                {
                  if (_TextOutput.Length >= _SelLenght + _SelStart)
                    _TextOutput = _TextOutput.Remove(_SelStart, _SelLenght);
                  else
                    _TextOutput = _TextOutput.Remove(_SelStart, _SelLenght - 1);
                } */

                break;

              #endregion

              #region variousMainFormCommands

              case "&menu":
                //Open settings form
                (Core.Instance.SettingsForm as frmSettings).UpdateSettingsDisplay();
                Core.Instance.SettingsForm.Show();
                Core.Instance.SettingsForm.Focus();
                break;

              case "&123":
                //Open number and scroll form
                Core.Instance._123Form.Show();
                Core.Instance._123Form.Focus();
                break;

              case "&history":
                //Open history form
                Core.Instance.HistoryForm.Show();
                Core.Instance.HistoryForm.Focus();
                break;

              case "&auto":
                //Call function that jumps to auto completion / next word suggestion list
                OnAutoComplete(this, new EventArgs());
                break;

              case "&say":
                //TODO: call text to speach engine
                Core.Instance.TextToSpeech.Say(_TextOutputBegin + _TextOutputEnd);
                break;

              case "&empty":
                // do nothing 
                break;
              #endregion

              #region settingMenuCommands

              case "&ScanSpeedDown":
                //decrease scanning highlight time (speed)
                Core.Instance.ScanSpeed--;
                (Core.Instance.SettingsForm as frmSettings).UpdateSettingsDisplay();
                break;

              case "&ScanSpeedUp":
                //increase scanning highlight time (speed)
                Core.Instance.ScanSpeed++;
                (Core.Instance.SettingsForm as frmSettings).UpdateSettingsDisplay();
                break;

              case "&SecondWordSuggestion":
                //Toggle next word suggestion on/off
                Core.Instance.NextWordSuggestionOn = !Core.Instance.NextWordSuggestionOn;
                (Core.Instance.SettingsForm as frmSettings).UpdateSettingsDisplay();
                break;

              case "&AutoWordComplete":
                //Toggle automatic word completion on/off
                Core.Instance.AutoWordCompeltionOn = !Core.Instance.AutoWordCompeltionOn;
                (Core.Instance.SettingsForm as frmSettings).UpdateSettingsDisplay();
                break;

              case "&YesNoColorTimeDown":
                //decrease yes/no back groud color time
                Core.Instance.YesNoDisplayTime -= 100; //in [ms] change per 0.1sec 
                (Core.Instance.SettingsForm as frmSettings).UpdateSettingsDisplay();
                break;

              case "&YesNoColorTimeUp":
                //decrease yes/no back groud color time
                Core.Instance.YesNoDisplayTime += 100; //in [ms] change per 0.1sec 
                (Core.Instance.SettingsForm as frmSettings).UpdateSettingsDisplay();
                break;

              case "&UnderscoreSpace":
                //decrease yes/no back groud color time
                Core.Instance.UnderscoreSpace = !Core.Instance.UnderscoreSpace;
                (Core.Instance.SettingsForm as frmSettings).UpdateSettingsDisplay();
                break;

              case "&GoBack":
                //Close settings form
                Core.Instance.SettingsForm.Hide();
                Core.Instance.MainForm.Focus();
                break;

              #endregion

              #region 123MenuCommands

              case "&wordBack":
                //Delete last word before cursor
                if (!string.IsNullOrEmpty(_TextOutputBegin))
                {
                  int _length = _TextOutputBegin.Length;
                  int i = _length - 1;

                  //search for first next space to the left
                  while (i > 0 && _TextOutputBegin[i] != ' ') i--;

                  _TextOutputBegin = _TextOutputBegin.Remove(i, _length - i);
                }
                break;

              case "&charLeft":
                //Move cursor 1 char to the left
                if (!string.IsNullOrEmpty(_TextOutputBegin))
                {
                  _TextOutputEnd = _TextOutputBegin[_TextOutputBegin.Length - 1] + _TextOutputEnd;
                  _TextOutputBegin = _TextOutputBegin.Remove(_TextOutputBegin.Length - 1, 1);
                }
                break;

              case "&charRight":
                //Move cursor 1 char to the right
                if (!string.IsNullOrEmpty(_TextOutputEnd))
                {
                  _TextOutputBegin = _TextOutputBegin + _TextOutputEnd[0];
                  _TextOutputEnd = _TextOutputEnd.Remove(0, 1);
                }
                break;

              case "&wordLeft":
                //Move cursor 1 word to the left
                if (!string.IsNullOrEmpty(_TextOutputBegin))
                {
                  int _length = _TextOutputBegin.Length;
                  int i = _length - 1;

                  //search for first next space to the left
                  while (i > 0 && _TextOutputBegin[i] != ' ') i--;

                  _TextOutputEnd = _TextOutputBegin.Substring(i, _length - i) + _TextOutputEnd;
                  _TextOutputBegin = _TextOutputBegin.Remove(i, _length - i);
                }
                break;

              case "&wordRight":
                //Move cursor 1 word to the right
                if (!string.IsNullOrEmpty(_TextOutputEnd))
                {
                  int _length = _TextOutputEnd.Length;
                  int i = 0;

                  //search for first next space to the right
                  while (i < _length - 1 && _TextOutputEnd[i] != ' ') i++;

                  _TextOutputBegin = _TextOutputBegin + _TextOutputEnd.Substring(0, i + 1);
                  _TextOutputEnd = _TextOutputEnd.Remove(0, i + 1);
                }
                break;

              case "&home":
                //Set cursor to begin of the line
                _TextOutputEnd = _TextOutputBegin + _TextOutputEnd;
                _TextOutputBegin = "";
                break;

              case "&end":
                //Set cursor to end of the line
                _TextOutputBegin = _TextOutputBegin + _TextOutputEnd;
                _TextOutputEnd = "";
                break;


              case "&questionMark":
                //Add question mark and space
                _TextOutputBegin += "? ";
                break;

              case "&exclamation":
                //Add exclamation and space
                _TextOutputBegin += "! ";
                break;

              case "&close123":
                //Close number form
                Core.Instance._123Form.Hide();
                Core.Instance.MainForm.Focus();
                break;

              #endregion


              #region historyMenuCommands
              case "&histDown":
                //Show next lines

                break;

              case "&histUp":
                //Show previous lines

                break;


              case "&histClose":
                //Close history form
                Core.Instance.HistoryForm.Hide();
                Core.Instance.MainForm.Focus();
                break;

              #endregion


              default:
                //Todo: insert nice exception handler.

                break;
            } 
          }
        }
      }
      OnTextChanged(this, new EventArgs());
      //Core.Instance.
    }
  }
}
