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
      _formSettings.Hide();
    }

    //added for debugging
    private List<string> _CommandOutput= new List<string>();
    
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

    
             

    //added for debugging
    public List<string> CommandOutput
    {
      get
      {
        return _CommandOutput;
      }
    }

    //added for debugging
    public void ClearCommand()
    {
      _CommandOutput.Clear();
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
        _TextOutput += command;
      }
      else
      {
        //Check if command is word for autocompletion
        if (command[0] == '#')
        {
          string word = command.TrimStart('#');
          //LastWord is?
          List<string> wordArray = new List<string>(_TextOutput.Split(new string[] { " ","\r\n" }, StringSplitOptions.RemoveEmptyEntries));
   
          if(wordArray != null && wordArray.Count > 0)
          {
            string lastWord = wordArray[wordArray.Count() - 1];
             if(_TextOutput[_TextOutput.Length-1]!=' ')
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
            case "&back":
              //back space action
              if (!String.IsNullOrEmpty(_TextOutput))
              {
                _TextOutput = _TextOutput.Remove(_TextOutput.Length - 1);
              }
              break;

            case "&menu":
              //Call menu that jumps to menu form
              _CommandOutput.Add("[menu]");
              _formSettings.updateSettingsDisply();
              _formSettings.Show();
              //throw new NotImplementedException();
              break;

            case "&auto":
              //Call function that jumps to suggestions from auto completion
              OnAutoComplete(this, new EventArgs());

              //added for debugging
              _CommandOutput.Add("[auto]");
              break;

            case "&space":
              _TextOutput += " ";
              break;

            case "&clear":
              _TextOutput = "";
              break;

            //commands from the settings menu

            case "&ScanSpeedDown":
              //TODO: decrease scanning speed
              Core.Instance.scanSpeed--;
              _formSettings.updateSettingsDisply();
              break;

            case "&ScanSpeedUp":
              //TODO: increase scanning speed
              Core.Instance.scanSpeed++;
              _formSettings.updateSettingsDisply();
              break;

            case "&SecondWordSuggestion":
              //TODO: switch highlighting from wave to hard on/off
              Core.Instance.nextWordSuggestionOn = !Core.Instance.nextWordSuggestionOn;
              _formSettings.updateSettingsDisply();
              break;

            case "&AutoWordComplete":
              //TODO: switch automatic word completion on/off
              Core.Instance.autoWordCompeltionOn = !Core.Instance.autoWordCompeltionOn;
              _formSettings.updateSettingsDisply();
              break;

            case "&say":
              //
              break;

            case "&GoBack":
              _formSettings.Hide();
              break;


            default:
              //Todo: insert nice exception handler.
              _CommandOutput.Add("[ERROR]");
              break;
          }
        }      
      }
      OnTextChanged(this, new EventArgs());
    }
  }
}
