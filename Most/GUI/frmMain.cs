using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Windows.Forms;
using System.IO;
using Voets.Diagnostics;

namespace Most.GUI
{
  public partial class frmMain : CustomForm
  {
    const string CLASSNAME = "FormAutoComplete";

    private AbcKeyboard Keyboard;

    private string _newLastWord;
    private string _newSecondLastWord;
    private string _oldLastWord;
    private string _oldSecondLastWord;

    private AutoComplete.DataAccess.AutoWords _autoWords;
    private AutoComplete.DataAccess.NextWords _nextWords;

    public frmMain()
    {
      InitializeComponent();

      Keyboard = new AbcKeyboard(pnlKeyboard.Controls);
      Keyboard.CustomKeyPressed += new CustomKeyPressedEventHandler(OnKeyPressed);
      CustomButtonDown += new CustomButtonEventHandler(OnButtonDown);
      CustomButtonUp += new CustomButtonEventHandler(OnButtonUp);

      _autoWords = new AutoComplete.DataAccess.AutoWords();
      _nextWords = new AutoComplete.DataAccess.NextWords();
    }

    private void ImportWords(string fileNamePath)
    {
      try
      {
        string[] wordArray = File.ReadAllLines(fileNamePath);

        foreach (string word in wordArray)
        {
          _autoWords.Add(new AutoComplete.Model.AutoWord(default(int), word, 0, false, false));
        }
      }
      catch (Exception ex)
      {
        Trace.WriteError("({0})", "ImportWords", CLASSNAME, ex, fileNamePath);
        throw;
      }
    }

    private void ResetDatabase()
    {
      _nextWords.Reset();
      _autoWords.Reset();
    }

    private List<string> GetWordArray(string line)
    {
      try
      {
        return new List<string>(line.Split(new string[] { " ", "\r", "\n" }, StringSplitOptions.RemoveEmptyEntries));
      }
      catch (Exception ex)
      {
        Trace.WriteError("({0})", "GetWordArray", CLASSNAME, ex, line);
        throw;
      }
    }

    private void SetWordsCount()
    {
      try
      {
        if (!string.IsNullOrEmpty(_newLastWord) && (_oldLastWord != _newLastWord) || (_oldSecondLastWord != _newSecondLastWord))
        {
          _autoWords.UpdateWordCount(_newLastWord);

          if (!string.IsNullOrEmpty(_newSecondLastWord))
          {
            _nextWords.UpdateNextWordCount(_newSecondLastWord, _newLastWord);
          }
        }
      }
      catch (Exception ex)
      {
        Trace.WriteError(string.Empty, "SetWordsCount", CLASSNAME, ex);
        throw;
      }
    }

    void OnButtonUp(object sender, CustomButtonEventArgs e)
    {
      textBoxOutput.Text += String.Format("(up {0})", e.Button.ToString());
    }

    void OnButtonDown(object sender, CustomButtonEventArgs e)
    {
      textBoxOutput.Text += String.Format("(down {0})", e.Button.ToString());
    }

    void OnKeyPressed(object sender, CustomKeyPressedEventArgs e)
    {
      //txtMessage.Text += String.Format("(screen {0})", e.Keys);
      textBoxOutput.Text += e.Keys;
    }

    private void frmMain_Load(object sender, EventArgs e)
    {
      this.Focus();
    }

    protected override ButtonType CheckButton(Keys keyData)
    {
      switch (keyData)
      {
        case Keys.Oemplus:
          return ButtonType.FirstButton;
        case Keys.OemMinus:
          return ButtonType.SecondButton;
        default:
          return ButtonType.None;
      }
    }

    private void textBoxOutput_TextChanged(object sender, EventArgs e)
    {
      try
      {
        if (string.IsNullOrEmpty(textBoxOutput.Text))
        {
          listBoxAutoSuggestions.DataSource = null;

          _newLastWord = string.Empty;
          _newSecondLastWord = string.Empty;
          _oldLastWord = string.Empty;
          _oldSecondLastWord = string.Empty;
        }
        else
        {
          List<string> wordArray = GetWordArray(textBoxOutput.Text);

          if ((textBoxOutput.Text.Substring(textBoxOutput.Text.Length - 1) == " ") && (wordArray.Count >= 1))
          {
            _oldLastWord = _newLastWord;
            _newLastWord = wordArray[wordArray.Count - 1];

            if (wordArray.Count >= 2)
            {
              _oldSecondLastWord = _newSecondLastWord;
              _newSecondLastWord = wordArray[wordArray.Count - 2];
            }

            SetWordsCount();

            listBoxAutoSuggestions.DataSource = _nextWords.GetNextWordList(wordArray[wordArray.Count - 1]);
          }
          else
          {
            listBoxAutoSuggestions.DataSource = _autoWords.GetWordList(wordArray[wordArray.Count - 1]);
          }
        }
      }
      catch (Exception ex)
      {
        Trace.WriteError(string.Empty, "textBoxOutput_TextChanged", CLASSNAME, ex);
        //MessageBox.Show(ex.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
      }
    }

    private void listBoxAutoSuggestions_KeyDown(object sender, KeyEventArgs e)
    {
      try
      {
        if ((e.KeyData == Keys.Enter) && (listBoxAutoSuggestions.SelectedItem != null))
        {
          List<string> wordArray = GetWordArray(textBoxOutput.Text);

          if ((textBoxOutput.Text.Substring(textBoxOutput.Text.Length - 1) == " ") && (wordArray.Count >= 1))
          {
            wordArray.Add(Convert.ToString(listBoxAutoSuggestions.SelectedItem));
          }
          else
          {
            wordArray[wordArray.Count - 1] = Convert.ToString(listBoxAutoSuggestions.SelectedItem);
          }

          textBoxOutput.Text = string.Format("{0} ", string.Join(" ", wordArray.ToArray()));

          textBoxOutput.Focus();

          textBoxOutput.Select(textBoxOutput.Text.Length, 0);
        }
      }
      catch (Exception ex)
      {
        Trace.WriteError(string.Empty, "listBoxAutoSuggestions_KeyDown", CLASSNAME, ex);
        //MessageBox.Show(ex.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error);
      }
    }

    private void buttonResetDatabase_Click(object sender, EventArgs e)
    {
      ResetDatabase();
    }
  }
}
