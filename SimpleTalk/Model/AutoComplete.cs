using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.IO;

namespace SimpleTalk.Model
{
  class AutoComplete
  {
    private List<string> _ListSuggestions = new List<string>();
    //remember length of word list, because items are only stored with increasing amount of text. 
    private int textLength = 0;
    private int previousTextLength = 0;

    public List<string> ListSuggestions
    {
      get
      {
        return _ListSuggestions;
      }
    }

    public void ImportWords(string fileNamePath)
    {
      string[] wordArray = File.ReadAllLines(fileNamePath);

      foreach (string word in wordArray)
      {
        // Todo:  make database connection and add word row to database
        // Todo: if word row does exists skip word

        //_autoWords.Add(new AutoComplete.Model.AutoWord(default(int), word, 0, false, false));
      }
    }

    private void SetWordsCount(string word)
    {
      //Todo: make database connection and update word counter.
      //Todo: if word row does not exists create row

      //_autoWords.UpdateWordCount(word);

      //for debugging:
      _ListSuggestions.Add("SetWordsCount(" + word + ")");
    }

    private void SetNextWordsCount(string secondLastWord, string lastWord)
    {
      //Todo: make database connection and update next word counter.
      //Todo: if nextWord row does not exists create it

      // _nextWords.UpdateNextWordCount(_newSecondLastWord, _newLastWord);

      //for debugging:
      _ListSuggestions.Add("SetNextWordsCount(" + secondLastWord + "," + lastWord + ")");
    }

    private void GetByWord(string word)
    {
      //Todo: make database connection and return word list.

      //_ListSuggestions = null; //Todo: change null to output of database 

      //for debugging:
      _ListSuggestions.Add("GetByWord(" + word + ")");
    }

    private void GetNextWord(string word)
    {
      //Todo: make database connection and return next word list.

      //_ListSuggestions = null; //Todo: change null to output of database 

      //for debugging:
      _ListSuggestions.Add("GetNextWord(" + word + ")");
    }

    public void OnTextChanged(string text)
    {
      textLength = text.Length;

      //for debugging:
      _ListSuggestions.Clear();

      if (string.IsNullOrEmpty(text))
      {
        //return empty list in case of empty string
        //_ListSuggestions.Clear();  //This line should be uncommented in the final code 

        //for debugging:
        _ListSuggestions.Add("Clear list");

        //todo: optional reset temporary stored data
      }
      else
      {
        List<string> lineArray = new List<string>(text.Split(new string[] { "\r\n" }, StringSplitOptions.RemoveEmptyEntries));
        List<string> wordArray = new List<string>();
        if (lineArray != null && lineArray.Count > 0)//if is required for handling empty list exception
        {
          foreach (string word in lineArray[lineArray.Count - 1].Split(new string[] { " " }, StringSplitOptions.RemoveEmptyEntries))
          {
            wordArray.Add(word);
          }
        }

        if (text[text.Length - 1].Equals(' ') || text[text.Length - 1].Equals('\r') || text[text.Length - 1].Equals('\n'))
        {
          //Last word is completed (with space or enter)

          //Count only words if text size is increasing to prevent double counting when line is cleared with backspace
          //When the wordArray is empty no words can be counted
          if (textLength > previousTextLength && wordArray.Count > 0 && text.Length >= 2)
          {
            //filter for two successive split comments to prevent double word counting
            //do not include '\r' because C# adds to the end of the string "\r\n"
            if (!text[text.Length - 2].Equals(' ') && !text[text.Length - 2].Equals('\n'))
            {
              SetWordsCount(wordArray[wordArray.Count - 1]);

              if (wordArray.Count >= 2)
              {
                //Two or more complete words
                SetNextWordsCount(wordArray[wordArray.Count - 2], wordArray[wordArray.Count - 1]);
              }
            }
          }

          if (text[text.Length - 1].Equals(' ') && wordArray.Count > 0)
          {
            //Get suggestions for next word only is space is added and there is a word on the last line
            GetNextWord(wordArray[wordArray.Count - 1]);
          }
        }
        else
        {
          if (wordArray.Count > 0)
          {
            //Incomplete last word (do not count it)
            GetByWord(wordArray[wordArray.Count - 1]);
          }
        }
      }
      //remember text length
      previousTextLength = textLength;
    }

    public List<string> GetAutoCompleteList()
    {
      return _ListSuggestions;
    }

    public void Reset()
    {
      //Todo: add database reset function

      //_nextWords.Reset();
      //_autoWords.Reset();

      throw new NotImplementedException();
    }
  }
}
